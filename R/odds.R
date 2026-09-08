# ------------------------------------------------------------------
# odds.R — Moteur de cotes et règlement des paris
# ------------------------------------------------------------------
# Principe :
#   * Marché "vainqueur" : probabilité Elo (recalculée à partir des
#     résultats joués) mélangée à la probabilité implicite du marché
#     (répartition des mises). Plus le volume de mises est grand,
#     plus le marché pèse. Une marge de bookmaker de 5 % est appliquée.
#   * Marché "écart" : trois tranches d'écart (1-2 / 3-5 / 6+) avec un
#     prior, mis à jour par la distribution empirique des écarts déjà
#     observés, puis ajusté par le flux de mises.
#   * La cote est figée dans la table bets au moment du pari.
# ------------------------------------------------------------------

ELO_INIT  <- 1000
ELO_K     <- 60      # K élevé : peu de matchs, on veut que ça bouge
MARGE     <- 0.95    # marge bookmaker : cote = 0.95 / p
COTE_MIN  <- 1.05
COTE_MAX  <- 20
POIDS_MARCHE_VAINQUEUR <- 150  # volume (StatCoins) où marché et Elo pèsent autant
POIDS_MARCHE_ECART     <- 100

ECART_TRANCHES <- c("1-2", "3-5", "6")
ECART_PRIOR    <- c("1-2" = 3.5, "3-5" = 4, "6" = 2.5)  # pseudo-effectifs

ecart_tranche <- function(e) {
  dplyr::case_when(
    e <= 2 ~ "1-2",
    e <= 5 ~ "3-5",
    TRUE   ~ "6"
  )
}

prob_elo <- function(ra, rb) 1 / (1 + 10^((rb - ra) / 400))

# Classement Elo recalculé from scratch sur les matchs joués,
# avec multiplicateur de marge de victoire.
compute_elo <- function(matches) {
  ratings <- setNames(
    rep(ELO_INIT, length(unique(c(matches$home_id, matches$away_id)))),
    unique(c(matches$home_id, matches$away_id))
  )
  played <- matches[matches$played == 1, , drop = FALSE]
  if (nrow(played) == 0) return(ratings)
  played <- played[order(played$journee, played$date_match, played$match_id), ]

  for (i in seq_len(nrow(played))) {
    h <- as.character(played$home_id[i])
    a <- as.character(played$away_id[i])
    res_h <- as.numeric(played$score_home[i] > played$score_away[i])
    ecart <- abs(played$score_home[i] - played$score_away[i])
    k_eff <- ELO_K * (1 + ecart / 10)
    p_h <- prob_elo(ratings[h], ratings[a])
    ratings[h] <- ratings[h] + k_eff * (res_h - p_h)
    ratings[a] <- ratings[a] + k_eff * ((1 - res_h) - (1 - p_h))
  }
  ratings
}

clamp_cote <- function(p) {
  round(pmax(COTE_MIN, pmin(COTE_MAX, MARGE / p)), 2)
}

# ------------------------------------------------------------------
# Cotes "vainqueur" pour un match donné
# Retourne c(home = ..., away = ...)
# ------------------------------------------------------------------

cotes_vainqueur <- function(con, match_row, matches = NULL) {
  if (is.null(matches)) matches <- get_matches(con)
  ratings <- compute_elo(matches)
  p_elo_h <- prob_elo(
    ratings[as.character(match_row$home_id)],
    ratings[as.character(match_row$away_id)]
  )

  flux <- dbx_get(con, "
    SELECT selection, SUM(mise) AS total
    FROM bets WHERE match_id = ? AND type = 'vainqueur'
    GROUP BY selection",
    params = list(match_row$match_id))

  mise_h <- sum(flux$total[flux$selection == as.character(match_row$home_id)])
  mise_a <- sum(flux$total[flux$selection == as.character(match_row$away_id)])
  volume <- mise_h + mise_a

  p_mkt_h <- (mise_h + 1) / (volume + 2)            # lissage de Laplace
  w <- volume / (volume + POIDS_MARCHE_VAINQUEUR)   # poids du marché
  p_h <- (1 - w) * p_elo_h + w * p_mkt_h
  p_h <- pmin(pmax(p_h, 0.05), 0.95)

  c(home = unname(clamp_cote(p_h)), away = unname(clamp_cote(1 - p_h)))
}

# ------------------------------------------------------------------
# Cotes "écart" pour un match donné
# Retourne un vecteur nommé sur les trois tranches
# ------------------------------------------------------------------

cotes_ecart <- function(con, match_row, matches = NULL) {
  if (is.null(matches)) matches <- get_matches(con)
  played <- matches[matches$played == 1, , drop = FALSE]

  effectifs <- ECART_PRIOR
  if (nrow(played) > 0) {
    obs <- table(ecart_tranche(abs(played$score_home - played$score_away)))
    for (tr in names(obs)) effectifs[tr] <- effectifs[tr] + obs[[tr]]
  }
  p_hist <- effectifs / sum(effectifs)

  flux <- dbx_get(con, "
    SELECT selection, SUM(mise) AS total
    FROM bets WHERE match_id = ? AND type = 'ecart'
    GROUP BY selection",
    params = list(match_row$match_id))

  mises <- setNames(rep(0, 3), ECART_TRANCHES)
  if (nrow(flux) > 0) mises[flux$selection] <- flux$total
  volume <- sum(mises)
  p_mkt <- (mises + 1) / (volume + 3)
  w <- volume / (volume + POIDS_MARCHE_ECART)

  p <- (1 - w) * p_hist[ECART_TRANCHES] + w * p_mkt[ECART_TRANCHES]
  p <- pmin(pmax(p, 0.03), 0.95)

  setNames(clamp_cote(p), ECART_TRANCHES)
}

# ------------------------------------------------------------------
# Règlement d'un match : enregistre le score, solde tous les paris.
# Retourne un petit résumé (nb de paris gagnants, total redistribué).
# ------------------------------------------------------------------

settle_match <- function(con, match_id, score_home, score_away) {
  dbx_exec(con, "
    UPDATE matches SET played = 1, score_home = ?, score_away = ?
    WHERE match_id = ?",
    params = list(score_home, score_away, match_id))

  m <- dbx_get(con, "SELECT * FROM matches WHERE match_id = ?",
                       params = list(match_id))
  vainqueur_id <- if (score_home > score_away) m$home_id else m$away_id
  tranche <- ecart_tranche(abs(score_home - score_away))

  paris <- dbx_get(con, "
    SELECT * FROM bets WHERE match_id = ? AND settled = 0",
    params = list(match_id))

  n_gagnants <- 0
  total_paye <- 0

  if (nrow(paris) > 0) {
    for (i in seq_len(nrow(paris))) {
      b <- paris[i, ]
      gagne <- (b$type == "vainqueur" && b$selection == as.character(vainqueur_id)) ||
               (b$type == "ecart"     && b$selection == tranche)
      gain <- if (gagne) round(b$mise * b$cote) else 0
      dbx_exec(con, "
        UPDATE bets SET settled = 1, gain = ? WHERE bet_id = ?",
        params = list(gain, b$bet_id))
      if (gain > 0) {
        add_transaction(con, b$user_id, gain,
                        sprintf("Gain pari #%d (match #%d)", b$bet_id, match_id))
        n_gagnants <- n_gagnants + 1
        total_paye <- total_paye + gain
      }
    }
  }

  list(n_paris = nrow(paris), n_gagnants = n_gagnants, total_paye = total_paye)
}

# ------------------------------------------------------------------
# Marché "champion" : vainqueur du tournoi + score de la FINALE
# ------------------------------------------------------------------
# Les demi-finales et la finale se jouent au meilleur des 3 manches
# (2 manches gagnantes). On ne parie que sur la FINALE : quelle équipe
# soulève le trophée, et le score de la série (2-0 ou 2-1).
#
# Une simulation Monte-Carlo rejoue les journées restantes, classe les
# équipes (victoires, puis différence de kubbs), retient le top 4 en
# demi-finales (1v4, 2v3, best-of-3) puis la finale (best-of-3). Elle
# fournit, par équipe : proba de qualif en demi (p_top4), proba de titre
# (p_champ) et répartition du score de finale, conditionnelle au titre
# (p_score : plus l'équipe domine, plus le 2-0 est probable).
#
# Plancher moral : toute équipe encore capable, mathématiquement, de se
# hisser en demi-finale conserve au moins 5 % de chances de titre.
# ------------------------------------------------------------------

COTE_MAX_CHAMP        <- 100    # combiné rare : plafond plus haut que 20
POIDS_MARCHE_CHAMPION <- 300    # volume (StatCoins) où marché et Elo pèsent autant
N_SIM_CHAMPION        <- 2000   # tirages Monte-Carlo
PLANCHER_TITRE        <- 0.05   # proba de titre mini si pas éliminée de la demi
N_QUALIFIES           <- 4      # nombre d'équipes qualifiées en demi-finale

# Classement officiel de la poule, selon les règles du tournoi :
#   1) nombre de victoires ; 2) goal average (différence de kubbs) ;
#   3) confrontation directe (l'équipe qui a battu l'autre passe devant).
# Renvoie les team_id (caractère), du mieux classé au moins bon.
classement_officiel <- function(matches) {
  teams  <- as.character(unique(c(matches$home_id, matches$away_id)))
  played <- matches[matches$played == 1, , drop = FALSE]
  wins <- setNames(numeric(length(teams)), teams)
  diff <- setNames(numeric(length(teams)), teams)
  if (nrow(played) > 0) {
    for (i in seq_len(nrow(played))) {
      h <- as.character(played$home_id[i]); a <- as.character(played$away_id[i])
      d <- played$score_home[i] - played$score_away[i]
      diff[h] <- diff[h] + d; diff[a] <- diff[a] - d
      if (d > 0) wins[h] <- wins[h] + 1 else wins[a] <- wins[a] + 1
    }
  }
  # Confrontation directe : +1 si x a battu y, -1 si y a battu x, 0 sinon
  # (jamais joué, ou une victoire chacun). Ne départage sûrement que 2 équipes.
  h2h <- function(x, y) {
    m <- played[(as.character(played$home_id) == x & as.character(played$away_id) == y) |
                (as.character(played$home_id) == y & as.character(played$away_id) == x), , drop = FALSE]
    if (nrow(m) == 0) return(0L)
    vx <- sum((as.character(m$home_id) == x & m$score_home > m$score_away) |
              (as.character(m$away_id) == x & m$score_away > m$score_home))
    as.integer(sign(vx - (nrow(m) - vx)))
  }
  # Pré-tri par victoires puis goal average ; la confrontation directe ne sert
  # qu'à départager les ex æquo stricts (mêmes victoires ET même différence).
  ids <- teams[order(-wins, -diff)]
  n <- length(ids)
  if (n > 1) for (i in 2:n) {
    j <- i
    while (j > 1 &&
           wins[ids[j]] == wins[ids[j - 1]] &&
           diff[ids[j]] == diff[ids[j - 1]] &&
           h2h(ids[j], ids[j - 1]) > 0) {
      tmp <- ids[j - 1]; ids[j - 1] <- ids[j]; ids[j] <- tmp
      j <- j - 1
    }
  }
  ids
}

# Équipes éliminées de la demi-finale (top N_QUALIFIES). `overrides` : vecteur
# nommé (team_id -> 0 = forcé en lice, 1 = forcé éliminé), qui prime sur le
# calcul automatique. Vecteur logique nommé par team_id.
#   - Poule terminée : classement officiel définitif -> hors top 4 = éliminé.
#   - Poule en cours : élimination mathématique conservatrice (même en gagnant
#     tout, au moins N_QUALIFIES équipes ont déjà plus de victoires acquises
#     que le plafond atteignable). Les cas limites se règlent à la main.
equipe_eliminee <- function(matches, overrides = NULL) {
  teams <- as.character(unique(c(matches$home_id, matches$away_id)))
  reste <- matches[matches$played == 0, , drop = FALSE]
  auto  <- setNames(logical(length(teams)), teams)

  if (nrow(reste) == 0) {
    ids       <- classement_officiel(matches)
    qualifies <- ids[seq_len(min(N_QUALIFIES, length(ids)))]
    auto[!(teams %in% qualifies)] <- TRUE
  } else {
    wins <- setNames(numeric(length(teams)), teams)
    played <- matches[matches$played == 1, , drop = FALSE]
    if (nrow(played) > 0) {
      for (i in seq_len(nrow(played))) {
        h <- as.character(played$home_id[i]); a <- as.character(played$away_id[i])
        if (played$score_home[i] > played$score_away[i]) wins[h] <- wins[h] + 1
        else                                             wins[a] <- wins[a] + 1
      }
    }
    rem <- setNames(numeric(length(teams)), teams)
    tb  <- table(c(as.character(reste$home_id), as.character(reste$away_id)))
    rem[names(tb)] <- as.numeric(tb)
    plafond <- wins + rem
    for (t in teams)
      auto[[t]] <- sum(wins[setdiff(teams, t)] > plafond[[t]]) >= N_QUALIFIES
  }

  # Overrides manuels de l'admin (priment sur l'automatique)
  if (!is.null(overrides) && length(overrides) > 0) {
    ov <- overrides[names(overrides) %in% teams]
    for (t in names(ov)) if (!is.na(ov[[t]])) auto[[t]] <- as.logical(ov[[t]])
  }
  auto
}

simulate_tournoi <- function(matches, n_sim = N_SIM_CHAMPION) {
  ratings <- compute_elo(matches)
  teams   <- names(ratings)
  k       <- length(teams)
  rat     <- as.numeric(ratings[teams])          # force par position
  idx     <- setNames(seq_len(k), teams)

  played    <- matches[matches$played == 1, , drop = FALSE]
  remaining <- matches[matches$played == 0, , drop = FALSE]

  # Classement de départ (matchs déjà joués), indexé par position
  base_wins <- numeric(k)
  base_diff <- numeric(k)
  if (nrow(played) > 0) {
    for (i in seq_len(nrow(played))) {
      h <- idx[[as.character(played$home_id[i])]]
      a <- idx[[as.character(played$away_id[i])]]
      d <- played$score_home[i] - played$score_away[i]
      base_diff[h] <- base_diff[h] + d
      base_diff[a] <- base_diff[a] - d
      if (d > 0) base_wins[h] <- base_wins[h] + 1 else base_wins[a] <- base_wins[a] + 1
    }
  }

  # Matchs restants : indices et proba de victoire à domicile (Elo)
  nr <- nrow(remaining)
  rem_h <- if (nr > 0) idx[as.character(remaining$home_id)] else integer(0)
  rem_a <- if (nr > 0) idx[as.character(remaining$away_id)] else integer(0)
  rem_p <- if (nr > 0) prob_elo(rat[rem_h], rat[rem_a]) else numeric(0)

  # Poule terminée : le classement (départages inclus) est figé -> seeding
  # déterministe des demi-finales, cohérent avec l'élimination affichée.
  fixed_ord <- if (nr == 0) unname(idx[classement_officiel(matches)]) else NULL

  # RNG isolé et reproductible : on n'altère pas l'état global
  seed_state <- if (exists(".Random.seed", envir = .GlobalEnv))
    get(".Random.seed", envir = .GlobalEnv) else NULL
  set.seed(20260724L + nrow(played) + nr)
  on.exit(if (!is.null(seed_state))
    assign(".Random.seed", seed_state, envir = .GlobalEnv))

  # Phase finale au meilleur des 3 manches (2 manches gagnantes)
  bo3_winner <- function(x, y) {                 # vainqueur d'une série
    xw <- 0L; yw <- 0L; p <- prob_elo(rat[x], rat[y])
    while (xw < 2L && yw < 2L) if (runif(1) < p) xw <- xw + 1L else yw <- yw + 1L
    if (xw == 2L) x else y
  }
  bo3_finale <- function(x, y) {                 # vainqueur + score de série
    xw <- 0L; yw <- 0L; p <- prob_elo(rat[x], rat[y])
    while (xw < 2L && yw < 2L) if (runif(1) < p) xw <- xw + 1L else yw <- yw + 1L
    if (xw == 2L) list(w = x, s = if (yw == 0L) "2-0" else "2-1")
    else          list(w = y, s = if (xw == 0L) "2-0" else "2-1")
  }

  champ <- numeric(k)
  top4  <- numeric(k)
  sc20  <- numeric(k)   # titres remportés 2-0, par équipe
  sc21  <- numeric(k)   # titres remportés 2-1, par équipe
  for (s in seq_len(n_sim)) {
    wins <- base_wins
    diff <- base_diff
    if (nr > 0) {
      hw  <- runif(nr) < rem_p                    # domicile gagne ?
      los <- sample.int(6, nr, replace = TRUE) - 1  # score du perdant (0-5)
      for (i in seq_len(nr)) {
        h <- rem_h[i]; a <- rem_a[i]; m <- 6 - los[i]
        if (hw[i]) { wins[h] <- wins[h] + 1; diff[h] <- diff[h] + m; diff[a] <- diff[a] - m }
        else       { wins[a] <- wins[a] + 1; diff[a] <- diff[a] + m; diff[h] <- diff[h] - m }
      }
    }
    ord <- if (nr == 0) fixed_ord                 # poule finie : ordre officiel
           else order(-wins, -diff, runif(k))     # sinon départage résiduel
    top4[ord[1:4]] <- top4[ord[1:4]] + 1
    f1  <- bo3_winner(ord[1], ord[4])             # demi-finale 1v4
    f2  <- bo3_winner(ord[2], ord[3])             # demi-finale 2v3
    fin <- bo3_finale(f1, f2)                     # finale
    champ[fin$w] <- champ[fin$w] + 1
    if (fin$s == "2-0") sc20[fin$w] <- sc20[fin$w] + 1 else sc21[fin$w] <- sc21[fin$w] + 1
  }

  # Répartition du score de finale, conditionnelle au titre (fallback marginal)
  m20 <- sum(sc20); m21 <- sum(sc21); mtot <- m20 + m21
  marg <- if (mtot > 0) c("2-0" = m20 / mtot, "2-1" = m21 / mtot)
          else          c("2-0" = 0.5, "2-1" = 0.5)
  p_score <- setNames(lapply(seq_len(k), function(i) {
    tot <- sc20[i] + sc21[i]
    if (tot >= 20) c("2-0" = sc20[i] / tot, "2-1" = sc21[i] / tot) else marg
  }), teams)

  list(p_champ = setNames(champ / n_sim, teams),
       p_top4  = setNames(top4  / n_sim, teams),
       p_score = p_score)
}

# Ajuste les probas d'affichage : plancher de titre pour les équipes encore
# en lice, et cohérence P(demi) >= P(titre). `elimine` = logique nommé.
ajuste_pronostic <- function(p_titre, p_qualif, elimine, plancher = PLANCHER_TITRE) {
  tid    <- names(p_titre)
  vivant <- !elimine[tid]
  titre  <- p_titre
  titre[vivant]  <- pmax(p_titre[vivant], plancher)
  qualif <- p_qualif[tid]
  qualif[vivant] <- pmax(qualif[vivant], titre[vivant])
  list(titre = setNames(titre, tid), qualif = setNames(qualif, tid))
}

# Pronostic par équipe (sans marché) : proba de demi et de titre, planchées.
pronostic_equipes <- function(matches, sim = NULL, overrides = NULL) {
  if (is.null(sim)) sim <- simulate_tournoi(matches)
  el  <- equipe_eliminee(matches, overrides)
  aj  <- ajuste_pronostic(sim$p_champ, sim$p_top4, el)
  tid <- names(sim$p_champ)
  list(team_id = tid, qualif = aj$qualif[tid], titre = aj$titre[tid],
       elimine = el[tid])
}

# Cotes du marché champion : p_team (titre, marché + plancher), p_top4
# (qualif), p_score (répartition 2-0/2-1 par équipe) et elimine. `sim` est
# fourni par l'appelant pour être mis en cache (recalcul coûteux).
cotes_champion <- function(con, matches = NULL, sim = NULL) {
  if (is.null(matches)) matches <- get_matches(con)
  if (is.null(sim))     sim <- simulate_tournoi(matches)
  teams_id <- names(sim$p_champ)
  el       <- equipe_eliminee(matches, get_elim_overrides(con))

  flux <- dbx_get(con, "
    SELECT team_id, SUM(mise) AS total FROM champion_bets GROUP BY team_id")
  mises <- setNames(rep(0, length(teams_id)), teams_id)
  if (nrow(flux) > 0) mises[as.character(flux$team_id)] <- flux$total
  vol <- sum(mises)
  k   <- length(teams_id)
  p_mkt <- (mises + 1) / (vol + k)                  # lissage de Laplace
  w     <- vol / (vol + POIDS_MARCHE_CHAMPION)       # poids du marché
  p_team <- (1 - w) * sim$p_champ[teams_id] + w * p_mkt[teams_id]

  # Plancher moral (5 %) + cohérence qualif >= titre
  aj <- ajuste_pronostic(p_team, sim$p_top4, el)

  list(p_team = aj$titre, p_top4 = aj$qualif, p_score = sim$p_score,
       elimine = el[teams_id])
}

# Équipes encore en lice : celles qui ne sont pas mathématiquement éliminées
# de la demi-finale (sécurité : ne jamais renvoyer une liste vide).
equipes_en_lice <- function(cc) {
  el    <- cc$elimine
  alive <- names(el)[!el]
  if (length(alive) == 0) alive <- names(el)
  alive
}

# Cote d'un pari champion précis (équipe + score de finale). Indexation
# simple ([]) : renvoie NA plutôt qu'une erreur si une clé est absente.
cote_champion <- function(cc, team_id, score) {
  pt <- unname(cc$p_team[as.character(team_id)])
  ps <- unname(cc$p_score[[as.character(team_id)]][as.character(score)])
  p  <- pt * ps
  if (length(p) == 0 || is.na(p) || p <= 0) return(NA_real_)
  round(pmax(COTE_MIN, pmin(COTE_MAX_CHAMP, MARGE / p)), 2)
}

# Règlement du marché champion : score = score de la série de finale (2-0/2-1).
settle_champion <- function(con, team_id, score) {
  paris <- dbx_get(con, "SELECT * FROM champion_bets WHERE settled = 0")
  n_gagnants <- 0
  total_paye <- 0
  if (nrow(paris) > 0) {
    for (i in seq_len(nrow(paris))) {
      b <- paris[i, ]
      gagne <- (b$team_id == team_id) && (b$score == score)
      gain  <- if (gagne) round(b$mise * b$cote) else 0
      dbx_exec(con, "
        UPDATE champion_bets SET settled = 1, gain = ? WHERE bet_id = ?",
        params = list(gain, b$bet_id))
      if (gain > 0) {
        add_transaction(con, b$user_id, gain,
                        sprintf("Gain pari champion #%d", b$bet_id))
        n_gagnants <- n_gagnants + 1
        total_paye <- total_paye + gain
      }
    }
  }
  dbx_exec(con, "
    UPDATE champion_result SET team_id = ?, score = ?, settled = 1 WHERE id = 1",
    params = list(team_id, score))
  list(n_paris = nrow(paris), n_gagnants = n_gagnants, total_paye = total_paye)
}

cotes_tous <- function(con, matches) {
  ratings <- compute_elo(matches)
  flux <- dbx_get(con, "
    SELECT match_id, type, selection, SUM(mise) AS total
    FROM bets GROUP BY match_id, type, selection")
  
  played <- matches[matches$played == 1, , drop = FALSE]
  effectifs <- ECART_PRIOR
  if (nrow(played) > 0) {
    obs <- table(ecart_tranche(abs(played$score_home - played$score_away)))
    for (tranche in names(obs)) effectifs[tranche] <- effectifs[tranche] + obs[[tranche]]
  }
  p_hist <- effectifs / sum(effectifs)
  
  res <- lapply(seq_len(nrow(matches)), function(i) {
    m <- matches[i, ]
    f <- flux[flux$match_id == m$match_id, , drop = FALSE]
    
    fv <- f[f$type == "vainqueur", , drop = FALSE]
    mise_h <- sum(fv$total[fv$selection == as.character(m$home_id)])
    mise_a <- sum(fv$total[fv$selection == as.character(m$away_id)])
    vol <- mise_h + mise_a
    w <- vol / (vol + POIDS_MARCHE_VAINQUEUR)
    p_elo_h <- prob_elo(ratings[as.character(m$home_id)],
                        ratings[as.character(m$away_id)])
    p_h <- pmin(pmax((1 - w) * p_elo_h + w * (mise_h + 1) / (vol + 2), 0.05), 0.95)
    
    fe <- f[f$type == "ecart", , drop = FALSE]
    mises <- setNames(rep(0, 3), ECART_TRANCHES)
    if (nrow(fe) > 0) mises[fe$selection] <- fe$total
    vole <- sum(mises)
    we <- vole / (vole + POIDS_MARCHE_ECART)
    pe <- pmin(pmax((1 - we) * p_hist[ECART_TRANCHES] +
                      we * (mises + 1)[ECART_TRANCHES] / (vole + 3), 0.03), 0.95)
    
    list(vainqueur = c(home = unname(clamp_cote(p_h)),
                       away = unname(clamp_cote(1 - p_h))),
         ecart = setNames(clamp_cote(pe), ECART_TRANCHES))
  })
  setNames(res, matches$match_id)
}
