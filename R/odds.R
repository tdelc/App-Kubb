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
