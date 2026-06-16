# ============ Boîte à outils des visuels (réseaux sociaux) ============

STAT_PAL    <- c("#2C6E9B", "#E08E45", "#3E8E7E", "#C0392B",
                 "#8E6C8A", "#D4A015", "#5A6F52", "#B5651D")
STAT_ACCENT <- "#2C6E9B"
STAT_VERT   <- "#3E8E7E"
STAT_ORANGE <- "#E08E45"

theme_stat <- function(base = 14) {
  ggplot2::theme_minimal(base_size = base) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", size = base * 1.55),
      plot.subtitle    = ggplot2::element_text(color = "grey35", size = base * 1.05,
                                               margin = ggplot2::margin(b = 8)),
      plot.caption     = ggplot2::element_text(color = "grey55", size = base * 0.72,
                                               margin = ggplot2::margin(t = 10)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      panel.grid.minor = ggplot2::element_blank(),
      axis.title       = ggplot2::element_text(color = "grey30"),
      legend.position  = "top",
      plot.margin      = ggplot2::margin(18, 20, 14, 18)
    )
}

stat_caption <- function() {
  ggplot2::labs(caption = sprintf("Tournoi Kubb · StatCoins  —  %s",
                                  format(Sys.Date(), "%d/%m/%Y")))
}

stat_vide <- function(msg = "Pas encore de données / Nog geen gegevens") {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = msg, size = 6, color = "grey55") +
    ggplot2::theme_void()
}

# ---- Builders : chacun reçoit d (les données) et topn ----

viz_recap <- function(d, topn = 8) {
  b <- d$bets; u <- d$users
  if (nrow(b) == 0 && nrow(u) == 0) return(stat_vide())
  fmt  <- function(x) format(round(x), big.mark = " ")
  gros <- b[b$settled == 1 & !is.na(b$gain) & b$gain > 0, , drop = FALSE]
  tiles <- data.frame(
    col = c(0, 1, 2, 0, 1, 2),
    row = c(1, 1, 1, 0, 0, 0),
    val = c(
      fmt(nrow(b)),
      fmt(sum(b$mise)),
      if (nrow(u)) fmt(max(u$statcoins)) else "0",
      if (nrow(gros)) fmt(max(gros$gain)) else "0",
      if (nrow(b)) sprintf("%.2f", max(b$cote)) else "0",
      fmt(nrow(u))
    ),
    lab = c(
      "Paris placés\nGeplaatste wedden.",
      "Total misé\nTotaal ingezet (SC)",
      "Meilleur solde\nBeste saldo (SC)",
      "Plus gros gain\nGrootste winst (SC)",
      "Cote max jouée\nHoogste quotering",
      "Parieur·euses\nWedders"
    ),
    stringsAsFactors = FALSE
  )
  ggplot2::ggplot(tiles, ggplot2::aes(col, row)) +
    ggplot2::geom_tile(width = 0.94, height = 0.94, fill = STAT_ACCENT, alpha = 0.10) +
    ggplot2::geom_text(ggplot2::aes(label = val), vjust = -0.15, size = 11,
                       fontface = "bold", color = STAT_ACCENT) +
    ggplot2::geom_text(ggplot2::aes(label = lab), vjust = 2.2, size = 3.7,
                       color = "grey30", lineheight = 0.95) +
    ggplot2::coord_equal(clip = "off") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = 0.45)) +
    ggplot2::labs(title = "Le tournoi en chiffres", subtitle = "Het toernooi in cijfers") +
    stat_caption() +
    ggplot2::theme_void(base_size = 14) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 26, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(color = "grey35", hjust = 0.5,
                                            margin = ggplot2::margin(b = 14)),
      plot.caption  = ggplot2::element_text(color = "grey55", size = 10, hjust = 0.5),
      plot.margin   = ggplot2::margin(18, 18, 14, 18)
    )
}

viz_classement <- function(d, topn = 10) {
  u <- d$users
  if (nrow(u) == 0) return(stat_vide())
  u <- u[order(-u$statcoins), , drop = FALSE]
  u <- utils::head(u, topn)
  u$pseudo <- factor(u$pseudo, levels = rev(u$pseudo))
  ggplot2::ggplot(u, ggplot2::aes(statcoins, pseudo)) +
    ggplot2::geom_col(fill = STAT_ACCENT, width = 0.72) +
    ggplot2::geom_text(ggplot2::aes(label = round(statcoins)), hjust = -0.15,
                       size = 4.2, color = "grey20") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.13))) +
    ggplot2::labs(title = "Classement des parieur·euses",
                  subtitle = "Klassement van de wedders — solde / saldo (StatCoins)",
                  x = "StatCoins", y = NULL) +
    stat_caption() + theme_stat()
}

viz_evolution <- function(d, topn = 6) {
  tx <- d$tx
  if (nrow(tx) == 0) return(stat_vide())
  tx <- tx |>
    dplyr::group_by(user_id, pseudo) |>
    dplyr::arrange(ts, .by_group = TRUE) |>
    dplyr::mutate(solde = cumsum(montant)) |>
    dplyr::ungroup()
  fin <- tx |>
    dplyr::group_by(pseudo) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(solde)) |>
    utils::head(topn)
  tx <- tx[tx$pseudo %in% fin$pseudo, , drop = FALSE]
  ggplot2::ggplot(tx, ggplot2::aes(ts, solde, color = pseudo)) +
    ggplot2::geom_step(linewidth = 1) +
    ggplot2::scale_color_manual(values = STAT_PAL, name = NULL) +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = " ")) +
    ggplot2::labs(title = "Évolution des soldes",
                  subtitle = "Evolutie van de saldo's (StatCoins)",
                  x = "Date / Datum", y = "Solde / Saldo") +
    stat_caption() + theme_stat()
}

viz_gros_gains <- function(d, topn = 8) {
  b <- d$bets
  b <- b[b$settled == 1 & !is.na(b$gain) & b$gain > 0, , drop = FALSE]
  if (nrow(b) == 0) return(stat_vide("Aucun gain réglé / Nog geen winst"))
  b <- b[order(-b$gain), , drop = FALSE]
  b <- b[seq_len(min(topn, nrow(b))), , drop = FALSE]
  b$lbl <- factor(paste0(b$pseudo, " · ", b$home, "–", b$away),
                  levels = rev(paste0(b$pseudo, " · ", b$home, "–", b$away)))
  ggplot2::ggplot(b, ggplot2::aes(gain, lbl)) +
    ggplot2::geom_col(fill = STAT_VERT, width = 0.72) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("+%d  @%.2f", round(gain), cote)),
                       hjust = -0.05, size = 3.8, color = "grey20") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.20))) +
    ggplot2::labs(title = "Les plus gros coups",
                  subtitle = "De grootste klappers — gain / winst (StatCoins)",
                  x = "Gain / Winst", y = NULL) +
    stat_caption() + theme_stat()
}

viz_meilleur_journee <- function(d, topn = 8) {
  b <- d$bets
  b <- b[b$settled == 1 & !is.na(b$gain) & b$gain > 0, , drop = FALSE]
  if (nrow(b) == 0) return(stat_vide("Aucun gain réglé / Nog geen winst"))
  best <- b |>
    dplyr::group_by(journee) |>
    dplyr::slice_max(gain, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
  ggplot2::ggplot(best, ggplot2::aes(factor(journee), gain)) +
    ggplot2::geom_col(fill = STAT_ACCENT, width = 0.72) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(pseudo, "\n+", round(gain))),
                       vjust = -0.3, size = 3.6, color = "grey20", lineheight = 0.9) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.20))) +
    ggplot2::labs(title = "Meilleur pari par journée",
                  subtitle = "Beste weddenschap per speeldag — gain / winst (StatCoins)",
                  x = "Journée / Speeldag", y = "Gain / Winst") +
    stat_caption() + theme_stat()
}

viz_mises_journee <- function(d, topn = 8) {
  b <- d$bets
  if (nrow(b) == 0) return(stat_vide())
  agg <- b |>
    dplyr::group_by(journee, type) |>
    dplyr::summarise(total = sum(mise), .groups = "drop")
  agg$type_lbl <- ifelse(agg$type == "vainqueur",
                         "Vainqueur / Winnaar", "Écart / Verschil")
  ggplot2::ggplot(agg, ggplot2::aes(factor(journee), total, fill = type_lbl)) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::scale_fill_manual(
      values = c("Vainqueur / Winnaar" = STAT_ACCENT, "Écart / Verschil" = STAT_ORANGE),
      name = NULL) +
    ggplot2::labs(title = "Mises par journée",
                  subtitle = "Inzetten per speeldag (StatCoins)",
                  x = "Journée / Speeldag", y = "Total misé / Totaal ingezet") +
    stat_caption() + theme_stat()
}

viz_types <- function(d, topn = 8) {
  b <- d$bets
  if (nrow(b) == 0) return(stat_vide())
  agg <- dplyr::count(b, type, wt = mise, name = "vol")
  agg$type_lbl <- ifelse(agg$type == "vainqueur",
                         "Vainqueur / Winnaar", "Écart / Verschil")
  agg$part <- agg$vol / sum(agg$vol)
  ggplot2::ggplot(agg, ggplot2::aes(x = "", y = part, fill = type_lbl)) +
    ggplot2::geom_col(width = 1, color = "white") +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(part, accuracy = 1)),
                       position = ggplot2::position_stack(vjust = 0.5),
                       color = "white", size = 6, fontface = "bold") +
    ggplot2::scale_fill_manual(
      values = c("Vainqueur / Winnaar" = STAT_ACCENT, "Écart / Verschil" = STAT_ORANGE),
      name = NULL) +
    ggplot2::labs(title = "Répartition des mises par type",
                  subtitle = "Verdeling van de inzetten per type") +
    stat_caption() +
    ggplot2::theme_void(base_size = 14) +
    ggplot2::theme(
      legend.position = "top",
      plot.title    = ggplot2::element_text(face = "bold", size = 22, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(color = "grey35", hjust = 0.5,
                                            margin = ggplot2::margin(b = 8)),
      plot.caption  = ggplot2::element_text(color = "grey55", size = 10, hjust = 0.5),
      plot.margin   = ggplot2::margin(18, 18, 14, 18)
    )
}

viz_cotes <- function(d, topn = 8) {
  b <- d$bets
  if (nrow(b) == 0) return(stat_vide())
  moy <- mean(b$cote)
  ggplot2::ggplot(b, ggplot2::aes(cote)) +
    ggplot2::geom_histogram(binwidth = 0.25, boundary = 1,
                            fill = STAT_ACCENT, color = "white") +
    ggplot2::geom_vline(xintercept = moy, linetype = "dashed",
                        color = STAT_ORANGE, linewidth = 0.9) +
    ggplot2::annotate("text", x = moy, y = Inf, vjust = 1.6, hjust = -0.05,
                      label = sprintf("moy. / gem. %.2f", moy),
                      color = STAT_ORANGE, size = 4.2) +
    ggplot2::labs(title = "Distribution des cotes jouées",
                  subtitle = "Verdeling van de gespeelde quoteringen",
                  x = "Cote / Quotering", y = "Nombre de paris / Aantal") +
    stat_caption() + theme_stat()
}

viz_equipes <- function(d, topn = 8) {
  b <- d$bets
  bv <- b[b$type == "vainqueur", , drop = FALSE]
  if (nrow(bv) == 0) return(stat_vide())
  bv$team_id <- suppressWarnings(as.integer(bv$selection))
  bv <- dplyr::left_join(bv, d$teams[, c("team_id", "nom")], by = "team_id")
  agg <- bv |>
    dplyr::group_by(nom) |>
    dplyr::summarise(vol = sum(mise), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(vol))
  agg$nom <- factor(agg$nom, levels = rev(agg$nom))
  ggplot2::ggplot(agg, ggplot2::aes(vol, nom)) +
    ggplot2::geom_col(fill = "#8E6C8A", width = 0.72) +
    ggplot2::geom_text(ggplot2::aes(label = round(vol)), hjust = -0.15,
                       size = 4, color = "grey20") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.13))) +
    ggplot2::labs(title = "Équipes les plus soutenues",
                  subtitle = "Meest gesteunde teams — mises sur la victoire / inzetten op winst",
                  x = "Total misé / Totaal ingezet", y = NULL) +
    stat_caption() + theme_stat()
}

# Registre : clé interne -> libellé bilingue + builder
STAT_VIZ <- list(
  recap      = list(label = "Récap / Overzicht",                         f = viz_recap),
  classement = list(label = "Classement / Klassement",                   f = viz_classement),
  evolution  = list(label = "Évolution des soldes / Evolutie saldo's",   f = viz_evolution),
  gros_gains = list(label = "Plus gros coups / Grootste klappers",       f = viz_gros_gains),
  meilleur_j = list(label = "Meilleur pari par journée / Beste per speeldag", f = viz_meilleur_journee),
  mises_j    = list(label = "Mises par journée / Inzetten per speeldag",  f = viz_mises_journee),
  types      = list(label = "Répartition par type / Verdeling per type",  f = viz_types),
  cotes      = list(label = "Distribution des cotes / Verdeling quoteringen", f = viz_cotes),
  equipes    = list(label = "Équipes soutenues / Gesteunde teams",        f = viz_equipes)
)

STAT_FORMATS <- list(
  carre   = list(w = 1080, h = 1080),
  paysage = list(w = 1200, h = 675),
  story   = list(w = 1080, h = 1350)
)