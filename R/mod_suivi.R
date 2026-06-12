# ------------------------------------------------------------------
# mod_suivi.R — Résultats, classements et graphiques
# ------------------------------------------------------------------

mod_suivi_ui <- function(id, i18n) {
  ns <- NS(id)
  tagList(
    layout_column_wrap(
      width = 1 / 3,
      fill = FALSE,
      value_box(
        title = i18n$t("Équipe en tête"),
        value = textOutput(ns("vb_leader")),
        showcase = bsicons::bs_icon("trophy"),
        theme = "primary"
      ),
      value_box(
        title = i18n$t("Meilleur·e parieur·euse"),
        value = textOutput(ns("vb_parieur")),
        showcase = bsicons::bs_icon("piggy-bank"),
        theme = "warning"
      ),
      value_box(
        title = i18n$t("Matchs joués"),
        value = textOutput(ns("vb_matchs")),
        showcase = bsicons::bs_icon("calendar-check"),
        theme = "secondary"
      )
    ),
    navset_card_tab(
      nav_panel(
        i18n$t("Résultats"),
        DT::DTOutput(ns("tbl_resultats"))
      ),
      nav_panel(
        i18n$t("Classement des équipes"),
        plotly::plotlyOutput(ns("plt_equipes"), height = "350px"),
        DT::DTOutput(ns("tbl_equipes"))
      ),
      nav_panel(
        i18n$t("Classement des parieur·euses"),
        plotly::plotlyOutput(ns("plt_parieurs"), height = "600px"),
        DT::DTOutput(ns("tbl_parieurs"))
      ),
      nav_panel(
        i18n$t("Évolution des StatCoins"),
        plotly::plotlyOutput(ns("plt_evolution"), height = "420px")
      )
    )
  )
}

mod_suivi_server <- function(id, con, db_ver, i18n_s, lang) {
  moduleServer(id, function(input, output, session) {
    tr <- function(x) i18n_s$t(x)

    matchs <- reactive({
      db_ver()
      get_matches(con)
    })

    joues <- reactive({
      dplyr::filter(matchs(), played == 1)
    })

    # ---------------- Classement des équipes ----------------
    classement <- reactive({
      m <- joues()
      equipes <- get_teams(con)

      if (nrow(m) == 0) {
        return(dplyr::tibble(
          equipe = equipes$nom, joues = 0L, victoires = 0L, defaites = 0L,
          kubbs_pour = 0L, kubbs_contre = 0L, difference = 0L
        ))
      }

      dplyr::bind_rows(
        dplyr::transmute(m, equipe = home, pour = score_home, contre = score_away),
        dplyr::transmute(m, equipe = away, pour = score_away, contre = score_home)
      ) |>
        dplyr::group_by(equipe) |>
        dplyr::summarise(
          joues        = dplyr::n(),
          victoires    = sum(pour > contre),
          defaites     = sum(pour < contre),
          kubbs_pour   = sum(pour),
          kubbs_contre = sum(contre),
          .groups = "drop"
        ) |>
        dplyr::full_join(dplyr::tibble(equipe = equipes$nom), by = "equipe") |>
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) tidyr::replace_na(x, 0))) |>
        dplyr::mutate(difference = kubbs_pour - kubbs_contre) |>
        dplyr::arrange(dplyr::desc(victoires), dplyr::desc(difference), equipe)
    })

    # ---------------- Classement des parieur·euses ----------------
    parieurs <- reactive({
      db_ver()
      dbx_get(con, "
        SELECT u.pseudo,
               u.statcoins,
               COUNT(b.bet_id)                            AS n_paris,
               COALESCE(SUM(CASE WHEN b.settled = 1 AND b.gain > 0 THEN 1 ELSE 0 END), 0) AS n_gagnes,
               COALESCE(SUM(CASE WHEN b.settled = 0 THEN b.mise ELSE 0 END), 0) AS en_jeu,
               COALESCE(SUM(CASE WHEN b.settled = 1 THEN b.gain ELSE 0 END), 0)
                                                          AS gains_totaux
        FROM users u
        LEFT JOIN bets b ON b.user_id = u.user_id
        GROUP BY u.user_id
        ORDER BY u.statcoins DESC, u.pseudo")
    })

    # ---------------- Value boxes ----------------
    output$vb_leader <- renderText({
      cl <- classement()
      if (sum(cl$joues) == 0) tr("Tournoi à venir") else cl$equipe[1]
    })

    output$vb_parieur <- renderText({
      p <- parieurs()
      if (nrow(p) == 0) tr("Personne pour l'instant")
      else sprintf("%s (%d SC)", p$pseudo[1], round(p$statcoins[1]))
    })

    output$vb_matchs <- renderText({
      sprintf("%d / %d", nrow(joues()), nrow(matchs()))
    })

    # ---------------- Résultats ----------------
    output$tbl_resultats <- DT::renderDT({
      lang()
      m <- joues()
      if (nrow(m) == 0) {
        return(DT::datatable(
          data.frame(x = tr("Aucun match joué pour le moment.")),
          rownames = FALSE, colnames = "", options = list(dom = "t")))
      }
      m$score <- sprintf("%d – %d", m$score_home, m$score_away)
      DT::datatable(
        m[order(-m$journee, m$date_match), c("journee", "date_match", "home", "score", "away")],
        colnames = c(tr("Journée"), tr("Date"), tr("Équipe 1"),
                     tr("Score"), tr("Équipe 2")),
        rownames = FALSE, selection="none",
        options = list(pageLength = 10, dom = "tip")
      )
    })

    # ---------------- Graphiques et tables ----------------
    output$plt_equipes <- plotly::renderPlotly({
      lang()
      cl <- classement()
      plotly::plot_ly(
        cl,
        x = ~victoires,
        y = ~stats::reorder(equipe, victoires),
        type = "bar", orientation = "h",
        marker = list(color = "#E8743B"),
        hovertemplate = paste0("%{y}<br>", tr("Victoires"), " : %{x}<extra></extra>")
      ) |>
        plotly::layout(
          xaxis = list(title = tr("Victoires"), dtick = 1),
          yaxis = list(title = ""),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        )
    })

    output$tbl_equipes <- DT::renderDT({
      lang()
      DT::datatable(
        classement(),
        colnames = c(tr("Équipe"), tr("Joués"), tr("Victoires"), tr("Défaites"),
                     tr("Kubbs pour"), tr("Kubbs contre"), tr("Différence")),
        rownames = FALSE, selection="none",
        options = list(pageLength = 8, dom = "t")
      )
    })

    output$plt_parieurs <- plotly::renderPlotly({
      lang()
      p <- parieurs()
      validate(need(nrow(p) > 0, tr("Personne pour l'instant")))
      
      p <- p |>
        dplyr::mutate(
          delta   = statcoins - CREDIT_INITIAL,
          couleur = dplyr::if_else(delta >= 0, "#2A9D8F", "#C44536"),
          rang    = dplyr::row_number(dplyr::desc(statcoins)),
          label   = dplyr::case_when(
            rang == 1 ~ paste0("\U0001F947 ", pseudo),   # 🥇
            rang == 2 ~ paste0("\U0001F948 ", pseudo),   # 🥈
            rang == 3 ~ paste0("\U0001F949 ", pseudo),   # 🥉
            TRUE      ~ pseudo
          )
        ) |>
        dplyr::arrange(statcoins)   # le/la meilleur·e en haut
      
      # Graduations exprimées en solde réel, pas en écart
      amp   <- max(abs(p$delta), 50)
      ticks <- pretty(c(-amp, amp))
      
      plotly::plot_ly(
        p,
        x = ~delta,
        y = ~factor(label, levels = label),
        type = "bar", orientation = "h",
        marker = list(color = ~couleur,
                      line = list(color = "rgba(59,44,32,0.25)", width = 1)),
        hovertemplate = paste0(
          "<b>%{y}</b><br>",
          "%{x:+,d} ", tr("vs crédit initial"), "<br>",
          "<extra></extra>"
        ),
        text = ~paste0(round(statcoins), " SC"),
        textposition = "outside",
        textfont = list(color = "#3B2C20", family = "Nunito"),
        cliponaxis = FALSE
      ) |>
        plotly::layout(
          xaxis = list(
            title = "StatCoins",
            tickvals = ticks,
            ticktext = ticks + CREDIT_INITIAL,   # l'axe affiche 800, 1000, 1200...
            range = c(min(ticks) * 1.15, max(ticks) * 1.15),
            zeroline = FALSE
          ),
          yaxis = list(title = ""),
          shapes = list(list(
            type = "line", x0 = 0, x1 = 0, y0 = -0.5, y1 = nrow(p) - 0.5,
            line = list(color = "#3B2C20", width = 1.5, dash = "dot")
          )),
          annotations = list(list(
            x = 0, y = 1.06, xref = "x", yref = "paper",
            text = paste0(tr("Crédit initial"), " (", CREDIT_INITIAL, " SC)"),
            showarrow = FALSE, font = list(size = 11, color = "#3B2C20")
          )),
          bargap = 0.35,
          margin = list(r = 70),
          font = list(family = "Nunito"),
          hoverlabel = list(bgcolor = "#FFFBF2", bordercolor = "#3B2C20",
                            font = list(family = "Nunito", color = "#3B2C20")),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        )
    })

    output$tbl_parieurs <- DT::renderDT({
      lang()
      p <- parieurs()
      p$statcoins <- round(p$statcoins)
      DT::datatable(
        p,
        colnames = c(tr("Pseudo"), "StatCoins", tr("Paris placés"),
                     tr("Paris gagnés"), tr("Paris perdus"), tr("Gains totaux")),
        rownames = FALSE, selection="none",
        options = list(pageLength = 10, dom = "tip")
      )
    })

    output$plt_evolution <- plotly::renderPlotly({
      lang()
      db_ver()
      tx <- get_transactions(con)
      validate(need(nrow(tx) > 0, tr("Personne pour l'instant")))

      evo <- tx |>
        dplyr::mutate(ts = as.POSIXct(ts)) |>
        dplyr::arrange(ts, tx_id) |>
        dplyr::group_by(pseudo) |>
        dplyr::mutate(solde = cumsum(montant)) |>
        dplyr::ungroup()

      plotly::plot_ly(
        evo,
        x = ~ts, y = ~solde, color = ~pseudo,
        type = "scatter", mode = "lines+markers",
        line = list(shape = "hv")
      ) |>
        plotly::layout(
          xaxis = list(title = ""),
          yaxis = list(title = "StatCoins"),
          legend = list(orientation = "h", y = -0.15),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        )
    })
  })
}
