# ------------------------------------------------------------------
# mod_admin.R — Administration (scores, StatCoins, supervision)
# ------------------------------------------------------------------

mod_admin_ui <- function(id, i18n) {
  ns <- NS(id)
  uiOutput(ns("ui"))
}

mod_admin_server <- function(id, con, user, db_ver, touch, i18n_s, lang) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tr <- function(x) i18n_s$t(x)

    est_admin <- reactive({
      u <- user()
      !is.null(u) &&
        (u$is_admin == 1 || tolower(u$pseudo) %in% tolower(ADMIN_PSEUDOS))
    })

    matchs <- reactive({
      db_ver()
      get_matches(con)
    })

    # ---------------- UI ----------------
    output$ui <- renderUI({
      lang()
      if (!est_admin()) {
        return(card(card_body(tr("Accès réservé à l'administration."))))
      }

      m <- matchs()
      non_joues <- m[m$played == 0, , drop = FALSE]
      choix_matchs <- setNames(
        non_joues$match_id,
        sprintf("J%d — %s vs %s (%s)", non_joues$journee,
                non_joues$home, non_joues$away, non_joues$date_match)
      )

      users <- DBI::dbGetQuery(con, "SELECT user_id, pseudo FROM users ORDER BY pseudo")
      choix_users <- setNames(users$user_id, users$pseudo)

      tagList(
        layout_column_wrap(
          width = 1 / 2,
          fill = FALSE,

          card(
            card_header(tagList(bsicons::bs_icon("pencil-square"),
                                tr("Saisir un résultat"))),
            card_body(
              selectInput(ns("score_match"), tr("Match"), choices = choix_matchs),
              uiOutput(ns("score_inputs")),
              actionButton(ns("btn_score"), tr("Valider le résultat"),
                           class = "btn-danger"),
              p(class = "text-muted small mt-2",
                tr("La validation règle définitivement tous les paris du match."))
            )
          ),

          card(
            card_header(tagList(bsicons::bs_icon("coin"),
                                tr("Ajuster les StatCoins"))),
            card_body(
              selectInput(ns("adj_user"), tr("Parieur·euse"), choices = choix_users),
              numericInput(ns("adj_montant"), tr("Montant (négatif pour retirer)"),
                           value = 100, step = 10),
              textInput(ns("adj_motif"), tr("Motif"),
                        placeholder = tr("Bonus de bonne humeur")),
              actionButton(ns("btn_adj"), tr("Appliquer"), class = "btn-warning")
            )
          ),

          card(
            card_header(tagList(bsicons::bs_icon("calendar-event"),
                                tr("Reprogrammer un match"))),
            card_body(
              selectInput(ns("date_match_sel"), tr("Match"), choices = choix_matchs),
              dateInput(ns("nouvelle_date"), tr("Nouvelle date"), value = Sys.Date()),
              textInput(ns("nouvelle_heure"), tr("Heure (HH:MM)"), value = "14:00"),
              actionButton(ns("btn_date"), tr("Reprogrammer"),
                           class = "btn-secondary")
            )
          )
        ),

        navset_card_tab(
          nav_panel(tr("Parieur·euses"), DT::DTOutput(ns("tbl_users"))),
          nav_panel(tr("Tous les paris"), DT::DTOutput(ns("tbl_bets")))
        )
      )
    })

    # Libellés des champs de score selon le match sélectionné
    output$score_inputs <- renderUI({
      lang()
      req(input$score_match)
      m <- matchs()
      m <- m[m$match_id == as.integer(input$score_match), , drop = FALSE]
      req(nrow(m) == 1)
      tagList(
        numericInput(ns("score_home"), sprintf("%s — %s", tr("Score"), m$home),
                     value = 0, min = 0, step = 1),
        numericInput(ns("score_away"), sprintf("%s — %s", tr("Score"), m$away),
                     value = 0, min = 0, step = 1)
      )
    })

    # ---------------- Validation d'un score (avec confirmation) ----------------
    observeEvent(input$btn_score, {
      req(est_admin(), input$score_match)
      sh <- suppressWarnings(as.integer(input$score_home))
      sa <- suppressWarnings(as.integer(input$score_away))

      if (is.na(sh) || is.na(sa) || sh < 0 || sa < 0) {
        showNotification(tr("Scores invalides."), type = "error")
        return()
      }
      if (sh == sa) {
        showNotification(tr("Pas de match nul au Kubb : départagez-les !"),
                         type = "error")
        return()
      }

      m <- matchs()
      m <- m[m$match_id == as.integer(input$score_match), , drop = FALSE]
      showModal(modalDialog(
        title = tr("Confirmer le résultat"),
        sprintf("%s vs %s : %d – %d", m$home, m$away, sh, sa),
        footer = tagList(
          modalButton(tr("Annuler")),
          actionButton(ns("btn_score_ok"), tr("Confirmer"), class = "btn-danger")
        )
      ))
    })

    observeEvent(input$btn_score_ok, {
      req(est_admin(), input$score_match)
      removeModal()
      res <- settle_match(con,
                          as.integer(input$score_match),
                          as.integer(input$score_home),
                          as.integer(input$score_away))
      touch()
      showNotification(
        sprintf("%s %d %s, %d %s, %d SC %s.",
                tr("Résultat enregistré :"), res$n_paris, tr("paris réglés"),
                res$n_gagnants, tr("gagnants"), res$total_paye, tr("redistribués")),
        type = "message", duration = 8)
    })

    # ---------------- Ajustement de StatCoins ----------------
    observeEvent(input$btn_adj, {
      req(est_admin(), input$adj_user)
      montant <- suppressWarnings(as.numeric(input$adj_montant))
      motif <- trimws(input$adj_motif %||% "")
      if (is.na(montant) || montant == 0) {
        showNotification(tr("Montant invalide."), type = "error")
        return()
      }
      if (motif == "") motif <- "Ajustement admin"
      add_transaction(con, as.integer(input$adj_user), montant,
                      paste0("[Admin] ", motif))
      touch()
      showNotification(tr("Ajustement appliqué."), type = "message")
    })

    # ---------------- Reprogrammation ----------------
    observeEvent(input$btn_date, {
      req(est_admin(), input$date_match_sel)
      heure <- trimws(input$nouvelle_heure %||% "")
      if (!grepl("^([01][0-9]|2[0-3]):[0-5][0-9]$", heure)) {
        showNotification(tr("Heure invalide (format HH:MM)."), type = "error")
        return()
      }
      DBI::dbExecute(con, "UPDATE matches SET date_match = ? WHERE match_id = ?",
                     params = list(paste(format(input$nouvelle_date), heure),
                                   as.integer(input$date_match_sel)))
      touch()
      showNotification(tr("Match reprogrammé."), type = "message")
    })

    # ---------------- Tables de supervision ----------------
    output$tbl_users <- DT::renderDT({
      lang()
      db_ver()
      req(est_admin())
      u <- DBI::dbGetQuery(con, "
        SELECT pseudo, nom, ROUND(statcoins) AS statcoins, is_admin, created_at
        FROM users ORDER BY statcoins DESC")
      DT::datatable(
        u,
        colnames = c(tr("Pseudo"), tr("Nom"), "StatCoins", "Admin", tr("Inscrit·e le")),
        rownames = FALSE,
        options = list(pageLength = 15, dom = "tip")
      )
    })

    output$tbl_bets <- DT::renderDT({
      lang()
      db_ver()
      req(est_admin())
      b <- get_bets(con)
      if (nrow(b) == 0) {
        return(DT::datatable(
          data.frame(x = tr("Aucun pari pour le moment.")),
          rownames = FALSE, colnames = "", options = list(dom = "t")))
      }
      b$match <- paste(b$home, "vs", b$away)
      DT::datatable(
        b[, c("placed_at", "pseudo", "match", "type", "selection",
              "mise", "cote", "settled", "gain")],
        colnames = c(tr("Date"), tr("Pseudo"), tr("Match"), tr("Type"),
                     tr("Sélection"), tr("Mise"), tr("Cote"),
                     tr("Réglé"), tr("Gain")),
        rownames = FALSE,
        options = list(pageLength = 15, dom = "tip")
      )
    })
  })
}
