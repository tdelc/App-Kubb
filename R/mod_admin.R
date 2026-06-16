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

      users <- dbx_get(con, "SELECT user_id, pseudo FROM users ORDER BY pseudo")
      choix_users <- setNames(users$user_id, users$pseudo)

      tagList(
        
        navset_card_tab(
          
          nav_panel(tr("Actions"),
          
            layout_column_wrap(
              width = 1 / 2,
              fill = FALSE,
    
              card(
                card_header(tagList(bsicons::bs_icon("pencil-square"),
                                    tr("Saisir un résultat"))),
                card_body(
                  selectInput(ns("score_match"), tr("Match"), choices = choix_matchs),
                  uiOutput(ns("score_inputs")),
                  tableOutput(ns("tbl_score_bets")),
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
            )
          ),
          nav_panel(tr("Parieur·euses"), DT::DTOutput(ns("tbl_users"))),
          nav_panel(tr("Tous les paris"), DT::DTOutput(ns("tbl_bets"))),
          nav_panel(tr("Annuler des paris"),
                    card(
                      card_header(tagList(bsicons::bs_icon("x-octagon"),
                                          tr("Annuler des paris en cours"))),
                      card_body(
                        p(class = "text-muted small",
                          tr("Sélectionnez une ou plusieurs lignes : la mise est remboursée au parieur·euse et le pari est supprimé. Seuls les paris non encore réglés sont listés.")),
                        actionButton(ns("btn_annul"),
                                     tr("Annuler les paris sélectionnés"),
                                     class = "btn-danger mt-2"),
                        DT::DTOutput(ns("tbl_annul"))
                        
                      )
                    )
          ),
          nav_panel(tr("Stats"),
                    card(
                      card_header(tagList(bsicons::bs_icon("bar-chart-line"),
                                          tr("Visuels prêts à partager"))),
                      card_body(
                        layout_column_wrap(
                          width = 1 / 3, fill = FALSE,
                          selectInput(ns("stat_viz"), tr("Visualisation"),
                                      choices = setNames(
                                        names(STAT_VIZ),
                                        vapply(STAT_VIZ, `[[`, character(1), "label"))),
                          selectInput(ns("stat_format"), tr("Format image"),
                                      choices = c("Carré 1080×1080"  = "carre",
                                                  "Paysage 1200×675" = "paysage",
                                                  "Story 1080×1350"  = "story")),
                          numericInput(ns("stat_topn"), tr("Top N (si applicable)"),
                                       value = 8, min = 3, max = 20, step = 1)
                        ),
                        div(class = "border rounded p-2 bg-white",
                            plotOutput(ns("stat_plot"), height = "560px")),
                        div(class = "d-flex gap-2 mt-2",
                            downloadButton(ns("dl_stat"), tr("Télécharger le PNG"),
                                           class = "btn-primary")),
                        p(class = "text-muted small mt-2",
                          tr("Astuce : clic droit sur l'image → « Copier l'image » pour un collage direct, ou téléchargez le PNG au format choisi. Les textes sont déjà en FR/NL dans l'image."))
                      )
                    )
          )
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
    
    output$tbl_score_bets <- renderTable({
    
      b <- get_bets(con)
      t <- get_teams(con)
      if (nrow(b) == 0) {
        return(DT::datatable(
          data.frame(x = tr("Aucun pari pour le moment.")),
          rownames = FALSE, colnames = "", options = list(dom = "t")))
      }
      b <- b |>
        dplyr::filter(match_id == as.integer(input$score_match)) |>
        dplyr::mutate(team_id = as.numeric(selection)) |>
        dplyr::left_join(t,by = "team_id") |>
        dplyr::group_by(type,nom,selection) |>
        dplyr::summarise(n = n(),
                         sum_bets = sum(mise),
                         mean_bets = mean(mise),
                         .groups = "drop")
      
      b
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
      db_touch_matchs(con)
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
      dbx_exec(con, "UPDATE matches SET date_match = ? WHERE match_id = ?",
                     params = list(paste(format(input$nouvelle_date), heure),
                                   as.integer(input$date_match_sel)))
      db_touch_matchs(con)
      showNotification(tr("Match reprogrammé."), type = "message")
    })
    
    # ---------------- Annulation de paris ----------------
    # Paris en cours uniquement (settled = 0) : ce sont les seuls
    # annulables (les doublons de double-clic en font partie).
    paris_annulables <- reactive({
      lang()
      db_ver()
      req(est_admin())
      b <- get_bets(con)
      b[b$settled == 0, , drop = FALSE]
    })
    
    output$tbl_annul <- DT::renderDT({
      b <- paris_annulables()
      if (nrow(b) == 0) {
        return(DT::datatable(
          data.frame(x = tr("Aucun pari en cours à annuler.")),
          rownames = FALSE, colnames = "", options = list(dom = "t")))
      }
      b$match <- paste(b$home, "vs", b$away)
      b$sel_lbl <- ifelse(
        b$type == "vainqueur",
        ifelse(b$selection == as.character(b$home_id), b$home, b$away),
        b$selection
      )
      DT::datatable(
        b[, c("placed_at", "pseudo", "match", "type", "sel_lbl", "mise", "cote")],
        colnames = c(tr("Date"), tr("Pseudo"), tr("Match"), tr("Type"),
                     tr("Sélection"), tr("Mise"), tr("Cote")),
        rownames = FALSE,
        selection = "multiple",
        options = list(pageLength = 15, dom = "tip")
      )
    })
    
    # Paris retenus pour annulation, figés à l'ouverture du modal
    annul_pending <- reactiveVal(NULL)
    
    observeEvent(input$btn_annul, {
      req(est_admin())
      sel <- input$tbl_annul_rows_selected
      if (length(sel) == 0) {
        showNotification(tr("Sélectionnez au moins un pari à annuler."),
                         type = "warning")
        return()
      }
      b <- paris_annulables()
      annul_pending(b$bet_id[sel])          # figé ici
      showModal(modalDialog(
        title = tr("Confirmer l'annulation"),
        sprintf("%d %s — %d SC %s.",
                length(sel), tr("pari·s seront annulé·s et remboursé·s :"),
                round(sum(b$mise[sel])), tr("au total")),
        footer = tagList(
          modalButton(tr("Retour")),
          actionButton(ns("btn_annul_ok"), tr("Confirmer l'annulation"),
                       class = "btn-danger")
        )
      ))
    })
    
    observeEvent(input$btn_annul_ok, {
      req(est_admin())
      removeModal()
      ids <- annul_pending()
      annul_pending(NULL)
      req(length(ids) > 0)
      res <- annuler_paris(con, ids)
      msg <- sprintf("%s %d %s, %d SC %s.",
                     tr("Annulation :"), res$n_annules,
                     tr("pari·s annulé·s"), round(res$total_rendu),
                     tr("remboursés"))
      if (res$n_ignores > 0) {
        msg <- paste0(msg, sprintf(" %d %s.", res$n_ignores,
                                   tr("déjà réglé·s, ignoré·s")))
      }
      showNotification(msg, type = "message", duration = 8)
    })
    
    # ---------------- Stats (visuels à partager) ----------------
    stats_data <- reactive({
      db_ver()
      req(est_admin())
      list(
        bets   = get_bets(con),
        tx     = get_transactions(con),
        users  = dbx_get(con, "SELECT user_id, pseudo, nom, statcoins FROM users"),
        teams  = get_teams(con),
        matchs = get_matches(con)
      )
    })
    
    stat_plot_courant <- reactive({
      req(est_admin(), input$stat_viz)
      topn <- suppressWarnings(as.integer(input$stat_topn))
      if (is.na(topn) || topn < 1) topn <- 8
      STAT_VIZ[[input$stat_viz]]$f(stats_data(), topn)
    })
    
    output$stat_plot <- renderPlot({ stat_plot_courant() }, res = 96)
    
    output$dl_stat <- downloadHandler(
      filename = function() {
        sprintf("kubb_%s_%s.png", input$stat_viz %||% "stat",
                format(Sys.time(), "%Y%m%d_%H%M"))
      },
      content = function(file) {
        dim <- STAT_FORMATS[[input$stat_format %||% "carre"]]
        ggplot2::ggsave(file, plot = stat_plot_courant(),
                        width = dim$w, height = dim$h, units = "px",
                        dpi = 120, bg = "white")
      }
    )
    
    # ---------------- Tables de supervision ----------------
    output$tbl_users <- DT::renderDT({
      lang()
      db_ver()
      req(est_admin())
      u <- dbx_get(con, "
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
