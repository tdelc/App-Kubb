# ------------------------------------------------------------------
# mod_paris.R — Paris sur les matchs à venir
# ------------------------------------------------------------------

mod_paris_ui <- function(id, i18n) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("entete")),
    navset_card_tab(
      nav_panel(
        i18n$t("Parier"),
        uiOutput(ns("cards"))
      ),
      nav_panel(
        i18n$t("Mes paris"),
        DT::DTOutput(ns("tbl_mes_paris"))
      )
    )
    # card(
    #   card_header(tagList(bsicons::bs_icon("ticket-perforated"), i18n$t("Mes paris"))),
    #   card_body(DT::DTOutput(ns("tbl_mes_paris")))
    # ),
    # uiOutput(ns("cards"))
  )
}

mod_paris_server <- function(id, con, user, db_ver, db_ver_matchs, touch, i18n_s, lang) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tr <- function(x) i18n_s$t(x)

    # matchs <- reactive({
    #   db_ver()
    #   get_matches(con)
    # })
    
    matchs <- reactive({ db_ver_matchs(); get_matches(con) })  # plus déclenché par les paris
    
    cotes_cache <- reactiveVal(NULL)
    observe({
      invalidateLater(3600000)   # rafraîchissement horaire
      db_ver_matchs()            # + immédiat quand un résultat tombe
      cotes_cache(cotes_tous(con, matchs()))
    })
    
    ids_ouverts <- reactiveVal(NULL)
    observe({
      invalidateLater(60000)            # clôture à l'heure du match
      m <- matchs()                     # suit db_ver (scores saisis, etc.)
      maintenant <- format(Sys.time(), "%Y-%m-%d %H:%M")
      ids <- m$match_id[m$played == 0 & m$date_match > maintenant]
      if (!identical(ids, ids_ouverts())) ids_ouverts(ids)   # silence sinon
    })
    
    # Cotes recalculées à chaque écriture en base — 1 requête, partagée
    cotes_du_moment <- reactive({
      db_ver()
      cotes_tous(con, matchs())
    }) |> bindCache(db_ver())

    # ---------------- Entête ----------------
    output$entete <- renderUI({
      lang()
      u <- user()
      if (is.null(u)) {
        p(class = "lead",
          tr("Connectez-vous dans l'onglet Compte pour pouvoir parier.")
        )
      } else {
        p(class = "lead",
          sprintf("%s : %s SC. %s", tr("Votre solde"), round(u$statcoins),
                  tr("Misez entre 1 et 200 StatCoins par pari.")))
      }
    })

    # ---------------- Cartes des matchs à venir ----------------
    output$cards <- renderUI({
      # lang()
      # av <- a_venir()
      lang()
      ids <- ids_ouverts()
      req(!is.null(ids))
      m_all <- isolate(matchs())
      av <- m_all[m_all$match_id %in% ids, , drop = FALSE]
      if (nrow(av) == 0) {
        return(card(card_body(tr("Aucun match à venir : le tournoi est terminé !"))))
      }

      m_all <- matchs()
      panels <- lapply(sort(unique(av$journee)), function(j) {
        mj <- av[av$journee == j, , drop = FALSE]
        date_j <- mj |> count(date_match = substr(date_match, 1, 10), name = "nb") |> 
          slice_max(nb, n = 1) |> pull(date_match)
        cartes <- lapply(seq_len(nrow(mj)), function(i) {
          carte_match(mj[i, ], m_all)
        })
        accordion_panel(
          title = sprintf("%s %d — %s", tr("Journée"), j,
                          format(as.Date(date_j),"%d/%m/%Y")),
          value = paste0("j", j),
          layout_column_wrap(width = 1 / 4, fill = FALSE, !!!cartes)
        )
      })

      accordion(
        id = ns("acc_journees"),
        open = paste0("j", min(av$journee)),
        # open = FALSE,
        !!!panels
      )
    })

    # Construit la carte d'un match, avec ses cotes du moment
    carte_match <- function(m, m_all) {
      cv <- cotes_vainqueur(con, m, m_all)
      ce <- cotes_ecart(con, m, m_all)
      mid <- m$match_id

      choix_vainqueur <- setNames(
        c(m$home_id, m$away_id),
        c(m$home,m$away)
      )
      choix_ecart <- setNames(
        ECART_TRANCHES,
        sprintf("%s %s", tr("Écart"), ECART_TRANCHES)
      )

      card(
        class = "carte-match",
        card_header(
          class = "d-flex justify-content-between align-items-center",
          span(strong(m$home), " vs ", strong(m$away)),
          span(class = "badge bg-secondary",m$date_match)
        ),
        card_body(
          uiOutput(ns(paste0("cotes_", mid))),
          radioButtons(ns(paste0("type_", mid)), tr("Type de pari"),
                       choiceNames = c(tr("Vainqueur"), tr("Écart de points (au score)")),
                       choiceValues = c("vainqueur", "ecart"),
                       inline = TRUE),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'vainqueur'", ns(paste0("type_", mid))),
            selectInput(ns(paste0("sel_v_", mid)), tr("Quelle équipe gagne ?"),
                        choices = choix_vainqueur)
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'ecart'", ns(paste0("type_", mid))),
            selectInput(ns(paste0("sel_e_", mid)), tr("Quel écart de points ?"),
                        choices = choix_ecart)
          ),
          numericInput(ns(paste0("mise_", mid)), tr("Mise (StatCoins)"),
                       value = 10, min = MISE_MIN, max = MISE_MAX, step = 1),
          actionButton(ns(paste0("parier_", mid)), tr("Parier"),
                       class = "btn-primary w-100")
        )
      )
    }

    # ---------------- Placement des paris ----------------
    # Les match_id sont connus dès le départ : un observateur par match.
    tous_les_matchs <- isolate(get_matches(con))
    purrr::walk(tous_les_matchs$match_id, function(mid) {
      observeEvent(input[[paste0("parier_", mid)]], {
        placer_pari(mid)
      }, ignoreInit = TRUE)
    })
    
    purrr::walk(tous_les_matchs$match_id, function(mid) {
      output[[paste0("cotes_", mid)]] <- renderUI({
        lang()
        ct <- cotes_cache()[[as.character(mid)]]
        req(ct)
        m <- tous_les_matchs[tous_les_matchs$match_id == mid, ]
        div(class = "cotes-resume mb-2",
            span(class = "badge bg-primary me-1",
                 sprintf("%s %.2f", m$home, ct$vainqueur["home"])),
            span(class = "badge bg-primary me-1",
                 sprintf("%s %.2f", m$away, ct$vainqueur["away"])),
            span(class = "badge bg-info",
                 sprintf("%s 1-2: %.2f | 3-5: %.2f | 6: %.2f", tr("Écart"),
                         ct$ecart["1-2"], ct$ecart["3-5"], ct$ecart["6"])))
      })
    })

    placer_pari <- function(mid) {
      u <- user()
      if (is.null(u)) {
        showNotification(tr("Connectez-vous pour parier."), type = "warning")
        return()
      }

      m_all <- get_matches(con)
      m <- m_all[m_all$match_id == mid, , drop = FALSE]
      maintenant <- format(Sys.time(), "%Y-%m-%d %H:%M")
      if (m$played == 1 || m$date_match <= maintenant) {
        showNotification(tr("Les paris sont clôturés pour ce match."),
                         type = "error")
        return()
      }

      type <- input[[paste0("type_", mid)]] %||% "vainqueur"
      mise <- suppressWarnings(as.numeric(input[[paste0("mise_", mid)]]))

      if (is.na(mise) || mise < MISE_MIN || mise > MISE_MAX || mise != round(mise)) {
        showNotification(tr("La mise doit être un entier entre 1 et 200"),
                         type = "warning")
        return()
      }
      if (mise > u$statcoins) {
        showNotification(tr("Solde insuffisant pour cette mise."), type = "error")
        return()
      }

      # Cote recalculée côté serveur au moment du clic, puis figée
      ct <- isolate(cotes_cache())[[as.character(mid)]]
      m  <- tous_les_matchs[tous_les_matchs$match_id == mid, ]
      if (type == "vainqueur") {
        sel  <- as.character(input[[paste0("sel_v_", mid)]])
        cote <- if (sel == as.character(m$home_id)) ct$vainqueur["home"] else ct$vainqueur["away"]
      } else {
        sel  <- as.character(input[[paste0("sel_e_", mid)]])
        cote <- ct$ecart[sel]
      }
      if (is.na(cote)) {
        showNotification(tr("Sélection invalide."), type = "error")
        return()
      }

      res <- dbx_get(con, "
            WITH u AS (
              UPDATE users SET statcoins = statcoins - ?::double precision
              WHERE user_id = ?::int AND statcoins >= ?::double precision
              RETURNING user_id
            ), b AS (
              INSERT INTO bets (user_id, match_id, type, selection, mise, cote)
              SELECT user_id, ?::int, ?::text, ?::text,
                     ?::double precision, ?::double precision
              FROM u
              RETURNING bet_id
            ), t AS (
              INSERT INTO transactions (user_id, montant, motif)
              SELECT user_id, ?::double precision, ?::text FROM u
            ), v AS (
              UPDATE meta SET version = version + 1
              WHERE EXISTS (SELECT 1 FROM u)
            )
            SELECT bet_id FROM b",
                     params = list(mise, u$user_id, mise,
                                   mid, type, sel, mise, unname(cote),
                                   -mise, sprintf("Mise sur le match #%d", mid)))
      
      if (nrow(res) == 0) {
        showNotification(tr("Solde insuffisant pour cette mise."), type = "error")
        return()
      }
      
      showNotification(
        sprintf("%s %d SC @ %.2f — %s", tr("Pari enregistré :"), mise, cote,
                tr("bonne chance !")),
        type = "message")
    }

    # ---------------- Mes paris ----------------
    output$tbl_mes_paris <- DT::renderDT({
      lang()
      u <- user()
      db_ver()
      if (is.null(u)) {
        return(NULL)
      }
      b <- get_bets(con, u$user_id)
      if (nrow(b) == 0) {
        return(DT::datatable(
          data.frame(x = tr("Aucun pari pour le moment.")),
          rownames = FALSE, colnames = "", options = list(dom = "t")))
      }

      b$match <- paste(b$home, "vs", b$away)
      b$type_lbl <- ifelse(b$type == "vainqueur", tr("Vainqueur"), tr("Écart"))
      b$sel_lbl <- ifelse(
        b$type == "vainqueur",
        ifelse(b$selection == as.character(b$home_id), b$home, b$away),
        b$selection
      )
      b$statut <- dplyr::case_when(
        b$settled == 0 ~ tr("En cours"),
        b$gain > 0     ~ sprintf("%s +%d SC", tr("Gagné"), round(b$gain)),
        TRUE           ~ tr("Perdu")
      )

      DT::datatable(
        b[, c("placed_at", "match", "type_lbl", "sel_lbl", "mise", "cote", "statut")],
        colnames = c(tr("Date"), tr("Match"), tr("Type"), tr("Sélection"),
                     tr("Mise"), tr("Cote"), tr("Statut")),
        rownames = FALSE,
        options = list(pageLength = 10, dom = "tip")
      )
    })
  })
}
