# ------------------------------------------------------------------
# mod_paris.R — Paris sur les matchs à venir
# ------------------------------------------------------------------

mod_paris_ui <- function(id, i18n) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("entete")),
    card(
      card_header(tagList(bsicons::bs_icon("ticket-perforated"), i18n$t("Mes paris"))),
      card_body(DT::DTOutput(ns("tbl_mes_paris")))
    ),
    uiOutput(ns("cards"))
  )
}

mod_paris_server <- function(id, con, user, db_ver, touch, i18n_s, lang) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tr <- function(x) i18n_s$t(x)

    matchs <- reactive({
      db_ver()
      get_matches(con)
    })

    a_venir <- reactive({
      invalidateLater(60000)  # clôture automatique à l'heure du match
      m <- matchs()
      maintenant <- format(Sys.time(), "%Y-%m-%d %H:%M")
      m[m$played == 0 & m$date_match > maintenant, , drop = FALSE]
    })

    # ---------------- Entête ----------------
    output$entete <- renderUI({
      lang()
      u <- user()
      if (is.null(u)) {
        card(
          class = "border-warning mb-3",
          card_body(tagList(
            bsicons::bs_icon("info-circle"),
            tr("Connectez-vous dans l'onglet Compte pour pouvoir parier.")
          ))
        )
      } else {
        p(class = "lead",
          sprintf("%s : %s SC. %s", tr("Votre solde"), round(u$statcoins),
                  tr("Misez entre 1 et 100 StatCoins par pari.")))
      }
    })

    # ---------------- Cartes des matchs à venir ----------------
    output$cards <- renderUI({
      lang()
      av <- a_venir()
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
          layout_column_wrap(width = 1 / 2, fill = FALSE, !!!cartes)
        )
      })

      accordion(
        id = ns("acc_journees"),
        # open = paste0("j", min(av$journee)),
        open = FALSE,
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
        c(sprintf("%s (%.2f)", m$home, cv["home"]),
          sprintf("%s (%.2f)", m$away, cv["away"]))
      )
      choix_ecart <- setNames(
        ECART_TRANCHES,
        sprintf("%s %s (%.2f)", tr("Écart"), ECART_TRANCHES, ce)
      )

      card(
        class = "carte-match",
        card_header(
          class = "d-flex justify-content-between align-items-center",
          span(strong(m$home), " vs ", strong(m$away)),
          span(class = "badge bg-secondary",m$date_match)
          #      substr(m$date_match, 12, 16),
          #      substr(m$date_match, 1, 10)),
          # span(class = "badge bg-secondary",
          #      substr(m$date_match, 12, 16))
        ),
        card_body(
          div(class = "cotes-resume mb-2",
              span(class = "badge bg-primary me-1",
                   sprintf("%s %.2f", m$home, cv["home"])),
              span(class = "badge bg-primary me-1",
                   sprintf("%s %.2f", m$away, cv["away"])),
              span(class = "badge bg-info",
                   sprintf("%s 1-2: %.2f | 3-5: %.2f | 6: %.2f",
                           tr("Écart"), ce["1-2"], ce["3-5"], ce["6"]))
          ),
          radioButtons(ns(paste0("type_", mid)), tr("Type de pari"),
                       choiceNames = c(tr("Vainqueur"), tr("Écart de points")),
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
        showNotification(tr("La mise doit être un entier entre 1 et 100."),
                         type = "warning")
        return()
      }
      if (mise > u$statcoins) {
        showNotification(tr("Solde insuffisant pour cette mise."), type = "error")
        return()
      }

      # Cote recalculée côté serveur au moment du clic, puis figée
      if (type == "vainqueur") {
        sel <- as.character(input[[paste0("sel_v_", mid)]])
        cv <- cotes_vainqueur(con, m, m_all)
        cote <- if (sel == as.character(m$home_id)) cv["home"] else cv["away"]
      } else {
        sel <- as.character(input[[paste0("sel_e_", mid)]])
        ce <- cotes_ecart(con, m, m_all)
        cote <- ce[sel]
      }
      if (is.na(cote)) {
        showNotification(tr("Sélection invalide."), type = "error")
        return()
      }

      DBI::dbExecute(con, "
        INSERT INTO bets (user_id, match_id, type, selection, mise, cote)
        VALUES (?, ?, ?, ?, ?, ?)",
        params = list(u$user_id, mid, type, sel, mise, unname(cote)))
      add_transaction(con, u$user_id, -mise,
                      sprintf("Mise sur le match #%d", mid))
      touch()

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
        return(DT::datatable(
          data.frame(x = tr("Connectez-vous pour voir vos paris.")),
          rownames = FALSE, colnames = "", options = list(dom = "t")))
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
