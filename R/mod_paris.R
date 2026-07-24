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
        tagList(bsicons::bs_icon("trophy"), i18n$t("Vainqueur du tournoi")),
        uiOutput(ns("champion"))
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
      invalidateLater(3600000/4)   # rafraîchissement horaire
      db_ver_matchs()            # + immédiat quand un résultat tombe
      cotes_cache(cotes_tous(con, matchs()))
    })
    
    ids_ouverts <- reactiveVal(NULL)
    observe({
      invalidateLater(60000)            # clôture à l'heure du match
      m <- matchs()                     # suit db_ver (scores saisis, etc.)
      # maintenant <- format(Sys.time(), "%Y-%m-%d %H:%M")
      maintenant <- maintenant_local()  # heure belge, pas l'UTC du serveur
      ids <- m$match_id[m$played == 0 & m$date_match > maintenant]
      if (!identical(ids, ids_ouverts())) ids_ouverts(ids)   # silence sinon
    })
    
    # Cotes recalculées à chaque écriture en base — 1 requête, partagée
    cotes_du_moment <- reactive({
      db_ver_matchs()
      cotes_tous(con, matchs())
    }) |> bindCache(db_ver_matchs())

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
      
      shinyjs::disable(paste0("parier_", mid))

      m_all <- get_matches(con)
      m <- m_all[m_all$match_id == mid, , drop = FALSE]
      # maintenant <- format(Sys.time(), "%Y-%m-%d %H:%M")
      maintenant <- maintenant_local()  # heure belge, pas l'UTC du serveur
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
      
      shinyjs::enable(paste0("parier_", mid))
      
      showNotification(
        sprintf("%s %d SC @ %.2f — %s", tr("Pari enregistré :"), mise, cote,
                tr("bonne chance !")),
        type = "message")
    }

    # ================================================================
    # Marché "Vainqueur du tournoi" (champion + score exact de la finale)
    # ================================================================

    # Cotes du champion : suivent l'Elo (résultats) ET le flux de mises.
    # db_ver() bascule à chaque pari => cotes réévaluées en direct.
    cotes_champ <- reactive({
      db_ver()
      cotes_champion(con, matchs())
    })

    resultat_champ <- reactive({
      db_ver()
      get_champion_result(con)
    })

    output$champion <- renderUI({
      lang()
      res <- resultat_champ()
      teams <- get_teams(con)

      # Marché clôturé : on affiche le champion désigné
      if (!is.null(res$settled) && res$settled == 1) {
        nom_champ <- teams$nom[match(res$team_id, teams$team_id)]
        return(card(
          class = "carte-match",
          card_header(tagList(bsicons::bs_icon("trophy-fill"),
                              tr("Champion du tournoi"))),
          card_body(
            h3(class = "text-center my-3",
               sprintf("\U0001F3C6 %s", nom_champ %||% "?")),
            p(class = "text-center lead",
              sprintf("%s : %s", tr("Score de la finale"), res$score %||% "?")),
            DT::DTOutput(ns("tbl_mes_champ"))
          )
        ))
      }

      u <- user()
      choix_equipes <- setNames(teams$team_id, teams$nom)
      choix_scores  <- setNames(SCORES_FINALE, SCORES_FINALE)

      solde_txt <- if (is.null(u)) {
        p(class = "text-muted", tr("Connectez-vous pour parier."))
      } else {
        p(class = "lead",
          sprintf("%s : %s SC. %s", tr("Votre solde"), round(u$statcoins),
                  tr("Misez de 1 StatCoin jusqu'à la totalité de votre solde (all-in).")))
      }

      tagList(
        div(class = "mb-3",
            h4(tagList(bsicons::bs_icon("trophy"), tr("Pariez sur le grand gagnant du tournoi !"))),
            p(class = "text-muted mb-1", tr("Le tournoi se termine par deux demi-finales et une finale.")),
            p(class = "text-muted", tr("Les équipes finalistes ne sont pas encore connues : à vous de deviner le champion et le score de la finale."))
        ),
        card(
          class = "carte-match",
          card_header(tagList(bsicons::bs_icon("trophy"), tr("Vainqueur du tournoi"))),
          card_body(
            solde_txt,
            selectInput(ns("champ_team"), tr("Quelle équipe soulève le trophée ?"),
                        choices = choix_equipes),
            selectInput(ns("champ_score"), tr("Score exact de la finale"),
                        choices = choix_scores, selected = "6-2"),
            uiOutput(ns("champ_cote")),
            numericInput(ns("champ_mise"), tr("Mise (StatCoins)"),
                         value = 10, min = MISE_MIN, step = 1),
            checkboxInput(ns("champ_allin"), tr("Tout miser (all-in)"), value = FALSE),
            actionButton(ns("champ_parier"), tr("Parier sur le champion"),
                         class = "btn-primary w-100")
          )
        ),
        card(
          card_header(tagList(bsicons::bs_icon("ticket-perforated"),
                              tr("Mes paris champion"))),
          card_body(DT::DTOutput(ns("tbl_mes_champ")))
        )
      )
    })

    # Cote de la sélection courante + gain potentiel
    output$champ_cote <- renderUI({
      lang()
      req(input$champ_team, input$champ_score)
      cc   <- cotes_champ()
      cote <- cote_champion(cc, input$champ_team, input$champ_score)
      req(!is.na(cote))
      mise <- suppressWarnings(as.numeric(input$champ_mise))
      gain <- if (!is.na(mise)) round(mise * cote) else NA
      div(class = "cotes-resume mb-2",
          span(class = "badge bg-primary me-1",
               sprintf("%s %.2f", tr("Cote"), cote)),
          if (!is.na(gain))
            span(class = "badge bg-success",
                 sprintf("%s : %d SC", tr("Gain potentiel"), gain))
      )
    })

    # All-in : verrouille la mise sur la totalité du solde
    observeEvent(input$champ_allin, {
      u <- user()
      if (isTRUE(input$champ_allin)) {
        solde <- if (is.null(u)) MISE_MIN else floor(u$statcoins)
        updateNumericInput(session, "champ_mise", value = max(MISE_MIN, solde))
        shinyjs::disable("champ_mise")
      } else {
        shinyjs::enable("champ_mise")
      }
    }, ignoreInit = TRUE)

    observeEvent(input$champ_parier, {
      u <- user()
      if (is.null(u)) {
        showNotification(tr("Connectez-vous pour parier."), type = "warning")
        return()
      }
      if (!champion_ouvert(con)) {
        showNotification(tr("Le marché du champion est clôturé."), type = "error")
        return()
      }

      team_id <- as.integer(input$champ_team)
      score   <- as.character(input$champ_score)
      if (is.na(team_id) || !(score %in% SCORES_FINALE)) {
        showNotification(tr("Sélectionnez une équipe et un score."), type = "warning")
        return()
      }

      # All-in : la mise = solde entier, quel qu'il soit (pas de plafond)
      mise <- if (isTRUE(input$champ_allin)) floor(u$statcoins)
              else suppressWarnings(as.numeric(input$champ_mise))
      if (is.na(mise) || mise < MISE_MIN || mise != round(mise)) {
        showNotification(tr("La mise doit être un entier positif."), type = "warning")
        return()
      }
      if (mise > u$statcoins) {
        showNotification(tr("Solde insuffisant pour cette mise."), type = "error")
        return()
      }

      # Cote figée côté serveur au moment du clic
      cc   <- isolate(cotes_champ())
      cote <- cote_champion(cc, team_id, score)
      if (is.na(cote)) {
        showNotification(tr("Sélectionnez une équipe et un score."), type = "error")
        return()
      }

      shinyjs::disable("champ_parier")
      res <- place_champion_bet(con, u$user_id, team_id, score, mise, cote)
      shinyjs::enable("champ_parier")

      if (nrow(res) == 0) {
        showNotification(tr("Solde insuffisant pour cette mise."), type = "error")
        return()
      }
      showNotification(
        sprintf("%s %d SC @ %.2f — %s", tr("Pari champion enregistré :"),
                mise, cote, tr("bonne chance !")),
        type = "message")
    })

    output$tbl_mes_champ <- DT::renderDT({
      lang()
      db_ver()
      u <- user()
      if (is.null(u)) return(NULL)
      b <- get_champion_bets(con, u$user_id)
      if (nrow(b) == 0) {
        return(DT::datatable(
          data.frame(x = tr("Aucun pari sur le champion pour le moment.")),
          rownames = FALSE, colnames = "", options = list(dom = "t")))
      }
      b$placed_at <- fmt_horodatage(b$placed_at)
      b$statut <- dplyr::case_when(
        b$settled == 0 ~ tr("En cours"),
        b$gain > 0     ~ sprintf("%s +%d SC", tr("Gagné"), round(b$gain)),
        TRUE           ~ tr("Perdu")
      )
      DT::datatable(
        b[, c("placed_at", "equipe", "score", "mise", "cote", "statut")],
        colnames = c(tr("Date"), tr("Champion"), tr("Score"),
                     tr("Mise"), tr("Cote"), tr("Statut")),
        rownames = FALSE,
        options = list(pageLength = 10, dom = "tip")
      )
    })

    # ---------------- Mes paris ----------------
    output$tbl_mes_paris <- DT::renderDT({
      lang()
      u <- user()
      db_ver_matchs()
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
      b$placed_at <- fmt_horodatage(b$placed_at)   # affichage en heure belge
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
