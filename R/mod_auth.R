# ------------------------------------------------------------------
# mod_auth.R — Inscription, connexion, profil
# ------------------------------------------------------------------

mod_auth_ui <- function(id, i18n) {
  ns <- NS(id)
  uiOutput(ns("ui"))
}

mod_auth_server <- function(id, con, user, user_id, db_ver, touch, i18n_s, lang) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tr <- function(x) i18n_s$t(x)
    
    observe({
      showModal(modalDialog(
        layout_column_wrap(
          width = 1 / 2,
          fill = FALSE,
          card(
            card_header(tagList(bsicons::bs_icon("door-open"), tr("Connexion"))),
            card_body(
              textInput(ns("login_pseudo"), tr("Pseudo")),
              passwordInput(ns("login_pwd"), tr("Mot de passe")),
              actionButton(ns("btn_login"), tr("Se connecter"),
                           class = "btn-primary")
            )
          ),
          card(
            card_header(tagList(bsicons::bs_icon("person-plus"), tr("Inscription"))),
            card_body(
              textInput(ns("reg_nom"), tr("Nom")),
              textInput(ns("reg_pseudo"), tr("Pseudo")),
              passwordInput(ns("reg_pwd"), tr("Mot de passe")),
              selectInput(ns("reg_lan"), tr("Langue"),choices = c("fr","nl","en")),
              actionButton(ns("btn_register"), tr("Créer mon compte"),
                           class = "btn-success"),
              p(class = "text-muted small mt-2",
                tr("L'inscription crédite votre compte de 1000 StatCoins."))
            )
          )
        ),size = "xl", easyClose = TRUE
      ))
    })

    # ---------------- UI dynamique selon l'état de connexion ----------------
    output$ui <- renderUI({
      lang()
      u <- user()

      if (is.null(u)) {
        layout_column_wrap(
          width = 1 / 2,
          fill = FALSE,
          card(
            card_header(tagList(bsicons::bs_icon("door-open"), tr("Connexion"))),
            card_body(
              textInput(ns("login_pseudo"), tr("Pseudo")),
              passwordInput(ns("login_pwd"), tr("Mot de passe")),
              actionButton(ns("btn_login"), tr("Se connecter"),
                           class = "btn-primary")
            )
          ),
          card(
            card_header(tagList(bsicons::bs_icon("person-plus"), tr("Inscription"))),
            card_body(
              textInput(ns("reg_nom"), tr("Nom")),
              textInput(ns("reg_pseudo"), tr("Pseudo")),
              passwordInput(ns("reg_pwd"), tr("Mot de passe")),
              selectInput(ns("reg_lan"), tr("Langue"),choices = c("fr","nl","en")),
              actionButton(ns("btn_register"), tr("Créer mon compte"),
                           class = "btn-success"),
              p(class = "text-muted small mt-2",
                tr("L'inscription crédite votre compte de 1000 StatCoins."))
            )
          )
        )
      } else {
        tagList(
          layout_column_wrap(
            width = 1 / 3,
            fill = FALSE,
            value_box(
              title = tr("Connecté·e en tant que"),
              value = u$pseudo,
              showcase = bsicons::bs_icon("person-circle"),
              theme = "primary",
              p(u$nom)
            ),
            value_box(
              title = tr("Solde"),
              value = paste(round(u$statcoins), "SC"),
              showcase = bsicons::bs_icon("coin"),
              theme = "warning"
            ),
            card(
              card_body(
                class = "d-flex align-items-center justify-content-center",
                actionButton(ns("btn_logout"), tr("Se déconnecter"),
                             class = "btn-outline-secondary")
              )
            )
          ),
          card(
            card_header(tagList(bsicons::bs_icon("clock-history"),
                                tr("Mes mouvements de StatCoins"))),
            card_body(DT::DTOutput(ns("tbl_tx")))
          )
        )
      }
    })

    # ---------------- Connexion ----------------
    observeEvent(input$btn_login, {
      pseudo <- trimws(input$login_pseudo %||% "")
      if (pseudo == "" || (input$login_pwd %||% "") == "") {
        showNotification(tr("Pseudo et mot de passe requis."), type = "warning")
        return()
      }
      u <- get_user_by_pseudo(con, pseudo)
      if (nrow(u) == 1 && identical(u$password, input$login_pwd)) {
        user_id(u$user_id)
        shiny.i18n::update_lang(u$language)
        i18n_s$set_translation_language(u$language)
        lang(u$language)
        removeModal()
        showNotification(
          sprintf("%s %s !", tr("Bienvenue"), u$pseudo), type = "message")
      } else {
        showNotification(tr("Pseudo ou mot de passe incorrect."), type = "error")
      }
    })

    # ---------------- Inscription ----------------
    observeEvent(input$btn_register, {
      nom    <- trimws(input$reg_nom %||% "")
      pseudo <- trimws(input$reg_pseudo %||% "")
      language <- trimws(input$reg_lan %||% "")
      pwd    <- input$reg_pwd %||% ""

      if (nom == "" || pseudo == "") {
        showNotification(tr("Nom et pseudo requis."), type = "warning")
        return()
      }
      if (nchar(pwd) < 4) {
        showNotification(tr("Le mot de passe doit faire au moins 4 caractères."),
                         type = "warning")
        return()
      }
      if (nrow(get_user_by_pseudo(con, pseudo)) > 0) {
        showNotification(tr("Ce pseudo est déjà pris."), type = "error")
        return()
      }

      uid <- dbx_get(con, "
        INSERT INTO users (nom, pseudo, language, password, is_admin)
        VALUES (?, ?, ?, ?, ?) RETURNING user_id",
                     params = list(nom, pseudo, pwd,
                                   as.integer(tolower(pseudo) %in% tolower(ADMIN_PSEUDOS))))$user_id
        # params = list(nom, pseudo, language, pwd,
        #               as.integer(tolower(pseudo) %in% tolower(ADMIN_PSEUDOS))))
      # uid <- dbx_get(con, "SELECT last_insert_rowid() AS id")$id
      add_transaction(con, uid, CREDIT_INITIAL, "Cr\u00e9dit initial")
      touch()
      user_id(uid)
      shiny.i18n::update_lang(language)
      i18n_s$set_translation_language(language)
      lang(language)
      removeModal()
      showNotification(
        sprintf("%s %s !", tr("Compte créé, bienvenue"), pseudo), type = "message")
    })

    # ---------------- Déconnexion ----------------
    observeEvent(input$btn_logout, {
      user_id(NULL)
      showNotification(tr("À bientôt !"), type = "message")
    })

    # ---------------- Historique personnel ----------------
    output$tbl_tx <- DT::renderDT({
      lang()
      u <- req(user())
      db_ver()
      tx <- dbx_get(con, "
        SELECT ts, motif, montant FROM transactions
        WHERE user_id = ? ORDER BY ts DESC, tx_id DESC",
        params = list(u$user_id))
      DT::datatable(
        tx,
        colnames = c(tr("Date"), tr("Motif"), tr("Montant")),
        rownames = FALSE,
        options = list(pageLength = 10, dom = "tip")
      ) |>
        DT::formatStyle("montant",
                        color = DT::styleInterval(0, c("#C0392B", "#1E8449")))
    })
  })
}
