# ------------------------------------------------------------------
# app.R — Tournoi de Kubb : paris amicaux en StatCoins
# ------------------------------------------------------------------
# Les fichiers du dossier R/ sont sourcés automatiquement par Shiny.
# Lancement : shiny::runApp()
# ------------------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(DT)
library(plotly)
library(shiny.i18n)
library(DBI)
library(RSQLite)
library(bsicons)

# file.remove("data/kubb.sqlite")

# ---------------- Base de données ----------------
con <- db_connect()
db_init(con)
shiny::onStop(function() DBI::dbDisconnect(con))

# ---------------- Internationalisation ----------------
i18n <- Translator$new(translation_json_path = "translations/translation.json")
i18n$set_translation_language("fr")
i18n$use_js()

# ---------------- Thème : chaleureux, été ----------------
theme_kubb <- bs_theme(
  version      = 5,
  bg           = "#FFFBF2",   # sable clair
  fg           = "#3B2C20",   # bois sombre
  primary      = "#E8743B",   # orange soleil couchant
  secondary    = "#2A9D8F",   # turquoise pelouse-piscine
  success      = "#7CB518",   # herbe d'été
  warning      = "#F4A259",   # abricot
  danger       = "#C44536"   # brique
  # base_font    = font_google("Nunito"),
  # heading_font = font_google("Fredoka")
)

# ---------------- UI ----------------
ui <- page_navbar(
  id = "nav",
  title = tagList(span("\u2600\ufe0f"), i18n$t("Tournoi de Kubb")),
  theme = theme_kubb,
  header = tagList(
    usei18n(i18n),
    tags$head(tags$link(rel = "stylesheet", href = "custom.css"))
  ),

  nav_panel(
    title = i18n$t("Compte"), value = "compte",
    icon = bsicons::bs_icon("person"),
    mod_auth_ui("auth", i18n)
  ),
  nav_panel(
    title = i18n$t("Suivi"), value = "suivi",
    icon = bsicons::bs_icon("bar-chart-line"),
    mod_suivi_ui("suivi", i18n)
  ),
  nav_panel(
    title = i18n$t("Paris"), value = "paris",
    icon = bsicons::bs_icon("dice-5"),
    mod_paris_ui("paris", i18n)
  ),
  nav_panel(
    title = i18n$t("Admin"), value = "admin",
    icon = bsicons::bs_icon("gear"),
    mod_admin_ui("admin", i18n)
  ),

  nav_spacer(),
  nav_item(
    selectInput("selected_lang", NULL,
                choices = setNames(i18n$get_languages(),
                                   toupper(i18n$get_languages())),
                selected = "fr", width = "85px")
  )
)

# ---------------- Server ----------------
server <- function(input, output, session) {

  # Traducteur propre à la session (le clone évite que la langue
  # d'un·e utilisateur·rice contamine les autres sessions)
  i18n_s <- i18n$clone()
  lang <- reactiveVal("fr")

  observeEvent(input$selected_lang, {
    shiny.i18n::update_lang(input$selected_lang)
    i18n_s$set_translation_language(input$selected_lang)
    lang(input$selected_lang)
  }, ignoreInit = TRUE)

  # Version de la base : toutes les sessions se synchronisent dessus
  db_ver <- reactivePoll(
    2000, session,
    checkFunc = function() db_version(con),
    valueFunc = function() db_version(con)
  )
  touch <- function() db_touch(con)

  # Utilisateur·rice courant·e (re-lu à chaque écriture en base,
  # pour que le solde affiché soit toujours juste)
  user_id <- reactiveVal(NULL)
  user <- reactive({
    db_ver()
    uid <- user_id()
    if (is.null(uid)) return(NULL)
    u <- get_user(con, uid)
    if (nrow(u) == 0) NULL else as.list(u)
  })

  # L'onglet Admin n'apparaît que pour les admins
  observe({
    u <- user()
    admin <- !is.null(u) &&
      (u$is_admin == 1 || tolower(u$pseudo) %in% tolower(ADMIN_PSEUDOS))
    if (admin) nav_show("nav", "admin") else nav_hide("nav", "admin")
  })

  mod_auth_server("auth", con, user, user_id, db_ver, touch, i18n_s, lang)
  mod_paris_server("paris", con, user, db_ver, touch, i18n_s, lang)
  mod_suivi_server("suivi", con, db_ver, i18n_s, lang)
  mod_admin_server("admin", con, user, db_ver, touch, i18n_s, lang)
  
  observeEvent(lang(),{
    updateSelectInput(session,"selected_lang",selected = lang())
  })
}

shinyApp(ui, server)
