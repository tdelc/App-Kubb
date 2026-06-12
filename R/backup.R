#!/usr/bin/env Rscript
# ------------------------------------------------------------------
# backup_kubb.R — Sauvegarde et restauration de la base Kubb
#                 (Postgres Railway, depuis votre PC local)
# ------------------------------------------------------------------
# Usage :
#   * Sauvegarde (planifiée ou manuelle) :
#       Rscript backup_kubb.R
#     -> écrit save/kubb_YYYY-MM-DD_HHMM.rds (archive fidèle, types préservés)
#        + save/dernier_etat/*.csv (copies lisibles, écrasées à chaque fois)
#
#   * Restauration (manuelle, en interactif uniquement) :
#       source("backup_kubb.R")
#       kubb_restore()                          # dernière sauvegarde
#       kubb_restore("save/kubb_2026-07-04_0800.rds")  # ou une archive précise
#
# Prérequis :
#   * packages DBI et RPostgres
#   * variable d'environnement DATABASE_URL contenant l'URL *PUBLIQUE*
#     de la base Railway (l'URL interne *.railway.internal ne résout
#     pas hors de Railway). Le plus simple : la mettre dans ~/.Renviron,
#     que Rscript lit automatiquement, y compris depuis un planificateur :
#       DATABASE_URL=postgresql://postgres:xxxx@yyyy.proxy.rlwy.net:12345/railway
# ------------------------------------------------------------------

TABLES   <- c("teams", "users", "matches", "bets", "transactions", "meta")
SAVE_DIR <- "save"
KEEP_N   <- 60   # nombre d'archives .rds conservées (rotation)

# Colonnes IDENTITY à réaligner après restauration
ID_COLS <- c(teams = "team_id", users = "user_id", matches = "match_id",
             bets = "bet_id", transactions = "tx_id")

# ------------------------------------------------------------------
# Connexion
# ------------------------------------------------------------------

parse_db_url <- function(url) {
  re <- "^postgres(ql)?://([^:]+):([^@]+)@([^:/]+):?([0-9]+)?/([^?]+)"
  m <- regmatches(url, regexec(re, url))[[1]]
  if (length(m) == 0) stop("DATABASE_URL illisible : ", url)
  list(
    user     = utils::URLdecode(m[3]),
    password = utils::URLdecode(m[4]),
    host     = m[5],
    port     = if (m[6] == "") 5432L else as.integer(m[6]),
    dbname   = sub("/$", "", m[7])
  )
}

kubb_connect <- function() {
  url <- Sys.getenv("DATABASE_URL")
  if (!nzchar(url)) {
    stop("DATABASE_URL absente de l'environnement (voir l'en-tête du script).")
  }
  p <- parse_db_url(url)
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = p$host, port = p$port, dbname = p$dbname,
    user = p$user, password = p$password,
    sslmode = "require",
    bigint = "integer",
    timezone = "Europe/Brussels"
  )
}

# ------------------------------------------------------------------
# Sauvegarde
# ------------------------------------------------------------------

kubb_backup <- function() {
  con <- kubb_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  dump <- lapply(setNames(TABLES, TABLES), function(t) DBI::dbReadTable(con, t))
  
  dir.create(SAVE_DIR, showWarnings = FALSE)
  horodatage <- format(Sys.time(), "%Y-%m-%d_%H%M")
  fichier <- file.path(SAVE_DIR, paste0("kubb_", horodatage, ".rds"))
  saveRDS(dump, fichier)
  
  # Copies CSV lisibles du dernier état (pour inspection rapide)
  csv_dir <- file.path(SAVE_DIR, "dernier_etat")
  dir.create(csv_dir, showWarnings = FALSE)
  for (t in TABLES) {
    utils::write.csv(dump[[t]], file.path(csv_dir, paste0(t, ".csv")),
                     row.names = FALSE)
  }
  
  # Rotation des archives
  archives <- sort(
    list.files(SAVE_DIR, pattern = "^kubb_.*\\.rds$", full.names = TRUE),
    decreasing = TRUE
  )
  if (length(archives) > KEEP_N) {
    file.remove(archives[-seq_len(KEEP_N)])
  }
  
  n <- vapply(dump, nrow, integer(1))
  cat(sprintf("[%s] Sauvegarde OK -> %s\n", Sys.time(), fichier))
  cat(paste(sprintf("  %-13s %5d lignes", names(n), n), collapse = "\n"), "\n")
  invisible(fichier)
}

# ------------------------------------------------------------------
# Restauration (ECRASE les données actuelles de la base !)
# ------------------------------------------------------------------

kubb_restore <- function(fichier = NULL) {
  if (is.null(fichier)) {
    archives <- sort(
      list.files(SAVE_DIR, pattern = "^kubb_.*\\.rds$", full.names = TRUE),
      decreasing = TRUE
    )
    if (length(archives) == 0) stop("Aucune archive trouvée dans ", SAVE_DIR, "/")
    fichier <- archives[1]
  }
  dump <- readRDS(fichier)
  stopifnot(all(TABLES %in% names(dump)))
  
  n <- vapply(dump[TABLES], nrow, integer(1))
  cat("Archive :", fichier, "\n")
  cat(paste(sprintf("  %-13s %5d lignes", names(n), n), collapse = "\n"), "\n")
  cat("ATTENTION : la restauration efface les données actuelles de la base.\n")
  
  if (interactive()) {
    rep <- readline("Taper RESTAURER pour confirmer : ")
    if (!identical(rep, "RESTAURER")) stop("Abandon : restauration non confirmée.")
  } else {
    stop("Par sécurité, kubb_restore() ne s'exécute qu'en session interactive.")
  }
  
  con <- kubb_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  DBI::dbWithTransaction(con, {
    # Vidage : tables filles d'abord (clés étrangères)
    for (t in c("bets", "transactions", "matches", "users", "teams", "meta")) {
      DBI::dbExecute(con, paste("DELETE FROM", t))
    }
    # Réinsertion : tables mères d'abord
    for (t in c("teams", "users", "matches", "bets", "transactions", "meta")) {
      if (nrow(dump[[t]]) > 0) {
        DBI::dbWriteTable(con, t, dump[[t]], append = TRUE, row.names = FALSE)
      }
    }
    # Réaligner les séquences IDENTITY sur les ids restaurés
    # (sinon la prochaine insertion entrerait en collision)
    for (t in names(ID_COLS)) {
      col <- ID_COLS[[t]]
      DBI::dbGetQuery(con, sprintf(
        "SELECT setval(pg_get_serial_sequence('%s','%s'),
                       COALESCE((SELECT MAX(%s) FROM %s), 0) + 1, false)",
        t, col, col, t))
    }
    # Réveiller les sessions Shiny éventuellement connectées
    DBI::dbExecute(con, "UPDATE meta SET version = version + 1")
  })
  
  cat(sprintf("[%s] Restauration OK depuis %s\n", Sys.time(), fichier))
  invisible(fichier)
}

# ------------------------------------------------------------------
# Exécuté via Rscript -> sauvegarde ; sourcé en interactif -> rien
# ------------------------------------------------------------------

if (!interactive()) {
  kubb_backup()
}