# ------------------------------------------------------------------
# db.R — Connexion, schéma et helpers d'accès à la base SQLite
# ------------------------------------------------------------------
# La base vit dans data/kubb.sqlite. Au premier lancement, les tables
# sont créées et le calendrier est généré (ou lu depuis data/teams.csv
# et data/matches.csv si ces fichiers existent).
# ------------------------------------------------------------------

DB_PATH <- "data/kubb.sqlite"

# Opérateur "valeur par défaut" (présent dans R >= 4.4 et rlang, redéfini par sûreté)
`%||%` <- function(a, b) if (is.null(a)) b else a

# Pseudos disposant des droits d'administration (en plus du flag is_admin en base)
ADMIN_PSEUDOS <- c("thomas")

CREDIT_INITIAL <- 1000
MISE_MIN <- 1
MISE_MAX <- 100

db_connect <- function(path = DB_PATH) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL;")
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000;")
  con
}

# ------------------------------------------------------------------
# Initialisation du schéma et seed
# ------------------------------------------------------------------

db_init <- function(con, date_debut = next_saturday()) {

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS meta (version INTEGER NOT NULL)")
  if (DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM meta")$n == 0) {
    DBI::dbExecute(con, "INSERT INTO meta (version) VALUES (0)")
  }

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS users (
      user_id    INTEGER PRIMARY KEY AUTOINCREMENT,
      nom        TEXT NOT NULL,
      pseudo     TEXT NOT NULL UNIQUE COLLATE NOCASE,
      password   TEXT NOT NULL,
      statcoins  REAL NOT NULL DEFAULT 0,   -- crédité via transactions
      is_admin   INTEGER NOT NULL DEFAULT 0,
      created_at TEXT NOT NULL DEFAULT (datetime('now', 'localtime'))
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS teams (
      team_id INTEGER PRIMARY KEY AUTOINCREMENT,
      nom     TEXT NOT NULL UNIQUE
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS matches (
      match_id   INTEGER PRIMARY KEY AUTOINCREMENT,
      journee    INTEGER NOT NULL,
      date_match TEXT NOT NULL,            -- 'YYYY-MM-DD HH:MM'
      home_id    INTEGER NOT NULL REFERENCES teams(team_id),
      away_id    INTEGER NOT NULL REFERENCES teams(team_id),
      played     INTEGER NOT NULL DEFAULT 0,
      score_home INTEGER,
      score_away INTEGER
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS bets (
      bet_id    INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id   INTEGER NOT NULL REFERENCES users(user_id),
      match_id  INTEGER NOT NULL REFERENCES matches(match_id),
      type      TEXT NOT NULL,             -- 'vainqueur' | 'ecart'
      selection TEXT NOT NULL,             -- team_id (en texte) ou '1-2' / '3-5' / '6+'
      mise      REAL NOT NULL,
      cote      REAL NOT NULL,             -- cote figée au moment du pari
      settled   INTEGER NOT NULL DEFAULT 0,
      gain      REAL,
      placed_at TEXT NOT NULL DEFAULT (datetime('now', 'localtime'))
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS transactions (
      tx_id   INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL REFERENCES users(user_id),
      montant REAL NOT NULL,
      motif   TEXT NOT NULL,
      ts      TEXT NOT NULL DEFAULT (datetime('now', 'localtime'))
    )")

  # --- Seed des équipes -------------------------------------------
  if (DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM teams")$n == 0) {
    if (file.exists("data/teams.csv")) {
      teams <- readr::read_csv("data/teams.csv", show_col_types = FALSE)
      purrr::walk(teams$nom, function(x) {
        DBI::dbExecute(con, "INSERT INTO teams (nom) VALUES (?)", params = list(x))
      })
    } else {
      defaut <- c(
        "Les B\u00fbcherons", "Kubb Kong", "Viking Social Club",
        "Les Rois Tomb\u00e9s", "Bois Sans Soif", "La Garde Royale",
        "Lancer Franc", "Les Valkubbries"
      )
      purrr::walk(defaut, function(x) {
        DBI::dbExecute(con, "INSERT INTO teams (nom) VALUES (?)", params = list(x))
      })
    }
  }

  # --- Seed du calendrier ------------------------------------------
  if (DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM matches")$n == 0) {
    if (file.exists("data/matches.csv")) {
      # colonnes attendues : journee, date_match, home (nom), away (nom)
      mt <- readr::read_csv("data/matches.csv", show_col_types = FALSE)
      teams <- DBI::dbGetQuery(con, "SELECT team_id, nom FROM teams")
      mt$home_id <- teams$team_id[match(mt$home, teams$nom)]
      mt$away_id <- teams$team_id[match(mt$away, teams$nom)]
      purrr::pwalk(
        mt[, c("journee", "date_match", "home_id", "away_id")],
        function(journee, date_match, home_id, away_id) {
          DBI::dbExecute(con, "
            INSERT INTO matches (journee, date_match, home_id, away_id)
            VALUES (?, ?, ?, ?)",
            params = list(journee, as.character(date_match), home_id, away_id))
        }
      )
    } else {
      seed_round_robin(con, date_debut)
    }
  }

  invisible(con)
}

next_saturday <- function(from = Sys.Date()) {
  u <- as.integer(format(from, "%u"))  # lundi = 1 ... dimanche = 7
  d <- (6 - u) %% 7
  if (d == 0) d <- 7
  from + d
}

# Calendrier round-robin (méthode du cercle) : 8 équipes, 7 journées,
# 4 matchs par journée, une journée par semaine, matchs à 14h, 15h, 16h, 17h.
seed_round_robin <- function(con, date_debut) {
  ids <- DBI::dbGetQuery(con, "SELECT team_id FROM teams ORDER BY team_id")$team_id
  stopifnot(length(ids) == 8)
  n <- length(ids)
  rotation <- ids[-1]

  for (j in seq_len(n - 1)) {
    ligne <- c(ids[1], rotation)
    date_j <- date_debut + (j - 1) * 7
    for (k in seq_len(n / 2)) {
      home <- ligne[k]
      away <- ligne[n + 1 - k]
      heure <- sprintf("%02d:00", 13 + k)  # 14:00 à 17:00
      DBI::dbExecute(con, "
        INSERT INTO matches (journee, date_match, home_id, away_id)
        VALUES (?, ?, ?, ?)",
        params = list(j, paste(format(date_j), heure), home, away))
    }
    rotation <- c(rotation[length(rotation)], rotation[-length(rotation)])
  }
}

# ------------------------------------------------------------------
# Versionnage léger : permet à toutes les sessions de se rafraîchir
# (reactivePoll dans app.R) quand n'importe qui écrit en base.
# ------------------------------------------------------------------

db_touch <- function(con) {
  DBI::dbExecute(con, "UPDATE meta SET version = version + 1")
}

db_version <- function(con) {
  DBI::dbGetQuery(con, "SELECT version FROM meta")$version[1]
}

# ------------------------------------------------------------------
# Helpers de lecture
# ------------------------------------------------------------------

get_user <- function(con, user_id) {
  DBI::dbGetQuery(con, "SELECT * FROM users WHERE user_id = ?",
                  params = list(user_id))
}

get_user_by_pseudo <- function(con, pseudo) {
  DBI::dbGetQuery(con, "SELECT * FROM users WHERE pseudo = ? COLLATE NOCASE",
                  params = list(pseudo))
}

get_teams <- function(con) {
  DBI::dbGetQuery(con, "SELECT * FROM teams ORDER BY nom")
}

get_matches <- function(con) {
  DBI::dbGetQuery(con, "
    SELECT m.*, th.nom AS home, ta.nom AS away
    FROM matches m
    JOIN teams th ON th.team_id = m.home_id
    JOIN teams ta ON ta.team_id = m.away_id
    ORDER BY m.journee, m.date_match, m.match_id")
}

get_bets <- function(con, user_id = NULL) {
  base <- "
    SELECT b.*, u.pseudo, m.journee, m.date_match, m.home_id, m.away_id,
           th.nom AS home, ta.nom AS away
    FROM bets b
    JOIN users u   ON u.user_id  = b.user_id
    JOIN matches m ON m.match_id = b.match_id
    JOIN teams th  ON th.team_id = m.home_id
    JOIN teams ta  ON ta.team_id = m.away_id"
  if (is.null(user_id)) {
    DBI::dbGetQuery(con, paste(base, "ORDER BY b.placed_at DESC"))
  } else {
    DBI::dbGetQuery(con, paste(base, "WHERE b.user_id = ? ORDER BY b.placed_at DESC"),
                    params = list(user_id))
  }
}

get_transactions <- function(con) {
  DBI::dbGetQuery(con, "
    SELECT t.*, u.pseudo
    FROM transactions t
    JOIN users u ON u.user_id = t.user_id
    ORDER BY t.ts, t.tx_id")
}

# ------------------------------------------------------------------
# Écriture : toute variation de StatCoins passe par le grand livre
# ------------------------------------------------------------------

add_transaction <- function(con, user_id, montant, motif) {
  DBI::dbExecute(con, "
    INSERT INTO transactions (user_id, montant, motif) VALUES (?, ?, ?)",
    params = list(user_id, montant, motif))
  DBI::dbExecute(con, "
    UPDATE users SET statcoins = statcoins + ? WHERE user_id = ?",
    params = list(montant, user_id))
}
