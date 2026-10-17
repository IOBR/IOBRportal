# Local visit statistics for IOBRportal.
# Requires DBI and RSQLite. No external website or browser tracking script.
# Count one new Shiny session as one visit; refreshes count again.
# Store aggregate daily counts only, using the Asia/Shanghai calendar day.
# Use one persistent, writable LOCAL database file for all app processes.

iobr_record_visit <- function(db_path) {
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA busy_timeout = 3000")

  DBI::dbExecute(con, paste(
    "CREATE TABLE IF NOT EXISTS daily_visits (",
    "day TEXT PRIMARY KEY, visits INTEGER NOT NULL)"
  ))
  day <- format(Sys.time(), format = "%Y-%m-%d", tz = "Asia/Shanghai")

  # One atomic increment avoids overwriting concurrent visits.
  DBI::dbExecute(con, paste(
    "INSERT INTO daily_visits (day, visits) VALUES (?, 1)",
    "ON CONFLICT(day) DO UPDATE SET visits = daily_visits.visits + 1"
  ), params = list(day))

  DBI::dbGetQuery(con, paste(
    "SELECT COALESCE(SUM(CASE WHEN day = ? THEN visits ELSE 0 END), 0) AS today,",
    "COALESCE(SUM(visits), 0) AS total FROM daily_visits"
  ), params = list(day))
}

# Replace the entire old map card with iobr_visits_ui().
# This fixed output ID is intentionally registered in the ROOT app server;
# do not wrap this helper in the home module's ns(). Include it only once.
iobr_visits_ui <- function() {
  shiny::div(
    class = "iobr-resource-card",
    style = paste(
      "width: 100%; max-width: 320px; margin: 0 auto;",
      "box-sizing: border-box; padding: 18px 20px;",
      "background: #f8fbff; border: 1px solid #e3edf8; border-radius: 12px;"
    ),
    shiny::div(
      class = "iobr-resource-title",
      style = "margin: 0 0 16px; font-size: 15px; color: #334155;",
      "Visitor Statistics"
    ),
    shiny::uiOutput("iobr_visit_counts")
  )
}

# Call once in the ROOT server, rather than inside home_introServer().
iobr_visits_server <- function(output, session, db_path = Sys.getenv(
                                 "IOBR_VISITS_DB",
                                 unset = file.path("data", "visitor_stats.sqlite")
                               )) {
  counts <- shiny::reactiveVal(NULL)

  # Write once after the initial reactive flush, not on every UI update.
  # A statistics error must not prevent the rest of the application running.
  session$onFlushed(function() {
    counts(tryCatch(
      iobr_record_visit(db_path),
      error = function(e) {
        message("IOBRportal visit statistics: ", conditionMessage(e))
        NULL
      }
    ))
  }, once = TRUE)

  output$iobr_visit_counts <- shiny::renderUI({
    x <- counts()
    fmt <- function(n) format(n, big.mark = ",", scientific = FALSE, trim = TRUE)
    values <- if (is.null(x)) c("\u2014", "\u2014") else c(fmt(x$today), fmt(x$total))

    metric <- function(value, label, divider = FALSE) {
      shiny::div(
        style = paste0(
          "flex: 1; min-width: 0; text-align: center; padding: 0 8px;",
          if (divider) "border-left: 1px solid #dce7f2;" else ""
        ),
        shiny::div(
          style = paste(
            "font-size: 28px; font-weight: 700; line-height: 1.2;",
            "color: #007bff; font-variant-numeric: tabular-nums;"
          ),
          value
        ),
        shiny::div(
          style = "margin-top: 5px; font-size: 12px; color: #64748b;",
          label
        )
      )
    }

    shiny::div(
      style = "display: flex; align-items: center;",
      metric(values[1], "Today's visits"),
      metric(values[2], "Total visits", divider = TRUE)
    )
  })
}
