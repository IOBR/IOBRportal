# =========================================================
# IOBRportal: Uploaded Data Privacy and Temporary File Cleanup
# =========================================================
#
# Recommended cloud-app location:
#   IOBRportal/privacy.R
#
# Load once from global.R, or near the top of app.R before ui/server:
#   source("privacy.R", local = FALSE)
#
# Main server usage:
#   server <- function(input, output, session) {
#     iobr_register_privacy_cleanup(session)
#     ...
#   }
#
# Uploaded files must be passed through iobr_copy_uploaded_file().
# Any result files that need an application-managed temporary path can use
# iobr_privacy_tempfile().


# Process-level state. This prevents creation of one global cleanup scheduler
# for every connected user.
.iobr_privacy_state <- new.env(parent = emptyenv())
.iobr_privacy_state$cleanup_started <- FALSE
.iobr_privacy_state$cleanup_observer <- NULL


iobr_privacy_retention_hours <- function() {
  value <- getOption("iobrportal.upload_retention_hours", 24)
  value <- suppressWarnings(as.numeric(value)[1])

  if (is.na(value) || !is.finite(value) || value <= 0) {
    value <- 24
  }

  value
}


iobr_privacy_cleanup_interval_minutes <- function() {
  value <- getOption("iobrportal.upload_cleanup_interval_minutes", 60)
  value <- suppressWarnings(as.numeric(value)[1])

  if (is.na(value) || !is.finite(value) || value <= 0) {
    value <- 60
  }

  value
}


iobr_privacy_heartbeat_minutes <- function() {
  value <- getOption("iobrportal.upload_heartbeat_minutes", 30)
  value <- suppressWarnings(as.numeric(value)[1])

  if (is.na(value) || !is.finite(value) || value <= 0) {
    value <- 30
  }

  value
}


iobr_make_private_dir <- function(path) {
  path <- path.expand(as.character(path)[1])

  if (is.na(path) || !nzchar(path)) {
    stop("The protected temporary-directory path is invalid.")
  }

  if (file.exists(path) && !dir.exists(path)) {
    stop("The protected temporary-directory path points to a file.")
  }

  if (!dir.exists(path)) {
    dir.create(
      path,
      recursive = TRUE,
      showWarnings = FALSE,
      mode = "0700"
    )
  }

  if (!dir.exists(path)) {
    stop("Failed to create the protected temporary directory.")
  }

  # Effective on POSIX systems. On platforms that do not support POSIX
  # permissions, access is still limited by the application process/account.
  try(Sys.chmod(path, mode = "0700"), silent = TRUE)

  invisible(path)
}


iobr_privacy_root <- function() {
  # dirname(tempdir()) is stable across ordinary R process restarts, unlike
  # tempdir() itself. A deployment-specific path can be supplied through either
  # the R option or the environment variable below.
  default_root <- file.path(
    dirname(tempdir()),
    "IOBRportal_user_uploads"
  )

  environment_root <- Sys.getenv(
    "IOBRPORTAL_UPLOAD_ROOT",
    unset = default_root
  )

  root <- getOption(
    "iobrportal.upload_root",
    environment_root
  )

  root <- path.expand(as.character(root)[1])

  if (is.na(root) || !nzchar(root)) {
    stop("The IOBRportal upload-root path is invalid.")
  }

  iobr_make_private_dir(root)
  normalizePath(root, winslash = "/", mustWork = TRUE)
}


iobr_privacy_safe_name <- function(x, fallback = "file") {
  if (length(x) == 0 || is.na(x[1])) {
    return(fallback)
  }

  x <- basename(as.character(x[1]))
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("^_+|_+$", "", x)

  if (!nzchar(x) || x %in% c(".", "..")) {
    x <- fallback
  }

  x
}


iobr_privacy_safe_token <- function(session) {
  token <- tryCatch(
    session$token,
    error = function(e) NULL
  )

  if (
    length(token) != 1 ||
    is.na(token) ||
    !nzchar(as.character(token))
  ) {
    token <- paste(
      "session",
      Sys.getpid(),
      format(Sys.time(), "%Y%m%d%H%M%OS6"),
      sample.int(1e9, 1),
      sep = "_"
    )
  }

  iobr_privacy_safe_name(token, fallback = "session")
}


iobr_privacy_session_dir <- function(session) {
  path <- file.path(
    iobr_privacy_root(),
    iobr_privacy_safe_token(session)
  )

  iobr_make_private_dir(path)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}


iobr_touch_privacy_path <- function(path) {
  if (length(path) == 1 && (dir.exists(path) || file.exists(path))) {
    try(Sys.setFileTime(path, Sys.time()), silent = TRUE)
  }

  invisible(path)
}


iobr_cleanup_expired_uploads <- function(
    retention_hours = iobr_privacy_retention_hours()) {
  retention_hours <- suppressWarnings(as.numeric(retention_hours)[1])

  if (
    is.na(retention_hours) ||
    !is.finite(retention_hours) ||
    retention_hours <= 0
  ) {
    retention_hours <- iobr_privacy_retention_hours()
  }

  root <- iobr_privacy_root()
  cutoff <- Sys.time() - retention_hours * 60 * 60

  entries <- list.files(
    root,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )

  if (length(entries) == 0) {
    return(invisible(character(0)))
  }

  info <- file.info(entries)
  expired <- entries[
    !is.na(info$mtime) &
      info$mtime < cutoff
  ]

  if (length(expired) == 0) {
    return(invisible(character(0)))
  }

  removed <- character(0)

  for (path in expired) {
    status <- tryCatch(
      unlink(path, recursive = TRUE, force = TRUE),
      error = function(e) 1L
    )

    if (identical(status, 0L) || identical(status, 0)) {
      removed <- c(removed, path)
    }
  }

  invisible(removed)
}


iobr_start_privacy_cleanup <- function(
    retention_hours = iobr_privacy_retention_hours(),
    interval_minutes = iobr_privacy_cleanup_interval_minutes()) {
  if (isTRUE(.iobr_privacy_state$cleanup_started)) {
    return(invisible(.iobr_privacy_state$cleanup_observer))
  }

  interval_minutes <- suppressWarnings(as.numeric(interval_minutes)[1])

  if (
    is.na(interval_minutes) ||
    !is.finite(interval_minutes) ||
    interval_minutes <= 0
  ) {
    interval_minutes <- iobr_privacy_cleanup_interval_minutes()
  }

  # Clean residual directories immediately when the first application session
  # starts. This also discovers files left by an earlier R process.
  try(
    iobr_cleanup_expired_uploads(retention_hours),
    silent = TRUE
  )

  cleanup_observer <- shiny::observe(
    {
      shiny::invalidateLater(
        interval_minutes * 60 * 1000,
        session = NULL
      )

      try(
        iobr_cleanup_expired_uploads(retention_hours),
        silent = TRUE
      )
    },
    domain = NULL,
    label = "iobrportal_privacy_cleanup"
  )

  .iobr_privacy_state$cleanup_observer <- cleanup_observer
  .iobr_privacy_state$cleanup_started <- TRUE

  invisible(cleanup_observer)
}


iobr_register_privacy_cleanup <- function(
    session,
    retention_hours = iobr_privacy_retention_hours(),
    heartbeat_minutes = iobr_privacy_heartbeat_minutes()) {
  if (isTRUE(session$userData$iobr_privacy_cleanup_registered)) {
    session_dir <- session$userData$iobr_privacy_session_dir
    iobr_touch_privacy_path(session_dir)
    return(invisible(session_dir))
  }

  heartbeat_minutes <- suppressWarnings(as.numeric(heartbeat_minutes)[1])

  if (
    is.na(heartbeat_minutes) ||
    !is.finite(heartbeat_minutes) ||
    heartbeat_minutes <= 0
  ) {
    heartbeat_minutes <- iobr_privacy_heartbeat_minutes()
  }

  session_dir <- iobr_privacy_session_dir(session)
  iobr_touch_privacy_path(session_dir)

  # Starts only once per R process.
  iobr_start_privacy_cleanup(retention_hours = retention_hours)

  # Keep an active session directory from being classified as expired.
  heartbeat_observer <- shiny::observe(
    {
      shiny::invalidateLater(
        heartbeat_minutes * 60 * 1000,
        session = session
      )

      iobr_touch_privacy_path(session_dir)
    },
    domain = session,
    label = "iobrportal_privacy_heartbeat"
  )

  session$userData$iobr_privacy_cleanup_registered <- TRUE
  session$userData$iobr_privacy_session_dir <- session_dir
  session$userData$iobr_privacy_heartbeat <- heartbeat_observer

  session$onSessionEnded(function() {
    try(heartbeat_observer$destroy(), silent = TRUE)
    try(
      unlink(session_dir, recursive = TRUE, force = TRUE),
      silent = TRUE
    )

    # Also opportunistically remove residual directories from other sessions.
    try(
      iobr_cleanup_expired_uploads(retention_hours),
      silent = TRUE
    )
  })

  invisible(session_dir)
}


iobr_copy_uploaded_file <- function(
    upload_info,
    session,
    purpose = "upload") {
  if (
    is.null(upload_info) ||
    is.null(upload_info$datapath)
  ) {
    stop("No uploaded file was provided.")
  }

  datapath <- as.character(upload_info$datapath)

  if (
    length(datapath) != 1 ||
    is.na(datapath) ||
    !nzchar(datapath) ||
    !file.exists(datapath)
  ) {
    stop("No valid uploaded file was provided.")
  }

  # Registration is idempotent, so this is safe even when it has already been
  # called from the main server.
  session_dir <- iobr_register_privacy_cleanup(session)

  upload_dir <- file.path(
    session_dir,
    iobr_privacy_safe_name(purpose, fallback = "upload")
  )
  iobr_make_private_dir(upload_dir)

  original_name <- if (
    !is.null(upload_info$name) &&
      length(upload_info$name) >= 1 &&
      !is.na(upload_info$name[1])
  ) {
    as.character(upload_info$name[1])
  } else {
    "uploaded_file"
  }

  original_name <- basename(original_name)

  raw_extension <- tools::file_ext(original_name)
  extension <- gsub("[^A-Za-z0-9]+", "", raw_extension)

  raw_stem <- tools::file_path_sans_ext(original_name)
  stem <- iobr_privacy_safe_name(
    raw_stem,
    fallback = "uploaded_file"
  )

  target <- tempfile(
    pattern = paste0(stem, "_"),
    tmpdir = upload_dir,
    fileext = if (nzchar(extension)) {
      paste0(".", extension)
    } else {
      ""
    }
  )

  copied <- file.copy(
    from = datapath,
    to = target,
    overwrite = FALSE,
    copy.mode = FALSE,
    copy.date = FALSE
  )

  if (!isTRUE(copied) || !file.exists(target)) {
    try(unlink(target, force = TRUE), silent = TRUE)
    stop(
      paste(
        "Failed to copy the uploaded file into the protected",
        "temporary directory."
      )
    )
  }

  try(Sys.chmod(target, mode = "0600"), silent = TRUE)
  iobr_touch_privacy_path(session_dir)

  normalizePath(target, winslash = "/", mustWork = TRUE)
}


iobr_privacy_tempfile <- function(
    session,
    purpose = "results",
    pattern = "result_",
    fileext = "") {
  session_dir <- iobr_register_privacy_cleanup(session)

  output_dir <- file.path(
    session_dir,
    iobr_privacy_safe_name(purpose, fallback = "results")
  )
  iobr_make_private_dir(output_dir)

  pattern <- iobr_privacy_safe_name(
    pattern,
    fallback = "result_"
  )

  fileext <- as.character(fileext)[1]
  if (is.na(fileext)) {
    fileext <- ""
  }
  fileext <- sub("^\\.", "", fileext)
  fileext <- gsub("[^A-Za-z0-9]+", "", fileext)

  target <- tempfile(
    pattern = pattern,
    tmpdir = output_dir,
    fileext = if (nzchar(fileext)) {
      paste0(".", fileext)
    } else {
      ""
    }
  )

  iobr_touch_privacy_path(session_dir)
  normalizePath(target, winslash = "/", mustWork = FALSE)
}