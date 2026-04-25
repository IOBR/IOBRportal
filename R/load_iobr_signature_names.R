# ------------------------------
# IOBR signature names loader
# ------------------------------
iobr_signature_names_dir <- "data/iobr_signature_names"
load_iobr_signature_names <- function(data_name) {
  
  names_file <- file.path(
    iobr_signature_names_dir,
    paste0(data_name, "_names.rds")
  )
  if (!file.exists(names_file)) {
    stop(
      paste0(
        "Local signature names file not found: ", names_file, "\n",
        "Please run scripts/cache_iobr_signature_names.R first."
      ),
      call. = FALSE
    )
  }
  
  sig_names <- readRDS(names_file)
  if (!is.character(sig_names) || length(sig_names) == 0) {
    stop(
      paste0("Invalid signature names file: ", names_file),
      call. = FALSE
    )
  }
  return(sig_names)
}
