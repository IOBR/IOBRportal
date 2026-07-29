# =========================================================
# Module: Upload Clinical Data
# =========================================================

# ---- Full UI ----
upload_clinical_dataUI <- function(id) {
  ns <- NS(id)

  bs4TabItem(
    tabName = "upload_clinical_data",
    h3(HTML(
      "Upload Clinical Data
      <span style='font-size:80%; color:#333;'>:
       Uploads and matches clinical data with the mutation matrix for survival analysis.</span>"
    )),
    upload_clinical_dataBodyUI(id)
  )
}


# ---- BodyUI ----
upload_clinical_dataBodyUI <- function(id, include_upload = TRUE) {
  ns <- NS(id)

  fluidRow(
    column(
      width = 3,

      bs4Card(
        title = "Parameter",
        status = "gray",
        headerBorder = TRUE,
        collapsible = TRUE,
        solidHeader = TRUE,
        width = 12,
        maximizable = TRUE,

        if (isTRUE(include_upload)) {
          tagList(
            uploadUI(ns("upload"), label = "Clinical Data") %>%
              helper(
                type = "markdown",
                icon = "question-circle",
                size = "m",
                colour = "#007bff",
                content = "demo_stad_pdata"
              )
          )
        },

        div(
          style = "text-align: center; margin-top: 10px;",
          actionButton(ns("run_upload_clinical_data"), "Run Analysis", class = "btn-primary")
        )
      ),

      dataDownloadUI(ns("download"))
    ),

    column(
      width = 9,

      bs4Card(
        title = "Plot and Data",
        status = "gray",
        headerBorder = TRUE,
        collapsible = TRUE,
        solidHeader = TRUE,
        width = 12,
        maximizable = TRUE,

        tabBox(
          width = 12,
          side = "left",
          tabPanel("Data", dataTableUI(ns("tbl_data")))
        )
      )
    )
  )
}


# ---- Server ----
upload_clinical_dataServer <- function(id, external_mut_matrix) {
  moduleServer(id, function(input, output, session) {

    uploaded_clinical <- uploadServer("upload", mode = "pdata")
    clinical_data_result <- reactiveVal(NULL)

    observeEvent(input$run_upload_clinical_data, {
      req(uploaded_clinical(), external_mut_matrix())

      withProgress(message = "Preparing mutation survival data...", value = 0, {
        setProgress(0.3, message = "Checking and matching sample IDs...")

        result <- tryCatch({
          mutation_data <- as.data.frame(external_mut_matrix(), stringsAsFactors = FALSE, check.names = FALSE)
          mutation_genes <- colnames(mutation_data)
          mutation_data <- tibble::rownames_to_column(mutation_data, var = "ID")

          clinical_data <- as.data.frame(uploaded_clinical(), stringsAsFactors = FALSE, check.names = FALSE)

          if (!"ID" %in% colnames(clinical_data)) stop("The clinical data must contain an ID column.")

          mutation_data$ID <- trimws(as.character(mutation_data$ID))
          clinical_data$ID <- trimws(as.character(clinical_data$ID))

          combined_data <- merge(clinical_data, mutation_data, by = "ID", all = FALSE, sort = FALSE)

          if (nrow(combined_data) == 0) stop("No matching IDs were found between clinical data and mutation matrix.")

          mutation_genes <- intersect(mutation_genes, colnames(combined_data))
          combined_data[mutation_genes] <- lapply(combined_data[mutation_genes], function(x) {
            factor(ifelse(x >= 1, "Mutated", "WT"), levels = c("WT", "Mutated"))
          })

          combined_data
        }, error = function(e) {
          setProgress(1, message = "Error")
          showNotification(paste("Failed to prepare survival data:", e$message), type = "error", duration = 8)
          return(NULL)
        })

        req(result)

        setProgress(0.8, message = "Preparing the matched data table...")
        clinical_data_result(result)
        setProgress(1, message = "Finished")
      })
    })

    dataTableServer("tbl_data", clinical_data_result)

    dataDownloadServer(
      "download",
      data_reactive = clinical_data_result,
      filename_prefix = "mutation_survival_data"
    )

    return(clinical_data_result)
  })
}