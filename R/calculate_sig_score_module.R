# =========================================================
# Module: calculate_sig_score
# =========================================================

# ---- Full UI ----
calculate_sig_scoreUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "calculate_sig_score",
    h3(HTML(
      "Calculate_sig_score
      <span style='font-size:80%; color:#333;'>:
       Computes signature scores using PCA, ssGSEA, z-score, or integration.</span>"
      )),     
    calculate_sig_scoreBodyUI(id)
  )
}


# ---- BodyUI ----
calculate_sig_scoreBodyUI <- function(id, include_upload = TRUE) {
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
            uploadUI(ns("upload")) %>%
              helper(
                type    = "markdown",
                icon    = "question-circle",
                size    = "m",
                colour  = "#007bff",
                content = "demo_stad_tpm"
              )
          )
        },

        div(
          style = "text-align: center; margin-top: 10px;",
          actionButton(
            inputId = ns("run_calculate_sig_score"),
            label = "Run Analysis",
            class = "btn-primary"
          )
        ),

        selectInput(
          inputId = ns("calculate_sig_score_method"),
          label = "Method",
          choices = c(
            "PCA" = "pca",
            "ssGSEA" = "ssgsea",
            "Z-score" = "zscore",
            "Integration" = "integration"
          ),
          selected = "ssgsea"
        ),

        selectInput(
          inputId = ns("calculate_sig_score_signature"),
          label = "Signature",
          choices = c(
            "TME"         = "signature_tme",
            "Metabolism"  = "signature_metabolism",
            "Tumor" = "signature_tumor",
            "Collection"  = "signature_collection",
            "Go_bp"       = "go_bp",
            "Go_cc"       = "go_cc",
            "Go_mf"       = "go_mf",
            "KEGG"        = "kegg",
            "Hallmark"    = "hallmark",
            "Reactome"    = "reactome"
          ),
          selected = "signature_tme"
        ),

        numericInput(
          inputId = ns("calculate_sig_score_mini_gene_count"),
          label = "Mini gene count",
          value = 3,
          min = 2,
          step = 1
        ),

        selectInput(
          inputId = ns("calculate_sig_score_adjust_eset"),
          label = "Adjust eset",
          choices = c("True" = "T", "False" = "F"),
          selected = "T"
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
calculate_sig_scoreServer <- function(id, external_eset = NULL) {
  moduleServer(id, function(input, output, session) {

    # 如果 workflow 传入 external_eset（reactive），则优先用它；
    input_data <- if (!is.null(external_eset)) {
      external_eset
    } else {
      uploadServer("upload")
    }

    sig_score_result <- reactiveVal(NULL)

    observeEvent(input$run_calculate_sig_score, {
      req(input_data())

      withProgress(message = "Running calculate_sig_score analysis...", value = 0, {
        setProgress(0.2, message = "Reading input data...")

        data <- input_data()
        req(nrow(data) > 0)

        setProgress(0.5, message = "Running signature score calculation...")

        selected_signature <- input$calculate_sig_score_signature
        signature_data <- switch(selected_signature,
          "signature_tme"        = IOBR::signature_tme,
          "signature_metabolism" = IOBR::signature_metabolism,
          "signature_collection" = IOBR::signature_collection,
          "go_bp"    = IOBR::go_bp,
          "go_cc"    = IOBR::go_cc,
          "go_mf"    = IOBR::go_mf,
          "kegg"     = IOBR::kegg,
          "hallmark" = IOBR::hallmark,
          "reactome" = IOBR::reactome,
          "signature_tumor"        = IOBR::signature_tumor,
        )

        result <- tryCatch({
          calculate_sig_score(
            eset            = data,
            signature       = signature_data,
            method          = input$calculate_sig_score_method,
            mini_gene_count = input$calculate_sig_score_mini_gene_count,
            adjust_eset     = (input$calculate_sig_score_adjust_eset == "T")
          )
        }, error = function(e) {
          setProgress(1, message = "Error")
          showNotification(
            paste("Error during calculate_sig_score():", e$message),
            type = "error",
            duration = 8
          )
          return(NULL)
        })
        req(result)

        colnames(data) <- gsub("\\.", "-", colnames(data))
        if (is.matrix(result)) result <- as.data.frame(result)

        setProgress(1, message = "Finished")
        sig_score_result(result)
      })
    })

    dataTableServer("tbl_data", sig_score_result)

    dataDownloadServer(
      "download",
      data_reactive   = sig_score_result,
      filename_prefix = "calculate_sig_score_result"
    )

    return(sig_score_result)
  })
}

