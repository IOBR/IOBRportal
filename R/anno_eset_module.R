# =========================================================
# Module: anno_eset
# =========================================================

# ---- Full UI ----
anno_esetUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "anno_eset",
    h3(HTML(
      "Anno_eset
      <span style='font-size:80%; color:#333;'>:
       Annotates an ExpressionSet with gene symbols.</span>"
      )),
    anno_esetBodyUI(id)
  )
}


# ---- BodyUI ----
anno_esetBodyUI <- function(id, include_upload = TRUE) {
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
            uploadUI(ns("upload"))%>%
              helper(
                type    = "markdown",
                icon    = "question-circle",
                size    = "m",
                colour  = "#007bff",
                content = "demo_eset_stad"
              )
          )
        },

        div(
          style = "text-align: center; margin-top: 10px;",
          actionButton(
            inputId = ns("run_anno_eset"),
            label = "Run Analysis",
            class = "btn-primary"
          )
        ),

        selectInput(
          inputId = ns("anno_eset_annotation"),
          label   = "Annotation",
          choices = c(
            "RNA-seq (anno_grch38)"       = "anno_grch38",
            "Affymetrix (anno_hug133plus2)"    = "anno_hug133plus2",
            "Illumina (anno_illumina)"      = "anno_illumina",
            "RNA-seq (Mouse)"       = "anno_gc_vm32"
          ),
          selected = "anno_grch38"
        ),

        selectInput(
          inputId = ns("anno_eset_method"),
          label = "Method",
          choices = c(
            "Mean" = "mean",
            "Sum"  = "sum",
            "Sd"   = "sd"
          ),
          selected = "mean"
        ),

        selectInput(
          inputId = ns("log2"),
          label = "Choose Log2",
          choices = c(
            "True" = "T",
            "False" = "F"
          ),
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
anno_esetServer <- function(id, external_eset = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # 如果 workflow 传入 external_eset（reactive），则优先用它；
    input_data <- if (!is.null(external_eset)) {
      external_eset
    } else {
      uploadServer("upload")
    }

    anno_eset_result <- reactiveVal(NULL)

    observeEvent(input$run_anno_eset, {
      req(input_data())

      withProgress(message = "Running anno_eset analysis...", value = 0, {
        setProgress(0.2, message = "Reading input data...")

        data <- input_data()
        req(nrow(data) > 0)

        annotation_file <- input$anno_eset_annotation
        data(list = annotation_file, package = "IOBR", envir = environment())
        annotation_data <- get(annotation_file, envir = environment())

        anno_eset_probe <- if (annotation_file %in% c("anno_illumina", "anno_hug133plus2")) {
          "probe_id"
        } else {
          "id"
        }

        setProgress(0.5, message = "Running IOBR::anno_eset...")

        anno_eset_data <- tryCatch({
          anno_eset(
              eset       = data,
              annotation = annotation_data,
              method     = input$anno_eset_method,
              probe      = anno_eset_probe
            )
          }, error = function(e) {
            setProgress(1, message = "Error")
            showNotification(
              paste("Error during anno_eset():", e$message),
              type = "error",
              duration = 8
            )
            return(NULL)  
          })

        req(!is.null(anno_eset_data))
        req(anno_eset_data)
        if (input$log2 == "T") {
           setProgress(0.8, message = "Performing Log2 transformation...")
           anno_eset_data <- log2eset(anno_eset_data) 
        }
        
        colnames(anno_eset_data) <- gsub("\\.", "-", colnames(anno_eset_data))
        anno_eset_result(anno_eset_data)

        setProgress(1, message = "Finished")
      })
    })

    dataTableServer("tbl_data", anno_eset_result)

    dataDownloadServer(
      "download",
      data_reactive   = anno_eset_result,
      filename_prefix = "anno_eset_Results"
    )

    return(anno_eset_result)
  })
}

