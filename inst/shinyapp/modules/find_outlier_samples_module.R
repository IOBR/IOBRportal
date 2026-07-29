# =========================================================
# Module: find_outlier_samples
# =========================================================

# ---- Full UI ----
find_outlier_samplesUI <- function(id) {
  ns <- NS(id)

  bs4TabItem(
    tabName = "find_outlier_samples",
    h3(HTML(
      "Find_outlier_samples
      <span style='font-size:80%; color:#333;'>:
       Identify and remove outlier samples.</span>"
    )),
    find_outlier_samplesBodyUI(id)
  )
}


# ---- Body UI ----
find_outlier_samplesBodyUI <- function(id, include_upload = TRUE, optional = FALSE) {
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
                type = "markdown",
                icon = "question-circle",
                size = "m",
                colour = "#007bff",
                content = "demo_stad_tpm"
              )
          )
        },

        if (isTRUE(optional)) {
          tagList(
            checkboxInput(ns("apply_outlier"), "Apply Outlier Removal", value = TRUE),
            tags$p(
              "Note: It is optional and defaults to True.",
              style = "color: #555; font-style: italic; font-size: 90%; margin-top: -10px;"
            )
          )
        },

        div(
          style = "text-align: center; margin-top: 15px; margin-bottom: 5px;",
          actionButton(ns("run_find_outlier_samples"), "Run Analysis", class = "btn-primary")
        )
      ),

      dataDownloadUI(ns("download_data"))
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

          tabPanel("Data_clean", dataTableUI(ns("tbl_data"))),

          tabPanel(
            "Plot",
            plotDownloadUI(ns("plot_download")),
            uiOutput(ns("find_outlier_samples_plot_container"))
          ),

          tabPanel(
            "Outliers",
            tags$b("Identified Outlier Samples:"),
            verbatimTextOutput(ns("outlier_text"))
          )
        )
      )
    )
  )
}


# ---- Server ----
find_outlier_samplesServer <- function(id, external_eset = NULL, optional = FALSE) {
  moduleServer(id, function(input, output, session) {

    # 1. Input data
    input_data <- if (!is.null(external_eset)) external_eset else uploadServer("upload")

    # 2. Result cache
    res_clean_data <- reactiveVal(NULL)
    res_outliers <- reactiveVal(NULL)
    res_plot_obj <- reactiveVal(NULL)

    # 3. Run analysis
    observeEvent(input$run_find_outlier_samples, {
      req(input_data())

      withProgress(message = "Detecting outlier samples...", value = 0, {
        setProgress(0.2, message = "Reading data...")

        data <- input_data()
        if (!is.null(colnames(data))) colnames(data) <- gsub("\\.", "-", colnames(data))

        setProgress(0.5, message = "Running Analysis...")

        pdf(file = NULL)
        dev.control(displaylist = "enable")

        outliers_vec <- NULL
        plot_recorded <- NULL
        run_success <- FALSE

        tryCatch({
          outliers_vec <- find_outlier_samples(
            eset = data,
            yinter = -3,
            project = NULL,
            plot_hculst = FALSE,
            show_plot = TRUE,
            save = FALSE
          )

          plot_recorded <- recordPlot()
          run_success <- TRUE

        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error", duration = 8)
        })

        dev.off()

        if (run_success) {
          if (!is.null(outliers_vec) && length(outliers_vec) > 0) {
            data_clean <- data[, !(colnames(data) %in% outliers_vec), drop = FALSE]
          } else {
            data_clean <- data
            outliers_vec <- "No outliers detected."
          }

          res_clean_data(data_clean)
          res_outliers(outliers_vec)
          res_plot_obj(plot_recorded)

          setProgress(1, message = "Finished")
        } else {
          setProgress(1, message = "Failed")
        }
      })
    })

    # 4. Select downstream and download data
    res_for_downstream <- reactive({
      req(input_data())

      # Standalone Functions mode
      if (!isTRUE(optional)) {
        req(res_clean_data())
        return(res_clean_data())
      }

      # Workflow mode: outlier removal disabled
      if (!isTRUE(input$apply_outlier)) {
        data_original <- input_data()
        if (!is.null(colnames(data_original))) colnames(data_original) <- gsub("\\.", "-", colnames(data_original))
        return(data_original)
      }

      # Workflow mode: outlier removal enabled
      validate(need(!is.null(res_clean_data()), "Please run Outlier Removal first."))

      res_clean_data()
    })

    # 5. Data table
    dataTableServer("tbl_data", res_clean_data)

    # 6. Outlier list
    output$outlier_text <- renderPrint({
      req(res_outliers())
      cat(paste(res_outliers(), collapse = ", "))
    })

    # 7. Plot
    dims <- plotDownloadServer("plot_download", plot_reactive = res_plot_obj, filename_prefix = "find_outlier_samples_plot")

    output$find_outlier_samples_plot <- renderPlot({
      req(res_plot_obj())
      replayPlot(res_plot_obj())
    })

    output$find_outlier_samples_plot_container <- renderUI({
      req(res_plot_obj())

      div(
        style = "overflow: auto;",
        plotOutput(
          session$ns("find_outlier_samples_plot"),
          width = paste0(dims$width(), "px"),
          height = paste0(dims$height(), "px")
        )
      )
    })

    # 8. Download selected data
    dataDownloadServer("download_data", data_reactive = res_for_downstream, filename_prefix = "outlier_step_output")

    # 9. Return selected data
    return(res_for_downstream)
  })
}