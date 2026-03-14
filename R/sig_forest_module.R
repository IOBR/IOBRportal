# =========================================================
# Module: sig_forest
# =========================================================

sig_forestUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "sig_forest",
    h3(HTML(
      "Sig_forest
      <span style='font-size:80%; color:#333;'>:
       Produces forest plots from batch survival results.</span>"
      )),
    sig_forestBodyUI(id)
  )
}

sig_forestBodyUI <- function(id, include_upload = TRUE) {
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
                content = "demo_batch_surv"
              )
          )
        },


        div(
          style = "text-align: center; margin-top: 0px; margin-bottom: 0px;",
          actionButton(
            inputId = ns("run_sig_forest"),
            label = "Run Analysis",
            class = "btn-primary"
          )
        ),
        br(),
        tags$p(
          style = "color: #555; font-style: italic; font-size: 90%; margin-top: -10px;",
          "Note: Supports data from Signature Data rather than TME data."
        ),

        textInput(
          inputId = ns("sig_forest_signature"),
          label = "Signature",
          value = "ID",
          placeholder = "Column name (e.g., TMEscore_plus)"
        ),

        textInput(
          inputId = ns("sig_forest_pvalue"),
          label = "P-value",
          value = "P",
          placeholder = "Column name (e.g., p_value)"
        ),

        textInput(
          inputId = ns("sig_forest_hr"),
          label = "HR",
          value = "HR",
          placeholder = "Column name (e.g., HR)"
        ),

        textInput(
          inputId = ns("sig_forest_CI_low_0.95"),
          label = "Lower CI (95%)",
          value = "CI_low_0.95",
          placeholder = "Column name (e.g., CI_low_0.95)"
        ),

        textInput(
          inputId = ns("sig_forest_CI_up_0.95"),
          label = "Upper CI (95%)",
          value = "CI_up_0.95",
          placeholder = "Column name (e.g., CI_up_0.95)"
        ),

        numericInput(
          inputId = ns("sig_forest_n"),
          label = "Signature Number",
          value = 10,
          min = 1,
          max = 50,
          step = 1
        ),

        sliderInput(
          ns("Text Size"),
          "text_size",
          min = 0,
          max = 30,
          value = 13,
          step = 1
        ),

        selectInput(
          ns("color_option"),
          "Color Option",
          choices = c("A", "B", "C", "D", "E", "F", "G", "H"),
          selected = "A",
          width = NULL
        )
      )
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
          tabPanel(
            "Plot", 
            plotDownloadUI(ns("plot_download")),
            uiOutput(ns("sig_forest_plot_container"))
          )
        )
      )
    )
  )
}



sig_forestServer <- function(id, external_eset = NULL) {
  moduleServer(id, function(input, output, session) {

    input_data <- if (!is.null(external_eset)) {
      external_eset
    } else {
      uploadServer("upload")
    }

    sig_forest_result <- reactiveVal(NULL)

    observeEvent(input$run_sig_forest, {
      req(input_data())

      if (nchar(input$sig_forest_signature) == 0 ||
          nchar(input$sig_forest_pvalue) == 0 ||
          nchar(input$sig_forest_hr) == 0 ||
          nchar(input$sig_forest_CI_low_0.95) == 0 ||
          nchar(input$sig_forest_CI_up_0.95) == 0) {
        showNotification("Please fill in all required text inputs.", type = "error")
        return(NULL)
      }

      withProgress(message = "Generating forest plot...", value = 0, {
        setProgress(0.2, message = "Reading data...")

        data <- input_data()
        data <- as.data.frame(data)

        required_cols <- c(
          input$sig_forest_signature,
          input$sig_forest_pvalue,
          input$sig_forest_hr,
          input$sig_forest_CI_low_0.95,
          input$sig_forest_CI_up_0.95
        )

        missing_cols <- required_cols[!required_cols %in% colnames(data)]
        if (length(missing_cols) > 0) {
          showNotification(paste("Missing columns:", paste(missing_cols, collapse = ", ")), type = "error")
          return(NULL)
        }

        setProgress(0.5, message = "Plotting...")

        pp <- tryCatch({
          sig_forest(
            data           = data,
            signature      = input$sig_forest_signature,
            pvalue         = input$sig_forest_pvalue,
            HR             = input$sig_forest_hr,
            CI_low_0.95    = input$sig_forest_CI_low_0.95,
            CI_up_0.95     = input$sig_forest_CI_up_0.95,
            n              = input$sig_forest_n,
            max_character  = 25,
            discrete_width = 35,
            color_option   = input$color_option,
            text.size      = input$text_size
          )
        }, error = function(e) {
          setProgress(1, message = "Error")
          showNotification(
            paste("Error during sig_forest():", e$message),
            type = "error",
            duration = 8
          )
          return(NULL)
        })
        
        req(pp)

        setProgress(1, message = "Finished")
        sig_forest_result(pp)
      })
    })

    # 1. 绘图内容渲染 (ID 对应 uiOutput 内部的 plotOutput)
    output$plot_output <- renderPlot({
      req(sig_forest_result())
      sig_forest_result()
    })

    # 2. 调用下载与尺寸控制模块
    dims <- plotDownloadServer(
      id = "plot_download", 
      plot_reactive = sig_forest_result, 
      filename_prefix = "sig_forest_plot"
    )

    # 3. 动态渲染 UI 容器
    output$sig_forest_plot_container <- renderUI({
      req(sig_forest_result())
      
      div(style = "overflow: auto;",
          plotOutput(session$ns("plot_output"), 
                     width = paste0(dims$width(), "px"), 
                     height = paste0(dims$height(), "px"))
      )
    })

    return(sig_forest_result)
  })
}

