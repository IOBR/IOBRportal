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
          label = "Signature Column",
          value = "ID",
          placeholder = "Column name"
        ),

        pickerInput(
          inputId = ns("sig_forest_variables"),
          label = "Signatures",
          choices = NULL,
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            liveSearch = TRUE,
            size = 10,
            selectedTextFormat = "count > 3",
            style = "btn-outline-secondary",
            dropupAuto = FALSE,
            container = "body",
            title = "Please select signatures..."
          )
        ),

        numericInput(
          inputId = ns("sig_forest_n"),
          label = "Signature Number",
          value = 10,
          min = 1,
          max = 50,
          step = 1
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

        sliderInput(
          ns("text_size"),
          "Text Size",
          min = 0,
          max = 30,
          value = 13,
          step = 1
        ),

        # selectInput(
        #   ns("color_option"),
        #   "Color Option",
        #   choices = c("A", "B", "C", "D", "E", "F", "G", "H"),
        #   selected = "A",
        #   width = NULL
        # ),

        textAreaInput(
          inputId = ns("sig_forest_custom_cols"),
          label = "Colors",
          value = "",
          placeholder = "e.g., #E64B35, #4DBBD5, #00A087\nSeparate by comma",
          rows = 3,
          resize = "vertical"
        ),
        br(),
          tags$p(
          style = "color: #555; font-style: italic; font-size: 90%; margin-top: -10px;",
          "Input hex codes or color names separated by comma. If filled, 'Palette' will be ignored."
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

    # 根据Signature Number自动选择signature
    observeEvent(
      list(input_data(), input$sig_forest_signature, input$sig_forest_pvalue, input$sig_forest_hr, input$sig_forest_n),
      {
        req(input_data(), input$sig_forest_n)

        data <- as.data.frame(input_data())
        signature_col <- input$sig_forest_signature
        pvalue_col <- input$sig_forest_pvalue
        hr_col <- input$sig_forest_hr

        if (!is.null(signature_col) &&
            nzchar(signature_col) &&
            signature_col %in% colnames(data) &&
            pvalue_col %in% colnames(data) &&
            hr_col %in% colnames(data)) {

          signature_choices <- unique(as.character(data[[signature_col]]))
          signature_choices <- signature_choices[!is.na(signature_choices) & signature_choices != ""]

          auto_n <- min(as.integer(input$sig_forest_n), length(signature_choices))
          selected_signatures <- signature_choices

          if (length(signature_choices) > auto_n) {
            data$HR_statistic <- as.numeric(data[[hr_col]]) - 1

            selected_signatures <- high_var_fea(
              result = data,
              target = signature_col,
              name_padj = pvalue_col,
              padj_cutoff = 1,
              name_logfc = "HR_statistic",
              logfc_cutoff = 0,
              n = auto_n / 2
            )

            selected_signatures <- as.character(selected_signatures)
          }

          updatePickerInput(
            session = session,
            inputId = "sig_forest_variables",
            choices = signature_choices,
            selected = selected_signatures
          )
        }
      },
      ignoreInit = FALSE
    )

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

      custom_cols_vec <- NULL
      raw_cols_text <- input$sig_forest_custom_cols

      if (!is.null(raw_cols_text) && trimws(raw_cols_text) != "") {
        split_cols <- unlist(strsplit(raw_cols_text, "[,;\n]"))
        split_cols <- trimws(split_cols)
        custom_cols_vec <- split_cols[split_cols != ""]
  
        if (length(custom_cols_vec) == 0) custom_cols_vec <- NULL
  
        if (!is.null(custom_cols_vec) && length(custom_cols_vec) < 2) {
          showNotification("Forest plot colors require at least 2 colors.", type = "error")
          return(NULL)
        }
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

        if (is.null(input$sig_forest_variables) || length(input$sig_forest_variables) == 0) {
          showNotification("Please select at least one Signature.", type = "error")
          return(NULL)
        }

        data <- data[as.character(data[[input$sig_forest_signature]]) %in% input$sig_forest_variables, ]
        data <- data[order(as.numeric(data[[input$sig_forest_hr]]), decreasing = FALSE), ]

        setProgress(0.5, message = "Plotting...")

        pp <- tryCatch({
          sig_forest(
            data           = data,
            signature      = input$sig_forest_signature,
            pvalue         = input$sig_forest_pvalue,
            HR             = input$sig_forest_hr,
            CI_low_0.95    = input$sig_forest_CI_low_0.95,
            CI_up_0.95     = input$sig_forest_CI_up_0.95,
            n              = nrow(data),
            max_character  = 25,
            discrete_width = 35,
            # color_option   = input$color_option,
            text.size      = input$text_size,
            cols = custom_cols_vec
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