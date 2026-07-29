# =========================================================
# Module: tme_cluster
# =========================================================

tme_clusterUI <- function(id) {
  ns <- NS(id)

  bs4TabItem(
    tabName = "tme_cluster",
    h3(HTML(
      "Tme_cluster
      <span style='font-size:80%; color:#333;'>:
       Performs tumor microenvironment clustering analysis.</span>"
    )),
    tme_clusterBodyUI(id)
  )
}


# ---- Body UI ----
tme_clusterBodyUI <- function(id, include_upload = TRUE, only_table = FALSE) {
  ns <- NS(id)

  fluidRow(
    # Left Panel: Parameters / Upload / Download
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

        # Upload
        if (isTRUE(include_upload)) {
          tagList(
            uploadUI(ns("upload")) %>%
              helper(
                type = "markdown",
                icon = "question-circle",
                size = "m",
                colour = "#007bff",
                content = "demo_stad_sig"
              )
          )
        },

        tags$p(
          "Optional: select interesting features for TME clustering; leave empty to skip this step.",
          style = "color: #555; font-style: italic; font-size: 90%;"
        ),

        div(
          style = "text-align: center; margin-top: 10px;",
          actionButton(ns("run_tme_cluster"), "Run Analysis", class = "btn-primary")
        ),

        # Feature Selection
        pickerInput(
          inputId = ns("tme_cluster_features"),
          label = "Features",
          choices = NULL,
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            liveSearch = TRUE,
            size = 10,
            selectedTextFormat = "count > 3",
            countSelectedText = "{0} features selected",
            style = "btn-outline-secondary",
            dropupAuto = FALSE,
            container = "body",
            title = "Please select features..."
          )
        ),

        numericInput(ns("tme_cluster_min_nc"), "Min Clusters", value = 2, min = 2, max = 20, step = 1),
        numericInput(ns("tme_cluster_max_nc"), "Max Clusters", value = 6, min = 2, max = 20, step = 1)
      ),

      dataDownloadUI(ns("download"))
    ),

    # Right Panel: Plots and Data
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

        do.call(
          tabBox,
          c(
            list(width = 12, side = "left"),

            if (!isTRUE(only_table)) {
              list(
                tabPanel(
                  "Plot",
                  plotDownloadUI(ns("plot_download")),
                  uiOutput(ns("tme_cluster_plot_container"))
                ),
                tabPanel("Data", dataTableUI(ns("tbl_data"))),
                tabPanel("Cluster", textOutput(ns("tme_cluster_cluster")))
              )
            } else {
              list(
                tabPanel("Data", dataTableUI(ns("tbl_data"))),
                tabPanel("Cluster", textOutput(ns("tme_cluster_cluster")))
              )
            }
          )
        )
      )
    )
  )
}


# ---- Server ----
tme_clusterServer <- function(id, external_eset = NULL, only_table = FALSE) {
  moduleServer(id, function(input, output, session) {

    # 1. Input data
    input_data <- if (!is.null(external_eset)) external_eset else uploadServer("upload")

    # Result cache
    tme_cluster_result <- reactiveVal(NULL)
    tme_cluster_plot <- reactiveVal(NULL)

    # 2. Update feature choices
    observeEvent(input_data(), {
      req(input_data())

      data <- input_data()
      colnames(data) <- gsub("\\.", "-", colnames(data))

      all_cols <- colnames(data)
      is_num <- sapply(data, is.numeric)
      pool_numeric <- all_cols[is_num]

      blacklist_pattern <- "(^|_)(time|status|os|event|censored|days|months|years|fustat|futime|rfs|pfs|dfs)(_|$)"
      is_clinical <- grepl(blacklist_pattern, pool_numeric, ignore.case = TRUE)
      numeric_cols <- pool_numeric[!is_clinical]

      updatePickerInput(
        session = session,
        inputId = "tme_cluster_features",
        choices = numeric_cols,
        selected = character(0)
      )
    })

    # 3. Run clustering
    observeEvent(input$run_tme_cluster, {
      req(input_data())

      data <- input_data()
      colnames(data) <- gsub("\\.", "-", colnames(data))

      if (is.null(input$tme_cluster_features) || length(input$tme_cluster_features) == 0) {
        showNotification("Please select at least one feature.", type = "error")
        return(NULL)
      }

      withProgress(message = "Running tme_cluster analysis...", value = 0, {
        setProgress(0.3, message = "Clustering...")

        result <- tryCatch({
          tme_cluster(
            input = data,
            features = input$tme_cluster_features,
            pattern = NULL,
            id = "ID",
            method = "kmeans",
            min_nc = input$tme_cluster_min_nc,
            max.nc = input$tme_cluster_max_nc
          )
        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error")
          return(NULL)
        })

        req(result)

        # 提取聚类标签
        cluster_info <- result[, c("ID", "cluster"), drop = FALSE]
        cluster_info$ID <- as.character(cluster_info$ID)

        # 提取原始数据中的全部数值 features
        all_features <- setdiff(colnames(data), c("ID", "cluster"))
        all_features <- all_features[vapply(data[, all_features, drop = FALSE], is.numeric, logical(1))]

        # 对全部原始 features 进行标准化
        scaled_data <- as.data.frame(scale(data[, all_features, drop = FALSE], center = TRUE, scale = TRUE))

        # 加回 ID
        scaled_data$ID <- as.character(data$ID)
        scaled_data <- dplyr::relocate(scaled_data, ID)

        # 加入 cluster，并放在 ID 后面
        result_for_output <- dplyr::left_join(scaled_data, cluster_info, by = "ID") %>%
          dplyr::relocate(cluster, .after = ID)

        tme_cluster_result(result_for_output)

        # Only generate heatmap when only_table = FALSE
        if (!isTRUE(only_table)) {
          setProgress(0.7, message = "Generating heatmap...")

          valid_features <- input$tme_cluster_features[
            input$tme_cluster_features %in% colnames(result)
          ]

          if (length(valid_features) == 0) {
            tme_cluster_plot(NULL)
            showNotification("No valid features found in clustering result.", type = "error")
            return(NULL)
          }

          pp <- tryCatch({
            sig_heatmap(
              input = result,
              id = "ID",
              features = valid_features,
              group = "cluster",
              condition = NULL,
              id_condition = "vars",
              col_condition = "condiction",
              cols_condition = NULL,
              scale = FALSE,
              palette = 2,
              cols_heatmap = NULL,
              palette_group = "jama",
              show_col = FALSE,
              show_palettes = FALSE,
              cols_group = NULL,
              show_plot = FALSE,
              width = 8,
              height = 6,
              size_row = 8,
              column_title = NULL,
              row_title = NULL,
              show_heatmap_col_name = FALSE,
              path = NULL,
              index = 1
            )
          }, error = function(e) {
            showNotification(paste("Heatmap error:", e$message), type = "error")
            return(NULL)
          })

          tme_cluster_plot(pp)
        }

        setProgress(1, message = "Finished")
      })
    })

    # 4. Select downstream and download data
    res_for_downstream <- reactive({
      req(input_data())

      # 未选择 Features：跳过聚类
      if (is.null(input$tme_cluster_features) || length(input$tme_cluster_features) == 0) {
        data_original <- input_data()
        if (!is.null(colnames(data_original))) colnames(data_original) <- gsub("\\.", "-", colnames(data_original))
        return(data_original)
      }

      # 已选择 Features：必须运行聚类
      validate(
        need(
          !is.null(tme_cluster_result()),
          "Please run TME Clustering first."
        )
      )

      tme_cluster_result()
    })

    # 5. Data table
    dataTableServer("tbl_data", tme_cluster_result)

    # 6. Cluster summary
    output$tme_cluster_cluster <- renderText({
      req(tme_cluster_result())

      res <- tme_cluster_result()

      if ("cluster" %in% colnames(res)) {
        cluster_tbl <- table(res$cluster)
        paste0("Group: ", paste(names(cluster_tbl), cluster_tbl, sep = ": ", collapse = "; "))
      } else {
        "No cluster column found."
      }
    })

    # 7. Plot
    if (!isTRUE(only_table)) {
      dims <- plotDownloadServer("plot_download", plot_reactive = tme_cluster_plot, filename_prefix = "tme_cluster_heatmap")

      output$tme_cluster_plot <- renderPlot({
        req(tme_cluster_plot())
        tme_cluster_plot()
      })

      output$tme_cluster_plot_container <- renderUI({
        req(tme_cluster_plot())

        div(
          style = "overflow: auto;",
          plotOutput(
            session$ns("tme_cluster_plot"),
            width = paste0(dims$width(), "px"),
            height = paste0(dims$height(), "px")
          )
        )
      })
    }

    # 8. Download selected data
    dataDownloadServer("download", data_reactive = res_for_downstream, filename_prefix = "tme_cluster_step_output")

    # 9. Return selected data
    return(res_for_downstream)
  })
}