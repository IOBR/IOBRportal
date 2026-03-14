# =========================================================
# Module: iobr (Signature + TME)
# =========================================================
workflow_iobrUI <- function(id) {
  ns <- NS(id)
  
  bs4TabItem(
    tabName = "workflow_iobr",
    # --- 标题区域 ---
    h3(HTML(
      "Integrated Workflow
      <span style='font-size:80%; color:#333;'>:
       Integrated Pipeline for Signature Scoring, TME Deconvolution, and Downstream Analysis.</span>"
    )) %>%
      helper(
        type = "markdown",
        icon = "question-circle",
        size = "m",
        colour = "#007bff",
        content = "workflow_iobr_help" # 请确保有对应的 help md 文件
      ),
    
    tabsetPanel(
      id = ns("main_categories"),
      type = "pills", 
      
      # =========================================================
      # Part 1: Preprocessing & Calculation
      # =========================================================
      tabPanel(
        title = "Part 1 · Preprocessing & Features",
        icon = icon("flask"),
        br(),
        tabsetPanel(
          id = ns("tabs_part1"),
          type = "tabs",

          tabPanel("Counts to TPM", count2tpmBodyUI(ns("mod_tpm"), include_upload = TRUE)),
          tabPanel("Detect Outliers", find_outlier_samplesBodyUI(ns("mod_detcct_outlier"), include_upload = FALSE)),
          
          # Calculate Features (二合一切换)
          tabPanel("Calculate Features", 
             
             div(
               # 1. 容器样式：
               # align-items: center; 确保按钮和文字垂直居中
               # margin-bottom: -5px; 抵消按钮自带的下边距
               style = "display: flex; justify-content: flex-start; align-items: center; margin-bottom: -5px;", 
               
               shinyWidgets::radioGroupButtons(
                 inputId = ns("feature_mode"),
                 label = NULL, 
                 choices = c(
                   "&nbsp; Calculate Sigscores" = "sig", 
                   "&nbsp; Deconvolute TME" = "tme"
                 ),
                 selected = "sig",
                 justified = FALSE, 
                 status = "outline-primary", 
                 checkIcon = list(
                   yes = icon("check", class = "fa-lg"), 
                   no = icon("circle", class = "far")
                 ),
                 size = "normal", 
                 individual = TRUE
               ),
               
               # 2. 提示文字样式：
               # margin-left: 15px;  距离左边按钮的间距
               # font-style: italic; 斜体
               # font-size: 15px;    字号调大 (比之前的 14px 或 90% 大)
               # color: #555;        深灰色
               # padding-top: 2px;   微调垂直位置
               span("Note: Select Calculate Sigscores or Deconvolute TME.", 
                    style = "margin-left: 15px; font-style: italic; color: #555; font-size: 15px; padding-top: 2px;")
             ),
             
             # --- Conditional Panels ---
             conditionalPanel(
               condition = paste0("input['", ns("feature_mode"), "'] == 'sig'"),
               calculate_sig_scoreBodyUI(ns("mod_score"), include_upload = FALSE)
             ),
             
             conditionalPanel(
               condition = paste0("input['", ns("feature_mode"), "'] == 'tme'"),
               deconvo_tmeBodyUI(ns("mod_tme"), include_upload = FALSE)
             )
          ),

          tabPanel("TME Cluster", tme_clusterBodyUI(ns("mod_cluster"), include_upload = FALSE, only_table = TRUE)),
          tabPanel("Combine Pdata", combine_pd_esetBodyUI(ns("mod_combine"), include_upload = TRUE))
        )
      ),
      
      # =========================================================
      # Part 2: Visualization
      # =========================================================
      tabPanel(
        title = "Part 2 · Visualization",
        icon = icon("chart-area"),
        br(),
        tabsetPanel(
          id = ns("tabs_part2"),
          type = "tabs",
          tabPanel("Heatmap",          sig_heatmapBodyUI(ns("mod_heatmap"), include_upload = FALSE)),
          tabPanel("Box Plot",         sig_boxBodyUI(ns("mod_box_vis"), include_upload = FALSE)),
          tabPanel("Percent Bar Plot", percent_bar_plotBodyUI(ns("mod_percent"), include_upload = FALSE)),
          tabPanel("Cell Bar Plot",    cell_bar_plotBodyUI(ns("mod_cellbar"), include_upload = FALSE))
        )
      ),
      
      # =========================================================
      # Part 3: Survival Analysis
      # =========================================================
      tabPanel(
        title = "Part 3 · Survival Analysis",
        icon = icon("heartbeat"),
        br(),
        tabsetPanel(
          id = ns("tabs_part3"),
          type = "tabs",
          tabPanel("Batch Survival",  batch_survBodyUI(ns("mod_batch_surv"), include_upload = FALSE)),
          tabPanel("Forest Plot",     sig_forestBodyUI(ns("mod_forest"), include_upload = FALSE)),
          tabPanel("Heatmap", sig_heatmapBodyUI(ns("mod_surv_heatmap"), include_upload = FALSE, show_top_n = TRUE)),          
          tabPanel("Survival Plot",   sig_surv_plotBodyUI(ns("mod_surv_plot"), include_upload = FALSE)),
          tabPanel("Survival Group",  surv_groupBodyUI(ns("mod_surv_group"), include_upload = FALSE)),
          tabPanel("Time ROC",        roc_timeBodyUI(ns("mod_roc"), include_upload = FALSE))
        )
      ),
      
      # =========================================================
      # Part 4: Correlation
      # =========================================================
      tabPanel(
        title = "Part 4 · Correlation",
        icon = icon("project-diagram"),
        br(),
        tabsetPanel(
          id = ns("tabs_part4"),
          type = "tabs",
          tabPanel("Batch Correlation",   batch_corBodyUI(ns("mod_batch_cor"), include_upload = FALSE)),
          tabPanel("Partial Correlation", batch_pccBodyUI(ns("mod_pcc"), include_upload = FALSE)),
          tabPanel("Single Correlation",  get_corBodyUI(ns("mod_single_cor"), include_upload = FALSE)),
          tabPanel("Correlation Matrix",  get_cor_matrixBodyUI(ns("mod_cor_matrix"), include_upload = FALSE))
        )
      ),
      
      # =========================================================
      # Part 5: Group Comparison
      # =========================================================
      tabPanel(
        title = "Part 5 · Group Comparison",
        icon = icon("chart-bar"),
        br(),
        tabsetPanel(
          id = ns("tabs_part5"),
          type = "tabs",
          tabPanel("Wilcoxon Test", batch_wilcoxonBodyUI(ns("mod_wilcoxon"), include_upload = FALSE)),
          tabPanel("Kruskal Test",  batch_kruskalBodyUI(ns("mod_kruskal"), include_upload = FALSE)),
          tabPanel("Heatmap", sig_heatmapBodyUI(ns("mod_group_heatmap"), include_upload = FALSE, show_top_n = TRUE)),
          tabPanel("Box Plot",      sig_boxBodyUI(ns("mod_box_stat"), include_upload = FALSE))
        )
      )
    )
  )
}


workflow_iobrServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --------------------------------------------------------
    # Part 1: Preprocessing & Calculation
    # --------------------------------------------------------
    
    # 1. Counts to TPM & detect outlier
    res_tpm <- count2tpmServer("mod_tpm")
    res_outlier <- find_outlier_samplesServer("mod_detcct_outlier", external_eset = res_tpm$tpm)

    # 2. Calculation Modules
    clean_eset <- reactive({
      if (!is.null(res_outlier())) {
        return(res_outlier())
      } else {
        return(res_tpm$tpm())
      }
    }) # 如果 outlier 模块没跑(NULL)，就回退用 TPM 数据

    res_sig_calc <- calculate_sig_scoreServer("mod_score", external_eset = clean_eset)
    res_tme_calc <- deconvo_tmeServer("mod_tme", external_eset = clean_eset)
    
    # 3. Feature Selection Logic (The "Switch")
    # 这是最关键的一步：根据 radioGroupButtons 的选择，决定下游使用的是 Signature 结果还是 TME 结果
    res_score <- reactive({
      req(input$feature_mode)
      
      if (input$feature_mode == "sig") {
        validate(need(res_sig_calc(), "Please run Signature Scoring analysis first."))
        return(res_sig_calc())
      } else {
        validate(need(res_tme_calc(), "Please run TME Deconvolution analysis first."))
        return(res_tme_calc())
      }
    })
    
    # 4. TME Clustering
    res_cluster <- tme_clusterServer("mod_cluster", external_eset = res_score, only_table = TRUE)
    
    # 5. iobr Data
    res_combined <- combine_pd_esetServer("mod_combine", external_eset = res_cluster)
    
    # --------------------------------------------------------
    # Part 2: Visualization
    # --------------------------------------------------------
    sig_heatmapServer("mod_heatmap", external_eset = res_combined)    
    sig_boxServer("mod_box_vis", external_eset = res_combined)
    percent_bar_plotServer("mod_percent", external_eset = res_combined)
    cell_bar_plotServer("mod_cellbar", external_eset = res_tme_calc) # 仅在 TME 模式下或数据符合格式时显示
    
    # --------------------------------------------------------
    # Part 3: Survival Analysis
    # --------------------------------------------------------
    res_batch_surv <- batch_survServer("mod_batch_surv", external_eset = res_combined)
    
    # Forest Plot 依赖 Batch Survival 的结果
    sig_forestServer("mod_forest", external_eset = res_batch_surv)

    # 此处的sig_heatmap 依赖 Batch Survival 的结果
    surv_sorted_ids <- reactive({
      req(res_batch_surv())
      df <- res_batch_surv()
  
      # 只要数据里有 P 和 ID，就排序并返回 ID 向量
      if ("P" %in% colnames(df) && "ID" %in% colnames(df)) {
         df <- df[order(df$P, decreasing = FALSE), ] # P值从小到大排序
         return(df$ID) # 返回所有 ID，让子模块去切 Top N
      }
      return(NULL)
    })
    sig_heatmapServer("mod_surv_heatmap", external_eset = res_combined, ordered_ids = surv_sorted_ids)

    sig_surv_plotServer("mod_surv_plot", external_eset = res_combined)
    surv_groupServer("mod_surv_group", external_eset = res_combined)
    roc_timeServer("mod_roc", external_eset = res_combined)

    # --------------------------------------------------------
    # Part 4: Correlation
    # --------------------------------------------------------
    batch_corServer("mod_batch_cor", external_eset = res_combined)
    batch_pccServer("mod_pcc", external_eset = res_combined)
    get_corServer("mod_single_cor", external_eset = res_combined)
    get_cor_matrixServer("mod_cor_matrix", external_eset = res_combined)
    
    # --------------------------------------------------------
    # Part 5: Group Comparison
    # --------------------------------------------------------
    res_wilcox <- batch_wilcoxonServer("mod_wilcoxon", external_eset = res_combined)
    res_kruskal <- batch_kruskalServer("mod_kruskal", external_eset = res_combined)

    group_sig_ids <- reactiveVal(NULL)
    # --- A. 监听 Wilcoxon 结果 ---
    observeEvent(res_wilcox(), {
      req(res_wilcox())
      df <- res_wilcox()
      
      if ("p.value" %in% colnames(df) && "sig_names" %in% colnames(df)) {
         group_sig_ids(unique(df$sig_names))
      }
    })

    # --- B. 监听 Kruskal 结果 ---
    observeEvent(res_kruskal(), {
      req(res_kruskal())
      df <- res_kruskal()
      
      if ("p.value" %in% colnames(df) && "sig_names" %in% colnames(df)) {
         group_sig_ids(unique(df$sig_names))
      }
    })
    sig_heatmapServer("mod_group_heatmap", external_eset = res_combined, ordered_ids = group_sig_ids)
    sig_boxServer("mod_box_stat", external_eset = res_combined)
    
  })
}