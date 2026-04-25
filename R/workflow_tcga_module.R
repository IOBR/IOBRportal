# =========================================================
# Module: workflow_tcga
# Description: TCGA 泛癌 Signature 分析工作流 (DuckDB版)
# =========================================================
workflow_tcgaUI <- function(id) {
  ns <- NS(id)
  
  bs4TabItem(
    tabName = "workflow_tcga",
    
    h3(HTML(
      "TCGA Cohorts Workflow
      <span style='font-size:80%; color:#333;'>:
       Database-driven Pipeline for Signature Scoring, TME Deconvolution, and Downstream Analysis.</span>"
    )) %>%
      helper(
        type = "markdown",
        icon = "question-circle",
        size = "m",
        colour = "#007bff",
        content = "workflow_tcga_help"
      ),
    
    tabsetPanel(
      id = ns("main_categories"),
      type = "pills", 
      
      # =========================================================
      # Part: Data Preparation & Clustering
      # =========================================================
      tabPanel(
        title = "Part 1 · Data Preparation",
        icon = icon("database"),
        br(),
        tabsetPanel(
          id = ns("tabs_part1"),
          type = "tabs",

          tabPanel("Data Selection", prepare_tcga_dataBodyUI(ns("mod_prepare"))),
          tabPanel("TME Cluster",    tme_clusterBodyUI(ns("mod_cluster"), include_upload = FALSE, only_table = TRUE))
        )
      ),
      
      # =========================================================
      # Part: Visualization
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
      # Part: Group Comparison
      # =========================================================
      tabPanel(
        title = "Part 3 · Group Comparison",
        icon = icon("chart-bar"),
        br(),
        tabsetPanel(
          id = ns("tabs_part3"),
          type = "tabs",
          tabPanel("Wilcoxon Test", batch_wilcoxonBodyUI(ns("mod_wilcoxon"), include_upload = FALSE)),
          tabPanel("Kruskal Test",  batch_kruskalBodyUI(ns("mod_kruskal"), include_upload = FALSE)),
          tabPanel("Heatmap", sig_heatmapBodyUI(ns("mod_group_heatmap"), include_upload = FALSE, show_top_n = TRUE)),
          tabPanel("Box Plot",      sig_boxBodyUI(ns("mod_box_stat"), include_upload = FALSE))
        )
      ),
      
      # =========================================================
      # Part: Survival Analysis
      # =========================================================
      tabPanel(
        title = "Part 4 · Survival Analysis",
        icon = icon("heartbeat"),
        br(),
        tabsetPanel(
          id = ns("tabs_part4"),
          type = "tabs",
          tabPanel("Batch Survival",   batch_survBodyUI(ns("mod_batch_surv"), include_upload = FALSE)),
          tabPanel("Forest Plot",      sig_forestBodyUI(ns("mod_forest"), include_upload = FALSE)),
          tabPanel("Heatmap", sig_heatmapBodyUI(ns("mod_surv_heatmap"), include_upload = FALSE, show_top_n = TRUE)),
          tabPanel("Survival Plot",    sig_surv_plotBodyUI(ns("mod_surv_plot"), include_upload = FALSE)),
          tabPanel("Survival Group",   surv_groupBodyUI(ns("mod_surv_group"), include_upload = FALSE)),
          tabPanel("Time ROC",         roc_timeBodyUI(ns("mod_roc"), include_upload = FALSE)),
          tabPanel("Sig ROC",         sig_rocBodyUI(ns("mod_sig_roc"), include_upload = FALSE))
        )
      ),
      
      # =========================================================
      # Part: Correlation Analysis
      # =========================================================
      tabPanel(
        title = "Part 5 · Correlation",
        icon = icon("project-diagram"),
        br(),
        tabsetPanel(
          id = ns("tabs_part5"),
          type = "tabs",
          tabPanel("Batch Correlation",   batch_corBodyUI(ns("mod_batch_cor"), include_upload = FALSE)),
          tabPanel("Partial Correlation", batch_pccBodyUI(ns("mod_pcc"), include_upload = FALSE)),
          tabPanel("Single Correlation",  get_corBodyUI(ns("mod_single_cor"), include_upload = FALSE)),
          tabPanel("Correlation Matrix",  get_cor_matrixBodyUI(ns("mod_cor_matrix"), include_upload = FALSE))
        )
      )
    )
  )
}


workflow_tcgaServer <- function(id, pool) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # =========================================================
    # 1. Prepare Data (调用二合一模块)
    # =========================================================
    res_source <- prepare_tcga_dataServer("mod_prepare", pool = pool)
    
    # =========================================================
    # 2. Base Combine (合并 Pdata + Expr)
    # =========================================================
    res_base_combined <- reactive({
      req(res_source()) 
      
      data_list <- res_source()
      expr  <- data_list$expr
      pdata <- data_list$pdata
      
      # ID 交集
      common <- intersect(expr$ID, pdata$ID)
      if(length(common) == 0) return(NULL)
      
      # 排序
      expr  <- expr[match(common, expr$ID), , drop = FALSE]
      pdata <- pdata[match(common, pdata$ID), , drop = FALSE]
      
      # 移除重复 ID 并合并
      expr_clean <- dplyr::select(expr, -ID)
      combined_df <- cbind(pdata, expr_clean)
      
      return(combined_df)
    })
    
    # =========================================================
    # 3. Run Cluster (运行聚类)
    # =========================================================
    res_expr_only <- reactive({
      req(res_source())
      res_source()$expr
    })
    
    # 必须运行聚类
    res_cluster <- tme_clusterServer("mod_cluster", external_eset = res_expr_only, only_table = TRUE)
    
    # =========================================================
    # 4. Final Combine (强制提取前两列 -> cluster -> 合并)
    # =========================================================
    res_final_combined <- reactive({
      # 强制等待：必须有基础表，必须有聚类结果
      req(res_base_combined())
      req(res_cluster()) 
      
      df_final <- res_base_combined()
      cluster_tab <- res_cluster()
      
      # --- 直接提取前两列，强制重命名 ---
      # 1. 提取 (ID, cluster)
      cluster_col <- cluster_tab[, c(1, 2)]
      
      # 2. 命名 (强制小写 "cluster")
      colnames(cluster_col) <- c("ID", "Cluster")
      
      # 3. 确保 ID 类型一致
      cluster_col$ID <- as.character(cluster_col$ID)
      df_final$ID    <- as.character(df_final$ID)

      # 4. 防止原表重名 (删掉旧 cluster)
      if ("cluster" %in% colnames(df_final)) {
        df_final <- dplyr::select(df_final, -cluster)
      }

      # 5. 合并
      df_final <- df_final %>% dplyr::left_join(cluster_col, by = "ID")

      if ("Grade" %in% colnames(df_final)) {
        df_final <- df_final %>% dplyr::relocate(Cluster, .after = Grade)
      }
      return(df_final)
    })
    
    # =========================================================
    # 5. Downstream Analysis
    # =========================================================
    
    # Visualization
    sig_heatmapServer("mod_heatmap", external_eset = res_final_combined)
    sig_boxServer("mod_box_vis", external_eset = res_final_combined)
    percent_bar_plotServer("mod_percent", external_eset = res_final_combined)

    res_for_cellbar <- reactive({
      # 必须有数据
      req(res_final_combined())
      # 必须有源信息的 metadata
      req(res_source())
      
      # 获取模式 ("sig" 或 "tme")
      current_mode <- res_source()$type 
      
      # 只有是 "tme" 模式时，才传递数据
      if (isTRUE(current_mode == "tme")) {
        return(res_final_combined())
      } else {
        return(NULL) # 传空值，模块内部的 req() 会拦截，从而不显示图
      }
    })
    # 将过滤后的 reactive 传给模块
    cell_bar_plotServer("mod_cellbar", external_eset = res_for_cellbar)

    # Comparison
    # batch_wilcoxonServer("mod_wilcoxon", external_eset = res_final_combined)
    # batch_kruskalServer("mod_kruskal", external_eset = res_final_combined)
    # sig_boxServer("mod_box_stat", external_eset = res_final_combined)

    res_wilcox <- batch_wilcoxonServer("mod_wilcoxon", external_eset = res_final_combined)
    res_kruskal <- batch_kruskalServer("mod_kruskal", external_eset = res_final_combined)

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
    sig_heatmapServer("mod_group_heatmap", external_eset = res_final_combined, ordered_ids = group_sig_ids)
    sig_boxServer("mod_box_stat", external_eset = res_final_combined)

    # Survival
    res_batch_surv <- batch_survServer("mod_batch_surv", external_eset = res_final_combined)
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
    sig_heatmapServer("mod_surv_heatmap", external_eset = res_final_combined, ordered_ids = surv_sorted_ids)

    sig_surv_plotServer("mod_surv_plot", external_eset = res_final_combined)
    roc_timeServer("mod_roc", external_eset = res_final_combined)
    surv_groupServer("mod_surv_group", external_eset = res_final_combined)
    sig_rocServer("mod_sig_roc", external_eset = res_final_combined)
    
    # Correlation
    batch_corServer("mod_batch_cor", external_eset = res_final_combined)
    batch_pccServer("mod_pcc", external_eset = res_final_combined)
    get_corServer("mod_single_cor", external_eset = res_final_combined)
    get_cor_matrixServer("mod_cor_matrix", external_eset = res_final_combined)
        
  })
}