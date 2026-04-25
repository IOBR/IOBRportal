workflow_immunotherapyUI <- function(id) {
  ns <- NS(id)
  
  bs4TabItem(
    tabName = "workflow_immunotherapy",

    h3(HTML(
      "IMMC Workflow
      <span style='font-size:80%; color:#333;'>:
       Database-driven Pipeline for Signature Scoring, TME Deconvolution, and Some Downstream Analysis.</span>"
    )) %>%
      helper(
        type = "markdown",
        icon = "question-circle",
        size = "m",
        colour = "#007bff",
        content = "workflow_immunotherapy_help" 
      ),
    
    tabsetPanel(
      id = ns("main_categories"),
      type = "pills", 

      tabPanel(
        title = "Part 1 · Data Preparation",
        icon = icon("database"),
        br(),
        tabsetPanel(
          id = ns("tabs_part1"),
          type = "tabs",
          tabPanel("Data Selection", prepare_immunotherapy_dataBodyUI(ns("mod_prepare_immunotherapy_data"))),
          tabPanel("TME Cluster",    tme_clusterBodyUI(ns("mod_cluster"), include_upload = FALSE, only_table = TRUE))
        )
      ),
      
      tabPanel(
        title = "Part 2 · Visualization",
        icon = icon("chart-area"),
        br(),
        tabsetPanel(
          id = ns("tabs_part2"),
          type = "tabs",
          tabPanel("Heatmap",       sig_heatmapBodyUI(ns("mod_heatmap"), include_upload = FALSE)),
          tabPanel("Box Plot",      sig_boxBodyUI(ns("mod_box_vis"), include_upload = FALSE)), 
          tabPanel("Cell Bar Plot", cell_bar_plotBodyUI(ns("mod_cellbar"), include_upload = FALSE))
        )
      ),

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
      )
    )
  )
}


workflow_immunotherapyServer <- function(id, pool) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # =========================================================
    # 1. 获取数据 (返回的是纯 Dataframe)
    # =========================================================
    # 注意：这里的 ID 必须和 UI 里的 ns("mod_prepare_immunotherapy_data") 保持一致
    res_source <- prepare_immunotherapy_dataServer("mod_prepare_immunotherapy_data", pool = pool)
    
    # =========================================================
    # 2. 运行聚类 (TME Cluster)
    # =========================================================
    # tme_clusterServer 需要 reactive dataframe，res_source 本身就是
    res_cluster <- tme_clusterServer("mod_cluster", external_eset = res_source, only_table = TRUE)
    
    # =========================================================
    # 3. 合并数据 (表达量 + 聚类结果)
    # =========================================================
    res_final_combined <- reactive({
      # 必须等待源数据和聚类结果都就绪
      req(res_source())
      req(res_cluster()) 
      
      # 【修复1】：这里直接使用 res_source()，因为没有 res_base_combined
      df_final <- res_source() 
      cluster_tab <- res_cluster()
      
      # --- 提取聚类信息并合并 ---
      # 1. 提取 (ID, cluster)
      cluster_col <- cluster_tab[, c(1, 2)]
      
      # 2. 命名 (强制小写 "cluster" 方便下游识别)
      colnames(cluster_col) <- c("ID", "Cluster")
      
      # 3. 确保 ID 类型一致，防止合并失败
      cluster_col$ID <- as.character(cluster_col$ID)
      df_final$ID    <- as.character(df_final$ID)

      # 4. 防止原表重名 (如果原表已有 cluster 列，先删掉)
      if ("cluster" %in% colnames(df_final)) {
        df_final <- dplyr::select(df_final, -cluster)
      }

      # 5. 合并
      df_final <- df_final %>% dplyr::left_join(cluster_col, by = "ID")
      
      return(df_final)
    })
    
    # Visualization
    sig_heatmapServer("mod_heatmap", external_eset = res_final_combined)
    sig_boxServer("mod_box_vis", external_eset = res_final_combined)

    res_for_cellbar <- reactive({
      req(res_final_combined())
      req(res_source())
  
      current_mode <- attr(res_source(), "data_type")
  
      if (identical(current_mode, "tme")) {
        res_final_combined()
      } else {
        NULL
      }
    })
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
    
    # Correlation
    batch_corServer("mod_batch_cor", external_eset = res_final_combined)
    batch_pccServer("mod_pcc", external_eset = res_final_combined)
    get_corServer("mod_single_cor", external_eset = res_final_combined)
    get_cor_matrixServer("mod_cor_matrix", external_eset = res_final_combined)
    
  })
}