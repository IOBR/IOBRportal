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
    # 2. Optional TME Cluster
    # =========================================================
    # tme_clusterServer 需要 reactive dataframe，res_source 本身就是
    res_cluster <- tme_clusterServer("mod_cluster", external_eset = res_source, only_table = TRUE)
    
    # =========================================================
    # 3. Final Data
    # =========================================================
    res_final_combined <- res_cluster
    
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