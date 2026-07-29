datasets_overviewUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "datasets_overview",
    
    # --- CSS: 样式修正 ---
    tags$style(HTML("
      /* 1. 自定义顶部描述块 (替代 Jumbotron) */
      .description-box {
        background-color: white;
        padding: 15px 20px;       /* 紧凑的内边距 */
        margin-bottom: 15px;      /* 与下方卡片的间距 */
        border-radius: 5px;
        box-shadow: 0 1px 2px rgba(0,0,0,0.05);
      }
      
      /* 标题: 黑色，字号适中 (比 ValueBox 数字小，比正文大) */
      .description-box h3 { 
        font-size: 1.6rem !important; 
        font-weight: bold !important; 
        color: #333 !important;   /* 纯黑/深灰，不抢眼 */
        margin-top: 0px;
        margin-bottom: 10px;
      }
      
      /* 正文: 灰色，行高舒适 */
      .description-box p { 
        font-size: 1.6rem !important; 
        color: #555 !important; 
        margin-bottom: 0px !important; /* 核心：去掉段落底部留白 */
        line-height: 1.6;
      }
      
      /* 关键词高亮: 品牌蓝 (同导航栏) */
      .description-box b { 
        color: #003388; 
        font-weight: 700;
      }
    ")),
    
    tags$script(HTML("
      $(document).on('shown.bs.tab shown.lte.cardwidget expanded.lte.cardwidget', function() {
        setTimeout(function() {
          window.dispatchEvent(new Event('resize'));
        }, 300);
      });

      $(document).on('collapsed.lte.pushmenu shown.lte.pushmenu', function() {
        setTimeout(function() {
          window.dispatchEvent(new Event('resize'));
        }, 300);
      });
    ")),

    # --- 1. 顶部简介 (纯 div 实现，无多余空白) ---
    div(
      class = "description-box",
      h3("IOBRportal Database Statistics"), # 标题改用 h3，黑色
      HTML("
        The IOBRportal integrates multi-omics data from four major cohorts: 
        <b>TCGA Cohort</b>, <b>MOLC (Molecular Cohort)</b> (predominantly GEO), <b>IMMC (Immunotherapy Cohort)</b>, and 
        <b>CLIC (Clinical Cohort)</b> (including CPTAC and TARGET). 
        The data is structured into two primary analysis modes—<b>Calculate Sigscore</b> 
        and <b>Deconvolute TME</b> features—designed to facilitate comprehensive 
        downstream bioinformatics analyses.
      ")
    ),
    
    # --- 2. 关键指标卡片 ---
    uiOutput(ns("value_boxes")), 
    
    # --- 3. 核心可视化区域 ---
    fluidRow(
      # 左侧：嵌套饼图 (Nested Pie)
      bs4Card(
        title = "Database Overview",
        width = 6,
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        echarts4rOutput(ns("plot_sunburst"), height = "450px") 
      ),
      
      # 右侧：堆叠条形图 (带滚动条)
      bs4Card(
        title = "Cancer Types Distribution",
        width = 6,
        status = "purple",
        solidHeader = TRUE,
        collapsible = TRUE,
        echarts4rOutput(ns("plot_stacked_bar"), height = "450px")
      )
    ),
    
    # --- 4. 数据明细表 ---
    fluidRow(
      
      # [左侧] 数据明细表 (改为宽度 6)
      bs4Card(
        title = "Detailed Cohort Metadata",
        width = 6,              # <--- 修改点：从 12 改为 6
        status = "gray", 
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = FALSE,      # 默认展开，方便查看
        height = "400px",
        dataTableOutput(ns("table_data"))
      ),
      
      # [右侧] 快速跳转区 (新增)
      bs4Card(
        title = "Quick Analysis Access",
        width = 6,              # <--- 占据另一半空间
        status = "gray",       # 干净的白色背景
        solidHeader = TRUE,
        height = "400px",
        
        br(),
        br(),
        # 提示文字
        p("Select a cohort to start your analysis workflow immediately:", 
          style = "color: #666; margin-bottom: 15px; font-style: italic;"),

        # 按钮组：两行两列布局
        fluidRow(
          # 1. TCGA
          column(6, 
            actionButton(
              inputId = ns("btn_tcga"), 
              label = "TCGA", 
              icon = icon("align-left"), 
              status = "primary",    # 蓝色
              class = "btn-lg btn-block", # 大按钮 + 充满宽度
              style = "margin-bottom: 15px; text-align: left; font-weight: bold;"
            )
          ),
          # 2. Cancer Cohort
          column(6, 
            actionButton(
              inputId = ns("btn_cancer"), 
              label = "MOLC", 
              icon = icon("procedures"), 
              status = "danger",     # 红色
              class = "btn-lg btn-block",
              style = "margin-bottom: 15px; text-align: left; font-weight: bold;"
            )
          ),
          # 3. Immunotherapy
          column(6, 
            actionButton(
              inputId = ns("btn_immuno"), 
              label = "IMMC", 
              icon = icon("syringe"), 
              status = "success",    # 绿色
              class = "btn-lg btn-block",
              style = "margin-bottom: 15px; text-align: left; font-weight: bold;"
            )
          ),
          # 4. Other Cohort
          column(6, 
            actionButton(
              inputId = ns("btn_other"), 
              label = "CLIC", 
              icon = icon("folder-open"), 
              status = "warning",    # 橙色
              class = "btn-lg btn-block",
              style = "margin-bottom: 15px; text-align: left; font-weight: bold;"
            )
          )
        )
      )
    )
  )
}


datasets_overviewServer <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {
    
    # --- 1. 读取并基础清洗数据 (不在此处过滤 Pan-Cancer) ---
    df_clean <- reactive({
      req(file.exists("data/cohorts_info.xlsx"))
      
      # df <- read.csv("data/cohorts_info.xlsx", header = TRUE, stringsAsFactors = FALSE)      
      df <- readxl::read_excel("data/cohorts_info.xlsx", sheet = "Sheet1") %>% as.data.frame()

      df %>%
        mutate(
          Sample = as.numeric(as.character(Sample)),
          Dataset = ifelse(is.na(Dataset) | Dataset == "", "Unknown", Dataset),
          CancerType = ifelse(is.na(CancerType) | CancerType == "", "Misc", CancerType)
        ) %>%
        filter(!is.na(Sample) & Sample > 0) %>%
        # 【逻辑同步】定义大类映射，但不删除 Pan-Cancer
        mutate(Category = case_when(
          CancerType %in% c("COAD", "READ", "CRC") ~ "CRC",
          CancerType %in% c("LUAD", "LUSC", "NSCLC") ~ "NSCLC",
          TRUE ~ CancerType
        )) %>%
        # 【Dataset 标签优化】用于右图堆叠
        mutate(SeriesName = paste0(Dataset, " (", CancerType, ")"))
    })
    
    # --- 2. Value Boxes (使用内联样式强制控制大小) ---
    # output$value_boxes <- renderUI({
    #   req(df_clean())
    #   data_stat <- df_clean()
      
    #   fluidRow(
    #     # 1. Total Samples
    #     bs4ValueBox(
    #       # 【数值】：直接用 tags$span 包裹
    #       value = tags$span(
    #         sum(data_stat$Sample), 
    #         style = "font-size: 2rem; font-weight: 400; line-height: 1.1;"
    #       ),
    #       # 【文字】：用 tags$span 包裹
    #       subtitle = tags$span(
    #         "Total Samples", 
    #         style = "font-size: 1.2rem; font-weight: normal; opacity: 0.8;"
    #       ),
    #       icon = icon("users"), 
    #       color = "info", 
    #       width = 4
    #     ),
        
    #     # 2. Cancer Categories
    #     bs4ValueBox(
    #       value = tags$span(
    #         n_distinct(data_stat$Category), 
    #         style = "font-size: 2rem; font-weight: 400; line-height: 1.1;"
    #       ),
    #       subtitle = tags$span(
    #         "Cancer Categories", 
    #         style = "font-size: 1.2rem; font-weight: normal; opacity: 0.8;"
    #       ),
    #       icon = icon("dna"), 
    #       color = "danger", 
    #       width = 4
    #     ),
        
    #     # 3. Databases
    #     bs4ValueBox(
    #       value = tags$span(
    #         n_distinct(data_stat$Dataset), 
    #         style = "font-size: 2rem; font-weight: 400; line-height: 1.1;"
    #       ),
    #       subtitle = tags$span(
    #         "Databases", 
    #         style = "font-size: 1.2rem; font-weight: normal; opacity: 0.8;"
    #       ),
    #       icon = icon("database"), 
    #       color = "warning", 
    #       width = 4
    #     )
    #   )
    # })
    
    output$value_boxes <- renderUI({
      req(df_clean())
      data_stat <- df_clean()

      # 更稳妥：避免空ID影响计数
      n_datasets <- data_stat %>%
        dplyr::filter(!is.na(ID), ID != "") %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::pull(n)

      fluidRow(
        # 1. Total Samples
        bs4ValueBox(
          value = tags$span(
            sum(data_stat$Sample, na.rm = TRUE),
            style = "font-size: 2rem; font-weight: 400; line-height: 1.1;"
          ),
          subtitle = tags$span(
            "Total Samples",
            style = "font-size: 1.1rem; font-weight: normal; opacity: 0.8;"
          ),
          icon = icon("users"),
          color = "info",
          width = 3
        ),

        # 2. Datasets (按 ID 去重)
        bs4ValueBox(
          value = tags$span(
            n_datasets,
            style = "font-size: 2rem; font-weight: 400; line-height: 1.1;"
          ),
          subtitle = tags$span(
            "Datasets",
            style = "font-size: 1.1rem; font-weight: normal; opacity: 0.8;"
          ),
          icon = icon("table"),
          color = "primary",
          width = 3
        ),

        # 3. Cancer Categories
        bs4ValueBox(
          value = tags$span(
            n_distinct(data_stat$Category),
            style = "font-size: 2rem; font-weight: 400; line-height: 1.1;"
          ),
          subtitle = tags$span(
            "Cancer Types",
            style = "font-size: 1.1rem; font-weight: normal; opacity: 0.8;"
          ),
          icon = icon("dna"),
          color = "danger",
          width = 3
        ),

        # 4. Data Sections
        bs4ValueBox(
          value = tags$span(
            n_distinct(data_stat$Dataset),
            style = "font-size: 2rem; font-weight: 400; line-height: 1.1;"
          ),
          subtitle = tags$span(
            "Data Sections",   
            style = "font-size: 1.1rem; font-weight: normal; opacity: 0.8;"
          ),
          icon = icon("layer-group"),
          color = "warning",
          width = 3
        )
      )
    })
    
    # --- 3. 左图：嵌套饼图 (保留 Pan-Cancer 逻辑) ---
    output$plot_sunburst <- renderEcharts4r({
      req(df_clean())
      # 这里直接使用全部数据，包含 Pan-Cancer
      df <- df_clean()

      data_inner <- df %>% group_by(Dataset) %>% summarise(value = sum(Sample)) %>% arrange(desc(value))
      # 这里按 Category 分组，Pan-Cancer 会作为一个独立的色块出现
      data_outer <- df %>% group_by(Category) %>% summarise(value = sum(Sample)) %>% arrange(desc(value)) 
      
      vibrant_colors <- c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F", 
                        "#8491B4", "#91D1C2", "#DC0000", "#7E6148", "#B09C85",
                        "#5470c6", "#91cc75", "#fac858", "#ee6666", "#73c0de")
      
      data_inner %>%
        e_charts(Dataset) %>% 
        e_color(vibrant_colors) %>%
        e_pie(value, radius = c(0, "30%"), label = list(position = "inner", color = "#555")) %>%
        e_data(data_outer, Category) %>% 
        e_pie(value, radius = c("45%", "70%"), name = "Category",
              label = list(show = TRUE, color = "#666"), 
              labelLine = list(smooth = 0.2, length = 10, length2 = 20)) %>%
        e_tooltip(formatter = htmlwidgets::JS("
          function(params) {
            return params.name + ': ' + params.value + ' (' + params.percent + '%)';
          }
        ")) %>%
        e_legend(show = FALSE) %>% 
        # e_theme("westeros") %>%
        e_title("Hierarchical overview of sample distribution")
    })
    
    # --- 4. 右图：堆叠条形图 (局部过滤 Pan-Cancer) ---
    output$plot_stacked_bar <- renderEcharts4r({
      req(df_clean())
      # 【关键：此处过滤】只在右图逻辑中剔除 Pan-Cancer
      df_final <- df_clean() %>% filter(!grepl("Pan-Cancer", CancerType, ignore.case = TRUE))

      # 2. 计算大类总数并排序 (Y轴顺序)
      rank_order <- df_final %>%
        group_by(Category) %>%
        summarise(Total = sum(Sample)) %>%
        arrange(Total)

      ordered_categories <- rank_order$Category

      # 3. 汇总数据，确保 Dataset 和具体小类都参与堆叠
      df_plot <- df_final %>%
        group_by(Category, SeriesName) %>%
        summarise(Value = sum(Sample), .groups = "drop")

      # 4. 绘图
      df_plot %>%
        group_by(SeriesName) %>%   # 按 "数据库(具体癌种)" 分组实现细分色块
        e_charts(Category) %>% 
        e_bar(Value, stack = "grp") %>% 
        e_flip_coords() %>%
        e_y_axis(type = "category", data = ordered_categories) %>%
        
        # --- 样式调整 ---
        e_legend(show = FALSE) %>%
        e_tooltip(trigger = "item") %>% # 悬停显示具体数据库的具体癌种
        e_grid(containLabel = TRUE, bottom = "10%") %>% 
        e_datazoom(y_index = 0, type = "slider", start = 60, end = 100) %>%
        e_title("Combined Distribution by Dataset & Subtype")
    })

    observeEvent(input$btn_tcga, {
      updateTabItems(session = parent_session, inputId = "workflow_menu", selected = "workflow_tcga")
    })
    
    observeEvent(input$btn_cancer, {
      updateTabItems(session = parent_session, inputId = "workflow_menu", selected = "workflow_cancercohort")
    })
    
    observeEvent(input$btn_immuno, {
      updateTabItems(session = parent_session, inputId = "workflow_menu", selected = "workflow_immunotherapy")
    })
    
    observeEvent(input$btn_other, {
      updateTabItems(session = parent_session, inputId = "workflow_menu", selected = "workflow_othercohort")
    })

    # --- 5. 数据表 (通常建议展示非过滤的原始清单) ---
    output$table_data <- renderDT({
      req(df_clean())
      datatable(
        df_clean() %>% select(Dataset, ID, CancerType, Sample),
        options = list(pageLength = 5, scrollX = TRUE),
        rownames = FALSE
      )
    })
    
  })
}