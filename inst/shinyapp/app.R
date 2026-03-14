library(shiny)
library(DT)
library(bs4Dash)
library(shinyWidgets)
#library(colourpicker)
#library(ggsci)
#library(ggplot2)
library(IOBR)
library(zip)
library(shinyhelper)
library(duckdb)
library(dplyr)
library(dbplyr)
library(pool)
library(DBI)
library(echarts4r)
# library(shinycssloaders)
library(waiter)
library(readxl)
library(writexl)


options(shiny.maxRequestSize = 1024 * 1024^2)  # 设置最大上传文件大小为 1024MB

# 1. 建立全局连接池

if (!exists("tcga_pool") || !dbIsValid(tcga_pool)) {
  tcga_pool <- dbPool(duckdb(), dbdir = "data/tcga_signatures.duckdb", read_only = TRUE)
}

if (!exists("othercohort_pool") || !dbIsValid(othercohort_pool)) {
  othercohort_pool <- dbPool(duckdb(), dbdir = "data/othercohort.duckdb", read_only = TRUE)
}

if (!exists("immunotherapy_pool") || !dbIsValid(immunotherapy_pool)) {
  immunotherapy_pool <- dbPool(duckdb(), dbdir = "data/immunotherapy.duckdb", read_only = TRUE)
}

if (!exists("cancercohort_pool") || !dbIsValid(cancercohort_pool)) {
  cancercohort_pool <- dbPool(duckdb(), dbdir = "data/cancercohort.duckdb", read_only = TRUE)
}

# --- 确保 App 退出时关闭所有连接 ---
onStop(function() {
  # 关闭 TCGA 连接
  if (exists("tcga_pool")) {
    poolClose(tcga_pool)
    message("Closed TCGA pool.")
  }
  
  # 关闭 OtherCohort 连接
  if (exists("othercohort_pool")) {
    poolClose(othercohort_pool)
    message("Closed OtherCohort pool.")
  }
  
  if (exists("immunotherapy_pool")) {
    poolClose(immunotherapy_pool)
    message("Closed immunotherapy pool.")
  }
  
  if (exists("cancercohort_pool")) {
    poolClose(cancercohort_pool)
    message("Closed cancercohort pool.")
  }
})



source("modules/upload_module.R")
source("modules/data_download_module.R")
source("modules/dataTable_module.R")
source("modules/plot_download_module.R")

source("modules/home_intro_module.R")
source("modules/count2tpm_module.R")
source("modules/anno_eset_module.R")
source("modules/calculate_sig_score_module.R")
source("modules/combine_pd_eset_module.R")
source("modules/sig_heatmap_module.R")
source("modules/sig_box_module.R")
source("modules/sig_surv_plot_module.R")
source("modules/roc_time_module.R")
source("modules/batch_surv_module.R")
source("modules/sig_forest_module.R")
source("modules/batch_cor_module.R")
source("modules/get_cor_module.R")
source("modules/get_cor_matrix_module.R")
source("modules/batch_pcc_module.R")
source("modules/batch_wilcoxon_module.R")
source("modules/batch_kruskal_module.R")

source("modules/deconvo_tme_module.R")
source("modules/cell_bar_plot_module.R")
source("modules/tme_cluster_module.R")
source("modules/lr_cal_module.R")

source("modules/surv_group_module.R")
source("modules/percent_bar_plot_module.R")
source("modules/sig_roc_module.R")


source("modules/sig_gsea_module.R")
source("modules/find_markers_in_bulk_module.R")


source("modules/find_outlier_samples_module.R")
source("modules/remove_batcheffect_module.R")
source("modules/remove_duplicate_genes_module.R")
source("modules/mouse2human_eset_module.R")
source("modules/iobr_pca_module.R")


source("modules/make_mut_matrix_module.R")
source("modules/find_mutations_module.R")


source("modules/workflow_mutation_module.R")
#source("modules/workflow_signature_module.R")
#source("modules/workflow_tme_module.R")
source("modules/workflow_iobr_module.R")
source("modules/workflow_siggenes_module.R")

source("modules/datasets_overview_module.R")
source("modules/workflow_tcga_module.R")
source("modules/prepare_tcga_data_module.R")

source("modules/workflow_othercohort_module.R")
source("modules/prepare_othercohort_data_module.R")

source("modules/workflow_immunotherapy_module.R")
source("modules/prepare_immunotherapy_data_module.R")

source("modules/workflow_cancercohort_module.R")
source("modules/prepare_cancercohort_data_module.R")


ui <- shinyUI(
  #=== 1.bs4DashPage
  bs4DashPage(
    title = "IOBR",
    skin = NULL,
    freshTheme = NULL,
    options = NULL,
    fullscreen = FALSE, #全屏开关
    help = NULL, #help开关
    dark = FALSE,
    scrollToTop = TRUE,
    
    # 动画
    preloader = list(
      html = tagList(
        waiter::spin_3(),
        br(),
        h4("Loading IOBRportal...", style = "color: #003388; font-weight: bold;")
      ),
      color = "#ffffff"
    ),
    
    #=== 1.1 bs4DashNavbar
    header = bs4DashNavbar(
      brand = span("| IOBRportal is an integrated web platform powered by the IOBR package.",
                   style = "margin-left: 10px;
                            color: #003388;
                            font-size: 25px;
                            font-weight: bolder;
                            text-shadow: 3px 3px 10px #888888;"),
      titleWidth = NULL, # 标题区域宽度；NULL = 自动计算
      disable = FALSE, # TRUE 时整个 Navbar 不渲染
      .list = NULL, # 可一次性传递一个列表形式的参数集
      skin = "light",
      status = "white",
      border = TRUE,
      compact = FALSE,
      sidebarIcon = shiny::icon("bars"),
      controlbarIcon = shiny::icon("bars"),
      fixed = FALSE
    ),
    
    #=== 1.2 bs4DashSidebar
    sidebar = bs4DashSidebar(
      disable = FALSE,
      width = NULL,
      skin = "dark",
      status = "primary",
      elevation = 4,
      collapsed = FALSE,
      minified = TRUE,
      expandOnHover = TRUE,
      fixed = TRUE,
      customArea = NULL,
      
      #=== 1.2.1.1 bs4SidebarUserPanel
      bs4SidebarUserPanel(
        name = tags$span("IOBRportal", 
                         style = "font-size:20px;font-weight: bold;"), #大小为20px，加粗；
        image = "IOBR.png" # 可加上头像
      ),
      hr(),
      
      
      tags$style(HTML("
        #menu_mode .btn-group .btn {
        margin: 0 !important;         /* 去掉间隙 */
        font-weight: bold;
        border: none !important;      /* 去掉多余边框 */
        }

        /* 默认状态：白底蓝字 */
        #menu_mode .btn {
        background-color: white !important;
        color: #007bff !important;
        }

        /* 选中状态：蓝底白字 */
        #menu_mode .btn.active {
        background-color: #007bff !important;
        color: white !important;
        }
      ")),
      
      radioGroupButtons(
        inputId = "menu_mode",
        choices = c("Functions", "Workflows"),
        selected = "Functions",
        justified = TRUE,
        status = "primary"
      ),
      hr(),
      
      # ========== Functions ==========
      conditionalPanel(
        condition = "input.menu_mode == 'Functions'",
        
        #=== 1.2.1 bs4SidebarMenu
        bs4SidebarMenu(
          id = NULL,
          .list = NULL,
          flat = FALSE,
          compact = FALSE,
          childIndent = FALSE,
          legacy = FALSE,
          
          bs4SidebarMenuItem(
            text = "IOBRportal Introduction",
            tabName = "home",
            icon = icon("gear"),
            href = NULL,
            newTab = TRUE
          ),
          
          bs4SidebarMenuItem(
            text = "Data Preparation",
            tabName = NULL,
            icon = icon("tools"),
            expandedName = "Data Preparation",
            
            bs4SidebarMenuSubItem(
              text = "Counts to TPM",
              tabName = "count2tpm",
              icon = icon("sliders-h")
            ),
            
            bs4SidebarMenuSubItem(
              text = "Annotate ExpressionSet",
              tabName = "anno_eset",
              icon = icon("sliders-h")
            ),
            
            bs4SidebarMenuSubItem(
              text = "Detect Outlier Samples",
              tabName = "find_outlier_samples",
              icon = icon("sliders-h")
            ),
            
            bs4SidebarMenuSubItem(
              text = "Remove Duplicate Genes",
              tabName = "remove_duplicate_genes",
              icon = icon("sliders-h")
            ),
            
            bs4SidebarMenuSubItem(
              text = "Correct Batch Effect",
              tabName = "remove_batcheffect",
              icon = icon("sliders-h")
            ),
            
            bs4SidebarMenuSubItem(
              text = "Mouse-to-Human Genes",
              tabName = "mouse2human_eset",
              icon = icon("sliders-h")
            )
          ),
          
          
          bs4SidebarMenuItem(
            text = "SigScore Calculation",
            tabName = NULL,
            icon = icon("chart-pie"),
            expandedName = "SigScore Calculation",
            
            bs4SidebarMenuSubItem(
              text = "Calculate SigScores",
              tabName = "calculate_sig_score",
              icon = icon("sliders-h")
            )
          ),
          
          
          bs4SidebarMenuItem(
            text = "TME Deconvolution",
            tabName = NULL,
            icon = icon("dna"),
            expandedName = "TME Deconvolution",
            
            bs4SidebarMenuSubItem(
              text = "Deconvolute TME",
              tabName = "deconvo_tme",
              icon = icon("sliders-h")
            )
          ),
          

          bs4SidebarMenuItem(
            text = "Statistical Analysis",
            tabName = NULL,
            icon = icon("calculator"),
            expandedName = "Statistical Analysis",
            
            bs4SidebarMenuSubItem(
              text = "Batch Correlation",
              tabName = "batch_cor",
              icon = icon("sliders-h")
            ),
            
            bs4SidebarMenuSubItem(
              text = "Batch Partial Correlation",
              tabName = "batch_pcc",
              icon = icon("sliders-h")
            ),
            
            bs4SidebarMenuSubItem(
              text = "Batch Survival",
              tabName = "batch_surv",
              icon = icon("sliders-h")
            ),
            
            bs4SidebarMenuSubItem(
              text = "Batch Wilcoxon",
              tabName = "batch_wilcoxon",
              icon = icon("sliders-h")
            ),
            
            bs4SidebarMenuSubItem(
              text = "Batch Kruskal-Wallis",
              tabName = "batch_kruskal",
              icon = icon("sliders-h")
            )
          ),
          
          
          bs4SidebarMenuItem(
            text = "Visualization",
            tabName = NULL,
            icon = icon("chart-line"),
            expandedName = "Visualization",
            
            bs4SidebarMenuSubItem(
              text = "Heatmap",
              tabName = "sig_heatmap",
              icon = icon("sliders-h")
            ),
            
            bs4SidebarMenuSubItem(
              text = "Box Plot",
              tabName = "sig_box",
              icon = icon("sliders-h")
            ),
            
            bs4SidebarMenuSubItem(
              text = "Percentage Bar Plot",
              tabName = "percent_bar_plot",
              icon = icon("sliders-h")
            ),
            
            bs4SidebarMenuSubItem(
              text = "Cell Bar Plot",
              tabName = "cell_bar_plot",
              icon = icon("sliders-h")
            ),
            
            bs4SidebarMenuSubItem(
              text = "Forest Plot",
              tabName = "sig_forest",
              icon = icon("sliders-h")
            ),
            
            bs4SidebarMenuSubItem(
              text = "Correlation Plot",
              tabName = "get_cor",
              icon = icon("sliders-h")
            ),
            
            bs4SidebarMenuSubItem(
              text = "Correlation Matrix",
              tabName = "get_cor_matrix",
              icon = icon("sliders-h")
            ),
            
            bs4SidebarMenuSubItem(
              text = "Survival Group Plot",
              tabName = "surv_group",
              icon = icon("sliders-h")
            ),
            bs4SidebarMenuSubItem(
              text = "Survival Plots",
              tabName = "sig_surv_plot",
              icon = icon("sliders-h")
            ),
            
            bs4SidebarMenuSubItem(
              text = "Signature ROC Curves",
              tabName = "sig_roc",
              icon = icon("sliders-h")
            ),
            
            bs4SidebarMenuSubItem(
              text = "Time ROC Curves",
              tabName = "roc_time",
              icon = icon("sliders-h")
            ),
            
            bs4SidebarMenuSubItem(
              text = "PCA Visualization",
              tabName = "iobr_pca",
              icon = icon("sliders-h")
            ),
            
            bs4SidebarMenuSubItem(
              text = "Signature GSEA",
              tabName = "sig_gsea",
              icon = icon("sliders-h")
            ),
            
            bs4SidebarMenuSubItem(
              text = "Find Markers",
              tabName = "find_markers_in_bulk",
              icon = icon("sliders-h")
            )
          ),
          
          
          bs4SidebarMenuItem(
            text = "TME Interaction",
            tabName = NULL,
            icon = icon("project-diagram"), 
            expandedName = "TME Interaction",
            
            bs4SidebarMenuSubItem(
              text = "TME Clustering",
              tabName = "tme_cluster",
              icon = icon("sliders-h")
            ),
            
            bs4SidebarMenuSubItem(
              text = "Ligand-Receptor Interaction",
              tabName = "lr_cal",
              icon = icon("sliders-h")
            )
          ),
          
          
          bs4SidebarMenuItem(
            text = "Mutation Module",
            tabName = NULL,
            icon = icon("band-aid"),  
            expandedName = "Mutation Module",
            
            bs4SidebarMenuSubItem(
              text = "Build Mutation Matrix",
              tabName = "make_mut_matrix",
              icon = icon("sliders-h")
            ),
            bs4SidebarMenuSubItem(
              text = "Identify Mutations",
              tabName = "find_mutations",
              icon = icon("sliders-h")
            )
          )
          
          # bs4SidebarMenuItem(
          #   text = "Help",
          #   tabName = NULL,
          #   icon = icon("question-circle"),
          #   expandedName = "Help",
          #   startExpanded = TRUE
          # )
        )),
      hr(),
      
      # ========== Workflows ==========
      conditionalPanel(
        condition = "input.menu_mode == 'Workflows'",
        
        bs4SidebarMenu(
          id = "workflow_menu", # 点击跳转的逻辑所在
          
          bs4SidebarMenuItem(
            text = "Datasets",
            tabName = NULL,
            icon = icon("database"),
            startExpanded = TRUE,
            
            bs4SidebarMenuSubItem(
              text = "Overview", 
              tabName = "datasets_overview",
              icon = icon("chart-pie")
            ),
            
            bs4SidebarMenuSubItem(
              text = "TCGA Cohorts",
              tabName = "workflow_tcga",
              icon = icon("align-left") 
            ),
            
            bs4SidebarMenuSubItem(
              text = "Cancer Cohorts",
              tabName = "workflow_cancercohort",
              icon = icon("globe") 
            ),
            
            bs4SidebarMenuSubItem(
              text = "Immunotherapy Cohorts",
              tabName = "workflow_immunotherapy",
              icon = icon("syringe")
            ),
            
            bs4SidebarMenuSubItem(
              text = "Other Cohorts",
              tabName = "workflow_othercohort",
              icon = icon("folder-open")
            )
          ),

          
          bs4SidebarMenuItem(
            text = "Analysis Modules", # 建议叫 Analysis Modules 或 Data Modules
            tabName = NULL,
            icon = icon("project-diagram"), # 使用 project-diagram 体现模块化
            startExpanded = TRUE,
            
            bs4SidebarMenuSubItem(
              text = "IOBR Analysis", # 就是workflow_iobr
              tabName = "workflow_iobr", 
              icon = icon("object-group") # 使用 object-group 体现“结合/整合”的意思
            ),
            
            bs4SidebarMenuSubItem(
              text = "Mutation Analysis", 
              tabName = "workflow_mutation", 
              icon = icon("dna")
            ),
            
            bs4SidebarMenuSubItem(
              text = "Signature-Gene Analysis", 
              tabName = "workflow_siggenes", 
              icon = icon("chart-line")
            )
          )
        )
      ),
      hr()
    
    ),
    
    #=== 1.3 bs4DashControlbar
    controlbar = bs4DashControlbar(
      style = "padding: 10px;",
      id = NULL,
      disable = FALSE,
      width = 300,
      collapsed = TRUE,
      overlay = TRUE,
      skin = "light",
      pinned = FALSE,
      
      skinSelector()  # 皮肤选择器
    ),
    
    #=== 1.4 bs4DashFooter
    footer = bs4DashFooter(
      left = span("xxxx", style = "font-weight:bold"),
      right = NULL,
      fixed = TRUE
    ),
    
    #=== 1.5 bs4DashBody (必须要加上这个 body)
    body = bs4DashBody(
      
      tags$head(
        tags$style(HTML("
        /* 一劳永逸压缩所有 fileInput 的下边距 */
        .shiny-input-container input[type='file'] {
        margin-bottom: 0px !important;
        }
    
        /* 压缩其外层容器（默认会有 mb-3） */
        .shiny-input-container {
        margin-bottom: 4px !important;
        }
    
        /* 同时也让所有按钮更紧贴上一行（Run 按钮等） */
        .shiny-input-container + div > .btn {
        margin-top: 0px !important;
        }
        "))
      ),
      
      bs4TabItems(
        
        #figure
        # bs4TabItem(
        #   tabName = "home",
        #   fluidRow(
        #     column(
        #       width = 12,
        #       h3(tags$b("Welcome to IOBRportal!")),   # 加粗标题
        #       tags$img(
        #         src = "IOBRportal overview.png",
        #         style = "width: 100%; height: auto; display: block; margin: 0 auto;"
        #       ),
        #       br(),
        #       tags$p(
        #         HTML("📖 Detailed Tutorial: 
        #       <a href='https://iobr.github.io/book/IOBR' target='_blank'
        #          style='font-size:18px; color:#003388; font-weight:bold;'>
        #          https://iobr.github.io/book/IOBR</a>"),
        #         style = "margin: 2px 0;"   # 控制上下间距
        #       ),
        #       tags$p(
        #         HTML("💻 Code Repository: 
        #       <a href='https://github.com/IOBR/IOBRshiny' target='_blank'
        #          style='font-size:18px; color:#003388; font-weight:bold;'>
        #          https://github.com/IOBR/IOBRshiny</a>"),
        #         style = "margin: 2px 0;"   # 控制上下间距
        #       )
        #     )
        #   )
        # ),
        
        
        home_introUI("home_intro"),
        #function
        count2tpmUI("mod_count2tpm"),
        anno_esetUI("mod_anno_eset"),
        
        calculate_sig_scoreUI("mod_calculate_sig_score"),
        sig_heatmapUI("mod_sig_heatmap"),
        sig_boxUI("mod_sig_box"),
        sig_surv_plotUI("mod_sig_surv_plot"),
        roc_timeUI("mod_roc_time"),
        batch_survUI("mod_batch_surv"),
        sig_forestUI("mod_sig_forest"),
        batch_corUI("mod_batch_cor"),
        get_corUI("mod_get_cor"),
        get_cor_matrixUI("mod_get_cor_matrix"),
        batch_pccUI("mod_batch_pcc"),
        batch_wilcoxonUI("mod_batch_wilcoxon"),
        batch_kruskalUI("mod_batch_kruskal"),
        
        deconvo_tmeUI("mod_deconvo_tme"),
        tme_clusterUI("mod_tme_cluster"),
        cell_bar_plotUI("mod_cell_bar_plot"),
        surv_groupUI("mod_surv_group"),
        sig_rocUI("mod_sig_roc"),
        percent_bar_plotUI("mod_percent_bar_plot"),
        lr_calUI("mod_lr_cal"),
        sig_gseaUI("mod_sig_gsea"),
        find_markers_in_bulkUI("mod_find_markers_in_bulk"),
        
        make_mut_matrixUI("mod_make_mut_matrix"),
        find_mutationsUI("mod_find_mutations"),
        
        datasets_overviewUI("mod_datasets_overview"),
        prepare_tcga_dataUI("mod_prepare_tcga_data"),
        prepare_othercohort_dataUI("mod_prepare_othercohort_data"),
        prepare_immunotherapy_dataUI("mod_prepare_immunotherapy_data"),
        prepare_cancercohort_dataUI("mod_prepare_cancercohort_data"),
        
        
        
        #workflow
        workflow_mutationUI("mod_workflow_mutation"),
        #workflow_signatureUI("mod_workflow_signature"),
        #workflow_tmeUI("mod_workflow_tme"),
        workflow_iobrUI("mod_workflow_iobr"),
        workflow_siggenesUI("mod_workflow_siggenes"),
        
        workflow_tcgaUI("mod_workflow_tcga"),
        workflow_othercohortUI("mod_workflow_othercohort"),
        workflow_immunotherapyUI("mod_workflow_immunotherapy"),
        workflow_cancercohortUI("mod_workflow_cancercohort"),
        
        
        find_outlier_samplesUI("mod_find_outlier_samples"),
        remove_batcheffectUI("mod_remove_batcheffect"),
        remove_duplicate_genesUI("mod_remove_duplicate_genes"),
        mouse2human_esetUI("mod_mouse2human_eset"),
        iobr_pcaUI("mod_iobr_pca")

      )
    )
    
  )
)


server <- function(input, output, session) {
  
  shinyhelper::observe_helpers(withMathJax = TRUE) #识别shinyhelper
  
  #function
  home_introServer("home_intro", parent_session = session)
  count2tpmServer("mod_count2tpm")
  anno_esetServer("mod_anno_eset")
  
  
  calculate_sig_scoreServer("mod_calculate_sig_score")
  sig_heatmapServer("mod_sig_heatmap")
  sig_boxServer("mod_sig_box")
  sig_surv_plotServer("mod_sig_surv_plot")
  roc_timeServer("mod_roc_time")
  batch_survServer("mod_batch_surv")
  sig_forestServer("mod_sig_forest")
  batch_corServer("mod_batch_cor")
  get_corServer("mod_get_cor")
  get_cor_matrixServer("mod_get_cor_matrix")
  batch_pccServer("mod_batch_pcc")
  batch_wilcoxonServer("mod_batch_wilcoxon")
  batch_kruskalServer("mod_batch_kruskal")
  
  deconvo_tmeServer("mod_deconvo_tme")
  tme_clusterServer("mod_tme_cluster")
  cell_bar_plotServer("mod_cell_bar_plot")
  surv_groupServer("mod_surv_group")
  sig_rocServer("mod_sig_roc")
  percent_bar_plotServer("mod_percent_bar_plot")
  lr_calServer("mod_lr_cal")
  sig_gseaServer("mod_sig_gsea")
  find_markers_in_bulkServer("mod_find_markers_in_bulk")
  
  
  make_mut_matrixServer("mod_make_mut_matrix")
  find_mutationsServer("mod_find_mutations")
  
  datasets_overviewServer("mod_datasets_overview", parent_session = session)
  prepare_tcga_dataServer("mod_prepare_tcga_data", pool = tcga_pool)
  prepare_othercohort_dataServer("mod_prepare_othercohort_data", pool = othercohort_pool)
  prepare_immunotherapy_dataServer("mod_prepare_immunotherapy_data", pool = immunotherapy_pool)
  prepare_cancercohort_dataServer("mod_prepare_cancercohort_data", pool = cancercohort_pool)
  
  #workflow
  workflow_mutationServer("mod_workflow_mutation")
  #workflow_signatureServer("mod_workflow_signature")
  #workflow_tmeServer("mod_workflow_tme")
  workflow_iobrServer("mod_workflow_iobr")
  workflow_siggenesServer("mod_workflow_siggenes")

  
  workflow_tcgaServer("mod_workflow_tcga", pool = tcga_pool)
  workflow_othercohortServer("mod_workflow_othercohort", pool = othercohort_pool)
  workflow_immunotherapyServer("mod_workflow_immunotherapy", pool = immunotherapy_pool)
  workflow_cancercohortServer("mod_workflow_cancercohort", pool = cancercohort_pool)
  
  
  find_outlier_samplesServer("mod_find_outlier_samples")
  remove_batcheffectServer("mod_remove_batcheffect")
  remove_duplicate_genesServer("mod_remove_duplicate_genes")
  mouse2human_esetServer("mod_mouse2human_eset")

  
  iobr_pcaServer("mod_iobr_pca")


  
}


shinyApp(ui, server)
