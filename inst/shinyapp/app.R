library(shiny)
library(DT)
library(bs4Dash)
library(shinyWidgets)
library(zip)
library(shinyhelper)
library(duckdb)
library(dplyr)
library(dbplyr)
library(pool)
library(DBI)
library(echarts4r)
library(waiter)
library(readxl)
library(writexl)
library(IOBR)

options(shiny.maxRequestSize = 1024 * 1024^2)
options(shiny.autoreload = TRUE)

options(bitmapType = "cairo")
pdf(NULL) 

# ------------------------------
# 1. Database mode
# ------------------------------
# 自动判断是否启用数据库模块：

db_files <- c(
  tcga          = "data/tcga_signatures.duckdb",
  othercohort   = "data/othercohort.duckdb",
  immunotherapy = "data/immunotherapy.duckdb",
  cancercohort  = "data/cancercohort.duckdb"
)

database_available <- all(file.exists(db_files))

if (database_available) {
  message("IOBRportal database mode: enabled.")
} else {
  message("IOBRportal local mode: database files not found. Database-related modules are disabled.")
  message("Missing database files: ", paste(names(db_files)[!file.exists(db_files)], collapse = ", "))
}


# ------------------------------
# 2. Create database pools safely
# ------------------------------

make_duckdb_pool <- function(db_path) {
  if (!database_available) {
    return(NULL)
  }
  
  dbPool(
    duckdb(),
    dbdir = db_path,
    read_only = TRUE
  )
}

tcga_pool          <- make_duckdb_pool(db_files[["tcga"]])
othercohort_pool   <- make_duckdb_pool(db_files[["othercohort"]])
immunotherapy_pool <- make_duckdb_pool(db_files[["immunotherapy"]])
cancercohort_pool  <- make_duckdb_pool(db_files[["cancercohort"]])


# ------------------------------
# 3. Close database pools safely
# ------------------------------

close_pool_safely <- function(pool_obj, pool_name) {
  if (!is.null(pool_obj)) {
    poolClose(pool_obj)
    message("Closed ", pool_name, " pool.")
  }
}

onStop(function() {
  close_pool_safely(tcga_pool, "TCGA")
  close_pool_safely(othercohort_pool, "OtherCohort")
  close_pool_safely(immunotherapy_pool, "Immunotherapy")
  close_pool_safely(cancercohort_pool, "CancerCohort")
})

# ------------------------------
# 4. Source modules
# ------------------------------
# Helper functions
source("modules/load_iobr_signature_names.R")

source("modules/upload_module.R")
source("modules/data_download_module.R")
source("modules/dataTable_module.R")
source("modules/plot_download_module.R")

# functions
source("modules/home_intro_module.R")
source("modules/count2tpm_module.R")
source("modules/anno_eset_module.R")
source("modules/calculate_sig_score_module.R")
source("modules/combine_pd_eset_module.R")
source("modules/find_outlier_samples_module.R")
source("modules/remove_batcheffect_module.R")
source("modules/remove_duplicate_genes_module.R")
source("modules/mouse2human_eset_module.R")
source("modules/iobr_pca_module.R")
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
source("modules/surv_group_module.R")
source("modules/percent_bar_plot_module.R")
source("modules/sig_roc_module.R")

source("modules/deconvo_tme_module.R")
source("modules/cell_bar_plot_module.R")
source("modules/tme_cluster_module.R")
source("modules/lr_cal_module.R")

source("modules/sig_gsea_module.R")
source("modules/find_markers_in_bulk_module.R")

source("modules/make_mut_matrix_module.R")
source("modules/find_mutations_module.R")
source("modules/workflow_mutation_module.R")

source("modules/workflow_iobr_module.R")
source("modules/workflow_siggenes_module.R")


# 只有数据库可用时，才 source 数据库相关模块。
if (database_available) {
  source("modules/datasets_overview_module.R")
  source("modules/workflow_tcga_module.R")
  source("modules/prepare_tcga_data_module.R")
  
  source("modules/workflow_othercohort_module.R")
  source("modules/prepare_othercohort_data_module.R")
  
  source("modules/workflow_immunotherapy_module.R")
  source("modules/prepare_immunotherapy_data_module.R")
  
  source("modules/workflow_cancercohort_module.R")
  source("modules/prepare_cancercohort_data_module.R")
}


# ------------------------------
# 5. Optional database UI items
# ------------------------------

database_sidebar_items <- list()
database_tab_items     <- list()

if (database_available) {
  
  database_sidebar_items <- list(
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
        text = "TCGA",
        tabName = "workflow_tcga",
        icon = icon("align-left") 
      ),
      
      bs4SidebarMenuSubItem(
        text = "MOLC",
        tabName = "workflow_cancercohort",
        icon = icon("globe") 
      ),
      
      bs4SidebarMenuSubItem(
        text = "IMMC",
        tabName = "workflow_immunotherapy",
        icon = icon("syringe")
      ),
      
      bs4SidebarMenuSubItem(
        text = "CLIC",
        tabName = "workflow_othercohort",
        icon = icon("folder-open")
      )
    )
  )
  
  database_tab_items <- list(
    datasets_overviewUI("mod_datasets_overview"),
    prepare_tcga_dataUI("mod_prepare_tcga_data"),
    prepare_othercohort_dataUI("mod_prepare_othercohort_data"),
    prepare_immunotherapy_dataUI("mod_prepare_immunotherapy_data"),
    prepare_cancercohort_dataUI("mod_prepare_cancercohort_data"),
    
    workflow_tcgaUI("mod_workflow_tcga"),
    workflow_othercohortUI("mod_workflow_othercohort"),
    workflow_immunotherapyUI("mod_workflow_immunotherapy"),
    workflow_cancercohortUI("mod_workflow_cancercohort")
  )
}


# ------------------------------
# 6. Workflow sidebar
# ------------------------------

workflow_sidebar_menu <- do.call(
  bs4SidebarMenu,
  c(
    list(id = "workflow_menu"),
    
    database_sidebar_items,
    
    list(
      bs4SidebarMenuItem(
        text = "Workflows",
        tabName = NULL,
        icon = icon("project-diagram"),
        startExpanded = TRUE,
        
        bs4SidebarMenuSubItem(
          text = "IOBR Workflows",
          tabName = "workflow_iobr", 
          icon = icon("object-group")
        ),
        
        bs4SidebarMenuSubItem(
          text = "Mutation Workflows", 
          tabName = "workflow_mutation", 
          icon = icon("dna")
        ),
        
        bs4SidebarMenuSubItem(
          text = "Signature-Gene Workflows", 
          tabName = "workflow_siggenes", 
          icon = icon("chart-line")
        )
      )
    )
  )
)


# ------------------------------
# 7. Body tab items
# ------------------------------

body_tab_items <- c(
  list(
    home_introUI("home_intro"),
    
    # function
    count2tpmUI("mod_count2tpm"),
    anno_esetUI("mod_anno_eset"),
    remove_duplicate_genesUI("mod_remove_duplicate_genes"),
    find_outlier_samplesUI("mod_find_outlier_samples"),
    remove_batcheffectUI("mod_remove_batcheffect"),
    mouse2human_esetUI("mod_mouse2human_eset"),
    
    calculate_sig_scoreUI("mod_calculate_sig_score"),
    deconvo_tmeUI("mod_deconvo_tme"),
    tme_clusterUI("mod_tme_cluster"),
    lr_calUI("mod_lr_cal"),
    
    batch_survUI("mod_batch_surv"),
    batch_corUI("mod_batch_cor"),
    batch_pccUI("mod_batch_pcc"),
    batch_wilcoxonUI("mod_batch_wilcoxon"),
    batch_kruskalUI("mod_batch_kruskal"),
    
    sig_heatmapUI("mod_sig_heatmap"),
    sig_boxUI("mod_sig_box"),
    sig_surv_plotUI("mod_sig_surv_plot"),
    roc_timeUI("mod_roc_time"),
    sig_forestUI("mod_sig_forest"),
    get_corUI("mod_get_cor"),
    get_cor_matrixUI("mod_get_cor_matrix"),
    iobr_pcaUI("mod_iobr_pca"),
    cell_bar_plotUI("mod_cell_bar_plot"),
    surv_groupUI("mod_surv_group"),
    sig_rocUI("mod_sig_roc"),
    percent_bar_plotUI("mod_percent_bar_plot"),
    sig_gseaUI("mod_sig_gsea"),
    find_markers_in_bulkUI("mod_find_markers_in_bulk"),
    
    make_mut_matrixUI("mod_make_mut_matrix"),
    find_mutationsUI("mod_find_mutations"),
    
    # workflow
    workflow_mutationUI("mod_workflow_mutation"),
    workflow_iobrUI("mod_workflow_iobr"),
    workflow_siggenesUI("mod_workflow_siggenes")
  ),
  
  database_tab_items
)

app_tab_items <- do.call(bs4TabItems, body_tab_items)

# ------------------------------
# 8. UI
# ------------------------------

ui <- shinyUI(
  bs4DashPage(
    title = "IOBRportal",
    skin = NULL,
    freshTheme = NULL,
    options = NULL,
    fullscreen = FALSE,
    help = NULL,
    dark = FALSE,
    scrollToTop = TRUE,
    
    preloader = list(
      html = tagList(
        h4("Loading IOBRportal...", style = "color: #003388; font-weight: bold;"),
        br(),
        waiter::spin_3()
      ),
      color = "#ffffff"
    ),
    
    header = bs4DashNavbar(
      brand = span(
        "| IOBRportal is an integrated web platform powered by the IOBR package.",
        style = "margin-left: 10px;
                 color: #003388;
                 font-size: 25px;
                 font-weight: bolder;
                 text-shadow: 3px 3px 10px #888888;"
      ),
      titleWidth = NULL,
      disable = FALSE,
      .list = NULL,
      skin = "light",
      status = "white",
      border = TRUE,
      compact = FALSE,
      sidebarIcon = shiny::icon("bars"),
      controlbarIcon = shiny::icon("bars"),
      fixed = FALSE
    ),
    
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
      
      bs4SidebarUserPanel(
        name = tags$span(
          "IOBRportal", 
          style = "font-size:20px;font-weight: bold;"
        ),
        image = "IOBRportal.png"
      ),
      hr(),
      
      tags$style(HTML("
        #menu_mode .btn-group .btn {
          margin: 0 !important;
          font-weight: bold;
          border: none !important;
        }

        #menu_mode .btn {
          background-color: white !important;
          color: #007bff !important;
        }

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
              text = "Survival Plots",
              tabName = "sig_surv_plot",
              icon = icon("sliders-h")
            ),
            
            bs4SidebarMenuSubItem(
              text = "Survival Group Plot",
              tabName = "surv_group",
              icon = icon("sliders-h")
            ),
            
            bs4SidebarMenuSubItem(
              text = "Time ROC Curves",
              tabName = "roc_time",
              icon = icon("sliders-h")
            ),
            
            bs4SidebarMenuSubItem(
              text = "Signature ROC Curves",
              tabName = "sig_roc",
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
        )
      ),
      hr(),
      
      # ========== Workflows ==========
      conditionalPanel(
        condition = "input.menu_mode == 'Workflows'",
        workflow_sidebar_menu
      ),
      hr()
    ),
    
    controlbar = bs4DashControlbar(
      style = "padding: 10px;",
      id = NULL,
      disable = FALSE,
      width = 300,
      collapsed = TRUE,
      overlay = TRUE,
      skin = "light",
      pinned = FALSE,
      
      skinSelector()
    ),
    
    footer = bs4DashFooter(
      left = span(
        "Contact with: Qingcong Luo (qingcongl@163.com) or Dr. Dongqiang Zeng (interlaken@smu.edu.cn)",
        style = "font-weight:bold"
      ),
      right = NULL,
      fixed = TRUE
    ),
    
    body = bs4DashBody(
      tags$head(
        tags$style(HTML("
          .shiny-input-container input[type='file'] {
            margin-bottom: 0px !important;
          }
      
          .shiny-input-container {
            margin-bottom: 4px !important;
          }
      
          .shiny-input-container + div > .btn {
            margin-top: 0px !important;
          }
        "))
      ),
      
      app_tab_items
    )
  )
)


# ------------------------------
# 9. Server
# ------------------------------

server <- function(input, output, session) {
  session$allowReconnect("force") # 允许网络波动时自动重连，保留之前的计算状态
  
  shinyhelper::observe_helpers(withMathJax = TRUE)
  
  # function
  home_introServer("home_intro", parent_session = session)
  count2tpmServer("mod_count2tpm")
  anno_esetServer("mod_anno_eset")
  find_outlier_samplesServer("mod_find_outlier_samples")
  remove_duplicate_genesServer("mod_remove_duplicate_genes")
  remove_batcheffectServer("mod_remove_batcheffect")
  mouse2human_esetServer("mod_mouse2human_eset")
  
  calculate_sig_scoreServer("mod_calculate_sig_score")
  deconvo_tmeServer("mod_deconvo_tme")
  tme_clusterServer("mod_tme_cluster")
  lr_calServer("mod_lr_cal")
  
  batch_survServer("mod_batch_surv")
  batch_corServer("mod_batch_cor")
  batch_pccServer("mod_batch_pcc")
  batch_wilcoxonServer("mod_batch_wilcoxon")
  batch_kruskalServer("mod_batch_kruskal")
  
  sig_heatmapServer("mod_sig_heatmap")
  sig_boxServer("mod_sig_box")
  sig_surv_plotServer("mod_sig_surv_plot")
  roc_timeServer("mod_roc_time")
  sig_forestServer("mod_sig_forest")
  get_corServer("mod_get_cor")
  iobr_pcaServer("mod_iobr_pca")
  get_cor_matrixServer("mod_get_cor_matrix")
  cell_bar_plotServer("mod_cell_bar_plot")
  surv_groupServer("mod_surv_group")
  sig_rocServer("mod_sig_roc")
  percent_bar_plotServer("mod_percent_bar_plot")
  sig_gseaServer("mod_sig_gsea")
  find_markers_in_bulkServer("mod_find_markers_in_bulk")
  
  make_mut_matrixServer("mod_make_mut_matrix")
  find_mutationsServer("mod_find_mutations")
  
  # workflow
  workflow_mutationServer("mod_workflow_mutation")
  workflow_iobrServer("mod_workflow_iobr")
  workflow_siggenesServer("mod_workflow_siggenes")
  # database workflow
  if (database_available) {
    datasets_overviewServer("mod_datasets_overview", parent_session = session)
    
    prepare_tcga_dataServer("mod_prepare_tcga_data", pool = tcga_pool)
    prepare_othercohort_dataServer("mod_prepare_othercohort_data", pool = othercohort_pool)
    prepare_immunotherapy_dataServer("mod_prepare_immunotherapy_data", pool = immunotherapy_pool)
    prepare_cancercohort_dataServer("mod_prepare_cancercohort_data", pool = cancercohort_pool)
    
    workflow_tcgaServer("mod_workflow_tcga", pool = tcga_pool)
    workflow_othercohortServer("mod_workflow_othercohort", pool = othercohort_pool)
    workflow_immunotherapyServer("mod_workflow_immunotherapy", pool = immunotherapy_pool)
    workflow_cancercohortServer("mod_workflow_cancercohort", pool = cancercohort_pool)
  }
  
  # 当用户关闭网页或断开连接时触发
  session$onSessionEnded(function() {
    message(paste("User session ended. Running garbage collection..."))
    gc() # 强制释放不再使用的内存
  })
}

shinyApp(ui, server)