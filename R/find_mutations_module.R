# =========================================================
# Module: find_mutations
# =========================================================

# ---- Full UI ----
find_mutationsUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "find_mutations",
    h3(HTML(
      "Find_mutations
      <span style='font-size:80%; color:#333;'>:
       Identifies phenotype-associated mutations.</span>"
      )),
    find_mutationsBodyUI(id)
  )
}

# ---- BodyUI ----
find_mutationsBodyUI <- function(id, include_upload = TRUE) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 3,
      
      bs4Card(
        title         = "Parameter",
        status        = "gray",
        headerBorder  = TRUE,
        collapsible   = TRUE,
        solidHeader   = TRUE,
        width         = 12,
        maximizable   = TRUE,

        #仅当 include_upload=TRUE 时渲染上传组件
        if (isTRUE(include_upload)) {
          tagList(
            uploadUI(ns("upload")) %>%
              helper(
                type    = "markdown",
                icon    = "question-circle",
                size    = "m",
                colour  = "#007bff",
                content = "demo_mut"
              )
          )
        },

        fileInput(
          inputId   = ns("find_mutations_input2"),
          label     = "Signature Matrix",
          multiple  = FALSE,
          accept    = NULL,
          width     = NULL,
          buttonLabel = "Browse",
          placeholder = "Choose a file"
        )%>%
          helper(
            type    = "markdown",
            icon    = "question-circle",
            size    = "m",
            colour  = "#007bff",
            content = "demo_stad_sig"
          ),
       
        div(
          style = "text-align: center; margin-top: 10px;",
          actionButton(
            inputId = ns("run_find_mutations"),
            label   = "Run Analysis",
            class   = "btn-primary"
          )
        ),
        
        textInput(
          inputId = ns("id_signature_matrix"),
          label   = "ID Column",
          value   = "ID"
        ),

        pickerInput(
          inputId = ns("signature"),
          label   = "Signature",
          choices = NULL, # 初始为空，等待文件上传
          options = pickerOptions(
            liveSearch = TRUE, 
            size = 10,
            style = "btn-outline-secondary", 
            dropupAuto = FALSE,
            container = "body"
          )
        ),
        
        selectInput(
          inputId = ns("min_mut_freq"), 
          label   = "Min Mutation Frequency", 
          choices = c(0.01, 0.05, 0.1), 
          selected = 0.01
        ),
        
        selectInput(
          inputId = ns("method"),
          label   = "Method",
          choices = c("Multi(Cuzick and Wilcoxon)" = "multi", "Wilcoxon" = "Wilcoxon"),
          selected = "multi"
        )
      ),

      # ==== Oncoprint ====
      bs4Card(
        title         = "Parameter for oncoprint",
        status        = "gray",
        headerBorder  = TRUE,
        collapsible   = TRUE,
        solidHeader   = TRUE,
        width         = 12,
        maximizable   = TRUE,
        collapsed     = TRUE,

        selectInput(
          inputId = ns("oncoprint_group_by"),
          label   = "Group By",
          choices = c("Mean" = "mean", "Quantile" = "quantile3"),
          selected = "mean"
        ),

        # textInput(
        #   inputId = ns("oncoprint_cols"),
        #   label   = "Colors",
        #   value   = "#224444",
        #   placeholder = "e.g., #224444"
        # ),
        
        sliderInput(
          inputId = ns("gene_counts"),
          label   = "Gene Counts",
          min     = 0,
          max     = 50,
          value   = 10,
          step    = 1
        )
      ),

      # ==== Box Plot ====
      bs4Card(
        title         = "Parameter for box plot",
        status        = "gray",
        headerBorder  = TRUE,
        collapsible   = TRUE,
        solidHeader   = TRUE,
        width         = 12,
        maximizable   = TRUE,
        collapsed     = TRUE,

        sliderInput(
          inputId = ns("point_size"),
          label   = "Point Size",
          min     = 0,
          max     = 10,
          value   = 4.5,
          step    = 0.1
        ),

        sliderInput(
          inputId = ns("point_alpha"),
          label   = "Point Transparency",
          min     = 0,
          max     = 1,
          value   = 0.1,
          step    = 0.1
        ),

        # selectInput(
        #   inputId = ns("palette"),
        #   label   = "Palette",
        #   choices = c("nrc","jama","aaas","jco","paired1","paired2","paired3","paired4","accent","set2"),
        #   selected = "jama"
        # ),

        selectInput(
          inputId = ns("jitter"),
          label   = "Show Jitter",
          choices = c("True" = "T", "False" = "F"),
          selected = "F"
        )
      ),

      bs4Card(
        title         = "All Results Download",
        status        = "gray",
        headerBorder  = TRUE,
        collapsible   = TRUE,
        solidHeader   = TRUE,
        width         = 12,
        maximizable   = TRUE,

        textInput(
          inputId = ns("save_path"),
          label   = "Output Folder Name",
          value   = "mutation_results"
        ),

        div(
          style = "text-align: center; margin-top: 10px;",
          downloadButton(
            outputId = ns("results_download"),
            label    = "Download all plots",
            class    = "btn-primary",
            icon     = icon("download")
          )
        )
      )
    ),

    column(
      width = 9,
      bs4Card(
        title         = "Plot and Data",
        status        = "gray",
        headerBorder  = TRUE,
        collapsible   = TRUE,
        solidHeader   = TRUE,
        width         = 12,
        maximizable   = TRUE,

        tabBox(
          width = 12,
          side  = "left",

          # Tab 1: Oncoprint
          tabPanel(
            "Oncoprint", 
            # 使用通用模块控制尺寸和下载
            plotDownloadUI(ns("plot_download_onco")),
            # 动态容器
            uiOutput(ns("oncoprint_container"))
          ),
          
          # Tab 2: Box Plot (修改此处，使其和 Oncoprint 格式一致)
          tabPanel(
            "Box Plot", 
            # 使用通用模块控制尺寸和下载
            plotDownloadUI(ns("plot_download_box")),
            # 动态容器
            uiOutput(ns("boxplot_container"))
          )
        )
      )
    )
  )
}


find_mutationsServer <- function(id, external_mut_matrix = NULL) {
  moduleServer(id, function(input, output, session) {

    # 1. 突变矩阵输入逻辑
    mut_data <- if (!is.null(external_mut_matrix)) {
      external_mut_matrix
    } else {
      uploadServer("upload")
    }

    # 2. 定义响应式变量
    res_onco_plot <- reactiveVal(NULL) 
    res_box_plot  <- reactiveVal(NULL) 
    res_folder    <- reactiveVal(NULL)

    sig_mat_data <- reactive({
      req(input$find_mutations_input2)
      
      sig_file <- input$find_mutations_input2
      ext      <- tools::file_ext(sig_file$name)
      
      data <- tryCatch({
        if (ext == "csv") {
          read.csv(sig_file$datapath, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
        } else if (ext == "tsv") {
          read.delim(sig_file$datapath, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
        } else {
          read.table(sig_file$datapath, header = TRUE, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)
        }
      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error")
        return(NULL)
      })
      
      return(data)
    })

    # 4. [逻辑优化] 监听读取好的数据，动态更新 Signature 选项
    observeEvent(sig_mat_data(), {
      data <- sig_mat_data()

      all_cols <- gsub("\\.", "-", colnames(data))
      is_num <- sapply(data, is.numeric)
      non_numeric_cols <- all_cols[!is_num]
      non_numeric_cols <- setdiff(non_numeric_cols, "ID") #只有字符，删去ID列

      pool_numeric <- all_cols[is_num] # 初始数字池
      blacklist_pattern <- "time|status|os|id"
      is_clinical <- grepl(blacklist_pattern, pool_numeric, ignore.case = TRUE)
      numeric_cols <- pool_numeric[!is_clinical]
      
      updatePickerInput(
        session  = session,
        inputId  = "signature",
        choices  = numeric_cols,
        selected = character(0)
      )
    })

    # 5. 主分析逻辑
    observeEvent(input$run_find_mutations, {
      # 依赖：突变数据、Signature数据(reactive)、Signature选择
      req(mut_data(), sig_mat_data(), input$signature)

      withProgress(message = "Running find_mutations...", value = 0, {
        
        setProgress(0.2, message = "Processing input data...")
        
        sig_data <- sig_mat_data()
        
        if(is.null(sig_data)) {
          showNotification("Signature Matrix is empty or invalid.", type = "error")
          return(NULL)
        }

        colnames(sig_data) <- gsub("\\.", "-", colnames(sig_data))

        # --- 生成结果文件夹 (相对路径) ---
        # folder_name <- paste0("Mutation_Results_", format(Sys.time(), "%Y%m%d_%H%M%S"))
        # if(!dir.exists(folder_name)) dir.create(folder_name)
        # res_folder(folder_name)
        # --- 生成结果文件夹（临时目录，不污染项目目录） ---
        tmp_root <- file.path(tempdir(), paste0("find_mutations_", session$token))
        if (!dir.exists(tmp_root)) dir.create(tmp_root, recursive = TRUE)

        folder_name <- paste0("Mutation_Results_", format(Sys.time(), "%Y%m%d_%H%M%S"))
        full_tmp_folder <- file.path(tmp_root, folder_name)
        if (!dir.exists(full_tmp_folder)) dir.create(full_tmp_folder, recursive = TRUE)

        # 存完整路径，供 ZIP 下载使用
        res_folder(full_tmp_folder)

        # --- 运行分析 ---
        setProgress(0.5, message = "Running analysis...")
        
        res <- tryCatch({

          old_wd <- getwd()
          on.exit(setwd(old_wd), add = TRUE)
          setwd(tmp_root)

          find_mutations(
            mutation_matrix      = mut_data(),
            signature_matrix     = sig_data,
            id_signature_matrix  = input$id_signature_matrix,
            signature            = input$signature,
            min_mut_freq         = as.numeric(input$min_mut_freq),
            method               = input$method,
            save_path            = folder_name,
            plot                 = TRUE,
            show_plot            = FALSE, 
            palette              = "jama",
            show_col             = FALSE,           
            oncoprint_group_by   = input$oncoprint_group_by,
            oncoprint_col        = "#224444",
            gene_counts          = input$gene_counts,
            jitter               = input$jitter == "T",
            point_size           = input$point_size,
            point_alpha          = input$point_alpha
          )
        }, error = function(e) {
          setProgress(1, message = "Error")
          showNotification(paste("Error:", e$message), type = "error", duration = 8)
          return(NULL)
        })
        
        req(res)

        # --- 更新缓存 ---
        res_onco_plot(res$onco_plot)
        res_box_plot(res$box_plot)

        setProgress(1, message = "Completed")
      })
    })

    # 6. 图形展示与下载
    # === Oncoprint ===
    dims_onco <- plotDownloadServer(
      id            = "plot_download_onco",
      plot_reactive = res_onco_plot,
      filename_prefix = "find_mutations_Oncoprint"
    )

    output$oncoprint_container <- renderUI({
      req(res_onco_plot())
      div(style = "overflow: auto;",
          plotOutput(session$ns("oncoprint_view"), 
                    width = paste0(dims_onco$width(), "px"), 
                    height = paste0(dims_onco$height(), "px"))
      )
    })
    
    output$oncoprint_view <- renderPlot({
      req(res_onco_plot())
      ComplexHeatmap::draw(res_onco_plot())
    })

    # === Box Plot ===
    dims_box <- plotDownloadServer(
      id            = "plot_download_box",
      plot_reactive = res_box_plot,
      filename_prefix = "find_mutations_Boxplot"
    )

    output$boxplot_container <- renderUI({
      req(res_box_plot())
      div(style = "overflow: auto;",
          plotOutput(session$ns("boxplot_view"), 
                     width = paste0(dims_box$width(), "px"), 
                     height = paste0(dims_box$height(), "px"))
      )
    })

    output$boxplot_view <- renderPlot({
      req(res_box_plot())
      res_box_plot()
    })

    # 7. ZIP 下载
    output$results_download <- downloadHandler(
      filename = function() {
        name <- input$save_path
        if(is.null(name) || name == "") name <- "Mutation_Results"
        paste0(name, ".zip")
      },
      content = function(file) {
        target_folder <- res_folder()
        full_path     <- target_folder
        
        if (is.null(target_folder) || !dir.exists(full_path)) {
          showNotification("Please run analysis first.", type = "error")
          return(NULL)
        }
        
        files_to_zip <- list.files(full_path, full.names = TRUE)
        
        if (length(files_to_zip) == 0) {
          showNotification("No results found.", type = "warning")
          return(NULL)
        }
        
        zip::zip(
          zipfile = file, 
          files   = files_to_zip, 
          mode    = "cherry-pick"
        )
      }
    )
  })
}