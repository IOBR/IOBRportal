# =========================================================
# Module: sig_gsea
# =========================================================

# ---- Full UI ----
sig_gseaUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "sig_gsea",
    h3(HTML(
      "Sig_gsea
      <span style='font-size:80%; color:#333;'>:
        Performs GSEA analysis based on differential expression results.</span>"
      )),
    sig_gseaBodyUI(id)
  )
}

# ---- BodyUI ----
sig_gseaBodyUI <- function(id, include_upload = TRUE) {
  ns <- NS(id)
  fluidRow(
    # --- 左侧参数区域 ---
    column(
      width = 3,
      
      # 1. 数据与 DEG 参数卡片
      bs4Card(
        title = "Parameter for data",
        status = "gray",
        headerBorder = TRUE,
        collapsible = TRUE,
        solidHeader = TRUE,
        width = 12,
        maximizable = TRUE,
        
        if (isTRUE(include_upload)) {
          tagList(
            uploadUI(ns("iobr_deg_input1"))%>%
              helper(
                type    = "markdown",
                icon    = "question-circle",
                size    = "m",
                colour  = "#007bff",
                content = "demo_eset_stad"
              ),
            
            uploadUI(ns("iobr_deg_input2"))%>%
              helper(
                type    = "markdown",
                icon    = "question-circle",
                size    = "m",
                colour  = "#007bff",
                content = "demo_stad_pdata"
              )
          )
        },
        
        div(
          style = "text-align: center; margin-top: 0px; margin-bottom: 0px;",  
          actionButton(
            inputId = ns("run_iobr_deg_sig_gsea"), 
            label = "Run Analysis", 
            class = "btn-primary"
          )
        ),

        textInput(
          ns("iobr_deg_pdata_id"), 
          "ID (Pdata)", 
          value = "ID", 
          placeholder = "Column name (e.g., ID)"
        ),
        
        pickerInput(
          inputId = ns("iobr_deg_group_id"),
          label = "Group",
          choices = NULL, # Server 端填充
          multiple = FALSE,
          options = pickerOptions(
            liveSearch = TRUE,
            size = 10,
            style = "btn-outline-secondary",
            dropupAuto = FALSE,
            container = "body",
            title = "Select Group Column"
          )
        ),

        tags$label("Contrast (Case vs Control)"),
        fluidRow(
          column(
            width = 6,
            pickerInput(
              inputId = ns("contrast_case"),
              label = NULL, # 使用 placeholder
              choices = NULL,
              multiple = FALSE,
              options = pickerOptions(
                title = "Case (Exp)",
                style = "btn-outline-secondary",
                size = 5
              )
            )
          ),

          column(
            width = 6,
            pickerInput(
              inputId = ns("contrast_control"),
              label = NULL,
              choices = NULL,
              multiple = FALSE,
              options = pickerOptions(
                title = "Control (Ref)",
                style = "btn-outline-secondary",
                size = 5
              )
            )
          )
        ),
        
        selectInput(ns("iobr_deg_array"), "Array", choices = c("True" = "T", "False" = "F"), selected = "F"),
        selectInput(ns("iobr_deg_method"), "Method", choices = c("DESeq2" = "DESeq2", "Limma" = "limma"), selected = "DESeq2"),
        br(),
        tags$p(
          style = "color: #555; font-style: italic; font-size: 90%; margin-top: -10px;",
          "DESeq2 uses raw counts and Limma uses log2-transformed TPM values."
        ),

        sliderInput(ns("iobr_deg_padj_cutoff"), "Padj Cutoff", min = 0, max = 0.1, value = 0.01, step = 0.01),
        sliderInput(ns("iobr_deg_logfc_cutoff"), "Logfc Cutoff", min = 0, max = 1, value = 0.5, step = 0.01)
      ),

      bs4Card(
        title = "Parameter for plot",
        status = "gray",
        headerBorder = TRUE,
        collapsible = TRUE,
        solidHeader = TRUE,
        width = 12,
        maximizable = TRUE,
        
        div(
          style = "text-align: center; margin-top: 0px; margin-bottom: 0px;",  
          actionButton(
            inputId = ns("run_sig_gsea"), 
            label = "Run Analysis", 
            class = "btn-primary"
          )
        ),
        
        selectInput(
          inputId = ns("sig_gsea_palette_gsea"),
          label = "Palette",
          choices = c("1", "2", "3", "4"),
          selected = "2"
        ),

        textAreaInput(
          inputId = ns("sig_gsea_custom_cols"),
          label = "Colors",
          value = "",
          placeholder = "e.g., #E64B35, #4DBBD5, #00A087\nSeparate by comma",
          rows = 3,
          resize = "vertical"
        ),
        br(),
        tags$p(
          style = "color: #555; font-style: italic; font-size: 90%; margin-top: -10px;",
          "Input hex codes or color names separated by comma."
        ),

        selectInput(
          inputId = ns("calculate_sig_score_signature"),
          label = "Signature",
          choices = c(
            "TME" = "signature_tme", "Metabolism" = "signature_metabolism",
            "Tumor" = "signature_tumor", "Collection" = "signature_collection",
            "Go_bp" = "go_bp", "Go_cc" = "go_cc", "Go_mf" = "go_mf",
            "KEGG" = "kegg", "Hallmark" = "hallmark", "Reactome" = "reactome"
          ),
          selected = "signature_tme"
        ),
        
        numericInput(
          inputId = ns("sig_gsea_show_gsea"),
          label = "Signature Numbers", 
          value = 8,  
          min = 1,   
          max = 50,   
          step = 1    
        )
      ),
      dataDownloadUI(ns("download_data"))
    ),
    
    # --- 右侧展示区域 ---
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
            plotDownloadUI(ns("download_plot")),
            uiOutput(ns("sig_gsea_plot_container"))
          ),
        tabPanel(
            "Data", 
            dataTableUI(ns("iobr_deg_output"))
          )
        )
      )
    )
  )
}


# ---- Server ----
sig_gseaServer <- function(id, external_eset = NULL, external_pdata = NULL) {
  moduleServer(id, function(input, output, session) {
    
    iobr_deg_result <- reactiveVal(NULL)
    sig_gsea_result <- reactiveVal(NULL)
    
    custom_cols_vec <- reactive({
      raw_cols_text <- input$sig_gsea_custom_cols
      out <- NULL

      if (!is.null(raw_cols_text) && trimws(raw_cols_text) != "") {
        # 1. 按逗号、分号或换行符分割
        split_cols <- unlist(strsplit(raw_cols_text, "[,;\n]"))
        # 2. 去除首尾空格
        split_cols <- trimws(split_cols)
        # 3. 去除空字符串
        out <- split_cols[split_cols != ""]

        if (length(out) == 0) out <- NULL
      }

      out
    })
    # =======================================================
    # 1. 定义数据源 Reactive
    # =======================================================
    
    # # --- Eset 数据源 ---
    # eset_data <- reactive({
    #   if (!is.null(external_eset)) {
    #     return(external_eset())
    #   }
    #   req(input$iobr_deg_input1)
    #   eset_ext <- tools::file_ext(input$iobr_deg_input1$name)
    #   eset <- tryCatch({
    #     if (eset_ext == "csv") {
    #       read.csv(input$iobr_deg_input1$datapath, row.names = 1, check.names = FALSE)
    #     } else if (eset_ext == "tsv") {
    #       read.delim(input$iobr_deg_input1$datapath, row.names = 1, check.names = FALSE)
    #     } else {
    #       read.table(input$iobr_deg_input1$datapath, row.names = 1, sep = "\t", check.names = FALSE)
    #     }
    #   }, error = function(e) { return(NULL) })
      
    #   if(!is.null(eset)) colnames(eset) <- gsub("\\.", "-", colnames(eset))
    #   return(eset)
    # })

    # # --- Pdata 数据源 ---
    # pdata_data <- reactive({
    #   if (!is.null(external_pdata)) {
    #     return(external_pdata())
    #   }
    #   req(input$iobr_deg_input2)
    #   pdata_ext <- tools::file_ext(input$iobr_deg_input2$name)
    #   pdata <- tryCatch({
    #     if (pdata_ext == "csv") {
    #       read.csv(input$iobr_deg_input2$datapath, header = TRUE, stringsAsFactors = FALSE)
    #     } else if (pdata_ext == "tsv") {
    #       read.delim(input$iobr_deg_input2$datapath, header = TRUE, stringsAsFactors = FALSE)
    #     } else {
    #       read.table(input$iobr_deg_input2$datapath, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    #     }
    #   }, error = function(e) { return(NULL) })
      
    #   if(!is.null(pdata)) colnames(pdata) <- gsub("\\.", "-", colnames(pdata))
    #   return(pdata)
    # })

    eset_data <- if (!is.null(external_eset)) {
      external_eset
    } else {
      uploadServer("iobr_deg_input1")
    }

    pdata_data <- if (!is.null(external_pdata)) {
      external_pdata
    } else {
      uploadServer("iobr_deg_input2")
    }

    # =======================================================
    # 2. 自动更新 UI
    # =======================================================

    # --- Step 1: Pdata 加载后，更新 Group 选项 ---
    observeEvent(pdata_data(), {
      req(pdata_data())
      data <- pdata_data()
      
      all_cols <- gsub("\\.", "-", colnames(data))
      is_num <- sapply(data, is.numeric)
      non_numeric_cols <- all_cols[!is_num]
      non_numeric_cols <- setdiff(non_numeric_cols, "ID") #只有字符，删去ID列
      
      updatePickerInput(
        session = session,
        inputId = "iobr_deg_group_id",
        choices = non_numeric_cols,
        selected = character(0) # 默认为空，强制用户选择
      )
    })

    # --- Step 2: Group 选定后，更新 Contrast 选项 ---
    observeEvent(input$iobr_deg_group_id, {
      req(pdata_data(), input$iobr_deg_group_id)
      data <- pdata_data()
      group_col <- input$iobr_deg_group_id
      
      if (group_col %in% colnames(data)) {
        # 获取该列的所有唯一值（去除NA）
        unique_vals <- unique(as.character(data[[group_col]]))
        unique_vals <- unique_vals[!is.na(unique_vals)]
        
        # 默认选中第1个作为 Case
        case_default <- unique_vals[1]
        updatePickerInput(
          session = session, 
          inputId = "contrast_case", 
          choices = unique_vals,
          selected = case_default
        )
        
        # 默认选中第2个作为 Control (如果只有1个值，则Control也选第1个)
        ctrl_default <- if(length(unique_vals) > 1) unique_vals[2] else unique_vals[1]
        updatePickerInput(
          session = session, 
          inputId = "contrast_control", 
          choices = unique_vals, 
          selected = ctrl_default
        )
      }
    })

    # =======================================================
    # 3. 运行分析逻辑 (Run Analysis)
    # =======================================================
    observeEvent(input$run_iobr_deg_sig_gsea, {
      
      # 检查必要输入
      req(eset_data(), pdata_data(), input$iobr_deg_group_id, input$contrast_case, input$contrast_control)
      
      # 进度条开始
      withProgress(message = "Running iobr_deg + sig_gsea analysis...", value = 0, {
        
        setProgress(0.2, message = "Preprocessing data...")
        eset <- eset_data()
        pdata <- pdata_data()

        # --- 获取 Contrast ---
        case_grp <- input$contrast_case
        ctrl_grp <- input$contrast_control
        
        if (case_grp == ctrl_grp) {
           showNotification("Warning: Case and Control groups are the same.", type = "warning", duration = 5)
        }
        contrast_vec <- c(case_grp, ctrl_grp)
        
        # --- 数据匹配 ---
        p_id_col <- input$iobr_deg_pdata_id
        
        if (!p_id_col %in% colnames(pdata)) {
            showNotification(paste("Column", p_id_col, "not found in Pdata"), type = "error")
            return(NULL)
        }

        # 取交集
        common_ids <- intersect(pdata[[p_id_col]], colnames(eset))
        if(length(common_ids) == 0) {
           showNotification("No matched samples between Eset and Pdata!", type = "error")
           return(NULL)
        }
        
        pdata <- pdata[pdata[[p_id_col]] %in% common_ids, ]
        eset  <- eset[, common_ids, drop = FALSE]
        pdata <- pdata[match(colnames(eset), pdata[[p_id_col]]), ]
        rownames(pdata) <- pdata[[p_id_col]]

        setProgress(0.5, message = "Running Differential Expression (iobr_deg)...")
        
        # --- 运行 iobr_deg (加 tryCatch) ---
        deg_result <- tryCatch({
          iobr_deg(
            eset = eset,
            pdata = pdata,
            pdata_id = input$iobr_deg_pdata_id,
            group_id = input$iobr_deg_group_id,
            contrast = contrast_vec,
            array = (input$iobr_deg_array == "T"),
            method = input$iobr_deg_method,
            padj_cutoff = input$iobr_deg_padj_cutoff,
            logfc_cutoff = input$iobr_deg_logfc_cutoff,
            path = NULL
          )
        }, error = function(e) {
          setProgress(1, message = "Error occurred.")
          showNotification(paste("Error in iobr_deg:", e$message), type = "error", duration = 8)
          return(NULL)
        })
        
        if (is.null(deg_result)) return(NULL)
        
        iobr_deg_result(deg_result)
        
        setProgress(0.7, message = "Running GSEA (sig_gsea)...")
        
        # --- 运行 sig_gsea (加 tryCatch) ---
        pp <- tryCatch({
          sig_gsea(
            deg = deg_result,
            genesets = get(input$calculate_sig_score_signature),
            palette_gsea = as.numeric(input$sig_gsea_palette_gsea),
            show_gsea = input$sig_gsea_show_gsea,
            print_bar = FALSE,
            cols_gsea = custom_cols_vec(),
            show_plot = FALSE,
            path = NULL
          )
        }, error = function(e) {
          setProgress(1, message = "Error occurred.")
          showNotification(paste("Error in sig_gsea:", e$message), type = "error", duration = 8)
          return(NULL)
        })

        if (is.null(pp)) return(NULL)

        sig_gsea_result(pp)
        
        setProgress(1, message = "Analysis complete.")
      })
    })
    
    # =======================================================
    # 4. 仅更新 Plot (Update Plot Only)
    # =======================================================
    observeEvent(input$run_sig_gsea, {
      req(iobr_deg_result())
      
      withProgress(message = "Updating sig_gsea plot only...", value = 0.5, {
        
        pp <- tryCatch({
          sig_gsea(
            deg = iobr_deg_result(),
            genesets = get(input$calculate_sig_score_signature),
            palette_gsea = as.numeric(input$sig_gsea_palette_gsea),
            show_gsea = input$sig_gsea_show_gsea,
            print_bar = FALSE,
            cols_gsea = custom_cols_vec(),
            show_plot = FALSE,
            path = NULL
          )
        }, error = function(e) {
          setProgress(1, message = "Error occurred.")
          showNotification(paste("Error in sig_gsea:", e$message), type = "error", duration = 8)
          return(NULL)
        })
        
        if (!is.null(pp)) {
           sig_gsea_result(pp)
           setProgress(1, message = "Analysis complete.")
        }
      })
    })
    
    dataTableServer("iobr_deg_output", iobr_deg_result)

    output$sig_gsea_output <- renderPlot({
      req(sig_gsea_result())
      sig_gsea_result()$plot_top
    })
    
    # --- Downloads ---
    dataDownloadServer("download_data", data_reactive = iobr_deg_result, filename_prefix = "iobr_deg_result")
    
    plot_for_download <- reactive({
      req(sig_gsea_result())
      sig_gsea_result()$plot_top
    })

    dims <- plotDownloadServer(
      "download_plot", 
      plot_reactive = plot_for_download, 
      filename_prefix = "sig_gsea_plot"
    )

    output$sig_gsea_plot_container <- renderUI({
      req(sig_gsea_result()) # 确保有结果才渲染
  
      div(style = "overflow: auto;",
      plotOutput(session$ns("sig_gsea_output"),
                 width = paste0(dims$width(), "px"), 
                 height = paste0(dims$height(), "px"))
      )
    })
    
    return(plot_for_download)
  })
}