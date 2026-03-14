# =========================================================
# Module: find_markers_in_bulk
# =========================================================

# ---- Full UI ----
find_markers_in_bulkUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "find_markers_in_bulk",
    h3(HTML(
      "Find_markers_in_bulk
      <span style='font-size:80%; color:#333;'>:
      Identifies marker genes for each group using Seurat workflow on bulk data.</span>"
    )),
    find_markers_in_bulkBodyUI(id)
  )
}

# ---- BodyUI ----
find_markers_in_bulkBodyUI <- function(id, include_upload = TRUE) {
  ns <- NS(id)
  fluidRow(
    # --- 左侧参数区域 ---
    column(
      width = 3,
      
      # 1. 数据与计算参数
      bs4Card(
        title = "Parameter for data",
        status = "gray", headerBorder = TRUE, collapsible = TRUE, solidHeader = TRUE, width = 12, maximizable = TRUE,
        
        if (isTRUE(include_upload)) {
          tagList(
            # 1. Eset 上传组件
            uploadUI(ns("upload_eset")) %>% 
              helper(
                type    = "markdown",
                icon    = "question-circle",
                colour  = "#007bff",
                content = "demo_stad_tpm"
              ),
            
            # 2. Pdata 上传组件
            uploadUI(ns("upload_pdata")) %>% 
              helper(
                type    = "markdown",
                icon    = "question-circle",
                colour  = "#007bff",
                content = "demo_stad_pdata"
              )
          )
        },
        
        div(style = "text-align: center; margin: 10px 0;", actionButton(ns("run_analysis"), "Run Analysis", class = "btn-primary")),
        
        textInput(ns("pdata_id_col"), "Pdata ID", value = "ID", placeholder = "Column name (e.g., ID)"),
        
        pickerInput(
          inputId = ns("group_col"),
          label = "Group Column",
          choices = NULL, 
          multiple = FALSE,
          options = pickerOptions(liveSearch = TRUE, size = 10, style = "btn-outline-secondary", dropupAuto = FALSE, container = "body", title = "Select Group")
        ),
        
        numericInput(ns("top_n_markers"), "Top N Markers", value = 20, min = 1, max = 100, step = 1),

        selectInput(
          inputId = ns("group_color_style"),
          label = "Group Color",
          choices = c("npg" = "npg", "aaas" = "aaas", "lancet" = "lancet", "set1" = "set1", "set2" = "set2", "paired" = "paired"),
          selected = "npg"
        ),

        selectInput(
          inputId = ns("heatmap_body_color"),
          label = "Heatmap Color",
          choices = c(
            "Red-White-Blue" = "RdBu",
            "Yellow-Black-Purple" = "viridis",
            "Red-Black-Green" = "RdGn",
            "Spectra" = "Spectral"
          ),
          selected = "RdBu"
        )
      ),

      dataDownloadUI(ns("download_data"))
    ),
    
    # --- 右侧展示区域 ---
    column(
      width = 9,
      bs4Card(
        title = "Plot and Data",
        status = "gray", headerBorder = TRUE, collapsible = TRUE, solidHeader = TRUE, width = 12, maximizable = TRUE,
        tabBox(
          width = 12, side = "left",
          tabPanel(
            "Plot", 
            plotDownloadUI(ns("download_plot")),
            uiOutput(ns("heatmap_container"))
          ),
          tabPanel("Marker Table", dataTableUI(ns("marker_table_output")))
        )
      )
    )
  )
}


# ---- Server ----
find_markers_in_bulkServer <- function(id, external_eset = NULL, external_pdata = NULL) {
  moduleServer(id, function(input, output, session) {
    
    res_list <- reactiveVal(NULL)
    plot_trigger <- reactiveVal(0)

    raw_eset_source <- if (!is.null(external_eset)) {
      external_eset
    } else {
      uploadServer("upload_eset")
    }

    raw_pdata_source <- if (!is.null(external_pdata)) {
      external_pdata
    } else {
      uploadServer("upload_pdata")
    }

    eset_data <- reactive({
      req(raw_eset_source()) # 确保有数据
      eset <- raw_eset_source()

      if(!is.null(eset)) {
        colnames(eset) <- gsub("\\.", "-", colnames(eset))
        eset <- eset[, !duplicated(colnames(eset)), drop = FALSE]
      }
      return(eset)
    })
    
    pdata_data <- reactive({
      req(raw_pdata_source()) # 确保有数据
      pdata <- raw_pdata_source()
      
      if(!is.null(pdata)) {
        colnames(pdata) <- gsub("\\.", "-", colnames(pdata))
      }
      return(pdata)
    })
    
    # --- 3. 动态更新 Group ---
    observeEvent(pdata_data(), {
      req(pdata_data())
      data <- pdata_data()

      all_cols <- gsub("\\.", "-", colnames(data))
      is_num <- sapply(data, is.numeric)
      non_numeric_cols <- all_cols[!is_num]
      non_numeric_cols <- setdiff(non_numeric_cols, "ID") #只有字符，删去ID列

      pool_numeric <- all_cols[is_num] # 初始数字池
      blacklist_pattern <- "time|status|os|id"
      is_clinical <- grepl(blacklist_pattern, pool_numeric, ignore.case = TRUE)
      numeric_cols <- pool_numeric[!is_clinical]

      updatePickerInput(session, "group_col", choices = non_numeric_cols, selected = character(0))
    })
    
    # =======================================================
    # 4. 运行分析 (极简模式：只去重，不匹配)
    # =======================================================
    observeEvent(input$run_analysis, {
      req(eset_data(), pdata_data(), input$group_col, input$pdata_id_col)
      
      withProgress(message = "Identifying Markers...", value = 0, {
        
        # 1. 拿数据
        eset <- eset_data()
        pdata <- pdata_data()
        p_id <- input$pdata_id_col
        
        if (!p_id %in% colnames(pdata)) {
          showNotification("ID column not found in Pdata!", type = "error")
          return(NULL)
        }

        #Pdata 去重
        rownames(pdata) <- NULL 
        pdata <- pdata[!duplicated(pdata[[p_id]]), ]
        
        # 3. 算一下 NPCs (为了防止 Seurat 报错)
        common_len <- length(intersect(pdata[[p_id]], colnames(eset)))
        if(common_len == 0) {
           showNotification("No matched samples found!", type = "error")
           return(NULL)
        }
        safe_npcs <- min(30, common_len - 1)
        if(safe_npcs < 2) safe_npcs <- 2
        
        setProgress(0.4, message = "Running Seurat...")
        
        # 4. 直接丢进函数，别的不管
        res <- tryCatch({
          find_markers_in_bulk(
            pdata = pdata,
            eset = eset,
            group = input$group_col,
            id_pdata = p_id,
            top_n = input$top_n_markers,
            nfeatures = 2000, 
            thresh.use = 0.25, 
            only.pos = TRUE, 
            min.pct = 0.25, 
            npcs = safe_npcs
          )
        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error", duration = 8)
          return(NULL)
        })
        
        if (!is.null(res)) {
          res_list(res)
          plot_trigger(plot_trigger() + 1)
        }
        setProgress(1, message = "Done!")
      })
    })
    
    # =======================================================
    # 5. 绘图 (顺序修复版)
    # =======================================================
    heatmap_plot <- reactive({
      req(plot_trigger(), res_list(), input$group_color_style)
      
      res <- res_list()
      sce <- res$sce
      top_markers <- res$top_markers
      group_col <- input$group_col

      if (!group_col %in% colnames(sce@meta.data)) {
        validate(need(FALSE, paste0("Error: Column '", group_col, "' not found in Seurat object.")))
      }
      
      #剔除 NA/空值 样本
      group_vals <- sce@meta.data[[group_col]]
      valid_cells <- rownames(sce@meta.data)[!is.na(group_vals) & group_vals != ""]
      
      if (length(valid_cells) < ncol(sce)) {
        sce <- subset(sce, cells = valid_cells)
      }
      
      current_groups <- as.character(sce@meta.data[[group_col]])
      unique_groups <- sort(unique(current_groups)) 
      n_groups <- length(unique_groups)
      
      message("Final groups for plot: ", n_groups, " -> ", paste(unique_groups, collapse=", "))

      # 5. 配色逻辑
      pal_db <- list(
        "npg"    = c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F", "#8491B4", "#91D1C2", "#DC0000", "#7E6148"),
        "aaas"   = c("#3B4992", "#EE0000", "#008B45", "#631879", "#008280", "#BB0021", "#5F559B", "#A20056"),
        "lancet" = c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F", "#FDAF91", "#AD002A", "#ADB6B6"),
        "set1"   = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF"),
        "set2"   = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3"),
        "paired" = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A")
      )
      
      base_colors <- pal_db[[input$group_color_style]]
      
      # 如果组数超过色板颜色数，使用渐变插值；否则直接取前 n 个
      if (n_groups <= length(base_colors)) {
        cols <- base_colors[1:n_groups]
      } else {
        cols <- colorRampPalette(base_colors)(n_groups)
      }

      heatmap_col_func <- switch(input$heatmap_body_color,
        "RdBu" = rev(colorRampPalette(RColorBrewer::brewer.pal(11, "RdBu"))(256)),
        "RdGn" = rev(colorRampPalette(RColorBrewer::brewer.pal(11, "RdYlGn"))(256)),
        "Spectral" = rev(colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(256)),
        "viridis" = viridis::viridis(256)
      )
      
      # 6. Seurat 绘图
      p <- Seurat::DoHeatmap(
        object = sce,
        features = top_markers$gene,
        group.by = group_col,
        group.colors = cols, 
        size = 4,
        draw.lines = FALSE
      ) + 
      ggplot2::scale_fill_gradientn(colours = heatmap_col_func) + 
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8))
      
      return(p)
    })

    dims <- plotDownloadServer(
      id = "download_plot", 
      plot_reactive = heatmap_plot, 
      filename_prefix = "marker_heatmap"
    )

    output$heatmap_output <- renderPlot({
      heatmap_plot()
    })

    output$heatmap_container <- renderUI({
      req(heatmap_plot())
      
      div(style = "overflow: auto;",
          plotOutput(session$ns("heatmap_output"), 
                     width = paste0(dims$width(), "px"), 
                     height = paste0(dims$height(), "px"))
      )
    })
    
    # 数据
    markers_table <- reactive({ req(res_list()); res_list()$markers })
    dataTableServer("marker_table_output", markers_table)
    dataDownloadServer("download_data", data_reactive = markers_table, filename_prefix = "all_markers")
  })
}



# # =========================================================
# # Module: find_markers_in_bulk (Calculation Only)
# # =========================================================

# # ---- Full UI ----
# find_markers_in_bulkUI <- function(id) {
#   ns <- NS(id)
#   bs4TabItem(
#     tabName = "find_markers_in_bulk",
#     h3(HTML(
#       "Find_markers_in_bulk
#       <span style='font-size:80%; color:#333;'>:
#       Identifies marker genes for each group using Seurat workflow on bulk data.</span>"
#     )),
#     find_markers_in_bulkBodyUI(id)
#   )
# }

# # ---- BodyUI ----
# find_markers_in_bulkBodyUI <- function(id, include_upload = TRUE) {
#   ns <- NS(id)
#   fluidRow(
#     # --- 左侧参数区域 ---
#     column(
#       width = 3,
      
#       # 1. 数据与计算参数
#       bs4Card(
#         title = "Parameter for data",
#         status = "gray", headerBorder = TRUE, collapsible = TRUE, solidHeader = TRUE, width = 12, maximizable = TRUE,
        
#         if (isTRUE(include_upload)) {
#           tagList(
#             # 1. Eset 上传组件
#             uploadUI(ns("upload_eset")) %>% 
#               helper(
#                 type    = "markdown",
#                 icon    = "question-circle",
#                 colour  = "#007bff",
#                 content = "demo_stad_tpm"
#               ),
            
#             # 2. Pdata 上传组件
#             uploadUI(ns("upload_pdata")) %>% 
#               helper(
#                 type    = "markdown",
#                 icon    = "question-circle",
#                 colour  = "#007bff",
#                 content = "demo_stad_pdata"
#               )
#           )
#         },
        
#         div(style = "text-align: center; margin: 10px 0;", actionButton(ns("run_analysis"), "Run Analysis", class = "btn-primary")),
        
#         textInput(ns("pdata_id_col"), "Pdata ID", value = "ID", placeholder = "Column name (e.g., ID)"),
        
#         pickerInput(
#           inputId = ns("group_col"),
#           label = "Group Column",
#           choices = NULL, 
#           multiple = FALSE,
#           options = pickerOptions(liveSearch = TRUE, size = 10, style = "btn-outline-secondary", dropupAuto = FALSE, container = "body", title = "Select Group")
#         ),
        
#         numericInput(ns("top_n_markers"), "Top N Markers", value = 20, min = 1, max = 100, step = 1)
#       ),

#       dataDownloadUI(ns("download_data"))
#     ),
    
#     # --- 右侧展示区域 (仅保留表格) ---
#     column(
#       width = 9,
#       bs4Card(
#         title = "Data",
#         status = "gray", headerBorder = TRUE, collapsible = TRUE, solidHeader = TRUE, width = 12, maximizable = TRUE,
#         tabBox(
#           width = 12, side = "left",
#           tabPanel("Markers", dataTableUI(ns("marker_table_output")))
#         )
#       )
#     )
#   )
# }


# # ---- Server ----
# find_markers_in_bulkServer <- function(id, external_eset = NULL, external_pdata = NULL) {
#   moduleServer(id, function(input, output, session) {
    
#     res_list <- reactiveVal(NULL)
    
#     raw_eset_source <- if (!is.null(external_eset)) {
#       external_eset
#     } else {
#       uploadServer("upload_eset")
#     }

#     raw_pdata_source <- if (!is.null(external_pdata)) {
#       external_pdata
#     } else {
#       uploadServer("upload_pdata")
#     }

#     eset_data <- reactive({
#       req(raw_eset_source()) # 确保有数据
#       eset <- raw_eset_source()

#       if(!is.null(eset)) {
#         # 统一分隔符
#         colnames(eset) <- gsub("\\.", "-", colnames(eset))
#         # 【核心】只保留前12位 (匹配 TCGA-3M-AB46)
#         colnames(eset) <- substring(colnames(eset), 1, 12)
#         # 去重
#         eset <- eset[, !duplicated(colnames(eset)), drop = FALSE]
#       }
#       return(eset)
#     })
    
#     pdata_data <- reactive({
#       req(raw_pdata_source()) # 确保有数据
#       pdata <- raw_pdata_source()
      
#       if(!is.null(pdata)) {
#         colnames(pdata) <- gsub("\\.", "-", colnames(pdata))
#       }
#       return(pdata)
#     })
    
#     # --- 3. 动态更新 Group ---
#     observeEvent(pdata_data(), {
#       req(pdata_data())
#       data <- pdata_data()

#       all_cols <- gsub("\\.", "-", colnames(data))
#       is_num <- sapply(data, is.numeric)
#       non_numeric_cols <- all_cols[!is_num]
#       non_numeric_cols <- setdiff(non_numeric_cols, "ID") #只有字符，删去ID列

#       pool_numeric <- all_cols[is_num] # 初始数字池
#       blacklist_pattern <- "time|status|os|id"
#       is_clinical <- grepl(blacklist_pattern, pool_numeric, ignore.case = TRUE)
#       numeric_cols <- pool_numeric[!is_clinical]

#       updatePickerInput(session, "group_col", choices = non_numeric_cols, selected = character(0))
#     })
    
#     # =======================================================
#     # 4. 运行分析 (极简模式：只去重，不匹配)
#     # =======================================================
#     observeEvent(input$run_analysis, {
#       req(eset_data(), pdata_data(), input$group_col, input$pdata_id_col)
      
#       withProgress(message = "Identifying Markers...", value = 0, {
        
#         # 1. 拿数据
#         eset <- eset_data()
#         pdata <- pdata_data()
#         p_id <- input$pdata_id_col
        
#         if (!p_id %in% colnames(pdata)) {
#           showNotification("ID column not found in Pdata!", type = "error")
#           return(NULL)
#         }

#         #Pdata 去重
#         rownames(pdata) <- NULL 
#         pdata <- pdata[!duplicated(pdata[[p_id]]), ]
        
#         # 3. 算一下 NPCs (为了防止 Seurat 报错)
#         common_len <- length(intersect(pdata[[p_id]], colnames(eset)))
#         if(common_len == 0) {
#            showNotification("No matched samples found!", type = "error")
#            return(NULL)
#         }
#         safe_npcs <- min(30, common_len - 1)
#         if(safe_npcs < 2) safe_npcs <- 2
        
#         setProgress(0.4, message = "Running Seurat...")
        
#         # 4. 直接丢进函数，别的不管
#         res <- tryCatch({
#           find_markers_in_bulk(
#             pdata = pdata,
#             eset = eset,
#             group = input$group_col,
#             id_pdata = p_id,
#             top_n = input$top_n_markers,
#             nfeatures = 2000, 
#             thresh.use = 0.25, 
#             only.pos = TRUE, 
#             min.pct = 0.25, 
#             npcs = safe_npcs
#           )
#         }, error = function(e) {
#           showNotification(paste("Error:", e$message), type = "error", duration = 8)
#           return(NULL)
#         })
        
#         if (!is.null(res)) {
#           res_list(res)
#         }
#         setProgress(1, message = "Done!")
#       })
#     })
    
#     # 数据
#     markers_table <- reactive({ req(res_list()); res_list()$markers })
#     dataTableServer("marker_table_output", markers_table)
#     dataDownloadServer("download_data", data_reactive = markers_table, filename_prefix = "all_markers")

#     return(res_list)
#   })
# }