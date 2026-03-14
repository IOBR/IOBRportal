# =========================================================
# Module: Plot Download & Resize
# =========================================================
plotDownloadUI <- function(id) {
  ns <- NS(id)
  
  # 使用一个带灰色背景的 div 包裹
  div(
    style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px; border: 1px solid #dee2e6;",
    
    fluidRow(
      # 1. 宽度输入
      column(2, numericInput(ns("screen_w"), "Width (px)", value = 600, min = 200, step = 50)),
      # 2. 高度输入
      column(2, numericInput(ns("screen_h"), "Height (px)", value = 400, min = 200, step = 50)),
      # 3. 下载按钮 (利用 margin-top 让按钮和输入框底部对齐)
      column(3, style = "margin-top: 32px;", downloadButton(ns("download"), "Download Plot", class = "btn-primary btn-block") 
      )
    )
  )
}


plotDownloadServer <- function(id, plot_reactive, filename_prefix = "plot", plot_type = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # -------------------------------------------------------------
    # 1. 智能推断 Plot Type (名称已根据你的要求对齐)
    # -------------------------------------------------------------
    current_type <- plot_type
    
    if (is.null(current_type)) {
      fname <- tolower(filename_prefix)
      
      if (grepl("remove_batcheffect", fname)) {
        current_type <- "batch_effect"
      } else if (grepl("iobr_pca", fname)) {
        current_type <- "pca"
      } else if (grepl("gsea", fname)) {
        current_type <- "gsea"
      } else if (grepl("find_mutations_oncoprint", fname) || grepl("mutation_onco", fname)) { # 覆盖 mutation 的 oncoprint
        current_type <- "mut_onco"
      } else if (grepl("find_mutations_boxplot", fname)) { # 覆盖 mutation 的 boxplot
        current_type <- "mut_box"
      } else if (grepl("box", fname)) {
        current_type <- "box"
      } else if (grepl("percent", fname)) {
        current_type <- "percent_bar"
      } else if (grepl("cell", fname) && grepl("bar", fname)) {
        current_type <- "cell_bar"
      } else if (grepl("forest", fname) || grepl("forrest", fname)) {
        current_type <- "sig_forrest"
      } else if (grepl("roc_time", fname)) {
        current_type <- "time_roc"     
      } else if (grepl("surv_group", fname) && grepl("group", fname)) {
        current_type <- "surv_group"
      } else if (grepl("sig_surv", fname)) {
        current_type <- "surv"         
      } else if (grepl("matrix", fname) && grepl("cor", fname)) {
        current_type <- "get_cor_matrix" 
      } else if (grepl("cor", fname)) {
        current_type <- "get_cor" 
      } else if (grepl("heatmap", fname)) {
        current_type <- "heatmap"
      } else {
        current_type <- "default"
      }
    }

    # -------------------------------------------------------------
    # 2. 尺寸计算逻辑
    # -------------------------------------------------------------
    get_dims_logic <- function(p, type) {
      
      # [固定尺寸表]
      fixed_sizes <- list(
        "batch_effect"       = c(w = 750, h = 250), # remove_batcheffect
        "pca"                = c(w = 450, h = 400), # iobr_pca
        "mut_onco"           = c(w = 600, h = 400), # find_mutation oncoprint
        "mut_box"            = c(w = 750, h = 800), # find_mutation box
        "gsea"               = c(w = 800, h = 550), # sig_gsea

        "box"            = c(w = 500,  h = 400),
        "percent_bar"    = c(w = 500,  h = 400),
        "sig_forrest"    = c(w = 550,  h = 300),
        "surv"           = c(w = 1000, h = 500),
        "time_roc"       = c(w = 500,  h = 400),
        "surv_group"     = c(w = 500,  h = 450),
        "get_cor"        = c(w = 450,  h = 400),
        "default"        = c(w = 600,  h = 450)
      )
      
      if (type %in% names(fixed_sizes)) return(as.list(fixed_sizes[[type]]))
      
      # [动态计算逻辑]
      w <- 600; h <- 450 # 保底
      
      # === A. 热图 / 相关性矩阵 (区分对待) ===
      if (type %in% c("heatmap", "get_cor_matrix")) {
        try({
          # 1. 查找维度
          find_dim <- function(x) {
            r <- tryCatch(nrow(x), error=function(e) NULL)
            c <- tryCatch(ncol(x), error=function(e) NULL)
            if (is.numeric(r) && is.numeric(c) && r > 0) return(list(r=r, c=c))
            
            if (any(grepl("InputHeatmap", class(x)))) {
               ht_real <- tryCatch(tidyHeatmap::as_ComplexHeatmap(x), error=function(e) NULL)
               if (!is.null(ht_real)) return(find_dim(ht_real))
            }
            if (isS4(x) && "matrix_param" %in% slotNames(x)) {
               mp <- x@matrix_param
               if (!is.null(mp$n_row) && !is.null(mp$n_col)) return(list(r=mp$n_row, c=mp$n_col))
            }
            if (isS4(x) && "matrix" %in% slotNames(x) && !is.null(x@matrix)) {
              return(list(r=nrow(x@matrix), c=ncol(x@matrix)))
            }
            if (isS4(x) && "ht_list" %in% slotNames(x)) {
              for (obj in x@ht_list) {
                d <- find_dim(obj)
                if (!is.null(d)) return(d)
              }
            }
            return(NULL)
          }

          dims <- find_dim(p)
          
          if (!is.null(dims)) {
            n_rows <- dims$r
            n_cols <- dims$c
          } else {
            # 备用
            if (inherits(p, "ggplot") && !is.null(p$data)) {
              n_rows <- length(unique(p$data[[1]])) 
              n_cols <- length(unique(p$data[[2]]))
            } else {
              n_rows <- 10; n_cols <- 10 
            }
          }

          # --- 【像素计算：区分对待】 ---
          
          if (type == "get_cor_matrix") {
             # [情况1：相关性矩阵]
             # 单独设置：25px * 25px (正方形大格子)
             row_px <- 15
             col_px <- 15
             
          } else {
             # [情况2：普通热图]
             row_px <- 10 
             col_px <- 15 
          }

          # 2. 计算总尺寸
          h <- 200 + (n_rows * row_px) 
          w <- 200 + (n_cols * col_px)
          
          # 3. 限制范围
          h <- max(300, min(h, 1500)) 
          w <- max(200, min(w, 1500))
          
        }, silent = TRUE)
        
      # === B. Cell Bar Plot ===
      } else if (type == "cell_bar") {
        try({
          if (inherits(p, "ggplot")) {
            n_bars <- 10
            if(!is.null(p$data)) {
               fac_cols <- sapply(p$data, function(x) !is.numeric(x))
               if(any(fac_cols)) n_bars <- length(unique(p$data[[which(fac_cols)[1]]]))
            }
            
            is_flipped <- inherits(p$coordinates, "CoordFlip")
            
            if(is_flipped) {
              # 翻转后：高度随条目增加，宽度固定 400
              h <- max(200, 100 + (n_bars * 15)) 
              w <- 700
            } else {
              # 未翻转：宽度随条目增加，高度固定 400
              w <- max(200, 150 + (n_bars * 15))
              h <- 700
            }
            
            # 封顶 1500
            w <- min(w, 1500); h <- min(h, 1500)
          }
        }, silent = TRUE)
      }
      
      return(list(w = round(w), h = round(h)))
    }

    # -------------------------------------------------------------
    # 3. 监听更新
    # -------------------------------------------------------------
    observeEvent(plot_reactive(), {
      req(plot_reactive())
      p <- plot_reactive()
      
      dims <- get_dims_logic(p, current_type)
      
      updateNumericInput(session, "screen_w", value = dims$w)
      updateNumericInput(session, "screen_h", value = dims$h)
    })

    # -------------------------------------------------------------
    # 4. 下载逻辑
    # -------------------------------------------------------------
    output$download <- downloadHandler(
      filename = function() { paste0(filename_prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf") },
      content = function(file) {
        req(plot_reactive())
        p <- plot_reactive()
        
        w_in <- (if(is.numeric(input$screen_w)) input$screen_w else 600) / 72
        h_in <- (if(is.numeric(input$screen_h)) input$screen_h else 400) / 72
        
        if (inherits(p, "ggplot")) {
          ggsave(file, plot = p, device = "pdf", width = w_in, height = h_in)
        } else if (inherits(p, "Heatmap") || inherits(p, "HeatmapList")) { 
          pdf(file, width = w_in, height = h_in); tryCatch({ draw(p) }, error = function(e) { print(p) }); dev.off()
        } else {
          pdf(file, width = w_in, height = h_in); print(p); dev.off()
        }
      }
    )
    
    return(list(
      width = reactive({ if(is.numeric(input$screen_w)) input$screen_w else 600 }),
      height = reactive({ if(is.numeric(input$screen_h)) input$screen_h else 400 })
    ))
  })
}