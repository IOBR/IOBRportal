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
    ),
    br(),
    tags$p(
      style = "color: #555; font-style: italic; font-size: 90%; margin-top: -10px; margin-bottom: 0px;",
      "You can manually adjust the width and height to resize the plot as needed."
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
      } else if (grepl("sig_roc", fname)) {
        current_type <- "sig_roc"
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
        "time_roc"       = c(w = 550,  h = 400),
        "surv_group"     = c(w = 500,  h = 450),
        "get_cor"        = c(w = 500,  h = 400),
        "sig_roc"        = c(w = 500,  h = 500),
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
            # if (inherits(p, "ggplot") && !is.null(p$data)) {
            #   n_rows <- length(unique(p$data[[1]])) 
            #   n_cols <- length(unique(p$data[[2]]))
            # } else {
            #   n_rows <- 10; n_cols <- 10 
            # }

            if (inherits(p, "ggplot") && !is.null(p$data)) {
              if (type == "get_cor_matrix") {
    
                # 关键：交换 row / col 计算逻辑
                n_cols <- length(unique(p$data[[1]]))  # x → 宽
                n_rows <- length(unique(p$data[[2]]))  # y → 高
    
              } else {
                n_rows <- length(unique(p$data[[1]])) 
                n_cols <- length(unique(p$data[[2]]))
              }
            } else {
              n_rows <- 10
              n_cols <- 10
            }
          }

          # --- 【像素计算：区分对待】 ---
          
          if (type == "get_cor_matrix") {

  row_px <- 35
  col_px <- 45

  x_labs <- character(0)
  y_labs <- character(0)

  if (inherits(p, "ggplot") && !is.null(p$data)) {
    x_labs <- as.character(unique(p$data[[1]]))
    y_labs <- as.character(unique(p$data[[2]]))
  }

  max_x_lab <- if (length(x_labs) > 0) max(nchar(x_labs), na.rm = TRUE) else 20
  max_y_lab <- if (length(y_labs) > 0) max(nchar(y_labs), na.rm = TRUE) else 20

  bottom_margin_px <- max(220, max_x_lab * 9)
  left_margin_px   <- max(220, max_y_lab * 9)

  h <- bottom_margin_px + 120 + (n_rows * row_px)
  w <- left_margin_px + 180 + (n_cols * col_px)

  h <- max(400, min(h, 3000))
  w <- max(600, min(w, 4000))

} else {

  row_px <- 10 
  col_px <- 15 

  h <- 200 + (n_rows * row_px) 
  w <- 200 + (n_cols * col_px)

  h <- max(400, min(h, 2000)) 
  w <- max(300, min(w, 2000))
}

          
          
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
              h <- max(300, 100 + (n_bars * 15)) 
              w <- 700
            } else {
              # 未翻转：宽度随条目增加，高度固定 400
              w <- max(300, 150 + (n_bars * 15))
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
      
      updateNumericInput(session, "screen_w", value = if (current_type == "get_cor_matrix") max(600, dims$w) else  dims$w)
      updateNumericInput(session, "screen_h", value = if (current_type == "get_cor_matrix") max(400, dims$h) else dims$h)
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
        
        # if (inherits(p, "ggplot")) {
        #   ggsave(file, plot = p, device = "pdf", width = w_in, height = h_in)
        # } 
        if (inherits(p, "ggplot")) {
          pdf(file, width = w_in, height = h_in, useDingbats = FALSE)
          print(p) # 将 ggplot 对象直接 print 到 pdf 设备中
          dev.off()
        } else if (inherits(p, "Heatmap") || inherits(p, "HeatmapList")) { 
          pdf(file, width = w_in, height = h_in); tryCatch({ draw(p) }, error = function(e) { print(p) }); dev.off()
        } else if (inherits(p, "recordedplot")) {
          pdf(file, width = w_in, height = h_in)
          on.exit(dev.off(), add = TRUE)
          replayPlot(p)
        } else if (current_type == "sig_roc") {
         pdf(file, width = w_in, height = h_in)
         on.exit(dev.off(), add = TRUE)

         p$fig.path <- NULL
         do.call(sig_roc, p)

        } else {
          pdf(file, width = w_in, height = h_in)
         on.exit(dev.off(), add = TRUE)
         print(p)
        }
      }
    )
    
    return(list(
      width = reactive({ if(is.numeric(input$screen_w)) input$screen_w else 600 }),
      height = reactive({ if(is.numeric(input$screen_h)) input$screen_h else 400 })
    ))
  })
}