# =========================================================
# Module: dataTable
# =========================================================
dataTableUI <- function(id, title = NULL) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      /* 不换行样式：行名、列名、单元格都保持单行 */
      table.dataTable thead th,
      table.dataTable thead td,
      table.dataTable tbody td {
        white-space: nowrap !important;
      }
    ")),
    
    if (!is.null(title)) h5(strong(title)),
    DTOutput(ns("table"))
  )
}


dataTableServer <- function(id, data, max_cols = reactive(100), max_rows = reactive(50)) {
  moduleServer(id, function(input, output, session) {
    
    output$table <- renderDT({
      req(data())
      dat <- data()
      req(is.data.frame(dat) || is.matrix(dat))

      # 1. 维度获取
      total_rows <- nrow(dat)
      total_cols <- ncol(dat)
      mc <- max_cols()
      # mr <- max_rows() # 不需要硬性限制最大行数了
      
      is_col_truncated <- FALSE
      
      # 列截断逻辑
      if (!is.null(mc) && total_cols > mc) {
        dat <- dat[, 1:mc, drop = FALSE]
        is_col_truncated <- TRUE
      }
      
      # 删除/注释掉行截断逻辑！
      # 只有把完整数据给DT，点击表头排序时才能排整个数据集
      # if (!is.null(mr) && total_rows > mr) {
      #   dat <- dat[1:mr, , drop = FALSE]
      # }

      # Caption 逻辑微调，只提示列被截断
      caption_text <- if (is_col_truncated) {
        paste0("Note: Display limited to the first ", mc, " columns. ", 
               "(Full Dataset: ", total_rows, " rows x ", total_cols, " columns)")
      } else {
        paste0("Full Dataset: ", total_rows, " rows x ", total_cols, " columns")
      }

      # 2. 简单获取所有数字列
      dat_df <- as.data.frame(dat)
      all_numeric_cols <- names(dat_df)[sapply(dat_df, is.numeric)]

      # 3. 全能渲染器
      universal_renderer <- DT::JS("
        function(data, type, row) {
          if (type === 'display') {
            if (data === null || data === undefined) return '';
            var val = parseFloat(data);
            
            // 逻辑 A: 如果是整数 (比如 Status=0, Time=2342)，直接显示整数
            // (val % 1 === 0) 是判断整数的标准方法
            if (val % 1 === 0) return val.toFixed(0);
            
            // 逻辑 B: 如果是非常小的非0小数 (< 0.0001)，用科学计数法
            if (Math.abs(val) < 0.0001) return val.toExponential(2);
            
            // 逻辑 C: 其他正常小数，保留 4 位
            return val.toFixed(4);
          }
          return data;
        }
      ")

      # 4. 构建配置
      my_defs <- list(
        list(className = 'dt-center', targets = "_all")
      )
      
      if (length(all_numeric_cols) > 0) {
        my_defs[[length(my_defs) + 1]] <- list(
          targets = all_numeric_cols, 
          render = universal_renderer
        )
      }

      # 5. 生成表格
      DT::datatable(
        dat, # 这里传入的是没有切断行的完整数据
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: left; color: #555; font-style: italic; font-size: 90%;',
          caption_text
        ),
        options = list(
          scrollX = TRUE,
          pageLength = 10, # 默认每页显示10行
          lengthMenu = c(10, 25, 50, 100), # 允许用户改变每页显示数量
          deferRender = TRUE,
          autoWidth = FALSE,
          columnDefs = my_defs
        ),
        rownames = TRUE,
        escape = FALSE,
        class = "stripe hover compact"
      )
    }, server = TRUE) # 显式声明开启服务端模式
  })
}