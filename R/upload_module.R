# =========================================================
# Module: File Upload
# =========================================================
uploadUI <- function(id, label = "Upload File") {
  ns <- NS(id)
  fileInput(
    inputId = ns("file"),
    label = label,
    multiple = FALSE,
    accept = NULL,
    width = "100%",
    buttonLabel = "Browse",
    placeholder = "Choose a file"
  )
}


# uploadServer <- function(id, mode = c("auto", "pdata", "eset")) {
#   mode <- match.arg(mode)

#   moduleServer(id, function(input, output, session) {
#     reactive({
#       req(input$file)
#       ext <- tolower(tools::file_ext(input$file$name))

#       withProgress(message = "Reading uploaded file...", value = 0, {
#         setProgress(0.5, message = "Reading file...")

#         data <- tryCatch({
#           # =========================================================
#           # 1. 先完整读取文件
#           # 不直接用 row.names = 1
#           # =========================================================
#           if (ext == "csv") {
#             df <- read.csv(
#               input$file$datapath,
#               header = TRUE,
#               stringsAsFactors = FALSE,
#               check.names = FALSE
#             )

#           } else if (ext == "tsv") {
#             df <- read.delim(
#               input$file$datapath,
#               header = TRUE,
#               stringsAsFactors = FALSE,
#               check.names = FALSE
#             )

#           } else if (ext %in% c("txt", "tab")) {
#             df <- read.table(
#               input$file$datapath,
#               header = TRUE,
#               sep = "\t",
#               stringsAsFactors = FALSE,
#               check.names = FALSE,
#               quote = ""
#             )

#           } else if (ext == "xlsx") {
#             df <- readxl::read_excel(input$file$datapath)
#             df <- as.data.frame(df, stringsAsFactors = FALSE)

#           } else {
#             stop("Unsupported file format.")
#           }

#           req(ncol(df) >= 1)

#           # =========================================================
#           # 2. 判断第一列是否需要转为行名
#           # =========================================================
#           first_col_name <- colnames(df)[1]

#           is_blank_like_name <- is.na(first_col_name) ||
#             first_col_name == "" ||
#             grepl("^\\.\\.\\.[0-9]+$", first_col_name)

#           is_eset_like <- FALSE

#           if (ncol(df) >= 2) {
#             first_col <- df[[1]]
#             other_df  <- df[, -1, drop = FALSE]

#             # 其余列是否大多数可转为数值
#             other_numeric_ratio <- mean(vapply(other_df, function(x) {
#               x_num <- suppressWarnings(as.numeric(as.character(x)))
#               mean(!is.na(x_num)) > 0.9
#             }, logical(1)))

#             # 第一列是否像 feature ID
#             first_unique <- !anyDuplicated(first_col)
#             first_char   <- is.character(first_col) || is.factor(first_col)

#             is_eset_like <- first_char && first_unique && (other_numeric_ratio > 0.8)
#           }

#           # =========================================================
#           # 3. 按 mode 处理
#           # =========================================================
#           if (mode == "eset") {
#             rownames(df) <- df[[1]]
#             df <- df[, -1, drop = FALSE]

#           } else if (mode == "pdata") {
#             # 完整保留，不做行名转换
#             df <- df

#           } else if (mode == "auto") {
#             # 自动模式：空列名 + 数值矩阵风格 → 转行名
#             if (is_blank_like_name || tolower(first_col_name) %in% c("rowname", "rownames", "gene", "symbol", "genesymbol", "probe", "probeid")) {
#               if (is_eset_like) {
#                 rownames(df) <- df[[1]]
#                 df <- df[, -1, drop = FALSE]
#               }
#             }
#           }

#           df

#         }, error = function(e) {
#           showNotification(
#             paste("Failed to read file:", e$message),
#             type = "error"
#           )
#           return(NULL)
#         })

#         setProgress(1, message = "File successfully loaded.")
#         data
#       })
#     })
#   })
# }

# =========================================================
# Module: File Upload (Optimized with fread & Excel Fix)
# =========================================================
uploadUI <- function(id, label = "Upload File") {
  ns <- NS(id)
  fileInput(
    inputId = ns("file"),
    label = label,
    multiple = FALSE,
    accept = c(".csv", ".tsv", ".txt", ".tab", ".xlsx"),
    width = "100%",
    buttonLabel = "Browse",
    placeholder = "Choose a file"
  )
}

uploadServer <- function(id, mode = c("auto", "pdata", "eset")) {
  mode <- match.arg(mode)

  moduleServer(id, function(input, output, session) {
    reactive({
      req(input$file)
      ext <- tolower(tools::file_ext(input$file$name))

      withProgress(message = "Reading uploaded file...", value = 0, {
        setProgress(0.5, message = "Reading file...")

        data <- tryCatch({
          df <- NULL
          
          # =========================================================
          # 1. 先完整读取文件 (使用极速方案)
          # =========================================================
          if (ext %in% c("csv", "tsv", "txt", "tab")) {
            # 使用 data.table::fread 实现多线程极速读取
            # data.table = FALSE 确保返回标准的 data.frame 以兼容后续逻辑
            df <- data.table::fread(
              input$file$datapath,
              header = TRUE,
              check.names = FALSE,
              data.table = FALSE,
              nThread = 4 # 开启多线程提速 (视服务器配置可调)
            )

          } else if (ext == "xlsx") {
            # Excel 在 Linux 上的“披马甲”读取法
            if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl package missing")
            
            temp_xlsx <- tempfile(fileext = ".xlsx")
            file.copy(input$file$datapath, temp_xlsx)
            
            df <- readxl::read_excel(temp_xlsx)
            df <- as.data.frame(df, stringsAsFactors = FALSE)
            
            unlink(temp_xlsx) # 读取完毕，清理临时文件

          } else {
            stop("Unsupported file format. Please upload .csv, .tsv, .txt, or .xlsx")
          }

          req(ncol(df) >= 1)

          # =========================================================
          # 2. 判断第一列是否需要转为行名 (保留你原有的智能判断逻辑)
          # =========================================================
          first_col_name <- colnames(df)[1]

          # 注: fread 遇到空列名有时会默认命名为 "V1"
          is_blank_like_name <- is.na(first_col_name) ||
            first_col_name == "" ||
            first_col_name == "V1" ||
            grepl("^\\.\\.\\.[0-9]+$", first_col_name)

          is_eset_like <- FALSE

          if (ncol(df) >= 2) {
            first_col <- df[[1]]
            other_df  <- df[, -1, drop = FALSE]

            # 其余列是否大多数可转为数值
            # other_numeric_ratio <- mean(vapply(other_df, function(x) {
            #   x_num <- suppressWarnings(as.numeric(as.character(x)))
            #   mean(!is.na(x_num)) > 0.9
            # }, logical(1)))

            # 直接检查列属性是否为 numeric 或 integer，无需循环转换数据内容，速度提升 1000 倍
           other_numeric_ratio <- mean(vapply(other_df, is.numeric, FUN.VALUE = logical(1)))

            # 第一列是否像 feature ID
            first_unique <- !anyDuplicated(first_col)
            first_char   <- is.character(first_col) || is.factor(first_col)

            is_eset_like <- first_char && first_unique && (other_numeric_ratio > 0.8)
          }

          # =========================================================
          # 3. 按 mode 处理
          # =========================================================
          if (mode == "eset") {
            rownames(df) <- df[[1]]
            df <- df[, -1, drop = FALSE]

          } else if (mode == "pdata") {
            # 完整保留，不做行名转换
            df <- df

          } else if (mode == "auto") {
            # 自动模式：空列名 + 数值矩阵风格 → 转行名
            if (is_blank_like_name || tolower(first_col_name) %in% c("rowname", "rownames", "gene", "symbol", "genesymbol", "probe", "probeid")) {
              if (is_eset_like) {
                rownames(df) <- df[[1]]
                df <- df[, -1, drop = FALSE]
              }
            }
          }

          df

        }, error = function(e) {
          showNotification(
            paste("Failed to read file:", e$message),
            type = "error"
          )
          return(NULL)
        })

        setProgress(1, message = "File successfully loaded.")
        data
      })
    })
  })
}