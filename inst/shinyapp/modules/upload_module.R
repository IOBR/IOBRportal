# =========================================================
# Module: File Upload
# =========================================================
# uploadUI <- function(id, label = "Upload File") {
#   ns <- NS(id)
#   fileInput(
#     inputId = ns("file"),
#     label = label,
#     multiple = FALSE,
#     accept = NULL,
#     width = "100%",
#     buttonLabel = "Browse",
#     placeholder = "Choose a file"
#   )
# }

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

    # =========================================================
    # Privacy: 将上传文件复制到当前会话的受保护临时目录
    # =========================================================
    private_file <- eventReactive(
      input$file,
      {
        req(input$file)

        iobr_copy_uploaded_file(
          upload_info = input$file,
          session = session,
          purpose = paste0("upload_", mode)
        )
      },
      ignoreNULL = TRUE
    )

    reactive({
      req(input$file)

      # 后续文件读取统一使用受保护路径
      file_path <- private_file()

      ext <- tolower(tools::file_ext(input$file$name))

      withProgress(message = "Reading uploaded file...", value = 0, {
        setProgress(0.5, message = "Reading file...")

        data <- tryCatch({
          df <- NULL

          # =========================================================
          # 1. 先完整读取文件（使用极速方案）
          # =========================================================
          if (ext %in% c("csv", "tsv", "txt", "tab")) {

            # 使用 data.table::fread 实现多线程极速读取
            # data.table = FALSE 确保返回标准 data.frame
            df <- data.table::fread(
              file_path,
              header = TRUE,
              check.names = FALSE,
              data.table = FALSE,
              nThread = 4
            )

          } else if (ext == "xlsx") {

            # Excel 在 Linux 上的“披马甲”读取法
            if (!requireNamespace("readxl", quietly = TRUE)) {
              stop("readxl package missing")
            }

            # 临时 Excel 文件同样放入当前会话的受保护目录
            temp_xlsx <- iobr_privacy_tempfile(
              session = session,
              purpose = "xlsx_read",
              fileext = "xlsx"
            )

            copied <- file.copy(
              from = file_path,
              to = temp_xlsx,
              overwrite = TRUE
            )

            if (!isTRUE(copied)) {
              stop("Failed to prepare the Excel file.")
            }

            # 即使读取 Excel 时发生错误，也会删除临时副本
            df <- tryCatch(
              readxl::read_excel(temp_xlsx),
              finally = {
                unlink(temp_xlsx, force = TRUE)
              }
            )

            df <- as.data.frame(
              df,
              stringsAsFactors = FALSE
            )

          } else {
            stop(
              paste(
                "Unsupported file format.",
                "Please upload .csv, .tsv, .txt, or .xlsx"
              )
            )
          }

          req(ncol(df) >= 1)

          # =========================================================
          # 2. 判断第一列是否需要转为行名
          # =========================================================
          first_col_name <- colnames(df)[1]

          # fread 遇到空列名有时会默认命名为 "V1"
          is_blank_like_name <-
            is.na(first_col_name) ||
            first_col_name == "" ||
            first_col_name == "V1" ||
            grepl("^\\.\\.\\.[0-9]+$", first_col_name)

          is_eset_like <- FALSE

          if (ncol(df) >= 2) {
            first_col <- df[[1]]
            other_df <- df[, -1, drop = FALSE]

            # 直接检查列属性是否为 numeric 或 integer
            # 无需循环转换数据内容
            other_numeric_ratio <- mean(
              vapply(
                other_df,
                is.numeric,
                FUN.VALUE = logical(1)
              )
            )

            # 第一列是否像 feature ID
            first_unique <- !anyDuplicated(first_col)
            first_char <-
              is.character(first_col) ||
              is.factor(first_col)

            is_eset_like <-
              first_char &&
              first_unique &&
              (other_numeric_ratio > 0.8)
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
            if (
              is_blank_like_name ||
              tolower(first_col_name) %in% c(
                "rowname",
                "rownames",
                "gene",
                "symbol",
                "genesymbol",
                "probe",
                "probeid"
              )
            ) {
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

        setProgress(
          1,
          message = "File successfully loaded."
        )

        data
      })
    })
  })
}