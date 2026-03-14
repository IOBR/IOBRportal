# =========================================================
# Module: home_intro
# Description: Advanced Introduction / Hero page for IOBRportal
# =========================================================

home_introUI <- function(id) {
  ns <- NS(id)

  bs4TabItem(
    tabName = "home",

    # ---------------------------
    # CSS (scoped for home intro)
    # ---------------------------
    tags$style(HTML("
      .iobr-hero-card {
        background: linear-gradient(135deg, #ffffff 0%, #f8fbff 100%);
        border-radius: 10px;
        padding: 18px 20px;
        margin-bottom: 15px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.06);
        border: 1px solid #eef2f7;
      }

      .iobr-hero-title {
        font-size: 2.0rem;
        font-weight: 800;
        color: #1f2937;
        margin: 0 0 10px 0;
      }

      /* 左侧描述文字：稍微放大，但不夸张 */
      .iobr-hero-subtitle {
        font-size: 1.08rem;
        color: #4b5563;
        line-height: 1.65;
        margin-bottom: 10px;
      }

      .iobr-callout {
        margin-top: 10px;
        background: #f8fbff;
        border-left: 4px solid #1f78ff;
        border-radius: 6px;
        padding: 10px 12px;
        color: #4b5563;
        font-size: 0.95rem;
        line-height: 1.5;
      }

      /* 右侧能力卡：恢复自然高度，避免压迫 */
      .iobr-mini-card {
        background: white;
        border-radius: 8px;
        border: 1px solid #edf2f7;
        box-shadow: 0 1px 3px rgba(0,0,0,0.04);
        padding: 14px 14px 10px 14px;
        min-height: 122px;
        margin-bottom: 12px;
      }

      .iobr-mini-card .icon-wrap {
        width: 34px;
        height: 34px;
        border-radius: 8px;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        margin-bottom: 8px;
        font-size: 16px;
      }

      .iobr-mini-card .mini-title {
        font-weight: 700;
        color: #1f2937;
        font-size: 1.02rem;
        margin-bottom: 6px;
      }

      .iobr-mini-card .mini-text {
        color: #5b6472;
        font-size: 0.9rem;
        line-height: 1.45;
      }

      .iobr-figure-card {
        background: white;
        border-radius: 10px;
        padding: 14px;
        margin-bottom: 15px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.05);
        border: 1px solid #eef2f7;
      }

      .iobr-figure-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        margin-bottom: 10px;
        gap: 12px;
      }

      .iobr-figure-title {
        font-size: 1.25rem;
        font-weight: 700;
        color: #1f2937;
        margin: 0;
      }

      .iobr-figure-badge {
        background: #eef5ff;
        color: #003388;
        border: 1px solid #d9e8ff;
        border-radius: 999px;
        padding: 4px 10px;
        font-size: 0.82rem;
        font-weight: 600;
        white-space: nowrap;
      }

      .iobr-figure-caption {
        margin-top: 8px;
        color: #6b7280;
        font-size: 0.94rem;
        font-style: italic;
        line-height: 1.5;
      }

      .iobr-resource-card {
        background: white;
        border-radius: 10px;
        padding: 14px 16px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.05);
        border: 1px solid #eef2f7;
        margin-bottom: 12px;
      }

      .iobr-resource-title {
        font-weight: 700;
        color: #1f2937;
        margin-bottom: 8px;
        font-size: 1rem;
      }

      .iobr-resource-item {
        margin: 6px 0;
        color: #4b5563;
        line-height: 1.5;
      }

      .iobr-resource-item a {
        color: #003388;
        font-weight: 700;
        text-decoration: none;
      }

      .iobr-resource-item a:hover {
        text-decoration: underline;
      }

      /* Hero 内 4 个 KPI：仅作用于这一行 */
      .iobr-home-kpi-row {
        margin-top: 12px;
      }

      .iobr-home-kpi-row .small-box .icon > i {
        top: 10px;
        font-size: 44px;
        opacity: 0.16;
      }
      .iobr-home-kpi-row .small-box {
        margin-bottom: 8px;
      }
      .iobr-home-kpi-row .small-box .inner {
        padding: 8px 10px;
      }
      .iobr-home-kpi-row .small-box h3 {
        font-size: 1.55rem !important;
        font-weight: 500 !important;
        line-height: 1.05;
        margin: 0 0 4px 0;
      }
      .iobr-home-kpi-row .small-box p {
        font-size: 0.82rem !important;
        margin: 0;
        line-height: 1.2;
      }
    ")),

    # =========================================================
    # 1) HERO INTRODUCTION (Capability-focused, no detailed data stats)
    # =========================================================
    div(
      class = "iobr-hero-card",
      fluidRow(

        column(
          width = 8,

          div(
            class = "iobr-hero-left-main",
            tags$div(class = "iobr-hero-title", "IOBRportal at a glance"),

            tags$p(
              class = "iobr-hero-subtitle",
              "Explore modular tools and pre-defined workflows for end-to-end TME and immuno-oncology analyses. IOBRportal provides an integrated interface for transcriptome preprocessing, signature scoring, TME deconvolution, statistical analysis, and visualization."
            ),

            tags$p(
              class = "iobr-hero-subtitle",
              "The portal provides both function-oriented tools and workflow-oriented analysis routes, allowing users to start from individual tasks or follow guided pipelines for end-to-end interpretation in a unified and reproducible interface."
            ),

            tags$p(
              class = "iobr-hero-subtitle",
              "It is designed for both pre-integrated public cohorts and user-uploaded datasets. For dataset scale, cohort composition, and distribution statistics, please refer to Workflows → Datasets → Overview."
            )
          ),

          # ---- Hero 内嵌 Snapshot KPI（4个一行）----
          div(
            class = "iobr-home-kpi-row",
            fluidRow(
              bs4ValueBox(
                value = tags$span("64362", style = "font-size: 2rem; font-weight: 400; line-height: 1.1;"),
                subtitle = tags$span("Total Samples", style = "font-size: 0.9rem; font-weight: normal; opacity: 0.85;"),
                icon = icon("users"),
                color = "info",
                width = 3
              ),
              bs4ValueBox(
                value = tags$span("396", style = "font-size: 2rem; font-weight: 400; line-height: 1.1;"),
                subtitle = tags$span("Datasets", style = "font-size: 0.9rem; font-weight: normal; opacity: 0.85;"),
                icon = icon("table"),
                color = "primary",
                width = 3
              ),
              bs4ValueBox(
                value = tags$span("36", style = "font-size: 2rem; font-weight: 400; line-height: 1.1;"),
                subtitle = tags$span("Cancer Types", style = "font-size: 0.88rem; font-weight: normal; opacity: 0.85;"),
                icon = icon("dna"),
                color = "danger",
                width = 3
              ),
              bs4ValueBox(
                value = tags$span("4", style = "font-size: 2rem; font-weight: 400; line-height: 1.1;"),
                subtitle = tags$span("Data Sections", style = "font-size: 0.9rem; font-weight: normal; opacity: 0.85;"),
                icon = icon("layer-group"),
                color = "warning",
                width = 3
              )
            )
          )
        ),

        column(
          width = 4,

          div(
            class = "iobr-mini-card",
            tags$div(class = "icon-wrap", style = "background:#eef5ff; color:#1f78ff;", icon("project-diagram")),
            tags$div(class = "mini-title", "Workflow-driven analysis"),
            tags$div(class = "mini-text", "Move from data preparation to downstream statistics through guided pipelines.")
          ),
          div(
            class = "iobr-mini-card",
            tags$div(class = "icon-wrap", style = "background:#eefcf3; color:#16a34a;", icon("database")),
            tags$div(class = "mini-title", "Integrated cohort resources"),
            tags$div(class = "mini-text", "Analyze pre-integrated public cohorts or your own uploaded expression/clinical datasets.")
          ),
          div(
            class = "iobr-mini-card",
            tags$div(class = "icon-wrap", style = "background:#fff7ed; color:#ea580c;", icon("chart-line")),
            tags$div(class = "mini-title", "Output-ready visualizations"),
            tags$div(class = "mini-text", "Generate interpretable plots for signatures, TME patterns, correlations, and survival outcomes.")
          )
        )
      )
    ),

    # =========================================================
    # 2) OVERVIEW FIGURE (Main visual)
    # =========================================================
    div(
      class = "iobr-figure-card",
      div(
        class = "iobr-figure-header",
        tags$h4(class = "iobr-figure-title", "Framework Overview"),
        tags$span(class = "iobr-figure-badge", "Workflow Summary")
      ),
      tags$img(
        src = "IOBRportal overview.svg",
        style = "width: 100%; height: auto; display: block; margin: 0 auto; border-radius: 6px;"
      ),
      tags$div(
        class = "iobr-figure-caption",
        "Overview of IOBRportal architecture, including data foundation, analysis workflows, and downstream visualization outputs."
      )
    ),

    # =========================================================
    # 3) LEARNING RESOURCES (No jump buttons)
    # =========================================================
    fluidRow(
      column(
        width = 12,
        div(
          class = "iobr-resource-card",
          tags$div(class = "iobr-resource-title", "Learning Resources"),
          tags$div(
            class = "iobr-resource-item",
            HTML("📖 <b>Detailed Tutorial:</b> <a href='https://iobr.github.io/book/IOBR' target='_blank'>https://iobr.github.io/book/IOBR</a>")
          ),
          tags$div(
            class = "iobr-resource-item",
            HTML("💻 <b>Code Repository:</b> <a href='https://github.com/IOBR/IOBRshiny' target='_blank'>https://github.com/IOBR/IOBRshiny</a>")
          )
        )
      )
    )
  )
}


# parent_session 参数保留兼容，但当前静态页面不需要 server 逻辑
home_introServer <- function(id, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    # no server logic needed for static intro page
  })
}