# Title:
# Description:
# Lats Updated: 6/2/2026

dashboardPage(
  dashboardHeader(
    title = "nicheR Shiny App",
    titleWidth = 200
    ),

  dashboardSidebar(
    width = 200,
    sidebarMenu(
      id = "sidebarMenu",
      menuItem("1. Data Inputs",
               tabName = "data_tab",
               icon = icon("table")),
      menuItem("2. Build Ellipsoid",
               tabName = "build_tab",
               icon = icon("gear")),
      menuItem("3. Prediction",
               tabName = "predict_tab",
               icon = icon("angles-right")),
      menuItem("4. Generate Occurrences",
               tabName = "generate_tab",
               icon = icon("eye-dropper")),
      menuItem("About",
               tabName = "about",
               icon = icon("circle-info"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    includeCSS("www/styles.css"),
    tabItems(
      tabItem(
        tabName = "data_tab",
        fluidRow(
          tabBox(
            id = "tabset1",
            width = 12,
            tabPanel(
              "Data Inputs",
              p(instructions$data_upload, class = "text-instruction"),
              fluidRow(
                column(width = 5,
                       fileInput(inputId = "raster_file",
                                 label = "Choose raster layers",
                                 multiple = FALSE,
                                 accept = c("tif", "tiff", "rds")),
                       fileInput(inputId = "df_file",
                                 label = "Choose CSV File",
                                 multiple = FALSE,
                                 accept = c("text/csv",
                                            "text/comma-separated-values",
                                            "text/plain",
                                            ".csv", "rds"))

                ),

                column(width = 7,
                       verbatimTextOutput("raster_print"),
                       tableOutput("df_header")
                )
              ),

              fluidRow(
                box(title = "Optional Bias Raster",
                    width = 12,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    column(width = 5,
                           fileInput(inputId = "bias_raster_file",
                                     label = "Choose bias raster layers",
                                     multiple = FALSE,
                                     accept = c("tif", "tiff", "rds"))

                    ),

                    column(width = 7,
                           verbatimTextOutput("bias_raster_print")
                    )
                )
              ),

              fluidRow(
                actionButton(inputId = "data_upload",
                             label = "Upload",
                             class = "btn-primary")
              )
            ),
            tabPanel(
              title = "Settings", value = "setting",
              uiOutput("variable_selectors_ui")
            )
          )
        )
      ),
      tabItem(
        tabName = "build_tab",
        fluidRow(
          box(
            width = 5,
            p(instructions$range_choice, class = "text-instruction"),
            radioButtons("range_method_choice",
                         label = "Select Range Method:",
                         choices = c("Manual" = "man",
                                     "From Data" = "df",
                                     "From Stats" = "stats")),

            uiOutput("range_method_ui")

          ),
          tabBox(
            title = "plot",
            width = 7,
            tabPanel(
              "E-space",
              plotOutput("build_espace_plot")
            ),
            tabPanel(
              "Plot Settings"
            )
          )
        )
      ),
      tabItem(
        tabName = "predict_tab",
        fluidRow(
          box(title = "Predict", width = 5,
              solidHeader = TRUE, status = "primary",
              "Predict function details"),
          tabBox(
            title = "plot",
            width = 7,
            tabPanel("E-space"),
            tabPanel("G-space"),
            tabPanel("Plot Settings")
          )
        )
      ),
      tabItem(
        tabName = "generate_tab",
        fluidRow(
          box(title = "Generate Occurrence", width = 5,
              solidHeader = TRUE, status = "primary",
              "Generate occurrence function details"),
          tabBox(
            title = "plot",
            width = 7,
            tabPanel("E-space"),
            tabPanel("G-space"),
            tabPanel("Plot Settings")
          )
        )
      ),
      tabItem(
        tabName = "about",
        "content of about"
      )
    )
  )
)
