# Title: UI for shiny nicheR
# Description: The UI of the app
# Lats Updated: 6/8/2026

dashboardPage(
  dashboardHeader(
    title = tags$span("nicheR", class = "text-app-title"),
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
    ),

    tags$div(
      style = "position: absolute; bottom: 50px; width: 100%; text-align: center;",
      img(src = "nicheR_lg.png",
          style = "max-width: 140px;")
    ),

    tags$div(
      style = "position: absolute; bottom: 15px; width: 100%; text-align: center;",
      tags$a(
        href   = "https://github.com/castanedaM/nicheR",
        target = "_blank",
        style  = "color: #aaa; font-size: 0.8em;",
        icon("github"), " GitHub"
      )
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
              tags$span("Data Inputs", class = "text-tab-title"),
              p(instructions$data_upload, class = "text-instruction"),
              fluidRow(
                column(width = 5,
                       fileInput(inputId = "raster_file",
                                 label = tagList(
                                   tags$span("Background Layers (Raster)", class = "text-widget-title"),
                                   tags$span(icon("circle-info"),
                                             title = "Environmental conditions of the study area in raster format.\nRequired if no CSV is provided. Accepted: .tif, .rds", class = "text-widget-title")),
                                 multiple = FALSE,
                                 accept = c("tif", "tiff", "rds")),

                       fileInput(inputId = "df_file",
                                 label = tagList(
                                   tags$span("Background Layers (CSV)", class = "text-widget-title"),
                                   tags$span(icon("circle-info"),
                                             title = "Same data as the raster but in tabular form.\nOptional if raster is provided. Accepted: .csv, .rds", class = "text-widget-title"))
                                 ,
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
                column(width = 5,
                       box(title = tagList(tags$span("Optional Bias Raster",
                                                     class = "text-tab-title"),
                                           tags$span(icon("circle-info"),
                                                     title = "Optional bias layers, intended to represent sampling bias.\nAccepted: .tif, .rds",  class = "text-widget-title")),
                           width = 9,
                           collapsible = TRUE,
                           collapsed = TRUE,
                           fileInput(inputId = "bias_raster_file",
                                     label = tags$span("Sampling Bias Layer/s (Raster)", class = "text-widget-title"),
                                     multiple = FALSE,
                                     accept = c("tif", "tiff", "rds"))
                       )
                ),

                column(width = 7,
                       verbatimTextOutput("bias_raster_print")
                )
              ),

              fluidRow(
                column(
                  width = 12,
                  class = "btn-spaced",
                  actionButton(inputId = "data_upload",
                               label = "Upload",
                               class = "btn-primary")
                )
              )
            ),
            tabPanel(
              title = tags$span("Settings", class = "text-tab-title"),
              value = "setting",
              uiOutput("variable_selectors_ui")
            )
          )
        )
      ),
      tabItem(
        tabName = "build_tab",
        fluidRow(
          column(width = 6,
                 box(
                   width = 12, collapsible = TRUE, collapsed = FALSE,
                   p(instructions$range_choice, class = "text-instruction"),
                   radioButtons("range_method_choice",
                                label = tags$span("Select Range Method:", class = "text-widget-title"),
                                choices = c("Manual" = "man",
                                            "From Data" = "df",
                                            "From Stats" = "stats"),
                                selected = character(0)),

                   uiOutput("range_method_ui")
                 ),

                 uiOutput("covariance_ui")
          ),
          column(width = 6,
                 tabBox(
                   title = "plot",
                   width = 12,
                   tabPanel(
                     "E-space",
                     radioButtons("plot_state",
                                  label = NULL, choices = c("Pairs" = "plot_pairs",
                                                            "2D" = "plot_2d")),
                     conditionalPanel("input.plot_state == 'plot_2d'",
                                      selectInput("plot_2d_vars", label = NULL,
                                                  choices = character(0)
                                      )
                     ),
                     plotOutput("build_espace_plot")
                   )
                 ),
                 verbatimTextOutput("ellipsoid_print")
          )
        )
      ),
      tabItem(
        tabName = "predict_tab",
        fluidRow(
          box(title = "Predict", width = 6,
              solidHeader = TRUE, status = "primary",
              "Predict function details"),
          tabBox(
            title = "plot",
            width = 6,
            tabPanel("E-space"),
            tabPanel("G-space"),
          )
        )
      ),
      tabItem(
        tabName = "generate_tab",
        fluidRow(
          box(title = "Generate Occurrence", width = 6,
              solidHeader = TRUE, status = "primary",
              "Generate occurrence function details"),
          tabBox(
            title = "plot",
            width = 6,
            tabPanel("E-space"),
            tabPanel("G-space"),
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
