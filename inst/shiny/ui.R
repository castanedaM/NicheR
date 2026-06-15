# Title: UI for shiny nicheR
# Description: The UI of the app
# Lats Updated: 6/10/2026

dashboardPage(
  dashboardHeader(
    title = tags$span("nicheR", class = "text-app-title"),
    titleWidth = 200
  ),

  dashboardSidebar(
    width = 200,
    sidebarMenu(
      id = "sidebarMenu",
      menuItem("1. Build Ellipsoid",
               tabName = "build_tab",
               icon = icon("gear")),
      menuItem("2. Prediction",
               tabName = "predict_tab",
               icon = icon("angles-right")),
      menuItem("3. Bias",
               tabName = "bias_tab",
               icon = icon("table")),
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
        tabName = "build_tab",
        fluidRow(
          column(width = 6,
                 tabBox(
                   id = "tabpanel-build",
                   width = 12,

                   tabPanel(
                     tags$span("Inputs", class = "text-tab-title"),
                     fluidRow(
                       column(width = 12,
                              radioButtons("data_input_type_choice",
                                           label = tags$span("Select Input Type:", class = "text-widget-title"),
                                           choices = c("Background Layers" = "bg_layers",
                                                       "Previous Session" = "prev_session",
                                                       "Virtual Mode" = "virtual_mode",
                                                       "Example Data" = "example_data"),
                                           selected = character(0), inline = TRUE),
                              uiOutput("data_input_type")
                       )
                     )
                   ),

                   tabPanel(
                     title = tags$span("Range", class = "text-tab-title"),
                     width = 12,
                     value = "range",

                     uiOutput("variable_selectors_ui"),

                     p(instructions$range_choice, class = "text-instruction"),
                     radioButtons("range_method_choice",
                                  label = tags$span("Select Range Method:", class = "text-widget-title"),
                                  choices = c("Manual" = "man",
                                              "From Data" = "df",
                                              "From Stats" = "stats"),
                                  selected = character(0)),


                     uiOutput("range_method_ui")
                   ),

                   tabPanel(
                     title = tags$span("Covariance", class = "text-tab-title"),
                     width = 12,
                     p(instructions$covariance, class = "text-instruction"),
                     fluidRow(
                       column(width = 1),
                       column(width = 10,
                              uiOutput("covariance_ui")
                       ),
                       column(width = 1),
                     )
                   )
                 )
          ),

          column(width = 6,
                 tabBox(
                   width = 12,
                   tabPanel(
                     "E-space",
                     uiOutput("plot_options_ui"),
                     plotOutput("build_espace_plot",
                                height = "500px")
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
            width = 6,
            tabPanel("E-space"),
            tabPanel("G-space"),
          )
        )
      ),

      tabItem(
        tabName = "bias_tab",
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
        tabBox(
          width = 6,
          tabPanel("E-space"),
          tabPanel("G-space"),
        )
      ),


      tabItem(
        tabName = "generate_tab",
        fluidRow(
          box(title = "Generate Occurrence", width = 6,
              solidHeader = TRUE, status = "primary",
              "Generate occurrence function details"),
          tabBox(
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
