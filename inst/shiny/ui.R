# Title: UI for shiny nicheR
# Description: The UI of the app
# Lats Updated: 6/23/2026

dashboardPage(
  dashboardHeader(
    title = tags$span("nicheR", class = "text-app-title"),
    titleWidth = 200,
    tags$li(
      class = "dropdown",
      downloadButton("save_session_btn",
                     label = "Save",
                     icon = icon("floppy-disk"),
                     title = "Save current session as .rds file")
    )
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
          column(width = 5,
                 tabBox(
                   id = "tabpanel-build",
                   width = 12,

                   tabPanel(
                     tags$span("Inputs", class = "text-tab-title"),
                     fluidRow(
                       column(width = 12,
                              p(instructions$data_input_type, class = "text-instruction"),
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
                     title = tags$span("Build", class = "text-tab-title"),
                     width = 12,
                     value = "range",
                     fluidRow(
                       column(width = 12,

                              uiOutput("variable_selectors_ui"),
                              uiOutput("range_method_choice_ui"),
                              uiOutput("range_method_ui"),
                              uiOutput("covariance_ui"),
                              uiOutput("centroid_mover_ui")
                       )
                     )
                   )
                 )
          ),

          column(width = 7,
                 tabBox(
                   id    = "plot_tabs",
                   width = 12,

                   tabPanel(
                     width = 12,
                     title = tags$span("E-space", class = "text-tab-title"),
                     value = "tab_espace",
                     uiOutput("plot_espace_options_ui"),
                     plotOutput("build_espace_plot")
                   ),

                   tabPanel(
                     width = 12,
                     title = tags$span("G-space", class = "text-tab-title"),
                     value = "tab_gspace",
                     plotOutput("build_gspace_plot")
                   ),

                   tabPanel(
                     width = 12,
                     title = tags$span("Combined", class = "text-tab-title"),
                     value = "tab_combined",
                     uiOutput("plot_combined_options_ui"),
                     plotOutput("build_combined_plot")
                   )
                 ),

                 # Export button and settings sit below the tabBox, outside all panels
                 column(width = 12, class = "btn-spaced",
                        uiOutput("export_btn_ui"),
                        br()),
                 uiOutput("plot_settings_ui")

                 # verbatimTextOutput("ellipsoid_print")
                 )
          )
      ),
      tabItem(
        tabName = "predict_tab",
        fluidRow(
          box(title = tags$span("Predict Suitable Area", class = "text-tab-title"),
              width = 5,

              uiOutput("ell_version_predict"),

              fluidRow(
                column(width = 8, tags$span("Prediction Layers to Include", class = "text-widget-title"))
              ),
              fluidRow(
                column(width = 6,
                       checkboxInput("pred_suitability",
                                     label = tags$span("Suitability", class = "text-widget-inner"),
                                     value = TRUE)),
                column(width = 6,
                       checkboxInput("pred_suitability_trunc",
                                     label = tags$span("Suitability (truncated)", class = "text-widget-inner"),
                                     value = FALSE))
              ),
              fluidRow(
                column(width = 6,
                       checkboxInput("pred_mahalanobis",
                                     label = tags$span("Mahalanobis", class = "text-widget-inner"),
                                     value = TRUE)),
                column(width = 6,
                       checkboxInput("pred_mahalanobis_trunc",
                                     label = tags$span("Mahalanobis (truncated)", class = "text-widget-inner"),
                                     value = FALSE))
              ),
              br(),
              fluidRow(
                uiOutput("advanced_settings_predict")
              ),

              actionButton(inputId = "ell_predict",
                           label = tags$span("Predict", class = "text-widget-title"),
                           class = "btn-default")
          ),
          tabBox(
            width = 7,
            tabPanel("E-space"),
            tabPanel("G-space"),
            tabPanel("Combined")
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
                                               title = "Optional bias layers, intended to represent sampling bias.\nAccepted: .tif, .rds",
                                               class = "tooltip-icon")),
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
          ),
          tabBox(
            width = 7,
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
