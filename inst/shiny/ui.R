# Title: UI for shiny nicheR
# Description: The UI of the app
# Lats Updated: 6/30/2026

dashboardPage(
  dashboardHeader(
    title = tags$span("nicheR", class = "text-app-title"),
    titleWidth = 200,
    tags$li(
      class = "dropdown",
      downloadButton("save_session_btn",
                     label = "Save",
                     icon = icon("floppy-disk"), class = "btn-primary",
                     title = "Save current session as .rds file")
    )
  ),

  dashboardSidebar(
    width = 200,
    sidebarMenu(
      id = "sidebarMenu",
      menuItem("About",
               tabName = "about",
               icon = icon("circle-info")),
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
               icon = icon("eye-dropper"))
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
        title = "About",
        tabName = "about",

        fluidRow(
          column(width = 12,
                 tags$div(tags$span("About nicheR", class = "text-app-title"), br(),
                          p("An R package for ellipsoid-based ecological niche modeling.
                       Build niche models from environmental ranges, predict suitable
                       areas, account for sampling bias, and generate virtual
                       occurrences, all in a single reproducible workflow.",
                            class = "text-instruction")
                 ),
                 br()
          )
        ),

        fluidRow(
          column(width = 2),

          column(width = 4,
                 box(title = tagList(icon("gear"),
                                     tags$span("1. Build ellipsoid",
                                               class = "text-widget-title")),
                     width = 12,
                     collapsible = TRUE,
                     collapsed = TRUE,
                     solidHeader = TRUE,
                     status = "primary",

                     p("Define a species niche as an ellipsoid in environmental space
                  using background layers and user-defined variable ranges.",
                       class = "text-instruction"),
                     tags$ul(
                       tags$li("Set ranges manually, from occurrence data, or from background statistics", class = "text-instruction"),
                       tags$li("Adjust covariance to rotate the ellipsoid", class = "text-instruction"),
                       tags$li("Move the centroid without rebuilding", class = "text-instruction"),
                       tags$li("Save multiple named versions", class = "text-instruction")
                     ),
                     tags$a(href   = "https://castanedaM.github.io/nicheR/articles/build.html",
                            target = "_blank",
                            icon("book"), " Build vignette")
                 )
          ),

          column(width = 4,
                 box(title = tagList(icon("angles-right"),
                                     tags$span("2. Predict suitable area",
                                               class = "text-widget-title")),
                     width = 12,
                     collapsible = TRUE,
                     collapsed = TRUE,
                     solidHeader = TRUE,
                     status = "primary",

                     p("Project the ellipsoid onto raster layers or a data frame to
                  produce continuous or binary suitability surfaces.",
                       class = "text-instruction"),
                     tags$ul(
                       tags$li("Output: suitability, Mahalanobis distance, and truncated versions", class = "text-instruction"),
                       tags$li("Binarization uses the ellipsoid's own confidence level", class = "text-instruction"),
                       tags$li("E-space and G-space plots included", class = "text-instruction"),
                       tags$li("Batch predict across multiple saved versions", class = "text-instruction")
                     ),
                     tags$a(href   = "https://castanedaM.github.io/nicheR/articles/predict.html",
                            target = "_blank",
                            icon("book"), " Predict vignette")
                 )
          ),

          column(width = 2)
        ),
        fluidRow(
          column(width = 2),

          column(width = 4,
                 box(title = tagList(icon("table"),
                                     tags$span("3. Bias correction",
                                               class = "text-widget-title")),
                     width = 12,
                     collapsible = TRUE,
                     collapsed = TRUE,
                     solidHeader = TRUE,
                     status = "primary",

                     p("Upload a sampling bias raster to weight occurrence generation
                  toward areas with higher observed detection effort.",
                       class = "text-instruction"),
                     tags$ul(
                       tags$li("Optional but important for data-limited species", class = "text-instruction"),
                       tags$li("Accepts any raster matching the background extent", class = "text-instruction"),
                       tags$li("Higher cell values increase occurrence probability", class = "text-instruction"),
                       tags$li("Use road density, collector coverage, or any detection proxy", class = "text-instruction")
                     ),
                     tags$a(href   = "https://castanedaM.github.io/nicheR/articles/bias.html",
                            target = "_blank",
                            icon("book"), " Bias vignette")
                 )
          ),

          column(width = 4,
                 box(title = tagList(icon("eye-dropper"),
                                     tags$span("4. Generate occurrences",
                                               class = "text-widget-title")),
                     width = 12,
                     collapsible = TRUE,
                     collapsed = TRUE,
                     status = "primary",
                     solidHeader = TRUE,
                     p("Sample virtual presences and absences from the fitted niche,
                  optionally weighted by the bias layer.",
                       class = "text-instruction"),
                     tags$ul(
                       tags$li("Specify number of presences and background ratio", class = "text-instruction"),
                       tags$li("Output is a data frame of coordinates and environmental values", class = "text-instruction"),
                       tags$li("Designed for virtual species and simulation workflows", class = "text-instruction"),
                       tags$li("Supports rare-species SDM validation studies", class = "text-instruction")
                     ),
                     tags$a(href   = "https://castanedaM.github.io/nicheR/articles/generate.html",
                            target = "_blank",
                            icon("book"), " Generate vignette")
                 )
          ),
          column(width = 2),
          br()
        ),

        fluidRow(
          column(width = 12,
                 tags$a(href   = "https://castanedaM.github.io/nicheR/authors.html",
                        target = "_blank", class = "text-instruction",
                        icon("user-group"), "If you use nicheR in your research, please cite:"),
                 br(),
                 tags$code("Castaneda-Guzman M, Hughes C, Paansri P, Cobos M (2026).
                          nicheR: Ellipsoid-based ecological niche modeling. R package version 0.1.0. https://github.com/castanedaM/nicheR",
                           style="color: grey;"),
                 br(), br()

          )
        ),

        fluidRow(
          column(width = 12,
                 tags$div(style = "font-size: 12px; color: #aaa; padding: 10px 0; border-top: 0.5px solid #ddd;
                             display: flex; gap: 12px; flex-wrap: wrap;",
                          tags$span("nicheR v0.1.0"),
                          tags$span("·"),
                          tags$a(href   = "https://github.com/castanedaM/nicheR/blob/main/LICENSE",
                                 target = "_blank", style = "color: #aaa;", "MIT license"),
                          tags$span("·"),
                          tags$a(href   = "https://github.com/castanedaM/nicheR/issues",
                                 target = "_blank", style = "color: #aaa;", "Report an issue")
                 )
          )
        )
      ),

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
                              uiOutput("covariance_ui"),
                              uiOutput("centroid_mover_ui"),
                              uiOutput("ellipsoid_library")
                       )
                     )
                   )
                 )
          ),

          column(width = 7,
                 tabBox(
                   id = "plot_tabs",
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

                 uiOutput("ellipsoid_info"),

                 # Export button and settings sit below the tabBox, outside all panels
                 column(width = 12, class = "btn-spaced",
                        uiOutput("export_btn_ui"),
                        br()),
                 uiOutput("plot_settings_ui")

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
      )
    )
  )
)
