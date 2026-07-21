# Title: UI for shiny nicheR
# Description: The UI of the app
# Lats Updated: 07/21/2026

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
                  toward areas with specific detection effort",
                       class = "text-instruction"),
                     tags$ul(
                       tags$li("Optional but important for data-limited species", class = "text-instruction"),
                       tags$li("Accepts any raster matching the background extent", class = "text-instruction"),
                       tags$li("Higher cell values increase occurrence probability", class = "text-instruction"),
                       tags$li("Exmples, urabinaztion, distance to water, road density, collector coverage, any detection proxy", class = "text-instruction")
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
                     p("Sample virtual presences from the fitted niche,
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
          column(width = 5),
          column(width = 2,
                 actionButton(inputId = "start_session",
                              icon = icon("play"),
                              label = "START", class = "btn-warning", width = "150px")
          ),
          column(width = 5)
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
                   id = "plot_build",
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
                 uiOutput("plot_settings_ui")

          )
        )
      ),
      tabItem(
        tabName = "predict_tab",
        fluidRow(
          column(width = 5,
                 box(width = 12,
                     title = tags$span("Predictions",
                                       class = "text-tab-title"),
                     uiOutput("pred_ell_select"),

                     fluidRow(
                       column(width = 8,
                              tags$span("Prediction Layers to Include",
                                        class = "text-widget-title"))
                     ),

                     fluidRow(
                       column(width = 6,
                              checkboxInput("pred_suitability",
                                            label = tags$span("Suitability",
                                                              class = "text-widget-inner"),
                                            value = TRUE)),
                       column(width = 6,
                              checkboxInput("pred_suitability_trunc",
                                            label = tags$span("Suitability (truncated)",
                                                              class = "text-widget-inner"),
                                            value = FALSE))
                     ),

                     fluidRow(
                       column(width = 6,
                              checkboxInput("pred_mahalanobis",
                                            label = tags$span("Mahalanobis",
                                                              class = "text-widget-inner"),
                                            value = TRUE)),
                       column(width = 6,
                              checkboxInput("pred_mahalanobis_trunc",
                                            label = tags$span("Mahalanobis (truncated)", class = "text-widget-inner"),
                                            value = FALSE))

                     ),

                     br(),

                     fluidRow(
                       box(title = tags$span("Advanced Prediciton Settings",
                                             class = "text-tab-title"),
                           width = 12,
                           collapsible = TRUE,
                           collapsed = TRUE,
                           fluidRow(
                             column(width = 8,
                                    tagList(tags$span("Truncation Level Adjustment",
                                                      class = "text-widget-title"),
                                            tags$span(icon("circle-info"),
                                                      title = "Adjust the level of truncation within the current elliposid, this will this will truncate prediction inwards",
                                                      class = "tooltip-icon"))),
                             column(width = 4,
                                    numericInput(inputId = "adjust_trunc",
                                                 label = NULL,
                                                 value = 0.95,
                                                 min = 0.0001, max = 0.99999, step = 0.05)
                             )
                           )
                       )
                     ),

                     fluidRow(
                       column(width = 12,
                              actionButton(inputId = "ell_predict",
                                           label = tags$span("Predict",
                                                             class = "text-widget-title"),
                                           class = "btn-default")
                       )
                     ),

                     br(),  br(),

                     fluidRow(
                       uiOutput("ellipsoid_library_pred")
                     )
                 )
          ),

          column(width = 7,

                 tabBox(
                   id    = "plot_pred",
                   width = 12,

                   tabPanel(
                     title = tags$span("E-space", class = "text-tab-title"),
                     value = "pred_espace",
                     uiOutput("pred_espace_options_ui"),
                     plotOutput("pred_espace_plot")
                   ),

                   tabPanel(
                     title = tags$span("G-space", class = "text-tab-title"),
                     value = "pred_gspace",
                     plotOutput("pred_gspace_plot")
                   ),

                   tabPanel(
                     title = tags$span("Combined", class = "text-tab-title"),
                     value = "pred_combined",
                     uiOutput("pred_combined_options_ui"),
                     plotOutput("pred_combined_plot")
                   )
                 ),

                 br(),

                 uiOutput("pred_plot_settings_ui")
          )
        )
      ),

      tabItem(
        tabName = "bias_tab",
        fluidRow(
          column(width = 5,
                 box(title = tagList(tags$span("3. Bias",
                                               class = "text-tab-title"),
                                     tags$span(icon("circle-info"),
                                               title = "Optional bias, intended to represent sampling bias.\nNeeds a raster file and accepts: .tif, .rds",
                                               class = "tooltip-icon")),
                     width = 12,

                     fluidRow(
                       column(width = 12,
                              actionButton(inputId = "skip_bias",
                                           label = "Skip bias",
                                           icon = icon("forward-step")),

                              actionButton(inputId = "continue_bias",
                                           label = "Continue with bias",
                                           icon = icon("arrow-right"))
                       )
                     ),

                     br(), br(),

                     uiOutput("upload_bias_ui"),
                     uiOutput("prepare_bias_ui"),
                     uiOutput("apply_bias_ui")

                 )
          ),

          column(width = 7,

                 tabPanel(
                   title = tags$span("E-space", class = "text-tab-title"),
                   value = "bias_espace",
                   uiOutput("bias_espace_options_ui"),
                   plotOutput("bias_espace_plot")
                 ),

                 tabPanel(
                   title = tags$span("G-space", class = "text-tab-title"),
                   value = "bias_gspace",
                   plotOutput("bias_gspace_plot")
                 ),

                 tabPanel(
                   title = tags$span("Combined", class = "text-tab-title"),
                   value = "bias_combined",
                   uiOutput("bias_combined_options_ui"),
                   plotOutput("bias_combined_plot")
                 ),

                 br(),

                 uiOutput("bias_plot_settings_ui")
          )
        )
      ),


      tabItem(
        tabName = "generate_tab",
        fluidRow(
          column(width = 5,

                 box(width = 12,
                     title = tags$span("Generate Occurences",
                                       class = "text-tab-title"),
                     uiOutput("generate_ell_select")
                 )
          ),
          column(width = 7,

                 tabBox(
                   id    = "plot_gen",
                   width = 12,

                   tabPanel(
                     title = tags$span("E-space", class = "text-tab-title"),
                     value = "gen_espace",
                     uiOutput("gen_espace_options_ui"),
                     plotOutput("gen_espace_plot")
                   ),

                   tabPanel(
                     title = tags$span("G-space", class = "text-tab-title"),
                     value = "gen_gspace",
                     plotOutput("gen_gspace_plot")
                   ),

                   tabPanel(
                     title = tags$span("Combined", class = "text-tab-title"),
                     value = "gen_combined",
                     uiOutput("gen_combined_options_ui"),
                     plotOutput("gen_combined_plot")
                   )
                 ),

                 br(),

                 uiOutput("gen_plot_settings_ui")
          )
        )
      )
    )
  )
)
