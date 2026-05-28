
library(shiny)
library(shinydashboard)

dashboardPage(

  dashboardHeader(
    title = "nicheR Shiny App",
    titleWidth = 200
  ),

  dashboardSidebar(
    width = 200,
    sidebarMenu(
      id = "sidebarMenu",

      menuItem("Data Inputs",
               tabName = "data_tab",
               icon = icon("table")),
      menuItem("Build Ellipsoid",
               tabName = "build_tab",
               icon = icon("gear")),
      menuItem("Prediction",
               tabName = "predict_tab",
               icon = icon("angles-right")),
      menuItem("Generate Occurrences",
               tabName = "generate_tab",
               icon = icon("eye-dropper")),
      menuItem("About",
               tabName = "about",
               icon = icon("circle-info"))

    )
  ),

  dashboardBody(
    tabItems(
      tabItem(
        tabName = "data_tab",
        fluidRow(
          tabBox(
            id = "tabset1",
            width = 12,
            tabPanel(
              "Data Inputs",
              fluidRow(
                column(width = 5,
                       fileInput(inputId = "raster_file",
                                 label = "Choose raster layers",
                                 multiple = FALSE,
                                 accept = c("tif")),

                       fileInput(inputId = "df_file",
                                 label = "Choose CSV File",
                                 multiple = FALSE,
                                 accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv"))
                ),
                column(width = 7,
                       verbatimTextOutput("raster_print"),
                       tableOutput("df_header")
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
          tabBox(
            width = 5,

            tabPanel(
              "Range Manual",
              uiOutput("range_manual_ui")
            ),
            tabPanel(
              "Range from Data"
            ),
            tabPanel(
              "Range from Stats"
            )
          ),

          tabBox(
            title = "plot",
            width = 7,
            tabPanel(
              "E-space",
              plotOutput("build_espace_plot")
            ),
            tabPanel(
              "G-space"
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
            tabPanel(
              "E-space"
            ),
            tabPanel(
              "G-space"
            ),
            tabPanel(
              "Plot Settings"
            )
          )
        )
      ),
      tabItem(
        tabName = "generate_tab",
        fluidRow(
          box(title = "Generate Occurence", width = 5,
              solidHeader = TRUE, status = "primary",
              "Generate occurrence function details"),

          tabBox(
            title = "plot",
            width = 7,
            tabPanel(
              "E-space"
            ),
            tabPanel(
              "G-space"
            ),
            tabPanel(
              "Plot Settings"
            )
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
