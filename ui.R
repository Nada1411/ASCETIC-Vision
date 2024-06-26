source("libraries.R")

shinyUI(
  dashboardPage(
    dashboardHeader(
      title = "ASCETIC 2.0",
      tags$li(class = "dropdown", 
              style = "display: flex; justify-content: center; 
              align-items: center; color: #222D32; font-size: 15px;
              font-weight: bold; padding: 15px 10px 0 0;", 
              uiOutput("project_info"))      
    ),
    dashboardSidebar(
      tags$head(
        tags$style(HTML("
        .fa-i, .fa-s {
          transition: color 0.3s ease-in-out; 
        }
        .sidebar-menu li.active .fa-i,
        .sidebar-menu li.active .fa-s {
          color: #222D32 !important; 
        }
        .fa-i {
          color: #A8FA7F !important; 
        }
        .fa-s {
          color: #FAEE38 !important; 
        }
      "))
      ),
      sidebarMenu(
        id = "sidebarMenu",
        menuItem("Home page", tabName = "home", icon = icon("home")),
        menuItem(
          HTML("Input data <span style='float: right; transform: scale(0.6);'><i class='fa fa-i'></i></span>"), 
          tabName = "input", 
          icon = icon("database")
        ),
        menuItem(
          HTML("Inference <span style='float: right; transform: scale(0.6);'><i class='fa fa-i'></i></span>"), 
          tabName = "inference", 
          icon = icon("chart-line")
        ),
        menuItem(
          HTML("Confidence estimation <span style='float: right; transform: scale(0.55);'><i class='fa fa-i'></i></span>"), 
          tabName = "confidence_estimation", 
          icon = icon("think-peaks")
        ),
        menuItem(
          HTML("Input data <span style='float: right; transform: scale(0.55);'><i class='fa fa-s'></i></span>"), 
          tabName = "input_surv", 
          icon = icon("database")
        ),
        menuItem(
          HTML("Survival output <span style='float: right; transform: scale(0.55);'><i class='fa fa-s'></i></span>"), 
          tabName = "output_surv", 
          icon = icon("chart-line")
        ),
        menuItem("Save project", tabName = "save", icon = icon("save"))
      )
    ),
    dashboardBody(
      includeCSS("./style.css"),
      tabItems(
        tabItem(
          tabName = "home",
          fluidPage(
            class = "custom-fluid-homePage",
            DTOutput("projectList"),
            fluidRow(
              class = "custom-fluid-homeRow",
              column(3,
                     actionButton("loadProjBtn", "Load existing project", 
                                  class = "custom-button")
              ),
              column(3,offset = 1,
                     actionButton("create_project_button", "Create New Project", 
                                  class = "custom-button")
              )
            )
          )
        ),
        tabItem(
          tabName = "input",
          fluidPage(
            class = "custom-fluid-inputPage",
            fluidRow(
              column(6,
                     uiOutput("dataFile"),
                     actionButton("loadBtn", "Load", class = "custom-button", 
                                  style = "margin-bottom: 25px;")
              ),
              column(6,
                     uiOutput("dataFile2"),
                     uiOutput("loadBtn2"),
                     fluidRow(
                       column(width = 12, style = "margin-top: -7px; 
                              margin-bottom: 30px;", uiOutput("directoryInput"))
                     )
              )
            ),
            fluidRow(
              column(6,
                     uiOutput("DeleteColumn"),
              ),
              column(6,
                     uiOutput("binarization")
              ),
            ),
            fluidRow(
              column(6,
                     uiOutput("DeleteRow"),
              ),
              column(6,
                     uiOutput("binarization_perc")
              ),
            ),
            fluidRow(
              column(12,
                     conditionalPanel(
                       condition = "input.loadBtn > 0 || input.loadProjBtn > 0",
                       uiOutput("switchViewBtn")
                     ),
              style = "margin-top: 50px; margin-bottom: 20px;"  
              )
            ),
            fluidRow(
              column(12,
                     conditionalPanel(
                       condition = "input.switchViewBtn % 2 == 1",
                       uiOutput("heatmapPlot")
                     )
              )
            ),
            fluidRow(
              column(12,
                     conditionalPanel(
                       condition = "input.switchViewBtn % 2 == 0",
                       DTOutput("dataTable")
                     )
              )
            )
            ,
            fluidRow(
              column(12, uiOutput("content"))
            ),
            conditionalPanel(
              condition = "output.dataTable2",
              tags$div("Resampling", style = "font-weight: bold; 
                       margin-top: 85px; margin-left: 0px; font-size: 17px; 
                       margin-bottom: 20px;"),  
            ),
            DTOutput("dataTable2"),
            conditionalPanel(
              condition = "output.dataTable2",
              tags$div(style = "height: 30px;")
            ),
            conditionalPanel(
              condition = "output.dataTable2",
              tags$div(style = "height: 30px;")
            )
          ),
            style = "margin-bottom: 500px;",
        ),
        tabItem(
          tabName = "inference",
          fluidPage(
            style = "margin-left: 10px; margin-right: 10px;",
            fluidRow(
              column(6,
                     style = "margin-top: -10px; margin-bottom: 30px;",
                     selectInput("regularization", "Regularization", 
                                 c("aic", "bic", "loglik", "ebic", 
                                   "bde", "bds", "mbde", 
                                   "bdla", "k2", "fnml", "qnml", "nal", 
                                   "pnal"), 
                                 multiple = TRUE, selected = "aic"),
                     selectInput("command", "Command", c("hc","tabu")),
                     numericInput("restarts", "Restarts", 10, min = 0),
              ),
              column(6, 
                     tags$div(
                       numericInput("seed", "Seed", 12345, min = 0),
                       style = "margin-top: -10px;"
                     ),
                     tags$div(
                       checkboxInput("resamplingFlag", 
                                     HTML("<strong>Resampling</strong>")),
                       style = "margin-top: 45px;", width = "500px"
                     ),
                     uiOutput("nresampling", style = "margin-top: 25px;")
              ),
              column(12, align = "left",
                     actionButton("submitBtn", "Submit", class = "custom-button")
              ),
            ),
            fluidPage(
              style = "margin-left: -15px; ",
              fluidRow(
                column(6,
                       style = "margin-top: 50px;",
                       uiOutput("visualize_inference", class = "custom-width"),
                       conditionalPanel(
                         class = "no-border-bg",
                         condition = "output.graph_inference != null",
                         sliderInput("fontSize", "Font size", min = 5, 
                                     max = 40, value = 12)
                       ),
                       uiOutput("gene_graph_tab", style = "margin-top: 50px;"),
                ),
                column(6, 
                       conditionalPanel(
                         class = "no-border-bg",
                         condition = "output.visualize_inference != null",
                         downloadButton("downloadCSV", "Download Data as CSV", 
                                        class = "custom-width1"),
                       ),
                       conditionalPanel(
                         class = "no-border-bg",
                         condition = "output.graph_inference != null",
                         sliderInput("nodeSize", "Node size", min = 5,
                                     max = 30, value = 10)
                       ),
                ),
              ),
            ),
            tags$div(style = "height: 40px;"),
            conditionalPanel(
              condition = "output.graph_inference",
              tags$div("Inference output", style = "font-weight: bold; 
                       margin-top: 30px; font-size: 17px; margin-bottom: 20px; 
                       text-align: center;"),
            ),
            div(
              style = "display: flex; justify-content: center;",
              visNetworkOutput("graph_inference", width = "80%", height = "500px")
            ),
            style = "margin-top: 30px;",
            div(
              style = "display: flex; justify-content: center; margin-top: -480px;",
              DTOutput("selected_result_output")
            ),
            style = "margin-bottom: 500px;",
          )
        ),
        tabItem(
          tabName = "confidence_estimation",
          fluidPage(
            style = "margin-left: 10px; margin-right: 10px;",
            fluidRow(
              column(6,
                     style = "margin-top: -25px; margin-bottom: 30px;",
                     tags$div(
                       checkboxInput("resamplingFlag_conf", 
                                     HTML("<strong>Resampling</strong>")),
                       style = "margin-top: 45px;", width = "500px"
                     ),
                     uiOutput("nresampling_conf", style = "margin-top: 25px;")
              ),
              column(6, 
                     style = "margin-top: -10px; margin-bottom: 30px;",
                     numericInput("iteration_confEstimation", "Iteration", 1, min = 0),
                     ),
              column(12, align = "left",
                     actionButton("submitBtn_confEstimation", "Invia", class = "custom-button")
              ),
            ),
            fluidPage(
              style = "margin-left: -15px; ",
              fluidRow(
                column(6,
                       style = "margin-top: 50px;",
                       uiOutput("visualize_conf", class = "custom-width"),
                       conditionalPanel(
                         class = "no-border-bg",
                         condition = "output.graph_conf != null",
                         sliderInput("fontSize", "Font size", min = 5, 
                                     max = 40, value = 12)
                       ),
                       uiOutput("gene_graph_tab_conf", style = "margin-top: 50px;"),
                ),
                column(6, 
                       conditionalPanel(
                         class = "no-border-bg",
                         condition = "output.visualize_conf != null",
                         downloadButton("downloadCSV_conf", "Download Data as CSV", 
                                        class = "custom-width1"),
                       ),
                       conditionalPanel(
                         class = "no-border-bg",
                         condition = "output.graph_conf != null",
                         sliderInput("nodeSize", "Node size", min = 5,
                                     max = 30, value = 10)
                       ),
                ),
              ),
            ),
            tags$div(style = "height: 40px;"),
            conditionalPanel(
              condition = "output.graph_conf",
              tags$div("Confidence output", style = "font-weight: bold; 
                       margin-top: 30px; font-size: 17px; margin-bottom: 20px; 
                       text-align: center;"),
            ),
            div(
              style = "display: flex; justify-content: center;",
              visNetworkOutput("graph_conf", width = "80%", height = "500px")
            ),
            style = "margin-top: 30px;",
            div(
              style = "display: flex; justify-content: center; margin-top: -480px;",
              DTOutput("selected_result_output_conf")
            ),
            style = "margin-bottom: 500px;",
          )
        ),
        tabItem(
          tabName = "input_surv",
          fluidPage(
            class = "custom-fluid-inputPage",
            fluidRow(
              column(6,
                     selectInput("regularization_surv", "Regularization", choices = NULL),
                     uiOutput("dataFile2_surv")
              ),
              column(6,
                     tags$div(
                       tags$div(style="margin-top: 25px;",  
                                checkboxInput("load_file", 
                                              HTML("<strong>Use a different genotype file</strong>")),
                       ),
                       tags$div(style="margin-top: 30px;",  
                                uiOutput("dataFile_surv")
                       ),
                     )
              ),
              column(12, align = "left",
                     actionButton("submit_surv", "Evolutionary step", class = "custom-button", style = "margin-top: 30px;"),
                     actionButton("calc_surv", "Calculate survival", class = "custom-button", style = "margin-top: 30px;")
              ),
            ),
            div(style = "margin-top: 30px;",  
                fluidRow(
                  column(6, uiOutput("DeleteColumn_surv")),
                  column(6, uiOutput("binarization_surv"))
                )
            ),
            
            fluidRow(
              column(6, uiOutput("DeleteRow_surv")),
              column(6, uiOutput("binarization_percSurv"))
            ),
            fluidRow(
              column(12,
                     style = "margin-top: 30px;",
                     DTOutput("dataTable_GenotypeSurv")
              )
            ),
            fluidRow(
              column(12,
                    style = "margin-top: 30px;",
                    DTOutput("dataTable_surv")
              )
            ),
          ),
          style = "margin-bottom: 500px;",
        ),
        tabItem(
          tabName = "output_surv",
          fluidPage(
            fluidRow(
              girafeOutput("combined_graph", width = "100%", height = "700px")
            ),
            div(style = "margin-top: 20px;"),
            fluidRow(
                  plotlyOutput("survPlot", width = "100%", height = "400px"),
                  style = "width: 75%; margin: 0 auto;"
            )
          )
        ),
        tabItem(
          tabName = "save",
          fluidPage(
            style = "margin-left: 10px; margin-top: 20px;",
            textInput("project_name", "Project name", ""),
            actionButton("saveBtn", "Save", class = "custom-button")
          )
        )
      )
    )
  )
)
