source("libraries.R")

shinyUI(
  dashboardPage(
    dashboardHeader(
      title = HTML("ASCETIC <i>plus</i>"),
      tags$li(class = "dropdown", 
              style = "display: flex; justify-content: center; 
          align-items: center; color: #222D32; font-size: 15px;
          font-weight: bold; padding: 15px 10px 0 0;", 
              uiOutput("project_info")
      ),
      tags$li(class = "dropdown",
              conditionalPanel(
                condition = "input.sidebarMenu != 'home'",
                actionButton("resetBtn", "Reset", class = "custom-button", 
                             style = "margin-right: 20px;")
              ),
              style = "display: flex; align-items: center; margin-left: 20px; margin-top: 10px;"
      )
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
        table.dataTable thead th {
          text-align: right !important;
      }
      "))
      ),
      sidebarMenu(
        id = "sidebarMenu",
        menuItem("Home page", tabName = "home", icon = icon("home")),
        menuItem(
          HTML("Input data (genomic) <span style='float: right; transform: scale(0.6);'><i class='fa fa-i'></i></span>"), 
          tabName = "input", 
          icon = icon("database")
        ),
        menuItem(
          HTML("Evolution model <br>inference<span style='float: right; transform: scale(0.6);'><i class='fa fa-i'></i></span>"), 
          tabName = "inference", 
          icon = icon("chart-line")
        ),
        menuItem(
          HTML("Confidence estimation <span style='float: right; transform: scale(0.55);'><i class='fa fa-i'></i></span>"), 
          tabName = "confidence_estimation", 
          icon = icon("think-peaks")
        ),
        menuItem(
          HTML("Input data (survival)<span style='float: right; transform: scale(0.55);'><i class='fa fa-s'></i></span>"), 
          tabName = "input_surv", 
          icon = icon("database")
        ),
        menuItem(
          HTML("Evolutionary signatures <span style='float: right; transform: scale(0.55);'><i class='fa fa-s'></i></span>"), 
          tabName = "output_surv", 
          icon = icon("chart-line")
        ),
        menuItem("Save project", tabName = "save", icon = icon("save"))
      )
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css")
      ),
      includeCSS("./style.css"),
      tabItems(
        tabItem(
          tabName = "home",
          fluidPage(
            class = "custom-fluid-homePage",
            fluidRow(
              titlePanel("Project list"),  
              tags$br(),
              tags$br(),
              DTOutput("projectList")
            ),
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
                     selectInput("data_type", 
                                 "Select the type of data you want to load", 
                                 choices = c("Select data type", "Bulk single", "Bulk multiple", "Single cell"),
                                 selected = "Select data type"),
                     uiOutput("dataFile"),
              ),
              column(6,
                     div(style = "margin-top: 70px;",  
                         uiOutput("dataFile2"),
                         div(style = "margin-top: -10px;",  
                             uiOutput("loadBtn2")),
                         fluidRow(
                           column(width = 12, style = "margin-top: 2px; margin-bottom: 30px;", 
                                  uiOutput("directoryInput"))
                         )
                     )
              )
            ),
            conditionalPanel(
              condition = "input.data_type === 'Select data type'",
              wellPanel(
                tags$div(
                  tags$h4("Select a data type and upload a tab-delimited text file."),
                  tags$p("Below are the details for each data type:"),
                  tags$strong("Bulk single:"),
                  tags$p("Data from a single region with the following columns:"),
                  tags$ul(
                    tags$li("Sample: Identifier of the analyzed sample."),
                    tags$li("Gene: List of genes analyzed."),
                    tags$li("CCF (Cancer Cell Fraction): Fraction of cancer cells carrying the mutation.")
                  ),
                  tags$strong("Bulk multiple:"),
                  tags$p("Data from multiple regions with the following columns:"),
                  tags$ul(
                    tags$li("Sample: Identifier of the analyzed sample."),
                    tags$li("Region: Specific region of the tumor."),
                    tags$li("Gene: List of genes analyzed."),
                    tags$li("CCF (Cancer Cell Fraction): Fraction of cancer cells carrying the mutation.")
                  ),
                  tags$strong("Single cell:"),
                  tags$p("Data for individual cells with the following columns:"),
                  tags$ul(
                    tags$li("Patient: Identifies the patient."),
                    tags$li("Cell: Name or identifier of the cell."),
                    tags$li("Gene: Specific gene examined."),
                    tags$li("CCF: Indicates presence (1) or absence (0) of a mutation.")
                  ),
                  style = "color: #242D33;margin-left: 10px; margin-right: 10px;"
                )
              )
            ),
            conditionalPanel(
              condition = "input.data_type === 'Bulk single' && output.dataTable2 == null",
              wellPanel(
                tags$div(
                  tags$h4("Additional file requirements for resampling:"),
                  tags$p("If resampling is required in subsequent steps, an additional file structured as follows must also be provided:"),
                  tags$ul(
                    tags$li("SAMPLE: Identifier of the analyzed sample."),
                    tags$li("GENE: Specific gene that has been analyzed."),
                    tags$li("REF_COUNT: Number of reads supporting the reference allele."),
                    tags$li("ALT_COUNT: Number of reads supporting the alternate allele."),
                    tags$li("COPY_NUMBER: Number of gene copies in the DNA."),
                    tags$li("NORMAL_PLOIDY: Normal ploidy of the sample."),
                    tags$li("CCF_ESTIMATE: Estimate of the Cancer Cell Fraction containing the mutated allele.")
                  ),
                  style = "color: #242D33;margin-left: 10px; margin-right: 10px;"
                )
              )
            ),
            conditionalPanel(
              condition = "output.DeleteColumn != null",
              h3("Sample and Features Selection", style = "margin-top: 20px;")
            ),
            tags$br(),
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
                     div(
                       style = "display: flex; justify-content: flex-end; margin-top: 20px; margin-bottom: 20px; margin-right: -20px;",
                       actionButton("inferenceBtn", "Inference", class = "custom-button2")
                     ),
                     tags$br(),
                     tags$br(),
                     DTOutput("dataTable")
              )
            ),
            fluidRow(
              column(12, uiOutput("content"))
            ),
            fluidRow(
              column(12,
                     tags$br(),
                    uiOutput("heatmapPlot")
              )
            )
            ,
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
            tags$h3(HTML("ASCETIC <i>plus</i> hyperparameters"), style = "margin-top: 20px;"),
            tags$br(),
            tags$br(),
            fluidRow(
              column(6,
                     style = "margin-top: -10px; margin-bottom: 30px;",
                     selectInput("regularization", "Regularization", 
                                 c("aic", "bic", "loglik", "ebic", 
                                   "bde", "bds", "mbde", 
                                   "bdla", "k2", "fnml", "qnml", "nal", 
                                   "pnal"), 
                                 multiple = TRUE, selected = "aic"),
                     selectInput("command", "Search algorithm", c("hc","tabu")),
                     numericInput("restarts", "Likelihood fit restarts", 10, min = 0),
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
                     actionButton("submitBtn", "Run", class = "custom-button")
              ),
            ),
            fluidPage(
              style = "margin-left: -15px; ",
              fluidRow(
                column(6,
                       div(
                         style = "display: flex; justify-content: flex-end; margin-top: 20px; margin-bottom: 20px; margin-right: -560px;",
                         actionButton("confidenceBtn", "Confidence estimation", class = "custom-button2")
                       ),
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
                       div(style = "margin-top:105px;", 
                           conditionalPanel(
                             class = "no-border-bg",
                             condition = "output.visualize_inference != null",
                             downloadButton("downloadCSV", title = "Download your matrix", 
                                            class = "custom-width1"),
                             bsTooltip(id = "downloadCSV", 
                                       title = "Download your matrix", 
                                       placement = "right", trigger = "hover")
                           ),
                       conditionalPanel(
                         class = "no-border-bg",
                         condition = "output.graph_inference != null",
                         sliderInput("nodeSize", "Node size", min = 5,
                                     max = 30, value = 8)
                       ),
                    ),
                ),
              ),
            ),
            tags$div(style = "height: 40px;"),
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
                       bsTooltip(id = "resamplingFlag_conf", 
                                 title = "Estimate the confidence of the model by resampling", 
                                 placement = "right", trigger = "hover"),
                       style = "margin-top: 45px;", width = "500px"
                     ),
                     uiOutput("nresampling_conf", style = "margin-top: 25px;")
              ),
              column(6, 
                     style = "margin-top: -10px; margin-bottom: 30px;",
                       numericInput("iteration_confEstimation", "Confidence estimate repetitions", value = 10, min = 3),
                       bsTooltip(id = "iteration_confEstimation", 
                                 title = "A higher number of iterations allows for more precise estimates, although it increases the computational load", 
                                 placement = "right", trigger = "hover")
                      ),
              column(12, align = "left",
                     actionButton("submitBtn_confEstimation", "Run", class = "custom-button")
              ),
            ),
            fluidPage(
              style = "margin-left: -15px; ",
              fluidRow(
                column(6,
                       div(
                         style = "display: flex; justify-content: flex-end; margin-top: 20px; margin-bottom: 20px; margin-right: -560px;",
                         actionButton("survBtn", "Input data survival", class = "custom-button2")
                       ),
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
                       div(style = "margin-top:105px;", 
                         conditionalPanel(
                           class = "no-border-bg",
                           condition = "output.visualize_conf != null",
                           downloadButton("downloadCSV_conf", 
                                          class = "custom-width1"),
                           bsTooltip(id = "downloadCSV_conf", 
                                     title = "Download your matrix", 
                                     placement = "right", trigger = "hover")
                         ),
                         conditionalPanel(
                           class = "no-border-bg",
                           condition = "output.graph_conf != null",
                           sliderInput("nodeSize", "Node size", min = 5,
                                       max = 30, value = 8)
                         ),
                       )
                ),
              ),
            ),
            tags$div(style = "height: 40px;"),
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
                     selectInput("regularization_surv", "Select an evolution model", choices = NULL),
                     uiOutput("dataFile2_surv"),
                     tags$br(),
                     tags$br(),
                       wellPanel(
                         tags$div(
                           style = "color: #242D33; margin-left: 10px; margin-right: 10px;",
                           tags$h4("Select and upload a tab-delimited text file."),
                           tags$p("The initial input for the survival analysis includes the survival data file, which consists of the following columns:"),
                           tags$ul(
                             tags$li("SAMPLE: Unique identifier for each patient."),
                             tags$li("STATUS: Indicates survival status (0 for alive, 1 for deceased)."),
                             tags$li("TIMES: Duration of survival from the initial diagnosis or start of treatment, measured in months.")
                           ),
                           tags$p("Please ensure that the patients' data are present in both uploaded files to perform a correct analysis. The system automatically matches the patients found in both datasets to ensure accurate results.")
                         )
                      )
              ),
              column(6,
                     tags$div(
                       tags$div(style="margin-top: 25px;",  
                                checkboxInput("load_file", 
                                              HTML("<strong>Use the same model with a different dataset</strong>")),
                       ),
                       tags$div(style="margin-top: 30px;",  
                                uiOutput("data_type_surv"),
                       ),
                       tags$div(style="margin-top: 20px;",  
                                uiOutput("dataFile_surv")
                       ),
                     )
              ),
              column(12, align = "left",
                     actionButton("submit_surv", "Show evolutionary step occurrence matrix", class = "custom-button", style = "margin-top: 30px;")              ),
            ),
            div(
              style = "display: flex; justify-content: flex-end; margin-top: 20px; margin-bottom: 20px; margin-right: -20px;",
              actionButton("out_survBtn", "Evolutionary signatures", class = "custom-button2")
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
                     DTOutput("dataTable_GenotypeSurv"),
                     uiOutput("heatmap_GenotypeSurv")
              )
            ),
            div(style = "margin-top: 30px;",  
                fluidRow(
                  column(6, uiOutput("DeleteColumn_surv2")),
                  column(6, uiOutput("binarization_surv2"))
                )
            ),
            
            fluidRow(
              column(6, uiOutput("DeleteRow_surv2")),
            ),
            fluidRow(
              column(12,
                    style = "margin-top: 30px;",
                    DTOutput("dataTable_surv"),
                    uiOutput("heatmap_surv")
              )
            ),
          ),
          style = "margin-bottom: 500px;",
        ),
        tabItem(
          tabName = "output_surv",
          fluidPage(
            actionButton("calc_surv", "Run Regularized Cox Regression", class = "custom-button", style = "margin-top: 30px;"),
            tags$br(),
            tags$br(),
            fluidRow(
              girafeOutput("combined_graph", width = "100%", height = "700px")
            ),
            div(style = "margin-top: 20px;"),
            fluidRow(
                  plotlyOutput("survPlot", width = "100%", height = "400px"),
                  style = "width: 75%; margin: 0 auto;",
                  tags$br(),
                  plotlyOutput("survPlot2", width = "100%", height = "200px"),
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
