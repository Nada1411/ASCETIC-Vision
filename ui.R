# ui.R

shinyUI(
  fluidPage(
    tags$head(
      tags$style(
        HTML("
        @import url('https://fonts.googleapis.com/css2?family=Roboto+Condensed:ital,wght@0,100..900;1,100..900&display=swap');
        
        
          #header {
            background-color: #5f9ea0;
            padding: 5;
            color: white;
            display: flex;
            justify-content: space-between;
            align-items: center;
          }

          #asceticTitle {
            font-weight: bold;
            background-color: #00796B;
            padding: 10px 35px;
            margin-right: 10px;
            color: white;
          }

          .sectionBtn {
            background: none;
            border: none;
            color: #008080;
            padding: 10px 20px;
            border-radius: 5px;
            margin-top: 50px;
          }

          .linea {
            border-bottom: 2px solid #008080;
            width: 100%;
            margin-top: 10px;
            display: none; 
          }

          #content {
            text-align: center; 
          }

          #tabset {
            margin-top: 30px; 
          }
          
          #loadBtn {
            margin-top: -15px;  
          }
          
          #loadBtn2 {
            margin-top: -7px;  
          }
          
          #switchViewBtn {
            margin-top: 40px;  
          }
          
          
          #dataTable2 {
            margin-top: 60px;  
          }
          
          #switchViewBtn {
            margin-top: -10px;  
          }
          
          #directoryInput{
            margin-top: 32px;  
          }

          
          #main_tabset{
            margin-top: 30px;  
          }
          
          #projectList{
            margin-top: 30px;  
          }
          
          #inference_tab{
            margin-top: 600px; 
          }
          
          body {
          font-family: Roboto Condensed;
        }
          
        ")
      )
    ),
    
    
    useShinyjs(), 
    
    tags$div(
      id = "header",
      tags$div(
        id = "asceticTitle",
        "ASCETIC 2.0"
      )
    ),
    
    tabsetPanel(
      id = "main_tabset",
      tabPanel("Home Page",
               fluidPage(
                 style = "margin-left: 100px; margin-right: 100px; margin-top: 100px;background-color: lightblue",
                 DTOutput("projectList"),
                 fluidRow(
                   style = "display: flex; justify-content: center; margin-top: 40px;", 
                   column(3,
                          actionButton("loadProjBtn", "Load existing project"),
                   ),
                   column(3,
                          actionButton("create_project_button", "Create New Project")
                   )
                 )
               )
      ),
      
      tabPanel("Input dati",
               id = "input_tab",
               style = "margin-left: 10px; margin-top: 20px;",
               fluidRow(
                 column(6,
                        fileInput("dataFile", "Genotipo")%>%
                          shinyInput_label_embed(
                            shiny_iconlink() %>%
                              bs_embed_popover(
                                title = "Genotipo"
                              )
                          ),
                        actionButton("loadBtn", "Load")
                 ),
                 column(6,
                          uiOutput("dataFile2"),
                          uiOutput("loadBtn2"),
                          uiOutput("directoryInput")

                 )),
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
                        uiOutput("binarization_perc"),
                 ),
               ),
               DTOutput("dataTable2"),
               conditionalPanel(
                 condition = "input.loadBtn > 0 || input.loadProjBtn > 0",
                 uiOutput("switchViewBtn"),
               ),
               conditionalPanel(
                 condition = "input.switchViewBtn % 2 == 1",
                 DTOutput("dataTable")
               ),
               conditionalPanel(
                 condition = "input.switchViewBtn % 2 == 0",
                 plotly::plotlyOutput("heatmapPlot")
               ),
               fluidRow(
                 column(12, uiOutput("content"))
               )
      ),
      tabPanel("Inference",
               id = "inference_tab",
               fluidRow(
                 column(6, 
                        checkboxInput("resamplingFlag", "Resampling"),
                        conditionalPanel(
                          condition = "input.resamplingFlag == true",
                          numericInput("nresampling", "Numero di campionamenti", 3, min = 3)
                        )
                 ),
                 column(6,
                        selectInput("regularization", "Regularization", c("aic", "bic", "loglik", "ebic", "pred-loglik", "bde", "bds", "mbde", "bdla", "k2", "fnml", "qnml", "nal", "pnal"), multiple = TRUE, selected = "aic"),
                        selectInput("command", "Command", c("hc","tabu"))
                 ),
                 column(6,
                        numericInput("restarts", "Restarts", 10, min = 0)
                 ),
                 column(6,
                        numericInput("seed", "Seed", 12345, min = 0)
                 )
               ),
               actionButton("submitBtn", "Invia"),
               uiOutput("visualize_inference"),
               DTOutput("selected_result_output"),
               plotOutput("graph_inference"),
               uiOutput("interruptButton"),
               uiOutput("spinner"),
               style = "margin-top: 30px;"
      ),
      
      tabPanel("Salva progetto",
               id = "save_tab",
               style = "margin-left: 10px; margin-top: 20px;",
               textInput("project_name", "Nome del progetto", ""),
               actionButton("saveBtn", "Salva")
      )
    ),
    uiOutput("tabsetUI"),
    div(id = "linea", class = "linea"), 
  )
)

