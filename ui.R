# ui.R

library(shiny)
library(shinyjs)
library("ASCETIC")
library(DT)
library(tidyr)
library(data.table)
library(reshape2)
library(dplyr)
library(shinyFiles)
library(igraph)

shinyUI(
  fluidPage(
    tags$head(
      tags$style(
        HTML("
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
          
          #SelectColumn{
            margin-top: 10px;  
          }
          
          #main_tabset{
            margin-top: 30px;  
          }
          
          #create_project_button{
            margin-top: 30px;  
          }

          
          #projectList{
            margin-top: 30px;  
            margin-left: -30px;  
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
      tabPanel("Gestione Progetti",
               fluidRow(
                 column(6,
                        actionButton("create_project_button", "Crea Nuovo Progetto")
                 ),
                 column(6,
                        DTOutput("projectList")
                 )
               )
      ),
      
      tabPanel("Input dati",
               id = "input_tab",
               style = "margin-left: 10px; margin-top: 20px;",
               fluidRow(
                 column(6,
                        fileInput("dataFile", "Genotipo"),
                        actionButton("loadBtn", "Load")
                 ),
                 column(6,
                        conditionalPanel(
                          condition = "input.loadBtn > 0",
                          uiOutput("dataFile2"),
                          uiOutput("loadBtn2"),
                          uiOutput("directoryInput")
                        )
                 )),
               fluidRow(
                 column(6,
                        uiOutput("SelectColumn"),
                 ),
                 column(6,
                        uiOutput("DeleteColumn"),
                 ),
               ),
               uiOutput("DeleteRow"),
               DTOutput("dataTable2"),
               conditionalPanel(
                 condition = "input.loadBtn > 0 || input.projectList_rows_all > 0",
                 actionButton("switchViewBtn", "Switch View"),
               ),
               conditionalPanel(
                 condition = "input.switchViewBtn % 2 == 1",
                 DTOutput("dataTable")
               ),
               conditionalPanel(
                 condition = "input.switchViewBtn % 2 == 0",
                 plotly::plotlyOutput("heatmapPlot")
               )
               
      ),
      tabPanel("Inference",
               id = "inference_tab",
               selectInput("method", "Seleziona il Metodo", c("CCF", "Phylogenies")),
               conditionalPanel(
                 condition = "input.method == 'CCF'",
                 fileInput("dataset", "Carica Dataset"),
                 fileInput("ccfDataset", "Carica CCF Dataset"),
                 numericInput("nsampling", "Numero di campionamenti", 100),
                 conditionalPanel(
                   condition = "input.nsampling > 0",
                   fileInput("vafDataset", "Carica VAF Dataset")
                 ),
                 selectInput("regularization", "Regularization", c("aic", "bic")),
                 textInput("command", "Command", "hc"),
                 numericInput("restarts", "Restarts", 10)
               ),
               conditionalPanel(
                 condition = "input.method == 'Phylogenies'",
                 fileInput("dataset_phylo", "Carica Dataset"),
                 fileInput("models_phylo", "Carica Models"),
                 numericInput("nsampling_phylo", "Numero di campionamenti", 100),
                 selectInput("regularization_phylo", "Regularization", c("aic", "bic")),
                 textInput("command_phylo", "Command", "hc"),
                 numericInput("restarts_phylo", "Restarts", 10)
               ),
               actionButton("submitBtn", "Invia")
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
    
    fluidRow(
      column(12, uiOutput("content"))
    )
  )
)

