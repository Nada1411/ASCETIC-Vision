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
library(shinycssloaders)
library("bsplus")




shinyUI(
  fluidPage(

    
    tags$head(
      tags$style(
        HTML("
          @import url('https://fonts.googleapis.com/css2?family=Roboto+Condensed:ital,wght@0,100..900;1,100..900&display=swap');
        
          body {
            font-family: Roboto Condensed;
          }
          
          button {
            color: red;
          }
        
          #header {
            background-color: #4A687E;
            padding: 5;
            color: white;
            display: flex;
            justify-content: space-between;
            align-items: center;
          }

          #asceticTitle {
            font-weight: bold;
            background-color: #242D33;
            padding: 10px 35px;
            margin-right: 10px;
            color: white;
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
            margin-top: -20px;  
          }
          
          #inference_tab{
            margin-top: 600px; 
          }

          .linea {
            border-bottom: 2px solid #008080;
            width: 100%;
            margin-top: 10px;
            display: none; 
          }

          .custom-button {
            color: white;
            background-color: #4A687E;
            border-color: #4A687E;
            border-radius: 3px;
          }
          
          .custom-button:hover, .custom-button:active {
            color: white;
            background-color: #242D33;
            border-color: #242D33;
          }
        ")
      ),
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
      tabPanel(HTML("<span style='color: #4A687E;'>Home page</span>"),
               class = "custom-tab-panel",
               fluidPage(
                 style = "margin-left: 100px; margin-right: 100px; margin-top: 100px;background-color: white",
                 DTOutput("projectList"),
                 fluidRow(
                   style = "display: flex; justify-content: center; margin-top: 40px;", 
                   column(3,
                          actionButton("loadProjBtn", "Load existing project", class = "custom-button"),
                   ),
                   column(3,
                          actionButton("create_project_button", "Create New Project", class = "custom-button")
                   )
                 )
               )
      ),
      
      tabPanel(HTML("<span style='color: #4A687E;'>Input dati</span>"),
               id = "input_tab",
               style = "margin-left: 10px; margin-right: 10px; margin-top: 40px;",
               fluidRow(
                 column(6,
                        fileInput("dataFile", "Genotipo")%>%
                          shinyInput_label_embed(
                            shiny_iconlink() %>%
                              bs_embed_popover(
                                title = "Genotipo"
                              )
                          ),
                        actionButton("loadBtn", "Load", class = "custom-button")
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
                 class = "text-center",
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
      tabPanel(HTML("<span style='color: #4A687E;'>Inference</span>"),
               id = "inference_tab",
               fluidRow(
                 column(6,
                        selectInput("regularization", "Regularization", c("aic", "bic", "loglik", "ebic", "pred-loglik", "bde", "bds", "mbde", "bdla", "k2", "fnml", "qnml", "nal", "pnal"), multiple = TRUE, selected = "aic"),
                        selectInput("command", "Command", c("hc","tabu")),
                        numericInput("restarts", "Restarts", 10, min = 0)
                 ),
                 column(6, 
                        numericInput("seed", "Seed", 12345, min = 0),
                        tags$div(
                          checkboxInput("resamplingFlag", HTML("<strong>Resampling</strong>")),
                          style = "margin-top: 45px;", width = "500px"
                        ),
                        conditionalPanel(
                          condition = "input.resamplingFlag == true",
                          tags$div(
                            numericInput("nresampling", "Numero di campionamenti", 3, min = 3),
                            style = "margin-top: 25px;"
                          )
                        )
                 ),
               ),
               actionButton("submitBtn", "Invia", class = "custom-button"),
               uiOutput("visualize_inference"),
               DTOutput("selected_result_output"),
               plotOutput("graph_inference"),
               uiOutput("interruptButton"),
               uiOutput("spinner"),
               style = "margin-top: 30px;"
      ),
      
      tabPanel(HTML("<span style='color: #4A687E;'>Salva progetto</span>"),
               id = "save_tab",
               style = "margin-left: 10px; margin-top: 20px;",
               textInput("project_name", "Nome del progetto", ""),
               actionButton("saveBtn", "Salva", class = "custom-button")
      )
    ),
    uiOutput("tabsetUI"),
    div(id = "linea", class = "linea"), 
  )
)

