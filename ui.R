
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
library(shinydashboard)

shinyUI(
  
  
  
  dashboardPage(
    dashboardHeader(title = "ASCETIC 2.0"),
    dashboardSidebar(
      sidebarMenu(
        id = "sidebarMenu",
        menuItem("Home page", tabName = "home", icon = icon("home")),
        menuItem("Input data", tabName = "input", icon = icon("database")),
        menuItem("Inference", tabName = "inference", icon = icon("chart-line")),
        menuItem("Save project", tabName = "save", icon = icon("save"))
      )
    ),
    dashboardBody(
      tags$head(
        tags$style(
          HTML("
          @import url('https://fonts.googleapis.com/css2?family=Roboto+Condensed:ital,wght@0,100..900;1,100..900&display=swap');
        
          body {
            font-family: Roboto Condensed;
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
        
          #restarts {
            border-radius: 5px;
          }
          
          #seed {
            border-radius: 5px;
          }
          
          #project_name {
            border-radius: 5px;
          }
          
          #binarization {
            border-radius: 5px;
          }
          
          #binarization_perc {
            border-radius: 5px;
          }

          .custom-button {
            color: white;
            background-color: #628291;
            border-color: #628291;
            border-radius: 3px;
          }
          
          .custom-button:hover, .custom-button:active {
            color: white;
            background-color: #242D33;
            border-color: #242D33;

          }
          
          table.dataTable tbody tr:hover {
              background-color: #7BA1B3 !important;
          }
          
          
          table.dataTable tbody tr.selected td, 
          table.dataTable tbody td.selected {
              box-shadow: inset 0 0 0 9999px #7BA1B3 !important;
          }
          
          table.dataTable tbody tr:active td {
              background-color: #7BA1B3 !important;
          }
          
          :root {
              --dt-row-selected: transparent !important;
          }
          
          .skin-blue .main-header .logo {
              background-color: #628291;
          }
          .skin-blue .main-header .logo:hover {
              background-color: #628291;
          }
                              
          .skin-blue .main-header .navbar {
              background-color: #79A0B3;
          }
        
          .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
              background-color: #ECF0F5;
              color: #222D32;
          }
                              

          
          .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
              background-color: #79A0B3;
              color: ECF0F5;
          }
        ")
        ),
      ),
      tabItems(
        tabItem(
          tabName = "home",
          fluidPage(
            style = "margin-left: 100px; margin-right: 100px; margin-top: 100px;",
            DTOutput("projectList"),
            fluidRow(
              style = "display: flex; justify-content: center; margin-top: 40px;", 
              column(3,
                     actionButton("loadProjBtn", "Load existing project", class = "custom-button")
              ),
              column(3,offset = 1,
                     actionButton("create_project_button", "Create New Project", class = "custom-button")
              )
            )
          )
        ),
        tabItem(
          tabName = "input",
          fluidPage(
            style = "margin-left: 10px; margin-right: 10px; margin-top: 20px;",
            fluidRow(
              column(6,
                     fileInput("dataFile", "Genotype") %>%
                       shinyInput_label_embed(
                         shiny_iconlink() %>%
                           bs_embed_popover(title = "Genotipo")
                       ),
                     actionButton("loadBtn", "Load", class = "custom-button")
              ),
              column(6,
                     uiOutput("dataFile2"),
                     uiOutput("loadBtn2"),
                     uiOutput("directoryInput")
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
            DTOutput("dataTable2"),
            conditionalPanel(
              condition = "input.loadBtn > 0 || input.loadProjBtn > 0",
              class = "text-center",
              uiOutput("switchViewBtn")
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
          )
        ),
        tabItem(
          tabName = "inference",
          fluidPage(
            style = "margin-left: 10px; margin-right: 10px;",
            fluidRow(
              column(6,
                     style = "margin-top: -10px;",
                     selectInput("regularization", "Regularization", 
                                 c("aic", "bic", "loglik", "ebic", 
                                   "pred-loglik", "bde", "bds", "mbde", 
                                   "bdla", "k2", "fnml", "qnml", "nal", 
                                   "pnal"), 
                                 multiple = TRUE, selected = "aic"),
                     selectInput("command", "Command", c("hc","tabu")),
                     numericInput("restarts", "Restarts", 10, min = 0)
              ),
              column(6, 
                     tags$div(
                       numericInput("seed", "Seed", 12345, min = 0),
                       style = "margin-top: -10px;"
                     ),
                     tags$div(
                       checkboxInput("resamplingFlag", 
                                     HTML("<strong>Resampling</strong>")),
                       style = "margin-top: 35px;", width = "500px"
                     ),
                     conditionalPanel(
                       condition = "input.resamplingFlag == true",
                       tags$div(
                         numericInput("nresampling", 
                                      "Number of samplings", 3, min = 3),
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
