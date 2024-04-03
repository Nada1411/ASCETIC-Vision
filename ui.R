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
library(visNetwork)


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
            fluidRow(
              column(12,
                     conditionalPanel(
                       condition = "input.loadBtn > 0 || input.loadProjBtn > 0",
                       uiOutput("switchViewBtn")
                     ),
                     style = "margin-top: 50px;"  
              )
            ),
            conditionalPanel(
              condition = "output.dataTable",
              tags$div("Genotype", style = "font-weight: bold; margin-top: 30px; 
                       margin-left: 0px; font-size: 17px; margin-bottom: 20px;"),  
            ),
            fluidRow(
              column(12,
                     conditionalPanel(
                       condition = "input.switchViewBtn % 2 == 1",
                       plotly::plotlyOutput("heatmapPlot")
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
                       margin-top: 30px; margin-left: 0px; font-size: 17px; 
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
                       style = "margin-top: 45px;", width = "500px"
                     ),
                     uiOutput("nresampling", style = "margin-top: 25px;"),
              ),
            ),
            actionButton("submitBtn", "Invia", class = "custom-button"),
            tags$div(style = "height: 40px;"),
            uiOutput("visualize_inference"),
            conditionalPanel(
              condition = "output.graph_inference",
              tags$div("Inference output", style = "font-weight: bold; 
                       margin-top: 30px; font-size: 17px; margin-bottom: 20px; 
                       text-align: center;"),
            ),
            div(
              style = "display: flex; justify-content: center;",
              visNetworkOutput("graph_inference", width = "50%", height = "400px")
            ),
            style = "margin-top: 30px;",
            div(
              style = "display: flex; justify-content: center; margin-top: -350px;",
              DTOutput("selected_result_output")
            ),
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
