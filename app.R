library(shiny)
library(shinyjs)
library("ASCETIC")



ui <- fluidPage(
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
  

  uiOutput("tabsetUI"),
  
  div(id = "linea", class = "linea"), 
  
  fluidRow(
    column(12, uiOutput("content")),
    column(12, actionButton("submitBtn", "Invia"))
  )
)

server <- function(input, output, session) {
  
  result <- reactiveVal(NULL)
  
  output$tabsetUI <- renderUI({
    tabsetPanel(
      id = "tabset",
      tabPanel("Inference", 
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
               )
      ),
      tabPanel("Parte 2", "Contenuto della sezione Parte 2", style = "margin-right: 30px;"), 
      tabPanel("Parte 3", "Contenuto della sezione Parte 3")
    )
  })
  
  
  loadEnvironment <- function(file_inputs) {
    environments <- list()
    for (i in seq_along(file_inputs)) {
      inFile <- file_inputs[[i]]
      file <- inFile$datapath
      e <- new.env()
      name <- load(file, envir = e)
      environments[[paste0("data_", i)]] <- e[[name]]
    }
    return(environments)
  }
  
  observeEvent(input$submitBtn, {
    method <- input$method
    nsampling <- input$nsampling
    nsampling_ph <- input$nsampling_phylo
    
    if (method == "CCF") {
      
      if (nsampling > 0) {
        file_inputs <- list(input$dataset, input$ccfDataset, input$vafDataset)
        environments <- loadEnvironment(file_inputs)
        
        resExampleSingleSamplesResampling  <- asceticCCFResampling(
          dataset = environments$data_1,
          ccfDataset = environments$data_2,
          vafDataset= environments$data_3,
          nsampling = nsampling,
          regularization = input$regularization,
          command = input$command, 
          restarts = input$restarts
        )
      } else {
        file_inputs <- list(input$dataset, input$ccfDataset)
        environments <- loadEnvironment(file_inputs)
        
        resExampleSingleSamples <- asceticCCF(
          dataset = environments$data_1,
          ccfDataset = environments$data_2,
          regularization = input$regularization,
          command = input$command, 
          restarts = input$restarts)
      }
    } else if (method == "Phylogenies") {
      file_inputs <- list(input$dataset_phylo, input$models_phylo)
      environments <- loadEnvironment(file_inputs)
      
      if (nsampling_ph > 0) {
        resExamplePhylogeniesDataset <- asceticPhylogeniesBootstrap(
          dataset = environments$data_1,
          models  = environments$data_2,
          nsampling = nsampling_ph,
          regularization = input$regularization_phylo,
          command = input$command_phylo,
          restarts = input$restarts_phylo
        )
      } else {
        resExamplePhylogeniesDataset <- asceticPhylogenies(
          dataset = environments$data_1,
          models  = environments$data_2,
          regularization = input$regularization_phylo,
          command = input$command_phylo,
          restarts = input$restarts_phylo
        )
      }
    }
    
  })

}

shinyApp(ui, server)
