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
    column(12, uiOutput("content"))
  )
)

server <- function(input, output, session) {
  
  output$tabsetUI <- renderUI({
    tabsetPanel(
      id = "tabset",
      tabPanel("Input dati", "Visualizzazione matrice e albero corrispondente", 
               style = "margin-right: 30px;",
               fileInput("dataFile", "Carica File Tabella (.txt)"),
               actionButton("loadBtn", "Load"),
               fileInput("dataFile1", "Carica File Tabella (.txt)"),
               actionButton("loadBtn1", "Load"),
               DTOutput("dataTable"),
               uiOutput("directoryInput")  
      ),
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
               ),
               actionButton("submitBtn", "Invia")
      )
    )
  })
  
  ##### FASE INPUT DATI
  
 ## File nsampling
  observeEvent(input$loadBtn1, {
    
    inFile <- input$dataFile1
    data <- read.table(inFile$datapath, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
    
      output$dataTable <- renderDT({
        datatable(data)
      })
  })
  
  # File matrice con mutazioni
  observeEvent(input$loadBtn, {
    
    inFile <- input$dataFile
    data <- read.table(inFile$datapath, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
    
    # Verifico il numero di colonne
    if (ncol(data) == 3) {
      
      reshaped_data <- acast(data, SAMPLE ~ GENE, value.var = "CCF", fill = 0)
      
      
      output$dataTable <- renderDT({
        datatable(reshaped_data)
      })
      
      
      non_zero_values <- reactiveVal(NULL)
      
      
      output$content <- renderUI({
        req(!is.null(non_zero_values()))
        
        tagList(
          tags$hr(),
          tags$p("POSET:"),
          tags$p(paste(non_zero_values(), collapse = " --> "))
        )
      })
      
      
      observeEvent(input$dataTable_cell_clicked, {
        info <- input$dataTable_cell_clicked
        
        if (!is.null(info)) {
          row_index <- info$row
          non_zero_values(names(which(reshaped_data[row_index, ] != 0)))
        }
      })
    
      # se le colonne sono più di tre
    } else {
      
      ## CON L'ALTRO DATASET QUI DOVRO' CONTROLLARE IL NOME DELLA SECONDA COLONNA PER CAPIRE SE RICADO IN UN CASO O NELL'ALTRO
      ## NELL'ALTRO CASO DOVRò FARE LE STESSE IDENTICHE COSE MA LA PRIMA TABELLA DOVRà CALCOLARE PER OGNI GENE LA PERCENTUALE DI 
      ## PRESENZA DI UNA MUTAZIONE ALL'INTERNO DI QUESTA FACENDO UNA MEDIA TRA TUTTE LE CELLULE
      
      
      reshaped_data <- data %>%
        group_by(SAMPLE, REGION, GENE) %>%
        summarise(CCF = sum(CCF)) %>%
        unite("ID", c("SAMPLE", "REGION"), sep = " ") %>%
        pivot_wider(names_from = GENE, values_from = CCF, values_fill = 0) %>%
        select(ID, everything())  
      
      
      output$dataTable <- renderDT({
        datatable(reshaped_data)
      })
      
      observeEvent(input$dataTable_cell_clicked, {
        info <- input$dataTable_cell_clicked
        
        if (!is.null(info)) {
          row_index <- info$row
          selected_id <- as.character(strsplit(as.character(reshaped_data[row_index, "ID"]), " ")[[1]][1])
          
          # Cerco il file nella directory selezionata
          req(input$dir)
          selected_folder <- parseDirPath(c(wd = getwd()), input$dir)
          file_to_search <- paste0(selected_id, ".txt")
          file_path <- file.path(selected_folder, file_to_search)
          file_data <- read.table(file_path, header = TRUE, stringsAsFactors = FALSE, sep = "\t")
          
          col_names <- colnames(file_data)
          
          file_data$ID <- col_names
          
          
          file_data <- file_data[, c("ID", setdiff(col_names, "ID"))]
          
          
          output$content <- renderUI({
            tagList(
              tags$hr(),
              tags$p("File selezionato:"),
              tags$p(file_path),
              tags$p("Contenuto del file:"),
              DTOutput("fileDataTable"),
              plotOutput("graphPlot") 
            )
          })
          
          output$fileDataTable <- renderDT({
            datatable(file_data)
          })
          
          
          graph_data <- reshape2::melt(file_data, id.vars = "ID")
          edges <- graph_data[graph_data$value != 0, c("ID", "variable")]
          
          # Creare il grafo
          graph <- graph_from_data_frame(edges, directed = TRUE)
          
          # Visualizza il grafo
          output$graphPlot <- renderPlot({
            plot(graph, edge.label = edges$value, layout = layout.circle)
          
          })
        }
      })
      
      
      output$directoryInput <- renderUI({
        if (ncol(data) > 3) {
          shinyDirButton("dir", "Seleziona una cartella", title = "Seleziona una cartella", multiple = FALSE)
        }
      })
    }
  })
  
  shinyDirChoose(input, "dir", roots = c(wd = getwd()), filetypes = c("", "txt"))
  
  observe({
    req(input$dir)
    selected_folder <- parseDirPath(c(wd = getwd()), input$dir)
    print(selected_folder)
  })
  

  
  #caricamento dati di tipo r
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
  
  # gestione fase inferenza
  observeEvent(input$submitBtn, {
    method <- input$method
    nsampling <- input$nsampling
    nsampling_ph <- input$nsampling_phylo
    
    result <- reactiveVal(NULL)
    
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
          restarts = input$restarts
        )
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