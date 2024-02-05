# server.R

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
library(POSetR)
library(plotly)


server <- function(input, output, session) {
  
  
######################################################### FASE INPUT DATI  #########################################################
  
  
  selected_folder <- reactiveVal(NULL)
  
  # Visualizzazione del file di resampling, nel caso in cui le colonne siano tre viene rappresentata la tabella
  # negli altri casi viene stampato un messaggio di errore
  observeEvent(input$loadBtn2, {
    inFile2 <- input$dataFile2
    data2 <- read.table(inFile2$datapath, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
    inFile <- input$dataFile
    data <- read.table(inFile$datapath, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
    
    # Verifica che il secondo file abbia due colonne
    if (ncol(data) == 3) {
      output$dataTable2 <- renderDT({
        datatable(data2, options = list(scrollX = TRUE))
      })
    } else {
      showNotification("Resampling non necessario", type = "error")
    }
  })
  
  
  # Visualizzazione della matrice agaenotipo
  observeEvent(input$loadBtn, {
    output$dataTable2 <- renderDT({})
    output$directoryInput <- renderUI({})
    output$SelectColumn <- NULL
    inFile <- input$dataFile
    
    if (is.null(inFile)) {
      showNotification("File non caricato.", type = "error")
    } else {
      data <- read.table(inFile$datapath, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
      
      #### Bulk singola biopsia
      if (ncol(data) == 3) {
        output$dataFile2 <- renderUI({
           fileInput("dataFile2", "Resampling")
        })
        
        output$loadBtn2 <- renderUI({
          actionButton("loadBtn2", "Load")
        })
        
        data <- distinct(data, SAMPLE, GENE, .keep_all = TRUE)
        reshaped_data <- acast(data, SAMPLE ~ GENE, value.var = "CCF", fill = 0)
       
        output$SelectColumn <- renderUI({
            selectInput("SelectColumn", "Select column", choices=colnames(reshaped_data), multiple=T)
        })
        
        output$DeleteColumn <- renderUI({
          selectInput("DeleteColumn", "Delete column", choices=colnames(reshaped_data), multiple=T)
        })
        
        output$DeleteRow <- renderUI({
          selectInput("DeleteRow", "Delete row", choices=row.names(reshaped_data), multiple=T)
        })
        
        # Funzionalità di cliccare su un paziente
        non_zero_values <- reactiveVal(character(0))

        observe({
          
          if (length(input$SelectColumn) > 0) {
              reshaped_data <- as.data.frame(reshaped_data)
              reshaped_data <- reshaped_data %>% select(!!!rlang::syms(input$SelectColumn))
              reshaped_data <- as.matrix(reshaped_data)
          }
          
          if (length(input$DeleteColumn) > 0) {
            reshaped_data <- as.data.frame(reshaped_data)
            reshaped_data <- reshaped_data %>%
              select(-all_of(input$DeleteColumn))
            reshaped_data <- as.matrix(reshaped_data)
          }
          
          if (length(input$DeleteRow) > 0) {
            reshaped_data <- as.data.frame(reshaped_data)
            reshaped_data <- reshaped_data %>%
              slice(-which(rownames(reshaped_data) %in% input$DeleteRow))
            reshaped_data <- as.matrix(reshaped_data)
          }
          
        
          output$dataTable <- renderDT({
              datatable(reshaped_data, options = list(scrollX = TRUE))
          })

          #rappresenta heatmap con possibilità di fare zoom
          output$heatmapPlot <- renderPlotly({
              heatmap_plot <- plot_ly(
                z = as.matrix(reshaped_data),
                x = colnames(reshaped_data),
                y = rownames(reshaped_data),
                type = "heatmap"
              ) %>%
                layout(
                  margin = list(l = 50, r = 50, b = 50, t = 50),
                  xaxis = list(side = "bottom"),
                  yaxis = list(autorange = "reversed")
                )
              
              # Zoom
              heatmap_plot <- heatmap_plot %>%
                config(displayModeBar = TRUE) %>%
                layout(dragmode = "select")
              
              return(heatmap_plot)
          })

            
          observeEvent(input$dataTable_cell_clicked, {
            info <- input$dataTable_cell_clicked
            
            
            if (!is.null(info)) {
                row_index <- info$row
                selected_genes <- names(which(reshaped_data[row_index, ] != 0))
                print(selected_genes)  
                # Declare poset_graph outside of the if block
                poset_graph <- NULL
                
                if (length(selected_genes) >= 2) {
                  sorted_genes <- names(sort(reshaped_data[row_index, selected_genes], decreasing = TRUE))
                  sorted_genes <- unlist(lapply(seq_along(sorted_genes), function(i) rep(sorted_genes[i], each = 2)))
                  sorted_genes <- sorted_genes[-c(1, length(sorted_genes))]
                  
                  poset_graph <- make_graph(edges = sorted_genes, directed = TRUE)
                  
                }
                
                # Rappresentare il grafo con igraph
                output$posetGraph <- renderPlot({
                    plot(poset_graph, layout = layout.circle)
                })
                
                output$content <- renderUI({
                  if (!is.null(poset_graph) && length(poset_graph) > 0) {
                    tagList(
                      tags$hr(),
                      plotOutput("posetGraph", width = "100%", height = "400px")
                    )
                  } 
                })
            }
          })
        })
      } else if (ncol(data) == 4){        #### Bulk multipla biopsia o single cell 
        if (colnames(data)[2]=="REGION") {
          output$dataFile2 <- renderUI({})
          output$loadBtn2 <- renderUI({})
          
          # Rimuovere duplicati mantenendo solo la prima occorrenza per ciascuna posizione
          data <- distinct(data, SAMPLE, REGION, GENE, .keep_all = TRUE)
           

          reshaped_data <- data %>%
            group_by(SAMPLE, REGION, GENE) %>%
            summarise(CCF = sum(CCF)) %>%
            unite("ID", c("SAMPLE", "REGION"), sep = " ") %>%
            pivot_wider(names_from = GENE, values_from = CCF, values_fill = 0) %>%
            select(ID, everything())

          output$SelectColumn <- renderUI({
            selectInput("SelectColumn", "Select column", choices=colnames(reshaped_data), multiple=T)
          })
          
          output$DeleteColumn <- renderUI({
            selectInput("DeleteColumn", "Delete column", choices=colnames(reshaped_data), multiple=T)
          })
          
          output$DeleteRow <- renderUI({
            selectInput("DeleteRow", "Delete row", choices=row.names(reshaped_data), multiple=T)
          })
               
          observe({
            if (length(input$SelectColumn) > 0) {
              reshaped_data <- as.data.frame(reshaped_data)
              reshaped_data <- reshaped_data %>% select(!!!rlang::syms(input$SelectColumn))
            }
            
            if (length(input$DeleteColumn) > 0) {
              reshaped_data <- as.data.frame(reshaped_data)
              reshaped_data <- reshaped_data %>%
                select(-all_of(input$DeleteColumn))
            }
            
            if (length(input$DeleteRow) > 0) {
              reshaped_data <- as.data.frame(reshaped_data)
              reshaped_data <- reshaped_data %>%
                slice(-which(rownames(reshaped_data) %in% input$DeleteRow))
            }
                 
            output$dataTable <- renderDT({
              datatable(reshaped_data, options = list(scrollX = TRUE))
            })

            
            output$heatmapPlot <- renderPlotly({
              if (ncol(data) == 4) {
                heatmap_plot <- plot_ly(
                  z = as.matrix(reshaped_data),
                  x = colnames(reshaped_data),
                  y = rownames(reshaped_data),
                  type = "heatmap"
                ) %>%
                  layout(
                    margin = list(l = 50, r = 50, b = 50, t = 50),
                    xaxis = list(side = "bottom"),
                    yaxis = list(autorange = "reversed")
                  )
                
                # zoom
                heatmap_plot <- heatmap_plot %>%
                  config(displayModeBar = TRUE) %>%
                  layout(dragmode = "select")
                
                return(heatmap_plot)
              }
            })
          })
            
          observeEvent(input$dataTable_cell_clicked, {
            info <- input$dataTable_cell_clicked
              
            if (!is.null(info) && !is.null(info$row) && !is.null(info$col)) {
              row_index <- info$row
              selected_id <- as.character(strsplit(as.character(reshaped_data[row_index, "ID"]), " ")[[1]][1])
                
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
                  plotOutput("graphPlot") 
                )
              })
                
              output$fileDataTable <- renderDT({
                datatable(file_data, options = list(scrollX = TRUE))
              })
                
              graph_data <- reshape2::melt(file_data, id.vars = "ID")
              edges <- graph_data[graph_data$value != 0, c("ID", "variable")]
                
              graph <- graph_from_data_frame(edges, directed = TRUE)
                
              output$graphPlot <- renderPlot({
                plot(graph, edge.label = edges$value, layout = layout.reingold.tilford)
              })
            }
          })
            
          output$directoryInput <- renderUI({
            shinyDirButton("dir", "Seleziona una cartella", title = "Seleziona una cartella", multiple = FALSE)
          })
            
          observe({
            req(input$dir)
            selected_folder(parseDirPath(c(wd = getwd()), input$dir))
          })
            
          shinyDirChoose(input, "dir", roots = c(wd = getwd()), filetypes = c("", "txt"))
            

        } else if (colnames(data)[2]=="CELL") {
          # Rimuovere duplicati mantenendo solo la prima occorrenza per ciascuna posizione
          data <- distinct(data, PATIENT, CELL, GENE, .keep_all = TRUE)
          

          reshaped_data <- data %>%
            group_by(PATIENT, GENE) %>%
            summarise(PERCENTAGE = mean(VALUE) * 100) %>%
            pivot_wider(names_from = GENE, values_from = PERCENTAGE, values_fill = list(PERCENTAGE = 0))
            
            output$SelectColumn <- renderUI({
              selectInput("SelectColumn", "Select column", choices=colnames(reshaped_data), multiple=T)
            })
            
            output$DeleteColumn <- renderUI({
              selectInput("DeleteColumn", "Delete column", choices=colnames(reshaped_data), multiple=T)
            })
            
            output$DeleteRow <- renderUI({
              selectInput("DeleteRow", "Delete row", choices=row.names(reshaped_data), multiple=T)
            })
              
            observe({
                
              if (length(input$SelectColumn) > 0) {
                reshaped_data <- as.data.frame(reshaped_data)
                reshaped_data <- reshaped_data %>% select(!!!rlang::syms(input$SelectColumn))
              }
              
              if (length(input$DeleteColumn) > 0) {
                reshaped_data <- as.data.frame(reshaped_data)
                reshaped_data <- reshaped_data %>%
                  select(-all_of(input$DeleteColumn))
              }
              
              if (length(input$DeleteRow) > 0) {
                reshaped_data <- as.data.frame(reshaped_data)
                reshaped_data <- reshaped_data %>%
                  slice(-which(rownames(reshaped_data) %in% input$DeleteRow))
              }
                
              output$dataTable <- renderDT({
                datatable(reshaped_data, options = list(scrollX = TRUE))
              })
            
            
              output$heatmapPlot <- renderPlotly({
                if (ncol(data) == 4) {
                  heatmap_plot <- plot_ly(
                    z = as.matrix(reshaped_data),
                    x = colnames(reshaped_data),
                    y = rownames(reshaped_data),
                    type = "heatmap"
                  ) %>%
                    layout(
                      margin = list(l = 50, r = 50, b = 50, t = 50),
                      xaxis = list(side = "bottom"),
                      yaxis = list(autorange = "reversed")
                    )
                  
                  # zoom
                  heatmap_plot <- heatmap_plot %>%
                    config(displayModeBar = TRUE) %>%
                    layout(dragmode = "select")
                  
                  return(heatmap_plot)
                }
              })
            })
            
            observeEvent(input$dataTable_cell_clicked, {
              info <- input$dataTable_cell_clicked
              
              if (!is.null(info)) {
                row_index <- info$row
                selected_id <- as.character(strsplit(as.character(reshaped_data[row_index, "PATIENT"]), " ")[[1]][1])
                
                req(input$dir)
                selected_folder <- parseDirPath(c(wd = getwd()), input$dir)
                file_to_search <- paste0(selected_id, ".txt")
                file_path <- file.path(selected_folder, file_to_search)
                
                if (file.exists(file_path)) {
                  file_data <- read.table(file_path, header = TRUE, stringsAsFactors = FALSE, sep = "\t")
                  
                  col_names <- colnames(file_data)
                  file_data$PATIENT <- col_names
                  file_data <- file_data[, c("PATIENT", setdiff(col_names, "PATIENT"))]
                  
                  output$content <- renderUI({
                    tagList(
                      tags$hr(),
                      plotOutput("graphPlot") 
                    )
                  })
                  
                  output$fileDataTable <- renderDT({
                    datatable(file_data, options = list(scrollX = TRUE))
                  })
                  
                  graph_data <- reshape2::melt(file_data, id.vars = "PATIENT")
                  edges <- graph_data[graph_data$value != 0, c("PATIENT", "variable")]
                  
                  graph <- graph_from_data_frame(edges, directed = TRUE)
                  
                  output$graphPlot <- renderPlot({
                    plot(graph, edge.label = edges$value, layout = layout.reingold.tilford)
                  })
                } else {
                  # File non trovato, gestisci qui
                  showNotification("File non trovato.", type = "error")
                }
              }
            })
            
            
            output$directoryInput <- renderUI({
              shinyDirButton("dir", "Seleziona una cartella", title = "Seleziona una cartella", multiple = FALSE)
            })
            
            observe({
              req(input$dir)
              selected_folder(parseDirPath(c(wd = getwd()), input$dir))
            })
            
            shinyDirChoose(input, "dir", roots = c(wd = getwd()), filetypes = c("", "txt"))

        }
        else {
          showNotification("File non riconosciuto. Assicurati che il nome delle colonne sia corretto.", type = "error")
        }
      }
      else {  #### File errato
        showNotification("File non riconosciuto. Assicurati che il numero di colonne sia corretto.", type = "error")
      }
    }
  })
  
  
  
  
  ######################################################### FASE INFERENZA  #########################################################  
  
  result <- reactiveVal(NULL)
  
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
        
        resExampleSingleSamplesResampling <- asceticCCFResampling(
          dataset = environments$data_1,
          ccfDataset = environments$data_2,
          vafDataset = environments$data_3,
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
          models = environments$data_2,
          nsampling = nsampling_ph,
          regularization = input$regularization_phylo,
          command = input$command_phylo,
          restarts = input$restarts_phylo
        )
      } else {
        resExamplePhylogeniesDataset <- asceticPhylogenies(
          dataset = environments$data_1,
          models = environments$data_2,
          regularization = input$regularization_phylo,
          command = input$command_phylo,
          restarts = input$restarts_phylo
        )
      }
    }
    result(resExamplePhylogeniesDataset)
  })
}
