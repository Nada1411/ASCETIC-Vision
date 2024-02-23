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
library(tibble)
library(shinyWidgets)

server <- function(input, output, session) {
  
  ######################################################### Variables  #########################################################

  reshaped_data_resampling <- reactiveVal(NULL)
  selected_folder <- reactiveVal(NULL)
  directory_output <- reactiveVal(NULL)
  reshaped_data <- reactiveVal(NULL)
  genotype_table <- reactiveVal(NULL)
  case <- reactiveVal(NULL)
  directory <- reactiveVal(NULL)
  calculationInProgress <- reactiveVal(NULL)
  

  ######################################################### Function  #########################################################
  
  #funzione che reimposta tutti i valori nulli in una pagina
  default_values <- function() {
    output$directoryInput <- renderUI({})
    output$binarization_perc <- renderUI({})
    output$binarization <- renderUI({})
    output$DeleteRow <- renderUI({})
    output$DeleteColumn <- renderUI({})
    output$dataFile2 <- renderUI({})
    output$dataFile <- renderUI({})
    output$switchViewBtn <- renderUI({})
    output$heatmapPlot <- renderUI({})
    output$loadBtn2 <- renderUI({})
    output$dataTable <- renderUI({})
    output$dataTable2 <- renderUI({})
  }
  
  
  # returns the list of project names in the output_project folder
  get_project_names <- function() {
    project_names <- list.files("output_project")
    project_names <- data.frame(project_names)
    return(project_names)
  }
  
  # generate heatmap
  generate_heatmap_plot <- function(data) {
    heatmap_plot <- plot_ly(
      z = as.matrix(data),
      x = colnames(data),
      y = rownames(data),
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
  }
  
  # Management click on genotype table and visualization of POSET
  observe_table_cell_clicked <- function(reshaped_data) {
    observeEvent(input$dataTable_cell_clicked, {
      info <- input$dataTable_cell_clicked
      
      if (!is.null(info)) {
        row_index <- info$row
        selected_genes <- names(which(reshaped_data[row_index, ] != 0))
        poset_graph <- NULL
        
        if (length(selected_genes) >= 2) {
          sorted_genes <- names(sort(reshaped_data[row_index, selected_genes], decreasing = TRUE))
          sorted_genes <- unlist(lapply(seq_along(sorted_genes), function(i) rep(sorted_genes[i], each = 2)))
          sorted_genes <- sorted_genes[-c(1, length(sorted_genes))]
          
          poset_graph <- make_graph(edges = sorted_genes, directed = TRUE)
        }
        
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
  }
  
  # selection/deletion of row and column
  modify_reshaped_data <- function(reshaped_data) {
    
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
    
    return(reshaped_data)
  }
  
  observe_data_modification <- function(reshaped_data) {
    observe({
      reshaped_data <- modify_reshaped_data(reshaped_data)
      
      output$dataTable <- renderDT({
        datatable(reshaped_data, options = list(scrollX = TRUE))
      })
      
      reshaped_data(reshaped_data)
      
      output$heatmapPlot <- renderPlotly({
        generate_heatmap_plot(reshaped_data)
      })
      
      observe_table_cell_clicked(reshaped_data)

      reshaped_data(reshaped_data)
    })
  }
  
  # manage click on genotype table in bulk multi region and single cell
  handle_dataTable_cell_clicked <- function(reshaped_data, id_column_name) {
    observeEvent(input$dataTable_cell_clicked, {
      info <- input$dataTable_cell_clicked
      
      if (!is.null(info) && !is.null(info$row) && !is.null(info$col)) {
        row_index <- info$row
        selected_id <- strsplit((rownames(reshaped_data)[row_index]), " ")[[1]][1]

        req(input$dir)
        selected_folder <- parseDirPath(c(wd = getwd()), input$dir)
        selected_id <- sub("\\s.*", "", selected_id)
        file_to_search <- paste0(selected_id, ".txt")
        file_path <- file.path(selected_folder, file_to_search)
        file_data <- read.table(file_path, header = TRUE, stringsAsFactors = FALSE, sep = "\t")

        
        col_names <- colnames(file_data)
        
        file_data[[id_column_name]] <- col_names
        
        file_data <- file_data[, c(id_column_name, setdiff(col_names, id_column_name))]
        
        if (length(input$DeleteColumn) > 0){
          for (col in input$DeleteColumn) {
            if(col %in% colnames(file_data)) {
            col_index <- which(colnames(file_data) == col)
            # nodo foglia
              if ((1 %in% file_data[[col_index]]) && (all(as.numeric(file_data[col_index-1, -1]) == 0))) {
                file_data[[col_index]][file_data[[col_index]] == 1] <- 0
              }
              
              #nodo radice
              if (!(1 %in% file_data[[col_index]]) && (any(as.numeric(file_data[col_index-1, -1]) == 1))) {
                file_data[col_index - 1, -1][file_data[col_index - 1, -1] == 1] <- 0
              }
              
              #nodo interno
              if ((1 %in% file_data[[col_index]]) && (any(as.numeric(file_data[col_index-1, -1]) == 1))){
                indici_riga <- which(file_data[col_index-1, -1] == 1)
                
                indici_colonna <- which(file_data[[col_index]] == 1)
                
                for (colonna in indici_colonna) {
                  for (riga in indici_riga) {
                    file_data[colonna, riga+1] <- 1
                  }
                }
                file_data[[col_index]][file_data[[col_index]] == 1] <- 0
                file_data[col_index - 1, -1][file_data[col_index - 1, -1] == 1] <- 0
              }
            }
          }
}

        output$content <- renderUI({
          tagList(
            tags$hr(),
            plotOutput("graphPlot") 
          )
        })
        
        output$fileDataTable <- renderDT({
          datatable(file_data, options = list(scrollX = TRUE))
        })
        
        graph_data <- reshape2::melt(file_data, id.vars = id_column_name)
        edges <- graph_data[graph_data$value != 0, c(id_column_name, "variable")]
        
        graph <- graph_from_data_frame(edges, directed = TRUE)
        
        output$graphPlot <- renderPlot({
          plot(graph, edge.label = edges$value, layout = layout.reingold.tilford)
        })
        }
    })
  }
  
  readMatrixFiles <- function(directory_path) {
    
    readMatrix <- function(filePath) {
      # Leggi i dati dal file
      matrix_data <- read.table(filePath, header = TRUE, stringsAsFactors = FALSE, sep = "\t")
      
      # Converti i dati in matrice di tipo double
      matrix_data <- as.matrix(matrix_data)
      
      # Imposta i nomi di righe e colonne uguali
      rownames(matrix_data) <- colnames(matrix_data)
      
      return(matrix_data)
    }
    
    files <- list.files(directory_path, full.names = TRUE)
    
    matrix_list <- list()
    
    # Itera su ogni file
    for (file in files) {
      matrix_name <- tools::file_path_sans_ext(basename(file))
      
      # Leggi la matrice dal file e convertila in double
      matrix_data <- readMatrix(file)
      
      # Aggiungi la matrice alla lista con il nome come nome di lista
      matrix_list[[matrix_name]] <- matrix_data
    }
    
    return(matrix_list)
  }
  
  # Function for the interrupt key in the inference phase
  interrupt <- function(calculationInProgress) {
    if (is.null(calculationInProgress)) {
      output$interruptButton <- renderUI({
        actionButton("interruptBtn", "Interrupt calcolo")
      })
      output$spinner <- renderUI({
        plotOutput("Test") %>% withSpinner(color="#0dc5c1")
      })
    }
    else  {
      output$interruptButton <- renderUI({})
      output$spinner <- renderUI({})
    }
  }
  
  
  
  ######################################################### Load or create project  #########################################################
  
  
  # Function to change the tabPanel to "Data Input" when the "Create New Project" button is clicked
  observeEvent(input$create_project_button, {
    updateTabsetPanel(session, "main_tabset", selected = "Input dati")
  })
  
  # Show project names in "output_project" folder
  project_names <- reactive({
    get_project_names()
  })
  
  
  # Show the table with the names of existing projects
  output$projectList <- renderDT({
    datatable(project_names(), rownames= FALSE)
    
  })
  
  observeEvent(input$loadProjBtn, {
    
    if (is.null(input$projectList_cell_clicked$row) || is.null(input$projectList_cell_clicked$col)) {
      showNotification("Select the project you want to upload", type = "error")
      
    } else {
      # Management click on existing project table
      observeEvent(input$projectList_cell_clicked, {
        if (!is.null(input$projectList_cell_clicked)) {
          clicked_row <- input$projectList_cell_clicked$row
          clicked_col <- input$projectList_cell_clicked$col
          
          project_name <- project_names()[clicked_row, 1]
          project_folder <- file.path("output_project", project_name)
          project_files <- list.files(project_folder)
          
          for (file in project_files) {
            file_name <- tools::file_path_sans_ext(file)
            if (file_name == "genotipo") {
              file_directory <- file.path(project_folder, file)
              data <- read.csv(file_directory)
              output$dataTable <- renderDT({
                datatable(data, options = list(scrollX = TRUE))
              })
              
              output$heatmapPlot <- renderPlotly({
                generate_heatmap_plot(data)
              })
            }
          }
        }
      })
      
      updateTabsetPanel(session, "main_tabset", selected = "Input dati")
    }
  })


  
  # Function to render UI for selecting columns
  render_select_column_ui <- function(input_id, label, data) {
    output <- renderUI({
      selectInput(input_id, label, choices = colnames(data), multiple = TRUE)
    })
    return(output)
  }
  
  # Function to render UI for deleting columns
  render_delete_column_ui <- function(input_id, label, data) {
    output <- renderUI({
      selectInput(input_id, label, choices = colnames(data), multiple = TRUE)
    })
    return(output)
  }
  
  # Function to render UI for deleting rows
  render_delete_row_ui <- function(input_id, label, data) {
    output <- renderUI({
      selectInput(input_id, label, choices = rownames(data), multiple = TRUE)
    })
    return(output)
  }
  
  

######################################################### Input data  #########################################################
  
  
  # Displaying the resampling file, in the case where there are three columns (single bulk case) the table is represented.
  # in other cases an error message is printed
  observeEvent(input$loadBtn2, {
    #resampling file
    inFile2 <- input$dataFile2
    data2 <- read.table(inFile2$datapath, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
    #genotype file
    inFile <- input$dataFile
    data <- read.table(inFile$datapath, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
    reshaped_data_resampling(data2)
    if (ncol(data) == 3) {
      output$dataTable2 <- renderDT({
        datatable(data2, options = list(scrollX = TRUE))
      })
    } else {
      showNotification("Resampling file not necessary", type = "error")
    }
  })
  
  reshaped_data <- reactiveVal(NULL)
  
  observeEvent(input$loadBtn, {
    default_values()
    inFile <- input$dataFile
    
    if (is.null(inFile)) {
      showNotification("File non caricato.", type = "error")
    } else {
      data <- read.table(inFile$datapath, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
      
      #### Bulk single biopsy
      if (ncol(data) == 3) {
        case("bulk single")
        
        # possibility of loading resampling
        output$dataFile2 <- renderUI({
          fileInput("dataFile2", "Resampling")
        })
        output$loadBtn2 <- renderUI({
          actionButton("loadBtn2", "Load")
        })
        
        data <- distinct(data, SAMPLE, GENE, .keep_all = TRUE)
        reshaped_data(
          acast(data, SAMPLE ~ GENE, value.var = "CCF", fill = 0)
        )
        
        output$DeleteColumn <- render_delete_column_ui("DeleteColumn", "Delete column", reshaped_data())
        output$DeleteRow <- render_delete_row_ui("DeleteRow", "Delete row", reshaped_data())
        
        # Management deletion or selection of rows and columns
        observe_data_modification(reshaped_data())
        
      } else if (ncol(data) == 4) {        #### Bulk multipla biopsia o single cell 
        if (colnames(data)[2]=="REGION") {
          default_values()
          case("bulk multiple")
          # Remove duplicates by keeping only the first occurrence for each position
          data <- distinct(data, SAMPLE, REGION, GENE, .keep_all = TRUE)
          
          reshaped_data(
            data %>%
              group_by(SAMPLE, REGION, GENE) %>%
              summarise(CCF = sum(CCF)) %>%
              unite("ID", c("SAMPLE", "REGION"), sep = "\t") %>%
              pivot_wider(names_from = GENE, values_from = CCF, values_fill = 0) %>%
              select(-ID, everything()) %>%
              column_to_rownames(var = "ID")
          )
          
          output$DeleteColumn <- render_delete_column_ui("DeleteColumn", "Delete column", reshaped_data())
          output$DeleteRow <- render_delete_row_ui("DeleteRow", "Delete row", reshaped_data())
          
          observe({
            modified_data <- modify_reshaped_data(reshaped_data())
            
            output$dataTable <- renderDT({
              datatable(modified_data, options = list(scrollX = TRUE))
            })
            
            reshaped_data(modified_data)
            
            output$heatmapPlot <- renderPlotly({
              generate_heatmap_plot(modified_data)
            })
          })
          
          output$binarization_perc <- renderUI({
            numericInput("binarization_perc", "Filtro per binarizzare percentuale", value = 1, min = 0, max = 1, step = 0.01)
          })
          
          handle_dataTable_cell_clicked(reshaped_data(), "ID")
          
          output$directoryInput <- renderUI({
            shinyDirButton("dir", "Seleziona una cartella", title = "Seleziona una cartella", multiple = FALSE)
          })
          
          observe({
            req(input$dir)
            selected_folder(parseDirPath(c(wd = getwd()), input$dir))
          })
          
          shinyDirChoose(input, "dir", roots = c(wd = getwd()), filetypes = c("", "txt"))
          
          
        } else if (colnames(data)[2]=="CELL") {
          case("single cell")
          # Remove duplicates by keeping only the first occurrence for each position
          data <- distinct(data, PATIENT, CELL, GENE, .keep_all = TRUE)
          
          data <- data %>%
              group_by(PATIENT, GENE) %>%
              summarise(PERCENTAGE = mean(VALUE)) %>%
              pivot_wider(names_from = GENE, values_from = PERCENTAGE, values_fill = list(PERCENTAGE = 0))
          
          reshaped_data(data %>%
            column_to_rownames(var = "PATIENT"))

          
          output$DeleteColumn <- render_delete_column_ui("DeleteColumn", "Delete column", reshaped_data())
          output$DeleteRow <- render_delete_row_ui("DeleteRow", "Delete row", reshaped_data())
          
          observe({
            
            modified_data <- modify_reshaped_data(reshaped_data())
            

            output$dataTable <- renderDT({
              datatable(modified_data, options = list(scrollX = TRUE))
            })
            
            reshaped_data(modified_data)
            
            output$heatmapPlot <- renderPlotly({
              generate_heatmap_plot(reshaped_data())
            })
            
          })
          
          handle_dataTable_cell_clicked(reshaped_data(), "PATIENT")
          
          output$directoryInput <- renderUI({
            shinyDirButton("dir", "Select a folder", title = "Select a folder", multiple = FALSE)
          })
          
          
          observe({
            req(input$dir)
            selected_folder(parseDirPath(c(wd = getwd()), input$dir))
          })
          
          shinyDirChoose(input, "dir", roots = c(wd = getwd()), filetypes = c("", "txt"))
          
        } else {
          showNotification("File not recognized. Make sure the column names are correct.", type = "error")
        }
      } else {  #### Wrong file
        showNotification("File not recognized. Make sure the number of columns is correct.", type = "error")
      }
    }
  })
  
  ######################################################### Inference  #########################################################
  
  # filter the genotype table according to the case
  observeEvent(input$submitBtn, {
    filter <- input$binarization
    filter_perc <- input$binarization_perc
    genotype_table(reshaped_data())

    if (is.null(case())) {
      showNotification("Load the files in the previous step", type = "error")
    }
    else if (case() == "bulk single" || case() == "single cell") {
      #filter the genotype table
      genotype_table(ifelse(genotype_table() >= filter, 1, 0))
    }
    else if (case() == "bulk multiple") {
      genotype_table(ifelse(genotype_table() >= filter, 1, 0))
      
      df <- as.data.frame(genotype_table())

      df <- df %>%
        mutate(id = sapply(strsplit(row.names(df), "\t"), `[`, 1))

      result <- df %>%
        group_by(id) %>%
        summarize(across(everything(), ~mean(. != 0)))
      
      result <- result %>%
        column_to_rownames(var = "id")

      genotype_table(ifelse(result >= filter_perc, 1, 0))

    }
    updateTabsetPanel(session, "main_tabset", selected = "Inference")
  })
  
  # Callback function to handle click on input resamplingFlag
  observeEvent(input$resamplingFlag, {
    # Check whether the resampling flag has been activated.
    if (input$resamplingFlag) {
      # if files were not uploaded in the previous case it gives error
      if (is.null(case())) {
        updateCheckboxInput(session, "resamplingFlag", value = FALSE)
        showNotification("Load the files in the previous step", type = "error")
      }
    #otherwise we evaluate the case, if we are in the first check that the resampling file was loaded in the previous step
     else if (case() == "bulk single") {
        if (is.null(input$dataFile2)) {
          # If the file has not been uploaded, turn off the flag and show an error message
          updateCheckboxInput(session, "resamplingFlag", value = FALSE)
          showNotification("Load the resampling file in the previous step", type = "error")
        }
      }
    }
  })
  
  
  observeEvent(input$interruptButton, {
    progress$interrupt()
  })
  
  
  observeEvent(input$submitBtn, {
    
    
    nsampling <- input$nresampling

    set.seed(input$seed)
    
    if (is.null(case())) {
      showNotification("Load the files in the previous step", type = "error")
    }
    else if(case()=="bulk single") {
      if (input$resamplingFlag == FALSE) {
        
        interrupt(calculationInProgress())
        
        progress <- withProgress(
          message = 'Ongoing calculation...',
          detail = 'This may take some time...',
          value = 0, {
            resExampleSingleSamples <- asceticCCF(
              dataset = genotype_table(),
              ccfDataset = reshaped_data(),
              regularization = input$regularization,
              command = input$command, 
              restarts = input$restarts
            )
          }
        )
        
        calculationInProgress(progress)
        
        interrupt(calculationInProgress())
      }
      else {
        interrupt(calculationInProgress())
        
        progress <- withProgress(
          message = 'Ongoing calculation...',
          detail = 'This may take some time...',
          value = 0, {
            resExampleSingleSamplesResampling <- asceticCCFResampling(
              dataset = genotype_table(),
              ccfDataset = reshaped_data(),
              vafDataset = reshaped_data_resampling(),
              nsampling = nsampling,
              regularization = input$regularization,
              command = input$command, 
              restarts = input$restarts
            )
          }
        )
        
        calculationInProgress(progress)
        
        interrupt(calculationInProgress())
      }
    }
    else if(case()=="bulk multiple" || case() == "single cell") {
      
      
      if (is.null(selected_folder())) {
        showNotification("Select the folder in the previous step", type = "error")
      }
      else if (input$resamplingFlag == FALSE) {
        models <- readMatrixFiles(selected_folder())
        interrupt(calculationInProgress())
        
        progress <- withProgress(
          message = 'Ongoing calculation...',
          detail = 'This may take some time...',
          value = 0, {
            resExamplePhylogeniesDataset <- asceticPhylogenies(
              dataset = genotype_table(),
              models = models,
              regularization = input$regularization,
              command = input$command,
              restarts = input$restarts
            )
          }
        )
        
        calculationInProgress(progress)
        
        interrupt(calculationInProgress())
        
      }
      else {
        models <- readMatrixFiles(selected_folder())
        interrupt(calculationInProgress())
        
        progress <- withProgress(
          message = 'Ongoing calculation...',
          detail = 'This may take some time...',
          value = 0, {
            resExamplePhylogeniesDataset <- asceticPhylogeniesBootstrap(
              dataset = genotype_table(),
              models = models,
              nsampling = nsampling,
              regularization = input$regularization,
              command = input$command,
              restarts = input$restarts
            )
          }
        )
        
        calculationInProgress(progress)
        
        interrupt(calculationInProgress())
      }
    }

  })
  
  ######################################################### FASE SALVATAGGIO  #########################################################  

  saveData <- function(data, nome) {
    write.csv(data, nome, row.names=FALSE)
  }
  
  
  observe({
    if (input$project_name != "") {
      shinyjs::enable("saveBtn")
    } else {
      shinyjs::disable("saveBtn")
    }
  })
  
  observeEvent(input$saveBtn, {
    nome_progetto <- input$project_name
    directory_output <- "output_project/"
    directory_completo <- paste0(directory_output, nome_progetto)
    
    # Verifica se la directory esiste, altrimenti creala
    if (!file.exists(directory_completo)) {
      dir.create(directory_completo, recursive = TRUE)
    }
    
    directory_file <- paste0(directory_completo, "/genotipo.csv")
    data <- reshaped_data()  # Ottieni i dati del dataTable dalla variabile reattiva
    saveData(data, directory_file)  # Salva i dati
  })

}
