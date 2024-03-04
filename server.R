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
library(stringr)

server <- function(input, output, session) {
  
  ######################################################### Variables  #########################################################
  
  resampling_data <- reactiveVal(NULL)
  selected_folder <- reactiveVal(NULL)
  directory_output <- reactiveVal(NULL)
  reshaped_data <- reactiveVal(NULL)
  genotype_table <- reactiveVal(NULL)
  case <- reactiveVal(NULL)
  calculationInProgress <- reactiveVal(NULL)
  resampling_res <- reactiveVal(NULL)

  ######################################################### Function  #########################################################
  
  # Function that resets all default values when loading a new "genotype" table
  default_values_load_genotype <- function() {
    output$directoryInput <- renderUI(NULL)
    output$binarization_perc <- renderUI(NULL)
    output$binarization <- renderUI(NULL)
    output$DeleteRow <- renderUI(NULL)
    output$DeleteColumn <- renderUI(NULL)
    output$dataFile2 <- renderUI(NULL)
    output$loadBtn2 <- renderUI(NULL)
    output$dataTable <- renderDataTable(NULL)
    output$dataTable2 <- renderDataTable(NULL)
    output$heatmapPlot <- plotly::renderPlotly(NULL)
    output$selected_result_output <- renderDataTable(NULL)
    output$graph_inference <- renderDataTable(NULL)
    updateCheckboxInput(session, "resamplingFlag", value = FALSE)
    updateNumericInput(session, "nresampling", value = 3)
    updateSelectInput(session, "regularization", selected = "aic")
    updateSelectInput(session, "command", selected = "hc")
    updateNumericInput(session, "restarts", value = 10)
    updateNumericInput(session, "seed", value = 12345)
    output$visualize_inference <- NULL
  }
  
  # Function that resets all default values when starting a new project
  default_values_create_project <- function() {
    default_values_load_genotype()
    output$switchViewBtn <- renderUI(NULL)
  }
  
  ### Load and create project functions
  
  # Returns the list of project names in the output_project folder
  get_project_names <- function() {
    project_names <- list.files("output_project")
    project_names <- data.frame(project_names)
    return(project_names)
  }
  
  
  ###Input dati functions
  
  # Generate heatmap
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
  
  # Management click on genotype table and visualization of POSET -bulk_single case-
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
  
 
  # Selection/deletion of row and column
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
  
  # Manage bulk_single case
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
  
  # Manage click on genotype table in bulk multi region and single cell
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
              
              # nodo radice
              if (!(1 %in% file_data[[col_index]]) && (any(as.numeric(file_data[col_index-1, -1]) == 1))) {
                file_data[col_index - 1, -1][file_data[col_index - 1, -1] == 1] <- 0
              }
              
              # nodo interno
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
  
  
  # Manage bulk_single case
  bulk_single_case <- function() {
    output$dataFile2 <- renderUI({
      fileInput("dataFile2", "Resampling")
    })
    output$loadBtn2 <- renderUI({
      actionButton("loadBtn2", "Load")
    })
    
    output$DeleteColumn <- render_delete_column_ui("DeleteColumn", "Delete column", reshaped_data())
    output$DeleteRow <- render_delete_row_ui("DeleteRow", "Delete row", reshaped_data())
    
    # Management deletion or selection of rows and columns
    observe_data_modification(reshaped_data())
  }
  
  # Manage bulk_multiple and single cell_case 
  bulk_multiple_single_cell_case <- function(case) {
    
    output$directoryInput <- renderUI({
      shinyDirButton("dir", "Select a folder", title = "Select a folder", multiple = FALSE)
    })
    
    modified_data <- reshaped_data()
    
    output$DeleteColumn <- render_delete_column_ui("DeleteColumn", "Delete column", reshaped_data())
    output$DeleteRow <- render_delete_row_ui("DeleteRow", "Delete row", reshaped_data())
    
    observe({
      
      modified_data <- modify_reshaped_data(modified_data)
      
      output$dataTable <- renderDT({
        datatable(modified_data, options = list(scrollX = TRUE))
      })
      
      reshaped_data(modified_data)
      
      output$heatmapPlot <- renderPlotly({
        generate_heatmap_plot(modified_data)
      })
    })
    
    
    handle_dataTable_cell_clicked(reshaped_data(), case)
    
    observe({
      req(input$dir)
      selected_folder(parseDirPath(c(wd = getwd()), input$dir))
    })
    
    shinyDirChoose(input, "dir", roots = c(wd = getwd()), filetypes = c("", "txt"))
  }
  
  
  ## Inference functions
  
  # Create models for bulk multiple and single cell case
  readMatrixFiles <- function(directory_path) {
    
    readMatrix <- function(filePath) {
      matrix_data <- read.table(filePath, header = TRUE, stringsAsFactors = FALSE, sep = "\t")
      matrix_data <- as.matrix(matrix_data)
      rownames(matrix_data) <- colnames(matrix_data)
      return(matrix_data)
    }
    
    files <- list.files(directory_path, full.names = TRUE)
    
    matrix_list <- list()
    
    for (file in files) {
      matrix_name <- tools::file_path_sans_ext(basename(file))
      matrix_data <- readMatrix(file)
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
  
  
  ## Rescue phase functions
  saveData <- function(data, name) {
    write.csv(data, name, row.names=TRUE)
  }
  
  ######################################################### Load or create project  #########################################################
  
  ##Create
  
  # Function to change the tabPanel to "Data Input" when the "Create New Project" button is clicked
  observeEvent(input$create_project_button, {
    default_values_create_project()
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
  
  
  ## Load
  
  # Managing the loading of an existing project
  observeEvent(input$loadProjBtn, {
    default_values_create_project()
    if (is.null(input$projectList_cell_clicked$row) || is.null(input$projectList_cell_clicked$col)) {
      showNotification("Select the project you want to upload", type = "error")
      
    } else {
      
      #Visualise switch bottom
      output$switchViewBtn <- renderUI({
        actionButton("switchViewBtn", "Switch View")
      })
      
      observeEvent(input$projectList_cell_clicked, {
        if (!is.null(input$projectList_cell_clicked)) {
          clicked_row <- input$projectList_cell_clicked$row
          clicked_col <- input$projectList_cell_clicked$col
          
          project_name <- project_names()[clicked_row, 1]
          
          # Set the case 
          parti <- unlist(strsplit(project_name, "_"))
          ultime_due_parole <- tail(parti, 2)
          nuova_stringa <- paste(ultime_due_parole, collapse = "_")
          case(nuova_stringa)
          
          
          project_folder <- file.path("output_project", project_name)
          project_files <- list.files(project_folder)
          
          
          #Read first the file of each case
          for (file in project_files) {
            file_name <- tools::file_path_sans_ext(file)
            file_directory <- file.path(project_folder, file)
            
            if (file_name == "parameters_single_cell"){
              parameters <- read.csv(file_directory)
              directory <- parameters$value[parameters$name == "directory"]
              updateShinyDirButton(session, "dir", value = directory)
            }
            else if (file_name == "resampling_table") {
              data2 <- read.csv(file_directory)
              output$dataTable2 <- renderDT({
                datatable(data2, options = list(scrollX = TRUE))
              })
              resampling_data(data2)
            }
            else if (file_name == "parameters_bulk_multiple") {
              parameters <- read.csv(file_directory)
              val_bin_perc <- as.numeric(parameters[parameters$name == "binarization_perc", "value"])
              output$binarization_perc <- renderUI({
                numericInput("binarization_perc", "Filter to binarize percentage", value = val_bin_perc, min = 0, max = 1, step = 0.01)
              })
            }
          }
          
          
          # Reading the remaining files
          for (file in project_files) {
            file_name <- tools::file_path_sans_ext(file)
            file_directory <- file.path(project_folder, file)
            
            if (file_name == "genotype") {
              data <- read.csv(file_directory, row.names = 1)
              data <- as.matrix(data)
              reshaped_data(data)
              
              output$dataTable <- renderDT({
                datatable(data, options = list(scrollX = TRUE))
              })
              
              output$heatmapPlot <- renderPlotly({
                generate_heatmap_plot(data)
              })


              if(case() == "bulk_single") {
                bulk_single_case()
              }
              else if(case() == "bulk_multiple") {
                bulk_multiple_single_cell_case("ID")
              }
              else if(case() == "single_cell"){
                bulk_multiple_single_cell_case("PATIENT")
              }
            }
            else if (file_name == "parameters") {
              parameters <- read.csv(file_directory)

              for (i in 1:nrow(parameters)) {
                parametro <- parameters[i, "name"]
                value <- parameters[i, "value"]

                if (parametro == "binarization") {
                  val_bin <- as.numeric(value)
                  output$binarization <- renderUI({
                    numericInput("binarization", "Filter to binarize", value = val_bin, min = 0, max = 1, step = 0.01)
                  })
                } else if (parametro == "del_col") {
                  output$DeleteColumn <- render_delete_column_ui("DeleteColumn", "Delete column", reshaped_data())
                } else if (parametro == "del_row") {
                  output$DeleteRow <- render_delete_row_ui("DeleteRow", "Delete row", reshaped_data())
                } else if (parametro == "flag_resampling") {
                  updateCheckboxInput(session, "resamplingFlag", value = value)
                } else if (parametro == "nresampling") {
                  updateNumericInput(session, "nresampling", value = as.numeric(value))
                } else if (parametro == "restarts") {
                  updateNumericInput(session, "restarts", value = as.numeric(value))
                } else if (parametro == "regularization") {
                  elements <- unlist(strsplit(value, ", "))
                  updateSelectInput(session, "regularization", selected = c(elements))
                } else if (parametro == "command") {
                  updateSelectInput(session, "command", selected = value)
                } else if (parametro == "seed") {
                  val_seed <- as.numeric(value)
                  updateNumericInput(session, "seed", value = val_seed)
                }
              }
              
            }
            else if (file_name == "resampling_res") {
              resampling_res(readRDS(file_directory))
              res <- resampling_res()
              names_to_remove <- c("dataset", "models", "ccfDataset", "inference")
              names <- setdiff(names(res), names_to_remove)
              names <- c(names, names(res$inference))
              output$visualize_inference <- renderUI({selectInput("visualize_inference", "Output inference", c(names))})
            }
          }
        }    
        
        })
      updateTabsetPanel(session, "main_tabset", selected = "Input dati")
    }
  })

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
    resampling_data(data2)
    if (ncol(data) == 3) {
      output$dataTable2 <- renderDT({
        datatable(data2, options = list(scrollX = TRUE))
      })
    } else {
      showNotification("Resampling file not necessary", type = "error")
    }
    resampling_data(data2)
  })
  
  reshaped_data <- reactiveVal(NULL)
  
  observeEvent(input$loadBtn, {
    inFile <- input$dataFile
    
    if (is.null(inFile)) {
      showNotification("File non caricato.", type = "error")
    } else {
      data <- read.table(inFile$datapath, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
      
      
      #### Bulk single biopsy
      if (ncol(data) == 3) {
        default_values_load_genotype()
        case("bulk_single")
        
        data <- distinct(data, SAMPLE, GENE, .keep_all = TRUE)
        reshaped_data(
          acast(data, SAMPLE ~ GENE, value.var = "CCF", fill = 0)
        )
        

        output$binarization <- renderUI({
          numericInput("binarization", "Filter to binarize", value = 1, min = 0, max = 1, step = 0.01)
        })
        output$switchViewBtn <- renderUI({
          actionButton("switchViewBtn", "Switch View")
        })
        
        bulk_single_case()

        
      } else if (ncol(data) == 4) {        #### Bulk multipla biopsia o single cell 
        if (colnames(data)[2]=="REGION") {
          default_values_load_genotype()

          case("bulk_multiple")
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
          
          
          
          output$binarization <- renderUI({
            numericInput("binarization", "Filter to binarize", value = 1, min = 0, max = 1, step = 0.01)
          })
          
          output$switchViewBtn <- renderUI({
            actionButton("switchViewBtn", "Switch View")
          })
          
          output$binarization_perc <- renderUI({
            numericInput("binarization_perc", "Filter to binarize percentage", value = 1, min = 0, max = 1, step = 0.01)
          })
          
          bulk_multiple_single_cell_case("ID")
          
          
        } else if (colnames(data)[2]=="CELL") {
          default_values_load_genotype()
          case("single_cell")
          # Remove duplicates by keeping only the first occurrence for each position
          data <- distinct(data, PATIENT, CELL, GENE, .keep_all = TRUE)
          
          data <- data %>%
              group_by(PATIENT, GENE) %>%
              summarise(PERCENTAGE = mean(VALUE)) %>%
              pivot_wider(names_from = GENE, values_from = PERCENTAGE, values_fill = list(PERCENTAGE = 0))
          
          reshaped_data(data %>%
            column_to_rownames(var = "PATIENT"))
          
          output$binarization <- renderUI({
            numericInput("binarization", "Filter to binarize", value = 1, min = 0, max = 1, step = 0.01)
          })
          output$switchViewBtn <- renderUI({
            actionButton("switchViewBtn", "Switch View")
          })
          
          bulk_multiple_single_cell_case("PATIENT")
          
        } else {
          showNotification("File not recognized. Make sure the column names are correct.", type = "error")
        }
      } else {  #### Wrong file
        showNotification("File not recognized. Make sure the number of columns is correct.", type = "error")
      }
    }
  })

  
  ######################################################### Inference  #########################################################
  
  # Filter the genotype table according to the case
  observeEvent(input$submitBtn, {
    filter <- input$binarization
    filter_perc <- input$binarization_perc
    genotype_table(reshaped_data())

    if (is.null(case())) {
      showNotification("Load the files in the previous step", type = "error")
    }
    else if (case() == "bulk_single" || case() == "single_cell") {
      # Filter the genotype table
      genotype_table(ifelse(genotype_table() >= filter, 1, 0))
    }
    else if (case() == "bulk_multiple") {
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
      # If files were not uploaded in the previous case it gives error
      if (is.null(case())) {
        updateCheckboxInput(session, "resamplingFlag", value = FALSE)
        showNotification("Load the files in the previous step", type = "error")
      }
    # Otherwise we evaluate the case, if we are in the first check that the resampling file was loaded in the previous step
     else if (case() == "bulk_single") {
        if (is.null(input$dataTable2) & is.null(resampling_data())) {
          # If the file has not been uploaded, turn off the flag and show an error message
          updateCheckboxInput(session, "resamplingFlag", value = FALSE)
          showNotification("Load the resampling file in the previous step", type = "error")
        }
     }
      else {
        if(is.integer(input$dir)) {
          updateCheckboxInput(session, "resamplingFlag", value = FALSE)
          showNotification("Select the in the previous step", type = "error")
        }
      }
    }
    else {
      output$nresampling <- renderUI({numericInput("nresampling", "Number of samplings", 3, min = 3)})
    }
  })
  
  
  observeEvent(input$interruptButton, {
    progress$interrupt()
  })
  
  res <- NULL
  
  # Inference
  observeEvent(input$submitBtn, {
    
    
    nsampling <- input$nresampling

    set.seed(input$seed)
    
    if (is.null(case())) {
      showNotification("Load the files in the previous step", type = "error")
    }
    else if(case()=="bulk_single") {
      if (input$resamplingFlag == FALSE) {
        
        interrupt(calculationInProgress())
        
        progress <- withProgress(
          message = 'Ongoing calculation...',
          detail = 'This may take some time...',
          value = 0, {
            res <- asceticCCF(
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
            res <- asceticCCFResampling(
              dataset = genotype_table(),
              ccfDataset = reshaped_data(),
              vafDataset = resampling_data(),
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
    else if(case()=="bulk_multiple" || case() == "single_cell") {
      
      
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
            res <- asceticPhylogenies(
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
            res <- asceticPhylogeniesBootstrap(
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

    resampling_res(res)

    if(!is.null(res)) {
      names_to_remove <- c("dataset", "models", "ccfDataset", "inference")

      names <- setdiff(names(res), names_to_remove)
      names <- c(names, names(res$inference))
      
      output$visualize_inference <- renderUI({selectInput("visualize_inference", "Output inference", c(names))})
    }
  })
  
  
  # Visualization of inference results
  observeEvent(input$visualize_inference, {
    
    res <- resampling_res()
    
    col_names <- colnames(res$dataset)
    
    if (input$visualize_inference %in% names(res)) {
      selected_result <- res[[input$visualize_inference]]
    } else {
      selected_result <- res$inference[[input$visualize_inference]]
    }
    
    if (input$visualize_inference=="rankingEstimate") {
      output$graph_inference <- ({NULL})
      output$selected_result_output <- renderDT({
        selected_result[, "variable"] <- row.names(selected_result)
        selected_result[, "rank"] <- (as.integer(selected_result[, "rank"])+1) 
        colnames(selected_result)[1] <- "genes"
        datatable(selected_result, options = list(scrollX = TRUE), rownames = FALSE)
      })
    }
    else {
      output$selected_result_output <- ({NULL})
      colnames(selected_result) <- col_names
      rownames(selected_result) <- col_names
      if(all(selected_result == 0)) {
        showNotification("Nessun DAG", type = "error")
      } else {
        grafo <- graph_from_adjacency_matrix(selected_result)
        output$graph_inference <- renderPlot({
          grafo <- graph_from_adjacency_matrix(selected_result)
          
          nodi_da_rimuovere <- V(grafo)[degree(grafo, mode = "in") == 0 & degree(grafo, mode = "out") == 0]
          grafo <- delete.vertices(grafo, nodi_da_rimuovere)
          plot(grafo, layout = layout_with_fr, edge.arrow.size = 0.5, vertex.label.dist = 0.5, vertex.label.cex = 1.2)
        })
      }
    }
  })


  ######################################################### RESCUE PHASE  #########################################################  
  
  observe({
    if (input$project_name != "") {
      shinyjs::enable("saveBtn")
    } else {
      shinyjs::disable("saveBtn")
    }
  })
  
  observeEvent(input$saveBtn, {
    name_project <- input$project_name
    name_project <- paste0(name_project, "_", case())
    directory_output <- "output_project/"
    directory_complete <- paste0(directory_output, name_project)

    if(!(is.null(case()))) {
      if (!file.exists(directory_complete)) {
        dir.create(directory_complete, recursive = TRUE)
      }

      # Genotype table
      directory_file <- paste0(directory_complete, "/genotype.csv")
      saveData(reshaped_data() , directory_file)

      values <- c("binarization", "del_col", "del_row", "flag_resampling", "nresampling", "restarts", "regularization", "command", "seed", "output_inference")

      sequence <- ifelse((!is.null(input$binarization)), input$binarization, NA)
      sequence <- c(sequence, ifelse((!is.null(input$DeleteColumn)), toString(input$DeleteColumn), NA))
      sequence <- c(sequence, ifelse((!is.null(input$DeleteRow)), toString(input$DeleteRow), NA))
      sequence <- c(sequence, ifelse((!is.null(input$resamplingFlag)), input$resamplingFlag, NA))
      sequence <- c(sequence, ifelse((!is.null(input$nresampling)), input$nresampling, NA))
      sequence <- c(sequence, ifelse((!is.null(input$restarts)), input$restarts, NA))
      sequence <- c(sequence, ifelse((!is.null(input$regularization)), toString(input$regularization), NA))
      sequence <- c(sequence, ifelse((!is.null(input$command)), input$command, NA))
      sequence <- c(sequence, ifelse((!is.null(input$seed)), input$seed, NA))

      
      matrice_dati <- matrix(data = c(values, sequence),
                             nrow = length(values),
                             ncol = 2,
                             byrow = FALSE)
      
      matrice_dataframe <- as.data.frame(matrice_dati)
      colnames(matrice_dataframe) <- c("name", "value")

      # Parameters
      directory_file <- paste0(directory_complete, "/parameters.csv")
      saveData(matrice_dataframe , directory_file)
      
      # Resampling result
      if(!is.null(resampling_res())) {
        directory_file <- paste0(directory_complete, "/resampling_res.rds")
        saveRDS(resampling_res(), directory_file)
      }
      
      # Resampling table
      if (case()=="bulk_single") {
        directory_file <- paste0(directory_complete, "/resampling_table.csv")
        if (!(is.null(resampling_data()))) {
          saveData(resampling_data() , directory_file)
        }
      } else {
        # Parameters case (trees directory)
        values <- c("directory")
        
        sequence <- ifelse(!is.null(input$dir), toString(input$dir), NA)
        
        matrice_dati <- matrix(data = c(values, sequence),
                               nrow = length(values),
                               ncol = 2,
                               byrow = FALSE)
        matrice_dataframe <- as.data.frame(matrice_dati)
        colnames(matrice_dataframe) <- c("name", "value")
        
        
        directory_file <- paste0(directory_complete, "/case_parameters.csv")
        saveData(matrice_dataframe , directory_file)
      }
      
      showNotification("Completed", type = "error")
    }
    else {
      showNotification("Nothing to save", type = "error")
    }
  })

}
