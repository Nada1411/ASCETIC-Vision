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
library(ggtree)
library(ggraph)
library(igraph)
library(visNetwork)



server <- function(input, output, session) {
  
  ############################ Variables  ######################################

  reshaped_data2 <- reactiveVal(NULL)
  selected_folder <- reactiveVal(NULL)
  directory_output <- reactiveVal(NULL)
  reshaped_data <- reactiveVal(NULL)
  reshaped_data2 <- reactiveVal(NULL)
  genotype_table <- reactiveVal(NULL)
  case <- reactiveVal(NULL)
  directory <- reactiveVal(NULL)
  calculationInProgress <- reactiveVal(NULL)
  resampling_res <- reactiveVal(NULL)
  reshaped_data2 <- reactiveVal(NULL)
  ranges <- reactiveValues(x = NULL, y = NULL)
  ranges <- reactiveValues(x = NULL, y = NULL)
  orig <- reactiveVal(NULL)

  ############################ Function  #######################################
  
  #funzione che reimposta tutti i valori nulli in una pagina
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
    output$visualize_inference <- renderDataTable(NULL)
    output$selected_result_output <- renderDataTable(NULL)
    output$graph_inference <- renderDataTable(NULL)
    updateCheckboxInput(session, "resamplingFlag", value = FALSE)
    updateNumericInput(session, "nresampling", value = 3)
    updateSelectInput(session, "regularization", selected = "aic")
    updateSelectInput(session, "command", selected = "hc")
    updateNumericInput(session, "restarts", value = 10)
    updateNumericInput(session, "seed", value = 12345)
    output$visualize_inference <- NULL
    output$graph_inference <- NULL
    orig <- reactiveVal(NULL)
  }
  
  default_values_create_project <- function() {
    output$directoryInput <- renderUI(NULL)
    output$binarization_perc <- renderUI(NULL)
    output$binarization <- renderUI(NULL)
    output$DeleteRow <- renderUI(NULL)
    output$DeleteColumn <- renderUI(NULL)
    output$dataFile2 <- renderUI(NULL)
    output$loadBtn2 <- renderUI(NULL)
    output$dataTable <- renderDataTable(NULL)
    output$dataTable2 <- renderDataTable(NULL)
    output$heatmapPlot <- plotly::renderPlotly({NULL})
    output$switchViewBtn <- renderUI(NULL)
    output$visualize_inference <- renderDataTable(NULL)
    output$selected_result_output <- renderDataTable(NULL)
    output$graph_inference <- renderDataTable(NULL)
    updateCheckboxInput(session, "resamplingFlag", value = FALSE)
    updateNumericInput(session, "nresampling", value = 3)
    updateSelectInput(session, "regularization", selected = "aic")
    updateSelectInput(session, "command", selected = "hc")
    updateNumericInput(session, "restarts", value = 10)
    updateNumericInput(session, "seed", value = 12345)
    output$visualize_inference <- NULL
    output$graph_inference <- NULL
    orig <- reactiveVal(NULL)
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
        yaxis = list(autorange = "reversed"),
        paper_bgcolor = "#ECF0F5"
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
          sorted_genes <- names(sort(reshaped_data[row_index, selected_genes], 
                                     decreasing = TRUE))
          sorted_genes <- unlist(lapply(seq_along(sorted_genes), 
                                        function(i) rep(sorted_genes[i], 
                                                        each = 2)))
          sorted_genes <- sorted_genes[-c(1, length(sorted_genes))]
          
          poset_graph <- make_graph(edges = sorted_genes, directed = TRUE)
        }
        
        
        output$posetGraph <- renderVisNetwork({
          
          nodes <- as_tibble(get.vertex.attribute(poset_graph))
          colnames(nodes) <- "id"
          nodes <- data.frame(nodes, label= nodes$id)
          edges <- as_tibble(as_edgelist(poset_graph))
          colnames(edges) <- c("from", "to")
          
          
          generateVisNetwork(nodes, edges, "poset", "Poset")
        })

        
        output$content <- renderUI({
          if (!is.null(poset_graph) && length(poset_graph) > 0) {
            tagList(
              tags$hr(),
              div(
                style = "display: flex; justify-content: center; margin-top: 50px;",
                visNetworkOutput("posetGraph", width = "50%", height = "400px")
              )
            )
          } 
        })
      }
    })
  }
  
 
  # selection/deletion of row and column
  modify_reshaped_data <- function(reshaped_data) {
    
    if(is.null(input$DeleteColumn)) {
      output$dataTable <- renderDT({
        datatable(orig(), options = list(scrollX = TRUE), selection = "single")
      })
      output$heatmapPlot <- renderPlotly({
        generate_heatmap_plot(orig())
      })
    }
    
    observeEvent(input$DeleteColumn, { 
      reshaped_data <- as.data.frame(reshaped_data)
      columns_to_delete <- which(colnames(reshaped_data) == input$DeleteColumn)
      reshaped_data <- reshaped_data[, -columns_to_delete, drop = FALSE]
      reshaped_data <- as.matrix(reshaped_data)
      output$dataTable <- renderDT({
        datatable(reshaped_data, options = list(scrollX = TRUE), selection = "single")
      })
      output$heatmapPlot <- renderPlotly({
        generate_heatmap_plot(reshaped_data)
      })
    })
    
      
    observeEvent(input$DeleteRow, { 
      reshaped_data <- as.data.frame(reshaped_data)
       reshaped_data <- reshaped_data %>%
         slice(-which(rownames(reshaped_data) %in% input$DeleteRow))
      reshaped_data <- as.matrix(reshaped_data)
      output$dataTable <- renderDT({
        datatable(reshaped_data, options = list(scrollX = TRUE), 
                  selection ="single")
      })
      output$heatmapPlot <- renderPlotly({
        generate_heatmap_plot(reshaped_data)
      })
    })
    return(reshaped_data)
  }
  
  
  observe_data_modification <- function(reshaped_data) {
    observe({
      reshaped_data <- modify_reshaped_data(reshaped_data)
      output$dataTable <- renderDT({
        datatable(reshaped_data, options = list(scrollX = TRUE), 
                  selection ="single")
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
        file_data <- read.table(file_path, header = TRUE, 
                                stringsAsFactors = FALSE, sep = "\t")

        
        col_names <- colnames(file_data)

        
        file_data[[id_column_name]] <- col_names
        
        file_data <- file_data[, c(id_column_name, setdiff(col_names, 
                                                           id_column_name))]
        if (length(input$DeleteColumn) > 0){
          for (col in input$DeleteColumn) {
            if(col %in% colnames(file_data)) {
            col_index <- which(colnames(file_data) == col)
            # nodo foglia
              if ((1 %in% file_data[[col_index]]) && 
                  (all(as.numeric(file_data[col_index-1, -1]) == 0))) {
                file_data[[col_index]][file_data[[col_index]] == 1] <- 0
              }
              
              #nodo radice
              if (!(1 %in% file_data[[col_index]]) && 
                  (any(as.numeric(file_data[col_index-1, -1]) == 1))) {
                file_data[col_index - 1, -1][file_data[col_index - 1, -1] == 1] <- 0
              }
              
              #nodo interno
              if ((1 %in% file_data[[col_index]]) && 
                  (any(as.numeric(file_data[col_index-1, -1]) == 1))){
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
            div(
              style = "display: flex; justify-content: center; margin-top: 50px;",
              visNetworkOutput("graphPlot", width = "50%", height = "400px")
            )
          )
        })
        
        output$fileDataTable <- renderDT({
          datatable(file_data, options = list(scrollX = TRUE), selection ="single")
        })
        
        graph_data <- reshape2::melt(file_data, id.vars = id_column_name)
        edges <- graph_data[graph_data$value != 0, c(id_column_name, "variable")]
        
        graph <- graph_from_data_frame(edges, directed = TRUE)
        
        
        output$graphPlot <- renderVisNetwork({
          
          nodes <- as_tibble(get.vertex.attribute(graph))
          colnames(nodes) <- "id"
          nodes <- data.frame(nodes, label= nodes$id)
          edges <- as_tibble(as_edgelist(graph))
          colnames(edges) <- c("from", "to")
          
          
          generateVisNetwork(nodes, edges, "other", "DAG")
        })
        
        }
    })
  }
  
  readMatrixFiles <- function(directory_path) {
    
    readMatrix <- function(filePath) {
      # Leggi i dati dal file
      matrix_data <- read.table(filePath, header = TRUE, 
                                stringsAsFactors = FALSE,
                                sep = "\t")
      
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
        actionButton("interruptBtn", 
                     "Interrupt calcolo", 
                     class = "custom-button")
      })
    }
    else  {
      output$interruptButton <- renderUI({})
      output$spinner <- renderUI({})
    }
  }
  
  
  # Function for the interrupt key in the inference phase
  bulk_single_case <- function() {
    
    output$dataFile2 <- renderUI({
      fileInput("dataFile2", "Resampling")
    })
    output$loadBtn2 <- renderUI({
      actionButton("loadBtn2", "Load", class = "custom-button")
    })
    
    output$DeleteColumn <- render_delete_column_ui("DeleteColumn", 
                                                   "Delete column", 
                                                   reshaped_data())
    output$DeleteRow <- render_delete_row_ui("DeleteRow", 
                                             "Delete row", 
                                             reshaped_data())
    
    # Management deletion or selection of rows and columns
    observe_data_modification(reshaped_data())
  }
  
  bulk_multiple_case <- function() {
    modified_data <- reshaped_data()
      
    output$DeleteColumn <- render_delete_column_ui("DeleteColumn", 
                                                   "Delete column", 
                                                   reshaped_data())
    output$DeleteRow <- render_delete_row_ui("DeleteRow", 
                                             "Delete row", 
                                             reshaped_data())

      
    handle_dataTable_cell_clicked(reshaped_data(), "ID")
      
    observe({
      req(input$dir)
      selected_folder(parseDirPath(c(wd = getwd()), input$dir))
    })
    
    observe({
      
      modified_data <- modify_reshaped_data(modified_data)
      
      output$dataTable <- renderDT({
        datatable(modified_data, options = list(scrollX = TRUE), 
                  selection ="single")
      })
      
      reshaped_data(modified_data)
      
      output$heatmapPlot <- renderPlotly({
        generate_heatmap_plot(modified_data)
      })
    })
    
    output$directoryInput <- renderUI({
      shinyDirButton("dir", "Seleziona una cartella", 
                     title = "Seleziona una cartella", multiple = FALSE)
    })
    
    observe({
      req(input$dir)
      selected_folder(parseDirPath(c(wd = getwd()), input$dir))
    })
    
    shinyDirChoose(input, "dir", roots = c(wd = getwd()), 
                   filetypes = c("", "txt"))
  }
  
  single_cell_case <- function() {
    modified_data <- reshaped_data()
    
    output$DeleteColumn <- render_delete_column_ui("DeleteColumn", 
                                                   "Delete column", 
                                                   reshaped_data())
    output$DeleteRow <- render_delete_row_ui("DeleteRow", 
                                             "Delete row", 
                                             reshaped_data())
    
    
    handle_dataTable_cell_clicked(reshaped_data(), "ID")
    
    observe({
      req(input$dir)
      selected_folder(parseDirPath(c(wd = getwd()), input$dir))
    })
    
    observe({
      
      modified_data <- modify_reshaped_data(modified_data)
      
      output$dataTable <- renderDT({
        datatable(modified_data, options = list(scrollX = TRUE), 
                  selection ="single")
      })
      
      reshaped_data(modified_data)
      
      output$heatmapPlot <- renderPlotly({
        generate_heatmap_plot(modified_data)
      })
    })
    
    output$directoryInput <- renderUI({
      shinyDirButton("dir", "Select a folder", title = "Select a folder", 
                     multiple = FALSE)
    })
    shinyDirChoose(input, "dir", roots = c(wd = getwd()), filetypes = c("", "txt"))
    
  }
  
  generateVisNetwork <- function(nodes, edges, layout_type, title) {
    main_options <- list(text = title,
                         style = "font-family:https://fonts.googleapis.com/css2?family=Roboto+Condensed:ital,wght@0,100..900;1,100..900&display=swap;
                                font-weight: bold;
                                text-align:center;")
    
    if (layout_type == "poset") {
      visNetwork(nodes, edges, main = main_options) %>%
        visHierarchicalLayout(direction = "LR") %>%
        visNodes(
          shape = "dot",
          color = list(
            background = "#23B3E8",
            border = "#013848",
            highlight = "#E112EB"
          ),
          shadow = list(enabled = TRUE, size = 10),
          font = list(size = 20, vadjust = -50)
        ) %>%
        visEdges(
          shadow = FALSE,
          color = list(color = "#0085AF", highlight = "#E112EB"),
          arrows = "to"
        )
    } else {
      visNetwork(nodes, edges, main = main_options) %>%
        visIgraphLayout(layout = "layout_with_sugiyama") %>%
        visNodes(
          shape = "dot",
          color = list(
            background = "#23B3E8",
            border = "#013848",
            highlight = "#E112EB"
          ),
          shadow = list(enabled = TRUE, size = 10),
          font = list(size = 20, vadjust = -50)
        ) %>%
        visEdges(
          shadow = FALSE,
          color = list(color = "#0085AF", highlight = "#E112EB"),
          arrows = "to"
        )
    }
  }
  
  
  
  #################### Load or create project  #################################
  
  ##Create
  
  # Function to change the tabPanel to "Data Input" when the "Create New Project" 
  observeEvent(input$create_project_button, {
    default_values_create_project()
    updateTabItems(session, "sidebarMenu", "input")
  })

  # Show project names in "output_project" folder
  project_names <- reactive({
    get_project_names()
  })
  
  # Show the table with the names of existing projects
  output$projectList <- renderDT({
    datatable(project_names(), rownames = FALSE, colnames = c("Project name"), 
              selection ="single", options = list(
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#232E33', 
      'color': '#fff'});",
      "}")
  )) 
  })
  
  
  ##Load
  
  observeEvent(input$loadProjBtn, {
    default_values_create_project()
    if (is.null(input$projectList_cell_clicked$row) || 
        is.null(input$projectList_cell_clicked$col)) {
      showNotification("Select the project you want to upload", type = "error")
      
    } else {
      
      #Visualise switch bottom
      output$switchViewBtn <- renderUI({
        actionButton("switchViewBtn", "Switch View", class = "custom-button")
      })
      
      # Management click on existing project table
      observeEvent(input$projectList_cell_clicked, {
        if (!is.null(input$projectList_cell_clicked)) {
          clicked_row <- input$projectList_cell_clicked$row
          clicked_col <- input$projectList_cell_clicked$col
          
          project_name <- project_names()[clicked_row, 1]
          
          
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
            
            if (file_name == "resampling_table") {
              data2 <- read.csv(file_directory)
              output$dataTable2 <- renderDT({
                datatable(data2, options = list(scrollX = TRUE), selection ="single")
              })
              reshaped_data2(data2)
            }
            else if (file_name == "parametri_bulk_multiple") {
              parametri <- read.csv(file_directory)
              
              for (i in 1:nrow(parametri)) {
                parametro <- parametri[i, "Nome"]
                valore <- parametri[i, "Valore"]
                
                # Assegna il valore all'input corrispondente
                if (parametro == "binarization_perc") {
                  val_bin_perc <- as.numeric(valore)
                  output$binarization_perc <- renderUI({
                    numericInput("binarization_perc", 
                                 "Filter to binarize percentage", 
                                 value = val_bin_perc, min = 0, 
                                 max = 1, step = 0.01)
                  })
                }
              }
            }
          }
          
          
          #Read the file that are for all the 3 cases
          for (file in project_files) {
            file_name <- tools::file_path_sans_ext(file)
            file_directory <- file.path(project_folder, file)
            
            if (file_name == "genotipo") {
              data <- read.csv(file_directory, row.names = 1)
              data <- as.matrix(data)
              reshaped_data(data)
              
              output$dataTable <- renderDT({
                datatable(data, options = list(scrollX = TRUE), selection ="single")
              })
              
              output$heatmapPlot <- renderPlotly({
                generate_heatmap_plot(data)
              })


              if(case() == "bulk_single") {
                bulk_single_case()
              }
              else if(case() == "bulk_multiple") {
                bulk_multiple_case()
              }
              else if(case() == "single_cell") {
                single_cell_case()
              }
              
            }
            else if (file_name == "parametri") {
              parametri <- read.csv(file_directory)


              # Ciclo attraverso ogni riga del dataframe
              for (i in 1:nrow(parametri)) {
                parametro <- parametri[i, "Nome"]
                valore <- parametri[i, "Valore"]

                # Assegna il valore all'input corrispondente
                if (parametro == "binarization") {
                  val_bin <- as.numeric(valore)
                  output$binarization <- renderUI({
                    numericInput("binarization", "Filter to binarize", 
                                 value = val_bin, min = 0, max = 1, step = 0.01)
                  })
                } else if (parametro == "del_col") {
                  output$DeleteColumn <- render_delete_column_ui("DeleteColumn", 
                                                                 "Delete column", 
                                                                 reshaped_data())
                } else if (parametro == "del_row") {
                  output$DeleteRow <- render_delete_row_ui("DeleteRow", 
                                                           "Delete row", 
                                                           reshaped_data())
                } else if (parametro == "flag_resampling") {
                  updateCheckboxInput(session, "resamplingFlag", 
                                      value = as.logical(valore))
                } else if (parametro == "nresampling") {
                  updateNumericInput(session, "nresampling", 
                                     value = as.numeric(valore))
                } else if (parametro == "restarts") {
                  updateNumericInput(session, "restarts", 
                                     value = as.numeric(valore))
                } else if (parametro == "regularization") {
                  elements <- unlist(strsplit(valore, ", "))
                  updateSelectInput(session, "regularization", 
                                    selected = c(elements))
                } else if (parametro == "command") {
                  updateSelectInput(session, "command", selected = valore)
                } else if (parametro == "seed") {
                  val_seed <- as.numeric(valore)
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
              output$visualize_inference <- renderUI({selectInput("visualize_inference", 
                                                                  "Output inference", 
                                                                  c(names))})
            }
          }
        }    
        
        })
      updateTabItems(session, "sidebarMenu", "input")
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
  
  

################# Input data  ##################################################
  
  
  # Displaying the resampling file, in the case where there are three columns 
  #(single bulk case) the table is represented.
  # in other cases an error message is printed
  observeEvent(input$loadBtn2, {
    #resampling file
    inFile2 <- input$dataFile2
    data2 <- read.table(inFile2$datapath, sep = "\t", header = TRUE, 
                        stringsAsFactors = FALSE)
    reshaped_data2(data2)

    output$dataTable2 <- renderDT({
      datatable(data2, options = list(scrollX = TRUE), selection ="single")
    })

    reshaped_data2(data2)
  })
  
  reshaped_data <- reactiveVal(NULL)
  
  observeEvent(input$loadBtn, {
    inFile <- input$dataFile
    
    if (is.null(inFile)) {
      showNotification("File non caricato.", type = "error")
    } else {
      data <- read.table(inFile$datapath, sep = "\t", header = TRUE, 
                         stringsAsFactors = FALSE)
      
      orig(data)
      #### Bulk single biopsy
      if (ncol(data) == 3) {
        default_values_load_genotype()
        case("bulk_single")
        
        data <- distinct(data, SAMPLE, GENE, .keep_all = TRUE)
        reshaped_data(
          acast(data, SAMPLE ~ GENE, value.var = "CCF", fill = 0)
        )

        output$binarization <- renderUI({
          numericInput("binarization", "Filter to binarize", value = 1, 
                       min = 0, max = 1, step = 0.01)
        })
        output$switchViewBtn <- renderUI({
          actionButton("switchViewBtn", "Switch View", class = "custom-button")
        })
        
        bulk_single_case()
        
      } else if (ncol(data) == 4) {   #### Bulk multipla biopsia o single cell 
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
          
          modified_data <- reshaped_data()
          
          output$DeleteColumn <- render_delete_column_ui("DeleteColumn", 
                                                         "Delete column", 
                                                         reshaped_data())
          output$DeleteRow <- render_delete_row_ui("DeleteRow", "Delete row", 
                                                   reshaped_data())
          
          observe({
            
            modified_data <- modify_reshaped_data(modified_data)
            
            output$dataTable <- renderDT({
              datatable(modified_data, options = list(scrollX = TRUE), 
                        selection ="single")
            })
            
            reshaped_data(modified_data)
            
            output$heatmapPlot <- renderPlotly({
              generate_heatmap_plot(modified_data)
            })
          })
          
          output$binarization <- renderUI({
            numericInput("binarization", "Filter to binarize", value = 1, 
                         min = 0, max = 1, step = 0.01)
          })
          
          output$switchViewBtn <- renderUI({
            actionButton("switchViewBtn", "Switch View", class = "custom-button")
          })
          
          output$binarization_perc <- renderUI({
            numericInput("binarization_perc", "Filter to binarize percentage", 
                         value = 1, min = 0, max = 1, step = 0.01)
          })
          
          handle_dataTable_cell_clicked(reshaped_data(), "ID")
          
          output$directoryInput <- renderUI({
            shinyDirButton("dir", "Seleziona una cartella", 
                           title = "Seleziona una cartella", multiple = FALSE)
          })
          
          observe({
            req(input$dir)
            selected_folder(parseDirPath(c(wd = getwd()), input$dir))
          })
          
          shinyDirChoose(input, "dir", roots = c(wd = getwd()), 
                         filetypes = c("", "txt"))
          
          
        } else if (colnames(data)[2]=="CELL") {
          default_values_load_genotype()
          case("single_cell")
          # Remove duplicates by keeping only the first occurrence for each position
          data <- distinct(data, PATIENT, CELL, GENE, .keep_all = TRUE)
          
          data <- data %>%
              group_by(PATIENT, GENE) %>%
              summarise(PERCENTAGE = mean(VALUE)) %>%
              pivot_wider(names_from = GENE, values_from = PERCENTAGE, 
                          values_fill = list(PERCENTAGE = 0))
          
          reshaped_data(data %>%
            column_to_rownames(var = "PATIENT"))
          
          modified_data <- reshaped_data()
          
          output$DeleteColumn <- render_delete_column_ui("DeleteColumn", 
                                                         "Delete column", 
                                                         reshaped_data())
          output$DeleteRow <- render_delete_row_ui("DeleteRow", 
                                                   "Delete row", 
                                                   reshaped_data())
          
          observe({
            
            modified_data <- modify_reshaped_data(modified_data)
            

            output$dataTable <- renderDT({
              datatable(modified_data, options = list(scrollX = TRUE), 
                        selection ="single")
            })
            
            reshaped_data(modified_data)
            
            output$heatmapPlot <- renderPlotly({
              generate_heatmap_plot(reshaped_data())
            })
            
          })
          
          output$binarization <- renderUI({
            numericInput("binarization", "Filter to binarize", value = 1, 
                         min = 0, max = 1, step = 0.01)
          })
          output$switchViewBtn <- renderUI({
            actionButton("switchViewBtn", "Switch View", class = "custom-button")
          })
          
          handle_dataTable_cell_clicked(reshaped_data(), "PATIENT")
          
          output$directoryInput <- renderUI({
            shinyDirButton("dir", "Select a folder", title = "Select a folder", 
                           multiple = FALSE)
          })
          
          
          observe({
            req(input$dir)
            selected_folder(parseDirPath(c(wd = getwd()), input$dir))
          })
          
          shinyDirChoose(input, "dir", roots = c(wd = getwd()), 
                         filetypes = c("", "txt"))
          
        } else {
          showNotification("File not recognized. Make sure the column names are correct.", 
                           type = "error")
        }
      } else {  #### Wrong file
        showNotification("File not recognized. Make sure the number of columns is correct.", 
                         type = "error")
      }
    }
  })

  
  ############## Inference  ####################################################
  
  # filter the genotype table according to the case
  observeEvent(input$submitBtn, {
    filter <- input$binarization
    filter_perc <- input$binarization_perc
    genotype_table(reshaped_data())

    if (is.null(case())) {
      showNotification("Load the files in the previous step", type = "error")
    }
    else if (case() == "bulk_single" || case() == "single_cell") {
      #filter the genotype table
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
    updateTabItems(session, "sidebarMenu", "inference")
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
    #otherwise we evaluate the case, if we are in the first check that the 
    #resampling file was loaded in the previous step
     else if (case() == "bulk_single") {
        if (is.null(input$dataTable2) & is.null(reshaped_data2())) {
          # If the file has not been uploaded, turn off the flag and show an error message
          updateCheckboxInput(session, "resamplingFlag", value = FALSE)
          showNotification("Load the resampling file in the previous step", 
                           type = "error")
        }
      }
    }
    else {
      output$nresampling <- renderUI({numericInput("nresampling", 
                                                   "Number of samplings", 
                                                   3, min = 3)})
    }
  })
  
  
  observeEvent(input$interruptButton, {
    progress$interrupt()
  })
  
  res <- NULL
  
  #inference function
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
              vafDataset = reshaped_data2(),
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
      
      
      output$visualize_inference <- renderUI({selectInput("visualize_inference", 
                                                          "Output inference", 
                                                          c(names))})
  
    }
  })
  
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
        datatable(selected_result, options = list(scrollX = TRUE), 
                  rownames = FALSE, selection ="single")
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
        
        output$graph_inference <- renderVisNetwork({
          grafo <- graph_from_adjacency_matrix(selected_result)
          
          nodi_da_rimuovere <- V(grafo)[degree(grafo, mode = "in") == 0 & 
                                        degree(grafo, mode = "out") == 0]
          grafo <- delete.vertices(grafo, nodi_da_rimuovere)
          

          nodes <- as_tibble(get.vertex.attribute(grafo))
          colnames(nodes) <- "id"
          nodes <- data.frame(nodes, label= nodes$id)
          edges <- as_tibble(as_edgelist(grafo))
          colnames(edges) <- c("from", "to")

          if (input$visualize_inference == 'poset') {
            generateVisNetwork(nodes, edges, "other", "Inference output")
          } else {
            generateVisNetwork(nodes, edges, "other", "Inference output")
          }
          
        })
      }
    }
  })



  ############# FASE SALVATAGGIO  ############################################## 

  saveData <- function(data, nome) {
    write.csv(data, nome, row.names=TRUE)
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
    nome_progetto <- paste0(nome_progetto, "_", case())
    directory_output <- "output_project/"
    directory_completo <- paste0(directory_output, nome_progetto)

    if(!(is.null(case()))) {
      
      # Verifica se la directory esiste, altrimenti creala
      if (!file.exists(directory_completo)) {
        dir.create(directory_completo, recursive = TRUE)
      }

      directory_file <- paste0(directory_completo, "/genotipo.csv")
      saveData(reshaped_data() , directory_file)
      


      valori <- c("binarization", "del_col", "del_row", "flag_resampling", 
                  "nresampling", "restarts", "regularization", "command", 
                  "seed", "output_inference")
      

      sequenza <- ifelse((!is.null(input$binarization)), input$binarization, NA)
      sequenza <- c(sequenza, ifelse((!is.null(input$DeleteColumn)), 
                                     toString(input$DeleteColumn), NA))
      sequenza <- c(sequenza, ifelse((!is.null(input$DeleteRow)), 
                                     toString(input$DeleteRow), NA))
      sequenza <- c(sequenza, ifelse((!is.null(input$resamplingFlag)), 
                                     input$resamplingFlag, NA))
      sequenza <- c(sequenza, ifelse((!is.null(input$nresampling)), 
                                     input$nresampling, NA))
      sequenza <- c(sequenza, ifelse((!is.null(input$restarts)), 
                                     input$restarts, NA))
      sequenza <- c(sequenza, ifelse((!is.null(input$regularization)), 
                                     toString(input$regularization), NA))
      sequenza <- c(sequenza, ifelse((!is.null(input$command)), 
                                     input$command, NA))
      sequenza <- c(sequenza, ifelse((!is.null(input$seed)), input$seed, NA))

      
      matrice_dati <- matrix(data = c(valori, sequenza),
                             nrow = length(valori),
                             ncol = 2,
                             byrow = FALSE)
      
      matrice_dataframe <- as.data.frame(matrice_dati)
      colnames(matrice_dataframe) <- c("Nome", "Valore")

      directory_file <- paste0(directory_completo, "/parametri.csv")
      saveData(matrice_dataframe , directory_file)
      
      if(!is.null(resampling_res())) {
        directory_file <- paste0(directory_completo, "/resampling_res.rds")
        saveRDS(resampling_res(), directory_file)
      }
      
      
      if (case()=="bulk_single") {
        directory_file <- paste0(directory_completo, "/resampling_table.csv")
        if (!(is.null(reshaped_data2()))) {
          saveData(reshaped_data2() , directory_file)
        }
      } else if (case()=="bulk_multiple") {
        
        
        valori <- c("directory", "binarization_perc")
        # Costruisci la sequenza di valori
        sequenza <- ifelse(!is.null(input$dir), toString(input$dir), NA)
        sequenza <- c(sequenza, ifelse(!is.null(input$binarization_perc), 
                                       input$binarization_perc, NA))
        
        matrice_dati <- matrix(data = c(valori, sequenza),
                               nrow = length(valori),
                               ncol = 2,
                               byrow = FALSE)
        
        matrice_dataframe <- as.data.frame(matrice_dati)
        colnames(matrice_dataframe) <- c("Nome", "Valore")
        
        # Scrivi il dataframe nel file CSV
        directory_file <- paste0(directory_completo, "/parametri_bulk_multiple.csv")
        saveData(matrice_dataframe , directory_file)
        
      } else if (case()=="single_cell") {
        valori <- c("directory")
        
        sequenza <- ifelse(!is.null(input$dir), toString(input$dir), NA)
        
        matrice_dati <- matrix(data = c(valori, sequenza),
                               nrow = length(valori),
                               ncol = 2,
                               byrow = FALSE)
        matrice_dataframe <- as.data.frame(matrice_dati)
        colnames(matrice_dataframe) <- c("Nome", "Valore")
        
        
        directory_file <- paste0(directory_completo, "/parametri_single_cell.csv")
        saveData(matrice_dataframe , directory_file)
      }
      
      showNotification("Completed", type = "error")
    }
    else {
      showNotification("Nothing to save", type = "error")
    }
  })

}
