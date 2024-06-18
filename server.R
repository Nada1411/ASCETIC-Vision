source("libraries.R")

server <- function(input, output, session) {
  
  ############################ Variables  ######################################
  
  selected_folder <- reactiveVal(NULL)
  directory_output <- reactiveVal(NULL)
  reshaped_data <- reactiveVal(NULL)
  reshaped_data_inference <- reactiveVal(NULL)
  genotype_table <- reactiveVal(NULL)
  case <- reactiveVal(NULL)
  resampling_res <- reactiveVal(NULL)
  conf_res <- reactiveVal(NULL)
  reshaped_data2 <- reactiveVal(NULL)
  rv <- reactiveValues(deletedColumns = character(0), deletedRows = character(0))
  app_activated <- reactiveVal(FALSE)
  nresampling <- reactiveVal(FALSE)
  nresampling_conf <- reactiveVal(FALSE)
  visualizeInferenceOutput <- reactiveVal(TRUE)
  visualizeConfidenceOutput <- reactiveVal(TRUE)
  reactive_selected_result <- reactiveVal(NULL)
  reactive_selected_result_conf <- reactiveVal(NULL)
  reshaped_data_matrix <- reactiveVal(NULL)
  buttonClicked <- reactiveVal(FALSE)
  reshaped_dataSurv <- reactiveVal(FALSE)
  genotype_table_surv <- reactiveVal(FALSE)
  
  # Active app
  observe({
    if (!app_activated()) {
      app_activated(TRUE)
    }
  })
  
  # Display the genotype table entry if the app is active
  output$dataFile <- renderUI({
    if (app_activated()) {
      tagList(
        div(style = "display: flex; align-items: center;", 
            fileInput("dataFile", 
                      label = span("Genotype ", 
                                   tags$i(id = "helpIcon", 
                                          class = "fa fa-question-circle", 
                                          style="margin-left: 5px;"))),
            bsTooltip(id = "helpIcon", 
                      title = "Select the file containing the information about the sample taken and its CCF.", 
                      placement = "right", trigger = "hover")
        )
      )
    }
  })
  
  # Display the genotype table entry in survival phase 
  output$dataFile_surv <- renderUI({
    # Verifica se il checkbox è selezionato
    if (input$load_file) {
      tagList(
        div(style = "align-items: center;", 
            fileInput("dataFile_surv", 
                      label = span("Genotype ", 
                                   tags$i(id = "helpIcon", 
                                          class = "fa fa-question-nitro", 
                                          style="margin-left: 5px;"))),
            bsTooltip(id = "helpIcon", 
                      title = "Select the file containing the information about the sample taken and its CCF.", 
                      placement = "right", trigger = "hover"),
            actionButton("loadBtn_surv", "Load", class = "custom-button")
        )
      )
    }
  })
  
  output$dataFile2_surv <- renderUI({
    tagList(
      div(style = "align-items: center;", 
          fileInput("dataFile2_surv", 
                    label = span("File ", 
                                 tags$i(id = "helpIcon", 
                                        class = "fa fa-question-nitro", 
                                        style="margin-left: 5px;"))),
          bsTooltip(id = "helpIcon", 
                    title = "Select the file containing the information about the sample taken and its CCF.", 
                    placement = "right", trigger = "hover"),
          actionButton("loadBtn2_surv", "Load", class = "custom-button")
      )
    )
  })
  
  
  # Regularization selectable in the confidence enstimation step are those that 
  # were used during the inference step
  observe({
    updateSelectInput(session, "regularization_confEstimation",
                      choices = input$regularization)
  })
  
  # Binarization field constraint management
  observeEvent(input$binarization, {
    x <- input$binarization
    if ( !is.na(x) && (x > 1 || x < 0)){
      updateNumericInput(session, "binarization", value = 1)
      showNotification("Allowed range 0-1 ", type = "warning")
    }
  })
  
  # Percentage binarization field constraint management
  observeEvent(input$binarization_perc, {
    x <- input$binarization_perc
    if ( !is.na(x) && (x > 1 || x < 0)){
      updateNumericInput(session, "binarization_perc", value = 1)
      showNotification("Allowed range 0-1 ", type = "warning")
    }
  })
  
  
  ############################ Function  #######################################
  
  reset_common_values <- function() {
    output$directoryInput <- renderUI(NULL)
    output$binarization_perc <- renderUI(NULL)
    output$binarization <- renderUI(NULL)
    output$loadBtn2 <- renderUI(NULL)
    output$dataTable <- NULL
    output$dataTable2 <- NULL
    output$dataFile2 <- NULL
    output$gene_graph_tab <- NULL
    output$graph_inference <- NULL
    output$graph_conf <- NULL
    output$content <- renderUI({NULL})
    output$project_info <- renderUI(NULL)
    output$heatmapPlot <- renderUI({NULL})
    output$dataTable_cell_clicked <- renderUI({NULL})
    updateCheckboxInput(session, "resamplingFlag", value = FALSE)
    updateCheckboxInput(session, "resamplingFlag_conf", value = FALSE)
    updateNumericInput(session, "nresampling", value = 3)
    updateNumericInput(session, "iteration_confEstimation", value = 1)
    updateNumericInput(session, "nresampling_conf", value = 3)
    updateSelectInput(session, "regularization", selected = "aic")
    updateSelectInput(session, "command", selected = "hc")
    updateNumericInput(session, "restarts", value = 10)
    updateNumericInput(session, "seed", value = 12345)
    rv <- reactiveValues(deletedColumns = character(0), deletedRows = character(0))
    reshaped_data2(NULL)
    nresampling(NULL)
    nresampling_conf(NULL)
    visualizeInferenceOutput(FALSE)
    visualizeConfidenceOutput(FALSE)
    resampling_res(NULL)
    conf_res(NULL)
    case(NULL)
    selected_folder(NULL)
  }
  
  # Resets values when loading a new project
  default_values_load_genotype <- function() {
    reset_common_values()
    output$selected_result_output <- renderDataTable(NULL)
    output$selected_result_output_conf <- renderDataTable(NULL)
    output$visualize_inference <- renderUI(NULL)
    output$visualize_conf <- NULL
    updateSelectInput(session, "DeleteColumn", selected = character(0))
    updateSelectInput(session, "DeleteRow", selected = character(0))
    updateSelectInput(session, "visualize_inference", selected = "poset")
    app_activated(FALSE)
    buttonClicked(FALSE)
  }
  
  # Resets values when loading a genotype file
  default_values_load_new_genotype <- function() {
    reset_common_values()
    output$visualize_inference <- NULL
    output$selected_result_output <- NULL
    output$selected_result_output_conf <- NULL
    output$visualize_conf <- NULL
    updateSelectInput(session, "DeleteColumn", selected = character(0))
    updateSelectInput(session, "DeleteRow", selected = character(0))
  }
  
  # Resets values when creating a new project
  default_values_create_project <- function() {
    reset_common_values()
    output$switchViewBtn <- renderUI(NULL)
    output$selected_result_output <- NULL
    output$selected_result_output_conf <- NULL
    orig <- reactiveVal(NULL)
    output$DeleteColumn <- NULL
    output$DeleteRow <- NULL
    output$visualize_inference <- NULL
    output$visualize_conf <- NULL
    app_activated(FALSE)
  }
  
  # Resets values when submit btm in inference
  default_values_inference <- function() {
    output$graph_inference <- NULL
    output$graph_conf <- NULL
    output$selected_result_output_conf <- renderDataTable(NULL)
    output$selected_result_output <- renderDataTable(NULL)
    output$visualize_inference <- renderUI(NULL)
    visualizeInferenceOutput(FALSE)
    visualizeConfidenceOutput(FALSE)
  }
  
  # Returns the list of project names in the output_project folder
  get_project_names <- function() {
    project_names <- list.files("output_project")
    file_infos <- file.info(file.path("output_project", project_names))
    last_modified <- file_infos$mtime
    project_info <- data.frame(project_names, last_modified)
    
    return(project_info)
  }
  
  # Generate heatmap
  generate_heatmap_plot <- function(data) {
    heatmap_plot <- plot_ly(
      z = as.matrix(data),
      x = colnames(data),
      y = rownames(data),
      type = "heatmap"    ) %>%
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
      if (case()=="bulk_single") {
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
                  visNetworkOutput("posetGraph", width = "80%", height = "400px")
                )
              )
            } 
          })
        }
      }
    })
  }
  
  observe({
    value <- resampling_res()
    # Update choices based on selected items in 'regularization surv'
    updateSelectInput(session, "regularization_surv",
                      choices = names(value$inference))
  })
  
  # Selection/deletion of row and column
  modify_reshaped_data <- function(reshaped_data) {
    observe({
      reshaped_data_df <- as.data.frame(reshaped_data) 
      
      if (!is.null(input$DeleteColumn) && length(input$DeleteColumn) > 0) {
        if (all(input$DeleteColumn %in% colnames(reshaped_data_df))) {
          reshaped_data_df <- reshaped_data_df[, !colnames(reshaped_data_df) 
                                               %in% input$DeleteColumn, drop = FALSE]
        }
      }
      
      if (!is.null(input$DeleteRow) && length(input$DeleteRow) > 0) {
        if (all(input$DeleteRow %in% rownames(reshaped_data_df)) || 
            all(as.numeric(input$DeleteRow) %in% 1:nrow(reshaped_data_df))) {
          reshaped_data_df <- reshaped_data_df[!rownames(reshaped_data_df) 
                                               %in% input$DeleteRow, , drop = FALSE]
        }
      }
      
      reshaped_data_matrix <- as.matrix(reshaped_data_df)
      
      output$dataTable <- renderDT({
        datatable(reshaped_data_matrix, options = list(scrollX = TRUE), 
                  selection = "single")
      })
      
      output$heatmapPlot <- renderUI({
        generate_heatmap_plot(reshaped_data_matrix)
      })
      
      # Detects clicks on table cells
      observe_table_cell_clicked(reshaped_data_matrix)
      
      reshaped_data_matrix(reshaped_data_matrix)
    })
    return(reshaped_data)
  }
  
  # Management bulk single case 
  observe_data_modification <- function(reshaped_data) {
    observe({
      reshaped_data <- modify_reshaped_data(reshaped_data)
      output$dataTable <- renderDT({
        datatable(reshaped_data, options = list(scrollX = TRUE), 
                  selection ="single")
      })
      
      reshaped_data(reshaped_data)
      
      output$heatmapPlot <- renderUI({
        generate_heatmap_plot(reshaped_data)
      })
      
      observe_table_cell_clicked(reshaped_data)
      
      reshaped_data(reshaped_data)
    })
  }
  
  # Manage click on genotype table in bulk multi region and single cell
  handle_dataTable_cell_clicked <- function(reshaped_data, id_column_name) {
    observeEvent(input$dataTable_cell_clicked, {
      output$content <- renderUI({NULL})
      info1 <- input$dataTable_cell_clicked
      
      if (!is.null(info1) && !is.null(info1$row) && !is.null(info1$col)) {
        row_index <- info1$row
        selected_id <- strsplit((rownames(reshaped_data)[row_index]), " ")[[1]][1]
        
        req(input$dir)
        selected_folder <- parseDirPath(c(wd = getwd()), input$dir)
        selected_id <- sub("\\s.*", "", selected_id)
        file_to_search <- paste0(selected_id, ".txt")
        file_path <- file.path(selected_folder, file_to_search)
        if (file.exists(file_path)) {
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
                #nodo foglia
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
            if (ecount(graph) > 0) {
              nodes <- as_tibble(get.vertex.attribute(graph))
              colnames(nodes) <- "id"
              nodes <- data.frame(nodes, label = nodes$id)
              edges <- as_tibble(as_edgelist(graph))
              colnames(edges) <- c("from", "to")
              
              generateVisNetwork(nodes, edges, "other", "")
            }
          })
        } 
      }
    })
  }
  
  # Process the present files of the selected folder in the case multiple bulk 
  # and single cell 
  readMatrixFiles <- function(directory_path) {
    
    readMatrix <- function(filePath) {
      matrix_data <- read.table(filePath, header = TRUE, 
                                stringsAsFactors = FALSE,
                                sep = "\t")
      
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
  
  # Manage bulk single case
  bulk_single_case <- function() {
    
    output$dataFile2 <- renderUI({
      tagList(
        div(style = "display: flex; align-items: center;", 
            fileInput("dataFile2", 
                      label = span("Resampling ", 
                                   tags$i(id = "helpIcon2", 
                                          class = "fa fa-question-circle", 
                                          style="margin-left: 5px;"))),
            bsTooltip(id = "helpIcon2", 
                      title = "If you want to perform the inference with resampling upload the file", 
                      placement = "right", trigger = "hover")
        )
      )
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
  
  # Manage bulk multiple case
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
      
      output$heatmapPlot <- renderUI({
        generate_heatmap_plot(modified_data)
      })
    })
    
    output$directoryInput <- renderUI({
      tagList(
        div(style = "display: flex; align-items: center; gap: 5px;",
            shinyDirButton("dir", "Select a folder", title = "Select a folder", 
                           multiple = FALSE),
            tags$i(id = "helpIconFolder", class = "fa fa-question-circle", 
                   style="cursor: pointer;", `data-toggle`="tooltip", 
                   `data-placement`="right",
                   title="To perform the inference operation, it's necessary to select the folder containing the files corresponding to each row of the database along with its respective Directed Acyclic Graph (DAG).")
        )
      )
    })
    
    
    observe({
      req(input$dir)
      selected_folder(parseDirPath(c(wd = getwd()), input$dir))
    })
    
    shinyDirChoose(input, "dir", roots = c(wd = getwd()), 
                   filetypes = c("", "txt"))
  }
  
  # Manage single cell case
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
      
      output$heatmapPlot <- renderUI({
        generate_heatmap_plot(modified_data)
      })
    })
    
    output$directoryInput <- renderUI({
      tagList(
        div(style = "display: flex; align-items: center; gap: 5px;",
            shinyDirButton("dir", "Select a folder", title = "Select a folder", 
                           multiple = FALSE),
            tags$i(id = "helpIconFolder", class = "fa fa-question-circle", 
                   style="cursor: pointer;", `data-toggle`="tooltip", 
                   `data-placement`="right",
                   title="To perform the inference operation, it's necessary to select the folder containing the files corresponding to each row of the database along with its respective Directed Acyclic Graph (DAG).")
        )
      )
    })
    shinyDirChoose(input, "dir", roots = c(wd = getwd()), filetypes = c("", "txt")) 
  }
  
  # Generate network graph
  generateVisNetwork <- function(nodes, edges, layout_type, title) {
    main_options <- list(text = title,
                         style = "font-family:https://fonts.googleapis.com/css2?family=Roboto+Condensed:ital,wght@0,100..900;1,100..900&display=swap;
                                font-weight: bold;
                                text-align:center;")
    
    if (layout_type == "poset") {
      visNetwork(nodes, edges, main = main_options, background = "white") %>%
        visHierarchicalLayout(direction = "LR") %>%
        visNodes(
          shape = "dot",
          size = 5,          color = list(
            background = "#23B3E8",
            border = "#013848",
            highlight = "#E34A33"
          ),
          shadow = list(enabled = TRUE, size = 10),
          font = list(size = 10, vadjust = -50)
        ) %>%
        visEdges(
          shadow = FALSE,
          color = list(color = "#0085AF", highlight = "#E34A33"),
          arrows = "to"
        )%>% 
        visOptions(nodesIdSelection = TRUE)%>% 
        visInteraction(navigationButtons = TRUE)%>% 
        visExport(type = "pdf", name = "network",
                  label = paste0("Export as pdf"),
                  style="color: white;
                        background-color: #628291;
                        border-color: #628291;
                        border-radius: 3px;
                        border-style: solid;
                        box-shadow: none; 
                        outline: none;") 
    } else {
      graph <- graph_from_data_frame(edges, directed = TRUE, vertices = nodes)
      layout_sugiyama <- layout_with_sugiyama(graph)
      layout_with_names <- cbind(nodes$id, layout_sugiyama$layout)
      
      #number of layers
      unique_numbers_count <- length(unique(layout_with_names[, 3]))
      list_of_node_lists <- split(layout_with_names[, 1], layout_with_names[, 3])
      colors <- brewer.pal(unique_numbers_count, "OrRd")
      
      
      nodes$color <- sapply(nodes$id, function(id) {
        layer <- layout_with_names[layout_with_names[, 1] == id, 3]
        colors[as.integer(layer)]  
      })
      
      visNetwork(nodes, edges, main = main_options, background = "white") %>%
        visIgraphLayout(layout = "layout_with_sugiyama") %>%  
        visNodes(
          shape = "dot",
          size = input$nodeSize, 
          color = list(
            background = nodes$color,
            border = "#013848",
            highlight = "#B20062"
          ),
          shadow = list(enabled = TRUE, size = 10),
          font = list(size = input$fontSize, vadjust = -50)
        ) %>%
        visEdges(
          shadow = FALSE,
          color = list(color = "#0085AF", highlight = "#B20062"),
          arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5)),
          width = 2
        ) %>%
        visOptions(nodesIdSelection = TRUE) %>%
        visInteraction(navigationButtons = TRUE) %>% 
        visExport(type = "pdf", name = "network",
                  label = paste0("Export as pdf"),
                  style="color: white;
                        background-color: #628291;
                        border-color: #628291;
                        border-radius: 3px;
                        border-style: solid;
                        box-shadow: none; 
                        outline: none;") 
    }
  }
  
  # Download btn CSV file
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste("inference-data-", Sys.Date(), ".csv")  
    },
    content = function(file) {
      res <- reactive_selected_result()
      write.csv(res, file, row.names = TRUE, col.names = TRUE)
    }
  )
  
  # Download btn CSV file confidence
  output$downloadCSV_conf <- downloadHandler(
    filename = function() {
      paste("confidence-data-", Sys.Date(), ".csv")  
    },
    content = function(file) {
      res <- reactive_selected_result_conf()
      write.csv(res, file, row.names = TRUE, col.names = TRUE)
    }
  )
  
  # Save CSV file
  saveData <- function(data, name) {
    write.csv(data, name, row.names=TRUE)
  }
  
  # Link to external db
  observe({
    if (!is.null(input$graph_inference_selected)&&input$graph_inference_selected!="") {
      selected_node_id <- input$graph_inference_selected
      edb <- EnsDb.Hsapiens.v86
      Tx <- transcripts(edb, filter = GeneNameFilter(selected_node_id))
      tx_ids <- Tx[1,1]
      row_id <- names(tx_ids)
      link <- paste0("https://www.ensembl.org/Homo_sapiens/Gene/Summary?g=", row_id)
      output$gene_graph_tab <- renderUI({
        a("Gene info", href=link, id='geneInfoLink')
      })
    }
  })
  
  # Link to external db confidence
  observe({
    if (!is.null(input$graph_conf_selected)&&input$graph_conf_selected!="") {
      selected_node_id <- input$graph_conf_selected
      edb <- EnsDb.Hsapiens.v86
      Tx <- transcripts(edb, filter = GeneNameFilter(selected_node_id))
      tx_ids <- Tx[1,1]
      row_id <- names(tx_ids)
      link <- paste0("https://www.ensembl.org/Homo_sapiens/Gene/Summary?g=", row_id)
      output$gene_graph_tab_conf <- renderUI({
        a("Gene info", href=link, id='geneInfoLink')
      })
    }
  })
  
  observeEvent(input$dir, {
    output$content <- renderUI({NULL})
  })
  
  observeEvent(input$tabName, {
    if(input$tabName == "input") {
      output$content <- renderUI({ NULL })
    }
  })
  
  ############################ Load or create project  ###########################
  
  ##Create
  
  # Function to change the tabPanel to "Data Input" when the "Create New Project" 
  observeEvent(input$create_project_button, {
    rv$dataFile=NULL
    default_values_create_project()
    updateTabItems(session, "sidebarMenu", "input")
  })
  
  # Show project names in "output_project" folder
  project_names <- reactive({
    get_project_names()
  })
  
  # Show the table with the names of existing projects
  output$projectList <- renderDT({
    project_data <- project_names()
    
    if (nrow(project_data) == 0) {
      project_data <- data.frame(Project_name = "There are no previously saved projects")
    } else {
      project_data$Data_type <- sapply(strsplit(project_data[,1], "_"), 
                                       function(x) paste(tail(x, 2), 
                                                         collapse = "_"))
      project_data$Data_type <- gsub("_", " ", project_data$Data_type) 
      project_data[,1] <- sub("_[^_]+$", "", sub("_[^_]+$", "", project_data[,1]))
      project_data <- project_data[, c(1, 3, 2)]
      project_data[,3] <- format(as.POSIXct(project_data[,3], 
                                            format = "%Y-%m-%dT%H:%M:%SZ"), 
                                 format = "%Y-%m-%d %H:%M:%S")
      
    }
    
    datatable(project_data, rownames = FALSE, 
              colnames = c("Project name", "Data type", "Last modified"), 
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
    buttonClicked(TRUE) 
  })
  
  # Upload saved project information
  observeEvent(input$loadProjBtn, {
    if (is.null(input$projectList_cell_clicked$row) || 
        is.null(input$projectList_cell_clicked$col)) {
      showNotification("Please select an existing project", type = "error")
      
    } else {
      # Visualise switch bottom
      output$switchViewBtn <- renderUI({
        actionButton("switchViewBtn", "Switch View", class = "custom-button")
      })
      
      # Management click on existing project table
      observeEvent(input$projectList_cell_clicked, {
        if (buttonClicked()) {
          default_values_load_genotype()
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
            
            # Read first the file of each case
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
              else if (file_name == "parameters_bulk_multiple") {
                parameters <- read.csv(file_directory)
                
                for (i in 1:nrow(parameters)) {
                  parametro <- parameters[i, "name"]
                  value <- parameters[i, "value"]
                  
                  # Assigns the value to the corresponding input
                  if (parametro == "binarization_perc") {
                    val_bin_perc <- as.numeric(value)
                    output$binarization_perc <- renderUI({
                      tagList(
                        div(style = "display: flex; align-items: center;",
                            numericInput("binarization_perc", 
                                         label = span("Filter to binarize percentage ", 
                                                      tags$i(id = "helpIcon4", 
                                                             class = "fa fa-question-circle", 
                                                             style="margin-left: 5px;")), 
                                         value = val_bin_perc, min = 0, max = 1, step = 0.01),
                            bsTooltip(id = "helpIcon4", 
                                      title = "Specify the threshold you want to use as a filter to binarize the percentage of mutated regions in each sample of the database to be inputted into the next inference phase.", 
                                      placement = "right", trigger = "hover")
                        )
                      )
                    })
                  }
                }
              }
            }
            
            # Read the file that are for all the 3 cases
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
                
                output$heatmapPlot <- renderUI({
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
              else if (file_name == "parameters") {
                parameters <- read.csv(file_directory)
                
                # Cycle through each row of the dataframe
                for (i in 1:nrow(parameters)) {
                  parametro <- parameters[i, "name"]
                  value <- parameters[i, "value"]
                  
                  # Assigns the value to the corresponding input
                  if (parametro == "binarization") {
                    val_bin <- as.numeric(value)
                    output$binarization <- renderUI({
                      tagList(
                        div(style = "display: flex; align-items: center;",
                            numericInput("binarization", 
                                         label = span("Filter to binarize ", 
                                                      tags$i(id = "helpIcon3", 
                                                             class = "fa fa-question-circle", 
                                                             style="margin-left: 5px;")), 
                                         value = val_bin, min = 0, max = 1, 
                                         step = 0.01),
                            bsTooltip(id = "helpIcon3", 
                                      title = "Specify which threshold you want to use as a filter to binarize the database to be inputted into the next inference phase.", 
                                      placement = "right", trigger = "hover")
                        )
                      )
                    })
                  }
                  else if (parametro =="del_col") {
                    vettore <- unlist(strsplit(gsub("\"", "", value), ",\\s*"))
                    del_col <- vettore[vettore != ""]
                    output$DeleteColumn <- render_delete_column_ui("DeleteColumn", 
                                                                   "Delete column", 
                                                                   reshaped_data(),
                                                                   selected_columns = del_col)
                    
                  }else if (parametro =="del_row") {
                    vettore <- unlist(strsplit(gsub("\"", "", value), ",\\s*"))
                    del_row <- vettore[vettore != ""]
                    output$DeleteRow <- render_delete_row_ui("DeleteRow", 
                                                             "Delete row", 
                                                             reshaped_data(),
                                                             selected_rows = del_row)
                  }else if (parametro == "flag_resampling") {
                    updateCheckboxInput(session, "resamplingFlag", 
                                        value = as.logical(value))
                  } else if (parametro == "nresampling") {
                    nresampling(as.numeric(value))
                    updateNumericInput(session, "nresampling", 
                                       value = as.numeric(value))
                  } else if (parametro == "restarts") {
                    updateNumericInput(session, "restarts", 
                                       value = as.numeric(value))
                  } else if (parametro == "regularization") {
                    elements <- unlist(strsplit(value, ", "))
                    updateSelectInput(session, "regularization", 
                                      selected = c(elements))
                  } else if (parametro == "command") {
                    updateSelectInput(session, "command", selected = value)
                  } else if (parametro == "seed") {
                    val_seed <- as.numeric(value)
                    updateNumericInput(session, "seed", value = val_seed)
                  } else if(parametro == "iteration") {
                    val_it <- as.numeric(value)
                    updateNumericInput(session, "iteration_confEstimation", value = val_it)
                  } else if (parametro == "flag_confidence") {
                    updateCheckboxInput(session, "resamplingFlag_conf", 
                                        value = as.logical(value))
                  } else if (parametro == "nresampling_confidence") {
                    nresampling_conf(as.numeric(value))
                    updateNumericInput(session, "nresampling_conf", 
                                       value = as.numeric(value))
                  }
                }
              }
              else if (file_name == "resampling_res") {
                resampling_res(readRDS(file_directory))
                res <- resampling_res()
                names_to_remove <- c("dataset", "models", "ccfDataset", "inference")
                names <- setdiff(names(res), names_to_remove)
                names <- c(names, names(res$inference))
                visualizeInferenceOutput(TRUE)
                output$visualize_inference <- renderUI({
                  selectInput("visualize_inference", 
                              "Output inference", 
                              c(names),
                              selected = "poset")  
                })
              }else if (file_name == "confidence_res") {
                conf_res(readRDS(file_directory))
                res <- conf_res()
                names_to_remove <- c("dataset", "models", "ccfDataset", "inference")
                names <- setdiff(names(res), names_to_remove)
                names <- c(names, names(res$inference))
                visualizeConfidenceOutput(TRUE)
                output$visualize_conf <- renderUI({
                  selectInput("visualize_conf", 
                              "Output Confidence", 
                              c(names),
                              selected = "poset")  
                })
              }
            }
          }
        }
      })
      
      updateTabItems(session, "sidebarMenu", "input")
      
      selected_row <- input$projectList_rows_selected
      project_data <- project_names()
      project_name <- project_data$project_names[selected_row]
      project_name <- sub("_[^_]+$", "", sub("_[^_]+$", "", project_name))
      output$project_info <- renderUI({
        tags$div(paste("Project:", project_name))
      })
    }
  })
  
  # Function to render UI for deleting columns
  render_delete_column_ui <- function(input_id, label, data, selected_columns = NULL) {
    output <- renderUI({
      selectInput(input_id, label, choices = colnames(data), 
                  selected = selected_columns, multiple = TRUE)
    })
    return(output)
  }
  
  # Function to render UI for deleting rows
  render_delete_row_ui <- function(input_id, label, data, selected_rows = NULL) {
    output <- renderUI({
      selectInput(input_id, label, choices = rownames(data), 
                  selected = selected_rows, multiple = TRUE)
    })
    return(output)
  }
  
  ############################ Input data  #######################################
  
  # Displaying the resampling file, in the case where there are three columns 
  #(single bulk case) the table is represented.
  #in other cases an error message is printed
  observeEvent(input$loadBtn2, {
    #resampling file
    inFile2 <- input$dataFile2
    
    if (is.null(inFile2)) {
      showNotification("Please select a file", type = "error")
    } else {
      data2 <- read.table(inFile2$datapath, sep = "\t", header = TRUE, 
                          stringsAsFactors = FALSE)
      
      if (!"REF_COUNT" %in% colnames(data2)) {
        showNotification("Select the correct resampling file", 
                         type = "error")
      } else {
        reshaped_data2(data2)
        output$dataTable2 <- renderDT({
          datatable(data2, options = list(scrollX = TRUE), selection ="single")
        })
        
        reshaped_data2(data2)
      }
    }
  })
  
  reshaped_data <- reactiveVal(NULL)
  
  # Displaying the genotype file
  observeEvent(input$loadBtn, {
    inFile <- input$dataFile
    
    if (is.null(inFile)) {
      showNotification("Please select a file", type = "error")
    } else {
      data <- read.table(inFile$datapath, sep = "\t", header = TRUE, 
                         stringsAsFactors = FALSE)
      
      #### Bulk single biopsy
      if (ncol(data) == 3) {
        default_values_load_new_genotype()
        case("bulk_single")
        data <- distinct(data, SAMPLE, GENE, .keep_all = TRUE)
        reshaped_data(
          acast(data, SAMPLE ~ GENE, value.var = "CCF", fill = 0)
        )
        
        output$binarization <- renderUI({
          tagList(
            div(style = "display: flex; align-items: center;",
                numericInput("binarization", 
                             label = span("Filter to binarize ", 
                                          tags$i(id = "helpIcon3", 
                                                 class = "fa fa-question-circle", 
                                                 style="margin-left: 5px;")), 
                             value = 1, min = 0, max = 1, step = 0.01),
                bsTooltip(id = "helpIcon3", 
                          title = "Specify which threshold you want to use as a filter to binarize the database to be inputted into the next inference phase.", 
                          placement = "right", trigger = "hover")
            )
          )
        })
        output$switchViewBtn <- renderUI({
          actionButton("switchViewBtn", "Switch View", class = "custom-button")
        })
        
        bulk_single_case()
        
      } else if (ncol(data) == 4) {   #### Bulk multipla biopsia o single cell 
        if (colnames(data)[2]=="REGION") {
          default_values_load_new_genotype()
          
          case("bulk_multiple")
          # Remove duplicates by keeping only the first occurrence for each position
          data <- distinct(data, SAMPLE, REGION, GENE, .keep_all = TRUE)
          
          reshaped_data(
            data %>%
              group_by(SAMPLE, REGION, GENE) %>%
              summarise(CCF = sum(CCF)) %>%
              unite("ID", c("SAMPLE", "REGION"), sep = "\t") %>%
              pivot_wider(names_from = GENE, values_from = CCF, values_fill = 0) %>%
              dplyr::select(-ID, everything()) %>% 
              column_to_rownames(var = "ID")
          )
          
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
            output$heatmapPlot <- renderUI({
              generate_heatmap_plot(modified_data)
            })
          })
          
          output$binarization <- renderUI({
            tagList(
              div(style = "display: flex; align-items: center;",
                  numericInput("binarization", 
                               label = span("Filter to binarize ", 
                                            tags$i(id = "helpIcon3", 
                                                   class = "fa fa-question-circle", 
                                                   style="margin-left: 5px;")), 
                               value = 1, min = 0, max = 1, step = 0.01),
                  bsTooltip(id = "helpIcon3", 
                            title = "Specify which threshold you want to use as a filter to binarize the database to be inputted into the next inference phase.", 
                            placement = "right", trigger = "hover")
              )
            )
          })
          
          output$switchViewBtn <- renderUI({
            actionButton("switchViewBtn", "Switch View", class = "custom-button")
          })
          
          output$binarization_perc <- renderUI({
            tagList(
              div(style = "display: flex; align-items: center;",
                  numericInput("binarization_perc", 
                               label = span("Filter to binarize percentage ", 
                                            tags$i(id = "helpIcon4", 
                                                   class = "fa fa-question-circle", 
                                                   style="margin-left: 5px;")), 
                               value = 1, min = 0, max = 1, step = 0.01),
                  bsTooltip(id = "helpIcon4", 
                            title = "Specify the threshold you want to use as a filter to binarize the percentage of mutated regions in each sample of the database to be inputted into the next inference phase.", 
                            placement = "right", trigger = "hover")
              )
            )
          })
          
          handle_dataTable_cell_clicked(reshaped_data(), "ID")
          
          output$directoryInput <- renderUI({
            tagList(
              div(style = "display: flex; align-items: center; gap: 5px;",
                  shinyDirButton("dir", "Select a folder", title = "Select a folder", 
                                 multiple = FALSE),
                  tags$i(id = "helpIconFolder", class = "fa fa-question-circle", 
                         style="cursor: pointer;", `data-toggle`="tooltip", 
                         `data-placement`="right",
                         title="To perform the inference operation, it's necessary to select the folder containing the files corresponding to each row of the database along with its respective Directed Acyclic Graph (DAG).")
              )
            )
          })
          
          observe({
            req(input$dir)
            selected_folder(parseDirPath(c(wd = getwd()), input$dir))
          })
          
          shinyDirChoose(input, "dir", roots = c(wd = getwd()), 
                         filetypes = c("", "txt"))
          
        } else if (colnames(data)[2]=="CELL") {
          default_values_load_new_genotype()
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
          
          output$DeleteColumn <- render_delete_column_ui("DeleteColumn", 
                                                         "Delete column", 
                                                         reshaped_data())
          
          output$DeleteRow <- render_delete_row_ui("DeleteRow", 
                                                   "Delete row", 
                                                   reshaped_data())
          
          observe({
            modified_data <- modify_reshaped_data(reshaped_data())
            output$dataTable <- renderDT({
              datatable(modified_data, options = list(scrollX = TRUE), 
                        selection ="single")
            })
            reshaped_data(modified_data)
            output$heatmapPlot <- renderUI({
              generate_heatmap_plot(reshaped_data())
            })
          })
          
          output$binarization <- renderUI({
            tagList(
              div(style = "display: flex; align-items: center;",
                  numericInput("binarization", 
                               label = span("Filter to binarize ", 
                                            tags$i(id = "helpIcon3", 
                                                   class = "fa fa-question-circle", 
                                                   style="margin-left: 5px;")), 
                               value = 1, min = 0, max = 1, step = 0.01),
                  bsTooltip(id = "helpIcon3", 
                            title = "Specify which threshold you want to use as a filter to binarize the database to be inputted into the next inference phase.", 
                            placement = "right", trigger = "hover")
              )
            )
          })
          output$switchViewBtn <- renderUI({
            actionButton("switchViewBtn", "Switch View", class = "custom-button")
          })
          
          handle_dataTable_cell_clicked(reshaped_data(), "PATIENT")
          
          output$directoryInput <- renderUI({
            tagList(
              div(style = "display: flex; align-items: center; gap: 5px;",
                  shinyDirButton("dir", "Select a folder", title = "Select a folder", 
                                 multiple = FALSE),
                  tags$i(id = "helpIconFolder", class = "fa fa-question-circle", 
                         style="cursor: pointer;", `data-toggle`="tooltip", 
                         `data-placement`="right",
                         title="To perform the inference operation, it's necessary to select the folder containing the files corresponding to each row of the database along with its respective Directed Acyclic Graph (DAG).")
              )
            )
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
  
  ############################ Inference  ########################################
  
  binarize_table <- function(filter, filter_perc, reshaped_data) {
    genotype_table(reshaped_data)
    
    if(is.na(filter)) {
      showNotification("Enter a valid value in the binarization field in the previous step", type = "error")
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
  }
  
  # Inference btn
  observeEvent(input$submitBtn, {
    default_values_inference()
    filter <- input$binarization
    filter_perc <- input$binarization_perc
    
    # filter the genotype table according to the case
    if (is.null(case())) {
      showNotification("Upload the genomic file in the previous step", type = "error")
    }
    else if ((input$resamplingFlag == FALSE) && (is.null(input$regularization)
                                                 ||is.null(input$command) || 
                                                 is.na(input$restarts) || is.na(input$seed))){
      showNotification("Fill in all fields", type = "error")
    }
    else if ((input$resamplingFlag == TRUE) && (is.null(input$regularization)
                                                ||is.null(input$command) || 
                                                is.na(input$restarts) || is.na(input$seed) || 
                                                is.na(input$nresampling))){
      showNotification("Fill in all fields", type = "error")
    }
    else {
      binarize_table(filter, filter_perc, reshaped_data_matrix())
      updateTabItems(session, "sidebarMenu", "inference")
      
      res <- NULL
      
      #inference function
      if (!is.null(case()) && !is.na(filter)) {
        tryCatch({
          
          nsampling <- input$nresampling
          set.seed(input$seed)
          if(case()=="bulk_single") {
            column <- intersect(colnames(genotype_table()), colnames(reshaped_data()))
            row <- intersect(rownames(genotype_table()), rownames(reshaped_data()))
            reshaped_data_inference(subset(reshaped_data(), rownames(reshaped_data()) %in% row, select = column))
            if (input$resamplingFlag == FALSE) {
              
              progress <- withProgress(
                message = 'Ongoing calculation...',
                detail = 'This may take some time...',
                value = 0, {
                  res <- asceticCCF(
                    dataset = genotype_table(),
                    ccfDataset = reshaped_data_inference(),
                    regularization = input$regularization,
                    command = input$command, 
                    restarts = input$restarts
                  )
                }
              )
            } else {
              progress <- withProgress(
                message = 'Ongoing calculation...',
                detail = 'This may take some time...',
                value = 0, {
                  res <- asceticCCFResampling(
                    dataset = genotype_table(),
                    ccfDataset = reshaped_data_inference(),
                    vafDataset = reshaped_data2(),
                    nsampling = nsampling,
                    regularization = input$regularization,
                    command = input$command, 
                    restarts = input$restarts
                  )
                }
              )
            }
          }
          else if((case()=="bulk_multiple" && !is.na(filter_perc))|| case() == "single_cell") {
            models <- readMatrixFiles(selected_folder())
            models_filtrati_dati <- models
            if (!is.null(input$DeleteRow)) {
              modelli_da_rimuovere <- unlist(strsplit(input$DeleteRow, ","))
              models_filtrati <- setdiff(names(models), modelli_da_rimuovere)
              models_filtrati_dati <- models[models_filtrati]
            }
            
            if (!is.null(input$DeleteColumn)) {
              for (modello in names(models_filtrati_dati)) {
                file_data <- models_filtrati_dati[[modello]]
                file_data <- as.data.frame(file_data)
                
                if (length(input$DeleteColumn) > 0) {
                  for (col in input$DeleteColumn) {
                    if (col %in% colnames(file_data)) {
                      col_index <- which(colnames(file_data) == col)
                      
                      # nodo foglia
                      if (any(file_data[, col_index] == 1) && all(file_data[col_index, -col_index] == 0)) {
                        file_data[file_data[, col_index] == 1, col_index] <- 0
                      }
                      
                      # nodo radice
                      if (any(file_data[col_index, -col_index] == 1) && all(file_data[, col_index] == 0)) {
                        file_data[col_index, -col_index][file_data[col_index, -col_index] == 1] <- 0
                      }
                      
                      # nodo interno
                      if (any(file_data[, col_index] == 1) && any(file_data[col_index, -col_index] == 1)) {
                        indici_riga <- which(file_data[, col_index] == 1)
                        for (indice in indici_riga) {
                          file_data[indice, -col_index] <- file_data[indice, -col_index] | file_data[col_index, -col_index]
                        }
                        file_data[indici_riga, col_index] <- 0
                        file_data[col_index, -col_index] <- 0
                      }
                      file_data <- file_data[-col_index, -col_index]
                    }
                  }
                }
                file_data <- as.matrix(file_data)
                models_filtrati_dati[[modello]] <- file_data
              }
            }
            
            if (is.null(selected_folder())) {
              showNotification("Select the folder in the previous step", type = "error")
            } else if (input$resamplingFlag == FALSE) {
              
              progress <- withProgress(
                message = 'Ongoing calculation...',
                detail = 'This may take some time...',
                value = 0, {
                  res <- asceticPhylogenies(
                    dataset = genotype_table(),
                    models = models_filtrati_dati,
                    regularization = input$regularization,
                    command = input$command,
                    restarts = input$restarts
                  )
                }
              )
            } else {
              
              progress <- withProgress(
                message = 'Ongoing calculation...',
                detail = 'This may take some time...',
                value = 0, {
                  res <- asceticPhylogeniesBootstrap(
                    dataset = genotype_table(),
                    models = models_filtrati_dati,
                    nsampling = nsampling,
                    regularization = input$regularization,
                    command = input$command,
                    restarts = input$restarts
                  )
                }
              )
            }
          }
          else {
            showNotification("Enter a valid value in the percentage binarization field in the previous step", type = "error")
          }
          resampling_res(res)
          
          if(!is.null(res)) {
            
            names_to_remove <- c("dataset", "models", "ccfDataset", "inference")
            names <- setdiff(names(res), names_to_remove)
            names <- c(names, names(res$inference))
            visualizeInferenceOutput(TRUE)
            output$visualize_inference <- renderUI({selectInput("visualize_inference", 
                                                                "Output inference", 
                                                                c(names),
                                                                selected = "poset")})
          }
        }, error = function(e) {
          visualizeInferenceOutput(FALSE)
          output$visualize_inference <- NULL
          output$selected_result_output <- NULL
          output$graph_inference <- NULL
          showNotification("Select the correct folder in the previous step", type = "error")
        })
      }
    }
  })
  
  # Callback function to handle click on input resamplingFlag
  observeEvent(input$resamplingFlag, {
    # Check whether the resampling flag has been activated.
    if (input$resamplingFlag) {
      # Check if case is not null and if it's bulk_single, whether the file is loaded
      if (!is.null(case()) && (case() != "bulk_single" || 
                               !is.null(input$dataTable2) || 
                               !is.null(reshaped_data2()))) {
        if (!is.null(nresampling())) {
          output$nresampling <- renderUI({
            numericInput("nresampling", "Number of samplings", nresampling(), min = 3)
          })
        } else {
          output$nresampling <- renderUI({
            numericInput("nresampling", "Number of samplings", 3, min = 3)
          })
        }
      } else {
        # If the condition is not met, show a warning
        showNotification("Load the resampling file in the previous step", type = "warning")
        # Update resamplingFlag back to FALSE
        updateCheckboxInput(session, "resamplingFlag", value = FALSE)
      }
    } else {
      # If resamplingFlag is off, remove the nresampling input
      output$nresampling <- NULL
    }
  })
  
  # Display the inference output
  observe({
    req(input$sidebarMenu == "inference")
    req(input$visualize_inference)
    res <- resampling_res()
    output$selected_result_output <- NULL
    col_names <- colnames(res$dataset)
    
    if(visualizeInferenceOutput()) {
      if (input$visualize_inference %in% names(res)) {
        selected_result <- res[[input$visualize_inference]]
      } else {
        selected_result <- res$inference[[input$visualize_inference]]
      }
      
      if (input$visualize_inference == "rankingEstimate") {
        
        output$graph_inference <- NULL
        selected_result[, "variable"] <- row.names(selected_result)
        selected_result[, "rank"] <- as.integer(selected_result[, "rank"]) + 1
        colnames(selected_result)[1] <- "genes"
        rownames(selected_result) <- NULL
        reactive_selected_result (selected_result)
        output$selected_result_output <- renderDT({
          datatable(reactive_selected_result(), options = list(scrollX = TRUE), rownames = FALSE, selection = "single")
        })
      } else if (input$visualize_inference == "poset"){
        colnames(selected_result) <- col_names
        rownames(selected_result) <- col_names
        output$graph_inference <- NULL
        reactive_selected_result (selected_result)
        output$selected_result_output <- renderDT({
          datatable(reactive_selected_result(), options = list(scrollX = TRUE), rownames = TRUE, selection = "single")
        })
      } else {
        colnames(selected_result) <- col_names
        rownames(selected_result) <- col_names
        if (all(selected_result == 0)) {
          showNotification("No DAG available", type = "message")
          output$graph_inference <- NULL
        } else {
          reactive_selected_result(selected_result)
          grafo <- graph_from_adjacency_matrix(selected_result)
          
          output$graph_inference <- renderVisNetwork({
            nodi_da_rimuovere <- V(grafo)[degree(grafo, mode = "in") == 0 & 
                                            degree(grafo, mode = "out") == 0]
            grafo <- delete.vertices(grafo, nodi_da_rimuovere)
            nodes <- as_tibble(get.vertex.attribute(grafo))
            colnames(nodes) <- "id"
            nodes <- data.frame(nodes, label= nodes$id)
            edges <- as_tibble(as_edgelist(grafo))
            colnames(edges) <- c("from", "to")
            
            generateVisNetwork(nodes, edges, "other", "")
          })
        }
      }
    }
  })
  
  ############################ Confidence estimation #############################
  
  # Callback function to handle click on input resamplingFlag
  observeEvent(input$submitBtn_confEstimation, {
    if (is.null(resampling_res())) {
      showNotification("Perform the inference phase first", type = "message")
    } 
    else if (is.null(case())) {
      showNotification("Upload the genomic file in the previous step", type = "error")
    }
    else if (is.null(input$iteration_confEstimation) || ((input$resamplingFlag_conf == TRUE) && (is.null(input$nresampling_conf)))){
      showNotification("Fill in all fields", type = "error")
    }
    else{
      
      if(case()=="bulk_single") {
        if(input$resamplingFlag_conf == FALSE) {
          res <- asceticCCFAssessment(
            inference = resampling_res(),
            niterations = input$iteration_confEstimation
          )
        }
        else {
          res <- asceticCCFAssessment(
            inference = resampling_res(),
            niterations = input$iteration_confEstimation,
            vafDataset = reshaped_data2(),
            nsampling = input$nresampling_conf
          )
        }
      }
      else{
        if(input$resamplingFlag_conf == FALSE) {
          res <- asceticPhylogeniesAssessment(
            inference = resampling_res(),
            niterations = input$iteration_confEstimation
          )
        }
        else {
          res <- asceticPhylogeniesAssessment(
            inference = resampling_res(),
            niterations = input$iteration_confEstimation,
            nsampling = input$nresampling_conf
          )
        }
      }
      conf_res(res)
      if(!is.null(res)) {
        
        names_to_remove <- c("dataset", "models", "ccfDataset", "inference")
        names <- setdiff(names(res), names_to_remove)
        names <- c(names, names(res$inference))
        visualizeConfidenceOutput(TRUE)
        output$visualize_conf <- renderUI({selectInput("visualize_conf", 
                                                       "Output confidence", 
                                                       c(names),
                                                       selected = "poset")})
      }
    }
  })
  
  # Display the confidence estimation  output
  observe({
    req(input$sidebarMenu == "confidence_estimation")
    req(input$visualize_conf)
    res <- conf_res()
    output$selected_result_output_conf <- NULL
    col_names <- colnames(res$dataset)
    
    if(visualizeConfidenceOutput()) {
      if (input$visualize_conf %in% names(res)) {
        selected_result <- res[[input$visualize_conf]]
      } else {
        selected_result <- res$inference[[input$visualize_conf]]
      }
      
      if (input$visualize_conf == "ranking") {
        ranking_df <- as.data.frame(selected_result)
        ranking_df <- data.frame(genes = rownames(ranking_df), rank = ranking_df$selected_result)
        
        reactive_selected_result_conf (ranking_df)
        output$selected_result_output_conf <- renderDT({
          datatable(
            reactive_selected_result_conf(),
            options = list(
              scrollX = TRUE,
              columnDefs = list(list(targets = 1, className = 'dt-body-left'))  
            ),
            rownames = FALSE,
            selection = "single"
          )
        })
        
      } else if (input$visualize_conf == "poset"){
        output$graph_conf <- NULL
        reactive_selected_result_conf (selected_result)
        output$selected_result_output_conf <- renderDT({
          datatable(reactive_selected_result_conf(), options = list(scrollX = TRUE), rownames = TRUE, selection = "single")
        })
      } else {
        
        if (all(selected_result == 0)) {
          showNotification("No DAG available", type = "message")
          output$graph_conf <- NULL
        } else {
          reactive_selected_result_conf(selected_result)
          grafo <- graph_from_adjacency_matrix(selected_result)
          
          output$graph_conf <- renderVisNetwork({
            nodi_da_rimuovere <- V(grafo)[degree(grafo, mode = "in") == 0 & 
                                            degree(grafo, mode = "out") == 0]
            grafo <- delete.vertices(grafo, nodi_da_rimuovere)
            nodes <- as_tibble(get.vertex.attribute(grafo))
            colnames(nodes) <- "id"
            nodes <- data.frame(nodes, label= nodes$id)
            edges <- as_tibble(as_edgelist(grafo))
            colnames(edges) <- c("from", "to")
            
            generateVisNetwork(nodes, edges, "other", "")
          })
        }
      }
    }
  })
  
  
  # Callback function to handle click on input resamplingFlag
  observeEvent(input$resamplingFlag_conf, {
    # Check whether the resampling flag has been activated.
    if (input$resamplingFlag_conf) {
      # Check if case is not null and if it's bulk_single, whether the file is loaded
      if (!is.null(case()) && (case() != "bulk_single" || 
                               !is.null(input$dataTable2) || 
                               !is.null(reshaped_data2()))) {
        if (!is.null(nresampling_conf())) {
          output$nresampling_conf <- renderUI({
            numericInput("nresampling_conf", "Number of samplings", nresampling_conf(), min = 3)
          })
        } else {
          output$nresampling_conf <- renderUI({
            numericInput("nresampling_conf", "Number of samplings", 3, min = 3)
          })
        }
      } else {
        # If the condition is not met, show a warning
        showNotification("Load the resampling file in the previous step", type = "warning")
        # Update resamplingFlag_conf back to FALSE
        updateCheckboxInput(session, "resamplingFlag_conf", value = FALSE)
      }
    } else {
      # If resamplingFlag_conf is off, remove the nresampling_conf input
      output$nresampling_conf <- NULL
    }
  })
  
  ############################ Input survival  #################################
  
  observeEvent(input$submit_surv, {
    if (is.null(conf_res())) {
      showNotification("Perform the confidence phase first", type = "message")
    } else {
      result <- conf_res()
      regularization <- input$regularization_surv
      matrix <- result$inference[[regularization]]
      
      positions_one <- which(matrix == 1, arr.ind = TRUE)
      
      pairs <- data.frame(
        Genes = apply(positions_one, 1, function(idx) paste(rownames(matrix)[idx[1]], ".to.", colnames(matrix)[idx[2]], sep="")),
        stringsAsFactors = FALSE
      )
      
      targets <- sapply(strsplit(pairs$Genes, "\\.to\\."), function(x) x[2])
      
      incoming_edges <- colSums(matrix[, targets] > 0)
      
      genes_no_incoming <- names(incoming_edges[incoming_edges == 0])
      
      pairs$Genes <- ifelse(targets %in% genes_no_incoming, 
                            paste("Root.to", targets, sep = "."),
                            pairs$Genes)
      
      split_names <- strsplit(pairs$Genes, "\\.to\\.")
      sources <- sapply(split_names, `[`, 1)
      destinations <- sapply(split_names, `[`, 2)
      
      sources_no_destination <- sources[!sources %in% destinations]
      
      root_entries <- paste("Root.to", sources_no_destination, sep = ".")
      
      new_pairs <- data.frame(Genes = root_entries, stringsAsFactors = FALSE)
      pairs <- rbind(pairs, new_pairs)
      pairs <- unique(pairs)
      
      if (input$load_file) {
        if (is.null(reshaped_dataSurv())) {
          showNotification("Please select a genotype file", type = "error")
        }
        else {
          filter_value <- input$binarization_surv
          filter_perc <- input$binarization_percSurv
          
          
          binarize_table_surv(filter_value, filter_perc, reshaped_dataSurv())
          
          mutation_db <- genotype_table_surv()
          
        }
      } else {
        
        filter_value <- input$binarization
        filter_perc <- input$binarization_perc
        
        
        binarize_table(filter_value, filter_perc, reshaped_data_matrix())
        mutation_db <- genotype_table()
      }
      
      
      output_db <- matrix(0, nrow = nrow(mutation_db), ncol = length(pairs$Genes), 
                          dimnames = list(rownames(mutation_db), pairs$Genes))

      for (i in seq_len(ncol(output_db))) {
        pair <- strsplit(colnames(output_db)[i], "\\.to\\.")[[1]]
        if (pair[1] == "Root") {
          if (pair[2] %in% colnames(mutation_db)) {
            output_db[, i] <- as.integer(mutation_db[, pair[2]] == 1)
          } else {
            output_db[, i] <- 0  
          }
        } else {
          if (all(c(pair[1], pair[2]) %in% colnames(mutation_db))) {
            output_db[, i] <- as.integer((mutation_db[, pair[1]] == 1) & (mutation_db[, pair[2]] == 1))
          } else {
            output_db[, i] <- 0  
          }
        }
      }
    
      
      # Checking for all-zero or all-one columns
      all_zero_one <- apply(output_db, 2, function(col) all(col == 0) || all(col == 1))
      if (any(all_zero_one)) {
        removed_cols <- colnames(output_db)[which(all_zero_one)]
        showNotification(paste("Columns", paste(removed_cols, collapse = " "), "have been removed"), type = "message")
        output_db <- output_db[, !all_zero_one]
      }
      
      
      output$dataTable_surv <- renderDT({
        datatable(output_db, options = list(scrollX = TRUE), 
                  selection = "single")
      })
      
    }
  })
  
  
  observeEvent(input$loadBtn_surv, {
    inFile2 <- input$dataFile_surv
    
    if (is.null(inFile2)) {
      showNotification("Please select a file", type = "error")
    } else {
      data <- read.table(inFile2$datapath, sep = "\t", header = TRUE, 
                         stringsAsFactors = FALSE)
      
      #### Bulk single biopsy
      if (ncol(data) == 3) {
        data <- distinct(data, SAMPLE, GENE, .keep_all = TRUE)
        reshaped_dataSurv(
          acast(data, SAMPLE ~ GENE, value.var = "CCF", fill = 0)
        )
        
        output$dataTable_GenotypeSurv <- renderDT({
          datatable(reshaped_dataSurv(), options = list(scrollX = TRUE), 
                    selection = "single")
        })
        
        output$binarization_surv <- renderUI({
          tagList(
            div(style = "display: flex; align-items: center;",
                numericInput("binarization_surv", 
                             label = span("Filter to binarize ", 
                                          tags$i(id = "helpIcon3", 
                                                 class = "fa fa-question-circle", 
                                                 style="margin-left: 5px;")), 
                             value = 1, min = 0, max = 1, step = 0.01),
                bsTooltip(id = "helpIcon3", 
                          title = "Specify which threshold you want to use as a filter to binarize the database to be inputted into the next inference phase.", 
                          placement = "right", trigger = "hover")
            )
          )
        })
      }
      else if (ncol(data) == 4) {   
        if (colnames(data)[2]=="REGION") {
          data <- distinct(data, SAMPLE, REGION, GENE, .keep_all = TRUE)
          
          reshaped_dataSurv(
            data %>%
              group_by(SAMPLE, REGION, GENE) %>%
              summarise(CCF = sum(CCF)) %>%
              unite("ID", c("SAMPLE", "REGION"), sep = "\t") %>%
              pivot_wider(names_from = GENE, values_from = CCF, values_fill = 0) %>%
              dplyr::select(-ID, everything()) %>% 
              column_to_rownames(var = "ID")
          )
          
          output$dataTable_GenotypeSurv <- renderDT({
            datatable(reshaped_dataSurv(), options = list(scrollX = TRUE), 
                      selection = "single")
          })
          
          output$binarization_surv <- renderUI({
            tagList(
              div(style = "display: flex; align-items: center;",
                  numericInput("binarization_surv", 
                               label = span("Filter to binarize ", 
                                            tags$i(id = "helpIcon3", 
                                                   class = "fa fa-question-circle", 
                                                   style="margin-left: 5px;")), 
                               value = 1, min = 0, max = 1, step = 0.01),
                  bsTooltip(id = "helpIcon3", 
                            title = "Specify which threshold you want to use as a filter to binarize the database to be inputted into the next inference phase.", 
                            placement = "right", trigger = "hover")
              )
            )
          })
          
          
          output$binarization_percSurv <- renderUI({
            tagList(
              div(style = "display: flex; align-items: center;",
                  numericInput("binarization_perc", 
                               label = span("Filter to binarize percentage ", 
                                            tags$i(id = "helpIcon4", 
                                                   class = "fa fa-question-circle", 
                                                   style="margin-left: 5px;")), 
                               value = 1, min = 0, max = 1, step = 0.01),
                  bsTooltip(id = "helpIcon4", 
                            title = "Specify the threshold you want to use as a filter to binarize the percentage of mutated regions in each sample of the database to be inputted into the next inference phase.", 
                            placement = "right", trigger = "hover")
              )
            )
          })
        }
        else if (colnames(data)[2]=="CELL") {
          data <- distinct(data, PATIENT, CELL, GENE, .keep_all = TRUE)
          
          data <- data %>%
            group_by(PATIENT, GENE) %>%
            summarise(PERCENTAGE = mean(VALUE)) %>%
            pivot_wider(names_from = GENE, values_from = PERCENTAGE, 
                        values_fill = list(PERCENTAGE = 0))
          
          reshaped_dataSurv(data %>%
                              column_to_rownames(var = "PATIENT"))
          
          output$dataTable_GenotypeSurv <- renderDT({
            datatable(reshaped_dataSurv(), options = list(scrollX = TRUE), 
                      selection = "single")
          })
          
          output$binarization_surv <- renderUI({
            tagList(
              div(style = "display: flex; align-items: center;",
                  numericInput("binarization_surv", 
                               label = span("Filter to binarize ", 
                                            tags$i(id = "helpIcon3", 
                                                   class = "fa fa-question-circle", 
                                                   style="margin-left: 5px;")), 
                               value = 1, min = 0, max = 1, step = 0.01),
                  bsTooltip(id = "helpIcon3", 
                            title = "Specify which threshold you want to use as a filter to binarize the database to be inputted into the next inference phase.", 
                            placement = "right", trigger = "hover")
              )
            )
          })
        }
      }
    }
  })
  
  binarize_table_surv <- function(filter, filter_perc, reshaped_data) {
    genotype_table_surv(reshaped_data)
    
    if(is.na(filter)) {
      showNotification("Enter a valid value in the binarization field", type = "error")
    }
    else if (case() == "bulk_single" || case() == "single_cell") {
      #filter the genotype_table_surv table
      genotype_table_surv(ifelse(genotype_table_surv() >= filter, 1, 0))
    }
    else if (case() == "bulk_multiple") {
      genotype_table_surv(ifelse(genotype_table_surv() >= filter, 1, 0))
      
      df <- as.data.frame(genotype_table_surv())
      
      df <- df %>%
        mutate(id = sapply(strsplit(row.names(df), "\t"), `[`, 1))
      
      result <- df %>%
        group_by(id) %>%
        summarize(across(everything(), ~mean(. != 0)))
      
      result <- result %>%
        column_to_rownames(var = "id")
      
      genotype_table_surv(ifelse(result >= filter_perc, 1, 0))
      
    }
  }
  
  
  
  
  
  ############################ Save project  ################################# 
  
  observe({
    if (input$project_name != "") {
      shinyjs::enable("saveBtn")
    } else {
      shinyjs::disable("saveBtn")
    }
  })
  
  # Manage save btn
  observeEvent(input$saveBtn, {
    project_name <- input$project_name
    project_name <- paste0(project_name, "_", case())
    directory_output <- "output_project/"
    directory_complete <- paste0(directory_output, project_name)
    if (input$project_name == "") {
      showNotification("Enter a name for the project to be saved", type = "warning")
    }
    else if(!(is.null(case()))) {
      if (!file.exists(directory_complete)) {
        dir.create(directory_complete, recursive = TRUE)
      }
      directory_file <- paste0(directory_complete, "/genotipo.csv")
      saveData(reshaped_data() , directory_file)
      values <- c("binarization", "del_col", "del_row", "flag_resampling", 
                  "nresampling", "restarts", "regularization", "command", 
                  "seed", "iteration", "flag_confidence", "nresampling_confidence", "output_inference")
      
      sequence <- ifelse((!is.null(input$binarization)), input$binarization, NA)
      sequence <- c(sequence, ifelse((!is.null(input$DeleteColumn)), 
                                     toString(input$DeleteColumn), NA))
      sequence <- c(sequence, ifelse((!is.null(input$DeleteRow)), 
                                     toString(input$DeleteRow), NA))
      sequence <- c(sequence, ifelse((!is.null(input$resamplingFlag)), 
                                     input$resamplingFlag, NA))
      sequence <- c(sequence, ifelse((!is.null(input$nresampling)), 
                                     input$nresampling, NA))
      sequence <- c(sequence, ifelse((!is.null(input$restarts)), 
                                     input$restarts, NA))
      sequence <- c(sequence, ifelse((!is.null(input$regularization)), 
                                     toString(input$regularization), NA))
      sequence <- c(sequence, ifelse((!is.null(input$command)), 
                                     input$command, NA))
      sequence <- c(sequence, ifelse((!is.null(input$seed)), input$seed, NA))
      
      sequence <- c(sequence, ifelse((!is.null(input$iteration_confEstimation)), input$iteration_confEstimation, NA))
      
      sequence <- c(sequence, ifelse((!is.null(input$resamplingFlag_conf)), input$resamplingFlag_conf, NA))
      
      sequence <- c(sequence, ifelse((!is.null(input$nresampling_conf)), input$nresampling_conf, NA))
      
      
      
      
      matrix_data <- matrix(data = c(values, sequence),
                            nrow = length(values),
                            ncol = 2,
                            byrow = FALSE)
      
      matrix_dataframe <- as.data.frame(matrix_data)
      colnames(matrix_dataframe) <- c("name", "value")
      
      directory_file <- paste0(directory_complete, "/parameters.csv")
      saveData(matrix_dataframe , directory_file)
      
      if(!is.null(resampling_res())) {
        directory_file <- paste0(directory_complete, "/resampling_res.rds")
        saveRDS(resampling_res(), directory_file)
      }
      
      if(!is.null(conf_res())) {
        directory_file <- paste0(directory_complete, "/confidence_res.rds")
        saveRDS(conf_res(), directory_file)
      }
      
      
      if (case()=="bulk_single") {
        directory_file <- paste0(directory_complete, "/resampling_table.csv")
        if (!(is.null(reshaped_data2()))) {
          saveData(reshaped_data2() , directory_file)
        }
      } else if (case()=="bulk_multiple") {
        
        values <- c("directory", "binarization_perc")
        sequence <- ifelse(!is.null(input$dir), toString(input$dir), NA)
        sequence <- c(sequence, ifelse(!is.null(input$binarization_perc), 
                                       input$binarization_perc, NA))
        
        matrix_data <- matrix(data = c(values, sequence),
                              nrow = length(values),
                              ncol = 2,
                              byrow = FALSE)
        
        matrix_dataframe <- as.data.frame(matrix_data)
        colnames(matrix_dataframe) <- c("name", "value")
        
        directory_file <- paste0(directory_complete, "/parameters_bulk_multiple.csv")
        saveData(matrix_dataframe , directory_file)
        
      } else if (case()=="single_cell") {
        values <- c("directory")
        
        sequence <- ifelse(!is.null(input$dir), toString(input$dir), NA)
        
        matrix_data <- matrix(data = c(values, sequence),
                              nrow = length(values),
                              ncol = 2,
                              byrow = FALSE)
        matrix_dataframe <- as.data.frame(matrix_data)
        colnames(matrix_dataframe) <- c("name", "value")
        
        directory_file <- paste0(directory_complete, "/parameters_single_cell.csv")
        saveData(matrix_dataframe , directory_file)
      }
      
      showNotification("Project saved", type = "message")
    }
    else {
      showNotification("Nothing has been uploaded to save", type = "warning")
    }
  })
}
