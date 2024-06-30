source("libraries.R")

server <- function(input, output, session) {
  
  ############################ Variables  ######################################
  
  #reactive var
  selected_folder <- reactiveVal(NULL)
  directory_output <- reactiveVal(NULL)
  reshaped_data <- reactiveVal(NULL)
  reshaped_data_inference <- reactiveVal(NULL)
  genotype_table <- reactiveVal(NULL)
  case <- reactiveVal(NULL)
  case_surv <- reactiveVal(NULL)
  resampling_res <- reactiveVal(NULL)
  conf_res <- reactiveVal(NULL)
  reshaped_data2 <- reactiveVal(NULL)
  rv <- reactiveValues(deletedColumns = character(0), deletedRows = character(0))
  app_activated <- reactiveVal(FALSE)
  app_activated_surv <- reactiveVal(FALSE)
  nresampling <- reactiveVal(FALSE)
  nresampling_conf <- reactiveVal(FALSE)
  visualizeInferenceOutput <- reactiveVal(TRUE)
  visualizeConfidenceOutput <- reactiveVal(TRUE)
  reactive_selected_result <- reactiveVal(NULL)
  reactive_selected_result_conf <- reactiveVal(NULL)
  reshaped_data_matrix <- reactiveVal(NULL)
  buttonClicked <- reactiveVal(FALSE)
  reshaped_dataSurv <- reactiveVal(NULL)
  genotype_table_surv <- reactiveVal(FALSE)
  orig_genotypeSurv <- reactiveVal(NULL)
  orig_dataSurv <- reactiveVal(NULL)
  surv_data <- reactiveVal(NULL)
  evo_step <- reactiveVal(NULL)
  reg_sel <- reactiveVal(NULL)
  resExampleEvosigs <- reactiveVal(NULL)
  col_names <- reactiveVal(NULL)

  # Active app
  observe({
    if (!app_activated()) {
      app_activated(TRUE)
    }
  })
  observe({
    if (!app_activated_surv()) {
      app_activated_surv(TRUE)
    }
  })
  
  # Display the secondgenotype table entry in survival input phase if the app is active
  output$dataFile2_surv <- renderUI({
    if (app_activated_surv()) {
      tagList(
        div(style = "align-items: center;", 
            fileInput("dataFile2_surv", 
                      label = span("Provide survival data input", 
                                   tags$i(id = "helpIcon", 
                                          class = "fa fa-question-nitro", 
                                          style="margin-left: 5px;"))),
            bsTooltip(id = "helpIcon", 
                      title = "Select the file containing the information about the sample taken and its CCF.", 
                      placement = "right", trigger = "hover"),
            actionButton("loadBtn2_surv", "Load", class = "custom-button", style = "margin-top: -25px;")
        )
      )
    }
  })
  
  # Display the genotype table entry if the app is active
  output$dataFile <- renderUI({
    if (app_activated() && input$data_type != "Select data type") {
      tagList(
        div(style = "display: flex; align-items: center;", 
            fileInput("dataFile", label = span(""))),
        div(style = "display: flex; align-items: center;",
            actionButton("loadBtn", "Load", class = "custom-button", 
                         style = "margin-top: -35px;"),
            bsTooltip(id = "loadBtn", 
                      title = "Load to view and edit the selected file.", 
                      placement = "right", trigger = "hover")
        ),
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
                      label = span("", 
                                   tags$i(id = "helpIcon", 
                                          class = "fa fa-question-nitro", 
                                          style="margin-left: 5px;"))),
            bsTooltip(id = "helpIcon", 
                      title = "Select the file containing the information about the sample taken and its CCF.", 
                      placement = "right", trigger = "hover"),
            actionButton("loadBtn_surv", "Load", class = "custom-button", 
                         style = "margin-top: -25px;"),
            bsTooltip(id = "loadBtn_surv", 
                      title = "Load to view and edit the selected file.", 
                      placement = "right", trigger = "hover")
        )
      )
    }
  })
  
  # Regularization selectable in the confidence enstimation step are those that 
  # were used during the inference step
  observe({
    updateSelectInput(session, "regularization_confEstimation",
                      choices = input$regularization)
  })
  
  # Iteration constraint management
  observeEvent(input$iteration_confEstimation, {
    x <- input$iteration_confEstimation
    if ( !is.na(x) && x < 3){
      updateNumericInput(session, "iteration_confEstimation", value = 3)
      showNotification("Minimum value 3 ", type = "warning")
    }
  })
  
  # Num. of resampling in confidence estimation constraint management
  observeEvent(input$nresampling_conf, {
    x <- input$nresampling_conf
    if ( !is.na(x) && (x < 3)){
      updateNumericInput(session, "nresampling_conf", value = 3)
      showNotification("Minimum value 3 ", type = "warning")
    }
  })
  
  # Num. of resampling constraint management
  observeEvent(input$nresampling, {
    x <- input$nresampling
    if ( !is.na(x) && (x < 3)){
      updateNumericInput(session, "nresampling", value = 3)
      showNotification("Minimum value 3 ", type = "warning")
    }
  })

  observeEvent(input$resamplingFlag, {
    updateNumericInput(session, "nresampling", value = 10)
  })
  
  observeEvent(input$resamplingFlag_conf, {
    updateNumericInput(session, "nresampling_conf", value = 10)
  })
  
  # Binarization field constraint management
  observeEvent(input$binarization, {
    x <- input$binarization
    if ( !is.na(x) && (x > 1 || x < 0)){
      updateNumericInput(session, "binarization", value = 0.05)
      showNotification("Allowed range 0-1 ", type = "warning")
    }
  })
  
  # Percentage binarization field constraint management
  observeEvent(input$binarization_perc, {
    x <- input$binarization_perc
    if ( !is.na(x) && (x > 1 || x < 0)){
      updateNumericInput(session, "binarization_perc", value = 0.00)
      showNotification("Allowed range 0-1 ", type = "warning")
    }
  })
  
  # Binarization field constraint management in input survival phase
  observeEvent(input$binarization_surv, {
    x <- input$binarization_surv
    if ( !is.na(x) && (x > 1 || x < 0)){
      updateNumericInput(session, "binarization_surv", value = 0.05)
      showNotification("Allowed range 0-1 ", type = "warning")
    }
  })
  
  # Percentage binarization field constraint management in input survival phase
  observeEvent(input$binarization_percSurv, {
    x <- input$binarization_percSurv
    if ( !is.na(x) && (x > 1 || x < 0)){
      updateNumericInput(session, "binarization_percSurv", value = 0.00)
      showNotification("Allowed range 0-1 ", type = "warning")
    }
  })
  
  observeEvent(input$binarization_surv2, {
    x <- input$binarization_surv2
    if ( !is.na(x) && (x > 1 || x < 0)){
      updateNumericInput(session, "binarization_surv2", value = 0.00)
      showNotification("Allowed range 0-1 ", type = "warning")
    }
  })
  
  
  ############################ Function  #######################################
  
  reset_common_values <- function() {
    output$dataTable2 <- NULL
    output$binarization_perc <- renderUI(NULL)
    output$binarization <- renderUI(NULL)
    output$dataTable <- NULL
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
    updateNumericInput(session, "iteration_confEstimation", value = 10)
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
    case_surv(NULL)
    selected_folder(NULL)
    output$combined_graph <- NULL
    output$survPlot <- NULL
    output$binarization_percSurv <- NULL
    output$binarization_surv <- NULL
    output$DeleteColumn_surv <- NULL
    output$DeleteRow_surv <- NULL
    output$binarization_surv2 <- NULL
    output$DeleteColumn_surv2 <- NULL
    output$DeleteRow_surv2 <- NULL
    updateCheckboxInput(session, "load_file", value = FALSE)
    output$dataTable_GenotypeSurv <- NULL
    output$dataTable_surv <- NULL
    output$heatmap_surv <- NULL
    output$heatmap_GenotypeSurv <- NULL
    output$survPlot2 <- NULL
    app_activated_surv(FALSE)
    conf_res(NULL)
    resampling_res(NULL)
    genotype_table_surv(FALSE)
    orig_genotypeSurv(NULL)
    orig_dataSurv(NULL)
    evo_step(NULL)
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
    output$dataTable2 <- NULL
    output$loadBtn2 <- renderUI(NULL)
    output$directoryInput <- renderUI(NULL)
    updateSelectInput(session, "data_type", selected = "Select data type")
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
    output$selected_result_output <- NULL
    output$selected_result_output_conf <- NULL
    orig <- reactiveVal(NULL)
    output$DeleteColumn <- NULL
    output$DeleteRow <- NULL
    output$visualize_inference <- NULL
    output$visualize_conf <- NULL
    app_activated(FALSE)
    output$dataTable2 <- NULL
    output$loadBtn2 <- renderUI(NULL)
    output$directoryInput <- renderUI(NULL)
  }
  
  # Resets values when submit btm in inference
  default_values_inference <- function() {
    output$graph_inference <- NULL
    output$graph_conf <- NULL
    output$selected_result_output_conf <- renderDataTable(NULL)
    output$selected_result_output <- renderDataTable(NULL)
    output$visualize_inference <- renderUI(NULL)
    output$visualize_conf <- renderUI(NULL)
    visualizeInferenceOutput(FALSE)
    visualizeConfidenceOutput(FALSE)
    resampling_res(NULL)
  }
  
  # Returns the list of project names in the output_project folder
  get_project_names <- function() {
    project_names <- list.files("output_project", full.names = TRUE)
    
    project_info <- data.frame(Project_name = character(),
                               Last_modified_directory = as.POSIXct(character()),
                               Last_modified_file = as.POSIXct(character()),
                               stringsAsFactors = FALSE)
    
    for (project_dir in project_names) {
      project_name <- basename(project_dir)
      
      dir_info <- file.info(project_dir)
      last_modified_directory <- dir_info$mtime
      
      modified_time_path <- file.path(project_dir, "modified_time.csv")
      if (file.exists(modified_time_path)) {
        file_info <- file.info(modified_time_path)
        last_modified_file <- file_info$mtime
      } else {
        last_modified_file <- NA
      }
      
      project_info <- rbind(project_info, data.frame(Project_name = project_name,
                                                     Last_modified_directory = last_modified_directory,
                                                     Last_modified_file = last_modified_file))
    }
    
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
  
  # Visualization of deletion column and row in input survival phase
  visualize_del <- function() {
    output$DeleteColumn_surv <- render_delete_column_ui("DeleteColumn_surv", 
                                                        "Remove DNA alterations (column)", 
                                                        reshaped_dataSurv())
    output$DeleteRow_surv <- render_delete_row_ui("DeleteRow_surv", 
                                                  "Remove samples (row)", 
                                                  reshaped_dataSurv())
  }
  
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
    
    output$DeleteColumn <- render_delete_column_ui("DeleteColumn", 
                                                   "Remove DNA alterations (column)", 
                                                   reshaped_data())
    output$DeleteRow <- render_delete_row_ui("DeleteRow", 
                                             "Remove samples (row)", 
                                             reshaped_data())
    
    # Management deletion or selection of rows and columns
    observe_data_modification(reshaped_data())
  }
  
  # Manage bulk multiple case
  bulk_multiple_case <- function() {
    modified_data <- reshaped_data()
    
    output$DeleteColumn <- render_delete_column_ui("DeleteColumn", 
                                                   "Remove DNA alterations (column)", 
                                                   reshaped_data())
    output$DeleteRow <- render_delete_row_ui("DeleteRow", 
                                             "Remove samples (row)", 
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
                   title = "Select the folder containing a file for each row, where each file corresponds to an adjacency matrix.")
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
                                                   "Remove DNA alterations (column)", 
                                                   reshaped_data())
    output$DeleteRow <- render_delete_row_ui("DeleteRow", 
                                             "Remove samples (row)", 
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
  
  prepareEdges <- function(edges, poset) {
    if (!is.null(poset)) {
      edges$weight <- mapply(function(from, to) poset[from, to], edges$from, edges$to)
      edges$width <- edges$weight + 1  
      edges$label <- paste0(" ", edges$weight) 
    }
    return(edges)
  }
  
  # Generate network graph
  generateVisNetwork <- function(nodes, edges, layout_type, title, poset = NULL) {
    edges <- prepareEdges(edges, poset)
    
    main_options <- list(text = title,
                         style = "font-family:https://fonts.googleapis.com/css2?family=Roboto+Condensed:ital,wght@0,100..900;1,100..900&display=swap;
                                font-weight: bold;
                                text-align:center;")
    
    nodes <- nodes[order(nodes$label), ]
    
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
          width = edges$width,
          label = edges$label 
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
  
  # Binarize scond genotype file in input survival phase
  binarize_table_surv <- function(filter, filter_perc, reshaped_data) {
    genotype_table_surv(reshaped_data)
    if(is.na(filter)) {
      showNotification("Enter a valid value in the binarization field", type = "error")
    }
    else if (is.null(case_surv()) || case_surv() != "bulk_multiple") {
      #filter the genotype_table_surv table
      genotype_table_surv(ifelse(genotype_table_surv() >= filter, 1, 0))
    }
    else if (case_surv() == "bulk_multiple") {
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
  
  # Delete column or row of genotype file in input survival phase
  modify_reshaped_dataSurv <- function(reshaped_data, del_row, del_col) {
    reshaped_data_df <- as.data.frame(reshaped_data)  # Conversione in data.frame
    
    if (!is.null(del_row) && length(del_row) > 0) {
      cols_to_delete <- unlist(strsplit(del_row, ",\\s*"))
      if (any(del_row %in% colnames(reshaped_data_df))) {
        reshaped_data_df <- reshaped_data_df[, !colnames(reshaped_data_df) 
                                             %in% del_row, drop = FALSE]
      }
    }
    
    if (!is.null(del_col) && length(del_col) > 0) {
      rows_to_delete <- as.numeric(unlist(strsplit(del_col, ",\\s*")))
      if (any(del_col %in% rownames(reshaped_data_df)) || 
          any(as.numeric(del_col) %in% 1:nrow(reshaped_data_df))) {
        reshaped_data_df <- reshaped_data_df[!rownames(reshaped_data_df) 
                                             %in% del_col, , drop = FALSE]
      }
    }
    
    return(reshaped_data_df)
  }
  
  # Visualize survival output (risk-prev and kaplan-meier)
  visualize_output_surv <- function(resExampleEvosigs) {
    output$combined_graph <- renderGirafe({
      # Assuming resExampleEvosigs is your result object with necessary data
      df_prev <- resExampleEvosigs$clustersPrevalence
      if (!is.data.frame(df_prev)) {
        df_prev <- as.data.frame(df_prev)
      }
      df_prev$Cluster <- factor(paste("EvoSig", 1:nrow(df_prev), sep="")) # Custom cluster labels
      names(df_prev) <- gsub(".to.", " > ", names(df_prev))
      df_prev_long <- melt(df_prev, id.vars = "Cluster", variable.name = "GenePair", value.name = "Prevalence")
      df_prev_long$Source <- "Prevalence"
      
      result <- resExampleEvosigs$clustersPrevalence
      for (col in colnames(result)) {
        if (col %in% names(resExampleEvosigs$evolutionarySteps)) {
          result[, col] <- ifelse(result[, col] != 0, 
                                  resExampleEvosigs$evolutionarySteps[col], 0)
        }
      }
      df_risk <- as.data.frame(result)
      df_risk$Cluster <- factor(paste("EvoSig", 1:nrow(df_risk), sep="")) # Custom cluster labels
      names(df_risk) <- gsub(".to.", " > ", names(df_risk))
      df_risk_long <- melt(df_risk, id.vars = "Cluster", variable.name = "GenePair", value.name = "Prevalence")
      df_risk_long$Source <- "Risk"
      
      combined_df <- rbind(df_prev_long, df_risk_long)
      combined_df$Source <- factor(combined_df$Source, levels = c("Risk", "Prevalence"))
      
      df_risk_long <- df_risk_long %>%
        arrange(desc(Prevalence))
      
      combined_df$GenePair <- factor(combined_df$GenePair, levels = unique(df_risk_long$GenePair))
      
      num_colors <- length(levels(combined_df$Cluster))
      colors_dark2 <- brewer.pal(num_colors, "Dark2")
      
      combined_df$Color <- ifelse(combined_df$Source == "Prevalence", "#FFC0CA",  
                                  colors_dark2[as.integer(combined_df$Cluster)])
      
      custom_labels <- function(x) {
        ifelse(x %% 1 == 0, as.character(x), as.character(x))
      }
      
      p <- ggplot(data = combined_df, aes(x = Prevalence, y = GenePair, fill = Color, tooltip = Prevalence, data_id = GenePair)) +
        geom_bar_interactive(stat = "identity", position = position_dodge(width = 0.5), width = 0.4, hover_css = "fill: black;") +
        facet_wrap(~ Cluster + Source, scales = "free_x", nrow = 1, strip.position = "top") +
        scale_fill_identity() +
        scale_x_continuous(breaks = seq(-1, 1, by = 0.5), labels = custom_labels) +
        theme_minimal() +
        theme(
          axis.title = element_blank(),
          axis.text.y = element_text(angle = 0, vjust = 0.5, size = 6),
          axis.text.x = element_text(angle = 0, hjust = 0.5, size = 6),
          legend.position = "none",
          strip.text.x = element_text(angle = 0, hjust = 0.5, size = 6),
          strip.text.y = element_text(angle = 0, hjust = 0.5, size = 6),
          plot.title = element_text(hjust = 0.4, size = 9),
          panel.background = element_rect(fill = alpha("grey90", 0.5), color = NA),
          panel.grid.major = element_line(color = "white")
        ) +
        labs(y = "", x = "Prevalence", title = "Evolutionary Signatures")
      
      girafe(ggobj = p)
    })
    
    new_df <- reactive({
      times <- resExampleEvosigs$survivalAnalysis$data$times
      status <- resExampleEvosigs$survivalAnalysis$data$status
      clusters <- resExampleEvosigs$survivalAnalysis$clusters
      df <- data.frame(times = times, status = status, clusters = clusters)
      rownames(df) <- rownames(resExampleEvosigs$survivalAnalysis$data)
      df
    })
    
    generate_labels <- function(num_clusters) {
        paste0("EvoSig", 1:num_clusters)
      }
    
    
      df <- new_df()  
      
      num_clusters <- length(unique(df$clusters))
      new_labels <- generate_labels(num_clusters)
      df$clusters <- factor(df$clusters)
      levels(df$clusters) <- new_labels
      fit <- survfit(Surv(times, status) ~ clusters, data = df)
      
      g <- ggsurvplot(fit, data = df, risk.table = TRUE,
                      title = "Kaplan Meier estimates",
                      palette = "Dark2", legend.title="",
                      legend.labs = new_labels,
                      ggtheme = theme_minimal(),
                      censor = FALSE, pval = FALSE,)  
      
      p <- ggplotly(g$plot)
      
      p <- p %>% layout(
        title = list(text = "Kaplan Meier estimates", font = list(size = 17), x = 0.5, xanchor = "center" ),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.05),
        xaxis = list(title = "Time (months)", titlefont = list(size = 12)),
        annotations = list(
          list(
            text = "p<0.0001",
            x = 0.05,  
            xref = "paper",
            y = 0.25,  
            yref = "paper",
            showarrow = FALSE,
            font = list(size = 12)
          )
        )
      )
      
      # Create risk table as a separate plot
      risk_table <- ggplotly(g$table)
      
      risk_table <- risk_table %>% layout(
        title = list(text = "Risk table", font = list(size = 17), x = 0.5, xanchor = "center" ),
        xaxis = list(title = "Time (months)", titlefont = list(size = 12)),
        showlegend = FALSE
      )
      
      output$survPlot2 <- renderPlotly({risk_table})
      output$survPlot <- renderPlotly({p})

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
  
  observe({
    value <- resampling_res()
    if (is.null(value)) {
      updateSelectInput(session, "regularization_surv", choices = list())
    } else {
      updateSelectInput(session, "regularization_surv", choices = names(value$inference), selected = reg_sel())
    }
  })
  
  ############################ Load or create project  ###########################
  
  ##Create
  
  # Function to change the tabPanel to "Data Input" when the "Create New Project" 
  observeEvent(input$create_project_button, {
    rv$dataFile=NULL
    default_values_create_project()
    updateSelectInput(session, "data_type", selected = "Select data type")
    updateTabItems(session, "sidebarMenu", "input")
  })
  
  observeEvent(input$data_type, {
      rv$dataFile=NULL
      if (input$data_type == "Bulk single") {
        output$directoryInput <- renderUI(NULL)
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
          actionButton("loadBtn2", "Load", class = "custom-button", style = "margin-top: -35px;")
        })
    } else if (input$data_type == "Bulk multiple" || input$data_type == "Single cell"){
      output$dataFile2 <- NULL
      output$loadBtn2 <- renderUI(NULL)
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
    } else {
      output$directoryInput <- renderUI(NULL)
      output$dataFile2 <- NULL
      output$loadBtn2 <- renderUI(NULL)
    }
  })
  
  observeEvent(input$resetBtn, {
    showModal(modalDialog(
      title = "Conferma Reset",
      "Sei sicuro di voler resettare tutto?",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Annulla"),
        actionButton("confirmReset", "Conferma")
      )
    ))
  })
  
  observeEvent(input$confirmReset, {
    removeModal()
    rv$dataFile <- NULL
    default_values_create_project()
    updateTabItems(session, "sidebarMenu", "input")
  })
  
  observeEvent(input$survBtn, {
    updateTabItems(session, "sidebarMenu", "input_surv")
  })
  
  observeEvent(input$inferenceBtn, {
    updateTabItems(session, "sidebarMenu", "inference")
  })
  
  observeEvent(input$confidenceBtn, {
    updateTabItems(session, "sidebarMenu", "confidence_estimation")
  })
  
  observeEvent(input$out_survBtn, {
    updateTabItems(session, "sidebarMenu", "output_surv")
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
      project_data <- project_data[, c(1, 4, 3, 2)]
      project_data[,4] <- format(as.POSIXct(project_data[,4], 
                                            format = "%Y-%m-%dT%H:%M:%SZ"), 
                                 format = "%Y-%m-%d %H:%M:%S")
      
      project_data[,3] <- format(as.POSIXct(project_data[,3], 
                                            format = "%Y-%m-%dT%H:%M:%SZ"), 
                                 format = "%Y-%m-%d %H:%M:%S")
      
    }
    
    datatable(project_data, rownames = FALSE, 
              colnames = c("Project name", "Data type", "Created", "Last modified"), 
              selection = "single", options = list(
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#232E33', 'color': '#fff'});",
                  "$(this.api().column(1).header()).append('<span id=\"dataTypeHelp\" data-toggle=\"tooltip\" title=\"Bulk multiple: data generated from bulk samples of multiple biopsies Bulk single: data generated from bulk samples of single biopsies Single cell: data generated from individual cells\" style=\"margin-left: 10px;\">&#9432;</span>');",
                  "$('[data-toggle=\"tooltip\"]').tooltip();",
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
                                         label = span("Prevalence threshold", 
                                                      tags$i(id = "helpIcon4", 
                                                             class = "fa fa-question-circle", 
                                                             style="margin-left: 5px;")), 
                                         value = val_bin_perc, min = 0, max = 1, step = 0.01),
                            bsTooltip(id = "helpIcon4", 
                                      title = "Specify which threshold you want to use to binarize percentage in each sample.", 
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
              else if (file_name == "genotipo_surv") {
                data <- read.csv(file_directory, row.names = 1)
                data <- as.matrix(data)
                orig_genotypeSurv(data)
                reshaped_dataSurv(data)
                output$dataTable_GenotypeSurv <- renderDT({
                  datatable(data, options = list(scrollX = TRUE))
                })
                output$heatmap_GenotypeSurv <- renderUI({
                  generate_heatmap_plot(data)
                })
              }
              else if (file_name == "surv_data") {
                data_surv <- read.csv(file_directory, row.names = 1)
                data_surv <- data.frame(data_surv)
                surv_data(data_surv)
              }
              else if (file_name == "evo_step") {
                data <- read.csv(file_directory, row.names = 1)
                data_evo <- as.matrix(data)
                evo_step(data)
                orig_dataSurv(data)
                output$dataTable_surv <- renderDT({
                  datatable(data_evo, options = list(scrollX = TRUE))
                })
                output$heatmap_surv <- renderUI({
                  generate_heatmap_plot(data_evo)
                })
              }
              else if (file_name == "parameters") {
                parameters <- read.csv(file_directory)
                
                # Cycle through each row of the dataframe
                for (i in 1:nrow(parameters)) {
                  parametro <- parameters[i, "name"]
                  value <- parameters[i, "value"]
                  
                  # Assigns the value to the corresponding input
                  if (parametro == "binarization") {
                    binarization <- as.numeric(value)
                    output$binarization <- renderUI({
                      tagList(
                        div(style = "display: flex; align-items: center;",
                            numericInput("binarization", 
                                         label = span("CCF threshold", 
                                                      tags$i(id = "helpIcon3", 
                                                             class = "fa fa-question-circle", 
                                                             style="margin-left: 5px;")), 
                                         value = binarization, min = 0, max = 1, 
                                         step = 0.01),
                            bsTooltip(id = "helpIcon3", 
                                      title = "Specify which threshold you want to use to binarize CCF data.", 
                                      placement = "right", trigger = "hover")
                        )
                      )
                    })
                  }
                  else if (parametro =="del_col") {
                    vettore <- unlist(strsplit(gsub("\"", "", value), ",\\s*"))
                    del_col_inf <- vettore[vettore != ""]
                    output$DeleteColumn <- render_delete_column_ui("DeleteColumn", 
                                                                   "Remove DNA alterations (column)", 
                                                                   reshaped_data(),
                                                                   selected_columns = del_col_inf)
                    
                  }else if (parametro =="data_type") {
                    updateSelectInput(session, "data_type", selected = value)
                    
                  }else if (parametro =="data_type_surv") {
                    updateSelectInput(session, "data_type_surv", selected = value)
                    
                  }else if (parametro =="del_row") {
                    vettore <- unlist(strsplit(gsub("\"", "", value), ",\\s*"))
                    del_row_inf <- vettore[vettore != ""]
                    output$DeleteRow <- render_delete_row_ui("DeleteRow", 
                                                             "Remove samples (row)", 
                                                             reshaped_data(),
                                                             selected_rows = del_row_inf)
                  }else if (parametro == "flag_resampling") {
                    updateCheckboxInput(session, "resamplingFlag", 
                                        value = as.logical(as.numeric(value)))
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
                  } else if (parametro == "reg_surv") {
                    reg_sel(value)
                  } else if (parametro == "flag_surv") {
                    updateCheckboxInput(session, "load_file", 
                                        value = value)
                  } else if (parametro == "binarization_surv" & !(is.na(value))){
                    binarization_surv <- as.numeric(value)
                    output$binarization_surv <- renderUI({
                      tagList(
                        div(style = "display: flex; align-items: center;",
                            numericInput("binarization_surv", 
                                         label = span("CCF threshold ", 
                                                      tags$i(id = "helpIcon3", 
                                                             class = "fa fa-question-circle", 
                                                             style="margin-left: 5px;")), 
                                         value = binarization_surv, min = 0, max = 1, 
                                         step = 0.01),
                            bsTooltip(id = "helpIcon3", 
                                      title = "Specify which threshold you want to use to binarize CCF data.", 
                                      placement = "right", trigger = "hover")
                        )
                      )
                    })
                    
                  }
                  else if (parametro =="del_col_surv") {
                    if (is.na(value)) {
                      del_col <- character(0)
                    } else {
                      vettore <- unlist(strsplit(gsub("\"", "", value), ",\\s*"))
                      del_col <- vettore[vettore != ""]
                    }
                    output$DeleteColumn_surv <- render_delete_column_ui("DeleteColumn_surv", 
                                                                   "Remove DNA alterations (column)", 
                                                                   orig_genotypeSurv(),
                                                                   selected_columns = del_col)
                    
                  }else if (parametro =="del_row_surv") {
                    if (is.na(value)) {
                      del_row <- character(0)
                    } else {
                      vettore <- unlist(strsplit(gsub("\"", "", value), ",\\s*"))
                      del_row <- vettore[vettore != ""]
                    }
                    output$DeleteRow_surv <- render_delete_row_ui("DeleteRow_surv", 
                                                             "Remove samples (row)", 
                                                             orig_genotypeSurv(),
                                                             selected_rows = del_row)
                  } else if (parametro == "binarizationPerc_surv" & !(is.na(value))) {
                    binarizationPerc_surv <- as.numeric(value)
                    output$binarization_percSurv <- renderUI({
                      tagList(
                        div(style = "display: flex; align-items: center;",
                            numericInput("binarization_percSurv", 
                                         label = span("Prevalence threshold ", 
                                                      tags$i(id = "helpIcon4", 
                                                             class = "fa fa-question-circle", 
                                                             style="margin-left: 5px;")), 
                                         value = binarizationPerc_surv, min = 0, max = 1, step = 0.01),
                            bsTooltip(id = "helpIcon4", 
                                      title = "Specify which threshold you want to use to binarize percentage in each sample.", 
                                      placement = "right", trigger = "hover")
                        )
                      )
                    })
                    case_surv("bulk_multiple")
                  } else if (parametro == "binarization_surv2" & !(is.na(value))){
                    binarization_surv2 <- as.numeric(value)

                    output$binarization_surv2 <- renderUI({
                      tagList(
                        div(style = "display: flex; align-items: center;",
                            numericInput("binarization_surv2", 
                                         label = span("Percentage threshold ", 
                                                      tags$i(id = "helpIcon6", 
                                                             class = "fa fa-question-circle", 
                                                             style="margin-left: 5px;")), 
                                         value = binarization_surv2, min = 0, max = 1, 
                                         step = 0.01),
                            bsTooltip(id = "helpIcon6", 
                                      title = "Specify  the percentage of presence that each step must have.", 
                                      placement = "right", trigger = "hover")
                        )
                      )
                    })

                  } else if (parametro =="del_col_surv2") {
                    if (is.na(value)) {
                      del_col2 <- character(0)
                    } else {
                      vettore <- unlist(strsplit(gsub("\"", "", value), ",\\s*"))
                      del_col2 <- vettore[vettore != ""]
                    }
                  output$DeleteColumn_surv2 <- render_delete_column_ui("DeleteColumn_surv2", 
                                                                      "Remove evolutionary step (column)", 
                                                                      orig_dataSurv(),
                                                                      selected_columns = del_col2)
                  
                }else if (parametro =="del_row_surv2") {
                  if (is.na(value)) {
                    del_row2 <- character(0)
                  } else {
                    vettore <- unlist(strsplit(gsub("\"", "", value), ",\\s*"))
                    del_row2 <- vettore[vettore != ""]
                  }
                  output$DeleteRow_surv2 <- render_delete_row_ui("DeleteRow_surv2", 
                                                                "Remove samples (row)", 
                                                                orig_dataSurv(),
                                                                selected_rows = del_row2)
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
                  select_input <- selectInput("visualize_inference", 
                                              "Select your output", 
                                              c(names),
                                              selected = ifelse("rankingEstimate" %in% names, names[3], names[2]))
                  
                  tooltip <- bsTooltip(id = "visualize_inference", 
                                       title = "Output types: rankingEstimate (Orders genetic mutations using the agony algorithm), poset (Shows all possible causal paths between genes in the ranking), DAG (Highlights significant relationships and repeated patterns of evolution among genetic mutations)",
                                       placement = "right", 
                                       trigger = "hover",
                                       options = list(container = "body", html = TRUE))
                  
                  tagList(select_input, tooltip)
                })
                
              }else if (file_name == "resExampleEvosigs") {
                resExampleEvosigs(readRDS(file_directory))
                visualize_output_surv(resExampleEvosigs())
              }
              else if (file_name == "confidence_res") {
                conf_res(readRDS(file_directory))
                res <- conf_res()
                names_to_remove <- c("dataset", "models", "ccfDataset", "inference")
                names <- setdiff(names(res), names_to_remove)
                names <- c(names, names(res$inference))
                visualizeConfidenceOutput(TRUE)
                output$visualize_conf <- renderUI({
                select_input <- selectInput("visualize_conf", 
                                            "Select your output", 
                                            c(names),
                                            selected = ifelse(any(c("ranking", "rankingEstimate") %in% names), names[3], names[2]))
                
                tooltip <- bsTooltip(id = "visualize_conf", 
                                     title = "Output types: rankingEstimate (Orders genetic mutations using the agony algorithm), poset (Shows all possible causal paths between genes in the ranking), DAG (Highlights significant relationships and repeated patterns of evolution among genetic mutations)",
                                     placement = "right", 
                                     trigger = "hover",
                                     options = list(container = "body", html = TRUE))
                tagList(select_input, tooltip)
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
      if (colnames(data2)[1]=="SAMPLE" & colnames(data2)[2]=="GENE" &
          colnames(data2)[3]=="REF_COUNT" & colnames(data2)[4]=="ALT_COUNT" &
          colnames(data2)[5]=="COPY_NUMBER" & colnames(data2)[6]=="NORMAL_PLOIDY" &
          colnames(data2)[7]=="VAF_ESTIMATE" & colnames(data2)[8]=="CCF_ESTIMATE") {
        
        
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
      } else {
        showNotification("File not recognized. Make sure the column names are correct.", 
                         type = "error")
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
        if (colnames(data)[1] =="SAMPLE" & colnames(data)[2] =="GENE" &
            colnames(data)[3] =="CCF") {
          if (input$data_type != "Bulk single") {
            showNotification("Select the correct file", type = "error")
            updateSelectInput(session, "data_type", selected = "Select data type")
          } else {

            default_values_load_new_genotype()
            output$directoryInput <- renderUI(NULL)
            case("bulk_single")
            data <- distinct(data, SAMPLE, GENE, .keep_all = TRUE)
            reshaped_data(
              acast(data, SAMPLE ~ GENE, value.var = "CCF", fill = 0)
            )
            
            output$binarization <- renderUI({
              tagList(
                div(style = "display: flex; align-items: center;",
                    numericInput("binarization", 
                                 label = span("CCF threshold", 
                                              tags$i(id = "helpIcon3", 
                                                     class = "fa fa-question-circle", 
                                                     style="margin-left: 5px;")), 
                                 value = 0.05, min = 0, max = 1, step = 0.01),
                    bsTooltip(id = "helpIcon3", 
                              title = "Specify which threshold you want to use to binarize CCF data.", 
                              placement = "right", trigger = "hover")
                )
              )
            })
            
            bulk_single_case()
          }
        } else {
          showNotification("File not recognized. Make sure the column names are correct.", 
                           type = "error")
        }
        
      } else if (ncol(data) == 4) {   #### Bulk multipla biopsia o single cell 
        if (colnames(data)[1]=="SAMPLE" & colnames(data)[2]=="REGION" &
            colnames(data)[3]=="GENE" & colnames(data)[4]=="CCF") {
          if (input$data_type != "Bulk multiple") {
            showNotification("Select the correct file", type = "error")
            updateSelectInput(session, "data_type", selected = "Select data type")
          } else {
            default_values_load_new_genotype()
            output$dataTable2 <- NULL
            output$loadBtn2 <- renderUI(NULL)
            
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
                                                           "Remove DNA alterations (column)", 
                                                           reshaped_data())
            output$DeleteRow <- render_delete_row_ui("DeleteRow", 
                                                     "Remove samples (row)", 
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
                                 label = span("CCF threshold", 
                                              tags$i(id = "helpIcon3", 
                                                     class = "fa fa-question-circle", 
                                                     style="margin-left: 5px;")), 
                                 value = 0.05, min = 0, max = 1, step = 0.01),
                    bsTooltip(id = "helpIcon3", 
                              title = "Specify which threshold you want to use to binarize CCF data.", 
                              placement = "right", trigger = "hover")
                )
              )
            })
            
            
            output$binarization_perc <- renderUI({
              tagList(
                div(style = "display: flex; align-items: center;",
                    numericInput("binarization_perc", 
                                 label = span("Prevalence threshold", 
                                              tags$i(id = "helpIcon4", 
                                                     class = "fa fa-question-circle", 
                                                     style="margin-left: 5px;")), 
                                 value = 0.00, min = 0, max = 1, step = 0.01),
                    bsTooltip(id = "helpIcon4", 
                              title = "Specify which threshold you want to use to binarize percentage in each sample.", 
                              placement = "right", trigger = "hover")
                )
              )
            })
            
            handle_dataTable_cell_clicked(reshaped_data(), "ID")
            
            observe({
              req(input$dir)
              selected_folder(parseDirPath(c(wd = getwd()), input$dir))
            })
            
            shinyDirChoose(input, "dir", roots = c(wd = getwd()), 
                           filetypes = c("", "txt"))
          }
          
        } else if (colnames(data)[1]=="PATIENT" & colnames(data)[2]=="CELL" &
                    colnames(data)[3]=="GENE" & colnames(data)[4]=="VALUE") {
          if (input$data_type != "Single cell") {
            showNotification("Select the correct file", type = "error")
            updateSelectInput(session, "data_type", selected = "Select data type")
          } else {  
            default_values_load_new_genotype()
            output$dataTable2 <- NULL
            output$loadBtn2 <- renderUI(NULL)
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
                                                           "Remove DNA alterations (column)", 
                                                           reshaped_data())
            
            output$DeleteRow <- render_delete_row_ui("DeleteRow", 
                                                     "Remove samples (row)", 
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
                                 label = span("CCF threshold", 
                                              tags$i(id = "helpIcon3", 
                                                     class = "fa fa-question-circle", 
                                                     style="margin-left: 5px;")), 
                                 value = 0.05, min = 0, max = 1, step = 0.01),
                    bsTooltip(id = "helpIcon3", 
                              title = "Specify which threshold you want to use to binarize CCF data.", 
                              placement = "right", trigger = "hover")
                )
              )
            })
  
            
            handle_dataTable_cell_clicked(reshaped_data(), "PATIENT")
            
            
            observe({
              req(input$dir)
              selected_folder(parseDirPath(c(wd = getwd()), input$dir))
            })
            
            shinyDirChoose(input, "dir", roots = c(wd = getwd()), 
                           filetypes = c("", "txt"))
          }
          
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
      
      # Apri il modal di calcolo in corso
      modal_session <- showModal(modalDialog(
        title = "Processing",
        div(
          style = "text-align: center;",
          tags$i(class = "fa fa-hourglass-half fa-spin fa-1x fa-fw"),
          p("Calculation in progress. Please wait...")
        ),
        easyClose = FALSE,
        footer = NULL
      ))
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
                
                res <- asceticCCF(
                  dataset = genotype_table(),
                  ccfDataset = reshaped_data_inference(),
                  regularization = input$regularization,
                  command = input$command, 
                  restarts = input$restarts
                )
                
              } else {
                
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
                
                
                res <- asceticPhylogenies(
                  dataset = genotype_table(),
                  models = models_filtrati_dati,
                  regularization = input$regularization,
                  command = input$command,
                  restarts = input$restarts
                )
                
              } else {
                
                
                res <- asceticPhylogeniesBootstrap(
                  dataset = genotype_table(),
                  models = models_filtrati_dati,
                  nsampling = nsampling,
                  regularization = input$regularization,
                  command = input$command,
                  restarts = input$restarts
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
              output$visualize_inference <- renderUI({
                select_input <- selectInput("visualize_inference", 
                                            "Select your output", 
                                            c(names),
                                            selected = ifelse("rankingEstimate" %in% names, names[3], names[2]))
                
                
                tooltip <- bsTooltip(id = "visualize_inference", 
                                     title = "Output types: rankingEstimate (Orders genetic mutations using the agony algorithm), poset (Shows all possible causal paths between genes in the ranking), DAG (Highlights significant relationships and repeated patterns of evolution among genetic mutations)",
                                     placement = "right", 
                                     trigger = "hover",
                                     options = list(container = "body", html = TRUE))
                
                tagList(select_input, tooltip)
              })
            }

          removeModal()
        }, error = function(e) {
          visualizeInferenceOutput(FALSE)
          output$visualize_inference <- NULL
          output$selected_result_output <- NULL
          output$graph_inference <- NULL
          showNotification("Select the correct folder in the previous step", type = "error")
          removeModal()
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
            tagList(
              div(style = "display: flex; align-items: center;",
                  numericInput("nresampling", 
                               label = span("Number of samplings", 
                                            tags$i(id = "helpIconNresampling", 
                                                   class = "fa fa-question-circle", 
                                                   style = "margin-left: 5px;")),
                               value = nresampling(), min = 3),
                  bsTooltip(id = "helpIconNresampling", 
                            title = "A higher resampling number requires more computational time.", 
                            placement = "right", trigger = "hover")
              )
            )
          })
        } else {
          output$nresampling <- renderUI({
            tagList(
              div(style = "display: flex; align-items: center;",
                  numericInput("nresampling", 
                               label = span("Number of samplings", 
                                            tags$i(id = "helpIconNresampling", 
                                                   class = "fa fa-question-circle", 
                                                   style = "margin-left: 5px;")),
                               value = 10, min = 3),
                  bsTooltip(id = "helpIconNresampling", 
                            title = "A higher resampling number requires more computational time.", 
                            placement = "right", trigger = "hover")
              )
            )
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
    col_names(col_names)
    
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
        reactive_selected_result(selected_result)
        output$selected_result_output <- renderDT({
          datatable(reactive_selected_result(), 
                    options = list(
                      scrollX = TRUE, 
                      order = list(list(1, 'asc'), list(0, 'asc')),
                      columnDefs = list(
                        list(className = 'dt-body-right', targets = c(0, 1))  
                      )
                    ),
                    rownames = FALSE, selection = "single", 
                    colnames = c('Genes', 'Rank'))
        })
      }
      else if (input$visualize_inference == "poset"){
        colnames(selected_result) <- col_names
        rownames(selected_result) <- col_names
        output$graph_inference <- NULL
        reactive_selected_result (selected_result)
        output$selected_result_output <- renderDT({
          data <- reactive_selected_result()
          data <- as.data.frame(data)
          data <- data[ , sort(names(data))]
          data <- data[order(row.names(data)), ]
          datatable(data, options = list(scrollX = TRUE), rownames = TRUE, selection = "single")
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
            
            if (nrow(nodes) != 0) {
              edges <- as_tibble(as_edgelist(grafo))
              colnames(edges) <- c("from", "to")
              
              generateVisNetwork(nodes, edges, "other", "")
            } else {
              showNotification("No DAG available", type = "message")
              output$graph_inference <- NULL
            }
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
    } else if (is.null(case())) {
      showNotification("Upload the genomic file in the previous step", type = "error")
    } else if (is.na(input$iteration_confEstimation)) {
      showNotification("Fill in all fields", type = "error")
    } else if (input$resamplingFlag_conf == TRUE && is.na(input$nresampling_conf)) {
      showNotification("Fill in all fields", type = "error")
    } else {
      modal_session <- showModal(modalDialog(
        title = "Processing",
        div(
          style = "text-align: center;",
          tags$i(class = "fa fa-hourglass-half fa-spin fa-1x fa-fw"),
          p("Calculation in progress. Please wait...")
        ),
        easyClose = FALSE,
        footer = NULL
      ))
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
        
        output$visualize_conf <- renderUI({
          select_input <- selectInput("visualize_conf", 
                                      "Select your output", 
                                      c(names),
                                      selected = ifelse(any(c("ranking", "rankingEstimate") %in% names), names[3], names[2]))
          
          tooltip <- bsTooltip(id = "visualize_conf", 
                               title = "Output types: rankingEstimate (Orders genetic mutations using the agony algorithm), poset (Shows all possible causal paths between genes in the ranking), DAG (Highlights significant relationships and repeated patterns of evolution among genetic mutations)",
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body", html = TRUE))
          
          tagList(select_input, tooltip)
        })
      }
      removeModal()
    }
  })
  
  # Display the confidence estimation  output
  observe({
    req(input$sidebarMenu == "confidence_estimation")
    req(input$visualize_conf)
    res <- conf_res()
    output$selected_result_output_conf <- NULL
    col_names <- col_names()
    
    res_inf <- resampling_res()
    res_inf <- res_inf$rankingEstimate
    
    
    if(visualizeConfidenceOutput()) {
      if (input$visualize_conf %in% names(res)) {
        selected_result <- res[[input$visualize_conf]]
      } else {
        selected_result <- res$inference[[input$visualize_conf]]
      }

      if (input$visualize_conf == "rankingEstimate" | input$visualize_conf == "ranking") {
        output$graph_conf <- NULL
        selected_result <- res[["ranking"]]
        ranking_df <- as.data.frame(selected_result)
        ranking_df <- data.frame(Genes = rownames(ranking_df), 
                                 Rank_sampling = ranking_df$selected_result,
                                 Rank = as.integer(res_inf[, "rank"]) + 1,
                                 stringsAsFactors = FALSE)
        
        ranking_df <- ranking_df %>%
          arrange(Rank_sampling, Rank, Genes)

        reactive_selected_result_conf (ranking_df)
        output$selected_result_output_conf <- renderDT({
          datatable(
            reactive_selected_result_conf(),
            options = list(
              scrollX = TRUE,
              columnDefs = list(
                list(className = 'dt-body-right', targets = c(0, 1, 2))  
              )
            ),
            rownames = FALSE,
            selection = "single"
          )
        })
        
      } else if (input$visualize_conf == "poset"){
        colnames(selected_result) <- col_names
        rownames(selected_result) <- col_names
        output$graph_conf <- NULL
        reactive_selected_result (selected_result)
        reactive_selected_result_conf (selected_result)
        output$selected_result_output_conf <- renderDT({
          data <- reactive_selected_result()
          data <- as.data.frame(data)
          data <- data[ , sort(names(data))]
          data <- data[order(row.names(data)), ]
          datatable(data, options = list(scrollX = TRUE),
                    rownames = TRUE, selection = "single")
        })
      } else {
        if (all(selected_result == 0)) {
          showNotification("No DAG available", type = "message")
          output$graph_conf <- NULL
        } else {
          
          reactive_selected_result_conf(selected_result)
          grafo <- graph_from_adjacency_matrix(selected_result)
          poset <- if (!is.null(res$poset)) res$poset else NULL
          
          output$graph_conf <- renderVisNetwork({
            nodi_da_rimuovere <- V(grafo)[degree(grafo, mode = "in") == 0 & 
                                            degree(grafo, mode = "out") == 0]
            grafo <- delete.vertices(grafo, nodi_da_rimuovere)
            nodes <- as_tibble(get.vertex.attribute(grafo))
            colnames(nodes) <- "id"
            nodes <- data.frame(nodes, label= nodes$id)
            if (nrow(nodes) != 0) {
              edges <- as_tibble(as_edgelist(grafo))
              colnames(edges) <- c("from", "to")
              
              generateVisNetwork(nodes, edges, "other", "", poset)
            } else {
              showNotification("No DAG available", type = "message")
              output$graph_conf <- NULL
            }
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
            tagList(
              div(style = "display: flex; align-items: center;",
                  numericInput("nresampling_conf", 
                               label = span("Number of samplings", 
                                            tags$i(id = "helpIconNresamplingConf", 
                                                   class = "fa fa-question-circle", 
                                                   style = "margin-left: 5px;")),
                               value = nresampling_conf(), min = 3),
                  bsTooltip(id = "helpIconNresamplingConf", 
                            title = "A higher resampling number requires more computational time.", 
                            placement = "right", trigger = "hover")
              )
            )
          })
        } else {
          output$nresampling_conf <- renderUI({
            tagList(
              div(style = "display: flex; align-items: center;",
                  numericInput("nresampling_conf", 
                               label = span("Number of samplings", 
                                            tags$i(id = "helpIconNresamplingConf", 
                                                   class = "fa fa-question-circle", 
                                                   style = "margin-left: 5px;")),
                               value = 10, min = 3),
                  bsTooltip(id = "helpIconNresamplingConf", 
                            title = "A higher resampling number requires more computational time.", 
                            placement = "right", trigger = "hover")
              )
            )
          })
        }
      } else {
        # If the condition is not met, show a warning
        showNotification("Load the resampling file in the previous step", 
                         type = "warning")
        # Update resamplingFlag_conf back to FALSE
        updateCheckboxInput(session, "resamplingFlag_conf", value = FALSE)
      }
    } else {
      # If resamplingFlag_conf is off, remove the nresampling_conf input
      output$nresampling_conf <- NULL
    }
  })
  
  
  ############################ Input survival  #################################
  
  # Calculate evolutionary step table
  observeEvent(input$submit_surv, {
    if (is.null(conf_res())) {
      showNotification("Perform the confidence phase first", type = "message")
    } else if (input$load_file & is.null(reshaped_dataSurv())) {
        showNotification("Please select a genotype file", type = "error")
    } else if (is.null(surv_data())) {
      showNotification("Please upload survival file", type = "error")
    } else if (is.null(input$regularization_surv)){
      showNotification("Please select a regularization value", type = "error")
    } else {
      result <- conf_res()
      regularization <- input$regularization_surv
      matrix <- result$inference[[regularization]]
      
      positions_one <- which(matrix == 1, arr.ind = TRUE)
      
      # Identification of pairs in the selected graph through the regularizer
      pairs <- data.frame(
        Genes = apply(positions_one, 1, 
                      function(idx) paste(rownames(matrix)[idx[1]], 
                                          ".to.", colnames(matrix)[idx[2]], 
                                          sep="")),
        stringsAsFactors = FALSE
      )
      
      if (nrow(pairs) != 0) {
        
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
        
        # Management of the second genotype file if it has been uploaded by the user
        if (input$load_file) {
            filter_value <- input$binarization_surv
            filter_perc <- input$binarization_percSurv

            binarize_table_surv(filter_value, filter_perc, reshaped_dataSurv())

            mutation_db <- genotype_table_surv()
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

        surv_data <- surv_data()
        common_samples <- intersect(rownames(output_db), surv_data$SAMPLE)
        
        output_db <- output_db[rownames(output_db) %in% common_samples, ]
        surv_data <- surv_data[surv_data$SAMPLE %in% common_samples, ]
        surv_data(surv_data)
        # Checking for all-zero or all-one columns
        all_zero_one <- apply(output_db, 2, function(col) all(col == 0) || all(col == 1))
        if (any(all_zero_one)) {
          removed_cols <- colnames(output_db)[which(all_zero_one)]
          output_db <- output_db[, !all_zero_one]
        }
        output_db <- data.frame(output_db)

        if (ncol(output_db) < 2) {
          showNotification("No valid evolutionary step found", type = "message")
          return()
        }
        else {
          output$dataTable_surv <- renderDT({
            datatable(output_db, options = list(scrollX = TRUE), 
                      selection = "single")
          })
          output$heatmap_surv <- renderUI({
            generate_heatmap_plot(output_db)
          })
          
          evo_step(output_db)
          
          output$binarization_surv2 <- renderUI({
            tagList(
              div(style = "display: flex; align-items: center;",
                  numericInput("binarization_surv2", 
                               label = span("Percentage threshold ", 
                                            tags$i(id = "helpBinarization_surv2", 
                                                   class = "fa fa-question-circle", 
                                                   style="margin-left: 5px;")), 
                               value = 0.00, min = 0, max = 1, 
                               step = 0.01),
                  bsTooltip(id = "helpBinarization_surv2", 
                            title = "Specify  the percentage of presence that each step must have.", 
                            placement = "right", trigger = "hover")
              )
            )
          })
          
          output$DeleteColumn_surv2 <- render_delete_column_ui("DeleteColumn_surv2", 
                                                              "Remove evolutionary step (column)", 
                                                              output_db)
          output$DeleteRow_surv2 <- render_delete_row_ui("DeleteRow_surv2", 
                                                        "Remove samples (row)", 
                                                        output_db)
          orig_dataSurv(output_db)
        }
      } else {
        showNotification("No DAG available with this regularizer", type = "error")
      }
    }
  })
  
  # Load survival file
  observeEvent(input$loadBtn2_surv, {
    inFile <- input$dataFile2_surv
    
    if (is.null(inFile)) {
      showNotification("Please select a file", type = "error")
    } else {
      data <- read.table(inFile$datapath, sep = "\t", header = TRUE, 
                         stringsAsFactors = FALSE)
      if (colnames(data)[1]=="SAMPLE" & colnames(data)[2]=="STATUS" &
          colnames(data)[3]=="TIMES") {
        surv_data(data)
        showNotification("File loaded successfully.",
                         type = "message")
      } else {
        showNotification("File not recognized. Make sure the column names are correct.", 
                         type = "error")
      }
    }
  })
  
  # Load the second genotype file
  observeEvent(input$loadBtn_surv, {
    inFile2 <- input$dataFile_surv
    output$dataTable_surv <- NULL
    output$heatmap_surv <- NULL
    
    if (is.null(inFile2)) {
      showNotification("Please select a file", type = "error")
    } else {
      data <- read.table(inFile2$datapath, sep = "\t", header = TRUE, 
                         stringsAsFactors = FALSE)
      
      #### Bulk single biopsy
      if (ncol(data) == 3) {
        if (colnames(data)[1] =="SAMPLE" & colnames(data)[2] =="GENE" &
            colnames(data)[3] =="CCF") {
          
          if (input$data_type_surv != "Bulk single") {
            showNotification("Select the correct file", type = "error")
          } else {
            case_surv("bulk_single")
            data <- distinct(data, SAMPLE, GENE, .keep_all = TRUE)
            reshaped_dataSurv(
              acast(data, SAMPLE ~ GENE, value.var = "CCF", fill = 0)
            )
            
            output$dataTable_GenotypeSurv <- renderDT({
              datatable(reshaped_dataSurv(), options = list(scrollX = TRUE))
            })
            
            output$heatmap_GenotypeSurv <- renderUI({
              generate_heatmap_plot(reshaped_dataSurv())
            })
            
            output$binarization_surv <- renderUI({
              tagList(
                div(style = "display: flex; align-items: center;",
                    numericInput("binarization_surv", 
                                 label = span("CCF threshold ", 
                                              tags$i(id = "helpIcon3", 
                                                     class = "fa fa-question-circle", 
                                                     style="margin-left: 5px;")), 
                                 value = 0.05, min = 0, max = 1, 
                                 step = 0.01),
                    bsTooltip(id = "helpIcon3", 
                              title = "Specify which threshold you want to use to binarize CCF data.", 
                              placement = "right", trigger = "hover")
                )
              )
            })
            output$binarization_percSurv <- NULL
            visualize_del()
            orig_genotypeSurv(reshaped_dataSurv())
          }
        } else{
          showNotification("File not recognized. Make sure the column names are correct.", 
                           type = "error")
        }
      }
      else if (ncol(data) == 4) {   
        if (colnames(data)[1]=="SAMPLE" & colnames(data)[2]=="REGION" &
               colnames(data)[3]=="GENE" & colnames(data)[4]=="CCF") {
          
          if (input$data_type_surv != "Bulk multiple") {
            showNotification("Select the correct file", type = "error")
          } else {
            case_surv("bulk_multiple")
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
              datatable(reshaped_dataSurv(), options = list(scrollX = TRUE))
            })
            
            output$heatmap_GenotypeSurv <- renderUI({
              generate_heatmap_plot(reshaped_dataSurv())
            })
            
            output$binarization_surv <- renderUI({
              tagList(
                div(style = "display: flex; align-items: center;",
                    numericInput("binarization_surv", 
                                 label = span("CCF threshold ", 
                                              tags$i(id = "helpIcon3", 
                                                     class = "fa fa-question-circle", 
                                                     style="margin-left: 5px;")), 
                                 value = 0.05, min = 0, max = 1, 
                                 step = 0.01),
                    bsTooltip(id = "helpIcon3", 
                              title = "Specify which threshold you want to use to binarize CCF data.", 
                              placement = "right", trigger = "hover")
                )
              )
            })
            
            output$binarization_percSurv <- renderUI({
              tagList(
                div(style = "display: flex; align-items: center;",
                    numericInput("binarization_percSurv", 
                                 label = span("Prevalence threshold ", 
                                              tags$i(id = "helpIcon4", 
                                                     class = "fa fa-question-circle", 
                                                     style="margin-left: 5px;")), 
                                 value = 0.00, min = 0, max = 1, step = 0.01),
                    bsTooltip(id = "helpIcon4", 
                              title = "Specify which threshold you want to use to binarize percentage in each sample.", 
                              placement = "right", trigger = "hover")
                )
              )
            })
            visualize_del()
            orig_genotypeSurv(reshaped_dataSurv())
          }
        }
        else if (colnames(data)[1]=="PATIENT" & colnames(data)[2]=="CELL" &
                         colnames(data)[3]=="GENE" & colnames(data)[4]=="VALUE") {
          
            if (input$data_type_surv != "Single cell") {
              showNotification("Select the correct file", type = "error")
            } else {
            case_surv("single_cell")
            data <- distinct(data, PATIENT, CELL, GENE, .keep_all = TRUE)
            
            data <- data %>%
              group_by(PATIENT, GENE) %>%
              summarise(PERCENTAGE = mean(VALUE)) %>%
              pivot_wider(names_from = GENE, values_from = PERCENTAGE, 
                          values_fill = list(PERCENTAGE = 0))
            
            reshaped_dataSurv(data %>%
                                column_to_rownames(var = "PATIENT"))
            
            output$dataTable_GenotypeSurv <- renderDT({
              datatable(reshaped_dataSurv(), options = list(scrollX = TRUE))
            })
            
            output$heatmap_GenotypeSurv <- renderUI({
              generate_heatmap_plot(reshaped_dataSurv())
            })
            
            output$binarization_surv <- renderUI({
              tagList(
                div(style = "display: flex; align-items: center;",
                    numericInput("binarization_surv", 
                                 label = span("CCF threshold ", 
                                              tags$i(id = "helpIcon3", 
                                                     class = "fa fa-question-circle", 
                                                     style="margin-left: 5px;")), 
                                 value = 0.05, min = 0, max = 1, 
                                 step = 0.01),
                    bsTooltip(id = "helpIcon3", 
                              title = "Specify which threshold you want to use to binarize CCF data.", 
                              placement = "right", trigger = "hover")
                )
              )
            })
            output$binarization_percSurv <- NULL
            visualize_del()
            orig_genotypeSurv(reshaped_dataSurv())
            }
        } else {
          showNotification("File not recognized. Make sure the column names are correct.", 
                           type = "error")
        }
      }
    }
  })
  
  observe({
    req(reshaped_dataSurv())  
    updated_data <- modify_reshaped_dataSurv(orig_genotypeSurv(), input$DeleteColumn_surv, input$DeleteRow_surv)
    reshaped_dataSurv(updated_data) 
    
    output$dataTable_GenotypeSurv <- renderDT({
      datatable(reshaped_dataSurv(), options = list(scrollX = TRUE))
    })
    updateTextInput(session, "DeleteColumn_surv", value = input$DeleteColumn_surv)
    updateTextInput(session, "DeleteRow_surv", value = input$DeleteRow_surv)
  })
  
  observe({
    req(orig_dataSurv())  
    updated_data <- modify_reshaped_dataSurv(orig_dataSurv(), input$DeleteColumn_surv2, input$DeleteRow_surv2)
    evo_step(updated_data) 
    
    output$dataTable_surv <- renderDT({
      datatable(evo_step(), options = list(scrollX = TRUE))
    })
    updateTextInput(session, "DeleteColumn_surv2", value = input$DeleteColumn_surv2)
    updateTextInput(session, "DeleteRow_surv2", value = input$DeleteRow_surv2)
  })
  
  # Rendering second genotype table in input survival phase
  output$dataTable_GenotypeSurv <- renderDT({
    req(reshaped_dataSurv())  # Assicura la presenza di dati
    datatable(reshaped_dataSurv(), options = list(scrollX = TRUE))
  })
  
  observeEvent(input$load_file, {
    if (input$load_file) {
      output$data_type_surv <- renderUI({selectInput("data_type_surv", 
                                           "Select the type of data you want to load", 
                                           choices = c("Bulk single", "Bulk multiple", "Single cell"))})
    } else {
      output$data_type_surv <- renderUI({NULL})
    }
  })
  
  ############################ Output survival  #################################
  
  # Calculation of survival output using ASCETIC function 'evoSigs'
  observeEvent(input$calc_surv, {
    if(is.null(evo_step())) {
      showNotification("Calculate the previous step first", type = "message")
    } else {
      if (!is.na(input$binarization_surv2) & input$binarization_surv2 != 0 ) {
        data <- evo_step()
        threshold <- input$binarization_surv2
        cols_to_remove <- sapply(data, function(column) {
          mean(column == 1) < threshold
        })
        filtered_evoStep <- data[, !cols_to_remove, FALSE]
      } else if (input$binarization_surv2 == 0){
        filtered_evoStep <- evo_step()
      }
      if (ncol(filtered_evoStep) < 1) {
        showNotification("There are no evolutionary steps with this percentage of presence", type = "warning")
      } else {
      
        #resExampleEvosigs <- evoSigs( survivalData = surv_data(),
        #                              evolutionarySteps = filtered_evoStep 
        #                              )
        data(amlExample)
        resExampleEvosigs <- evoSigs( survivalData = amlExample$survival_data,
                                      evolutionarySteps = amlExample$evolutionary_steps )
        
        resExampleEvosigs(resExampleEvosigs)
      
        visualize_output_surv(resExampleEvosigs)
      }
    }
  })

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
      
      directory_file <- paste0(directory_complete, "/genotipo_surv.csv")
      if (!is.null(orig_genotypeSurv())) {
        saveData(orig_genotypeSurv() , directory_file)
      }
      
      directory_file <- paste0(directory_complete, "/modified_time.csv")
      saveData(Sys.time() , directory_file)

      directory_file <- paste0(directory_complete, "/surv_data.csv")
      if (!is.null(surv_data())) {
        saveData(surv_data() , directory_file)
      }
      
      directory_file <- paste0(directory_complete, "/resExampleEvosigs.rds")
      if (!is.null(resExampleEvosigs())) {
        saveRDS(resExampleEvosigs() , directory_file)
      }

      
      values <- c("binarization", "del_col", "del_row", "flag_resampling", 
                "nresampling", "restarts", "regularization", "command", 
                "seed", "iteration", "flag_confidence", "nresampling_confidence",  
                "reg_surv", "flag_surv", "binarization_surv", "data_type", "data_type_surv", 
                "binarization_surv2")
      
      if (!is.null(case_surv()) && case_surv() == "bulk_multiple") {
        values <- c(values, "binarizationPerc_surv")
      }
      
      if (!is.null(orig_genotypeSurv())) {
        values <- c(values, "del_col_surv", "del_row_surv")
      }
      
      if (!is.null(evo_step())) {
        values <- c(values, "del_col_surv2", "del_row_surv2")
      }
      
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
      sequence <- c(sequence, ifelse((!is.null(input$iteration_confEstimation)), 
                                     input$iteration_confEstimation, NA))
      sequence <- c(sequence, ifelse((!is.null(input$resamplingFlag_conf)), 
                                     input$resamplingFlag_conf, NA))
      sequence <- c(sequence, ifelse((!is.null(input$nresampling_conf)), 
                                     input$nresampling_conf, NA))
      sequence <- c(sequence, ifelse((!is.null(input$regularization_surv)), 
                                     input$regularization_surv, NA))
      sequence <- c(sequence, ifelse((!is.null(input$load_file)), 
                                     input$load_file, NA))
      sequence <- c(sequence, ifelse((!is.null(input$binarization_surv)), 
                                     toString(input$binarization_surv), NA))
      sequence <- c(sequence, ifelse((!is.null(input$data_type)), 
                                     toString(input$data_type), NA))
      sequence <- c(sequence, ifelse((!is.null(input$data_type_surv)), 
                                     toString(input$data_type_surv), NA))
      sequence <- c(sequence, ifelse((!is.null(input$binarization_surv2)), 
                                     toString(input$binarization_surv2), NA))
      if ((!is.null(case_surv())) && case_surv() == "bulk_multiple") {
        sequence <- c(sequence, ifelse((!is.null(input$binarization_percSurv)), 
                                       toString(input$binarization_percSurv), NA))
      }
      if (!is.null(orig_genotypeSurv())) {
        sequence <- c(sequence, ifelse((!is.null(input$DeleteColumn_surv)), 
                                       toString(input$DeleteColumn_surv), NA))
        sequence <- c(sequence, ifelse((!is.null(input$DeleteRow_surv)), 
                                       toString(input$DeleteRow_surv), NA))
      }
      if (!is.null(evo_step())) {
        sequence <- c(sequence, ifelse((!is.null(input$DeleteColumn_surv2)), 
                                       toString(input$DeleteColumn_surv2), NA))
        sequence <- c(sequence, ifelse((!is.null(input$DeleteRow_surv2)), 
                                       toString(input$DeleteRow_surv2), NA))
      }

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
      
      if(!is.null(evo_step()) && ncol(evo_step()) >= 2) {
        directory_file <- paste0(directory_complete, "/evo_step.csv")
        saveData(evo_step(), directory_file)
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
