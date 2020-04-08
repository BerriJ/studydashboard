#' @title Launches the studydashboard
#'
#' @description This function launches the studydashboard in your console.
#'
#' @export launchApp
#'
#' @seealso \code{\link{launchApp_background}}, \code{\link{reset_db}}
#'
#' @import shiny
#' @import shinydashboard
#' @import shiny.i18n
#' @import ggplot2
#' @import dplyr


launchApp <- function(){

  fs <- pool::dbPool(drv = RSQLite::SQLite(), dbname = paste0(system.file("extdata", package="studydashboard"), "/fs.db"))
  pruefung <- pool::dbPool(drv = RSQLite::SQLite(), dbname = paste0(system.file("extdata", package="studydashboard"), "/pruefung.db"))
  studium <- pool::dbPool(drv = RSQLite::SQLite(), dbname = paste0(system.file("extdata", package="studydashboard"), "/studium.db"))

  # Set up for translation ####
  translator <- Translator$new(translation_json_path = system.file("extdata/translation_de.json", package="studydashboard"))

  # Add www folder path to display 404 image
  addResourcePath(prefix = 'www', directoryPath = system.file("www/", package="studydashboard"))

  # Correct encodings
  fix.encoding <- function(df, originalEncoding = "UTF-8") {
    numCols <- ncol(df)
    df <- data.frame(df)
    for (col in 1:numCols)
    {
      if(class(df[, col]) == "character"){
        Encoding(df[, col]) <- originalEncoding
      }

      if(class(df[, col]) == "factor"){
        Encoding(levels(df[, col])) <- originalEncoding
      }
    }
    return(as_tibble(df))
  }

  # Check if FS DB is empty (to open upload_data tab instead of FS Data tab)
  if(length(pool::dbListTables(fs)) == 0){
    upload_selected = TRUE
  } else {
    upload_selected = NULL
  }

  ui <- dashboardPage(skin = "blue",
                      title = "Studydashboard",

                      dashboardHeader(title = textOutput('header_text')),
                      dashboardSidebar(sidebarMenuOutput('menu')),

                      dashboardBody( # Dashboard Body ####

                                     # We are using Shinyjs to hide elements dynamically
                                     shinyjs::useShinyjs(),
                                     tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))),
                                     tags$head(tags$style(".rightAlign{float:right;}")),
                                     tags$head(tags$link(rel="icon", href="www/favicon.ico")),
                                     # extendShinyjs(text = jscode),
                                     # Boxes need to be put in a row (or column)
                                     tabItems(
                                       # First tab content
                                       tabItem(tabName = "data_fs", # FS Data tab ####
                                               div(id = "fs_box_missing",
                                                   box(width = 12,
                                                       title = h2(textOutput("fs_data_missing"), align = "center"),
                                                       tags$div(HTML('<p style="text-align:center;"><img src="www/error.svg" width="70%" class="center"/></p>')),
                                                   ),
                                               ),
                                               div(id = "fs_boxes_content",
                                                   box(title = h3(textOutput("fs_data_tab_heading")), width = 12,
                                                       collapsible = TRUE, collapsed = TRUE,
                                                       selectInput(inputId = 'datset_fs',
                                                                   label = "Choose a dataset:",
                                                                   choices = pool::dbListTables(fs)),
                                                       DT::DTOutput(outputId = "table_fs")
                                                   ),
                                                   box(width = 12,
                                                       title = textOutput("exit_reason_heading"),
                                                       collapsible = TRUE,
                                                       h2(textOutput("heading_linreg")),
                                                       uiOutput("picker_compare_fs_exit_reasons_plot"),
                                                       plotOutput("testplot"),
                                                       downloadButton("DL_exit_reasons"),
                                                       uiOutput("description_exit_reasons_plot", class = 'rightAlign')
                                                   ),

                                                   box(width = 12,
                                                       title = textOutput("num_graduates_heading"),
                                                       collapsible = TRUE,
                                                       uiOutput("picker_compare_fs_num_graduates_plot"),
                                                       plotOutput("num_graduates_plot"),
                                                       downloadButton("DL_num_graduates"),
                                                       uiOutput("description_num_graduates_plot", class = 'rightAlign'))

                                               )),
                                       tabItem(tabName = "data_pruefung", # Pruefung Data tab ####
                                               div(id = "pruefung_box_missing",
                                                   box(width = 12,
                                                       title = h2(textOutput("pruefung_data_missing"), align = "center"),
                                                       tags$div(HTML('<p style="text-align:center;"><img src="www/error.svg" width="70%" class="center"/></p>')),
                                                   ),
                                               ),
                                               div(id = "pruefung_boxes_content",
                                                   box(width = 12, collapsible = TRUE, collapsed = TRUE, title =  h3(textOutput("pruefung_data_tab_heading")),
                                                       selectInput(inputId = 'datset_pruefung',
                                                                   label = "Choose a dataset:",
                                                                   choices = pool::dbListTables(pruefung)),
                                                       DT::DTOutput(outputId = "table_pruefung")
                                                   ),
                                                   box(width = 12,
                                                       title = textOutput("exams_per_semester_heading"),
                                                       collapsible = TRUE,
                                                       uiOutput("picker_compare_pruefung_num_exams_plot"),
                                                       plotOutput("exams_per_semester_plot"),
                                                       uiOutput("description_num_exams_plot", class = 'rightAlign'),
                                                       downloadButton("DL_eps")),
                                                   box(width = 12,
                                                       title = textOutput("cum_credits_heading"),
                                                       collapsible = TRUE,
                                                       uiOutput("picker_compare_pruefung_cum_cp_plot"),
                                                       plotOutput("cum_credits_plot"),
                                                       uiOutput("description_num_credits_plot", class = 'rightAlign'),
                                                       downloadButton("DL_ccp")),
                                                   box(width = 12,
                                                       title = textOutput("credits_per_student_heading"),
                                                       collapsible = TRUE,
                                                       uiOutput("picker_compare_pruefung_cp_per_student_plot"),
                                                       plotOutput("credits_per_student_plot"),
                                                       uiOutput("description_avg_earned_credits_plot", class = 'rightAlign'),
                                                       downloadButton("DL_cps")),
                                                   box(width = 12,
                                                       title = textOutput("number_of_attempts_heading"),
                                                       collapsible = TRUE,
                                                       uiOutput("picker_compare_pruefung_number_of_attempts_plot"),
                                                       uiOutput("picker_select_pruefung"),
                                                       plotOutput("number_of_attempts_plot"),
                                                       uiOutput("description_number_of_attemps_plot", class = 'rightAlign'),
                                                       downloadButton("DL_noa")),
                                                   box(width = 12,
                                                       title = textOutput("exams_semester_heading"),
                                                       collapsible = TRUE,
                                                       uiOutput("picker_compare_pruefung_semester_exams_plot"),
                                                       uiOutput("picker_select_pruefung_semester"),
                                                       plotOutput("exams_semester_plot"),
                                                       uiOutput("description_semester_exams_plot", class = 'rightAlign'),
                                                       downloadButton("DL_se")
                                                   ))),

                                       tabItem(tabName = "data_studium", # Studium Data tab ####
                                               div(id = "studium_box_missing",
                                                   box(width = 12,
                                                       title = h2(textOutput("studium_data_missing"), align = "center"),
                                                       tags$div(HTML('<p style="text-align:center;"><img src="www/error.svg" width="70%" class="center"/></p>')),
                                                   ),
                                               ),
                                               div(id = "studium_boxes_content",
                                                   box(width = 12, collapsible = TRUE, collapsed = TRUE, title =  h3(textOutput("studium_data_tab_heading")),
                                                       selectInput(inputId = 'datset_studium',
                                                                   label = "Choose a dataset:",
                                                                   choices = pool::dbListTables(studium)),
                                                       DT::DTOutput(outputId = "table_studium")
                                                   ),
                                                   box(width = 12,
                                                       title = textOutput("study_length_heading"),
                                                       collapsible = TRUE,
                                                       uiOutput("picker_compare_studium_study_length_plot"),
                                                       plotOutput("study_length"),
                                                       uiOutput("description_study_length_plot", class = 'rightAlign'),
                                                       downloadButton("DL_sl")))),
                                       tabItem(tabName = "upload", # Upload data tab ####
                                               box(width = 12,
                                                   title = textOutput("data_heading"),
                                                   fileInput('target_upload', textOutput("data_file"),
                                                             buttonLabel = textOutput("data_button"),
                                                             accept = c('.rds')),
                                                   shinyWidgets::actionBttn("save_to_db", textOutput("data_save"),
                                                                            style = "material-flat"),
                                                   DT::dataTableOutput("sample_table")
                                               ))
                                     )
                      )
  )


  server <- function(input, output, session) {

    # Render the menu sever side

    output$header_text <- renderText({
      paste(i18n()$t("Student Progress Dashboard"))
    })

    output$menu <- renderMenu({

      sidebarMenu( # Sidebar Menu ####

                   menuItem(i18n()$t("FS Data"),
                            icon = icon("table"),tabName = "data_fs"),
                   # menuSubItem(i18n$t("Status"),
                   #             tabName = "status",
                   #             icon = icon("chart-line"))),

                   menuItem(i18n()$t("Pruefung Data"),
                            icon = icon("table"), tabName = "data_pruefung"),

                   menuItem(i18n()$t("Studium Data"),
                            icon = icon("table"), tabName = "data_studium"),
                   menuItem(i18n()$t("Add Data"),
                            icon = icon("upload"), tabName = "upload",
                            # Select this tab if databases are empty
                            selected = upload_selected),
                   uiOutput("language_selector_input")
      )

    })



    # Render titles

    output$fs_data_tab_heading <- renderText(i18n()$t("The FS Data:"))
    output$pruefung_data_tab_heading <- renderText(i18n()$t("The Pruefung Data:"))
    output$studium_data_tab_heading <- renderText(i18n()$t("The Studium Data:"))
    output$exit_reason_heading <- renderText(i18n()$t("Number of Cases per Exit Reason"))
    output$num_graduates_heading <- renderText(i18n()$t("Number of Graduates per Semester"))
    output$exams_per_semester_heading <- renderText(i18n()$t("Number of Exams per Semester of Examination"))
    output$cum_credits_heading <- renderText(i18n()$t("Number of Credits earned per Academic Semester"))
    output$credits_per_student_heading <- renderText(i18n()$t("Average of earned Credits per Semester"))
    output$number_of_attempts_heading <- renderText(i18n()$t("Average Number of Attempts per Exam"))
    output$exams_semester_heading <- renderText(i18n()$t("Distribution of Academic Semesters in which Students have taken the Exam"))
    output$study_length_heading <- renderText(i18n()$t("Distribution of Number of Students over the total Number of Semesters enrolled"))
    output$data_heading <- renderText(i18n()$t("Add new .rds file to the Database:"))
    output$data_file <- renderText(i18n()$t("Choose file to upload"))
    output$data_button <- renderText(i18n()$t("Browse..."))
    output$data_save <- renderText(i18n()$t("Save Table to Database"))
    output$fs_data_missing <- renderText(i18n()$t("Oh no! The database is still empty. Please upload some data to reveal the content."))
    output$studium_data_missing <- renderText(i18n()$t("Oh no! The database is still empty. Please upload some data to reveal the content."))
    output$pruefung_data_missing <- renderText(i18n()$t("Oh no! The database is still empty. Please upload some data to reveal the content."))


    # Server Translation Setup

    i18n <- reactive({
      selected <- input$selected_language
      if (length(selected) > 0 && selected %in% translator$languages) {
        translator$set_translation_language(selected)
      }
      translator
    })

    output$language_selector_input <- renderUI({

      selectInput("selected_language",
                  h3(i18n()$t("Language")),
                  choices = translator$languages,
                  selected = input$selected_language)

    })

    # Data Setup ####

    # Create a reactive object which stores all data
    data <- reactiveValues()
    # Add FS Data from DB
    data[['fs']] <- pool::dbListTables(fs) %>%
      purrr::map(.f = function(x) pool::dbReadTable(fs, x)) %>%
      stats::setNames(pool::dbListTables(fs))
    # Add Studium Data from DB
    data[['studium']] <- pool::dbListTables(studium) %>%
      purrr::map(.f = function(x) pool::dbReadTable(studium, x)) %>%
      stats::setNames(pool::dbListTables(studium))
    # Add Pruefung Data from DB
    data[['pruefung']] <- pool::dbListTables(pruefung) %>%
      purrr::map(.f = function(x) pool::dbReadTable(pruefung, x)) %>%
      stats::setNames(pool::dbListTables(pruefung))

    # Upload Data

    # Create reactive object which temporarily stores data to be uploaded
    df_products_upload <- reactive({
      inFile <- input$target_upload
      if (is.null(inFile) | !isTRUE(tools::file_ext(input$target_upload[[1]]) == "rds"))
        return(NULL)
      df <- readRDS(inFile$datapath) %>%
        select_if(function(.) n_distinct(.) > 1) %>%
        mutate_if(is.character, forcats::as_factor) %>%
        fix.encoding()
      return(df)
    })

    # preview data to be uploaded as datatable table
    output$sample_table<- DT::renderDataTable({
      DT::datatable(df_products_upload(),
                    rownames = FALSE,
                    options = list(scrollX = TRUE))
    })

    # Save the data to db in user clicks accordingly
    observeEvent(input$save_to_db, {

      if(!is.null(input$target_upload) &
         is.data.frame(df_products_upload()) &
         isTRUE(tools::file_ext(input$target_upload[[1]]) == "rds")
      ){

        if(ncol(readRDS(input$target_upload$datapath)) == 29){
          pool::dbWriteTable(pruefung, sub('\\.rds$', '',
                                           input$target_upload[[1]]),
                             df_products_upload(), overwrite = T)
          data[['pruefung']] <- pool::dbListTables(pruefung) %>%
            purrr::map(.f = function(x) pool::dbReadTable(pruefung, x)) %>%
            stats::setNames(pool::dbListTables(pruefung))

          index_list[['pruefung']] <- pool::dbListTables(pruefung) %>%
            purrr::map(.f = function(x) pool::dbReadTable(pruefung, x)) %>%
            stats::setNames(pool::dbListTables(pruefung)) %>%
            purrr::map(function(x) return(1:nrow(x)))

          updateSelectInput(session = session,
                            inputId =  "datset_pruefung",
                            choices = pool::dbListTables(pruefung),
                            selected = pool::dbListTables(pruefung)[1])

        } else if(ncol(readRDS(input$target_upload$datapath)) == 22) {
          pool::dbWriteTable(fs, sub('\\.rds$', '', input$target_upload[[1]]),
                             df_products_upload(), overwrite = T)
          data[['fs']] <- pool::dbListTables(fs) %>%
            purrr::map(.f = function(x) pool::dbReadTable(fs, x)) %>%
            stats::setNames(pool::dbListTables(fs))
          index_list[['fs']] <- pool::dbListTables(fs) %>%
            purrr::map(.f = function(x) pool::dbReadTable(fs, x)) %>%
            stats::setNames(pool::dbListTables(fs)) %>%
            purrr::map(function(x) return(1:nrow(x)))
          updateSelectInput(session = session,
                            inputId =  "datset_fs",
                            choices = pool::dbListTables(fs))
        } else if(ncol(readRDS(input$target_upload$datapath)) == 18) {
          pool::dbWriteTable(studium, sub('\\.rds$', '', input$target_upload[[1]]),
                             df_products_upload(), overwrite = T)
          data[['studium']] <- pool::dbListTables(studium) %>%
            purrr::map(.f = function(x) pool::dbReadTable(studium, x)) %>%
            stats::setNames(pool::dbListTables(studium))
          index_list[['studium']] <- pool::dbListTables(studium) %>%
            purrr::map(.f = function(x) pool::dbReadTable(studium, x)) %>%
            stats::setNames(pool::dbListTables(studium)) %>%
            purrr::map(function(x) return(1:nrow(x)))
          updateSelectInput(session = session,
                            inputId =  "datset_studium",
                            choices = pool::dbListTables(studium))
        }
        shinyWidgets::sendSweetAlert(session,
                                     "Success!",
                                     text = i18n()$t("Your table was succesfully added to the database."),
                                     type = "success")
      } else {
        shinyWidgets::sendSweetAlert(session,
                                     "File",
                                     text = i18n()$t("Please select a valid file. The file must be of type 'rds'."))
      }
    })

    #   #################################################################################
    #   ################################### Output  #####################################
    #   #################################################################################
    #
    # Code to dinamically filter the data based on users selection
    index_list <- reactiveValues(
      pruefung = pool::dbListTables(pruefung) %>%
        purrr::map(.f = function(x) pool::dbReadTable(pruefung, x)) %>%
        stats::setNames(pool::dbListTables(pruefung)) %>%
        purrr::map(function(x) return(1:nrow(x))),
      studium = pool::dbListTables(studium) %>%
        purrr::map(.f = function(x) pool::dbReadTable(studium, x)) %>%
        stats::setNames(pool::dbListTables(studium)) %>%
        purrr::map(function(x) return(1:nrow(x))),
      fs = pool::dbListTables(fs) %>%
        purrr::map(.f = function(x) pool::dbReadTable(fs, x)) %>%
        stats::setNames(pool::dbListTables(fs)) %>%
        purrr::map(function(x) return(1:nrow(x)))
    )
    #
    # Observe if user filtered the datatable, set reactive index object
    observeEvent(input$table_fs, {
      if(nchar(input$datset_fs) != 0){
        if(!is.null(input$table_fs_rows_all)){
          index_list[['fs']][[input$datset_fs]] <-
            input$table_fs_rows_all
        }else{
          index_list[['fs']][[input$datset_fs]] <-
            1:nrow(pool::dbReadTable(fs, input$datset_fs))
        }
      }
    })
    #
    #   # fs
    observe({
      if(nchar(input$datset_fs) != 0){
        if(!is.null(input$table_fs_rows_all)){
          index_list[['fs']][[input$datset_fs]] <-
            input$table_fs_rows_all
        }else{
          index_list[['fs']][[input$datset_fs]] <-
            1:nrow(pool::dbReadTable(fs, input$datset_fs))
        }
      }})

    # studium
    observe({
      if(nchar(input$datset_studium) != 0){
        if(!is.null(input$table_studium_rows_all)){
          index_list[['studium']][[input$datset_studium]] <-
            input$table_studium_rows_all
        }else{
          index_list[['studium']][[input$datset_studium]] <-
            1:nrow(pool::dbReadTable(studium, input$datset_studium))
        }
      }})

    # pruefung
    observe({
      if(nchar(input$datset_pruefung) != 0){
        if(!is.null(input$table_pruefung_rows_all)){
          index_list[['pruefung']][[input$datset_pruefung]] <-
            input$table_pruefung_rows_all
        }else{
          index_list[['pruefung']][[input$datset_pruefung]] <-
            1:nrow(pool::dbReadTable(pruefung, input$datset_pruefung))
        }
      }})

    # Render UI Content  ####

    # Note, we are rendering some input elements inside the server because they
    # depend on the dataset() which isn't available inside UI

    # fs
    output$picker_compare_fs_exit_reasons_plot <- renderUI({

      shinyWidgets::pickerInput(
        inputId = "compare_fs_exit_reasons_plot",
        label = i18n()$t("Compare this Data to:"),
        choices = names(data[['fs']]) %>%
          dplyr::setdiff(input$datset_fs),
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      )

    })

    output$picker_compare_fs_num_graduates_plot <- renderUI({

      shinyWidgets::pickerInput(
        inputId = "compare_fs_num_graduates_plot",
        label = i18n()$t("Compare this Data to:"),
        choices = names(data[['fs']]) %>%
          dplyr::setdiff(input$datset_fs),
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      )

    })

    # pruefung
    output$picker_compare_pruefung_num_exams_plot <- renderUI({

      shinyWidgets::pickerInput(
        inputId = "compare_pruefung_num_exams_plot",
        label = i18n()$t("Compare this Data to:"),
        choices = names(data[['pruefung']]) %>%
          dplyr::setdiff(input$datset_pruefung),
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      )

    })

    output$picker_compare_pruefung_cum_cp_plot  <- renderUI({

      shinyWidgets::pickerInput(
        inputId = "compare_pruefung_cum_cp_plot",
        label = i18n()$t("Compare this Data to:"),
        choices = names(data[['pruefung']]) %>%
          dplyr::setdiff(input$datset_pruefung),
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      )

    })

    output$picker_compare_pruefung_cp_per_student_plot  <- renderUI({

      shinyWidgets::pickerInput(
        inputId = "compare_pruefung_cp_per_student_plot",
        label = i18n()$t("Compare this Data to:"),
        choices = names(data[['pruefung']]) %>%
          dplyr::setdiff(input$datset_pruefung),
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      )

    })

    output$picker_compare_pruefung_number_of_attempts_plot  <- renderUI({

      shinyWidgets::pickerInput(
        inputId = "compare_pruefung_number_of_attempts_plot",
        label = i18n()$t("Compare this Data to:"),
        choices = names(data[['pruefung']]) %>%
          dplyr::setdiff(input$datset_pruefung),
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      )

    })

    output$picker_compare_pruefung_semester_exams_plot  <- renderUI({

      shinyWidgets::pickerInput(
        inputId = "compare_pruefung_semester_exams_plot",
        label = i18n()$t("Compare this Data to:"),
        choices = names(data[['pruefung']]) %>%
          dplyr::setdiff(input$datset_pruefung),
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      )

    })

    # Get the exams which the user can choose from (attempts plot)

    pruefungen_attempts_plot<- reactiveVal()

    observe({

      if(nchar(input$datset_pruefung) > 1){
        pruefungen_attempts_plot(data %>% purrr::pluck('pruefung') %>%
                                   magrittr::extract(c(input$datset_pruefung,c(input$compare_pruefung_number_of_attempts_plot))) %>%
                                   purrr::map2(index_list[['pruefung']][c(input$datset_pruefung,
                                                                          c(input$compare_pruefung_number_of_attempts_plot))], slice) %>%
                                   purrr::map(function(x) {x %>% dplyr::select(Bezeichnung)}) %>%
                                   purrr::reduce(intersect) %>%
                                   distinct() %>%
                                   arrange(Bezeichnung) %>%
                                   mutate(rowname = strtrim(Bezeichnung, 100)) %>%
                                   dplyr::select(rowname, Bezeichnung) %>%
                                   tibble::deframe())
      }
    })

    # Get the exams which the user can choose from (semester plot)

    pruefungen_exam_plot<- reactiveVal()

    observe({

      if(nchar(input$datset_pruefung) > 1){
        pruefungen_exam_plot(data %>% purrr::pluck('pruefung') %>%
                               magrittr::extract(c(input$datset_pruefung,c(input$compare_pruefung_semester_exams_plot))) %>%
                               purrr::map2(index_list[['pruefung']][c(input$datset_pruefung,
                                                                      c(input$compare_pruefung_semester_exams_plot))], slice) %>%
                               purrr::map(function(x) {x %>% dplyr::select(Bezeichnung)}) %>%
                               purrr::reduce(intersect) %>%
                               distinct() %>%
                               arrange(Bezeichnung) %>%
                               mutate(rowname = strtrim(Bezeichnung, 100)) %>%
                               dplyr::select(rowname, Bezeichnung) %>%
                               tibble::deframe())
      }
    })

    # Picker um einzelne pruefungen herauszupicken (attempts plot)
    output$picker_select_pruefung  <- renderUI({

      shinyWidgets::pickerInput(
        inputId = "picker_select_pruefung_attempts_plot",
        label = i18n()$t("Modules to be compared:"),
        choices = pruefungen_attempts_plot(),
        selected = data %>% purrr::pluck('pruefung') %>%
          magrittr::extract(c(input$datset_pruefung,c(input$compare_pruefung_number_of_attempts_plot))) %>%
          purrr::map2(index_list[['pruefung']][c(input$datset_pruefung,
                                                 c(input$compare_pruefung_number_of_attempts_plot))], slice) %>%
          purrr::map(function(x) {x %>% dplyr::select(Bezeichnung)}) %>%
          purrr::reduce(intersect) %>%
          distinct() %>%
          arrange(Bezeichnung) %>%
          pull(Bezeichnung) %>%
          strtrim(100) %>% .[1:10],
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      )

    })

    # Picker um einzelne pruefungen herauszupicken (semester plot)
    output$picker_select_pruefung_semester  <- renderUI({

      shinyWidgets::pickerInput(
        inputId = "picker_select_pruefung_semsester_plot",
        label = i18n()$t("Modules to be compared:"),
        choices = pruefungen_attempts_plot(),
        selected = data %>% purrr::pluck('pruefung') %>%
          magrittr::extract(c(input$datset_pruefung,c(input$compare_pruefung_semester_exams_plot))) %>%
          purrr::map2(index_list[['pruefung']][c(input$datset_pruefung,
                                                 c(input$compare_pruefung_semester_exams_plot))], slice) %>%
          purrr::map(function(x) {x %>% dplyr::select(Bezeichnung)}) %>%
          purrr::reduce(intersect) %>%
          distinct() %>%
          arrange(Bezeichnung) %>%
          pull(Bezeichnung) %>%
          strtrim(100) %>% .[1:3],
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      )

    })

    # studium
    output$picker_compare_studium_study_length_plot <- renderUI({

      shinyWidgets::pickerInput(
        inputId = "compare_studium_study_length_plot",
        label = renderText({paste(i18n()$t("Compare this Data to:"))}),
        choices = names(data[['studium']]) %>%
          dplyr::setdiff(input$datset_studium),
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      )

    })

    # This will store the filters that the user applied to the dt
    # It's used to make the filters persistent
    dt_colfilters <- reactiveValues()
    dt_colfilters[['fs']] <- list()
    dt_colfilters[['studium']] <- list()
    dt_colfilters[['pruefung']] <- list()
    last_opened_fs <- reactiveVal()
    last_opened_studium <- reactiveVal()
    last_opened_pruefung <- reactiveVal()

    # Setup proxies, those are used to (re)apply filters to the respective tables
    proxy_fs <- DT::dataTableProxy("table_fs")
    proxy_studium <- DT::dataTableProxy("table_studium")
    proxy_pruefung <- DT::dataTableProxy("table_pruefung")

    # Render datatables and (re)apply filters which might be active ####

    # fs data
    observeEvent(input$datset_fs, {
      # Init the variable at session start
      if(is.null(last_opened_fs())){
        last_opened_fs(input$datset_fs)
        dt_colfilters[['fs']][[last_opened_fs()]] <- NULL
      }
      # On input$datset_fs change: store dt filters from the old dt
      dt_colfilters[['fs']][[last_opened_fs()]] <- input$table_fs_search_columns
      # Render the new dt
      output$table_fs <- DT::renderDT(pool::dbReadTable(fs, input$datset_fs), filter = "top",
                                      rownames = FALSE,
                                      options = list(scrollX = TRUE))
      # Apply the filters which are stored in dt_colfilters
      proxy_fs %>% DT::updateSearch(
        keywords = list(columns = dt_colfilters[['fs']][[input$datset_fs]])
      )
      # Save which table is opened
      last_opened_fs(input$datset_fs)
    })

    # studium data
    observeEvent(input$datset_studium, {
      # Init the variable at session start
      if(is.null(last_opened_studium())){
        last_opened_studium(input$datset_studium)
        dt_colfilters[['studium']][[last_opened_studium()]] <- NULL
      }
      # On input$datset_fs change: store dt filters from the old dt
      dt_colfilters[['studium']][[last_opened_studium()]] <-
        input$table_studium_search_columns
      # Render the new dt
      output$table_studium <- DT::renderDT(pool::dbReadTable(studium, input$datset_studium),
                                           filter = "top",
                                           rownames = FALSE,
                                           options = list(scrollX = TRUE))
      # Apply the filters which are stored in dt_colfilters
      proxy_studium %>% DT::updateSearch(
        keywords = list(columns = dt_colfilters[['studium']][[input$datset_studium]])
      )

      # Save which table is opened
      last_opened_studium(input$datset_studium)
    })

    # pruefung data
    observeEvent(input$datset_pruefung, {
      # Init the variable at session start
      if(is.null(last_opened_pruefung())){
        last_opened_pruefung(input$datset_pruefung)
        dt_colfilters[['pruefung']][[last_opened_pruefung()]] <- NULL
      }
      # On input$datset_fs change: store dt filters from the old dt
      dt_colfilters[['pruefung']][[last_opened_pruefung()]] <-
        input$table_pruefung_search_columns
      # Render the new dt
      output$table_pruefung <- DT::renderDT(pool::dbReadTable(pruefung, input$datset_pruefung),
                                            filter = "top",
                                            rownames = FALSE,
                                            options = list(scrollX = TRUE))
      # Apply the filters which are stored in dt_colfilters
      proxy_pruefung %>% DT::updateSearch(
        keywords = list(columns = dt_colfilters[['pruefung']][[input$datset_pruefung]])
      )
      # Save which table is opened
      last_opened_pruefung(input$datset_pruefung)
    })

    # render plots ####

    # fs data plots ####
    output$testplot <- renderPlot(
      data %>%
        # Select the dataset
        purrr::pluck('fs') %>%
        # Select the selected table and all comparison tables
        magrittr::extract(c(
          input$datset_fs,
          c(input$compare_fs_exit_reasons_plot)
        )) %>%
        # Slice the data according to the selections
        purrr::map2(index_list[['fs']][c(input$datset_fs,
                                         c(input$compare_fs_exit_reasons_plot))], slice) %>%
        # Select cols that we need for comparison
        purrr::map(function(x) {
          x %>%
            select(Student.Pseudonym, Austrittsgrund) %>%
            # arrange(Austrittsgrund) %>%
            group_by(Austrittsgrund) %>%
            summarise(count = n())
        }) %>%
        purrr::reduce(
          full_join,
          by = "Austrittsgrund",
          suffix = c(input$datset_fs,
                     c(input$compare_fs_exit_reasons_plot))
        ) %>%
        tidyr::pivot_longer(-Austrittsgrund, names_to = "Studiengang", names_prefix = "count",
                            values_to = "count") %>%
        tidyr::drop_na() %>%
        ggplot(aes(fill = switch((length(c(input$datset_fs,
                                           c(input$compare_fs_exit_reasons_plot))) == 1) + 2,
                                 NULL, Studiengang),
                   x = Austrittsgrund, y = count)) +
        geom_bar(position = "dodge", stat = "identity") +
        coord_flip() +
        theme(text = element_text(size = 25)) +
        labs(fill = i18n()$t("Study Programme")) +
        xlab(i18n()$t("Exit Reasons")) +
        ylab(i18n()$t("Number")) +
        cowplot::theme_cowplot()
    )

    plotInput_exit_reasons = function() {
      data %>%
        # Select the dataset
        purrr::pluck('fs') %>%
        # Select the selected table and all comparison tables
        magrittr::extract(c(
          input$datset_fs,
          c(input$compare_fs_exit_reasons_plot)
        )) %>%
        # Slice the data according to the selections
        purrr::map2(index_list[['fs']][c(input$datset_fs,
                                         c(input$compare_fs_exit_reasons_plot))], slice) %>%
        # Select cols that we need for comparison
        purrr::map(function(x) {
          x %>%
            select(Student.Pseudonym, Austrittsgrund) %>%
            # arrange(Austrittsgrund) %>%
            group_by(Austrittsgrund) %>%
            summarise(count = n())
        }) %>%
        purrr::reduce(
          full_join,
          by = "Austrittsgrund",
          suffix = c(input$datset_fs,
                     c(input$compare_fs_exit_reasons_plot))
        ) %>%
        tidyr::pivot_longer(-Austrittsgrund, names_to = "Studiengang", names_prefix = "count",
                            values_to = "count") %>%
        tidyr::drop_na() %>%
        ggplot(aes(fill = switch((length(c(input$datset_fs,
                                           c(input$compare_fs_exit_reasons_plot))) == 1) + 2,
                                 NULL, Studiengang),
                   x = Austrittsgrund, y = count)) +
        geom_bar(position = "dodge", stat = "identity") +
        coord_flip() +
        theme(text = element_text(size = 25)) +
        labs(fill = i18n()$t("Study Programme")) +
        xlab(i18n()$t("Exit Reasons")) +
        ylab(i18n()$t("Number")) +
        cowplot::theme_cowplot()
    }

    output$DL_exit_reasons = downloadHandler(
      filename = 'SPA_exit_reasons.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = 12, height = 6,
                         res = 300, units = "in")
        }
        ggsave(file, plot = plotInput_exit_reasons(), device = device)
      })

    # Description Exit Reasons
    # In order for the translation to work, it has to be written in one line

    output$description_exit_reasons_plot <- renderUI({

      shinyWidgets::dropdownButton(
        inputId = "mydropdown_001",
        icon = icon("info"),
        status = "primary",
        width = "400px",
        up = TRUE,
        right = TRUE,
        circle = FALSE,
        renderText(i18n()$t(
          "This bar chart shows the absolute number of students who drop out or complete their studies, depending on the reason for dropping out. It shows which reason for leaving led how often to the ending of the studies in the respective subject."
        )))})

    output$description_num_graduates_plot <- renderUI({

      shinyWidgets::dropdownButton(
        inputId = "mydropdown_002",
        icon = icon("info"),
        status = "primary",
        width = "400px",
        up = TRUE,
        right = TRUE,
        circle = FALSE,
        renderText(i18n()$t(
          "This bar chart shows the absolute number of students who graduate per semester."
        )))})

    output$description_num_exams_plot <- renderUI({
      shinyWidgets::dropdownButton(
        inputId = "mydropdown_003",
        icon = icon("info"),
        status = "primary",
        width = "500px",
        up = TRUE,
        right = TRUE,
        circle = FALSE,
        renderText(i18n()$t(
          "The bar chart shows for each semester the number of examinations for which students have registered. The semesters are displayed on the x-axis and the absolute number of assessments on the y-axis. If you filter, for example, the table above for the variable 'Status.Pruefung' by entering 'B', the system only displays the assessments which students have either passed (BE) or failed (NB). If a student were to withdraw from an assessment registration, he or she would still be included in the graphic at the beginning, but would not be included after the filtering process."
        )))})

    output$description_num_credits_plot <- renderUI({
      shinyWidgets::dropdownButton(
        inputId = "mydropdown_004",
        icon = icon("info"),
        status = "primary",
        width = "400px",
        up = TRUE,
        right = TRUE,
        circle = FALSE,
        renderText(i18n()$t(
          "The figure shows the average number of recorded ECTSCP at cumulative level. For the calculation, the variable 'verbuchteECTSCP' is first summed up for the individual semesters, then cumulated and finally averaged."
        )))})

    output$description_avg_earned_credits_plot <- renderUI({
      shinyWidgets::dropdownButton(
        inputId = "mydropdown_005",
        icon = icon("info"),
        status = "primary",
        width = "400px",
        up = TRUE,
        right = TRUE,
        circle = FALSE,
        renderText(i18n()$t(
          "The bar chart reflects the average number of ECTSCP acquired by the students per examination semester. The total number of gained ECTSCP in an examination semester is divided by the number of students enrolled in that semester."
        )))})

    output$description_number_of_attemps_plot <- renderUI({
      shinyWidgets::dropdownButton(
        inputId = "mydropdown_006",
        icon = icon("info"),
        status = "primary",
        width = "500px",
        up = TRUE,
        right = TRUE,
        circle = FALSE,
        renderText(i18n()$t(
          "The bar chart shows the average number of required attempts per subject. If the user does not filter the above table, the chart will also include students who have still not passed the subject or who have finished/cancelled their studies. The subjects to be compared can be selected using the selection button located above the graph. By default, the first ten modules are selected based on alphabetical order. If a subject can be selected by students from different courses, this illustration can for example be used to compare which students need more or less attempts."
        )))})

    output$description_semester_exams_plot <- renderUI({
      shinyWidgets::dropdownButton(
        inputId = "mydropdown_007",
        icon = icon("info"),
        status = "primary",
        width = "400px",
        up = TRUE,
        right = TRUE,
        circle = FALSE,
        renderText(i18n()$t(
          "Each bar chart shows the absolute number of students that have registered for the examination in a specific semester."
        )))})
    output$description_study_length_plot <- renderUI({
      shinyWidgets::dropdownButton(
        inputId = "mydropdown_008",
        icon = icon("info"),
        status = "primary",
        width = "400px",
        up = TRUE,
        right = TRUE,
        circle = FALSE,
        renderText(i18n()$t(
          "The bar chart shows the absolute number of students on the x-axis and the total number of semesters enrolled on the y-axis. If, for example, you add a student to the data set who is only enrolled in the study program for one semester, the bar increases by one value at y equals 1."
        )))})

    output$num_graduates_plot <- renderPlot(

      data %>%
        # Select the dataset
        purrr::pluck('fs') %>%
        # Select the selected table and all comparison tables
        magrittr::extract(c(input$datset_fs,
                            c(input$compare_fs_num_graduates_plot))) %>%
        # Slice the data according to the selections
        purrr::map2(index_list[['fs']][c(input$datset_fs,
                                         c(input$compare_fs_num_graduates_plot))], slice) %>%
        # Select cols that we need for comparison
        purrr::map(function(x){
          x %>%
            select(Student.Pseudonym, Semester, Austrittsgrund) %>%
            filter(stringr::str_detect(Austrittsgrund, "^Beendig.Stud.nach")) %>%
            group_by(Semester) %>%
            summarise(count = n())
        }) %>%
        purrr::reduce(full_join, by = "Semester", suffix = c(input$datset_fs,
                                                             c(input$compare_fs_num_graduates_plot))) %>%
        tidyr::pivot_longer(-Semester, names_to = "Studiengang", names_prefix = "count",
                            values_to = "count") %>%
        tidyr::drop_na() %>%
        ggplot(aes(x = factor(Semester), y = count,
                   fill = switch((length(c(input$datset_fs,
                                           c(input$compare_fs_num_graduates_plot))) == 1) + 2,
                                 NULL, Studiengang))) +
        geom_bar(position = "dodge", stat = "identity") +
        cowplot::theme_cowplot() +
        theme(text = element_text(size=25),
              axis.text.x = element_text(angle = 90)) +
        labs(fill = i18n()$t("Study Programme"))  +
        xlab("Semester") +
        ylab(i18n()$t("Number of Graduates"))
    )

    plotInput_num_graduates = function() {
      data %>%
        # Select the dataset
        purrr::pluck('fs') %>%
        # Select the selected table and all comparison tables
        magrittr::extract(c(input$datset_fs,
                            c(input$compare_fs_num_graduates_plot))) %>%
        # Slice the data according to the selections
        purrr::map2(index_list[['fs']][c(input$datset_fs,
                                         c(input$compare_fs_num_graduates_plot))], slice) %>%
        # Select cols that we need for comparison
        purrr::map(function(x){
          x %>%
            select(Student.Pseudonym, Semester, Austrittsgrund) %>%
            filter(stringr::str_detect(Austrittsgrund, "^Beendig.Stud.nach")) %>%
            group_by(Semester) %>%
            summarise(count = n())
        }) %>%
        purrr::reduce(full_join, by = "Semester", suffix = c(input$datset_fs,
                                                             c(input$compare_fs_num_graduates_plot))) %>%
        tidyr::pivot_longer(-Semester, names_to = "Studiengang", names_prefix = "count",
                            values_to = "count") %>%
        tidyr::drop_na() %>%
        ggplot(aes(x = factor(Semester), y = count,
                   fill = switch((length(c(input$datset_fs,
                                           c(input$compare_fs_num_graduates_plot))) == 1) + 2,
                                 NULL, Studiengang))) +
        geom_bar(position = "dodge", stat = "identity") +
        cowplot::theme_cowplot() +
        theme(text = element_text(size=25),
              axis.text.x = element_text(angle = 90)) +
        labs(fill = i18n()$t("Study Programme"))  +
        xlab("Semester") +
        ylab(i18n()$t("Number of Graduates"))
    }

    output$DL_num_graduates = downloadHandler(
      filename = 'SPA_number_graduates.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = 12, height = 6,
                         res = 300, units = "in")
        }
        ggsave(file, plot = plotInput_num_graduates(), device = device)
      })

    # pruefung data plots ####

    output$exams_per_semester_plot <- renderPlot(

      data %>%
        # Select the dataset
        purrr::pluck('pruefung') %>%
        # Select the selected table and all comparison tables
        magrittr::extract(c(input$datset_pruefung,
                            c(input$compare_pruefung_num_exams_plot))) %>%
        # Slice the data according to the selections
        purrr::map2(index_list[['pruefung']][c(input$datset_pruefung,
                                               c(input$compare_pruefung_num_exams_plot))], slice) %>%
        # Select cols that we need for comparison
        purrr::map(function(x){
          x %>%
            select(Student.Pseudonym, Pruefungssemester, verbuchteECTSCP) %>%
            # arrange(Pruefungssemester) %>%
            group_by(Pruefungssemester) %>%
            summarise(count = n()) %>%
            mutate(Pruefungssemester = forcats::as_factor(Pruefungssemester))
        }) %>%
        purrr::reduce(full_join, by = "Pruefungssemester", suffix = c(input$datset_pruefung,
                                                                      c(input$compare_pruefung_num_exams_plot))) %>%
        tidyr::pivot_longer(-Pruefungssemester, names_to = "Studiengang", names_prefix = "count",
                            values_to = "count") %>%
        tidyr::drop_na() %>%
        ggplot(aes(x = Pruefungssemester, y = count,
                   fill = switch((length(c(input$datset_pruefung,
                                           c(input$compare_pruefung_num_exams_plot))) == 1) + 2, NULL, Studiengang))) +
        geom_bar(position = "dodge", stat = "identity") +
        #geom_point() +
        #geom_smooth() +
        cowplot::theme_cowplot() +
        theme(text = element_text(size=25),
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
        labs(fill = i18n()$t("Study Programme"))  +
        xlab(i18n()$t("Semester of Examination")) +
        ylab(i18n()$t("Number of Exams"))
    )

    plotInput_eps = function() {
      data %>%
        # Select the dataset
        purrr::pluck('pruefung') %>%
        # Select the selected table and all comparison tables
        magrittr::extract(c(input$datset_pruefung,
                            c(input$compare_pruefung_num_exams_plot))) %>%
        # Slice the data according to the selections
        purrr::map2(index_list[['pruefung']][c(input$datset_pruefung,
                                               c(input$compare_pruefung_num_exams_plot))], slice) %>%
        # Select cols that we need for comparison
        purrr::map(function(x){
          x %>%
            select(Student.Pseudonym, Pruefungssemester, verbuchteECTSCP) %>%
            # arrange(Pruefungssemester) %>%
            group_by(Pruefungssemester) %>%
            summarise(count = n()) %>%
            mutate(Pruefungssemester = forcats::as_factor(Pruefungssemester))
        }) %>%
        purrr::reduce(full_join, by = "Pruefungssemester", suffix = c(input$datset_pruefung,
                                                                      c(input$compare_pruefung_num_exams_plot))) %>%
        tidyr::pivot_longer(-Pruefungssemester, names_to = "Studiengang", names_prefix = "count",
                            values_to = "count") %>%
        tidyr::drop_na() %>%
        ggplot(aes(x = Pruefungssemester, y = count,
                   fill = switch((length(c(input$datset_pruefung,
                                           c(input$compare_pruefung_num_exams_plot))) == 1) + 2, NULL, Studiengang))) +
        geom_bar(position = "dodge", stat = "identity") +
        theme(text = element_text(size = 25), legend.position = "bottom",
              axis.text.x = element_text(angle = 90)) +
        labs(fill = i18n()$t("Study Programme"))  +
        xlab(i18n()$t("Semester of Examination")) +
        ylab(i18n()$t("Number of Exams")) +
        cowplot::theme_cowplot()
    }

    output$DL_eps = downloadHandler(
      filename = 'SPA_exams_per_semester.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = 18, height = 6,
                         res = 300, units = "in")
        }
        ggsave(file, plot = plotInput_eps(), device = device)
      })



    output$cum_credits_plot <- renderPlot(

      data %>%
        # Select the dataset
        purrr::pluck('pruefung') %>%
        # Select the selected table and all comparison tables
        magrittr::extract(c(input$datset_pruefung,
                            c(input$compare_pruefung_cum_cp_plot))) %>%
        # Slice the data according to the selections
        purrr::map2(index_list[['pruefung']][c(input$datset_pruefung,
                                               c(input$compare_pruefung_cum_cp_plot))], slice) %>%
        # Select cols that we need for comparison
        purrr::map(function(x){
          x %>%
            select(Student.Pseudonym, FS, verbuchteECTSCP) %>%
            group_by(Student.Pseudonym, FS) %>%
            summarise(count = n(), cp_sum = sum(verbuchteECTSCP, na.rm = TRUE)) %>%
            ungroup() %>% group_by(FS) %>%
            summarise(mean_cp = mean(cp_sum)) %>%
            mutate(cum_credits = cumsum(mean_cp)) %>%
            select(-mean_cp)
        }) %>%
        purrr::reduce(full_join, by = "FS", suffix = c(input$datset_pruefung,
                                                       c(input$compare_pruefung_cum_cp_plot))) %>%
        tidyr::pivot_longer(-FS, names_to = "Studiengang", names_prefix = "cum_credits",
                            values_to = "count") %>%
        tidyr::drop_na() %>%
        ggplot(aes(x = FS, y = count,
                   col = switch((length(c(input$datset_pruefung,
                                          c(input$compare_pruefung_cum_cp_plot))) == 1) + 2,
                                NULL, Studiengang))) +
        geom_line(size = 0.5) +
        geom_point(size = 3) +
        cowplot::theme_cowplot() +
        theme(text = element_text(size=25)) +
        labs(col = "Studiengang")  +
        xlab(i18n()$t("Academic Semester")) +
        ylab(i18n()$t("Number of Credits earned"))
    )

    plotInput_ccp = function() {
      data %>%
        # Select the dataset
        purrr::pluck('pruefung') %>%
        # Select the selected table and all comparison tables
        magrittr::extract(c(input$datset_pruefung,
                            c(input$compare_pruefung_cum_cp_plot))) %>%
        # Slice the data according to the selections
        purrr::map2(index_list[['pruefung']][c(input$datset_pruefung,
                                               c(input$compare_pruefung_cum_cp_plot))], slice) %>%
        # Select cols that we need for comparison
        purrr::map(function(x){
          x %>%
            select(Student.Pseudonym, FS, verbuchteECTSCP) %>%
            group_by(Student.Pseudonym, FS) %>%
            summarise(count = n(), cp_sum = sum(verbuchteECTSCP, na.rm = TRUE)) %>%
            ungroup() %>% group_by(FS) %>%
            summarise(mean_cp = mean(cp_sum)) %>%
            mutate(cum_credits = cumsum(mean_cp)) %>%
            select(-mean_cp)
        }) %>%
        purrr::reduce(full_join, by = "FS", suffix = c(input$datset_pruefung,
                                                       c(input$compare_pruefung_cum_cp_plot))) %>%
        tidyr::pivot_longer(-FS, names_to = "Studiengang", names_prefix = "cum_credits",
                            values_to = "count") %>%
        tidyr::drop_na() %>%
        ggplot(aes(x = FS, y = count,
                   col = switch((length(c(input$datset_pruefung,
                                          c(input$compare_pruefung_cum_cp_plot))) == 1) + 2,
                                NULL, Studiengang))) +
        geom_line(size = 0.5) +
        geom_point(size = 3) +
        cowplot::theme_cowplot() +
        theme(text = element_text(size = 25)) +
        labs(col = "Studiengang")  +
        xlab(i18n()$t("Academic Semester")) +
        ylab(i18n()$t("Number of Credits earned"))
    }

    output$DL_ccp = downloadHandler(
      filename = 'SPA_cumulative_credit_points.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = 15, height = 6,
                         res = 300, units = "in")
        }
        ggsave(file, plot = plotInput_ccp(), device = device)
      })


    output$credits_per_student_plot <- renderPlot(

      data %>%
        # Select the dataset
        purrr::pluck('pruefung') %>%
        # Select the selected table and all comparison tables
        magrittr::extract(c(input$datset_pruefung,
                            c(input$compare_pruefung_cp_per_student_plot))) %>%
        # Slice the data according to the selections
        purrr::map2(index_list[['pruefung']][c(input$datset_pruefung,
                                               c(input$compare_pruefung_cp_per_student_plot))], slice) %>%
        # Select cols that we need for comparison
        purrr::map(function(x){
          x %>%
            select(Student.Pseudonym, Pruefungssemester, verbuchteECTSCP) %>%
            group_by(Pruefungssemester, Student.Pseudonym) %>%
            summarise(cp_sum = sum(verbuchteECTSCP, na.rm = TRUE)) %>%
            group_by(Pruefungssemester) %>%
            summarise(mean_cp = mean(cp_sum)) %>%
            mutate(Pruefungssemester = forcats::as_factor(Pruefungssemester))
        }) %>%
        purrr::reduce(full_join, by = "Pruefungssemester", suffix = c(input$datset_pruefung,
                                                                      c(input$compare_pruefung_cp_per_student_plot))) %>%
        tidyr::pivot_longer(-Pruefungssemester, names_to = "Studiengang", names_prefix = "mean_cp",
                            values_to = "mean_cp") %>%
        tidyr::drop_na() %>%
        ggplot(aes(x = Pruefungssemester, y = mean_cp,
                   fill = switch((length(c(input$datset_pruefung,
                                           c(input$compare_pruefung_cp_per_student_plot))) == 1) + 2,
                                 NULL, Studiengang))) +
        geom_bar(position = "dodge", stat = "identity") +
        cowplot::theme_cowplot() +
        theme(text = element_text(size=25)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
        labs(fill = i18n()$t("Study Programme"))  +
        xlab(i18n()$t("Semester of Examination")) +
        ylab(i18n()$t("Avg. of earned Credits"))
    )

    plotInput_cps = function() {
      data %>%
        # Select the dataset
        purrr::pluck('pruefung') %>%
        # Select the selected table and all comparison tables
        magrittr::extract(c(input$datset_pruefung,
                            c(input$compare_pruefung_cp_per_student_plot))) %>%
        # Slice the data according to the selections
        purrr::map2(index_list[['pruefung']][c(input$datset_pruefung,
                                               c(input$compare_pruefung_cp_per_student_plot))], slice) %>%
        # Select cols that we need for comparison
        purrr::map(function(x){
          x %>%
            select(Student.Pseudonym, Pruefungssemester, verbuchteECTSCP) %>%
            group_by(Pruefungssemester, Student.Pseudonym) %>%
            summarise(cp_sum = sum(verbuchteECTSCP, na.rm = TRUE)) %>%
            group_by(Pruefungssemester) %>%
            summarise(mean_cp = mean(cp_sum)) %>%
            mutate(Pruefungssemester = forcats::as_factor(Pruefungssemester))
        }) %>%
        purrr::reduce(full_join, by = "Pruefungssemester", suffix = c(input$datset_pruefung,
                                                                      c(input$compare_pruefung_cp_per_student_plot))) %>%
        tidyr::pivot_longer(-Pruefungssemester, names_to = "Studiengang", names_prefix = "mean_cp",
                            values_to = "mean_cp") %>%
        tidyr::drop_na() %>%
        ggplot(aes(x = Pruefungssemester, y = mean_cp,
                   fill = switch((length(c(input$datset_pruefung,
                                           c(input$compare_pruefung_cp_per_student_plot))) == 1) + 2,
                                 NULL, Studiengang))) +
        geom_bar(position = "dodge", stat = "identity") +
        cowplot::theme_cowplot() +
        theme(text = element_text(size=25)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
        labs(fill = i18n()$t("Study Programme"))  +
        xlab(i18n()$t("Semester of Examination")) +
        ylab(i18n()$t("Avg. of earned Credits"))
    }

    output$DL_cps = downloadHandler(
      filename = 'SPA_credits_per_student.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = 12, height = 6,
                         res = 300, units = "in")
        }
        ggsave(file, plot = plotInput_cps(), device = device)
      })


    #DL_cps

    output$number_of_attempts_plot <- renderPlot(
      data %>%
        # Select the dataset
        purrr::pluck('pruefung') %>%
        # Select the selected table and all comparison tables
        magrittr::extract(c(
          input$datset_pruefung,
          c(input$compare_pruefung_number_of_attempts_plot)
        )) %>%
        # Slice the data according to the selections
        purrr::map2(index_list[['pruefung']][c(input$datset_pruefung,
                                               c(input$compare_pruefung_number_of_attempts_plot))], slice) %>%
        # Select cols that we need for comparison
        purrr::map(function(x) {
          x %>%
            select(Student.Pseudonym, Bezeichnung) %>%
            # arrange(Pruefungssemester) %>%
            group_by(Bezeichnung) %>%
            summarise(count = n(), n_stud = length(unique(Student.Pseudonym, na.rm = TRUE))) %>%
            mutate(count = count / n_stud, n_stud = NULL)
        }) %>%
        purrr::reduce(full_join, by = "Bezeichnung", suffix = c(input$datset_pruefung,
                                                                c(input$compare_pruefung_number_of_attempts_plot))
        ) %>%
        dplyr::filter(Bezeichnung %in% input$picker_select_pruefung_attempts_plot) %>%
        # mutate(Bezeichnung = strtrim(Bezeichnung, 20)) %>%
        tidyr::pivot_longer(-Bezeichnung, names_to = "Studiengang", names_prefix = "count",
                            values_to = "count") %>%
        tidyr::drop_na() %>%
        ggplot(aes(fill = switch((length(c(input$datset_pruefung,
                                           c(input$compare_pruefung_number_of_attempts_plot))) == 1) + 2,
                                 NULL, Studiengang),
                   x = stringr::str_wrap(Bezeichnung, 50), y = count)) +
        geom_bar(position = "dodge", stat = "identity") +
        #coord_flip() +
        theme(text = element_text(size = 20)) +
        theme(legend.position = "bottom") +
        labs(fill = i18n()$t("Study Programme")) +
        xlab(i18n()$t("Exam")) +
        ylab(i18n()$t("Avg. No. of Attempts")) +
        coord_flip() +
        cowplot::theme_cowplot()
    )

    plotInput_noa = function() {
      data %>%
        # Select the dataset
        purrr::pluck('pruefung') %>%
        # Select the selected table and all comparison tables
        magrittr::extract(c(
          input$datset_pruefung,
          c(input$compare_pruefung_number_of_attempts_plot)
        )) %>%
        # Slice the data according to the selections
        purrr::map2(index_list[['pruefung']][c(input$datset_pruefung,
                                               c(input$compare_pruefung_number_of_attempts_plot))], slice) %>%
        # Select cols that we need for comparison
        purrr::map(function(x) {
          x %>%
            select(Student.Pseudonym, Bezeichnung) %>%
            # arrange(Pruefungssemester) %>%
            group_by(Bezeichnung) %>%
            summarise(count = n(), n_stud = length(unique(Student.Pseudonym, na.rm = TRUE))) %>%
            mutate(count = count / n_stud, n_stud = NULL)
        }) %>%
        purrr::reduce(full_join, by = "Bezeichnung", suffix = c(input$datset_pruefung,
                                                                c(input$compare_pruefung_number_of_attempts_plot))
        ) %>%
        dplyr::filter(Bezeichnung %in% input$picker_select_pruefung_attempts_plot) %>%
        # mutate(Bezeichnung = strtrim(Bezeichnung, 20)) %>%
        tidyr::pivot_longer(-Bezeichnung, names_to = "Studiengang", names_prefix = "count",
                            values_to = "count") %>%
        tidyr::drop_na() %>%
        ggplot(aes(fill = switch((length(c(input$datset_pruefung,
                                           c(input$compare_pruefung_number_of_attempts_plot))) == 1) + 2,
                                 NULL, Studiengang),
                   x = stringr::str_wrap(Bezeichnung, 50), y = count)) +
        geom_bar(position = "dodge", stat = "identity") +
        #coord_flip() +
        theme(text = element_text(size = 20)) +
        labs(fill = i18n()$t("Study Programme")) +
        xlab(i18n()$t("Exam")) +
        ylab(i18n()$t("Avg. No. of Attempts")) +
        coord_flip()
    }

    output$DL_noa = downloadHandler(
      filename = 'SPA_number_of_attempts.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = 12, height = 6,
                         res = 300, units = "in")
        }
        ggsave(file, plot = plotInput_noa(), device = device)
      })


    output$exams_semester_plot <- renderPlot(
      data %>%
        # Select the dataset
        purrr::pluck('pruefung') %>%
        # Select the selected table and all comparison tables
        magrittr::extract(c(
          input$datset_pruefung,
          c(input$compare_pruefung_semester_exams_plot)
        )) %>%
        # Slice the data according to the selections
        purrr::map2(index_list[['pruefung']][c(input$datset_pruefung,
                                               c(input$compare_pruefung_semester_exams_plot))], slice) %>%
        # Select cols that we need for comparison
        purrr::map2(names(.), function(x,y){
          x %>%
            select(Bezeichnung, FS) %>%
            dplyr::filter(Bezeichnung %in% input$picker_select_pruefung_semsester_plot) %>%
            mutate(Studiengang =y)}) %>%
        purrr::reduce(bind_rows) %>%
        ggplot(aes(x = factor(FS), fill = switch((length(c(input$datset_pruefung,
                                                           c(input$compare_pruefung_semester_exams_plot))) == 1) + 2,
                                                 NULL, Studiengang))) +
        geom_bar(position = "dodge") +
        facet_wrap(Bezeichnung~., scales = "free_y", ncol = 1) +
        cowplot::theme_cowplot() +
        labs(fill = i18n()$t("Study Programme"),
             x = "Semester",
             y = "Anzahl")
    )

    plotInput_se = function() {
      data %>%
        # Select the dataset
        purrr::pluck('pruefung') %>%
        # Select the selected table and all comparison tables
        magrittr::extract(c(
          input$datset_pruefung,
          c(input$compare_pruefung_semester_exams_plot)
        )) %>%
        # Slice the data according to the selections
        purrr::map2(index_list[['pruefung']][c(input$datset_pruefung,
                                               c(input$compare_pruefung_semester_exams_plot))], slice) %>%
        # Select cols that we need for comparison
        purrr::map2(names(.), function(x,y){
          x %>%
            select(Bezeichnung, FS) %>%
            dplyr::filter(Bezeichnung %in% input$picker_select_pruefung_semsester_plot) %>%
            mutate(Studiengang =y)}) %>%
        purrr::reduce(bind_rows) %>%
        ggplot(aes(x = factor(FS), fill = switch((length(c(input$datset_pruefung,
                                                           c(input$compare_pruefung_semester_exams_plot))) == 1) + 2,
                                                 NULL, Studiengang))) +
        geom_bar(position = "dodge") +
        facet_wrap(Bezeichnung~., scales = "free_y", ncol = 1) +
        cowplot::theme_cowplot() +
        labs(fill = i18n()$t("Study Programme"),
             x = "Semester",
             y = "Anzahl")
    }

    output$DL_se = downloadHandler(
      filename = 'SPA_exams_semester.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = 12, height = 6,
                         res = 300, units = "in")
        }
        ggsave(file, plot = plotInput_se(), device = device)
      })

    # studium data plots ####
    output$study_length <- renderPlot(
      data %>%
        # Select the dataset
        purrr::pluck('studium') %>%
        # Select the selected table and all comparison tables
        magrittr::extract(c(
          input$datset_studium,
          c(input$compare_studium_study_length_plot)
        )) %>%
        # Slice the data according to the selections
        purrr::map2(index_list[['studium']][c(input$datset_studium,
                                              c(input$compare_studium_study_length_plot))], slice) %>%
        # Select cols that we need for comparison
        purrr::map(function(x) {
          x %>%
            mutate(student_total = 1+floor((Ende_Semester-Start_Semester)/5)) %>%
            select(Student.Pseudonym, student_total) %>%
            # arrange(Pruefungssemester) %>%
            group_by(student_total) %>%
            summarise(count = n())
        }) %>%
        purrr::reduce(
          full_join,
          by = "student_total",
          suffix = c(input$datset_studium,
                     c(input$compare_studium_study_length_plot))
        ) %>%
        tidyr::pivot_longer(-student_total, names_to = "Studiengang",  names_prefix = "count",
                            values_to = "count") %>%
        tidyr::drop_na() %>%
        ggplot(aes(fill = switch((length(c(input$datset_studium,
                                           c(input$compare_studium_study_length_plot))) == 1) + 2,
                                 NULL, Studiengang),
                   x = student_total, y = count)) +
        geom_bar(position = "dodge", stat = "identity") +
        #coord_flip() +
        theme(text = element_text(size = 25)) +
        #theme(legend.position="bottom") +
        labs(fill = i18n()$t("Study Programme")) +
        xlab(i18n()$t("Total No. of Semesters enrolled")) +
        ylab(i18n()$t("Number of Students")) +
        cowplot::theme_cowplot()
    )


    plotInput_sl = function() {
      data %>%
        # Select the dataset
        purrr::pluck('studium') %>%
        # Select the selected table and all comparison tables
        magrittr::extract(c(
          input$datset_studium,
          c(input$compare_studium_study_length_plot)
        )) %>%
        # Slice the data according to the selections
        purrr::map2(index_list[['studium']][c(input$datset_studium,
                                              c(input$compare_studium_study_length_plot))], slice) %>%
        # Select cols that we need for comparison
        purrr::map(function(x) {
          x %>%
            mutate(student_total = 1+floor((Ende_Semester-Start_Semester)/5)) %>%
            select(Student.Pseudonym, student_total) %>%
            # arrange(Pruefungssemester) %>%
            group_by(student_total) %>%
            summarise(count = n())
        }) %>%
        purrr::reduce(
          full_join,
          by = "student_total",
          suffix = c(input$datset_studium,
                     c(input$compare_studium_study_length_plot))
        ) %>%
        tidyr::pivot_longer(-student_total, names_to = "Studiengang",  names_prefix = "count",
                            values_to = "count") %>%
        tidyr::drop_na() %>%
        ggplot(aes(fill = switch((length(c(input$datset_studium,
                                           c(input$compare_studium_study_length_plot))) == 1) + 2,
                                 NULL, Studiengang),
                   x = student_total, y = count)) +
        geom_bar(position = "dodge", stat = "identity") +
        #coord_flip() +
        theme(text = element_text(size = 25)) +
        theme(legend.position="bottom") +
        labs(fill = i18n()$t("Study Programme")) +
        xlab("Insgesamt eingeschriebene Semester") +
        ylab("Anzahl Studenten")
    }

    output$DL_sl = downloadHandler(
      filename = 'SPA_length_of_study.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = 12, height = 6,
                         res = 300, units = "in")
        }
        ggsave(file, plot = plotInput_sl(), device = device)
      })

    # Hide the "compare" inputs when comparison isn't possible:

    observe({

      # FS data
      shinyjs::toggle(id = "picker_compare_fs_exit_reasons_plot",
                      condition = length(names(data[['fs']])) > 1)
      shinyjs::toggle(id = "picker_compare_fs_num_graduates_plot",
                      condition = length(names(data[['fs']])) > 1)
      # Pruefung data
      shinyjs::toggle(id = "picker_compare_pruefung_num_exams_plot",
                      condition = length(names(data[['pruefung']])) > 1)
      shinyjs::toggle(id = "picker_compare_pruefung_cum_cp_plot",
                      condition = length(names(data[['pruefung']])) > 1)
      shinyjs::toggle(id = "picker_compare_pruefung_cp_per_student_plot",
                      condition = length(names(data[['pruefung']])) > 1)
      shinyjs::toggle(id = "picker_compare_pruefung_number_of_attempts_plot",
                      condition = length(names(data[['pruefung']])) > 1)
      shinyjs::toggle(id = "picker_compare_pruefung_semester_exams_plot",
                      condition = length(names(data[['pruefung']])) > 1)
      # Studium data
      shinyjs::toggle(id = "picker_compare_studium_study_length_plot",
                      condition = length(names(data[['studium']])) > 1)

    })

    # Hide Boxes when DB is empty and show hint
    observe({

      # FS data
      shinyjs::toggle(id = "fs_box_missing",
                      condition = length(names(data[['fs']])) == 0)
      shinyjs::toggle(id = "fs_boxes_content",
                      condition = length(names(data[['fs']])) > 0)

      # Pruefung data
      shinyjs::toggle(id = "pruefung_box_missing",
                      condition = length(names(data[['pruefung']])) == 0)
      shinyjs::toggle(id = "pruefung_boxes_content",
                      condition = length(names(data[['pruefung']])) > 0)

      # Studium data
      shinyjs::toggle(id = "studium_box_missing",
                      condition = length(names(data[['studium']])) == 0)
      shinyjs::toggle(id = "studium_boxes_content",
                      condition = length(names(data[['studium']])) > 0)
    })

  }

  return(shinyApp(ui, server))

}
