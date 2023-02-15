## To be copied in the UI
# mod_station_ui("station_ui_1")

## To be copied in the server
# callModule(mod_station_server, "station_ui_1")
#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinydashboard dashboardSidebar
#' @noRd
app_ui_hydb <- function(request) {
  tagList(
    shinydashboard::dashboardPage(

      header = shinydashboard::dashboardHeader(title = "Hydroclimatic Data"),

    # Leave this function for adding external resources
    golem_add_external_resources(),

      sidebar = shinydashboard::dashboardSidebar(

        dashboardthemes::shinyDashboardThemes(theme = "blue_gradient"),

        shinydashboard::sidebarMenu(id = 'menu1',

        shinydashboard::menuItem(
                          "Welcome",
                          tabName = "get_started",
                          icon = icon("door-open")
                        ),
        shinydashboard::menuItem(
                          "Upload Hydro Data",
                          tabName = "upload",
                          icon = icon("tint")),
        conditionalPanel(condition = "input.menu1 === 'upload'",
                         fileInput("file", "Please Select File",
                                   accept = c('.xlsx', '.csv', '.txt', '.tsv'))

        ))),
      body = shinydashboard::dashboardBody(
        shinydashboard::tabItems(
                          shinydashboard::tabItem(
                          tabName = "get_started",
                          tags$style(type = 'text/css', '#welcome {height: calc(100vh - 80px) !important;}'),htmltools::tags$iframe(src = "hydb_welcome.html", width = '100%',  height = 1000,  style = "border:none;")
                        ),
        shinydashboard::tabItem(

          tags$head(tags$style("
                             .shiny-notification {
             margin-left: -10px !important;
             height: 40px !important;
             border-color: black;
             }
             ")),
          tags$head(tags$style(".modal-dialog{ width:350px}")),
          tags$head(tags$style(".modal-body{ min-height:25px}")),
          tabName = 'upload',
          fluidRow(shinydashboard::tabBox(width = 12, id = 'tabchart',
                                  tabPanel(
                               title = 'Upload Data',
                               syle = 'height:92vh',
                      hydbModUI("hydb_ui_1"),
                      uiOutput('select_table'),
                      uiOutput("sid"),
                      actionButton('begin_upload', 'Submit Table')
          )

          ))
        )))
        )
)
}



#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server_hydb <- function( input, output, session ) {
  # List the first level callModules here
  values <- reactiveValues()

  path <- 'T:/FS/NFS/Kootenai/Program/2500Watershed/GIS/SO/hydb'

  values$mydb <- DBI::dbConnect(RSQLite::SQLite(), paste0(path,"/hydb.sqlite"))

  output$select_table <- shiny::renderUI(shiny::selectInput('db_table',
                     label = 'Select a Database Table to Upload into.',
                     selected = '',
                     choices = c('', DBI::dbListTables(values$mydb))))

  output$sid <- shiny::renderUI(shiny::selectInput(
    'sid',
    "Station ID",
    choices = c('',fetch_hydb('metadata')$station_nm),
    selected=""
  ))

  callModule(hydbMod, "hydb_ui_1", values = values, file_path = reactive(input$file))

  observeEvent(input$tabchart, once = TRUE, {

    shiny::showModal(modalDialog(title = "Welcome new user",
                                  easyClose = FALSE,
                                  footer = actionButton('done_welcome_upload','Done'),
                                  tags$style(
                                    type = 'text/css',
                                    '.modal-dialog {
    width: fit-content !important;
    margin: 100px;}'
                                  )))})

  values$table_selection <- reactive(input$db_table)

  observeEvent(input$begin_upload,{

    req(input$db_table != '')

    # error catching
    #check lengths based on table type

    if(!error_catching_input(values$selected_df(), values$table_selection())) {

      statement <- switch(sub('.*\\_', '', values$table_selection()),
             'dv' = paste0('"',values$table_selection(),'"',
                           ' Requires ', 2, ' columns.'),
             'iv' = paste0('"',values$table_selection(),'"',
                           ' Requires ', 2, ' columns.'),
             'obs' = paste0('"',values$table_selection(),'"',
                           ' Requires ', 2 ,' or ', 3, ' columns.'))

      shinyWidgets::show_alert(statement,
                               'please reselect',
                               type = 'warning'
                                )

    } else if (!error_catching_param_names(values$selected_df(), values$table_selection())[[1]]) {



      statement <- switch(sub('.*\\_', '', values$table_selection()),
                          'dv' = paste0('"',values$table_selection(),'"',
                                        ' Requires ', '"value" and "date" columns.'),
                          'iv' = paste0('"',values$table_selection(),'"',
                                        ' Requires ', '"value" and "dt" columns.'),
                          'obs' = paste0('"',values$table_selection(),'"',
                                         ' Requires ', '"value", "date", and/or "time" columns.'))

      shinyWidgets::show_alert(statement,
                               'please reselect with the correct column name',
                               type = 'warning'
      )

    } else {

    showModal(modalDialog(
      title = "Data that you selected",
      footer = tagList(
                modalButton('Cancel'),
                actionButton('done_begin_upload','Upload')),
    easyClose = TRUE,
      tags$style(
        type = 'text/css',
        '.modal-dialog {
    width: fit-content !important;}'
      ),
    shinydashboard::box(width = 12,
                        DT::dataTableOutput('selected_df_dt'))

    ))
}
  })




  output$selected_df_dt <- DT::renderDataTable({

  values$station_sid <- reactive({
                    fetch_hydb('metadata') %>%
                    dplyr::filter(station_nm %in% input$sid) %>%
                    dplyr::pull(sid)
                                    })

  print(values$station_sid())
  quality_controlled_df <-

    if(sub('.*\\_', '', values$table_selection()) %in% 'obs'){

      if(any(names(error_catching_param_names(values$selected_df(), values$table_selection())[[2]]) %in% 'time')){

      error_catching_param_names(values$selected_df(), values$table_selection())[[2]] %>%
        dplyr::mutate(sid = values$station_sid()) %>%
          na.omit()

      } else {

        error_catching_param_names(values$selected_df(), values$table_selection())[[2]] %>%
          dplyr::mutate(sid = values$station_sid(),
                        time = NA_real_)

      }

    } else {

      error_catching_param_names(values$selected_df(), values$table_selection())[[2]] %>%
                           dplyr::mutate(sid = values$station_sid()) %>%
                           na.omit()

    }

  values$selected_df2 <- reactive(switch(sub('.*\\_', '', values$table_selection()),
                        'dv' = quality_controlled_df %>%
                          dplyr::mutate(date = lubridate::as_date(date)),
                        'iv' = quality_controlled_df %>%
                          dplyr::mutate(dt = lubridate::as_datetime(dt)),
                        'obs' = quality_controlled_df %>%
                          dplyr::mutate(date = lubridate::as_date(date))))

  DT::datatable(values$selected_df2())


  })

  observeEvent(input$done_welcome_upload, {
    removeModal()

  })


  observeEvent(input$done_begin_upload,{


    values$identical <- switch(sub('.*\\_', '', values$table_selection()),
                               'dv' = all.equal(fetch_hydb(values$table_selection(), values$station_sid()) %>%
                                                  dplyr::filter(date %in% values$selected_df2()$date),values$selected_df2(), check.attributes = F),
                               'iv' = all.equal(fetch_hydb(values$table_selection(), values$station_sid()) %>%
                                                  dplyr::filter(dt %in% values$selected_df2()$dt),values$selected_df2(), check.attributes = F),
                               'obs' = all.equal(fetch_hydb(values$table_selection(), values$station_sid()) %>%
                                                   dplyr::filter(date %in% values$selected_df2()$date,
                                                                 time %in% values$selected_df2()$time),values$selected_df2(), check.attributes = F))

    print(values$station_sid())
    print(fetch_hydb(values$table_selection(), values$station_sid()) %>%
            dplyr::filter(date %in% values$selected_df2()$date))
    if(length(values$identical) == 1){

      shinyWidgets::show_alert("Looks like you've added this dataset before...",
                               'please reselect and try again',
                               type = 'warning'
      )

    } else {

  showModal(modalDialog(
    title = "Final Chance to Bail",
      footer = tagList(
        modalButton('Cancel'),
        actionButton('done_begin_final_upload','Upload')),
    easyClose = TRUE,
    tags$style(
      type = 'text/css',
      '.modal-dialog {
    width: fit-content !important;}'
    ),
    shinydashboard::box(width = 12, paste0('Are you sure you want to add ',
                                           values$station_sid(),' with ', values$table_selection(),
                                           ' into the database?'
                                           )
                        )))
    }
})

  observeEvent(input$done_begin_final_upload, {



    dbAppendTable(values$mydb, values$table_selection(), values$selected_df2())


    removeModal()

  })

  sessionEnded <- session$onSessionEnded(function() {

    DBI::dbDisconnect(values$mydb)

  })

}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  golem::add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys('app/www'),
      app_title = 'hydb'
    ),
    shinyjs::useShinyjs()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
