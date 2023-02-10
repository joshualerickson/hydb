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

  observeEvent(input$begin_upload,{

    req(input$db_table != '')

    showModal(modalDialog(
      title = "Uploading the current data",
      easyClose = FALSE,
      footer = actionButton('done_begin_upload','Done'),
      tags$style(
        type = 'text/css',
        '.modal-dialog {
    width: fit-content !important;}'
      ),
    shinydashboard::box(width = 4,
                        shiny::selectInput("sid", "Station ID",
                                                     choices = fetch_hydb('metadata')$station_nm,
                                                     selected=" ")),
    shinydashboard::box(width = 8,
                        DT::dataTableOutput('selected_df_dt'))

    ))

  })




  output$selected_df_dt <- DT::renderDataTable({

  station_sid <- reactive({
                    fetch_hydb('metadata') %>%
                    dplyr::filter(station_nm %in% input$sid) %>%
                    dplyr::pull(sid)
                                    })

  values$selected_df2 <- reactive(values$selected_df() %>%
                                    dplyr::mutate(sid = station_sid()) %>%
                                             na.omit())

  DT::datatable(values$selected_df2())

  })

  observeEvent(input$done_welcome_upload, {
    removeModal()

  })


  observeEvent(input$done_begin_upload, {
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
