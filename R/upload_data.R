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
#' @importFrom dplyr '%>%'
#' @noRd
app_ui_hydb <- function(request) {

  tagList(


    shinydashboard::dashboardPage(

      header = shinydashboard::dashboardHeader(title = "Hydrologic Database (hydb)"),

    # Leave this function for adding external resources
    golem_add_external_resources(),

      sidebar = shinydashboard::dashboardSidebar(

        dashboardthemes::shinyDashboardThemes(theme = "poor_mans_flatly"),

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
                         tags$style(
          "
          .control-label {

          color: #18BC9C;
          background: rgba(51, 170, 51,  0);
          }

          .progress-bar {
          background-image: linear-gradient(to right, red , yellow) !important;
          background-size: auto !important;
          }"),

          fileInput("file", "Please Select File",
          accept = c('.xlsx','.xls', '.csv', '.txt', '.tsv')),
          selectInput(inputId = "sheets", label = "Select sheet", choices = NULL, selected = NULL)

        ),
        shinydashboard::menuItem(
          "Explore hydb data",
          tabName = "explore",
          icon = icon("binoculars")),
        shinydashboard::menuItem(
          "USGS Sites",
          tabName = "usgs",
          icon = icon("binoculars")))),

      ### body

      body = shinydashboard::dashboardBody(

        # login section
        shinyauthr::loginUI(id = "login"),

        uiOutput('startup'),

        shinydashboard::tabItems(
                          shinydashboard::tabItem(
                          tabName = "get_started",
                          tags$style(type = 'text/css', '#welcome {height: calc(100vh - 80px) !important;}'),
                          uiOutput('html_welcome')
                        ),
        shinydashboard::tabItem(

          tags$head(tags$style("
                             .shiny-notification {
             margin-left: -10px !important;
             height: 40px !important;
             border-color: black;
                             }

             .modal-dialog{ width:350px}

             .modal-body{ min-height:25px}

             .selectize-dropdown {
              bottom: 100% !important;
              top: auto !important;
                                  }
             ")),
          tabName = 'upload',
          fluidRow(shinydashboard::tabBox(width = 12, id = 'tabchart',
                                  tabPanel(
                               title = HTML('&#8601 Bring in File'),
                               syle = 'height:92vh',
                      hydbModUI("hydb_ui_1"),
                      uiOutput('select_table'),
                      uiOutput("sid"),
                      actionButton('begin_upload', 'Submit Table')
          )

          ))
        ),
        shinydashboard::tabItem(

          tags$head(tags$style("
                             .shiny-notification {
             margin-left: -10px !important;
             height: 40px !important;
             border-color: black;
                             }

             .modal-dialog{ width:350px}

             .modal-body{ min-height:25px}

             .selectize-dropdown {
              bottom: 100% !important;
              top: auto !important;
                                  }
             ")),
          tabName = 'explore',
          fluidRow(shinydashboard::tabBox(width = 12, id = 'graphing',
                                          tabPanel(
                                            title = HTML('&#8601 Choose a station'),
                                            syle = 'height:92vh',
                                            graphingUI('graphing_ui_1'),
                                            uiOutput('select_table_graphing'),
                                            uiOutput('sid_graphing')

                                          )

          ))
        ),
        shinydashboard::tabItem(

          tags$head(tags$style("
                             .shiny-notification {
             margin-left: -10px !important;
             height: 40px !important;
             border-color: black;
                             }

             .modal-dialog{ width:350px}

             .modal-body{ min-height:25px}

             .selectize-dropdown {
              bottom: 100% !important;
              top: auto !important;
                                  }
             ")),
          tabName = 'usgs',
          fluidRow(shinydashboard::tabBox(width = 12, id = 'usgs_inst',
                                          tabPanel(
                                            title = HTML('&#8601 Choose a station'),
                                            syle = 'height:92vh',
                                            gwavr::usgsinstModUI('usgsiv-ui')

                                          )

          ))
        )
        ))
        )
)
}



#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom dplyr '%>%'
#' @noRd
app_server_hydb <- function( input, output, session ) {

  credentials <- shinyauthr::loginServer(
    id = "login",
    data = dplyr::tibble(user = 'usfs', password = 'waterwins'),
    user_col = user,
    pwd_col = password,
    sodium_hashed = F
  )

  observe({
    if(credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })

  # List the first level callModules here
  values <- reactiveValues()

  output$startup <- renderUI({

    req(credentials()$user_auth)
  shinybusy::show_modal_spinner(
    spin = "orbit",
    color = "#18BC9C",
    text = HTML("Loading hydb database from T:drive <br> Might take a few seconds...")
  )

  path <- 'T:/FS/NFS/Kootenai/Program/2500Watershed/GIS/SO/hydb'

  values$mydb <- DBI::dbConnect(RSQLite::SQLite(), paste0(path,"/hydb.sqlite"))

  values$md <- fetch_hydb('metadata')

  shinybusy::remove_modal_spinner()

  })

  output$html_welcome <- renderUI({
    req(credentials()$user_auth)
    htmltools::tags$iframe(seamless = 'seamless',
                         src = "www/hydb_welcome.html",
                         width = '100%',
                         height = 1000,
                         style = "border:none;")
  })

  output$select_table <- shiny::renderUI({

    req(credentials()$user_auth)
                     shiny::selectInput('db_table',
                     label = 'Select a Database Table to Upload into.',
                     selected = '',
                     choices = c('', DBI::dbListTables(values$mydb)))})

  output$sid <- shiny::renderUI({

    req(credentials()$user_auth)
    shiny::selectInput(
    'sid',
    "Station ID",
    choices = c('',fetch_hydb('metadata')$station_nm),
    selected=""
  )
    })



  output$select_table_graphing <- shiny::renderUI({
    shiny::selectInput('db_table_graphing',
      label = 'Select a Database Table.',
      selected = '',
      choices = c('', DBI::dbListTables(values$mydb)))
    })


  output$sid_graphing <- shiny::renderUI(shiny::selectInput(
    'sid_graphing',
    label = "Station ID",
    choices = c('',fetch_hydb('metadata')$station_nm),
    selected="",
    multiple = TRUE
  ))

  values$sid_graph <- reactive(input$sid_graphing)

  values$db_table_graph <- reactive(input$db_table_graphing)

  observe({

  if(sub('.*\\.','', values$file_path()$name) %in% c('xls', 'xlsx')) {

    sheet_names <- readxl::excel_sheets(values$file_path()$datapath)

  } else {

    sheet_names <- "Doesn't have sheets"
  }

    shiny::updateSelectInput(
      inputId = "sheets",
      choices = sheet_names,
      selected = sheet_names[[1]] # Choose first sheet as default
    )

  }) %>%

  bindEvent(values$file_path())

  values$sheet <- reactive(input$sheets)

  values$file_path <- reactive(input$file)

  callModule(hydbMod, "hydb_ui_1", values = values, file_path = values$file_path, sheet = values$sheet)

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
    fluidPage(
      fluidRow(
      column(width = 12,
                        DT::dataTableOutput('selected_df_dt'),
                        plotly::plotlyOutput('data_graph', width = "800px") %>%
                        shinycssloaders::withSpinner())
    ))))
}
  })




    output$selected_df_dt <- DT::renderDataTable({

    values$station_sid <- reactive({
                      fetch_hydb('metadata') %>%
                      dplyr::filter(station_nm %in% input$sid) %>%
                      dplyr::pull(sid)
                                      })

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

        suppressWarnings(DT::datatable(values$selected_df2()))


    })

    output$data_graph <- plotly::renderPlotly({

      switch(sub('.*\\_', '', values$table_selection()),

             'dv' =  print(plotly::ggplotly(values$selected_df2() %>%
                     ggplot2::ggplot(ggplot2::aes(.data$date, .data[[paste0('dv_', param_cd(values$table_selection()))]])) +
                     ggplot2::geom_line()+
                     ggplot2::theme_bw(base_size = 14))),

             'iv' = print(plotly::ggplotly(values$selected_df2() %>%
                     ggplot2::ggplot(ggplot2::aes(.data$dt, .data[[paste0('iv_', param_cd(values$table_selection()))]])) +
                     ggplot2::geom_line()+
                     ggplot2::theme_bw(base_size = 14))),

             'obs' = print(plotly::ggplotly(values$selected_df2() %>%
                     ggplot2::ggplot(ggplot2::aes(.data$date, .data[[paste0('obs_', param_cd(values$table_selection()))]])) +
                     ggplot2::geom_point() +
                     ggplot2::theme_bw(base_size = 14)))
      )

    })

    observeEvent(input$done_welcome_upload, {
      removeModal()

    })


    observeEvent(input$done_begin_upload,{

    shinybusy::show_modal_spinner(
      spin = "orbit",
      color = "#18BC9C",
      text = "Please wait..."
    )
    values$identical <- switch(sub('.*\\_', '', values$table_selection()),
                               'dv' = all.equal(fetch_hydb(values$table_selection(), values$station_sid()) %>%
                                                  dplyr::filter(date %in% values$selected_df2()$date),values$selected_df2(), check.attributes = F),
                               'iv' = all.equal(fetch_hydb(values$table_selection(), values$station_sid()) %>%
                                                  dplyr::filter(dt %in% values$selected_df2()$dt),values$selected_df2(), check.attributes = F),
                               'obs' = all.equal(fetch_hydb(values$table_selection(), values$station_sid()) %>%
                                                   dplyr::filter(date %in% values$selected_df2()$date,
                                                                 time %in% values$selected_df2()$time),values$selected_df2(), check.attributes = F))


    if(length(values$identical) == 1){

      shinyWidgets::show_alert("Looks like you've added this dataset before...",
                               'please reselect and try again',
                               type = 'warning'
      )

    shinybusy::remove_modal_spinner()

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



    DBI::dbAppendTable(values$mydb, values$table_selection(), values$selected_df2())


    removeModal()

  })


  callModule(graphingMod, "graphing_ui_1", values = values)

  observeEvent(input$usgs_inst, {
  crud_mod <- reactive(shiny::callModule(
    gwavr::usgsinstMod,
    'usgsiv-ui',
    values = values
  ))

  observe({crud_mod()})
  })



  onStop(function(){
        observe(
           DBI::dbDisconnect(values$mydb)
        )
  })

  sessionEnded <- session$onSessionEnded(function() {

    stopApp()

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
    )
  )
}
