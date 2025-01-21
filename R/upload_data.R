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
          icon = icon("binoculars")),
        shinydashboard::menuItem(
          "Basin Delineation",
          tabName = "basin",
          icon = icon("binoculars")),
        shinydashboard::menuItem(
          "Hydro Application",
          tabName = "happ",
          icon = icon("binoculars"))
        )),

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
                      uiOutput("select_metric"),
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
                                            title = HTML('&#8601 Explore'),
                                            style = 'height:92vh',
                                            esquisse::esquisse_ui(
                                                                  id = "esquisse",
                                                                  container = esquisse::esquisseContainer(height = "900px"),
                                                                  header = FALSE, # dont display gadget title
                                                                ),
                                            uiOutput('select_table_graphing')
                                            )

                                            )



          )
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
          tabName = 'basin',
          fluidRow(shinydashboard::tabBox(width = 12, id = 'basin_mod',
                                          tabPanel(
                                            title = HTML('&#8601 Find a basin'),
                                            syle = 'height:92vh',
                                            gwavr::basinModUI('basin-ui')

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
          tabName = 'happ',
          fluidRow(shiny::navbarPage('hydroapps',
                                      tabPanel('USGS',hydroapps:::mod_station_ui("station_ui_1")
                                                                                         ),
                                      tabPanel('SNOTEL',hydroapps:::mod_snotel_ui('snotel_ui_1')))

          )
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

  # credentials <- shinyauthr::loginServer(
  #   id = "login",
  #   data = dplyr::tibble(user = 'usfs', password = 'waterwins'),
  #   user_col = user,
  #   pwd_col = password,
  #   sodium_hashed = F
  # )

  # observe({
  #   if(credentials()$user_auth) {
  #     shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
  #   } else {
  #     shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  #   }
  # })

  # create a backup here
  windows_path <- normalizePath(file.path(Sys.getenv("HOMEDRIVE"), Sys.getenv("HOMEPATH")), winslash = .Platform$file.sep)

  path <- file.path('/USDA/Northern Region Hydrology - Documents/data-madness/hydb/copy_hydb.sqlite')

  db_path <- paste0(windows_path,path)

  RSQLite::sqliteCopyDatabase(hydb_connect(),db_path)

  # List the first level callModules here
  values <- reactiveValues()

  output$startup <- renderUI({

    # req(credentials()$user_auth)
  shinybusy::show_modal_spinner(
    spin = "orbit",
    color = "#18BC9C",
    text = HTML("Loading hydb database from T:drive <br> Might take a few seconds...")
  )


  mydb <- hydb_connect()

  values$mydb <- mydb
  values$md <- fetch_hydb('station_metadata')

  shinybusy::remove_modal_spinner()

  })

  output$html_welcome <- renderUI({
    # req(credentials()$user_auth)
    htmltools::tags$iframe(seamless = 'seamless',
                         src = "www/hydb_welcome.html",
                         width = '100%',
                         height = 1000,
                         style = "border:none;")
  })

  output$select_table <- shiny::renderUI({

    # req(credentials()$user_auth)
                     shiny::selectInput('db_table',
                     label = 'Select a Database Table to Upload into.',
                     selected = '',
                     choices = c('', DBI::dbListTables(values$mydb)))})

  output$sid <- shiny::renderUI({

    # req(credentials()$user_auth)
    shiny::selectInput(
    'sid',
    "Station Name",
    choices = list(
      `Helena-Lewis and Clark National Forest` = sort(values$md[values$md$forest == 'Helena-Lewis and Clark National Forest', ]$station_nm),
      `Idaho Panhandle National Forest` = sort(values$md[values$md$forest == 'Idaho Panhandle National Forests', ]$station_nm),
      `Kootenai National Forest` = sort(values$md[values$md$forest == 'Kootenai National Forest', ]$station_nm),
      `Lolo National Forest` = c("", sort(values$md[values$md$forest == 'Lolo National Forest', ]$station_nm)),
      `Nez Perce-Clearwater National Forest` = sort(values$md[values$md$forest == 'Nez Perce-Clearwater National Forest', ]$station_nm)
                        ),
    selected=""
  )
    })



  output$select_table_graphing <- shiny::renderUI({
    shiny::selectInput('db_table_graphing',
      label = 'Select a Database Table.',
      selected = '',
      choices = c('', DBI::dbListTables(values$mydb)))
    })

  output$select_metric <- shiny::renderUI({
    shiny::radioButtons('db_table_metric',
      label = 'What is your metric?',
      choices = 'Celcius',selected = character(0))
    })

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
    # check file name and station selected

    if(!stringr::str_detect(tolower(values$file_path()$name), stringr::word(tolower(input$sid), 1))) {


    showModal(modalDialog(
        title = "Your station name is not within the file you have selected.",
        footer = tagList(
          modalButton('Cancel'),
          actionButton('override','Override')),
        easyClose = TRUE,
        tags$style(
          type = 'text/css',
          '.modal-dialog {
            width: fit-content !important;}'
        )))

      observeEvent(input$override, {

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
      })


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

             # title for page
             title = 'Double-click to edit table cells',

             DT::dataTableOutput('selected_df_dt'),
             # add button to finish adding column and variables
                        plotly::plotlyOutput('data_graph', width = "800px") %>%
                        shinycssloaders::withSpinner()
             )


    ))))

  }

  })




    output$selected_df_dt <-   DT::renderDataTable({

      values$station_sid <- reactive({
        fetch_hydb('station_metadata') %>%
          dplyr::filter(station_nm %in% input$sid) %>%
          dplyr::pull(sid)
      })

      quality_controlled_df <- error_catching_param_names(values$selected_df(), values$table_selection())[[2]] %>%
        dplyr::mutate(sid = values$station_sid()) %>%
        dplyr::mutate(dplyr::across(dplyr::matches('iv_|obs_|dv_') & where(is.character), ~readr::parse_number(.x))) %>%
        na.omit()



      values$selected_df_dates <- switch(sub('.*\\_', '', values$table_selection()),
                                             'dv' = quality_controlled_df %>%
                                               dplyr::mutate(date = lubridate::as_date(date)),
                                             'iv' = quality_controlled_df %>%
                                               dplyr::mutate(dt = lubridate::as_datetime(dt)),
                                             'obs' = quality_controlled_df %>%
                                               dplyr::mutate(date = lubridate::as_date(date)))
      print(input$db_table_metric == 'Celcius')

      if(input$db_table_metric == 'Celcius'){

        values$selected_df2 <- reactive(values$selected_df_dates %>% dplyr::mutate(dplyr::across(c(2), ~.x*1.8 + 32)))

      } else {

        values$selected_df2 <-  reactive(values$selected_df_dates)

        }


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

if(any(class(tryCatch(fetch_hydb(values$table_selection(),
                                 values$station_sid()), error = function(e) e)) == 'error')){

    values$selected_df_final_upload <- values$selected_df2()


    showModal(modalDialog(
      title = "First submission for this station.",
      footer = tagList(
        actionButton('continue', 'Continue', icon = NULL),
        modalButton('Cancel')),
      easyClose = TRUE,
      tags$style(
        type = 'text/css',
        '.modal-dialog {
        width: fit-content !important;}'
      )
    ))

    } else {

    values$selected_df_final_upload <- hydb_handling_duplicates(values$selected_df2(),
                                                            fetch_hydb(values$table_selection(),
                                                                       values$station_sid()))
    }


})

observeEvent(input$continue, {
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

})



observeEvent(input$done_begin_final_upload, {

    DBI::dbAppendTable(values$mydb, values$table_selection(), values$selected_df_final_upload)

    removeModal()

  })


  #callModule(graphingMod, "graphing_ui_1", values = values)

  observeEvent(input$usgs_inst, {
  crud_mod <- reactive(shiny::callModule(
    gwavr::usgsinstMod,
    'usgsiv-ui',
    values = values
  ))

  observe({crud_mod()})


  })

  observeEvent(input$basin_mod, {

    crud_mod <- reactive(shiny::callModule(
      map = NULL,
      gwavr::basinMod,
      'basin-ui',
      values = values,
      dem = NULL
    ))

    observe({crud_mod()})


  })



  callModule(hydroapps:::mod_station_server, "station_ui_1", values = values)

  callModule(hydroapps:::mod_snotel_server, 'snotel_ui_1', values = values)


  onStop(function(){
        observe(
           DBI::dbDisconnect(values$mydb)
        )
  })

  sessionEnded <- session$onSessionEnded(function() {

    stopApp()

  })




observeEvent(input$db_table_graphing, {

  validate(need(!values$db_table_graph() %in% '', "Need to select a Table"))


  values$df <- reactive(fetch_hydb(values$db_table_graph()) %>%
               dplyr::left_join(values$md, by = 'sid'))

  esquisse::esquisse_server(
    id = "esquisse",
    data_rv = values$df)
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
