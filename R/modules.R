#' Shiny Module UI for DT uploading
#'
#' @description A shiny Module to.
#'
#' @param id \code{character} id for the the Shiny namespace
#' @param ... other arguments to \code{leafletOutput()}
#'
#' @importFrom shiny NS tagList reactiveValues observe
#' @importFrom dplyr filter select mutate slice_max ungroup rename
#' @return UI function for Shiny module
#' @export
#'
hydbModUI <- function(id, ...){
  ns <- shiny::NS(id)
  tagList(
    DT::dataTableOutput(ns('data')),
    DT::dataTableOutput(ns('into_db'))
  )
}

#' Shiny Module Server for DT uploading
#' @param input Shiny server function input
#' @param output Shiny server function output
#' @param session Shiny server function session
#' @param values A reactive Values list to pass
#' @param file_path A reactive Values list to pass
#' @param sheet A reactive Values list to pass
#' @return server function for Shiny module
#' @importFrom dplyr '%>%'
#' @export
#'
hydbMod <- function(input, output, session, values, file_path, sheet){
  ns <- session$ns


  observe({

    req(file_path())

    inFile <- file_path()

    file_type <- sub('.*\\.','', inFile$name)

    sheet <- if(file_type %in% c('xls', 'xlsx')){if(!sheet() %in% readxl::excel_sheets(inFile$datapath)){1}else{sheet()}}

    df <-   switch (file_type,
                    'xlsx' = readxl::read_xlsx(inFile$datapath, sheet = sheet) %>%
                        janitor::clean_names(),
                    'xls' = readxl::read_xls(inFile$datapath, sheet = sheet) %>%
                      janitor::clean_names(),
                    'csv' = readr::read_csv(inFile$datapath) %>%
                      janitor::clean_names(),
                    'tsv' = readr::read_tsv(inFile$datapath) %>%
                      janitor::clean_names(),
                    'txt' = readr::read_table(inFile$datapath) %>%
                      janitor::clean_names()
    )

    output$data <- DT::renderDataTable(DT::datatable(df, extensions = 'Select', selection = list(target = "column")))

    values$selected_df <- suppressWarnings(reactive(df[,input$data_columns_selected, drop = FALSE]))

  })

}

#' Shiny Module UI for exploring database
#'
#' @description A shiny Module to.
#'
#' @param id \code{character} id for the the Shiny namespace
#' @param ... other arguments to \code{leafletOutput()}
#'
#' @importFrom shiny NS tagList reactiveValues observe
#' @importFrom dplyr filter select mutate slice_max ungroup rename
#' @return UI function for Shiny module
#' @export
#'
graphingUI <- function(id, ...){
  ns <- shiny::NS(id)
  tagList(
    plotly::plotlyOutput(ns('plot')) %>% shinycssloaders::withSpinner(color = '#18BC9C')
  )
}

#' Shiny Module Server for exploring database
#' @param input Shiny server function input
#' @param output Shiny server function output
#' @param session Shiny server function session
#' @param values A reactive Values list to pass
#' @return server function for Shiny module
#' @importFrom dplyr '%>%'
#' @export
#'
graphingMod <- function(input, output, session, values){
  ns <- session$ns
 # validate(need(!values$sid_graph() %in% '', "Need to select a Station(s)"))
 #
 #
 #  validate(need(!values$db_table_graph() %in% '', "Need to select a Table"))
 #
 #
 #  values$station_sid_graphing <- reactive({
 #    values$md %>%
 #      dplyr::filter(station_nm %in% values$sid_graph()) %>%
 #      dplyr::pull(sid)
 #  })
 #
 #  df <- fetch_hydb(values$db_table_graph(), sid = values$station_sid_graphing())
 #
 #  df <- df %>% dplyr::left_join(values$md, by = 'sid')
 #
 #  esquisse::esquisse_server(
 #    id = "esquisse",
 #    data_rv = values$db_table_graph())



  #
  # output$plot <- plotly::renderPlotly({
  #
  #
  #
  # switch(sub('.*\\_', '', values$db_table_graph()),
  #
  #        'dv' =  print(plotly::ggplotly(df %>%
  #                                         ggplot2::ggplot(ggplot2::aes(.data$date, .data[[paste0('dv_', param_cd(values$db_table_graph()))]])) +
  #                                         ggplot2::geom_line(ggplot2::aes(color = station_nm))+
  #                                         ggplot2::theme_bw(base_size = 14) +
  #                                         ggplot2::labs(color = 'Station Name') +
  #                                         ggplot2::facet_wrap(~station_nm, scales = 'free'))),
  #
  #        'iv' = print(plotly::ggplotly(df %>%
  #                                        ggplot2::ggplot(ggplot2::aes(.data$dt, .data[[paste0('iv_', param_cd(values$db_table_graph()))]])) +
  #                                        ggplot2::geom_line(ggplot2::aes(color = station_nm))+
  #                                        ggplot2::theme_bw(base_size = 14) +
  #                                        ggplot2::labs(color = 'Station Name') +
  #                                        ggplot2::facet_wrap(~station_nm, scales = 'free'))),
  #
  #        'obs' = print(plotly::ggplotly(df %>%
  #                                         ggplot2::ggplot(ggplot2::aes(.data$date, .data[[paste0('obs_', param_cd(values$db_table_graph()))]])) +
  #                                         ggplot2::geom_point(ggplot2::aes(color = station_nm)) +
  #                                         ggplot2::theme_bw(base_size = 14)+
  #                                         ggplot2::labs(color = 'Station Name') +
  #                                         ggplot2::facet_wrap(~station_nm, scales = 'free')))
  # )
  #
  #
  # })

}
