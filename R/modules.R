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
#' @return server function for Shiny module
#' @importFrom dplyr '%>%'
#' @export
#'
hydbMod <- function(input, output, session, values, file_path){
  ns <- session$ns

  observeEvent(file_path(), {

  inFile <- file_path()

  df <- readxl::read_xlsx(inFile$datapath) %>%
                   janitor::clean_names()

  output$data <- DT::renderDataTable(DT::datatable(df, extensions = 'Select', selection = list(target = "column")))

  values$selected_df <- suppressWarnings(reactive(df[,input$data_columns_selected, drop = FALSE] %>%
                                                    dplyr::rename_with(~name_params_to_update(.x))))


  })

}
