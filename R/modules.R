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
#' @param sheet A reactive Values list to pass
#' @return server function for Shiny module
#' @importFrom dplyr '%>%'
#' @export
#'
hydbMod <- function(input, output, session, values, file_path, sheet){
  ns <- session$ns


  observe({

    req(nchar(sheet())>1)

    inFile <- file_path()

    df <-   switch (sub('.*\\.','', inFile$name),
                    'xlsx' = readxl::read_xlsx(inFile$datapath, sheet = sheet()) %>%
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
