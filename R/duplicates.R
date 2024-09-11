#' Handling Duplicates
#'
#' @param data Data selected in GUI.
#' @param existing_data Data with same sid in hydb.
#'
#' @return Nothing. Side-effect to database.
hydb_handling_duplicates <- function(data, existing_data) {

  # Get the new rows that are not duplicates
  new_rows <- dplyr::anti_join(data, existing_data)

  if (nrow(new_rows) > 0) {

    showModal(modalDialog(
        title = sprintf("%d new rows added to the table.", nrow(new_rows)),
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


    new_rows

  } else {

    showModal(modalDialog(
      title = 'No new rows were added, all were duplicates!',
      footer = tagList(
        modalButton('Ok', icon = NULL)),
      easyClose = TRUE,
      tags$style(
        type = 'text/css',
        '.modal-dialog {
        width: fit-content !important;}'
      )))

  }

}

