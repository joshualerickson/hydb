

#' Bulk load Excel Sheets
#'
#' @param path A character path to .xlsx file.
#' @param skip A numeric.
#'
#' @return A tibble.
#' @note Skips the first sheet. Will make more dynamic to handle
#' @importFrom dplyr "%>%"
#' @export
#'
hydb_bulk_excel <- function(path, skip = 0) {


  sheets <- readxl::excel_sheets(path)[-skip]

  dplyr::bind_rows(purrr::map(sheets, ~readxl::read_xlsx(path, sheet = .x))) %>%
    janitor::clean_names()


}
