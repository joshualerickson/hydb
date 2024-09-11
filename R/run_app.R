#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
    ...
) {

  with_golem_options(
    app = shinyApp(
      ui = app_ui_hydb,
      server = app_server_hydb,
      options = list(launch.browser = TRUE)
    ),
    golem_opts = list(...)
  )


}

