
#' Clean Spatial Duplicates
#'
#' @description
#' Get cleaned duplicates.
#'
#' @param point An sf POINT object
#' @importFrom dplyr "%>%"
#'
#' @return An sf object
hydb_clean_duplicate_metadata <- function(point) {

  eq_list <- sf::st_equals(point)

  group_mapping <- sapply(seq_along(eq_list), function(i) {min(unlist(eq_list[[i]]))})

  point <- point %>%
    dplyr::mutate(group_id = group_mapping) %>%
    dplyr::group_by(group_id) %>%
    dplyr::summarise(dplyr::across(dplyr::where(is.character), ~paste(unique(.), collapse = ", "), .names = "{col}"),
                     dplyr::across(dplyr::where(is.numeric), first),
                     geometry = sf::st_union(geometry)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-group_id)
}





