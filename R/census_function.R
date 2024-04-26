
#' @title census
#' @description This function allows you to quantify species representation in your dataset.
#' @param col_name species name column in original dataset
#' @keywords species
#' @export
#' @examples
#' census <- function(col_name)

census <- function(col_name){

  species_names <- list()
  species_names <- unique(col_name)

  individuals <- vector()
  for (i in 1:length(species_names)){
    individuals[[i]] <- length(which(col_name == species_names[[i]]))
  }

  census <- data.frame(species_names, individuals)
  return(census)
}
