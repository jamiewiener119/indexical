
#' @title div_index
#' @description This function allows you explore your census data with several diversity indices (Species Richness, Simpson's Index, Shannon's Index, Berger-Parker Index) and evenness measures (Simpson's Evenness, Shannon's Evenness).
#' @param species_col species_name column from census() function output
#' @param pop_col individuals column from census() function output
#' @keywords diversity
#' @export
#' @examples
#' div_index <- function(species_col, pop_col)
#' div_ind <- div_index(c$species_names, c$individuals)

div_index <- function(species_col, pop_col){

  index_name <- c("Species Richness", "Simpson's Index", "Simpson's Evenness", "Shannon-Weiner Index", "Shannon Hmax", "Shannon's Evenness", "Berger-Parker Index")
  index_values <- vector()

  simps_ind <- vector()
  shannon_ind <- vector()
  berger_ind <- vector()

  num_spe <- length(species_col)
  tot_pop <- sum(pop_col)

  #Species Richness
  index_values[[1]] <- num_spe

  #Simpsons
  for (i in 1:length(species_col)){
    simps_ind[[i]] <- (pop_col[[i]]/tot_pop)^2
    index_values[[2]] <- 1-sum(simps_ind)
  }

  #Simpson's evenness
  index_values[[3]] <- 1/(index_values[[2]]*num_spe)

  #Shannon and Hmax
  for (i in 1:length(species_col)){
    shannon_ind[[i]] <- (pop_col[[i]]/tot_pop)*log(pop_col[[i]]/tot_pop)
    index_values[[4]] <- sum(shannon_ind)*-1
    index_values[[5]] <- log(num_spe)
  }

  #Shannon's evenness
  index_values[[6]] <- (index_values[[4]]/index_values[[5]])

  #Berger
  for (i in 1:length(species_col)){
    berger_ind[[i]] <- (pop_col[[i]]/tot_pop)
    index_values[[7]] <- max(berger_ind)
  }

  index_values <- format(index_values, scientific = FALSE)
  div_index <- data.frame(index_name, index_values)
  return(div_index)
}
