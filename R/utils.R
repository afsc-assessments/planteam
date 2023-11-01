#' age plus group to biomass name
#'
#' @param data input dataframe
#'
#' @export nameit
#'

nameit <- function(data) {
  if(is.na(mean(data$rec_age[data$year==yr], na.rm=T))) {
    "Biomass"
  } else {
    paste0("age-", mean(data$rec_age[data$year==yr], na.rm=T), "+ Biomass")
  }

}
