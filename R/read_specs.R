#' read in specs dataframe
#'
#' @param path location of file
#' @param year current year
#' @param range the range of cells to include from the spreadsheet default:A5:L91
#' @param col_names specify names of all columns included
#' @param col_types specify column types of all columns included
#'
#' @export read_specs
#'
#' @examples
#' \dontrun{
#' read_specs('data/goa_specs2022.xlxs', col_names = c("species", "area", "0ofl", "0abc", "0tac", "0catch", "1ofl", "1abc", "1tac", "1catch", "2ofl", "2abc"), col_types = c("text", "text", rep("numeric", 10)))
#' }
read_specs <- function(path, year,
                       range="A5:L91",
                       col_names = c("species", "area", "0ofl", "0abc", "0tac", "0catch", "1ofl", "1abc", "1tac", "1catch",
                                                         "2ofl", "2abc"),
                       col_types = c("text", "text", rep("numeric", 10))) {

  readxl::read_xlsx(path=path, range=range, col_names = col_names, col_types=col_types) %>%
    tidyr::fill(species, .direction = 'down')

}
# read_specs("C:/Users/Ben.Williams/Downloads/goa_specs.xlsx") -> specs


