#' Catch report generator
#'
#' @param title what do you want to call it?
#' @param authors who are the authors
#' @param stock a lowercase shorthand that matches with the data file https://docs.google.com/spreadsheets/d/1uHmCuY3GXfSBCbsP61nAQeATBfoslXeS3PQNLlioWIk/edit#gid=0
#' @param area lowercase typically "GOA" or "BSAI", though other "BSAI" areas can be called (e.g., Bogoslof) - must match the google sheet names
#' @param year current year
#' @param last_full_year last year that a full assessment was done
#' @param next_full_year next year that a full assessment is scheduled to occur
#' @param catch_data_date date that catch data are through, e.g., "November 5, 2023"
#' @param output_dir default output is the project, if you have a year folder the something like "2023" to id which folder to place the results in
#'
#' @export
#'
#' @examples
#' /dontrun{
#' catch_report(title = "1.B. Assessment of walleye pollock in the Bogoslof Island Region",
#'              authors = "James N. Ianelli, S. J. Barbeaux, Ivonne Ortiz, and D. McKelvey",
#'              stock = "pollock",
#'              area = "Bogoslof",
#'              year = 2023,
#'              last_full_year = 2021,
#'              next_full_year = 2024,
#'              catch_data_date = "November 5, 2023",
#'              output_dir = "2023")
#' }
catch_report <- function(title, authors, stock, area, year, last_full_year, next_full_year, catch_data_date, output_dir=NULL) {

  if(is.null(output_dir)) {
    output = here::here()
  } else {
    output = here::here(output_dir)
  }
  stock = tolower(stock)
  if(!(stock %in% c("pollock", "pcod", "sablefish", "shallow_flat", "deep_flat",
                    "rex", "arrowtooth", "flathead", "pop", "nork", "shortraker",
                    "dusky", "rebs", "bsre", "dsr", "thornyhead", "orox", "atka", "big_skate",
                    "longnose", "skates", "sharks", "octopus", "yellowfin", "greenland",
                    "kamchatka", "ak_plaice", "flatfish"))) {
    stop("stock must be one of the following: pollock, pcod, sablefish, shallow_flat, deep_flat,
                    rex, arrowtooth, flathead, pop, nork, shortraker,
                    dusky, rebs, bsre, dsr, thornyhead, orox, atka, big_skate,
                    longnose, skates, sharks, octopus, yellowfin, greenland,
                    kamchatka, ak_plaice, flatfish")
  }

  rmarkdown::render(
    input = system.file("catch_report.Rmd", package = 'planteam'),
    params = list(doc_title = title,
                  authors = authors,
                  stock = stock,
                  area = area,
                  year = year,
                  last_full_year = last_full_year,
                  next_full_year = next_full_year,
                  catch_data_date = catch_data_date),
    output_file = paste(toupper(area), stock, ".docx", sep = "_"),
    output_dir = output_dir,
    clean = TRUE)
}






