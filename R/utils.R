#' age age plus group to biomass name
#'
#' @param spp species
#' @param biomass biomass data
#' @param spec specs data
#'
#' @export sum_tbl
#'
#'
sum_tbl <- function(spp, biomass, spec) {

  bio %>%
    dplyr::filter(stock %in% spp) %>%
    dplyr::rename(!!nameit(.) := biomass) %>%
    dplyr::select(-type, -stock) %>%
    left_join(specs2, .) %>%
    dplyr::select(Year = year, dplyr::contains("biomass"),
                  OFL=ofl, ABC=abc, TAC=tac, Catch=catch) %>%
    # dplyr::arrange(dplyr::desc(Area)) %>%
    flextable::flextable() %>%
    flextable::merge_v(j=1) %>%
    flextable::colformat_num(j=1, big.mark = "") %>%
    # flextable::hline(i=c(4,8)) %>%
    flextable::align(j=1, part="all") %>%
    flextable::valign(valign="bottom", part="header") %>%
    flextable::fix_border_issues()

}


#' age age plus group to biomass name
#'
#' @param data input dataframe
#'
#' @export nameit
#'
nameit <- function(data) {
  if(is.na(unique(data$type))) {
    "Biomass"
  } else {
    paste0("age-", unique(data$type), "+ Biomass")
  }

}

