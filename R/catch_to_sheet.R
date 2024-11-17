
#' Send AKRO catch data to the plan team google sheet
#'
#' @param year current year
#' @param data catch data file
#' @param area 'goa' or 'bsai' - currently this only works for 'goa'
#'
#' @export catch_to_sheet
#'
#' @examples
#' /dontrun{
#' catch_to_sheet(year=2024, data=goa_nov_catch, area='goa')
#' }
catch_to_sheet <- function(year, data, area){
  googlesheets4::gs4_auth()
  # globals
  yr = year
  area = tolower(area)
  if(area!='goa'){stop('only works for goa right now')}

  goa_species = data.frame(stock = c("pollock","pcod","sablefish","shallow_flat",
                                     "deep_flat","rex","arrowtooth","flathead",
                                     "pop","nork","shortraker","dusky",
                                     "rebs","dsr","thornyhead","orox","atka","big_skate",
                                     "longnose","skates","sharks","octopus"),
                           id = c("Pollock", "Pacific cod", "Sablefish", "Shallow-water flatfish",
                                  "Deep-water flatfish", "Rex sole", "Arrowtooth flounder",
                                  "Flathead sole", "Pacific ocean perch","Northern rockfish",
                                  "Shortraker rockfish","Dusky rockfish","Rougheye and Blackspotted rockfish",
                                  "Demersal shelf rockfish", "Thornyhead rockfish",
                                  "Other rockfish", "Atka mackerel","Big skate","Longnose skate",
                                  "Other skate","Sharks","Octopuses"))

  bsai_species = data.frame(stock = c("pollock","pcod","sablefish","yellowfin",
                                      "greenland","arrowtooth","kamchatka","northern_rs","flathead",
                                      "plaice","flatfish",
                                      "pop","nork","bsre","shortraker","orox","atka","skates",
                                      "sharks","octopus"),
                            id = c("Pollock", "Pacific cod", "Sablefish", "Yellowfin sole",
                                   "Greenland turbot", "Arrowtooth flounder","Kamchatka flounder",
                                   "Northern rock sole", "Alaska plaice",
                                   "Flathead sole", "Other flatfish","Pacific ocean perch","Northern rockfish",
                                   "Blackspotted/Rougheye rockfish",
                                   "Shortraker rockfish",
                                   "Other rockfish", "Atka mackerel","Skates","Sharks","Octopuses"))

  goa_levels = c("PWS GHL", "W", "W (610)", "C", "C (620)", "C (630)", "W/C", "E", "WYAK",
                 "W/C/WYAK", "Subtotal", "EYAK/SEO", "SEO", "GOA Total", "GOA-wide", "AK Total", "Total")

  bsai_levels = c("BSAI", "EBS", "BS", "AI", "Bogoslof", "BSAI/GOA", "EAI", "EAI/BS", "CAI", "WAI", "EBS/EAI", "CAI/WAI")

  # data ----
  data <- vroom::vroom(here::here("dev", 2024, "catch_table_nov_pt.csv"))
  if(area=='goa'){
  df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1uHmCuY3GXfSBCbsP61nAQeATBfoslXeS3PQNLlioWIk/edit#gid=1812144961",
                                  sheet = "goa",
                                  col_types = "cccddddddccc")
  } else {
    df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1uHmCuY3GXfSBCbsP61nAQeATBfoslXeS3PQNLlioWIk/edit#gid=1812144961",
                                    sheet = "bsai",
                                    col_types = "cccddddddcc")

  }

  # rename stocks and areas
  data %>%
    dplyr::rename_with(tolower) %>%
    dplyr::mutate(spp = tolower(species),
                  stock = dplyr::case_when(grepl("pollo", spp) ~ "pollock",
                                           grepl("pcod", spp) ~ "pcod",
                                           grepl("sable", spp) ~ "sablefish",
                                           grepl("shallow", spp) ~ "shallow_flat",
                                           grepl("deep", spp) ~ "deep_flat",
                                           grepl("rex", spp) ~ "rex",
                                           grepl("arr", spp) ~ "arrowtooth",
                                           grepl("flathead", spp) ~ "flathead",
                                           grepl("pop", spp) ~ "pop",
                                           grepl("north", spp) ~ "nork",
                                           grepl("shortr", spp) ~ "shortraker",
                                           grepl("rough", spp) ~ "rebs",
                                           grepl("dusk", spp) ~ "dusky",
                                           grepl("demer", spp) ~ "dsr",
                                           grepl("thorn", spp) ~ "thornyhead",
                                           grepl("other r", spp) ~ "orox",
                                           grepl("skates", spp) ~ "skates",
                                           grepl("other", spp) ~ "orox",
                                           grepl("atka", spp) ~ "atka",
                                           grepl("big", spp) ~ "big_skate",
                                           grepl("longn", spp) ~ "longnose",
                                           grepl("shark", spp) ~ "sharks",
                                           grepl("octo", spp) ~ "octopus"),
                  year = yr,
                  area = ifelse(area=="GOA Total", "GOA-wide", area)) %>%
    dplyr::select(stock, year, area, catch) -> dat

    # calculate the total catches for most stocks
  dat %>%
    tidyr::drop_na(stock)
    dplyr::mutate(area = ifelse(area=='GOA-wide' & stock=="orox", 'W/C', area)) %>%
    dplyr::filter(!(stock %in% c("pollock", "sablefish", "dsr")), !(grepl("GOA-wide", area))) %>%
    dplyr::group_split(year, stock) %>%
    purrr::map(~ tibble::add_row(.x,
                                 area = "Total",
                                 stock = tail(.x$stock, 1),
                                 year = yr,
                                 catch = sum(.x$catch, na.rm = T))) -> tots

  # sablefish catches
  dat %>%
    dplyr::filter(stock == "sablefish") %>%
    dplyr::group_by(year, stock, area) %>%
    dplyr::summarise(catch = sum(catch), .groups="drop") %>%
    dplyr::group_split(year, stock) %>%
    purrr::map(~ tibble::add_row(.x,
                                 area = "AK Total",
                                 stock = tail(.x$stock, 1),
                                 year = yr,
                                 catch = sum(.x$catch, na.rm = T))) %>%
    purrr::map(~ tibble::add_row(.x,
                                 area = "GOA Total",
                                 stock = tail(.x$stock, 1),
                                 year = yr,
                                 catch = sum(.x$catch[!(.x$area %in% c('AI','BS', 'AK Total'))], na.rm = T))) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(!(area %in% c('AI', 'BS'))) -> sable

  # pollock catches
  dat %>%
    dplyr::filter(stock=="pollock") %>%
    dplyr::group_split(year, stock) %>%
    purrr::map(~ tibble::add_row(.x,
                                 area = "Total",
                                 stock = tail(.x$stock, 1),
                                 year = yr,
                                 catch = sum(.x$catch[.x$area!="PWS GHL"], na.rm = T))) %>%
    purrr::map(~ tibble::add_row(.x,
                                 area = "Subtotal",
                                 stock = tail(.x$stock, 1),
                                 year = yr,
                                 catch = sum(.x$catch[!(.x$area%in% c("EYAK/SEO","PWS GHL", "Total"))], na.rm = T))) -> poll

  dat %>%
    dplyr::filter(stock == "pop") %>%
    dplyr::group_split(year, stock) %>%
    purrr::map(~ tibble::add_row(.x,
                                 area = "W/C/WYAK",
                                 stock = tail(.x$stock, 1),
                                 year = yr,
                                 catch = sum(.x$catch[.x$area%in%c('W', 'C', "WYAK")], na.rm = T))) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(area=="W/C/WYAK") -> pop

  data.frame(stock="nork", year=yr, area='E') -> nork

  #  dsr catches, then rejoin with other catches
  df %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::filter(!(year %in% yr)) %>%
    dplyr::bind_rows(dat %>%
                dplyr::filter(stock %in% c("dsr") | grepl("GOA-wide", area), stock!='orox') %>%
                dplyr::mutate(area = ifelse(area!= "GOA-wide", "Total", area)) %>%
                dplyr::group_by(stock, year, area) %>%
                dplyr::summarise(catch = sum(catch), .groups="drop") %>%
                dplyr::bind_rows(tots, sable, poll, pop, nork)) %>%
    dplyr::mutate(stock = factor(stock, levels = goa_species$stock),
                  area = factor(area, levels = goa_levels)) %>%
    dplyr::arrange(stock, year, area) %>%
    dplyr::select(catch) -> ct


  googlesheets4::sheet_write(ct, ss="https://docs.google.com/spreadsheets/d/1uHmCuY3GXfSBCbsP61nAQeATBfoslXeS3PQNLlioWIk/edit#gid=1812144961",
                             sheet = "goa")
}


