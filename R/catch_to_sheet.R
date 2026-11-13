
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
catch_to_sheet <- function(year, data, area, include_next_year = TRUE) {
  googlesheets4::gs4_auth()
  
  target_area <- tolower(area)
  if (!(target_area %in% c("goa", "bsai"))) {
    stop("area must be 'goa' or 'bsai'")
  }

  # Standardize input year column 
  data <- data %>%
    dplyr::rename_with(tolower) %>%
    dplyr::mutate(year = as.numeric(as.character(year)))

  yr <- as.numeric(year)
  if (include_next_year) {
    data_years <- sort(unique(data$year))
    yr <- sort(unique(c(yr, data_years[data_years > max(yr)])))
  }

  goa_species <- data.frame(
    stock = c("pollock", "pcod", "sablefish", "shallow_flat",
              "deep_flat", "rex", "arrowtooth", "flathead",
              "pop", "nork", "shortraker", "dusky",
              "rebs", "dsr", "thornyhead", "orox", "atka", "big_skate",
              "longnose", "skates", "sharks", "octopus"),
    id = c("Pollock", "Pacific cod", "Sablefish", "Shallow-water flatfish",
           "Deep-water flatfish", "Rex sole", "Arrowtooth flounder",
           "Flathead sole", "Pacific ocean perch", "Northern rockfish",
           "Shortraker rockfish", "Dusky rockfish", "Rougheye and Blackspotted rockfish",
           "Demersal shelf rockfish", "Thornyhead rockfish",
           "Other rockfish", "Atka mackerel", "Big skate", "Longnose skate",
           "Other skate", "Sharks", "Octopuses")
  )

  bsai_species <- data.frame(
    stock = c("pollock", "pcod", "sablefish", "yellowfin",
              "greenland", "arrowtooth", "kamchatka", "northern_rs", "flathead",
              "plaice", "flatfish",
              "pop", "nork", "bsre", "shortraker", "orox", "atka", "skates",
              "sharks", "octopus"),
    id = c("Pollock", "Pacific cod", "Sablefish", "Yellowfin sole",
           "Greenland turbot", "Arrowtooth flounder", "Kamchatka flounder",
           "Northern rock sole", "Alaska plaice",
           "Flathead sole", "Other flatfish", "Pacific ocean perch", "Northern rockfish",
           "Blackspotted/Rougheye rockfish",
           "Shortraker rockfish",
           "Other rockfish", "Atka mackerel", "Skates", "Sharks", "Octopuses")
  )

  goa_levels <- c("PWS GHL", "W", "W (610)", "C", "C (620)", "C (630)", "W/C", "E", "WYAK",
                  "W/C/WYAK", "Subtotal", "EYAK/SEO", "SEO", "GOA Total", "GOA-wide", "AK Total", "Total")

  bsai_levels <- c("BSAI", "EBS", "BS", "AI", "Bogoslof", "BSAI/GOA", "EAI", "EAI/BS", "CAI", "WAI", "EBS/EAI", "CAI/WAI")

  # area-specific variables
  if (target_area == "goa") {
    target_species <- goa_species$stock
    target_levels <- goa_levels
    wide_stocks <- c("atka", "octopus", "sharks", "skates")
    wide_label <- "GOA-wide"
    total_label <- "Total"
  } else {
    target_species <- bsai_species$stock
    target_levels <- bsai_levels
    wide_stocks <- c("sharks", "skates", "octopus", "sculpins")
    wide_label <- "BSAI"
    total_label <- "BSAI"
  }

  # Return NA (rather than 0) when all catches in a group are NA (e.g., 2027)
  sum_na <- function(x) {
    if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
  }

  # Read and clean existing Google Sheet 
  ss_url <- "https://docs.google.com/spreadsheets/d/1uHmCuY3GXfSBCbsP61nAQeATBfoslXeS3PQNLlioWIk/edit#gid=1812144961"
  df <- googlesheets4::read_sheet(ss_url, sheet = target_area, col_types = "cccddddddccc")

  num_cols <- intersect(c("year", "biomass", "rec_age", "ofl", "abc", "tac", "catch"), names(df))
  df <- df %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(num_cols),
      ~ suppressWarnings(as.numeric(gsub(",", "", trimws(as.character(.x)))))
    ))

  # Standardize input data 
  dat <- data %>%
    dplyr::filter(year %in% yr) %>%
    dplyr::mutate(
      spp = tolower(species),
      stock = dplyr::case_when(
        grepl("pollo", spp) ~ "pollock",
        grepl("pcod", spp) ~ "pcod",
        grepl("sable", spp) ~ "sablefish",
        grepl("yellow", spp) ~ "yellowfin",
        grepl("green", spp) | grepl("turbot", spp) ~ "greenland",
        grepl("arr", spp) ~ "arrowtooth",
        grepl("kamch", spp) ~ "kamchatka",
        grepl("rock sole", spp) | grepl("northern_rs", spp) ~ "northern_rs",
        grepl("plaice", spp) ~ "plaice",
        grepl("flathead", spp) ~ "flathead",
        grepl("shallow", spp) ~ "shallow_flat",
        grepl("deep", spp) ~ "deep_flat",
        grepl("rex", spp) ~ "rex",
        grepl("other flat", spp) | (grepl("^flatfish", spp) & !grepl("shallow|deep", spp)) ~ "flatfish",
        grepl("pop", spp) | grepl("pacific ocean perch", spp) ~ "pop",
        grepl("north", spp) & !grepl("sole|rs|rock sole", spp) ~ "nork",
        grepl("shortr", spp) ~ "shortraker",
        grepl("rough", spp) | grepl("bsre", spp) | grepl("blackspot", spp) ~ ifelse(target_area == "goa", "rebs", "bsre"),
        grepl("dusk", spp) ~ "dusky",
        grepl("demer", spp) ~ "dsr",
        grepl("thorn", spp) ~ "thornyhead",
        grepl("other r", spp) ~ "orox",
        grepl("atka", spp) ~ "atka",
        grepl("big", spp) ~ "big_skate",
        grepl("longn", spp) ~ "longnose",
        grepl("skate", spp) ~ "skates",
        grepl("shark", spp) ~ "sharks",
        grepl("octo", spp) ~ "octopus",
        TRUE ~ spp
      ),
      area = dplyr::case_when(
        target_area == "goa" & area %in% c("GOA Total", "GOA", "Total") & stock %in% wide_stocks ~ "GOA-wide",
        target_area == "goa" & area %in% c("GOA Total", "GOA") ~ "GOA-wide",
        target_area == "bsai" & area %in% c("BSAI Total", "Total") ~ "BSAI",
        stock == "orox" & area == "GOA-wide" ~ "W/C/WYAK",
        TRUE ~ area
      )
    ) %>%
    tidyr::drop_na(stock)

  specs <- dat %>%
    dplyr::select(stock, year, area, ofl, abc, tac) %>%
    dplyr::mutate(dplyr::across(c(ofl, abc, tac), as.numeric)) %>%
    dplyr::distinct(stock, year, area, .keep_all = TRUE)

  dat_catch <- dat %>%
    dplyr::select(stock, year, area, catch) %>%
    dplyr::mutate(catch = as.numeric(catch))

  # Calculate catches by stock group 
  
  # Standard multi-area stocks
  tots <- dat_catch %>%
    dplyr::filter(!(stock %in% c("pollock", "sablefish", "dsr", wide_stocks)),
                  !(area %in% c("Total", "Subtotal", "GOA", "GOA-wide", "AK Total", "W/C/WYAK", "BSAI"))) %>%
    dplyr::group_by(year, stock) %>%
    dplyr::group_modify(~ tibble::add_row(.x,
                                          area = total_label,
                                          catch = sum_na(.x$catch))) %>%
    dplyr::ungroup()

  # Region-wide only stocks (atka, octopus, sharks, skates in GOA; skates, sharks, octopus in BSAI)
  wide <- dat_catch %>%
    dplyr::filter(stock %in% wide_stocks) %>%
    dplyr::group_by(year, stock) %>%
    dplyr::summarise(
      catch = if (any(area == wide_label)) {
        sum_na(catch[area == wide_label])
      } else {
        sum_na(catch)
      },
      .groups = "drop"
    ) %>%
    dplyr::mutate(area = wide_label)

  # Region-specific logic for sablefish, pollock, and dsr
  if (target_area == "goa") {
    
    sable <- dat_catch %>%
      dplyr::filter(stock == "sablefish",
                    !(area %in% c("Total", "Subtotal", "GOA", "GOA-wide", "GOA Total", "AK Total"))) %>%
      dplyr::group_by(year, stock, area) %>%
      dplyr::summarise(catch = sum_na(catch), .groups = "drop") %>%
      dplyr::group_by(year, stock) %>%
      dplyr::group_modify(~ {
        ak_tot <- sum_na(.x$catch)
        goa_tot <- sum_na(.x$catch[!(.x$area %in% c("AI", "BS"))])
        .x %>%
          tibble::add_row(area = "GOA-wide", catch = goa_tot) %>%
          tibble::add_row(area = "AK Total", catch = ak_tot)
      }) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!(area %in% c("AI", "BS")))

    poll <- dat_catch %>%
      dplyr::filter(stock == "pollock",
                    !(area %in% c("Total", "Subtotal"))) %>%
      dplyr::group_by(year, stock) %>%
      dplyr::group_modify(~ {
        tot <- sum_na(.x$catch[.x$area != "PWS GHL"])
        subtot <- sum_na(.x$catch[!(.x$area %in% c("SEO", "EYAK/SEO", "PWS GHL"))])
        .x %>%
          tibble::add_row(area = "Subtotal", catch = subtot) %>%
          tibble::add_row(area = "Total", catch = tot)
      }) %>%
      dplyr::ungroup()

    dsr <- dat_catch %>%
      dplyr::filter(stock == "dsr", area != "Total") %>%
      dplyr::group_by(stock, year, area) %>%
      dplyr::summarise(catch = sum_na(catch), .groups = "drop")
      
  } else {
    # BSAI logic: simple aggregations 
    sable <- dat_catch %>%
      dplyr::filter(stock == "sablefish", !(area %in% c("BSAI", "Total", "AK Total"))) %>%
      dplyr::group_by(year, stock) %>%
      dplyr::group_modify(~ tibble::add_row(.x, area = "BSAI", catch = sum_na(.x$catch))) %>%
      dplyr::ungroup()
      
    poll <- dat_catch %>%
      dplyr::filter(stock == "pollock", !(area %in% c("BSAI", "Total"))) %>%
      dplyr::group_by(year, stock) %>%
      dplyr::group_modify(~ tibble::add_row(.x, area = "BSAI", catch = sum_na(.x$catch))) %>%
      dplyr::ungroup()
      
    # DSR does not exist in BSAI
    dsr <- tibble::tibble(stock = character(), year = numeric(), area = character(), catch = numeric())
  }

  # Combine catches + specs and merge into existing sheet
  new_data <- dplyr::bind_rows(tots, wide, sable, poll, dsr) %>%
    dplyr::left_join(specs, by = c("stock", "year", "area")) %>%
    dplyr::rename(species = stock) %>%
    dplyr::mutate(dplyr::across(c(year, ofl, abc, tac, catch), as.numeric))

  # Ensure every target year has the full (species, area) layout of the template
  base_year <- max(df$year[df$year < min(yr)], na.rm = TRUE)
  if (is.finite(base_year)) {
    template <- df %>%
      dplyr::filter(year == base_year, !is.na(species)) %>%
      dplyr::distinct(species, area)

    scaffold <- tidyr::crossing(year = yr, template)
    df <- dplyr::full_join(df, scaffold, by = c("year", "species", "area"))
  }

  ct <- df %>%
    dplyr::full_join(new_data, by = c("year", "species", "area")) %>%
    dplyr::mutate(
      ofl = dplyr::coalesce(ofl.y, ofl.x),
      abc = dplyr::coalesce(abc.y, abc.x),
      tac = dplyr::coalesce(tac.y, tac.x),
      catch = dplyr::coalesce(catch.y, catch.x),
      species = factor(species, levels = target_species),
      area = factor(area, levels = target_levels)
    ) %>%
    dplyr::select(-dplyr::ends_with(".x"), -dplyr::ends_with(".y")) %>%
    dplyr::arrange(species, year, area) %>%
    dplyr::mutate(
      species = as.character(species),
      area = as.character(area)
    )

  # Write complete dataframe back to Google Sheets 
  googlesheets4::sheet_write(ct, ss = ss_url, sheet = target_area)
}