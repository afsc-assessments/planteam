#' Create an Excel specs table
#'
#' @param year current year
#' @param month sep or nov specs
#' @param area goa or bsai
#' @param catch_date date that catch dat is pulled e.g., ("9/10/2023")
#'
#' @export spec_table
#'
#' @examples
#' /dontrun{
#' spec_table(year, month = "sep", area = "goa", catch_date="11/9/2023")
#' }
#'
spec_table <- function(year, month = "sep", area = "goa", catch_date="11/9/2023") {

  if(!(area %in% c("goa", "bsai"))){
    stop("area must be 'goa' or 'bsai'")
  }

  if(!(month%in%c("sep", "nov"))){
    stop("month must be 'sep' or 'nov'")
  }

  # globals
  yr = year
  area = tolower(area)

  sep_names = c("stock","area",paste0("ofl_",yr-1), paste0("abc_",yr-1), paste0("tac_",yr-1), paste0("catch_",yr-1),
                paste0("ofl_",yr), paste0("abc_",yr), paste0("tac_",yr), paste0("catch_",yr),
                paste0("ofl_",yr+1), paste0("abc_",yr+1),paste0("tac_",yr+1))

  nov_names = c("stock","area",paste0("ofl_",yr), paste0("abc_",yr), paste0("tac_",yr), paste0("catch_",yr),
                paste0("ofl_",yr+1), paste0("abc_",yr+1),
                paste0("ofl_",yr+2), paste0("abc_",yr+2))

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


  # data
  if(area=="goa"){
    df = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1uHmCuY3GXfSBCbsP61nAQeATBfoslXeS3PQNLlioWIk/edit#gid=0",
                                   sheet = "goa",
                                   col_types = "cccddddddcc")  %>%
      dplyr::select(-biomass, -rec_age, -completed, -checked)
  } else {
    df = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1uHmCuY3GXfSBCbsP61nAQeATBfoslXeS3PQNLlioWIk/edit?pli=1#gid=276255793",
                                   sheet = "bsai",
                                   col_types = "cccddddddcc")  %>%
      dplyr::select(-biomass, -rec_age, -completed, -checked)
  }



  if(area=="goa" & month=="sep"){
 # FLAG: need to fix the sums
    df %>%
      dplyr::filter(year %in% (yr-1):(yr+1)) %>%
      tidyr::pivot_wider(names_from = year, values_from = c(-stock, -area, -year)) %>%
      dplyr::select(-paste0("catch_", yr+1)) %>%
      dplyr::relocate(stock, area, paste0("ofl_",year-1), paste0("abc_",year-1),
                      paste0("tac_",year-1), paste0("catch_",year-1), paste0("ofl_",year),
                      paste0("abc_",year), paste0("tac_",year), paste0("catch_",year),
                      paste0("ofl_",year+1), paste0("abc_",year+1), paste0("tac_",year+1)) %>%
      dplyr::left_join(goa_species) %>%
      dplyr::mutate(stock = id,
                    stock = ifelse(stock=="sablefish" & area=="AK-wide",
                                   "Alaska-wide OFL and ABC", stock)) %>%
      dplyr::select(-id)  %>%
      dplyr::mutate(stock = factor(stock, levels = goa_species$id)) -> dat

    dat %>%
      dplyr::bind_rows(
        dat %>%
          dplyr::ungroup() %>%
          dplyr::filter(area %in% c("Total", "AK Total", "GOA-wide")) %>%
          dplyr::summarise(dplyr::across(where(is.numeric),~sum(., na.rm=TRUE))) %>%
          dplyr::mutate(stock="Total", "area"="Total")) -> data

    data %>%
      flextable::flextable() %>%
      flextable::font(fontname = "Times New Roman", part = "all") %>%
      flextable::fontsize(size = 11, part = "all") %>%
      flextable::add_header_row(values=c("","",rep(year-1,3),"Catch as of", rep(year,3), "Catch as of", rep(paste0("Plan Team Proposed ", year+1,"/", year+2),3))) %>%
      flextable::compose(i=2, j=1:13,
                         value = flextable::as_paragraph(c("Species", "Area", "OFL", "ABC", "TAC", paste0("12/31/", year-1),
                                                           "OFL", "ABL", "TAC", catch_date, "OFL", "ABC", "TAC")),
                         part="header") -> tbl


    tbl %>%
      flextable::theme_zebra(even_body = "#ADD8E6", odd_body = "transparent",
                             even_header = "#ADD8E6", odd_header = "#ADD8E6") %>%
      flextable::bg(j=1,bg = "transparent") %>%
      flextable::align(i=1, align = "center", part = "header") %>%
      flextable::align(j=2, i=c(6,8,12,18,23,28,33,38,43,49,53,57,62,66,67,71,75,76,80,84), align = "right", part = "body") %>%
      flextable::hline_top(part = "header") %>%
      flextable::hline_top() %>%
      flextable::merge_h(i=1, part = "header") %>%
      flextable::merge_v(j= ~stock) %>%
      flextable::width(j=1, width=2.0) %>%
      flextable::width(j=c(2,6,10), width=1.25) %>%
      flextable::hline(i = rle(cumsum(.$body$spans$columns[,1]))$values) %>%
      flextable::merge_at(i=88, j=1:2) %>%
      flextable::align(i=88,j=1, align="center") %>%
      flextable::vline(j=c(2,6,8,10)) %>%
      flextable::bold(j=1) %>%
      flextable::bold(i=88,j=3:13) %>%
      flextable::colformat_double(j=c(6,10), digits=0) %>%
      flextable::fix_border_issues() -> ft

  } else if(area=='goa' & month=='nov'){
    df %>%
      filter
      dplyr::filter(year %in% (yr):(yr+2)) %>%
      tidyr::pivot_wider(names_from = year, values_from = c(-stock, -area, -year)) %>%
      dplyr::select(dplyr::any_of(nov_names)) %>%
      dplyr::left_join(bsai_species) %>%
      dplyr::mutate(stock = id,
                    stock = ifelse(stock=="sablefish" & area=="AK-wide", "Alaska-wide OFL and ABC", stock)) %>%
      dplyr::select(-id)  %>%
      dplyr::mutate(stock = factor(stock, levels = bsai_species$id),
                    area = factor(area, levels = bsai_levels)) -> dat


    dat %>%
      dplyr::filter(stock=="Sablefish" & area == "AK Total") -> st
    st[,5:6] <- NA

    dat %>%
      dplyr::filter(stock=="Sablefish" & area == "GOA Total") -> gt
    gt[,c(3:4,7:10)] <- NA

    dplyr::bind_rows(dat,
                     dat %>%
                     dplyr::ungroup() %>%
                     dplyr::filter(area %in% c("Total", "GOA-wide")) %>%
                     dplyr::bind_rows(st, gt) %>%
                     dplyr::summarise(dplyr::across(where(is.numeric),~sum(., na.rm=TRUE))) %>%
                     dplyr::mutate(stock="Total", "area"="Total")) -> data

    data %>%
      flextable::flextable() %>%
      flextable::font(fontname = "Times New Roman", part = "all") %>%
      flextable::fontsize(size = 11, part = "all") %>%
      flextable::add_header_row(values=c("","",rep(yr,3),"Catch as of", rep(yr+1,2), rep(yr+2 ,2))) %>%
      flextable::compose(i=2, j=1:10,
                         value = flextable::as_paragraph(c("Species", "Area", "OFL", "ABC", "TAC", catch_date,
                                                           "OFL", "ABL", "OFL", "ABC")),
                         part="header") -> tbl


    tbl %>%
      flextable::theme_zebra(even_body = "#ADD8E6", odd_body = "transparent",
                             even_header = "#ADD8E6", odd_header = "#ADD8E6") %>%
      flextable::bg(j=1,bg = "transparent") %>%
      flextable::align(i=1, align = "center", part = "header") %>%
      flextable::align(j=2, i=c(6,8,12,18,23,28,33,38,43,49,53,57,62,66,67,71,75,76,80,84), align = "right", part = "body") %>%
      flextable::hline_top(part = "header") %>%
      flextable::hline_top() %>%
      flextable::merge_h(i=1, part = "header") %>%
      flextable::merge_v(j= ~stock) %>%
      flextable::width(j=1, width=2.0) %>%
      flextable::width(j=c(2,6,10), width=1.25) %>%
      flextable::hline(i = rle(cumsum(.$body$spans$columns[,1]))$values) %>%
      flextable::merge_at(i=88, j=1:2) %>%
      flextable::align(i=88,j=1, align="center") %>%
      flextable::vline(j=c(2,6,8,10)) %>%
      flextable::bold(j=1) %>%
      flextable::bold(i=88,j=3:10) %>%
      flextable::colformat_double(j=6, digits=0) %>%
      flextable::fix_border_issues() -> ft

  } else if(area=="bsai" & month=="sep"){

    df %>%
      dplyr::filter(year %in% (yr-1):(yr+1)) %>%
      tidyr::pivot_wider(names_from = year, values_from = c(-stock, -area, -year)) %>%
      dplyr::select(-paste0("catch_", yr+1)) %>%
      dplyr::relocate(stock, area, paste0("ofl_",year-1), paste0("abc_",year-1),
                      paste0("tac_",year-1), paste0("catch_",year-1), paste0("ofl_",year),
                      paste0("abc_",year), paste0("tac_",year), paste0("catch_",year),
                      paste0("ofl_",year+1), paste0("abc_",year+1), paste0("tac_",year+1)) %>%
      dplyr::left_join(goa_species) %>%
      dplyr::mutate(stock = id,
                    stock = ifelse(stock=="sablefish" & area=="AK-wide",
                                   "Alaska-wide OFL and ABC", stock)) %>%
      dplyr::select(-id)  %>%
      dplyr::mutate(stock = factor(stock, levels = goa_species$id)) -> dat

    dat %>%
      dplyr::bind_rows(
        dat %>%
          dplyr::ungroup() %>%
          dplyr::filter(area %in% c("Total", "AK Total", "GOA-wide")) %>%
          dplyr::summarise(dplyr::across(where(is.numeric),~sum(., na.rm=TRUE))) %>%
          dplyr::mutate(stock="Total", "area"="Total")) -> data

    data %>%
      flextable::flextable() %>%
      flextable::font(fontname = "Times New Roman", part = "all") %>%
      flextable::fontsize(size = 11, part = "all") %>%
      flextable::add_header_row(values=c("","",rep(year-1,3),"Catch as of", rep(year,3), "Catch as of", rep(paste0("Plan Team Proposed ", year+1,"/", year+2),3))) %>%
      flextable::compose(i=2, j=1:13,
                         value = flextable::as_paragraph(c("Species", "Area", "OFL", "ABC", "TAC", paste0("12/31/", year-1),
                                                           "OFL", "ABL", "TAC", catch_date, "OFL", "ABC", "TAC")),
                         part="header") -> tbl


    tbl %>%
      flextable::theme_zebra(even_body = "#ADD8E6", odd_body = "transparent",
                             even_header = "#ADD8E6", odd_header = "#ADD8E6") %>%
      flextable::bg(j=1,bg = "transparent") %>%
      flextable::align(i=1, align = "center", part = "header") %>%
      flextable::align(j=2, i=c(6,8,12,18,23,28,33,38,43,49,53,57,62,66,67,71,75,76,80,84), align = "right", part = "body") %>%
      flextable::hline_top(part = "header") %>%
      flextable::hline_top() %>%
      flextable::merge_h(i=1, part = "header") %>%
      flextable::merge_v(j= ~stock) %>%
      flextable::width(j=1, width=2.0) %>%
      flextable::width(j=c(2,6,10), width=1.25) %>%
      flextable::hline(i = rle(cumsum(.$body$spans$columns[,1]))$values) %>%
      flextable::merge_at(i=88, j=1:2) %>%
      flextable::align(i=88,j=1, align="center") %>%
      flextable::vline(j=c(2,6,8,10)) %>%
      flextable::bold(j=1) %>%
      flextable::bold(i=88,j=3:13) %>%
      flextable::colformat_double(j=c(6,10), digits=0) %>%
      flextable::fix_border_issues() -> ft
  } else if(area=="bsai" & month=="nov"){

    df %>%
      dplyr::filter(year %in% (yr):(yr+2)) %>%
      tidyr::pivot_wider(names_from = year, values_from = c(-stock, -area, -year)) %>%
      dplyr::select(dplyr::any_of(nov_names)) %>%
      dplyr::left_join(bsai_species) %>%
      dplyr::mutate(stock = id,
                    stock = ifelse(stock=="sablefish" & area=="AK-wide", "Alaska-wide OFL and ABC", stock)) %>%
      dplyr::select(-id)  %>%
      dplyr::mutate(stock = factor(stock, levels = bsai_species$id),
                    area = factor(area, levels = bsai_levels)) -> dat

        dat %>%
      dplyr::bind_rows(dat %>%
                         dplyr::filter(area %in% c('BSAI', 'BSAI/GOA') | stock%in%c("Pacific cod", "Pollock")) %>%
                         dplyr::summarise(dplyr::across(where(is.numeric),~sum(., na.rm=TRUE))) %>%
                         dplyr::mutate(stock="Total", "area"="Total")) -> data

    data %>%
      flextable::flextable() %>%
      flextable::font(fontname = "Times New Roman", part = "all") %>%
      flextable::fontsize(size = 11, part = "all") %>%
      flextable::add_header_row(values=c("","",rep(yr,3),"Catch as of", rep(yr+1,2), rep(yr+2 ,2))) %>%
      flextable::compose(i=2, j=1:10,
                         value = flextable::as_paragraph(c("Species", "Area", "OFL", "ABC", "TAC", catch_date,
                                                           "OFL", "ABL", "OFL", "ABC")),
                         part="header") -> tbl


    tbl %>%
      flextable::theme_zebra(even_body = "#ADD8E6", odd_body = "transparent",
                             even_header = "#ADD8E6", odd_header = "#ADD8E6") %>%
      flextable::bg(j=1,bg = "transparent") %>%
      flextable::align(i=1, align = "center", part = "header") %>%
      flextable::hline_top(part = "header") %>%
      flextable::hline_top() %>%
      flextable::merge_h(i=1, part = "header") %>%
      flextable::merge_v(j= ~stock) %>%
      flextable::width(j=1, width=2.0) %>%
      flextable::width(j=c(2,6,10), width=1.25) %>%
      flextable::hline(i = rle(cumsum(.$body$spans$columns[,1]))$values) %>%
      flextable::merge_at(i=39, j=1:2) %>%
      flextable::align(i=39,j=1, align="center") %>%
      flextable::vline(j=c(2,6,8,10)) %>%
      flextable::bold(j=1) %>%
      flextable::bold(i=39,j=3:10) %>%
      flextable::colformat_double(j=6, digits=0) %>%
      flextable::fix_border_issues() -> ft
  }




  # options("openxlsx2.numFmt" = "#,##0")
  openxlsx2::wb_workbook() %>%
    openxlsx2::wb_add_worksheet('goa') %>%
    openxlsx2::wb_add_data


  wb <- openxlsx2::wb_workbook()$add_worksheet("goa") %>%
    openxlsx2::wb_add_data(data, sheet="goa")

  # wb <- flexlsx::wb_add_flextable(wb, "goa",data)

  openxlsx2::wb_add_formula(wb, sheet="goa", x = "SUM(C8,C9)", dims = "C10") %>%
    openxlsx2::wb_add_formula(wb, sheet="goa", x = "SUM(C10,C14,C20,C25,C30,C35,C40,C45,C51,C55,C59,C64,C68,C69,C73,C77,C78,C82,C86,C87,C88,C89)", dims = "C90")


  openxlsx2::wb_save(wb, file="test.xlsx", overwrite = TRUE)

}
