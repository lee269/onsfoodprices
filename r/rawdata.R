# Wrangle ONS published spreadsheet
# Online price changes taken from the Coronavirus Faster Indicators publication
# https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/bulletins/coronavirustheukeconomyandsocietyfasterindicators/previousReleases
# Datasets are here:
# https://www.ons.gov.uk/economy/inflationandpriceindices/datasets/onlineweeklypricechanges/2020

pacman::p_load(here, tidyverse, tidyxl, unpivotr, lubridate)

extract_indices <- function(xlsx) {

  tidy_data <- 
    tidyxl::xlsx_cells(xlsx) %>% 
    dplyr::filter(sheet == "Indices") %>% 
    dplyr::filter(!is_blank, row > 1 & row <= 155) %>% 
    unpivotr::behead("up", week) %>% 
    unpivotr::behead("left-up", category) %>% 
    unpivotr::behead("left", item) %>%
    dplyr::select(category, item, week, value = numeric) %>% 
    dplyr::mutate(week = lubridate::dmy(week))  

  return(tidy_data)
}


rawxls <- here("data", "raw", "8thoctoberonlineweeklypricesdatasetcleaned.xlsx")
tidy_prices <- extract_indices(rawxls)

saveRDS(tidy_prices, file = here("data", "tidy", "pricedata.rds" ))