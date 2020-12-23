get_ecdc_url <- function() {
  urlc <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
  stopifnot(url.exists(urlc))
  urlc
}

fetch_ecdc_data <- function(urlc) {
  read_csv(urlc, col_types = cols())
}

process_ecdc_data <- function(raw) {
  raw %>% 
    rename(
      country = countriesAndTerritories,
      code = countryterritoryCode,
      geo_id = geoId,
      cases = cases_weekly,
      deaths = deaths_weekly,
      population = popData2019,
      continent = continentExp
    ) %>%
    mutate(
      country = str_replace_all(country, "_", " "),
      country = recode(country, 'United States of America' = "United States"),
      code = recode(code, "XKX" = "KOS"),
      date = as.Date(dateRep, format="%d/%m/%Y")
    ) %>% 
    select(-dateRep) %>% 
    separate(year_week, c("year", "week")) %>% 
    group_by(country) %>% 
    arrange(date) %>% 
    mutate(
      tot_cases = sum(cases),
      cum_cases = cumsum(cases),
      cum_deaths = cumsum(deaths)
    ) %>% 
    ungroup() %>% 
    filter(
      country != "Cases on an international conveyance Japan"
    ) %>% 
    mutate(
      cases_pop = 1e6 * cases / population,
      deaths_pop = 1e6 * deaths/ population,
      cum_cases_pop = 1e6 * cum_cases / population,
      cum_deaths_pop = 1e6 * cum_deaths/ population
    )
}

##########################################################

get_ft_url <- function() {
  urlc <- "https://github.com/Financial-Times/coronavirus-excess-mortality-data/raw/master/data/ft_excess_deaths.csv"
  stopifnot(url.exists(urlc))
  urlc
}

fetch_ft_data <- function(urlc) {
  read_csv(urlc, col_types = cols())
}

# Ignore monthly data
process_ft_data <- function(raw) {
  raw %>%
    filter(!is.na(week)) %>% 
    select(-month)
}


