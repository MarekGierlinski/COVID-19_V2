# 2018 population, source: Wikipedia
uk_pop <- tibble::tribble(
  ~nation, ~population,
  "England", 55977178,
  "Scotland", 5438100,
  "Wales", 3138631,
  "Northern Ireland", 1881641,
  "UK", 66435550	
)




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

##########################################################

get_gov_url <- function() {
  urlc <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation&structure={"areaType":"areaType","areaName":"areaName","areaCode":"areaCode","date":"date","newCasesBySpecimenDate":"newCasesBySpecimenDate","cumCasesBySpecimenDate":"cumCasesBySpecimenDate","hospitalCases":"hospitalCases","newAdmissions":"newAdmissions","newDeaths28DaysByDeathDate":"newDeaths28DaysByDeathDate"}&format=csv'
}

fetch_gov_data <- function(urlc) {
  read_csv(urlc, col_types = cols()) 
}

process_gov_data <- function(raw) {
  raw %>% 
    rename(
      nation = areaName,
      cases = newCasesBySpecimenDate,
      hospital_cases = hospitalCases,
      admissions = newAdmissions,
      deaths = newDeaths28DaysByDeathDate
    ) %>% 
    drop_na() %>% 
    left_join(uk_pop, by="nation") %>% 
    mutate(
      cases_pop = 1e6 * cases / population,
      hospital_cases_pop = 1e6 * hospital_cases / population,
      admissions_pop = 1e6 * admissions / population,
      deaths_pop = 1e6 * deaths / population
    )
}