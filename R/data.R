# 2018 population, source: Wikipedia
uk_pop <- tibble::tribble(
  ~nation, ~population,
  "England", 55977178,
  "Scotland", 5438100,
  "Wales", 3138631,
  "Northern Ireland", 1881641,
  "UK", 66435550	
)

# for figure annotation only
url_corona <- "http://coronavirus.data.gov.uk"


get_ecdc_url <- function() {
  urlc <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
  stopifnot(url.exists(urlc))
  urlc
}

fetch_ecdc_data <- function(urlc) {
  read_csv(urlc, col_types = cols())
}

process_ecdc_data <- function(raw) {
  d <- raw %>% 
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
  print(paste("ECDC last date", max(d$date)))
  d
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
  d <- raw %>%
    filter(!is.na(week)) %>% 
    select(-month)
  print(paste("FT last date", max(d$date)))
  d
}

##########################################################

# Obsolete
# get_gov_url_v1 <- function() {
#   'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation&structure={"areaType":"areaType","areaName":"areaName","areaCode":"areaCode","date":"date","newCasesBySpecimenDate":"newCasesBySpecimenDate","cumCasesBySpecimenDate":"cumCasesBySpecimenDate","hospitalCases":"hospitalCases","newAdmissions":"newAdmissions","newDeaths28DaysByDeathDate":"newDeaths28DaysByDeathDate"}&format=csv'
# }

# Metrics to read from coronavirus.gov.uk. Values are metrics, names are used to
# rename them into more convenient variables.

gov_metrics <-   list(
  cases = "newCasesBySpecimenDate",
  cases_pub = "newCasesByPublishDate",
  hospital_cases = "hospitalCases",
  admissions = "newAdmissions",
  deaths = "newDeaths28DaysByDeathDate",
  deaths_pub = "newDeaths28DaysByPublishDate",
  tests = "newVirusTests",
  dose1 = "newPeopleReceivingFirstDose",
  dose2 = "newPeopleReceivingSecondDose"
) 

get_gov_url_v2 <- function() {
  metrics <- paste(glue("metric={gov_metrics}"), collapse="&")
  glue("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&{metrics}&format=csv")
}

fetch_gov_data <- function(urlc) {
  read_csv(urlc, col_types = cols())
}

process_gov_data <- function(raw) {
  d <- raw %>% 
    select(-c(areaType, areaCode)) %>% 
    rename(nation = areaName) %>% 
    rename(!!!gov_metrics) %>% 
    left_join(uk_pop, by="nation")
  print(paste("GOV last date", max(d$date)))
  d
}