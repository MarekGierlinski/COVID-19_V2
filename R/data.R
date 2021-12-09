# 2019 population, source: Wikipedia
uk_pop <- tibble::tribble(
  ~nation, ~population,
  "England", 56286961,
  "Scotland", 5463300,
  "Wales", 3152879,
  "Northern Ireland", 1893667,
  "UK", 66796807
)

# for figure annotation only
url_corona <- "http://coronavirus.data.gov.uk"


get_time_stamp <- function(dat, src) {
  ts <- format(Sys.time(), "%d-%b-%Y %X")
  glue("Source: {src}, accessed on {ts}")
}

get_ecdc_url <- function() {
  urlc <- "https://opendata.ecdc.europa.eu/covid19/nationalcasedeath/csv"
  #urlc <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
  stopifnot(url.exists(urlc))
  urlc
}

fetch_ecdc_data <- function(urlc) {
  read_csv(urlc, col_types = cols())
}

date_from_week <- function(d) {
  d %>% 
    separate(year_week, c("year", "week"), sep="-", remove=FALSE) %>% 
    mutate_at(c("year", "week"), as.integer) %>% 
    group_split(year) %>% 
    map_dfr(function(w) {
      year <- first(w$year)
      new_year <- ymd(glue("{year}-01-01"))
      new_year_wday <- wday(new_year, week_start = 1)
      first_monday <- 8 - new_year_wday + new_year
      # I think they count weeks differently in 2020
      if(year == 2020) first_monday <- first_monday - 7
      
      w %>%
        mutate(date = first_monday + weeks(week - 1))
    })
}

process_ecdc_data <- function(raw) {
  d <- raw %>% 
    rename(
      count = weekly_count,
      rate = rate_14_day
    ) %>%
    date_from_week() %>% 
    group_by(country, indicator) %>% 
    mutate(
      tot_count = sum(count)
    ) %>% 
    ungroup() %>% 
    filter(
      country != "Cases on an international conveyance Japan" &
      !str_detect(country, "total")
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
#' Extracts paginated data by requesting all of the pages
#' and combining the results.
#'
#' @param filters    API filters. See the API documentations for 
#'                   additional information.
#'                   
#' @param structure  Structure parameter. See the API documentations 
#'                   for additional information.
#'                   
#' @return list      Comprehensive list of dictionaries containing all 
#'                   the data for the given ``filter`` and ``structure`.`
get_paginated_data <- function (filters, structure) {
  
    endpoint     <- "https://api.coronavirus.data.gov.uk/v1/data"
    results      <- list()
    current_page <- 1
    
    repeat {

        httr::GET(
            url   = endpoint,
            query = list(
                filters   = paste(filters, collapse = ";"),
                structure = jsonlite::toJSON(structure, auto_unbox = TRUE),
                page      = current_page
            ),
            httr::timeout(10)
        ) -> response
        
        # Handle errors:
        if ( response$status_code >= 400 ) {
            err_msg = httr::http_status(response)
            stop(err_msg)
        } else if ( response$status_code == 204 ) {
            break
        }
        
        # Convert response from binary to JSON:
        json_text <- httr::content(response, "text")
        dt        <- jsonlite::fromJSON(json_text)
        results   <- rbind(results, dt$data)
        
        if ( is.null( dt$pagination$`next` ) ){
            break
        }
        
        current_page <- current_page + 1;

    }
    
    return(results)
}


# Metrics to read from coronavirus.gov.uk. Values are metrics, names are used to
# rename them into more convenient variables.

query_filters <- c(
  "areaType=nation"
)

query_structure <-   list(
  date = "date",
  nation = "areaName",
  cases_pub = "newCasesByPublishDate",
  hospital_cases = "hospitalCases",
  admissions = "newAdmissions",
  deaths = "newDeaths28DaysByDeathDate",
  deaths_pub = "newDeaths28DaysByPublishDate",
  tests = "newVirusTestsByPublishDate",
  weekly_dose1 = "weeklyPeopleVaccinatedFirstDoseByVaccinationDate",
  weekly_dose2 = "weeklyPeopleVaccinatedSecondDoseByVaccinationDate",
  dose1 = "newPeopleVaccinatedFirstDoseByPublishDate",
  dose2 = "newPeopleVaccinatedSecondDoseByPublishDate",
  cum_dose1 = "cumPeopleVaccinatedFirstDoseByPublishDate",
  cum_dose2 = "cumPeopleVaccinatedSecondDoseByPublishDate"
) 


get_gov_url_v1 <- function() {
  metrics <- map_chr(query_structure, function(x) glue('"{x}":"{x}"')) %>% paste(collapse=",") 
  paste0('https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation&structure={', metrics, "}&format=csv") }


get_gov_url_v2 <- function() {
  metrics <- paste(glue("metric={gov_metrics}"), collapse="&")
  glue("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&{metrics}&format=csv")
}

fetch_gov_data <- function() {
  get_paginated_data(query_filters, query_structure) %>% 
    as_tibble() %>% 
    mutate(cases = cases_pub)  # by specimen date no longer available
}

process_gov_data <- function(raw) {
  d <- raw %>% 
    mutate(date = as.Date(date)) %>% 
    left_join(uk_pop, by="nation")
  print(paste("GOV last date", max(d$date)))
  d
}

##########################################################

get_url_owid_excess <- function() {
  "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/excess_mortality/excess_mortality.csv"
}

get_url_owid_vaccinations <- function() {
  "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"
}

get_url_owid_testing <- function() {
  "https://github.com/owid/covid-19-data/raw/master/public/data/testing/covid-testing-all-observations.csv"
}

fetch_owid <- function(urlc) {
  read_csv(urlc, col_types = cols())
}

process_owid_vaccination_data <- function(owd) {
  print(paste("OWID last date", max(owd$date)))
  owd
}
