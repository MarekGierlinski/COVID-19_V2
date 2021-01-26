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
      start_date <- ymd(paste(min(w$year), "-01-01"))
      mon <- wday(start_date, week_start = 1)
      mon_dif = 8 - mon
      w %>%
        mutate(date = start_date + weeks(week - 1))
    })
}

process_ecdc_data <- function(raw) {
  d <- raw %>% 
    rename(
      count = weekly_count,
      rate = rate_14_day
    ) %>%
    date_from_week() %>% 
    group_by(country) %>% 
    arrange(date) %>% 
    mutate(
      tot_count = sum(count)
    ) %>% 
    ungroup() %>% 
    filter(
      country != "Cases on an international conveyance Japan"
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
  weekly_dose1 = "weeklyPeopleVaccinatedFirstDoseByVaccinationDate",
  weekly_dose2 = "weeklyPeopleVaccinatedSecondDoseByVaccinationDate",
  dose1 = "newPeopleVaccinatedFirstDoseByPublishDate",
  dose2 = "newPeopleVaccinatedSecondDoseByPublishDate",
  cum_dose1 = "cumPeopleVaccinatedFirstDoseByPublishDate",
  cum_dose2 = "cumPeopleVaccinatedSecondDoseByPublishDate"
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