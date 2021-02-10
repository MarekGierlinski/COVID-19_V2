week_from_first_monday <- function(dt) {
  # weekly data starting on the first Monday in data set
  wk <- tibble(date = dt) %>% 
    arrange(date) %>% 
    mutate(wday = lubridate::wday(date, week_start=1))
  
  first_monday <- wk %>% filter(wday == 1) %>% pull(date) %>% first()
  
  week <-  floor(as.integer(dt - first_monday) / 7) + 1
}


plot_gov_weekly_val <- function(gv, what) {
  d <- gv %>%
    select(date, nation, population, value = !!sym(what)) %>% 
    mutate(value = 1e6 * value / population)
  
  w <- d %>% 
    arrange(date) %>% 
    mutate(week = week_from_first_monday(date)) %>% 
    group_by(nation, week) %>%
    summarise(value = mean(value), n = n(), week_date = first(date)) %>% 
    ungroup() %>% 
    filter(n >= 6) %>% 
    drop_na()
  # add one more point in cases and deaths for nice line ending
  ww <- w %>% 
    group_split(nation) %>% 
    map_dfr(function(x) {
      x %>% 
        add_row(week=last(x$week)+1,  value=last(x$value), week_date=last(x$week_date)+7, nation=first(x$nation))
    })

  ggplot(d, aes(x=date, y=value)) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(size=0.2, colour="grey90")
    ) +
    #geom_point(size=0.6) +
    geom_col(width=1, fill="grey70") +
    geom_step(data=ww, aes(x=week_date, y=value), alpha=0.8, colour="black", size=1) +
    facet_wrap(~nation, scale="fixed", ncol=4) +
    labs(x=NULL, y=glue("Daily {what} per million")) +
    scale_y_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.'), expand=expansion(mult=c(0,0.05)), limits=c(0, NA)) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b", limits=c(as.Date("2020-03-01"), max(d$date)))
}

plot_gov_weekly <- function(gv) {
  last_date <- max(gv$date, na.rm=TRUE)
  map(c("tests", "cases_pub", "admissions", "deaths_pub"), function(vl) {
    plot_gov_weekly_val(gv, vl) 
  }) %>% 
    plot_grid(plotlist = ., ncol = 1, align="v")
}


sum_gov <- function(gv, by_publish_date = FALSE) {
  if(by_publish_date) gv <- gv %>% mutate(cases = cases_pub, deaths = deaths_pub)
  gv %>%
    group_by(date) %>%
    summarise(
      admissions = sum(admissions),
      cases = sum(cases),
      deaths = sum(deaths),
      dose1 = sum(dose1),
      dose2 = sum(dose2),
      tests = sum(tests),
      n = n()
    ) %>% 
    filter(n == 4) %>% 
    select(-n) %>% 
    pivot_longer(-date) %>% 
    mutate(name = name %>% str_to_title) %>% 
    mutate(name = factor(name, levels=c("Tests", "Cases", "Admissions", "Deaths", "Dose1", "Dose2")))
}


plot_admissions_cases_deaths <- function(gv, by_publish_date = FALSE) {
  tit <- ifelse(by_publish_date, "By publication date", "By sample/death date")
  gov_uk <- sum_gov(gv %>% filter(date >= "2020-03-01"), by_publish_date) %>% 
    filter(name %in% c("Tests", "Cases", "Admissions", "Deaths"))
  w <- gov_uk %>%
    mutate(week = week_from_first_monday(date)) %>% 
    group_by(name, week) %>%
    summarise(m = mean(value, na.rm=TRUE), n = sum(!is.na(value)), s = sd(value, na.rm=TRUE) / sqrt(n), week_date = first(date)) %>% 
    ungroup() %>% 
    filter(n >= 6)
  # add one more point in cases and deaths for nice line ending
  ww <- w %>% 
    group_split(name) %>% 
    map_dfr(function(x) {
      x %>% 
        add_row(week=last(x$week)+1,  m=last(x$m), s=NA, week_date=last(x$week_date)+7, name=first(x$name))
    })
  
  ggplot() +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank()
    ) +
    scale_y_log10(label=scales::comma_format(accuracy=1), breaks=10^(0:7)) +
    geom_step(data=gov_uk, aes(x=date, y=value, colour=name), alpha=0.3) +
    geom_step(data=ww, aes(x=week_date, y=m, colour=name), size=1) +
    #geom_errorbar(data=ww, aes(x=week_date+3.5, ymin=m-s, ymax=m+s), width=2, colour="grey") +
    scale_colour_manual(values=okabe_ito_palette) +
    labs(x=NULL, y="Daily count", colour=NULL, title="UK COVID-19 daily count", subtitle=tit) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b")
}


plot_vaccination <- function(gv) {
  # include older weekly reports
  gw <- gv %>% 
    filter(date < "2021-01-05") %>% 
    filter(weekly_dose1 > 0 | weekly_dose2 > 0) %>% 
    select(date, weekly_dose1, weekly_dose2, population, nation) %>% 
    pivot_longer(cols=c("weekly_dose1", "weekly_dose2"), names_to="dose", values_to="count") %>% 
    mutate(dose = recode(dose, weekly_dose1 = "First dose", weekly_dose2 = "Second dose")) %>% 
    arrange(date) %>% 
    group_by(nation, dose) %>% 
    mutate(cumulative = 100 * cumsum(count) / population) %>% 
    ungroup()
  
  
  gd <- gv %>% 
    filter(cum_dose1 > 0) %>% 
    mutate(
      cum_dose1 = if_else(is.na(dose1) | dose1 > 0, cum_dose1, as.numeric(NA)),
      cum_dose2 = if_else(is.na(dose2) | dose1 > 0, cum_dose2, as.numeric(NA))
    ) %>% 
    select(date, cum_dose1, cum_dose2, population, nation) %>% 
    pivot_longer(cols=c("cum_dose1", "cum_dose2"), names_to="dose", values_to="count") %>% 
    mutate(dose = recode(dose, cum_dose1 = "First dose", cum_dose2 = "Second dose")) %>% 
    mutate(cumulative = 100 * count / population) 
  
  gf <- bind_rows(gw, gd) %>% 
    drop_na() %>% 
    filter(count > 0)
  
  ggplot(gf, aes(x=date, y=cumulative, colour=nation, shape=dose)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_line() +
    geom_point(fill="white") +
    scale_colour_manual(values = uk_palette) +
    scale_shape_manual(values = c(19, 21)) +
    scale_y_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.'), expand=expansion(mult=c(0,0.05)), limits=c(0, NA)) +
    scale_x_date() +
    labs(x="Date", y="Percentage of population vaccinated", colour=NULL, shape=NULL, title="COVID-19 vaccination in the UK")
}


# Johnson set out a target of 15 m people by mid-February, on Jan 4.
# This was later revised to 13.9 m.
plot_vaccination_target <- function(gov) {
  target <- 100 * 13.9e6 / uk_pop$population[5]
  plot_vaccination(gov) +
    geom_vline(xintercept = as.Date("2021-02-15"), linetype="dashed") +
    geom_hline(yintercept = target, linetype="dashed") +
    xlim(as.Date(c("2020-12-12", "2021-02-17"))) +
    labs(title="Vaccination target")
}

plot_cum_deaths <- function(gv) {
  gov_uk <- sum_gov(gv, by_publish_date = TRUE)
  gov_uk %>%
    filter(name == "Deaths" & value > 0) %>%
    arrange(date) %>%
    mutate(cum = cumsum(value)) %>%
  ggplot(aes(x=date, y=cum)) +
    geom_line(colour="salmon4") +
    geom_hline(yintercept = 41500, linetype="dashed") +
    theme_bw() +
    scale_y_continuous(expand=expansion(mult=c(0,0.03)), labels = scales::comma_format(big.mark = ',', decimal.mark = '.', accuracy=1)) +
    theme(panel.grid = element_blank()) +
    labs(x=NULL, y="Cumulative deaths", title="UK COVID-19 deaths in 2020/2021")
}

tail_fit <- function(d, dmin, dmax) {
  dd <- d %>% 
    filter(date >= dmin & date <= dmax) %>% 
    mutate(x = as.integer(date - dmin), y = log(value))
  f <- lm(y ~ x, data=dd)
  f$coefficients
}


plot_second_wave_prediction <- function(gov, pred.date=NULL, remove.last=5) {
  deaths <- sum_gov(gov, by_publish_date = FALSE) %>%
    filter(name == "Deaths") %>% 
    # last few data points incomplete for "by death date"
    filter(date < max(date) - remove.last) %>% 
    drop_na() 
  if(is.null(pred.date)) {
    pd <- max(deaths$date) - remove.last
  } else {
    # initial prediction made on 2 Feb, based on available data up to 31 Jan, with 
    # last 5 dates of incomplete data removed. Hence 26 Jan.
    pd <- pred.date - 1 - remove.last
  }
  d1 <- as.Date("2020-04-08")  # peak of first wave
  d2 <- as.Date("2021-01-19")  # peak of second wave
  delta1 <- 110  # duration of a wave
  delta2 <- 120
  
  # not easy
  fc1 <- tail_fit(deaths, d1, d1 + delta1)
  fc2 <- tail_fit(deaths, d2, pd)
  r1 <- tibble(
    x = seq(d1, d1 + delta1, 1),
    y = exp(fc1[1] + fc1[2] * (as.integer(x - d1)))
  ) %>% filter(row_number() > 7)
  
  r2 <- tibble(
    x = seq(d2, d2 + delta2, 1),
    y = exp(fc2[1] + fc2[2] * as.integer(x - d2))
  ) %>% filter(row_number() > 1)
  
  r <- tibble(xmin=c(d1, d2), xmax=c(d1+delta1, d2+delta2), ymin=c(0,0), ymax=c(1500,1500))
  
  ggplot(deaths, aes(x=date, y=value)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    scale_y_continuous(expand=c(0,0)) +
    scale_x_date(limits=as.Date(c("2020-03-01", "2021-06-01")), date_breaks="1 month", date_labels="%b") +
    labs(x=NULL, y="Daily death count", title=glue("Second wave prediction made on {pred.date}")) +
    geom_rect(data=r, aes(x=NULL, y=NULL, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey80", alpha=0.3) +
    geom_point(size=0.8) +
    geom_line(data=r1, aes(x=x, y=y), colour="red") +
    geom_line(data=r2, aes(x=x, y=y), colour="red", linetype="dashed")
}