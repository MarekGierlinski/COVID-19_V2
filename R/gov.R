plot_gov_weekly_val <- function(gv, what) {
  d <- gv %>%
    select(date, nation, population, value = !!sym(what)) %>% 
    mutate(value = 1e6 * value / population)

  w <- d %>%
    arrange(date) %>% 
    mutate(week = lubridate::week(date)) %>%
    group_by(nation, week) %>%
    summarise(value = mean(value), n = n(), week_date = first(date)) %>% 
    ungroup() %>% 
    filter(n == 7) %>% 
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
    scale_x_date(date_breaks = "2 months", date_labels = "%b")
}

plot_gov_weekly <- function(gv) {
  map(c("cases", "admissions", "deaths"), function(vl) {
    plot_gov_weekly_val(gv, vl)
  }) %>% 
    plot_grid(plotlist = ., ncol = 1)
}


sum_gov <- function(gv, by_publish_date = FALSE) {
  if(by_publish_date) gv <- gv %>% mutate(cases = cases_pub, deaths = deaths_pub)
  gv %>%
    group_by(date) %>%
    summarise(
      admissions = sum(admissions),
      cases = sum(cases),
      deaths = sum(deaths),
      dose1 = sum(dose1, na.rm=TRUE),
      dose2 = sum(dose2, na.rm=TRUE),
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
  tit <- ifelse(by_publish_date, "By publish date", "By sample/death date")
  gov_uk <- sum_gov(gv %>% filter(date >= "2020-03-01"), by_publish_date) %>% 
    filter(name %in% c("Cases", "Admissions", "Deaths"))
  w <- gov_uk %>%
    arrange(date) %>% 
    mutate(week = lubridate::week(date)) %>%
    group_by(name, week) %>%
    summarise(value = mean(value), n = n(), week_date = first(date)) %>% 
    ungroup() %>% 
    filter(n == 7)
  # add one more point in cases and deaths for nice line ending
  ww <- w %>% 
    group_split(name) %>% 
    map_dfr(function(x) {
      x %>% 
        add_row(week=last(x$week)+1,  value=last(x$value), week_date=last(x$week_date)+7, name=first(x$name))
    })
  
  ggplot() +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank()
    ) +
    scale_y_log10(label=scales::comma) +
    geom_step(data=gov_uk, aes(x=date, y=value, colour=name), alpha=0.3) +
    geom_step(data=ww, aes(x=week_date, y=value, colour=name), size=1) +
    scale_colour_manual(values=okabe_ito_palette) +
    labs(x=NULL, y="Daily count", colour=NULL, title=tit) +
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
    select(date, cum_dose1, cum_dose2, population, nation) %>% 
    pivot_longer(cols=c("cum_dose1", "cum_dose2"), names_to="dose", values_to="count") %>% 
    mutate(dose = recode(dose, cum_dose1 = "First dose", cum_dose2 = "Second dose")) %>% 
    mutate(cumulative = 100 * count / population) 
  
  gf <- bind_rows(gw, gd) 
  
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

plot_vaccination_target <- function(gov) {
  target <- 100 * 13.9e6 / uk_pop$population[5]
  plot_vaccination(gov) +
    geom_vline(xintercept = as.Date("2021-02-15"), linetype="dashed") +
    geom_hline(yintercept = target, linetype="dashed") +
    xlim(as.Date(c("2020-12-12", "2021-02-17"))) +
    labs(title="Vaccination target")
}