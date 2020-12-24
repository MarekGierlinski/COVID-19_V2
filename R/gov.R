plot_gov_weekly_val <- function(gv, what) {
  vl <- glue("{what}_pop")
  
  d <- gv %>%
    select(date, nation, value = !!sym(vl))

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


sum_gov <- function(gv) {
  gv %>%
    group_by(date) %>%
    summarise(admissions = sum(admissions), cases = sum(cases), deaths = sum(deaths), n = n()) %>% 
    filter(n == 4) %>% 
    select(-n) %>% 
    pivot_longer(-date) %>% 
    mutate(name = name %>% str_to_title) %>% 
    mutate(name = factor(name, levels=c("Cases", "Admissions", "Deaths")))
}


plot_admissions_cases_deaths <- function(gv) {
  gov_uk <- sum_gov(gv)
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
    labs(x=NULL, y="Daily count", colour=NULL) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b")
}
