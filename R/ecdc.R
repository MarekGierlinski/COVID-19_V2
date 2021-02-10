plot_countries_col <- function(cvd, sel, what="cases", title=NULL, subtitle=NULL) {
  d <- cvd %>%
    filter(indicator == what) %>% 
    #mutate(value = 1e6 * count / population, value = value / 7) %>% 
    mutate(value = rate) %>% 
    filter(country %in% sel & date >= as.Date("2020-02-15")) %>% 
    arrange(date)
  
  ggplot(d, aes(x=date, y=value)) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(size=0.2, colour="grey90")
    ) +
    #geom_point(size=0.6) +
    geom_col(width=7, fill="grey40") +
    facet_wrap(~country, scale="free_y") +
    labs(x=NULL, y=glue("Daily {what} per million"), title=title, subtitle=subtitle) +
    scale_y_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.'), expand=expansion(mult=c(0,0.05)), limits=c(0, NA)) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b")
}

plot_countries_ridge <- function(cvd, sel, what="cases", scl=0.1) {
  d <- cvd %>%
    filter(indicator == what) %>% 
    mutate(value = 1e6 * count / population, value = value / 7) %>% 
    filter(country %in% sel & date >= as.Date("2020-02-15") & value >= 0) %>% 
    arrange(date)
  
  ggplot(d, aes(x=date, y=country, height=value, fill=country)) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      legend.position = "none"
    ) +
    geom_ridgeline(scale=scl) +
    scale_fill_cyclical(values = c(okabe_ito_palette[1:7])) +
    labs(x=NULL, y=NULL, title=glue("Reported {what} per million"))
}

plot_countries_line <- function(cvd, sel=NULL, what="cases", n.col=NULL) {
  d <- cvd %>%
    filter(indicator == what) %>% 
    mutate(value = 1e6 * count / population, value = value / 7) %>% 
    filter(date >= as.Date("2020-02-15")) %>% 
    arrange(date) %>% 
    filter(value > 0) %>% 
    group_by(country) %>% 
    mutate(n = n()) %>% 
    filter(n > 2) %>% 
    ungroup()
    
  if(!is.null(sel)) d <- d %>% filter(country %in% sel)
  
  ggplot(d, aes(x=date, y=value)) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill="grey90", colour=NA)
    ) +
    geom_step() +
    facet_wrap(~country, scale="free_y", ncol=n.col) +
    labs(x=NULL, y=glue("Daily {what} per million")) +
    scale_y_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.'), expand=expansion(mult=c(0,0.05)), limits=c(0, NA)) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b")
}


plot_countries_hysteresis <- function(cvd, sel) {
  brk <- seq.Date(as.Date("2020-01-01"), as.Date("2021-12-01"), by = "2 months")
  lbs <- format(brk, "%b")
  cvd %>%
    filter(country %in% sel & date >= as.Date("2020-02-15")) %>%
    mutate(value = 1e6 * count / (7 * population)) %>% 
    select(date, country, indicator, value) %>% 
    pivot_wider(id_cols = c(date, country), names_from = indicator, values_from = value) %>% 
    ggplot(aes(x=cases, y=deaths, group=country, colour=date)) +
    theme_bw() +
    theme(panel.grid = element_blank(), text=element_text(size=8)) +
    geom_path() +
    geom_point(size=0.5) +
    facet_wrap(~country, scales="free") +
    #scale_colour_viridis_c(trans="date", option="cividis") +
    scale_colour_distiller(palette="RdYlBu", trans="date", breaks=brk, labels=lbs) +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.05)), limits=c(0, NA)) +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.05)), limits=c(0, NA)) +
    labs(x="Daily cases per million", y = "Daily deaths per million", colour="Week starting on")
}


plot_heatmap_clust <- function(cvd, sel=NULL, mx.limit=10, pop.limit=1e6) {
  if(!is.null(sel)) cvd <- cvd %>% filter(country %in% sel)
  
  tab <- cvd %>% 
    filter(date > as.Date("2020-04-01") & indicator == "cases") %>% 
    group_by(country) %>% 
    mutate(value = 1e6 * count / (population * 7), mx = max(value)) %>% 
    ungroup() %>% 
    filter(mx >= mx.limit & population >= pop.limit) %>% 
    pivot_wider(id_cols = country, names_from = date, values_from = value, values_fill = 0) %>% 
    as.data.frame() %>% 
    column_to_rownames("country")
  
  ggheatmap(tab, palette="viridis", order.col = FALSE, with.y.text = TRUE, legend.name="Cases per million")
}

select_top_recent <- function(cvd, what, n=12) {
  cvd %>%
    filter(indicator == what & population>1e6) %>%
    mutate(val = count / population) %>%
    select(date, country, val) %>%
    drop_na() %>%
    group_by(country) %>%
    arrange(date) %>%
    summarise(last_val = last(val)) %>%
    arrange(desc(last_val)) %>%
    head(n) %>%
    pull(country)
}


plot_cases_diff_deaths <- function(cvd, pop=FALSE, x.min=NULL) {
  brkmin <- 0
  xmin <- 0.05
  xlab <- "Reported deaths (red) and cases (blue)"
  if(pop) {
    cvd <- mutate(cvd, count = 1e6 * count / population)
    xmin <- 0.005
    xlab <- paste(xlab, "per million")
    brkmin <- -3
  }
  if(!is.null(x.min)) xmin <- x.min
  
  brks <- c(1) * 10^sort(rep(brkmin:6,3))
  labs <- sprintf("%f", brks) %>% str_remove("0+$") %>% str_remove("\\.$") %>% prettyNum(big.mark = ",") %>% str_remove("^\\s+")
  
  d <- cvd %>%
    filter(!str_detect(country, "total")) %>% 
    group_by(country, indicator) %>%
    summarise(count_tot = sum(count)) %>%
    ungroup() %>% 
    pivot_wider(id_cols = country, names_from = indicator, values_from = count_tot) %>% 
    filter(deaths > 0) %>% 
    mutate(country=fct_reorder(country, deaths), lab=paste0(as.character(country), "  ")) %>% 
    mutate(font = if_else(country == "United Kingdom", 2, 1))
  ggplot(d, aes(y=country)) +
    theme_bw() +
    geom_segment(aes(x=deaths, xend=cases, y=country, yend=country), colour="grey50") +
    geom_point(aes(x = cases), shape=21, fill="blue", size=1.5, colour="grey50") +
    geom_point(aes(x = deaths), shape=21, fill="red", size=1.5, colour="grey50") +
    geom_text(aes(x=deaths, y=country, label=lab, fontface=font), hjust=1, size=2.5) +
    scale_x_log10(breaks=brks, labels=labs, limits=c(xmin, max(d$cases)*1.05)) +
    scale_y_discrete(expand=c(0,1)) +
    labs(x=xlab, y=NULL) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      text = element_text(size=8),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    )
}

plot_cases_deaths_pop <- function(cvd) {
  d <- cvd %>%
    filter(!str_detect(country, "total")) %>% 
    mutate(count = 1e6 * count / population) %>% 
    group_by(country, population, indicator) %>%
    summarise(count_tot = sum(count)) %>%
    ungroup() %>% 
    pivot_wider(id_cols = c(country, population), names_from = indicator, values_from = count_tot) %>% 
    filter(deaths > 0) %>% 
    mutate(font = if_else(country == "United Kingdom", 2, 1))
  ds <- d %>%
    filter((cases > 15000 | deaths > 300) | country %in% c("China", "India"))
  ggplot(d, aes(x=cases, y=deaths, label=country)) +
    theme_bw() +
    geom_point(aes(size=population/1e6), colour="grey60")  +
    geom_text_repel(data=ds, aes(fontface=font), size=2.5) +
    scale_size_area(max_size = 20, breaks=c(1, 10, 100, 1000)) +
    labs(x="Reported cases per million", y="Reported deaths per million", size="Population (million)") +
    scale_x_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.'), expand=expansion(mult=c(0,0.05)), limits=c(0, NA)) +
    scale_y_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.'), expand=expansion(mult=c(0,0.05)), limits=c(0, NA))
}

plot_cases_deaths_track <- function(cvd, sel, min.date="2020-09-01", title=NULL) {
  d <- cvd %>%
    filter(country %in% sel) %>% 
    mutate(count = 1e6 * count / population) %>% 
    group_by(country, indicator) %>% 
    arrange(date) %>% 
    mutate(count = cumsum(count)) %>% 
    select(date, country, population, indicator, count) %>%
    filter(date >= as.Date(min.date)) %>% 
    pivot_wider(id_cols = c(date, country, population), names_from = indicator, values_from = count)
  dt <- d %>% 
    filter(date == max(date)) %>% 
    mutate(font = if_else(country == "United Kingdom", 2, 1))
  ggplot(d, aes(x=cases, y=deaths, colour=date, group=country)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_path() +
    #geom_point(size=0.3) +
    scale_colour_distiller(palette="RdYlBu", trans="date") +
    geom_point(data=dt, aes(x=cases, y=deaths, group=1), colour="black") +
    geom_text_repel(data=dt, aes(x=cases, y=deaths, label=country, fontface=font), colour="black", size=2.5) +
    labs(x="Cumulative cases per million", y="Cumulative deaths per million", title=title, colour=NULL) +
    scale_x_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.'), expand=expansion(mult=c(0,0.05)), limits=c(0, NA)) +
    scale_y_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.'), expand=expansion(mult=c(0,0.05)), limits=c(0, NA))
}


animate_cases_deaths <- function(cvd, sel, min.date="2020-03-01") {
  d <- cvd %>%
    filter(country %in% sel) %>% 
    mutate(count = 1e6 * count / population) %>% 
    group_by(country, indicator) %>% 
    arrange(date) %>% 
    mutate(count = cumsum(count)) %>% 
    select(date, country, population, indicator, count) %>%
    filter(date >= as.Date("2020-03-01")) %>% 
    pivot_wider(id_cols = c(date, country, population), names_from = indicator, values_from = count)
  ggplot(d, aes(x=cases, y=deaths, group=country)) + 
    theme_bw() +
    geom_path(colour="grey70") +
    geom_point() +
    geom_text(aes(label=country), vjust=-1.5) +
    transition_reveal(date) +
    view_follow() +
    labs(title="{frame_along}", x="Cumulative cases per million", y="Cumulative deaths per million") +
    scale_x_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.'), expand=expansion(mult=c(0,0.05)), limits=c(0, NA)) +
    scale_y_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.'), expand=expansion(mult=c(0,0.05)), limits=c(0, NA))
}