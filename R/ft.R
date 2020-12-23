# 2018 population, source: Wikipedia
uk_pop <- tibble::tribble(
  ~nation, ~population,
  "England", 55977178,
  "Scotland", 5438100,
  "Wales", 3138631,
  "Northern Ireland", 1881641,
  "UK", 66435550	
)

last_week <- function(x, ctry="UK") {
  x %>%
    filter(country == ctry & region == ctry & year == 2020) %>%
    pull(week) %>%
    max()
}

last_date <- function(x, ctry="UK") {
  x %>%
    filter(country == ctry & region == ctry & year == 2020) %>% 
    pull(date) %>%
    max()
}

annual_deaths <- function(x, ctry = "UK", reg = NULL) {
  if(is.null(reg)) reg <- ctry
  d <- x %>%
    filter(country == ctry & region == reg)
  week.end <- last_week(x, ctry)
  d <- d %>%
    filter(week <= week.end) %>%
    group_by(year) %>%
    summarise(tot = sum(deaths), n = n())
}


excess_deaths <- function(x, ctry, reg=NULL, year1=2010, year2=2019) {
  d <- annual_deaths(x, ctry, reg)
  base <- d %>% filter(year >= year1, year <= year2)
  M <- mean(base$tot)
  SE <- sd(base$tot) / sqrt(nrow(base))
  tc <- qt(0.975, nrow(base) - 1)
  CI <- SE * tc
  D <- d[d$year == 2020, ]$tot - M
  
  list(
    diff = signif(D, 2),
    se = signif(SE, 2),
    ci = signif(CI, 2)
  )
}


prep_country_region <- function(d, ctry, by.region) {
  if(is.null(ctry)) ctry <- unique(d$country)
  
  if(by.region) {
    d <- d %>% 
      mutate(group = region) %>% 
      filter(region != country)
  } else {
    d <- d %>% 
      mutate(group = country) %>% 
      filter(region == country)
  }
  
  d <- d %>% 
    filter(country %in% ctry & date > "2015-01-01")
}


UK_nation_excess <- function(x) {
  map_dfr(uk_pop$nation, function(r) {
    ad <- annual_deaths(x, "UK", r)
    current <- ad[ad$year=="2020", ]$tot
    ad %>%
      filter(year >= 2015, year <= 2019) %>% 
      mutate(diff = current - tot, nation = r)
  }) %>% 
    left_join(uk_pop, by="nation") %>% 
    mutate(diff_pop = 1e6 * diff / population)
}





plot_excess_details <- function(d, ctry=NULL, by.region=FALSE, ncol=1, y.scale=3) {
  d <- prep_country_region(d, ctry, by.region)
  bkg <- d %>% filter(date < "2020-01-01")
  fg <- d %>% filter(date >= "2020-01-01")
  xmx <- max(fg$week) + 1
  ymx <- max(d$deaths) * 1.05
  
  bl <- d %>%
    group_by(group) %>%
    summarise(maxy = median(expected_deaths) * y.scale, week=1)
  
  ggplot() +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_blank(data=bl, aes(x=week, y=maxy)) +
    geom_blank(data=bl, aes(x=week, y=0)) +
    geom_beeswarm(data=bkg, aes(x=week, y=deaths), colour="grey70", size=0.5, cex=0.6) +
    #geom_step(data=fg, aes(x=week-0.5, y=deaths), size=0.7) +
    geom_line(data=fg, aes(x=week, y=deaths), colour="grey90") +
    geom_point(data=fg, aes(x=week, y=deaths), size=1.5, shape=22, fill=okabe_ito_palette[3]) +
    facet_wrap(~ group, ncol=ncol, scales = "free_y") +
    scale_x_continuous(breaks=c(1,10,20,30,40,50), limits=c(0, xmx)) +
    scale_y_continuous(expand=c(0,0)) +
    labs(x="Week", y="Deaths")
}

plot_excess_prop <- function(x, ctry=NULL, by.region=FALSE, ncol=1, y.scale=3) {
  d <- prep_country_region(x, ctry, by.region) %>% 
    filter(week <= max(week[year==2020])) %>%
    group_split(week, group) %>%
    map_dfr(function(w) {
      if(nrow(w) > 1 & nrow(w[w$year==2020, ]) == 1) {
        d2020 <- w[w$year==2020, ]$deaths
        w <- w %>% 
          mutate(prop = d2020 / deaths) %>% 
          filter(year < 2020)
        w$pval = t.test(w$prop, mu = 1, alternative = "greater")$p.value
        w
      }
    }) %>% 
    group_by(group) %>% 
    mutate(p.adj = p.adjust(pval, method="BH"), sig = p.adj < 0.05)
  
  ymx <- max(d$prop) * 1.05
  
  bl <- d %>%
    group_by(group) %>%
    summarise(maxy = max(prop)*1.05, week=1)
  
  ggplot(d) +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    ) +
    #geom_blank(data=bl, aes(x=week, y=maxy)) +
    #geom_blank(data=bl, aes(x=week, y=0)) +
    geom_beeswarm(aes(x=week, y=prop, colour=sig), size=1, cex=0.5) +
    scale_colour_manual(values=c("black", "red"))+
    facet_wrap(~ group, ncol=ncol, scales = "fixed") +
    scale_x_continuous(breaks=c(1,5,10,15,20,25,30,35,40,45,50)) +
    scale_y_continuous(expand=expansion(mult = c(0, 0.1))) +
    labs(x="Week", y="Excess deaths ratio")
}


plot_exc_total_deaths <- function(x, ctry="UK") {
  d <- annual_deaths(x, ctry)
  
  week.end <- x %>%
    filter(country == ctry & region == ctry) %>% 
    filter(year == 2020) %>% pull(week) %>% max()
  
  ggplot(d, aes(x=year, y=tot/1000)) + 
    theme_bw() +
    geom_col() +
    scale_x_continuous(breaks=seq(2000,2020,2)) +
    scale_y_continuous(expand=expansion(mult = c(0, 0.1))) +
    labs(x="Year", y=glue("Deaths (thousands) weeks 1-{week.end}"))
}

plot_exc_weekly_deaths <- function(x, ctry = "uk") {
  x %>% 
    filter(country == ctry & region == ctry) %>%
    ggplot(aes(x=date, y=deaths)) + 
    theme_bw() +
    geom_step() +
    labs(x = "Date", y="Weekly deaths")
}



plot_uk_nation_excess <- function(x) {
  x <- x %>% 
    mutate(year = as_factor(year), nation = factor(nation, levels=uk_pop$nation)) %>% 
    filter(nation != "UK")
  ggplot(x, aes(x=nation, y=diff_pop, group=nation)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_boxplot(outlier.shape = NA, colour="grey80") +
    geom_beeswarm() +
    scale_y_continuous(expand=expansion(mult = c(0, 0.1))) +
    scale_colour_viridis_d() +
    labs(x=NULL, y="2020 excess deaths per million vs 2015-2019")
}




