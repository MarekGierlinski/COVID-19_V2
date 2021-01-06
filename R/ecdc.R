plot_countries_col <- function(cvd, sel, what="cases") {
  d <- cvd %>%
    mutate(value = 1e6 * !!sym(what) / population, value = value / 7) %>% 
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
    labs(x=NULL, y=glue("Daily {what} per million")) +
    scale_y_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.'), expand=expansion(mult=c(0,0.05)), limits=c(0, NA)) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b")
}

plot_countries_ridge <- function(cvd, sel, what="cases", scl=0.1) {
  d <- cvd %>%
    mutate(value = 1e6 * !!sym(what) / population, value = value / 7) %>% 
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
    mutate(value = 1e6 * !!sym(what) / population, value = value / 7) %>% 
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
  brk <- seq.Date(as.Date("2020-01-01"), as.Date("2020-12-01"), by = "1 month")
  lbs <- format(brk, "%b")
  cvd %>%
    filter(country %in% sel & date >= as.Date("2020-02-15")) %>%
    ggplot(aes(x=1e6 * cases / (7 * population), y=1e6*deaths/(7*population), group=country, colour=date)) +
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
    filter(date > as.Date("2020-04-01")) %>% 
    group_by(country) %>% 
    mutate(value = 1e6 * cases / population, mx = max(value)) %>% 
    ungroup() %>% 
    filter(mx >= mx.limit & population >= pop.limit) %>% 
    pivot_wider(id_cols = country, names_from = date, values_from = value, values_fill = 0) %>% 
    as.data.frame() %>% 
    column_to_rownames("country")
  
  ggheatmap(tab, palette="viridis", order.col = FALSE, with.y.text = TRUE)
}