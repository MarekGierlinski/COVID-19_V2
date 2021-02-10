non_countries <- c("Gibraltar", "Isle of Man", "Jersey", "Guernsey")

plot_owid_vaccination <- function(owd, n.top=10, what="people_vaccinated_per_hundred", sub) {
  d <- owd %>% 
    mutate(perc = !!sym(what)) %>% 
    filter(!is.na(perc) & !is.na(iso_code) & !(location %in% non_countries)) %>% 
    select(location, date, perc)
  d_last <- d %>% 
    group_by(location) %>% 
    arrange(date) %>% 
    summarise(date = last(date), perc = last(perc)) %>% 
    arrange(desc(perc)) %>% 
    head(n.top) %>% 
    mutate(location = as_factor(location))
  d <- d %>% 
    filter(location %in% as.character(d_last$location)) %>% 
    mutate(location = factor(location, levels = d_last$location))
  ggplot() +
    theme_bw() +
    theme(legend.position = "none", panel.grid.minor = element_blank()) +
    geom_line(data = d, aes(x=date, y=perc, group=location, colour=location), size=0.3) +
    geom_text_repel(data = d_last, aes(x=date, y=perc, colour=location, label=location), size=2.5, nudge_x = 1, direction = "y", hjust = "left", segment.colour="grey90", segment.size=0.3, segment.curvature=0) +
    geom_point(data = d_last, aes(x=date, y=perc, colour=location), size=0.8) +
    scale_colour_manual(values=tableau_10_palette) +
    scale_x_date(expand=expansion(mult=c(0.03, 0.2))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(x=NULL, y="Percentage of people vaccinated", title=glue("Top {n.top} countries with highest vaccination proportion"), subtitle=sub)
}