non_countries <- c("Gibraltar", "Isle of Man")

plot_owid_vaccination <- function(owd, n.top=10) {
  d <- owd %>% 
    filter(!is.na(total_vaccinations_per_hundred) & !is.na(iso_code) & !(location %in% non_countries)) %>% 
    select(location, date, perc = total_vaccinations_per_hundred)
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
    theme(legend.position = "none") +
    geom_line(data = d, aes(x=date, y=perc, group=location, colour=location)) +
    geom_point(data = d_last, aes(x=date, y=perc, colour=location)) +
    geom_text_repel(data = d_last, aes(x=date, y=perc, colour=location, label=location)) +
    scale_colour_manual(values=tableau_10_palette) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.03))) +
    labs(x=NULL, y="Percentage of people vaccinated", title=glue("Top {n.top} countries with highest vaccination proportion"))
}