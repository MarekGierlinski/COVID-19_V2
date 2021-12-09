non_countries <- c("Gibraltar", "Isle of Man", "Jersey", "Guernsey", "England", "Scotland", "Wales", "Northern Ireland", "European Union", "World")

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
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank()
    ) +
    geom_line(data = d, aes(x=date, y=perc, group=location, colour=location), size=0.3) +
    geom_text_repel(data = d_last, aes(x=date, y=perc, colour=location, label=location), size=2.5, nudge_x = 1, direction = "y", hjust = "left", segment.colour="grey90", segment.size=0.3, segment.curvature=0) +
    geom_point(data = d_last, aes(x=date, y=perc, colour=location), size=0.8) +
    scale_colour_manual(values=tableau_10_palette) +
    scale_x_date(expand=expansion(mult=c(0.03, 0.2))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(x=NULL, y="Percentage of people vaccinated", title=glue("Top {n.top} countries with highest vaccination proportion"), subtitle=sub)
}

# Summarise vaccinations per country
# Mark "old" that are not up to date by more than day.delta days.
vac_summary <- function(owd, day.delta) {
  owd %>%
    select(location, date, first = people_vaccinated_per_hundred, fully = people_fully_vaccinated_per_hundred) %>% 
    drop_na() %>% 
    pivot_longer(cols = c(first, fully)) %>% 
    group_by(location, name) %>%
    arrange(date) %>% 
    summarise(
      value = last(value),
      last_date = last(date)
    ) %>% 
    ungroup() %>% 
    mutate(old = max(last_date) - last_date > day.delta)
}



plot_owid_vac_first_fully <- function(owd, sel=NULL, rep.size=2.5, day.delta=3, trans="identity", brks=waiver()) {
  if(!is.null(sel)) owd <- owd %>% filter(location %in% sel)
  owd %>% 
    filter(!(location %in% non_countries)) %>% 
    vac_summary(day.delta = day.delta) %>% 
    pivot_wider(id_cols = c(location, old), names_from = name, values_from = value) %>% 
  ggplot(aes(x=first, y=fully, label=location, colour=old)) + 
    theme_bw() +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank()
    ) +
    geom_point() +
    geom_text_repel(size=rep.size, max.time=2, max.overlaps = 50, max.iter = 100000) +
    labs(x="Percentage with first dose", y="Percentage fully vaccinated") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.03)), limits=c(0, NA), trans=trans, breaks=brks) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.03)), limits=c(0, NA), trans=trans, breaks=brks) +
    scale_colour_manual(values=c("black", "grey70"))
}


anim_owid_vac_fully_race <- function(owd, sel=NULL) {
  if(!is.null(sel)) owd <- owd %>% filter(location %in% sel)
  owdg <- owd %>% 
    filter(!(location %in% non_countries) & date >= as.Date("2021-01-01")) %>%
    select(location, date, `First vaccine`=people_vaccinated_per_hundred, `Second vaccine`=people_fully_vaccinated_per_hundred) %>% 
    drop_na() %>% 
    pivot_longer(-c(location, date), names_to="vaccine") %>% 
    mutate(vaccine = fct_rev(vaccine)) %>% 
    filter(value > 0) %>% 
    mutate(idate = as.integer(date)) %>% 
    group_by(location, vaccine) %>% 
    mutate(ys = predict(loess(value ~ idate, span=0.3))) %>% 
    mutate(ys = if_else(ys < 0, 0, ys)) %>% 
    ungroup() %>% 
    group_by(date, vaccine) %>% 
    arrange(date, -ys) %>% 
    mutate(rank = 1:n()) %>% 
    filter(rank <= 12) %>% 
    ungroup()
  my_theme <- theme_bw() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(), 
      axis.line.y = element_blank(),
      legend.position = "none",
      panel.grid = element_blank()
    )
  owdg %>% 
    ggplot() +
    facet_grid(.~vaccine) +
    aes(xmin=0, xmax = ys, ymin=rank-0.45, ymax=rank+0.45, y=rank, group=location) +
    geom_rect(aes(fill=location), alpha = 0.7) +
    scale_fill_viridis_d(option="magma", direction=-1) +
    geom_text(aes(label = location), hjust="right", colour="grey13", x=-3) +
    scale_x_continuous(limits = c(-40, 100), breaks=c(seq(0,100,25))) +
    scale_y_reverse() +
    labs(x="Percentage vaccinated", y=NULL, title="{frame_time}") +
    my_theme +
    transition_time(date)
}