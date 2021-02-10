plan_figures <- function() {
  
  ecdc_figures <- drake_plan(
    fig_ecdc_cases_col = plot_countries_col(ecdc, sel1, what="cases") %>% sz(8, 6, time_stamp_ecdc),
    fig_ecdc_cases_top = plot_countries_col(ecdc, top_recent_cases, what="cases", title="Top 12 countries with highest recent case rate", subtitle="(only countries with population over 1 million included)") %>% sz(8, 6, time_stamp_ecdc),
    fig_ecdc_deaths_col = plot_countries_col(ecdc, sel1, what="deaths") %>% sz(8, 6, time_stamp_ecdc),
    fig_ecdc_deaths_top = plot_countries_col(ecdc, top_recent_deaths, what="deaths", title="Top 12 countries with highest recent death rate", subtitle="(only countries with population over 1 million included)") %>% sz(8, 6, time_stamp_ecdc),
    fig_ecdc_hysteresis = plot_countries_hysteresis(ecdc, sel1) %>% sz(8, 6, time_stamp_ecdc),
    #fig_ecdc_heatmap = plot_heatmap_clust(ecdc, mx.limit=100) %>% sz(8, 10, time_stamp_ecdc),
    fig_ecdc_cases_all = plot_countries_line(ecdc, what="cases", n.col=18) %>% sz(26, 16, time_stamp_ecdc),
    fig_ecdc_cases_deaths_all = plot_cases_diff_deaths(ecdc, pop=TRUE) %>% sz(8, 20, time_stamp_ecdc),
    fig_ecdc_cases_deaths_pop = plot_cases_deaths_pop(ecdc) %>% sz(10, 8, time_stamp_ecdc),
    fig_ecdc_death_tracks = plot_cases_deaths_track(ecdc, c(EU, "United Kingdom"), title="Second wave tracks for UK and EU") %>% sz(7, 6, time_stamp_ecdc)
  )
  
  excess_figures <- drake_plan(
    fig_excess_uk = plot_excess_details(ft, "UK", by.region=TRUE, ncol=4, y.scale=4) %>% sz(7, 7, time_stamp_ft),
    fig_excess_prop_uk = plot_excess_prop(ft, "UK", by.region=TRUE, ncol=4) %>% sz(8, 8, time_stamp_ft),
    fig_excess_uk_annual = plot_exc_total_deaths(ft, "UK") %>% sz(7, 5, time_stamp_ft),
    fig_excess_uk_weekly = plot_exc_weekly_deaths(ft, "UK") %>% sz(6, 4, time_stamp_ft),
    fig_excess_uk_nations = plot_uk_nation_excess(uk_exc_nations) %>% sz(5, 5, time_stamp_ft)
  )
  
  gov_figures <- drake_plan(
    fig_gov_separate = plot_gov_weekly(gov) %>% sz(9, 8, time_stamp_gov),
    fig_gov_aggregated_sample = plot_admissions_cases_deaths(gov, by_publish_date = FALSE) %>% sz(7, 4, time_stamp_gov),
    fig_gov_aggregated_publish = plot_admissions_cases_deaths(gov, by_publish_date = TRUE) %>% sz(7, 4, time_stamp_gov),
    fig_gov_vaccinations = plot_vaccination(gov) %>% sz(6, 4, time_stamp_gov),
    fig_gov_vaccinations_target = plot_vaccination_target(gov) %>% sz(6, 4, time_stamp_gov),
    fig_gov_cum_deaths = plot_cum_deaths(gov) %>% sz(5, 4, time_stamp_gov),
    fig_gov_second_wave_prediction = plot_second_wave_prediction(gov, pred.date=as.Date("2021-02-01")) %>% sz(6, 4, time_stamp_gov)
  )
  
  owid_figures <- drake_plan(
    fig_owid_vaccination_first = plot_owid_vaccination(owid_vac, n.top=10, what="people_vaccinated_per_hundred", sub="people receiving first dose") %>% sz(6, 5, time_stamp_owid_vac),
    fig_owid_vaccination_full = plot_owid_vaccination(owid_vac, n.top=10, what="people_fully_vaccinated_per_hundred", sub="people fully vaccinated") %>% sz(6, 5, time_stamp_owid_vac)
  )
  
  animations <- drake_plan(
    anim_ecdc_death_tracks = animate_cases_deaths(ecdc, c(EU, "United Kingdom")),
    save_anim_death_tracks = animate(anim_ecdc_death_tracks, renderer = gifski_renderer(file="fig/ecdc_anim_death_tracks.gif"), nframes=600, end_pause = 60)
  )
  
  fig_plan <- figs_from_plan(bind_rows(ecdc_figures, excess_figures, gov_figures, owid_figures))
  
  save_figures <- drake_plan(
    png_figures = target(
      command = annotate_save(filename, obj),
      transform = map(.data = !!fig_plan, .id=eval(obj))
    )
  )
  
  bind_rows(
    ecdc_figures,
    excess_figures,
    gov_figures,
    owid_figures,
    animations,
    save_figures
  )
  
}