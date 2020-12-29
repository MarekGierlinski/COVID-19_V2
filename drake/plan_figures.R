plan_figures <- function() {
  
  ecdc_figures <- drake_plan(
    fig_ecdc_cases_col = plot_countries_col(ecdc, sel1, what="cases") %>% sz(8, 6, url_ecdc),
    fig_ecdc_cases_ridge = plot_countries_ridge(ecdc, sel1, "cases", scl=0.002) %>% sz(6, 8, url_ecdc),
    fig_ecdc_deaths_col = plot_countries_col(ecdc, sel1, what="deaths") %>% sz(8, 6, url_ecdc),
    fig_ecdc_deaths_ridge = plot_countries_ridge(ecdc, sel1, "deaths", scl=0.1) %>% sz(6, 8, url_ecdc),
    fig_ecdc_histeresis = plot_countries_hysteresis(ecdc, sel1) %>% sz(8, 6, url_ecdc),
    
    fig_ecdc_cases_all = plot_countries_line(ecdc, what="cases", n.col=18) %>% sz(26, 16, url_ecdc)
  )
  
  excess_figures <- drake_plan(
    fig_excess_uk = plot_excess_details(ft, "UK", by.region=TRUE, ncol=4, y.scale=4) %>% sz(7, 7, url_ft),
    fig_excess_prop_uk = plot_excess_prop(ft, "UK", by.region=TRUE, ncol=4) %>% sz(8, 8, url_ft),
    fig_excess_uk_annual = plot_exc_total_deaths(ft, "UK") %>% sz(7, 5, url_ft),
    fig_excess_uk_weekly = plot_exc_weekly_deaths(ft, "UK") %>% sz(6, 4, url_ft),
    fig_excess_uk_nations = plot_uk_nation_excess(uk_exc_nations) %>% sz(5, 5, url_ft)
  )
  
  gov_figures <- drake_plan(
    fig_gov_separate = plot_gov_weekly(gov) %>% sz(9, 8, url_gov),
    fig_gov_aggregated = plot_admissions_cases_deaths(gov) %>% sz(7, 4, url_gov)
  )
  
  fig_plan <- figs_from_plan(bind_rows(ecdc_figures, excess_figures, gov_figures))
  
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
    save_figures
  )
  
}