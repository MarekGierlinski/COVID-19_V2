plan_figures <- function() {
  
  ecdc_figures <- drake_plan(
    fig_col_cases_sel1 = plot_countries_col(ecdc, sel1, what="cases") %>% sz(8, 6, url_ecdc),
    fig_hister_cases_sel1 = plot_countries_hysteresis(ecdc, sel1) %>% sz(8, 6, url_ecdc),
    fig_all_cases = plot_countries_line(ecdc, what="cases") %>% sz(20, 16, url_ecdc)
  )
  
  excess_figures <- drake_plan(
    fig_exc_uk = plot_excess_details(ft, "UK", by.region=TRUE, ncol=4, y.scale=4) %>% sz(7, 7, url_ft),
    fig_exc_prop_uk = plot_excess_prop(ft, "UK", by.region=TRUE, ncol=4) %>% sz(8, 8, url_ft),
    fig_exc_deaths_uk = plot_exc_total_deaths(ft, "UK") %>% sz(7, 5, url_ft),
    fig_exc_weekly_deaths_uk = plot_exc_weekly_deaths(ft, "UK") %>% sz(6, 4, url_ft),
    fig_exc_uk_nations = plot_uk_nation_excess(uk_exc_nations) %>% sz(5, 5, url_ft)
  )
  
  gov_figures <- drake_plan(
    fig_weekly_cases_uk = plot_gov_weekly(gov) %>% sz(9, 8, url_gov),
    fig_gov = plot_admissions_cases_deaths(gov) %>% sz(7, 4, url_gov)
  )
  
  fig_plan <- figs_from_plan(bind_rows(ecdc_figures, excess_figures, gov_figures))
  
  save_figures <- drake_plan(
    pdf_figures = target(
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