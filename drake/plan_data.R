plan_data <- function() {
  
  ecdc_data <- drake_plan(
    url_ecdc = get_ecdc_url(),
    ecdc_raw = fetch_ecdc_data(url_ecdc),
    ecdc = process_ecdc_data(ecdc_raw),
    time_stamp_ecdc = get_time_stamp(ecdc_raw, url_ecdc),
    top_recent_cases = select_top_recent(ecdc, "cases", n=12),
    top_recent_deaths = select_top_recent(ecdc, "deaths", n=12)
  )
  
  ft_data <- drake_plan(
    url_ft = get_ft_url(),
    ft_raw = fetch_ft_data(url_ft),
    ft = process_ft_data(ft_raw),
    uk_exc_nations = UK_nation_excess(ft),
    time_stamp_ft = get_time_stamp(ft_raw, url_ft)
  )
  
  gov_data <- drake_plan(
    url_gov = get_gov_url_v2(),
    gov_raw = fetch_gov_data(url_gov),
    gov = process_gov_data(gov_raw),
    time_stamp_gov = get_time_stamp(gov_raw, url_corona)
  )
  
  owid_data <- drake_plan(
    url_owid_exc = get_url_owid_excess(),
    url_owid_vac = get_url_owid_vaccinations(),
    owid_exc_raw = fetch_owid(url_owid_exc),
    owid_vac_raw = fetch_owid(url_owid_vac),
    time_stamp_owid_exc = get_time_stamp(owid_exc_raw, url_owid_exc),
    time_stamp_owid_vac = get_time_stamp(owid_vac_raw, url_owid_vac)
  )
  
  bind_rows(
    ecdc_data,
    ft_data,
    gov_data,
    owid_data
  )
}