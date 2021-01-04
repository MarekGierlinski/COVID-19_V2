plan_data <- function() {
  
  ecdc_data <- drake_plan(
    url_ecdc = get_ecdc_url(),
    ecdc_raw = fetch_ecdc_data(url_ecdc),
    ecdc = process_ecdc_data(ecdc_raw)
  )
  
  ft_data <- drake_plan(
    url_ft = get_ft_url(),
    ft_raw = fetch_ft_data(url_ft),
    ft = process_ft_data(ft_raw),
    uk_exc_nations = UK_nation_excess(ft)
  )
  
  gov_data <- drake_plan(
    url_gov = get_gov_url_v2(),
    gov_raw = fetch_gov_data(url_gov),
    gov = process_gov_data(gov_raw)
  )
  
  bind_rows(
    ecdc_data,
    ft_data,
    gov_data
  )
}