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
  
  bind_rows(
    ecdc_data,
    ft_data
  )
}