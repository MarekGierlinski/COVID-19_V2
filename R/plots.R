if(!dir.exists("fig")) dir.create("fig")

shapes <- c(15:18, 0:14)
okabe_ito_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "grey20", "grey40", "grey60", "grey80", "grey90", "black")
wd <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

ukPalette <- c(
  England = rgb(255, 90, 90, max=255),
  Scotland = rgb(110, 110, 255, max=255),
  Wales = rgb(255, 214, 0, max=255),
  `Northern Ireland` = rgb(20, 185, 90, max=255)
)


sz <- function(gg, width, height, urlc){
  gg$width <- width
  gg$height <- height
  gg$source <- urlc
  gg
}

figs_from_plan <- function(plan) {
  plan %>% 
    select(-command, name = target) %>% 
    mutate(
      obj = rlang::syms(name),
      filename = paste0("fig/", str_remove(name, "fig_"), ".png"),
      width = obj$width,
      height = obj$height,
      urlc = obj$urlc
    ) 
}


annotate_save <- function(filename, g) {
  ann <- ggplot() +
    xlim(0, 1) +
    annotate("text", label=glue::glue("Source: {g$source}"), hjust=0, x=0, y=0, size=2, colour="grey50") +
    theme_nothing() +
    theme(plot.margin = unit(c(-5,0,-5,-5), "mm"))
  plt <- plot_grid(g ,ann, ncol=1, rel_heights = c(20, 1))
  ggsave(filename, plt, device="png", width=g$width, height=g$height)
}



