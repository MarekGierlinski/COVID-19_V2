if(!dir.exists("fig")) dir.create("fig")

shapes <- c(15:18, 0:14)
okabe_ito_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "grey20", "grey40", "grey60", "grey80", "grey90", "black")

wd <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

uk_palette <- c(
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
    annotate("text", label=g$source, hjust=0, x=0, y=0, size=2, colour="grey50") +
    theme_nothing() +
    theme(plot.margin = unit(c(-5,0,-5,-5), "mm"))
  plt <- plot_grid(g ,ann, ncol=1, rel_heights = c(20, 1))
  ggsave(filename, plt, device="png", width=g$width, height=g$height)
}



ggheatmap <- function(tab, order.col = TRUE, order.row = TRUE, dendro.line.size = 0.5, 
                      text.size = 12, legend.text.size=12, legend.name = "value",
                      dist.method = "euclidean", clust.method = "complete", title=NULL,
                      with.y.text=FALSE, with.x.text=TRUE, palette="distiller") {
  
  d <- tab %>% 
    as_tibble(rownames = "rowname") %>% 
    mutate(rowname = factor(rowname, levels=rownames(tab))) %>%
    pivot_longer(-rowname, names_to = "variable", values_to = "value") %>% 
    mutate(variable = as.Date(variable)) %>% 
    mutate(value = as.numeric(value))
  
  dd <- function(X) {
    X %>% dist(method = dist.method) %>% hclust(method = clust.method) %>% as.dendrogram()
  }
  
  # Cluster rows
  if (order.row) {
    dd.row <- dd(tab)
    row.ord <- order.dendrogram(dd.row)
    ordered_row_names <- row.names(tab[row.ord, ])
    d$rowname <- factor(d$rowname, levels = ordered_row_names)
  }
  
  # Cluster columns
  if (order.col) {
    dd.col <- dd(t(tab))
    col.ord <- order.dendrogram(dd.col)
    ordered_col_names <- colnames(tab[, col.ord])
    d$variable <- factor(d$variable, levels = ordered_col_names)
  }
  
  eley <- ifelse(with.y.text, element_text(), element_blank())
  
  mx <- max(abs(d$value), na.rm=TRUE)
  heat_plot <- ggplot(d, aes(x = variable, y = rowname, fill = value)) + 
    geom_tile(width=7) + 
    theme_classic() +
    theme(
      axis.line = element_blank(),
      text = element_text(size = text.size)
    ) + 
    scale_y_discrete(position = "right", expand=c(0,0)) + 
    #scale_x_discrete(expand = c(0,0)) +
    scale_x_date(expand = c(0,0)) +
    labs(x=NULL, y=NULL, title=title)

  if(palette == "viridis") {
    heat_plot <- heat_plot + scale_fill_viridis_c(option="cividis", name = legend.name)
  } else {
    heat_plot <- heat_plot + scale_fill_distiller(type="div", palette="RdBu", limits=c(-mx, mx))
  }
  
  if(with.x.text) {
    heat_plot <- heat_plot + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  } else {
    heat_plot <- heat_plot + theme(axis.text.x = element_blank())
  }
  
  if(with.y.text) {
    heat_plot <- heat_plot + theme(axis.text.y = element_text(size=8))
  } else {
    heat_plot <- heat_plot + theme(axis.text.y = element_blank())
  }
  
  final_plot <- heat_plot
  if (order.row) {
    dendro_data_row <- ggdendro::dendro_data(dd.row, type = "rectangle")
    dendro_row <- axis_canvas(heat_plot, axis = "y", coord_flip = TRUE) + 
      geom_segment(data = ggdendro::segment(dendro_data_row), aes(y = -y, x = x, xend = xend, yend = -yend), size = dendro.line.size) + 
      coord_flip()
    final_plot <- insert_yaxis_grob(final_plot, dendro_row, grid::unit(0.2, "null"), position = "left")
  }
  
  if (order.col) {
    dendro_data_col <- ggdendro::dendro_data(dd.col, type = "rectangle")
    dendro_col <- axis_canvas(heat_plot, axis = "x") + 
      geom_segment(data = ggdendro::segment(dendro_data_col), aes(x = x, y = y, xend = xend, yend = yend), size = dendro.line.size)
    final_plot <- insert_xaxis_grob(final_plot, dendro_col, grid::unit(0.2, "null"), position = "top")
  }
  
  ggdraw(final_plot)
}


