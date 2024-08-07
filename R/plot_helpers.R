get_pie_plot <- function(plot_dat, breakdowntype){

  location_levels <- c(
    sort(unique(plot_dat$location[grepl("^influent", plot_dat$location)])),
    sort(unique(plot_dat$location[grepl("^effluent", plot_dat$location)]))
  )

  # Convert location to a factor with the custom levels
  plot_dat$location <- factor(plot_dat$location, levels = location_levels)

  event_labeller <- function(event_val){
    return(paste("Event:", event_val))
  }

  create_plot_for_event <- function(event_data, event_name) {
    ggplot(event_data, aes(x = factor(1), y = percentage, fill = as.factor(!!sym(breakdowntype)))) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      facet_wrap(~ location, labeller = labeller(location = label_both)) +
      geom_text(aes(label = paste0(round(percentage, 1), "%")),
                position = position_stack(vjust = 0.5),
                show.legend = FALSE) +
      theme_void() +
      theme(
        legend.position = "right",
        legend.text = element_text(size = 20),
        text = element_text(size = 24)
      ) +
      labs(fill = breakdowntype, title = paste("Event:", event_name))
  }

  # Split data by event
  event_data_list <- split(plot_dat, plot_dat$event)

  # Generate plots for each event
  plot_list <- lapply(names(event_data_list), function(event_name) {
    create_plot_for_event(event_data_list[[event_name]], event_name)
  })

  # Arrange plots in a single column using ggarrange
  final_plot <- ggpubr::ggarrange(plotlist = plot_list, ncol = 1, nrow = length(plot_list), align = 'v')

  final_plot

}

get_concentration_plot <- function(plot_dat, bmpselect, yearselect, sizefractionselect, replicateselect){
  COLOR_PALETTE <- c(`1`="#0000FF0A", `2`="#00008B", `3` = "#FFC0CB", `4` = "#8B0000")

  # Ensure the locations are in the right order
  location_levels <- c(
    sort(unique(plot_dat$location[grepl("^influent", plot_dat$location)])),
    sort(unique(plot_dat$location[grepl("^effluent", plot_dat$location)]))
  )

  plot_dat$location <- factor(plot_dat$location, levels = location_levels)

  dodge <- position_dodge(width = 0.7)
  y_lim <- max(plot_dat$total_concentration) + (max(plot_dat$total_concentration) * 0.1)

  p <- ggplot(plot_dat, aes(x = location, y = total_concentration, group = event, fill = as.factor(event))) +
    geom_bar(stat = "identity", position = dodge, width = 0.5, color = "black") +
    geom_text(aes(label = round(total_concentration, 1)),
              position = dodge,
              vjust = -0.5,
              size = 6,
              show.legend = FALSE) +
    ylim(0, y_lim) +
    scale_fill_manual(values = COLOR_PALETTE) +
    labs(title = glue("{bmpselect}-Y{yearselect}-SF{sizefractionselect}-Rep{replicateselect}"),
         x = "Location",
         y = "Concentration (P/L)",
         fill = "Event") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right",
      legend.text = element_text(size = 20),
      text = element_text(size = 30)
    )
  p

}


