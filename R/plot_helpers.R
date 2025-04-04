#' Get Pie Plot
#'
#' Creates pie plots based on the breakdown type for each event and location. The plots are arranged in a single column.
#'
#' @param plot_dat A data frame containing the plot data with percentage values for each category.
#' @param breakdowntype A string representing the type of breakdown to display in the pie chart.
#'
#' @return A ggplot object containing the arranged pie plots for each event.
#' @noRd
get_stacked_bar_plot <- function(plot_dat, breakdowntype){

  # Set custom levels for location
  location_levels <- c(
    sort(unique(plot_dat$location[grepl("^influent", plot_dat$location)])),
    sort(unique(plot_dat$location[grepl("^effluent", plot_dat$location)]))
  )
  plot_dat$location <- factor(plot_dat$location, levels = location_levels)

  # Create a dedicated factor column for the breakdown variable.
  plot_dat$category <- as.factor(plot_dat[[breakdowntype]])

  library(dplyr)
  # Summarize for custom legend labels (without numeric values)
  legend_summary <- plot_dat %>%
    group_by(category) %>%
    summarise(dummy = 1, .groups = 'drop')

  if(breakdowntype == "size_fraction"){
    legend_labels <- paste0(as.character(legend_summary$category), "µm")
  } else {
    legend_labels <- as.character(legend_summary$category)
  }
  names(legend_labels) <- legend_summary$category

  if (nrow(plot_dat) > 0) {
    final_plot <- ggplot(plot_dat, aes(x = location, y = percentage, fill = category)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ event, labeller = labeller(event = function(x) paste("Event", x))) +
      labs(fill = breakdowntype) +
      scale_fill_discrete(labels = legend_labels) +
      scale_y_continuous(breaks = seq(0, 100, by = 10))
    final_plot
  } else {
    final_plot <- ggplot()
  }

  final_plot
}



#' Get Concentration Plot
#'
#' Creates a concentration bar plot based on selected BMP, year, size fraction, and replicate. The plot shows total concentration by location and event.
#'
#' @param plot_dat A data frame containing the plot data with total concentration values.
#' @param bmpselect A string representing the selected BMP.
#' @param yearselect A string representing the selected year.
#' @param sizefractionselect A string representing the selected size fraction.
#' @param replicateselect A string representing the selected replicate.
#'
#' @return A ggplot object containing the concentration bar plot.
#' @noRd
get_concentration_plot <- function(plot_dat, bmpselect, yearselect, sizefractionselect, replicateselect){

  COLOR_PALETTE <- c(`1`="#0000FF0A", `2`="#00008B", `3` = "#FFC0CB", `4` = "#8B0000")

  ylabel <- 'Concentration (MP/L)'

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
    labs(x = "Location",
         y = ylabel,
         fill = "Event")
  p

}
