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

  # Create a dedicated factor column for the breakdown variable
  plot_dat$category <- as.factor(plot_dat[[breakdowntype]])

  # Apply ordering and blue color scale if size_fraction
  if (breakdowntype == "size_fraction") {
    # Force order and remove µm if present
    size_levels <- c("20", "63", "125", "355", "500")
    plot_dat$category <- factor(gsub("µm", "", plot_dat$category), levels = size_levels)

    # Define legend labels with µm suffix
    legend_labels <- setNames(paste0(size_levels, "µm"), size_levels)

    # Create color palette from light to dark blue
    #blue_palette <- colorRampPalette(c("#e6f0ff", "#0000FF"))(length(size_levels))
    blue_palette <- c(
      "20"  = "#cce5ff",
      "63"  = "#66b2ff",
      "125" = "#3399FF",
      "355" = "#0066CC",
      "500" = "#0000FF"
    )
    fill_scale <- scale_fill_manual(
      values = setNames(blue_palette, size_levels),
      labels = legend_labels
    )
  } else {
    # Default behavior for other breakdowns
    legend_summary <- plot_dat %>%
      group_by(category) %>%
      summarise(dummy = 1, .groups = 'drop')

    legend_labels <- as.character(legend_summary$category)
    names(legend_labels) <- legend_summary$category

    fill_scale <- scale_fill_discrete(labels = legend_labels)
  }

  if (nrow(plot_dat) > 0) {
    final_plot <- ggplot(plot_dat, aes(x = location, y = percentage, fill = category)) +
      geom_bar(stat = "identity", width = 0.3) +
      facet_wrap(~ event, labeller = labeller(event = function(x) paste("Event", x)), nrow = 1) +
      labs(fill = breakdowntype) +
      fill_scale +
      scale_y_continuous(breaks = seq(0, 100, by = 10))
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
get_concentration_plot <- function(plot_dat, bmpselect, yearselect, sizefractionselect, replicateselect, is_mp = FALSE){

  COLOR_PALETTE <- c(`1` = "#0000FF", `2` = "#00FF00", `3` = "#FFC0CB", `4` = "#8B0000")
  ylabel <- 'Concentration (Particles/L)'

  location_levels <- c(
    sort(unique(plot_dat$location[grepl("^influent", plot_dat$location)])),
    sort(unique(plot_dat$location[grepl("^effluent", plot_dat$location)]))
  )
  plot_dat$location <- factor(plot_dat$location, levels = location_levels)

  dodge <- position_dodge(width = 0.7)
  y_lim <- max(plot_dat$total_concentration, plot_dat$total_concentration_mp, na.rm = TRUE) * 1.1

  if (is_mp) {
    # Format labels
    plot_dat$label_total <- as.character(round(plot_dat$total_concentration, 1))
    plot_dat$label_mp <- paste0(" (", round(plot_dat$total_concentration_mp, 1), ")")

    # Factor to control label color order
    plot_dat$label_type_total <- factor("All Particle Concentration",
                                        levels = c("All Particle Concentration", "MP Particle Concentration"))
    plot_dat$label_type_mp <- factor("MP Particle Concentration",
                                     levels = c("All Particle Concentration", "MP Particle Concentration"))

    text_colors <- c("All Particle Concentration" = "red",
                     "MP Particle Concentration" = "darkred")

    # Main plot with bars and text labels (turned off legend for text)
    p <- ggplot(plot_dat, aes(x = location, group = event)) +
      # Bars
      geom_bar(aes(y = total_concentration, fill = as.factor(event)),
               stat = "identity", position = dodge, width = 0.5,
               color = "black", alpha = 0.3) +
      geom_bar(aes(y = total_concentration_mp, fill = as.factor(event)),
               stat = "identity", position = dodge, width = 0.5,
               color = "black", show.legend = FALSE) +

      # Text labels (do not generate legend)
      geom_text(aes(label = label_total,
                    y = pmax(total_concentration, total_concentration_mp),
                    color = label_type_total),
                position = dodge, vjust = -0.5, hjust = 1, size = 5.5,
                show.legend = FALSE) +
      geom_text(aes(label = label_mp,
                    y = pmax(total_concentration, total_concentration_mp),
                    color = label_type_mp),
                position = dodge, vjust = -0.5, hjust = 0, size = 5.5,
                show.legend = FALSE) +

      # Dummy layer to create a legend with squares
      geom_point(
        data = data.frame(legend = factor(c("All Particle Concentration", "MP Particle Concentration"),
                                          levels = c("All Particle Concentration", "MP Particle Concentration"))),
        aes(x = Inf, y = Inf, color = legend),
        shape = 15, size = 0.001, #as small as possible
        inherit.aes = FALSE  # do not inherit global aes like group = event
      ) +

      scale_fill_manual(name = "Event", values = COLOR_PALETTE) +
      scale_color_manual(name = "", values = text_colors) +
      guides(
        fill = guide_legend(order = 1, override.aes = list(alpha = 0.3)),
        color = guide_legend(order = 2, override.aes = list(shape = 15, size = 6))
      ) +
      ylim(0, y_lim) +
      labs(x = "Location", y = ylabel)

  } else {
    # Alternative branch for non-MP
    p <- ggplot(plot_dat, aes(x = location, y = total_concentration, group = event, fill = as.factor(event))) +
      geom_bar(stat = "identity", position = dodge, width = 0.5, color = "black", alpha = 0.3) +
      geom_text(aes(label = round(total_concentration, 1)),
                position = dodge, vjust = -0.5, size = 6, color = "black", show.legend = FALSE) +
      scale_fill_manual(name = "Event", values = COLOR_PALETTE) +
      guides(fill = guide_legend(override.aes = list(alpha = 0.3))) +
      ylim(0, y_lim) +
      labs(x = "Location", y = ylabel)
  }

  return(p)
}






