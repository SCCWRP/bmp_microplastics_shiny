get_concentration_plot <- function(data_path, is_mp = FALSE){
  library(readr)
  library(ggplot2)
  plot_dat <- read_csv(data_path)

  COLOR_PALETTE <- c(`1` = "#0000FF", `2` = "#00FF00", `3` = "#FFC0CB", `4` = "#8B0000")
  ylabel <- 'Concentration (Particles/L)'

  plot_dat$location <- trimws(plot_dat$location)

  location_levels <- c(
    sort(unique(plot_dat$location[grepl("^(?i)(inlet|influent)", plot_dat$location)])),
    sort(unique(plot_dat$location[grepl("^(?i)(outlet|effluent)", plot_dat$location)]))
  )
  plot_dat$location <- factor(plot_dat$location, levels = location_levels)

  dodge <- position_dodge(width = 0.7)
  y_lim <- max(plot_dat$total_concentration, plot_dat$total_concentration_mp, na.rm = TRUE) * 1.1

  if (is_mp) {
    ylabel <- 'Concentration (MP Particles/L)'
    p <- ggplot(plot_dat, aes(x = location, y = total_concentration_mp, group = event, fill = as.factor(event))) +
      geom_bar(stat = "identity", position = dodge, width = 0.5, color = "black") +
      geom_text(aes(label = round(total_concentration_mp, 1)),
                position = dodge, vjust = -0.5, size = 6, color = "black", show.legend = FALSE) +
      scale_fill_manual(name = "Event", values = COLOR_PALETTE) +
      #guides(fill = guide_legend(override.aes = list(alpha = 0.3))) +
      ylim(0,  max(plot_dat$total_concentration_mp, na.rm = TRUE) * 1.1) +
      labs(x = "Location", y = ylabel)
  } else {
    # Format labels
    plot_dat$label_total <- as.character(round(plot_dat$total_concentration_mp, 1))
    plot_dat$label_mp <- paste0(" (", round(plot_dat$total_concentration, 1), ")")

    # Factor to control label color order
    plot_dat$label_type_total <- factor("MP Concentration",
                                        levels = c( "MP Concentration", "Total Concentration"))
    plot_dat$label_type_mp <- factor("Total Concentration",
                                     levels = c("MP Concentration", "Total Concentration"))

    text_colors <- c(
      "MP Concentration" = "black",
      "Total Concentration" = "red"
    )

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
        data = data.frame(legend = factor(c("MP Concentration","Total Concentration"),
                                          levels = c("MP Concentration", "Total Concentration"))),
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

  }

  return(p)
}

data_path <- "C:/Users/duyn/Downloads/concentration-data-2025-04-25.csv"
only_mp <- TRUE
p <- get_concentration_plot(data_path, only_mp)



