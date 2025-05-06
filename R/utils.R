get_stacked_bar_plot_top <- function(path, breakdowntype, is_mp) {
  plot_dat <- read.csv(path)
  # Set custom levels for location
  plot_dat$location <- trimws(plot_dat$location)

  location_levels <- c(
    sort(unique(plot_dat$location[grepl("^(?i)(inlet|influent)", plot_dat$location)])),
    sort(unique(plot_dat$location[grepl("^(?i)(outlet|effluent)", plot_dat$location)]))
  )
  plot_dat$location <- factor(plot_dat$location, levels = location_levels)

  # Create a dedicated factor column for the breakdown variable
  plot_dat$category <- as.factor(plot_dat[[breakdowntype]])

  if (breakdowntype == "size_fraction") {
    size_levels <- c("20", "63", "125", "355", "500")
    plot_dat$category <- factor(gsub("µm", "", plot_dat$category), levels = size_levels)
    legend_labels <- setNames(paste0(size_levels, "µm"), size_levels)
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
  } else if (breakdowntype == "color") {
    color_vals <- as.character(plot_dat$category)
    valid_colors <- grDevices::colors()
    is_hex <- grepl("^#(?:[0-9a-fA-F]{3}){1,2}$", color_vals)
    is_named <- color_vals %in% valid_colors
    valid <- is_named | is_hex

    # Assign fallback colors for invalid values
    unique_vals <- unique(color_vals)
    fallback_colors <- c(
      "multicolor" = "#00FFFF",  # Purple
      "clear"      = "#00FFFF",  # Dark Gray
      "unknown"    = "#00FFFF"   # Chocolate
    )

    # Build color mapping
    color_mapping <- sapply(unique_vals, function(val) {
      if (val %in% valid_colors || grepl("^#(?:[0-9a-fA-F]{3}){1,2}$", val)) {
        val
      } else if (val %in% names(fallback_colors)) {
        fallback_colors[[val]]
      } else {
        fallback_colors[["unknown"]]
      }
    })
    names(color_mapping) <- unique_vals

    fill_scale <- scale_fill_manual(values = color_mapping)

  } else {
    legend_summary <- plot_dat %>%
      group_by(category) %>%
      summarise(dummy = 1, .groups = 'drop')
    legend_labels <- as.character(legend_summary$category)
    names(legend_labels) <- legend_summary$category
    fill_scale <- scale_fill_discrete(labels = legend_labels)
  }

  if (nrow(plot_dat) > 0) {
    final_plot <- ggplot(plot_dat, aes(
      x = location,
      y = percentage,
      fill = category
    )) +
      geom_bar(stat = "identity", width = 0.3) +
      facet_wrap(~ event, labeller = labeller(event = function(x) paste("Event", x)), nrow = 1) +
      labs(fill = breakdowntype, y = "Percentage of Particle per Category (%)", x = "Location") +
      fill_scale +
      scale_y_continuous(breaks = seq(0, 100, by = 10))
  } else {
    final_plot <- ggplot()
  }

  final_plot
}

get_stacked_bar_bottom <- function(path, breakdowntype, is_mp) {

  plot_dat <- read.csv(path)

  if (breakdowntype != "size_fraction") {
    out <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = paste("Concentration plot per category cannot be calculated for", breakdowntype,". You can still download the particle count data."), size = 5, hjust = 0.5) +
      theme_void()
    return (out)
  }

  plot_dat <- plot_dat %>% filter(
    location != 'not applicable'
  )

  plot_dat$location <- trimws(plot_dat$location)

  location_levels <- c(
    sort(unique(plot_dat$location[grepl("^(?i)(inlet|influent)", plot_dat$location)])),
    sort(unique(plot_dat$location[grepl("^(?i)(outlet|effluent)", plot_dat$location)]))
  )

  plot_dat$location <- factor(plot_dat$location, levels = location_levels)

  plot_dat$category <- as.factor(plot_dat[[breakdowntype]])


  size_levels <- c("20", "63", "125", "355", "500")
  plot_dat$category <- factor(gsub("µm", "", plot_dat$category), levels = size_levels)
  legend_labels <- setNames(paste0(size_levels, "µm"), size_levels)
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

  # Filter out infinite y-values
  if (is_mp) {
    plot_dat <- plot_dat %>% filter(is.finite(concentration_mp))
  } else {
    plot_dat <- plot_dat %>% filter(is.finite(concentration_all))
  }

  if (nrow(plot_dat) > 0) {
    if (is_mp){
      ylabel <- 'Concentration (MP Particles/L)'
      ymax <- max(plot_dat$concentration_mp, na.rm = TRUE) * 1.1
      final_plot <- ggplot(plot_dat, aes(x = location, y = concentration_mp, fill = category))
    } else {
      ylabel <- 'Concentration (Particles/L)'
      ymax <- max(plot_dat$concentration_all, na.rm = TRUE) * 1.1
      final_plot <- ggplot(plot_dat, aes(x = location, y = concentration_all, fill = category))
    }
    final_plot <- final_plot +
      geom_bar(stat = "identity", width = 0.3) +
      facet_wrap(~ event, labeller = labeller(event = function(x) paste("Event", x)), nrow = 1) +
      labs(fill = breakdowntype, y = ylabel, x = 'Location') +
      fill_scale
  } else {
    final_plot <- ggplot()
  }

  final_plot
}

# # Run this block for top
# path <- 'C:/Users/duyn/Downloads/pieplot-data-2025-05-06 (3).csv'
# breakdowntype <- 'size_fraction'
# is_mp <- TRUE
# top_plot <- get_stacked_bar_plot_top(path, breakdowntype, is_mp)
# print(top_plot)
#
# # Run this block for bottom
# path <- 'C:/Users/duyn/Downloads/pieplot-data-2025-05-06 (3).csv'
# breakdowntype <- 'size_fraction'
# is_mp <- TRUE
# bottm_plot <- get_stacked_bar_plot_top(path, breakdowntype, is_mp)
# print(bottm_plot)


