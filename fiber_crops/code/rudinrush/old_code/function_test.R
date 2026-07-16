# Side-by-side state/period comparison map template
#
# Usage:
#   p <- plot_state_period_comparison(
#     data = ctn_data_unq,
#     shapes = county_shapes,
#     metric = "ttl_acres_plntd",
#     states = c("Iowa", "Nebraska"),
#     periods = list(1990:2000, 2011:2019),
#     period_labels = c("1990-2000", "2011-2019")
#   )
#   print(p)

plot_state_period_comparison_2 <- function(
    data,
    shapes,
    metric,
    states,
    periods,
    period_labels = NULL,
    state_col = "state_name",
    year_col = "year",
    fips_col = "area_fips"
) {
  required_packages <- c("dplyr", "ggplot2", "sf", "tigris")
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  
  if (length(missing_packages) > 0) {
    stop(
      "Missing required packages: ",
      paste(missing_packages, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (length(periods) != 2) {
    stop("`periods` must contain exactly two time periods.", call. = FALSE)
  }
  
  if (is.null(period_labels)) {
    period_labels <- c("Period 1", "Period 2")
  }
  
  if (length(period_labels) != length(periods)) {
    stop("`period_labels` must match the length of `periods`.", call. = FALSE)
  }
  
  state_strip_labels <- paste0("State: ", states)
  period_strip_labels <- paste0("Period: ", period_labels)
  
  state_lookup <- dplyr::distinct(
    as.data.frame(tigris::fips_codes)[, c("state_code", "state_name")]
  )
  
  shapes_local <- shapes |>
    dplyr::mutate(state_code = substr(.data[[fips_col]], 1, 2)) |>
    dplyr::left_join(state_lookup, by = "state_alpha") |>
    dplyr::filter(.data[[state_col]] %in% states) |>
    dplyr::mutate(
      state_panel = factor(
        .data[[state_col]],
        levels = states,
        labels = state_strip_labels
      )
    )
  
  period_lookup <- dplyr::bind_rows(
    dplyr::tibble(
      !!year_col := periods[[1]],
      period_label = period_labels[[1]]
    ),
    dplyr::tibble(
      !!year_col := periods[[2]],
      period_label = period_labels[[2]]
    )
  )
  
  plot_data <- data |>
    dplyr::filter(
      .data[[state_col]] %in% states,
      .data[[year_col]] %in% unlist(periods)
    ) |>
    dplyr::left_join(period_lookup, by = year_col) |>
    dplyr::group_by(.data[[fips_col]], .data[[state_col]], period_label) |>
    dplyr::summarise(
      value = mean(.data[[metric]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      state_panel = factor(
        .data[[state_col]],
        levels = states,
        labels = state_strip_labels
      ),
      period_panel = factor(
        period_label,
        levels = period_labels,
        labels = period_strip_labels
      )
    ) |>
    dplyr::left_join(
      dplyr::select(
        shapes_local,
        dplyr::all_of(c(fips_col, state_col, "state_panel")),
        geometry
      ),
      by = c(fips_col, state_col)
    ) |>
    sf::st_as_sf()
  
  ggplot2::ggplot(plot_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = value), color = NA) +
    ggplot2::facet_grid(period_panel ~ state_panel) +
    ggplot2::scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "grey85") +
    ggplot2::labs(
      title = paste0("Side-by-side comparison of ", metric),
      subtitle = paste(period_labels, collapse = " vs "),
      fill = metric
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
}
