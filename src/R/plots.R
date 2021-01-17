library(tidyverse)
library(plot3D)

scatter_plot_2d = function(dfx, x, y, z) {
  xlabel = rlang::enquo(x)
  ylabel = rlang::enquo(y)
  dfx %>%
    ggplot2::ggplot(
      aes(x = {{ x }}, y = {{ y }}, col = {{ z}})
    ) + 
    ggplot2::geom_point() +
    ggplot2::scale_color_gradient(
      low = "yellow",
      high = "red"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 15, face = "bold"),
      axis.text = ggplot2::element_text(size = 15, face = "bold"),
      legend.text = ggplot2::element_text(size = 15, face = "bold"),
      legend.title = ggplot2::element_text(size = 15, face = "bold")
    ) +
  xlab(glue::glue("Pressure {rlang::as_label(xlabel)}")) +
  ylab(glue::glue("Pressure {rlang::as_label(ylabel)}")) +
  xlim(0, 60) +
  ylim(0, 60)
}


scatter_plot_3d = function(dfx, x, y, z) {
  x_ = dfx %>% dplyr::pull({{x}})
  y_ = dfx %>% dplyr::pull({{y}})
  z_ = dfx %>% dplyr::pull({{z}})
  plot3D::scatter3D(
    x = x_,
    y = y_,
    z = z_,
    theta = 50,
    phi = 10,
    colkey = F,
    colvar = NULL,
    xlab = "Pressure P1",
    ylab = "Pressure P2",
    zlab = "Depth",
    ticktype = "detailed",
    d = 2,
    cex = 1.5
  )
}


pdepth_plot = function(dfx, pressure, depth) {
  dfx %>%
    ggplot2::ggplot(aes(
      x = {{ depth }},
      y = {{ pressure }}
    )) +
    ggplot2::geom_line() +
    ggplot2::scale_y_reverse() +
    xlab("Depth") +
    ylab("Pressure")
}

box_plot = function(dfx) {
  melted_df = tibble::tibble(reshape::melt(data.frame(dfx)))
  melted_df %>%
    ggplot2::ggplot(aes(x = variable, y = value)) + 
    ggplot2::geom_boxplot() +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 15, face = "bold"),
      axis.text = ggplot2::element_text(size = 15, face = "bold"),
      legend.text = ggplot2::element_text(size = 15, face = "bold"),
      legend.title = ggplot2::element_text(size = 15, face = "bold")
    ) + 
    xlab("") + 
    ylab("")
}

histogram_plot = function(dfx) {
  melted_df = tibble::tibble(reshape::melt(data.frame(dfx)))
  melted_df %>%
    ggplot2::ggplot(aes(x = value)) + 
    ggplot2::geom_histogram(aes(y = ..density..), color = "black", fill = "white") +
    ggplot2::geom_density(linetype = "dashed", color = "red") +
    ggplot2::facet_wrap( ~ variable) + 
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 15, face = "bold"),
      axis.text = ggplot2::element_text(size = 15, face = "bold"),
      legend.text = ggplot2::element_text(size = 15, face = "bold"),
      legend.title = ggplot2::element_text(size = 15, face = "bold")
    ) + 
    xlab("") + 
    ylab("")
}
