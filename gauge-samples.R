library(plotly)

p <- plot_ly(
  type = "pie",
  values = c(40, 10, 10, 10, 10, 10, 10),
  labels = c("-", "0", "20", "40", "60", "80", "100"),
  rotation = 108,
  direction = "clockwise",
  hole = 0.4,
  textinfo = "label",
  textposition = "outside",
  hoverinfo = "none",
  domain = list(x = c(0, 0.48), y = c(0, 1)),
  marker = list(colors = c('rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)')),
  showlegend = FALSE
)

p
base_plot <- add_trace(
  p,
  type = "pie",
  values = c(50, 10, 10, 10, 10, 10),
  labels = c("Error Log Level Meter", "Debug", "Info", "Warn", "Error", "Fatal"),
  rotation = 90,
  direction = "clockwise",
  hole = 0.3,
  textinfo = "label",
  textposition = "inside",
  hoverinfo = "none",
  domain = list(x = c(0, 0.48), y = c(0, 1)),
  marker = list(colors = c('rgb(255, 255, 255)', 'rgb(232,226,202)', 'rgb(226,210,172)', 'rgb(223,189,139)', 'rgb(223,162,103)', 'rgb(226,126,64)')),
  showlegend= FALSE
)
base_plot