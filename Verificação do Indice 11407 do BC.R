# Load required libraries
library(rbcb)
library(dplyr)
library(lubridate)
library(plotly)

# Download External Debt / GDP data
divida <- get_series(11407, as = "data.frame")

# Check if data was retrieved successfully
if (is.null(divida) || nrow(divida) == 0) {
  stop("Failed to retrieve data from BCB")
}

# Print column names to debug
cat("Column names in divida:", names(divida), "\n")

# Adjust column names and add indicator
# Assuming the value column is the one that isn't 'date'
value_col <- names(divida)[names(divida) != "date"]
if (length(value_col) == 0) {
  stop("No value column found in the data")
}

divida <- divida %>%
  rename(date = date, valor = !!value_col) %>%  # Dynamically rename the value column
  mutate(indicador = "Dívida Externa / PIB (%)",
         trimestre = paste0(year(date), "Q", quarter(date))) %>%
  arrange(date)

# --- Bar Chart Race ---
df_race <- divida %>%
  group_by(trimestre, indicador) %>%
  summarise(valor = mean(valor, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(valor))

p_race <- plot_ly(
  data = df_race,
  x = ~valor,
  y = ~reorder(indicador, valor),
  frame = ~trimestre,
  type = "bar",
  orientation = "h",
  text = ~paste0(round(valor, 2), "%"),
  textposition = "auto",
  marker = list(
    color = "steelblue",
    line = list(color = "black", width = 1)
  ),
  hovertemplate = paste(
    "<b>%{y}</b><br>",
    "Trimestre: %{frame}<br>",
    "Valor: %{x:.2f}%<br>",
    "<extra></extra>"
  )
) %>%
  layout(
    title = list(
      text = "Corrida de Barras: Dívida Externa / PIB",
      x = 0.5,
      xanchor = "center"
    ),
    xaxis = list(
      title = "Valor (%)",
      range = c(0, max(df_race$valor, na.rm = TRUE) * 1.1)
    ),
    yaxis = list(title = ""),
    showlegend = FALSE,
    margin = list(l = 100, r = 50, t = 100, b = 50),
    barmode = "overlay",
    transition = list(duration = 600, easing = "cubic-in-out")
  ) %>%
  animation_opts(
    frame = 1000,
    transition = 600,
    redraw = TRUE
  ) %>%
  animation_slider(
    currentvalue = list(prefix = "Trimestre: ")
  )

# --- Animated Area Chart ---
p_area <- plot_ly(
  data = divida,
  x = ~date,
  y = ~valor,
  type = "scatter",
  mode = "none",
  fill = "tozeroy",
  frame = ~trimestre,
  fillcolor = "rgba(70, 130, 180, 0.6)",
  hovertemplate = paste(
    "<b>Data: %{x|%Y-%m-%d}</b><br>",
    "Valor: %{y:.2f}%<br>",
    "<extra></extra>"
  )
) %>%
  layout(
    title = list(
      text = "Evolução Animada - Dívida Externa / PIB",
      x = 0.5,
      xanchor = "center"
    ),
    xaxis = list(
      title = "Data",
      type = "date",
      tickformat = "%Y-%m"
    ),
    yaxis = list(
      title = "Valor (%)",
      range = c(0, max(divida$valor, na.rm = TRUE) * 1.1)
    ),
    showlegend = FALSE,
    margin = list(l = 50, r = 50, t = 100, b = 50)
  ) %>%
  animation_opts(
    frame = 1000,
    transition = 600,
    redraw = TRUE
  ) %>%
  animation_slider(
    currentvalue = list(prefix = "Trimestre: ")
  )

# Display plots (run one at a time)
p_race
# p_area


# Clear cache to ensure fresh data
if (requireNamespace("memoise", quietly = TRUE)) {
  memoise::forget(get_series)
}

# Download External Debt / GDP data
divida <- tryCatch({
  get_series(11407, as = "data.frame")
}, error = function(e) {
  stop("Error retrieving data from BCB: ", e$message)
})

# Check if data was retrieved successfully
if (is.null(divida) || nrow(divida) == 0) {
  stop("No data retrieved from BCB for series 11407")
}

# Debug: Print data summary
cat("Number of rows in divida:", nrow(divida), "\n")
cat("Column names in divida:", names(divida), "\n")
cat("First few rows of divida:\n")
print(head(divida, 10))
cat("Date range:", min(divida$date, na.rm = TRUE), "to", max(divida$date, na.rm = TRUE), "\n")

# Adjust column names dynamically
value_col <- names(divida)[names(divida) != "date"]
if (length(value_col) == 0) {
  stop("No value column found in the data")
}

divida <- divida %>%
  rename(date = date, valor = !!value_col) %>%
  mutate(
    indicador = "Dívida Externa / PIB (%)",
    date = as.Date(date),  # Ensure date is in Date format
    valor = as.numeric(valor)  # Ensure value is numeric
  ) %>%
  filter(!is.na(valor) & !is.na(date)) %>%  # Remove missing values
  arrange(date)

# Debug: Check processed data
cat("Number of rows after processing:", nrow(divida), "\n")
if (nrow(divida) == 0) {
  stop("No valid data after processing (all values or dates are NA)")
}

# Create a simple interactive line plot
p_line <- plot_ly(
  data = divida,
  x = ~date,
  y = ~valor,
  type = "scatter",
  mode = "lines+markers",
  line = list(color = "steelblue", width = 2),
  marker = list(size = 6, color = "steelblue", line = list(color = "black", width = 1)),
  text = ~paste("Data:", format(date, "%Y-%m-%d"), "<br>Valor:", sprintf("%.2f%%", valor)),
  hoverinfo = "text"
) %>%
  layout(
    title = list(
      text = "Dívida Externa / PIB (%) - Brasil",
      x = 0.5,
      xanchor = "center"
    ),
    xaxis = list(
      title = "Data",
      type = "date",
      tickformat = "%Y-%m",
      rangeslider = list(visible = TRUE),
      rangeselector = list(
        buttons = list(
          list(count = 1, label = "1 ano", step = "year", stepmode = "backward"),
          list(count = 5, label = "5 anos", step = "year", stepmode = "backward"),
          list(count = 10, label = "10 anos", step = "year", stepmode = "backward"),
          list(step = "all", label = "Tudo")
        )
      )
    ),
    yaxis = list(
      title = "Valor (%)",
      range = c(min(divida$valor, na.rm = TRUE) * 0.9, max(divida$valor, na.rm = TRUE) * 1.1)
    ),
    showlegend = FALSE,
    margin = list(l = 50, r = 50, t = 100, b = 50),
    hovermode = "closest"
  )

# Display the plot
p_line
