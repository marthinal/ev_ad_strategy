# Install and load necessary libraries
if (!require("ggplot2")) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if (!require("dplyr")) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if (!require("plotly")) install.packages("plotly", repos = "http://cran.us.r-project.org")
if (!require("htmltools")) install.packages("htmltools", repos = "http://cran.us.r-project.org")
if (!require("reshape2")) install.packages("reshape2", repos = "http://cran.us.r-project.org")

library(ggplot2)
library(dplyr)
library(plotly)
library(htmltools)
library(reshape2)

# Read the CSV file
ruta_csv <- "~/ev_ad_strategy/datasources/ev_charging_patterns.csv"
data <- read.csv(ruta_csv, stringsAsFactors = FALSE)

# Convert Charging.End.Time to date and time format
data$Charging.End.Time <- as.POSIXct(data$Charging.End.Time, format="%Y-%m-%d %H:%M:%S", tz="UTC")

# Create the End.Hour and Day.of.Week columns
data <- data %>%
  mutate(End.Hour = as.numeric(format(Charging.End.Time, "%H")),
         Day.of.Week = weekdays(Charging.End.Time))

# Check if the columns are present
print(head(data))

# Bivariate Analysis: Create the dataframe for the heatmap
heatmap_data <- data %>%
  group_by(End.Hour, Day.of.Week) %>%
  summarise(Count = n(), .groups = "drop")

# Create the heatmap
heatmap_plot <- ggplot(heatmap_data, aes(x = Day.of.Week, y = End.Hour, fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of End Time vs Day of Week", x = "Day of Week", y = "End Hour", fill = "Frequency") +
  theme_minimal()

# Convert to interactive plot immediately
heatmap_plot_interactive <- ggplotly(heatmap_plot)

# Bivariate Analysis: Boxplot for End Time vs Day of Week
boxplot_plot <- ggplot(data, aes(x = Day.of.Week, y = End.Hour, fill = Day.of.Week)) +
  geom_boxplot() +
  labs(title = "Boxplot of End Time by Day of Week", x = "Day of Week", y = "End Hour of Charging") +
  theme_minimal() +
  theme(legend.position = "none")

# Convert to interactive plot immediately
boxplot_plot_interactive <- ggplotly(boxplot_plot)

# Create the HTML content with the graphs
html_content <- tags$div(
  tags$h1("Bivariate Analysis - End Time vs Day of Week"),
  tags$h2("Heatmap of End Time vs Day of Week"),
  heatmap_plot_interactive,
  tags$h2("Boxplot of End Time by Day of Week"),
  boxplot_plot_interactive
)

# Save the report to an HTML file
htmltools::save_html(html_content, file = "bivariate_analysis_endtime_dayofweek.html")
cat("The report has been saved as 'bivariate_analysis_endtime_dayofweek_fixed_v2.html'\n")