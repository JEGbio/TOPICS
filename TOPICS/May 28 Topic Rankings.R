# Load necessary libraries
library(ggplot2)
library(fmsb)

# Set the working directory to the TOPICS folder on your USB drive
setwd("D:/TOPICS")

# Read the CSV file
data <- read.csv("May 28 Topic Rankings.csv")

# Function to convert rankings to points
convert_to_points <- function(rank1, rank2, rank3) {
  points <- c(Pathogens = 0, Models = 0, Coevolution = 0)
  points[rank1] <- points[rank1] + 2
  points[rank2] <- points[rank2] + 1
  # No points for the third ranking
  return(points)
}

# Apply the conversion to each row and store the results in a data frame
points_data <- t(apply(data[, c("rank1", "rank2", "rank3")], 1, function(row) {
  convert_to_points(row[1], row[2], row[3])
}))

# Convert the matrix to a data frame and add the names and labs back
points_data <- as.data.frame(points_data)
points_data$name <- data$name
points_data$lab <- data$lab

# Split the data by lab
shapiro_data <- points_data[points_data$lab == "Shapiro", ]
kassen_data <- points_data[points_data$lab == "Kassen", ]

# Sum the points for each lab
sum_points_shapiro <- colSums(shapiro_data[, c("Pathogens", "Models", "Coevolution")])
sum_points_kassen <- colSums(kassen_data[, c("Pathogens", "Models", "Coevolution")])

# Find the maximum sum of points across both labs
max_sum_points <- max(sum(sum_points_shapiro), sum(sum_points_kassen))

# Define a maximum value for scaling
max_value <- max_sum_points * 0.18  # Adjust as needed

# Normalize the data for each lab
normalize_data <- function(data) {
  normalized_data <- data / max_value
  return(normalized_data)
}

# Apply normalization to each lab's data
normalized_shapiro_data <- normalize_data(sum_points_shapiro)
normalized_kassen_data <- normalize_data(sum_points_kassen)

# Prepare the data for the radar chart
radar_data <- rbind(rep(max_value, 3), rep(0, 3), normalized_shapiro_data, normalized_kassen_data)

# Convert radar_data to data frame
radar_data <- as.data.frame(radar_data)

# Plotting the radar chart
par(mar = c(5, 4, 4, 2) + 0.1) # Adjust margins

# Radar chart for both labs
radarchart(radar_data, 
           axistype = 1,
           pcol = c(rgb(1, 0.5, 0, 0.9), rgb(0, 0.5, 1, 0.9)),  # Shapiro in orange, Kassen in blue
           pfcol = c(rgb(1, 0.5, 0, 0.5), rgb(0, 0.5, 1, 0.5)),  # Shapiro in light orange, Kassen in light blue
           plwd = 2,  # Set the line width to 2 for darker lines
           cglcol = "black",  # Set the grid line color to black
           cglty = 1, 
           axislabcol = "black",  # Set the axis label color to black
           caxislabels = NA,  # Remove axis labels
           vlcex = 1.2,
           title = "TOPICS Rankings by Lab",
           label.font = 2,  # Set the font to bold
           family = "Arial"  # Set the font family to Arial
)

# Add legend below the figure, in the center
legend("bottom", legend = c("Shapiro", "Kassen"), fill = c(rgb(1, 0.5, 0, 0.5), rgb(0, 0.5, 1, 0.5)), 
       title = "Lab", horiz = TRUE, xjust = "center")

