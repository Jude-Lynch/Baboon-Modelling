library(readr)

# Read the data
data <- read_csv("C:/Users/grace/Downloads/mi docs/Uni/Bsc Marine bio/Year 3 - SHOAL Group/Work/Projects/Baboon project/Plotting relationships/class.csv")

# Extract unique years
years <- sort(unique(data$year))

# Define transitions to plot
transitions_to_plot <- c('IM-SAM', 'IF-SAF', 'SAM-AM', 'SAF-AF','AF-I')

# List to store the data frames
data_frames_list <- list()

# Create plots
plot.new()
par(mfrow=c(2,3), mar=c(4,4,1,1))

for (trs in transitions_to_plot) {
  x_class <- strsplit(trs, '-')[[1]][1]
  y_class <- strsplit(trs, '-')[[1]][2]
  
  # Data frame to store the current iteration's data
  current_data <- data.frame()
  
  for (i in 2:(length(years)-1)) {
    start_y <- years[i]
    end_y <- years[i+1]
    
    data_start <- data[data$year == start_y, ]
    data_end <- data[data$year == end_y, ]
    
    # Check for mismatches of troops across years
    common_troops <- intersect(data_start$troop, data_end$troop)
    data_start <- data_start[data_start$troop %in% common_troops, ]
    data_end <- data_end[data_end$troop %in% common_troops, ]
    
    # Ensure the data frames are in the same order
    data_end <- data_end[match(data_start$troop, data_end$troop), ]
    
    # Append current iteration's data to the data frame
    current_data <- rbind(current_data, data.frame(data_start[, x_class], data_start[, y_class], data_end[, y_class]))
  }
  
  # Append the data frame to the list
  data_frames_list[[trs]] <- current_data
  
  # Plot the data
  plot(current_data[,1], current_data[,3], xlab = x_class, ylab = y_class, pch = 16)
}

# Access and check data frames
data_frames_list[['IM-SAM']]
data_frames_list[['IF-SAF']]
data_frames_list[['SAM-AM']]
data_frames_list[['SAF-AF']]
data_frames_list[['AF-I']]

# List to store the linear models
linear_models_list <- list()

for (trs in transitions_to_plot) {
  
  # Access the data frame for the current transition
  current_data <- data_frames_list[[trs]]
  
  # Extract column names
  x_t <- names(current_data)[1]
  y_t <- names(current_data)[2]
  y_t1 <- names(current_data)[3]
  
  # Fit a linear model using lm()
  lm_model <- lm(paste(y_t1, "~", x_t, "+", y_t), data = current_data)
  
  # Store the linear model in the list
  linear_models_list[[trs]] <- lm_model
}

# Access the linear models from the list
linear_models_list[['IM-SAM']]
linear_models_list[['IF-SAF']]
linear_models_list[['SAM-AM']]
linear_models_list[['SAF-AF']]
linear_models_list[['AF-I']]

