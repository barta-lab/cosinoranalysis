# Install and load the necessary packages
if (!require(nlme)) {
  install.packages("nlme")
}
library(nlme)

if (!require(ggplot2)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Function to read data from clipboard (copied from Excel)
read_excel_clipboard <- function() {
  read.table(file = "clipboard", sep = "\t", header = TRUE)
}

# Function for cosinor model
cosinor.model <- function(data, time, avg_replicate) {
  lm(avg_replicate ~ cos(2 * pi * time / 24) + sin(2 * pi * time / 24), data = data)
}

# Read data from clipboard
data <- read_excel_clipboard()

# Ensure that 'time' and replicates are in the correct format
data$time <- as.numeric(as.character(data$time))
for (i in 1:5) {
  replicate_col <- paste("replicate", i, sep = "")
  data[[replicate_col]] <- as.numeric(as.character(data[[replicate_col]]))
}

# Averaging the replicates and calculating standard error
replicate_columns <- paste("replicate", 1:5, sep = "")
data$average_replicate <- rowMeans(data[, replicate_columns])
data$std_error <- apply(data[, replicate_columns], 1, sd) / sqrt(5) # Assuming 5 replicates for standard error

# Apply the cosinor model to the averaged data
model <- cosinor.model(data, data$time, data$average_replicate)

# Enhanced plotting using ggplot2
ggplot() +
  geom_point(data = data, aes(x = time, y = replicate1), color = "blue", size = 1) +
  geom_line(data = data, aes(x = time, y = replicate1), color = "blue") +
  geom_point(data = data, aes(x = time, y = replicate2), color = "green", size = 1) +
  geom_line(data = data, aes(x = time, y = replicate2), color = "green") +
  geom_point(data = data, aes(x = time, y = replicate3), color = "orange", size = 1) +
  geom_line(data = data, aes(x = time, y = replicate3), color = "orange") +
  geom_point(data = data, aes(x = time, y = replicate4), color = "purple", size = 1) +
  geom_line(data = data, aes(x = time, y = replicate4), color = "purple") +
  geom_point(data = data, aes(x = time, y = replicate5), color = "brown", size = 1) +
  geom_line(data = data, aes(x = time, y = replicate5), color = "brown") +
  geom_line(data = data, aes(x = time, y = predict(model)), color = "red", size = 2) +
  theme_minimal() +
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.line = element_line(color = "black"))+
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        #panel.background = element_blank()) +
  ggtitle("Cosinor Analysis (5 Replicates)", subtitle = "Individual Replicates and Cosinor Model Fit") +
  scale_y_continuous(limits = c(0, 14))+
  xlab("Time (units)") +
  ylab("Response") 

# Display model summary
print(summary(model))

