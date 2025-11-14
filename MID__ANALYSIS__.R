#mport the dataset into R using appropriate functions 
data <- read.csv(file.choose())
#View the first few rows of the dataset
head(data)
#Check the structure of the dataset
str(data)
#Check for missing values in the dataset  
sum(is.na(data))
#Summary statistics of the dataset
summary(data)
#Show the number of rows, columns, and data types for each variable. 
dim(data)
sapply(data, class)
#Install and load necessary packages for data analysis
library(dplyr)
library(ggplot2)
#Data Cleaning: Remove duplicates and handle missing values
data <- data %>% distinct() # Remove duplicates
data <- na.omit(data) # Remove rows with missing values
#Perform a comprehensive data cleaning process: 
# Handle missing values appropriately 
#Remove duplicates
#Correct inconsistent data entries
data <- data %>%
  distinct() %>% # Remove duplicates
  mutate(across(everything(), ~ ifelse(. == "N/A" | . == "", NA, .))) %>% # Standardize missing values
  na.omit() # Remove rows with missing values
# Convert variables to appropriate data types (numeric, factor, date). 
data$Date <- as.Date(data$Date, format="%Y-%m-%d") # Convert to Date type
data$Pollutant_Concentration <- as.numeric(data$Pollutant_Concentration) # Convert to numeric type
data$Location <- as.factor(data$Location) # Convert to factor type
#Correct inconsistent data entries
data$Location <- recode(data$Location, "loc1" = "Location 1", "loc2" = "Location 2")
# Standardize date and time formats. 
data$Date <- as.Date(data$Date, format="%Y-%m-%d") # Standardize date format
data$Time <- format(as.POSIXct(data$Time, format="%H:%M"), "%H:%M") # Standardize time format
#Identify and handle outliers in pollutant concentration variables
Q1 <- quantile(data$Pollutant_Concentration, 0.25)
Q3 <- quantile(data$Pollutant_Concentration, 0.75)
IQR <- Q3 - Q1
# Define outlier thresholds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
# Remove outliers
outliers <- data %>% filter(Pollutant_Concentration >= lower_bound & Pollutant_Concentration <= upper_bound)
#CHECK OUTLIERS
length(outliers)
#Exploratory Data Analysis (EDA):
# Descriptive Statistics 
# Compute means, median, variance, and standard deviation of major pollutants
mean_pollutant <- mean(data$Pollutant_Concentration)
median_pollutant <- median(data$Pollutant_Concentration)
variance_pollutant <- var(data$Pollutant_Concentration)
sd_pollutant <- sd(data$Pollutant_Concentration)
mean_pollutant
median_pollutant
variance_pollutant
sd_pollutant

#Generate a correlation matrix among numerical variables. 
correlation_matrix <- cor(data %>% select(where(is.numeric)))
correlation_matrix


# Visual Analysis 
# Histogram or density plot for pollutant distributions. 
ggplot(data, aes(x=Pollutant_Concentration)) +
  geom_histogram(binwidth=1, fill="blue", color="black", alpha=0.7) +
  labs(title="Histogram of Pollutant Concentration", x="Pollutant Concentration", y="Frequency")
# Boxplots showing pollutant variation across locations or countries. 
ggplot(data, aes(x=Location, y=Pollutant_Concentration)) +
  geom_boxplot(fill="orange", color="black", alpha=0.7) +
  labs(title="Boxplot of Pollutant Concentration by Location", x="Location", y="Pollutant Concentration")
# Time-series line plot showing pollutant concentration trends over time. 
ggplot(data, aes(x=Date, y=Pollutant_Concentration)) +
  geom_line(color="green") +
  labs(title="Time-Series of Pollutant Concentration Over Time", x="Date", y="Pollutant Concentration")
# Correlation heatmap using ggcorrplot or similar package. 
library(ggcorrplot)
ggcorrplot(correlation_matrix, 
           method = "circle", 
           type = "lower", 
           lab = TRUE, 
           title = "Correlation Heatmap of Numerical Variables")
#) Interpretation 
# Provide at least three meaningful insights drawn from your descriptive and visual analysis. 
#Pollutant concentration is right-skewed, showing most values are low with occasional high spikes.
#Pollution levels vary noticeably across locations, indicating uneven environmental conditions.
#Pollutant concentration changes over time, revealing clear temporal trends in air quality.


