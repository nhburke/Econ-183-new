install.packages("dplyr")
install.packages("tidyr")
install.packages("plm")
install.packages("readxl")
install.packages("ggplot2")
install.packages("stargazer")
install.packages("fixest")
library(dplyr)
library(tidyr)
library(plm)
library(readxl)
library(ggplot2)
library(scales)
library(stargazer)
library("fixest")

#set WD
setwd("C:/Users/NHBurke/OneDrive - Department of Health and Social Care/EMAP/Econ 183/Air Qaulity project")

# Load the data
admissions <- read.csv("data_summary_wide.csv")
air_quality <- readxl::read_excel("Air pollution in England by MSOA over time, measured by PM2.5 exposure, 2003 to 2023 (3).xlsx", sheet = "Sheet1")

# Clean up the column names by removing the 'X' from the years
colnames(admissions) <- gsub("^X", "", colnames(admissions))

# Remove the first column of the admissions data
admissions <- admissions %>%
  select(-1)

#drop MSOAs for NAs, Northern Ireland, Channel Islands and Isle of Man
admissions <- admissions %>%
  filter(!(MSOA11 %in% c("L99999999", "M99999999", "N99999999", "NA")))

# Count the number of unique values in the MSOA11 column
num_unique_msoa11_admiss <- length(unique(admissions$MSOA11))

# Print the result
print(paste("Number of unique MSOA11 values:", num_unique_msoa11_admiss))

# Calculate the total for each column
england_total <- colSums(admissions[, -which(colnames(admissions) == "MSOA11")], na.rm = TRUE)

# Add the "England Total" as a new row
england_row <- c(MSOA11 = "England Total", england_total)

# Append the new row to the dataset
admissions <- rbind(admissions, england_row)

# Reshape the admissions data into long format and rename the column to "Admissions"
admissions_long <- admissions %>%
  pivot_longer(cols = `2003`:`2023`,  
               names_to = "Year",     
               values_to = "Admissions")  

# Filter the data to only include the "England Total"
england_total_data <- admissions_long %>%
  filter(MSOA11 == "England Total")

# Convert Admissions to numeric, removing commas if needed
england_total_data$Admissions <- as.numeric(gsub(",", "", england_total_data$Admissions))

# Display the values for each year
print(england_total_data, n=21)

# Plot the "England Total" over the years as a line graph
ggplot(england_total_data, aes(x = Year, y = Admissions, group = 1)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "England Total Respiratory Admissions 2003-2023",
    x = "Year",
    y = "Total Respiratory Admissions"
  ) +
  scale_y_continuous(labels = comma) +  # Format y-axis with commas
  theme_minimal()

# Remove the "England Total" row from the dataset
admissions_long <- admissions_long %>%
  filter(MSOA11 != "England Total")

# Ensure Admissions is numeric and Year is Integer
admissions_long$Admissions <- as.numeric(admissions_long$Admissions)
admissions_long$Year <- as.integer(admissions_long$Year)

# Re-aggregate the data to calculate average admissions per year
average_admissions <- admissions_long %>%
  group_by(Year) %>%
  summarise(AverageAdmissions = mean(Admissions, na.rm = TRUE))

# View the resulting data
print(average_admissions, n=21)

# Plot the average admissions as a line graph with a red line and updated titles
ggplot(average_admissions, aes(x = Year, y = AverageAdmissions)) +
  geom_line(color = "red", size = 1) +  # Red line graph
  labs(
    title = "Average MOSA Level Respiratory Admissions 2003-2023",
    x = "Year",
    y = "Average Respiratory Admissions"
  ) +
  theme_minimal()

# Ensure that 'Year' is in integer format
admissions_long$Year <- as.integer(admissions_long$Year)

# Rename 'msoa11cd' to 'MSOA11' 
air_quality <- air_quality %>%
  rename(MSOA11 = msoa11cd)

# Count the number of unique values in the MSOA11 column in air quality
num_unique_msoa11 <- length(unique(air_quality$MSOA11))

# Print the result
print(paste("Number of unique MSOA11 values:", num_unique_msoa11))

# Convert '2003' and '2004' columns from character to numeric
air_quality$`2003` <- as.numeric(air_quality$`2003`)
air_quality$`2004` <- as.numeric(air_quality$`2004`)

# Reshape the air quality data into long format
air_quality_long <- air_quality %>%
  pivot_longer(cols = `2003`:`2023`,  
               names_to = "Year",     
               values_to = "PM2.5")  

# Ensure that 'Year' is in integer format
air_quality_long$Year <- as.integer(air_quality_long$Year)

# Merge the datasets on MSOA11 and Year
merged_data <- left_join(admissions_long, air_quality_long, by = c("MSOA11", "Year"))

#remove NAs
merged_data_clean <- merged_data %>%
  filter(!is.na(MSOA11) & !is.na(Year))

# Drop rows where Year equals 2023,2022m2021,2020
merged_data_clean <- merged_data_clean %>%
  filter(Year != 2023) %>%
  filter(Year != 2022) %>%
  filter(Year != 2021) %>%
  filter(Year != 2020)

# Convert Admissions and PM2.5 to numeric
merged_data_clean <- merged_data_clean %>%
  mutate(
    Admissions = as.numeric(Admissions),
    PM2.5 = as.numeric(PM2.5)
  )

# Set up the panel data structure with MSOA and Year
panel_data_clean <- pdata.frame(merged_data_clean, index = c("MSOA11", "Year"))

# Run the two-way fixed effects model
model_no_controls <- plm(
  Admissions ~ PM2.5, 
  data = panel_data_clean, 
  model = "within",  # Specifies fixed effects
  effect = "twoways" # Enables two-way fixed effects
)

# View the summary of the model
summary(model_no_controls)

# Calculate robust standard errors with vcovHC and type = "HC1"
robust_se <- vcovHC(model_no_controls, type = "HC1")

# Display the summary with robust standard errors
summary(model_no_controls, vcov = robust_se)

# Create a table for the results
stargazer(model_two_way, 
          type = "html",                   # Change to "text" for console or "latex" for LaTeX output
          title = "Two-Way Fixed Effects Regression Results",
          dep.var.labels = "Admissions",  # Dependent variable label
          covariate.labels = c("PM2.5", "Percent Over 65", "Total Population"), # Rename variables
          align = TRUE, 
          no.space = TRUE,                # Removes extra spaces for cleaner output
          digits = 3,                     # Number of decimal places
          notes = "* p<0.1; ** p<0.05; *** p<0.01",
          out = "results_table.html")      # Save as HTML file

# Specify the file path
file_path <- "sapemsoaquinaryage20112022.xlsx"

# Initialize an empty data frame to store all the data
Population_data_frame <- data.frame()

# Loop through each year from 2011 to 2022
for (year in 2011:2022) {
  
  # Read the sheet for the current year
  sheet_name <- paste0("Mid-", year, " MSOA 2021")
  Pop_data <- read_excel(file_path, sheet = sheet_name, skip = 3)
  
  # Sum the relevant columns to create the 'over_65' column
  Pop_data$over_65 <- Pop_data$`F65 to 69` + Pop_data$`F70 to 74` + Pop_data$`F75 to 79` + 
    Pop_data$`F80 to 84` + Pop_data$`F85 to 89` + Pop_data$`F90 and over` + 
    Pop_data$`M65 to 69` + Pop_data$`M70 to 74` + Pop_data$`M75 to 79` + 
    Pop_data$`M80 to 84` + Pop_data$`M85 to 89` + Pop_data$`M90 and over`
  
  # Create the 'percent_over_65' column by dividing 'over_65' by 'Total' and multiplying by 100
  Pop_data$percent_over_65 <- (Pop_data$over_65 / Pop_data$Total) * 100
  
  # Add the year as a new column
  Pop_data$Year <- year
  
  # Append the current year data to the Population_data_frame
  Population_data_frame <- bind_rows(Population_data_frame, Pop_data)
}

# Rename the columns
colnames(Population_data_frame)[which(colnames(Population_data_frame) == "Total")] <- "TotalPop"
colnames(Population_data_frame)[which(colnames(Population_data_frame) == "MSOA 2021 Code")] <- "MSOA11"

# Select only the necessary columns from Population_data_frame
selected_columns <- Population_data_frame %>%
  select(MSOA11, Year, TotalPop, percent_over_65)

# Perform the left join on MSOA11 and Year to bind the columns into merged_data
merged_data_clean <- left_join(merged_data_clean, selected_columns, by = c("MSOA11", "Year"))

# Specify the file path for the new dataset
file_path <- "SAPE8DT4-MSOA-syoa-unformatted-persons-mid2002-to-mid2010.xls"

# Initialize an empty data frame to store all the data
Population_data_frame_2003_2010 <- data.frame()

# Loop through each year from 2003 to 2010
for (year in 2003:2010) {
  
  # Read the sheet for the current year
  sheet_name <- paste0("Mid-", year)
  Pop_data <- read_excel(file_path, sheet = sheet_name)
  
  # Calculate the 'over_65' column by summing the relevant columns (p65 to p90plus)
  Pop_data$over_65 <- rowSums(Pop_data[, grep("^p6[5-9]|^p[7-9][0-9]|^p90plus", colnames(Pop_data))], na.rm = TRUE)
  
  # Create the 'percent_over_65' column by dividing 'over_65' by 'all_ages' and multiplying by 100
  Pop_data$percent_over_65 <- (Pop_data$over_65 / Pop_data$all_ages) * 100
  
  # Add the year as a new column
  Pop_data$Year <- year
  
  # Append the current year data to the Population_data_frame_2003_2010
  Population_data_frame_2003_2010 <- bind_rows(Population_data_frame_2003_2010, Pop_data)
}

# After the loop, rename the columns
colnames(Population_data_frame_2003_2010)[which(colnames(Population_data_frame_2003_2010) == "all_ages")] <- "TotalPop"
colnames(Population_data_frame_2003_2010)[which(colnames(Population_data_frame_2003_2010) == "MSOA11CD")] <- "MSOA11"

# View the column names of both merged_data and Population_data_frame_2003_2010
colnames(merged_data_clean)
colnames(Population_data_frame_2003_2010)

# Select only the necessary columns from Population_data_frame_2003_2010
selected_columns <- Population_data_frame_2003_2010 %>%
  select(MSOA11, Year, TotalPop, percent_over_65)

# Perform the left join to bring TotalPop and percent_over_65 into merged_data_clean
merged_data_clean <- left_join(merged_data_clean, selected_columns, by = c("MSOA11", "Year"))

# Use ifelse to replace NA values in TotalPopx and percent_over_65x with values from the merged dataset (TotalPop.y, percent_over_65.y)
merged_data_clean <- merged_data_clean %>%
  mutate(
    TotalPop.x = ifelse(is.na(TotalPop.x), TotalPop.y, TotalPop.x),
    percent_over_65.x = ifelse(is.na(percent_over_65.x), percent_over_65.y, percent_over_65.x)
  )

# Remove the extra columns created by the left join (TotalPop.y, percent_over_65.y)
merged_data_clean <- merged_data_clean %>%
  select(-TotalPop.y, -percent_over_65.y)

# Drop rows where Year equals 2023
merged_data_clean <- merged_data_clean %>%
  filter(Year != 2023) %>%
  filter(Year != 2022) %>%
  filter(Year != 2021) %>%
  filter(Year != 2020)

# Rename columns TotalPop.x to TotalPop and percent_over_65.x to percent_over_65
merged_data_clean <- merged_data_clean %>%
  rename(
    TotalPop = TotalPop.x,
    percent_over_65 = percent_over_65.x
  )

# Set Year and MSOA11 as factors for panel data
merged_data_clean$Year <- as.factor(merged_data_clean$Year)
merged_data_clean$MSOA11 <- as.factor(merged_data_clean$MSOA11)

# Set Year and MSOA11 as factors for panel data
merged_data_clean$Year <- as.factor(merged_data_clean$Year)
merged_data_clean$MSOA11 <- as.factor(merged_data_clean$MSOA11)

# Run the fixed effects model with PM2.5 on admissions controlling for TotalPop and percent_over_65
model1 <- plm(
  Admissions ~ PM2.5 + TotalPop + percent_over_65,  # Model formula
  data = merged_data_clean,
  index = c("MSOA11", "Year"),  # Panel data identifiers
  model = "within"  # Fixed effects (within estimator)
)

# Summary of the model
summary(model1)

# Calculate robust standard errors with vcovHC and type = "HC1"
robust_se <- vcovHC(model1, type = "HC1")

# Display the summary with robust standard errors
summary(model1, vcov = robust_se)


# Create a new column 'admissions_per_capita' that is admissions divided by TotalPop
merged_data_clean <- merged_data_clean %>%
  mutate(Admissions_per_capita = Admissions / TotalPop)

# Run the fixed effects model for PM2.5 on admissions_per_capita by MSOA and Year, with percent_over_65 as a control
model2 <- plm(
  Admissions_per_capita ~ PM2.5 + percent_over_65,  # Model formula with PM2.5 and percent_over_65 as controls
  data = merged_data_clean,
  index = c("MSOA11", "Year"),  # Panel data identifiers (MSOA and Year)
  model = "within"  # Fixed effects (within estimator)
)

# Summary of the model
summary(model2)

# Calculate robust standard errors with vcovHC and type = "HC1"
robust_se <- vcovHC(model2, type = "HC1")

# Display the summary with robust standard errors
summary(model2, vcov = robust_se)
