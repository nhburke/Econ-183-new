library(dplyr)
library(tidyr)
library(plm)
library(readxl)
library(ggplot2)
library(scales)
library(lmtest)
library(stargazer)



# Load the data
admissions <- read.csv("admissions_data_summary.csv") %>% 
  filter(!(MSOA11 %in% c("L99999999", "M99999999", "N99999999", "NA"))) %>% 
  select(-1)

air_quality <- readxl::read_excel("Air pollution in England by MSOA over time, measured by PM2.5 exposure, 2003 to 2023 (3).xlsx", sheet = "Sheet1")


# Count the number of unique values in the MSOA11 column
num_unique_msoa11_admiss <- length(unique(admissions$MSOA11))
print(paste("Number of unique MSOA11 values:", num_unique_msoa11_admiss))##NICK: check last row in adms. here - there's an NA which has large values, so I'm removing
admissions <- admissions[-nrow(admissions), ]

# Pivot the data to long format for all the specified variables
admissions_long <- admissions %>%
  pivot_longer(cols = starts_with(c("row_count", "male_percentage", "female_percentage", 
                                    "under_16_percentage", "age_16_65_percentage", "over_65_percentage")),
               names_to = c("variable", "Year"),
               names_pattern = "(.*)_(\\d{4})",  # Capture the variable and the year from the column names
               values_to = "value") %>%
  mutate(variable = ifelse(variable == "row_count_", "Admissions", variable)) %>% 
  mutate(variable = ifelse(variable == "male_percentage_", "male_percentage", variable)) %>% 
  mutate(variable = ifelse(variable == "female_percentage_", "female_percentage", variable)) %>%  
  mutate(variable = ifelse(variable == "under_16_percentage_", "under_16_percentage", variable)) %>%  
  mutate(variable = ifelse(variable == "age_16_65_percentage_", "age_16_65_percentage", variable)) %>%  
  mutate(variable = ifelse(variable == "over_65_percentage_", "over_65_percentage", variable)) %>%  
  arrange(MSOA11)  # Optionally, arrange by your unique identifier (MSOA11)

admissions_wide <- admissions_long %>%
  pivot_wider(names_from = "variable", values_from = "value")

# Rename 'msoa11cd' to 'MSOA11' 
air_quality <- air_quality %>%
  rename(MSOA11 = msoa11cd)%>%
  mutate(
    `2003` = as.numeric(`2003`),
    `2004` = as.numeric(`2004`)
  )

# Count the number of unique values in the MSOA11 column in air quality
num_unique_msoa11 <- length(unique(air_quality$MSOA11))
print(paste("Number of unique MSOA11 values:", num_unique_msoa11))


# Reshape the air quality data into long format
air_quality_long <- air_quality %>%
  pivot_longer(cols = `2003`:`2023`,  
               names_to = "Year",     
               values_to = "PM2.5")  

# Calculate the average PM2.5 for each unique MSOA and Year
average_msoa_data <- air_quality_long %>%
  group_by(Year) %>%
  summarise(average_MSOA = mean(PM2.5, na.rm = TRUE))  # Calculate the mean, removing any NA values

# Ensure that 'Year' is treated as numeric
average_msoa_data$Year <- as.numeric(average_msoa_data$Year)

# Plot the average PM2.5 as a line graph with a red line, x-axis at 5-year intervals, and y-axis between 0 and 20
ggplot(average_msoa_data, aes(x = Year, y = average_MSOA, group = 1)) + 
  geom_line(color = "red", size = 1) +  # Red line to connect the dots
  labs(
    title = "Average Air pollution in England, measured by PM2.5 exposure 2003-2023",
    x = "Year",
    y = "Average MSOA PM2.5 exposure - micrograms per cubic metre (µg m³)"
  ) +
  scale_x_continuous(breaks = seq(2003, 2023, by = 5)) +  # Set x-axis breaks in 5-year steps
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20)) +  # Set y-axis between 0 and 20
  geom_hline(yintercept = 10, linetype = "dashed", color = "blue", size = 1) +  # Add the first horizontal line
  geom_text(aes(x = 2020, y = 10, label = "UK 2040 Target"), color = "blue", vjust = -0.5) +  # Label for the first line
  geom_hline(yintercept = 5, linetype = "dashed", color = "black", size = 1) +  # Add the second horizontal line
  geom_text(aes(x = 2020, y = 5, label = "WHO recommended level"), color = "black", vjust = -0.5) +  # Label for the second line
  theme_minimal()  # Use a minimal theme for the plot

# Re-aggregate the data to calculate average admissions per year
average_admissions <- admissions_wide %>%
  group_by(Year) %>%
  summarise(AverageAdmissions = mean(Admissions, na.rm = TRUE))

# View the resulting data
print(average_admissions, n=21)

# Ensure that 'Year' is treated as numeric
average_admissions$Year <- as.numeric(average_admissions$Year)

# Plot the average admissions as a line graph with a red line and updated titles
ggplot(average_admissions, aes(x = Year, y = AverageAdmissions, group = 1)) +  # Add group = 1
  geom_line(color = "red", size = 1) +  # Red line graph
  labs(
    title = "Average MOSA Level Respiratory Admissions 2003-2023",
    x = "Year",
    y = "Average Respiratory Admissions"
  ) +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +  # Set x-axis breaks from 2000 with a step of 5 years
  scale_y_continuous(expand = c(0, 0), limits = c(0, 200)) +  # Set y-axis between 0 and 200
  theme_minimal()  # Use a minimal theme for the plot

# Merge the datasets on MSOA11 and Year
merged_data_clean <- left_join(admissions_wide, air_quality_long, by = c("MSOA11", "Year")) %>% 
  filter(!is.na(MSOA11) & !is.na(Year))%>%
  filter(Year != 2023) %>%
  filter(Year != 2022) %>%
  filter(Year != 2021) %>%
  filter(Year != 2020) %>% 
  mutate(
    Admissions = as.numeric(Admissions),
    PM2.5 = as.numeric(PM2.5)
  )

# Set up the panel data structure
panel_data_clean <- pdata.frame(merged_data_clean, index = c("MSOA11", "Year"))

# Run the fixed effects model with only PM2.5 as the independent variable
model_no_controls <- plm(Admissions ~ PM2.5, 
                         data = panel_data_clean, 
                         model = "within",
                         effect = "twoways")  # "within" specifies fixed effects model

# View the summary of the model
summary(model_no_controls)

coeftest(model_no_controls, vcov. = vcovHC, type = "HC1")

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
  select(MSOA11, Year, TotalPop)

merged_data_clean$Year <- as.integer(merged_data_clean$Year)
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
  select(MSOA11, Year, TotalPop)

# Perform the left join to bring TotalPop and percent_over_65 into merged_data_clean
merged_data_clean <- left_join(merged_data_clean, selected_columns, by = c("MSOA11", "Year"))

# Use ifelse to replace NA values in TotalPopx and percent_over_65x with values from the merged dataset (TotalPop.y, percent_over_65.y)
merged_data_clean <- merged_data_clean %>%
  mutate(
    TotalPop.x = ifelse(is.na(TotalPop.x), TotalPop.y, TotalPop.x),
  )

# Remove the extra columns created by the left join (TotalPop.y, percent_over_65.y)
merged_data_clean <- merged_data_clean %>%
  select(-TotalPop.y)

# Drop rows where Year equals 2023
merged_data_clean <- merged_data_clean %>%
  filter(Year != 2023) %>%
  filter(Year != 2022) %>%
  filter(Year != 2021) %>%
  filter(Year != 2020)

# Rename columns TotalPop.x to TotalPop and percent_over_65.x to percent_over_65
merged_data_clean <- merged_data_clean %>%
  rename(
    TotalPop = TotalPop.x
  )

# Get summary statistics for all numeric columns
summary_stats <- merged_data_clean %>%
  summarise(
    across(where(is.numeric), 
           list(
             Mean = ~mean(., na.rm = TRUE),
             Median = ~median(., na.rm = TRUE),
             SD = ~sd(., na.rm = TRUE),
             Min = ~min(., na.rm = TRUE),
             Max = ~max(., na.rm = TRUE),
             N = ~n()
           ), 
           .names = "{col}_{fn}")  # Naming columns like 'ColumnName_Mean'
  )

# Display the result
summary_stats

# Set Year and MSOA11 as factors for panel data
merged_data_clean$Year <- as.factor(merged_data_clean$Year)
merged_data_clean$MSOA11 <- as.factor(merged_data_clean$MSOA11)

# Run the fixed effects model with PM2.5 on admissions controlling for TotalPop and percent_over_65
model1 <- plm(
  Admissions ~ PM2.5 + TotalPop + male_percentage + over_65_percentage+under_16_percentage,  
  data = merged_data_clean,
  index = c("MSOA11", "Year"),  
  model = "within",
  effect = "twoways"
)

# Summary of the model
summary(model1)
coeftest(model1, vcov. = vcovHC, type = "HC1")
##Stargazer output
stargazer(model1, model2, type = "html", out = "regression_table.html",
          title = "Regression Results", align = TRUE, digits = 2)

##Pooled OLS
pooled_ols_model <- plm(Admissions ~ PM2.5 + TotalPop, 
                        data = merged_data_clean, 
                        model = "pooling")
summary(pooled_ols_model)

#Hausmann test to see whether FE is needed

hausman_test <- phtest(model1, pooled_ols_model)
summary(hausman_test) ##FE better due to low p-value


# Create a new column 'admissions_per_capita' that is admissions divided by TotalPop
merged_data_clean <- merged_data_clean %>%
  mutate(Admissions_per_capita = Admissions / TotalPop)

# Run the fixed effects model for PM2.5 on admissions_per_capita by MSOA and Year, with percent_over_65 as a control
model2 <- plm(
  Admissions_per_capita ~ PM2.5 + male_percentage + over_65_percentage+under_16_percentage ,  # Model formula with PM2.5 and percent_over_65 as controls
  data = merged_data_clean,
  index = c("MSOA11", "Year"),  # Panel data identifiers (MSOA and Year)
  model = "within" ,
  effect = "twoways"# Fixed effects (within estimator)
)

# Summary of the model
summary(model2)
coeftest(model2, vcov. = vcovHC, type = "HC1")