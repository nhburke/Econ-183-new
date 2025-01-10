library(dplyr)
library(tidyr)
library(plm)
library(readxl)
library(ggplot2)
library(scales)
library(lmtest)
library(stargazer)



# Load the data
admissions <- read.csv("data_summary_wide.csv") %>% 
  filter(!(MSOA11 %in% c("L99999999", "M99999999", "N99999999", "NA"))) %>% 
  select(-1) %>%
  {colnames(.) <- gsub("^X", "", colnames(.)); .}

air_quality <- readxl::read_excel("Air pollution in England by MSOA over time, measured by PM2.5 exposure, 2003 to 2023 (3).xlsx", sheet = "Sheet1")


# Count the number of unique values in the MSOA11 column
num_unique_msoa11_admiss <- length(unique(admissions$MSOA11))
print(paste("Number of unique MSOA11 values:", num_unique_msoa11_admiss))##NICK: check last row in adms. here - there's an NA which has large values, so I'm removing
admissions <- admissions[-nrow(admissions), ]


# Reshape the admissions data into long format and rename the column to "Admissions"
admissions_long <- admissions %>%
  pivot_longer(cols = `2003`:`2023`,  
               names_to = "Year",     
               values_to = "Admissions")  


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


# Merge the datasets on MSOA11 and Year
merged_data_clean <- left_join(admissions_long, air_quality_long, by = c("MSOA11", "Year")) %>% 
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

# Set Year and MSOA11 as factors for panel data
merged_data_clean$Year <- as.factor(merged_data_clean$Year)
merged_data_clean$MSOA11 <- as.factor(merged_data_clean$MSOA11)

# Run the fixed effects model with PM2.5 on admissions controlling for TotalPop and percent_over_65
model1 <- plm(
  Admissions ~ PM2.5 + TotalPop,  
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
  Admissions_per_capita ~ PM2.5 ,  # Model formula with PM2.5 and percent_over_65 as controls
  data = merged_data_clean,
  index = c("MSOA11", "Year"),  # Panel data identifiers (MSOA and Year)
  model = "within" ,
  effect = "twoways"# Fixed effects (within estimator)
)

# Summary of the model
summary(model2)
coeftest(model2, vcov. = vcovHC, type = "HC1")
