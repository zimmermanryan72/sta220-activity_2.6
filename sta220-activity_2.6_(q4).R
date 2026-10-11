#Import tidyverse
library(tidyverse)

#Read data file
q4_2025_data = read_csv("data/2025_q4.csv")

#Use 3 checks
#We expect 48 rows and 4 columns
dim(q4_2025_data)
names(q4_2025_data)
summary(q4_2025_data)


#Clean data
#Create a new column where days_to_close has a NA where there was a -1
q4_2025_data <- q4_2025_data |> 
  mutate(days_to_close_clean = ifelse(days_to_close == -1, NA, days_to_close))

#Analyze
#Limit of 30 days is found in Situation
max_days_to_close <- 30

#Create new column for if the days to close is greater than 30,
#Then it is considered late and a true is added to column
q4_2025_data <- q4_2025_data |> 
  mutate(late = days_to_close_clean > max_days_to_close)

#Find the claims, open claims, average days to close, and percent
#of claims that were late for each type of claim
q4_summary = q4_2025_data |>  
  group_by(claim_type) |> 
  summarise(
    claims = sum(!is.na(days_to_close_clean)),
    claims_open = sum(is.na(days_to_close_clean)),
    mean_days = mean(days_to_close_clean, na.rm = TRUE), 
    percent_late = mean(late, na.rm = TRUE)
  )

#Print the table from above
q4_summary

#Save to output folder
write_csv(q4_2025_data, file = "output/cleaned_q4_2025.csv")
