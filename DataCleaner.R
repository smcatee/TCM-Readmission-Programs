## Authors: Sean D McAtee, Sandy Carrillo-Argueta
## Version: 2022-03-09

## Libraries
require(tidyverse)
require(readxl)

## hello
## Load Data
filename <- file.choose()

readxl::excel_sheets(filename)

# MRN should be loaded as chr to avoid loss of leading 0
raw_data <- readxl::read_xlsx(filename)


## Filter for pt populations
# Pt population: "home or self-care" under the column "D/C disposition"
#   and: "med LB 45a, 45b, 45c, 49a, 49b, 49c, 54a, 55a, 55b, attending only, NP" under the column "primary team"
#   and: up to Jan 20222
team_list <- c("MED LB 45A", "MED LB 45B", "MED LB 45C", "MED LB 49A", "MED LB 49B", "MED LB 49C", "MED LB 54A", "MED LB 55A", "MED LB 55B", "MED LB ATTENDING ONLY", "MED LB NP")
filtered_data <- raw_data %>% filter(`D/C Disposition` %in% c("Home - under care of Home Health Services", "Home or Self-Care"), 
                                     `Primary Team` %in% team_list,
                                     `Hospital Discharge Date/Time` <= as.POSIXct("2022-02-01 10:50:00", tz = "UTC"))


# Similar columns from sheet1 and sheet2
# `Drg Apr Desc.sheet1`, `Drg Federal Desc.sheet1`, `Insurance Product.sheet1`, `Neighborhood.sheet1`, `CMI Fed`, `Cmi Federal`, `Race IP Dis`, `Enc - Race Desc`, `Patient Race`, `Zip Code`, `Patient Zip (without extension)`, `Patient Zip`
# only the two Neighborhood columns are combined since they are mostly equivalent
filtered_data <- filtered_data %>% 
  mutate(Neighborhood = if_else(Neighborhood.sheet1 != Neighborhood.sheet2, "BKN-Sunset Park/Boro Park", Neighborhood.sheet1)) %>% 
  select(-c(Neighborhood.sheet1, Neighborhood.sheet2))


## Fix NA values
# Fix Race NA variable "0"
filtered_data$`Enc - Race Desc`[filtered_data$`Enc - Race Desc` == 0] <- NA
# Referral Status, NA to "Not Referred"
filtered_data$`Referral Status`[is.na(filtered_data$`Referral Status`)] <- "Not Referred"
filtered_data$`Referral Status`[filtered_data$`Referral Status`== "Referred, Not Completed"] <- "Not Completed, Referred"


## Fix Boolean Variables
# Fix Variable Types, convert ("Y", NA, NA) to (TRUE, FALSE, FALSE)
filtered_data$`Scheduled TCM w/in 10 days` <- if_else(filtered_data$`Scheduled TCM w/in 10 days`=="Y", T, F)
filtered_data$`Scheduled TCM w/in 10 days`[is.na(filtered_data$`Scheduled TCM w/in 10 days`)] <- FALSE
filtered_data$`Readmit w/in 10` <- if_else(filtered_data$`Readmit w/in 10`=="Y", T, F)
filtered_data$`Readmit w/in 10`[is.na(filtered_data$`Readmit w/in 10`)] <- FALSE
filtered_data$`Readmit w/in 20` <- if_else(filtered_data$`Readmit w/in 20`=="Y", T, F)
filtered_data$`Readmit w/in 20`[is.na(filtered_data$`Readmit w/in 20`)] <- FALSE
filtered_data$`Readmit w/in 30` <- if_else(filtered_data$`Readmit w/in 30`=="Y", T, F)
filtered_data$`Readmit w/in 30`[is.na(filtered_data$`Readmit w/in 30`)] <- FALSE
filtered_data$COMORBIDITY_FLAG <- if_else(filtered_data$COMORBIDITY_FLAG=="Y", T, F)
filtered_data$COMORBIDITY_FLAG[is.na(filtered_data$COMORBIDITY_FLAG)] <- FALSE
# Recode "Yes","No" to TRUE,FALSE
bool_vars <- c("Discharge Summary Reviewed", "Medications Confirmed Visually", "Specialty Appointments Scheduled on the Day of Hospital Discharge", "Discrepancy Between Discharge Medication List and Today's Visit", "Patient in Possession of all Prescribed Medications", "Possession of Prescribed Meds", "PCP Was Routed This Note", "Discharging Resident Was Routed This Note", "Patient Connected to any Community Resources")
bool_cols <- map_dfc(.x = bool_vars, .f = function(x) recode(filtered_data[[x]], Yes = T, No = F, .default = NA))
colnames(bool_cols) <- bool_vars
walk(.x = bool_vars, .f = function(x) filtered_data[x] <<- bool_cols[x] )
# Convert numeric to logical
filtered_data$`No Show` <- as.logical(filtered_data$`No Show`)
filtered_data$Referred <- as.logical(filtered_data$Referred)
filtered_data$`No PCP` <- as.logical(filtered_data$`No PCP`)



#### IN DEVELOPMENT
# classify dx and readmit dx codes in `Admission Dx` `Readmission Dx`


#### IN DEVELOPMENT



## View Cleaned Data
glimpse(filtered_data)

## Save Cleaned Data
write_csv(filtered_data, file = sub(".xlsx{1}$", "__Cleaned.csv",filename) )
