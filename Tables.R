# Authors: Sean D McAtee, Sandy Carrillo-Argueta
# Version: 2022-03-09

# Libraries
library(gtsummary)
library(tidyverse)

filename <- file.choose()
readmiss_data <- read_csv(filename)

# Set Theme
theme_gtsummary_journal(journal = "jama")



## Referral Status vs ...

table_summary_p <- function(var_list){
  # Make Table without P values
  t0 <- readmiss_data %>% select( c(`Referral Status`, var_list)) %>% 
    tbl_summary(by = `Referral Status`,
                statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)"),
                digits = all_continuous() ~ 2,
                missing_text = "Missing"
    )
  # Add P values
  t1 <- readmiss_data %>% select(c(`Referral Status`, var_list)) %>%
    filter(`Referral Status` %in% c("Completed, Referred", "Not Completed, Referred")) %>%
    tbl_summary(by = `Referral Status`, missing = "no") %>%
    add_p() %>%
    modify_header(p.value ~ "**C-R vs NC-R**") %>%
    modify_column_hide(all_stat_cols()) # hide summary stat columns
  
  t2 <- readmiss_data %>% select(c(`Referral Status`, var_list)) %>%
    tbl_summary(by = `Referral Status`, missing = "no") %>%
    add_p() %>%
    modify_header(p.value ~ "**All**") %>%
    modify_column_hide(all_stat_cols()) # hide summary stat columns
  # merging the 3 tables together, and adding additional gt formatting
  tbl_merge(list(t0, t1, t2)) %>%
    modify_header(label ~ "**Variable**") %>% 
    modify_spanning_header(
      list(all_stat_cols() ~ "**Referral Status**", starts_with("p.value") ~ "**p-values**")
     )
}


## Readmit w/in X

table_summary_p2 <- function(var_list){
  # Make Table without P values
  readmiss_data %>% filter(!is.na(`Readmit w/in X`)) %>% 
    select( c(`Readmit w/in X`, var_list)) %>% 
    tbl_summary(by = `Readmit w/in X`,
                statistic = list(all_continuous() ~ "{mean} ({sd})",
                                 all_categorical() ~ "{n} ({p}%)"),
                digits = all_continuous() ~ 2,
                missing_text = "Missing"
    ) %>% 
    add_p() %>%
    modify_header(label ~ "**Variable**")
}



## ... vs Variables
variable_list <- c("Referral Status")
variable_list <- c("Readmit Days after Discharge")
variable_list <- c("Readmit w/in 10", "Readmit w/in 20", "Readmit w/in 30")
variable_list <- c(Age = "Patient Age",  Race = "Patient Race ReRecode")
variable_list <- c("Insurance Product", "No PCP")
variable_list <- c("LOS")
variable_list <- c("Type of Visit", "TCM Location") 
variable_list <- c("Type of Visit") 
variable_list <- c("Had D/C Challenges" = "Discharge Challenges T/F", "Medications Confirmed Visually", "Specialty Appointments Scheduled on the Day of Hospital Discharge", "Discrepancy Between Discharge Medication List and Today's Visit", "Is there a Discrepancy between Discharge Medication List and Today's Visit", "Patient in Possession of all Prescribed Medications", "Is Patient in Possession of All Prescribed Medications", "Possession of Prescribed Meds", "Patient Have an Onsite Caregiver", "Onsite Caregiver Participating in Virtual Visit", "Patient Connected to any Community Resources", "Medication Reconciliation was Attempted or Completed", "Were All Home Care Services in Place at time of This Visit", "If no, Were We Able to Resulve This")


## Make tables
table_summary_p(variable_list)
table_summary_p2(variable_list)








## Plots

# Plot density of readmit days after dc, with percent readmissions
readmiss_data %>% group_by(`Referral Status`) %>% 
  mutate(`Plot Labels Referral Status` = str_c( `Referral Status`, "\n(", signif(mean(!is.na(`Readmit Days after Discharge`))*100, 2), "% readmitted in 30d)")) %>% 
  ggplot( aes(fill = `Plot Labels Referral Status`, x = `Readmit Days after Discharge`)) +
  geom_density(alpha = 0.5, trim = T) 



# `Readmit Days after Discharge` Histogram with NA
readmiss_data %>% group_by(`Readmit Days after Discharge`) %>% count(`Readmit Days after Discharge`) %>% 
  ggplot(aes(x = as.factor(`Readmit Days after Discharge`), y = n )) +
  geom_bar(stat = 'identity')


# Returned in 30 days, grouped by Referral status
readmiss_data %>% ggplot(aes(fill = `Referral Status`, x = `Readmit w/in 30`)) +
  geom_histogram(stat="count") + scale_fill_manual(values=c("#a8a8a8", "#828282", "#5d5d5d")) +
  labs(title = "Readmitted within 30 days, grouped by Referral Status")



# Map Plot Zip
# vs `Referral Status`, `Type of Visit`, `TCM Location`, 
#   Type of visit, clinics are in BK
# clinic locations:
library(rgeos)
library(maptools)
library(geojsonio)
library(ggthemes)


URL <- "https://data-beta-nyc-files.s3.amazonaws.com/resources/6df127b1-6d04-4bb7-b983-07402a2c3f90/f4129d9aa6dd4281bc98d0f701629b76nyczipcodetabulationareas.geojson?Signature=Q9nL0edVD8P9Hh%2BCZtVGa1ssPQc%3D&Expires=1646868303&AWSAccessKeyId=AKIAWM5UKMRH2KITC3QA"
fil <- "nyc_zip.geojson"
if (!file.exists(fil)) download.file(URL, fil)
nyc_zips <- geojson_read(fil, what="sp")
nyc_zips_map <- broom::tidy(nyc_zips, region = "postalCode")


gg <- ggplot()
gg <- gg + geom_map(data = nyc_zips_map, map = nyc_zips_map,
                    aes(x=long, y=lat, map_id=id),
                    color="#2b2b2b", size=0.15, fill=NA)
gg <- gg + geom_map(data = filter(readmiss_data, `Referral Status`!="Not Referred"), map = nyc_zips_map,
                    aes(fill = `Referral Status`, map_id = `Patient Zip (without extension)`),
                    color="#2b2b2b", size=0.15)
gg <- gg + scale_fill_viridis(name="Referral Status")
gg <- gg + coord_map()
gg <- gg + ggthemes::theme_map()
gg <- gg + theme(legend.position=c(0.1,0.5))
gg





# 3d scatter plot
library(plotly)

plot_ly(readmiss_data, x = ~`Enc - Age`, y = ~`Readmit Days after Discharge`, z = ~`Referral Status`, color = ~`Referral Status`, colors = c("#a8a8a8", "#828282", "#5d5d5d"))
