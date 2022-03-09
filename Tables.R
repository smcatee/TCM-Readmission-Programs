# Authors: Sean D McAtee, Sandy Carrillo-Argueta
# Version: 2022-03-09


### Items to update
# Maybe use median/IQR for skewed data
# in tables list missing even for continuous


# Libraries
library(gtsummary)
library(tidyverse)

filename <- file.choose()
readmiss_data <- read_csv(filename)

# Set Theme
theme_gtsummary_journal(journal = "jama")

colnames(readmiss_data)
# check n when grouped by potential covars

## Treatment: `Referral Status`
readmiss_data$`Referral Status` %>% table(useNA = "always")


## include PCP, telephone visits
readmiss_data %>%  select( `Referral Status`, `No PCP`) %>% 
  tbl_summary(by = `Referral Status`,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 2,
              missing_text = "Missing"
  ) %>% 
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Referral Status**") %>%
  add_p()

readmiss_data$`Patient Race` %>% table()
## Demographic Vars
# Completed vs not completed vs not referred
readmiss_data %>% select( `Referral Status`, Age = "Patient Age",  Race = "Patient Race Recode") %>% 
  tbl_summary(by = `Referral Status`,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 2,
              missing_text = "Missing"
  ) %>% 
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Referral Status**") %>%
  add_p()

# Completed vs not completed
readmiss_data %>% filter(`Referral Status` != "Not Referred") %>% 
  select( `Referral Status`, Age = "Patient Age", Race = "Patient Race Recode") %>% 
  tbl_summary(by = `Referral Status`,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 2,
              missing_text = "Missing"
  ) %>% 
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Referral Status**") %>%
  add_p()




## Visit Vars
# Comparing LOS to Days until readmission for 
plot_los_read <- readmiss_data %>% 
  ggplot(aes(x=`LOS`, y=`Readmit Days after Discharge`, colour = `Referral Status`)) + 
  geom_point(alpha = 0.6, size = 1, position = "jitter") +
  xlim(0,30)
ggMarginal(plot_los_read, groupColour = TRUE, groupFill = TRUE)



# Med Vars
readmiss_data %>% 
  select(`Referral Status`, "Med Discrepancy" = `Discrepancy Between Discharge Medication List and Today's Visit`,
         "Patient Has All Meds" = `Patient in Possession of all Prescribed Medications`,
         `Cmi Federal`) %>% 
  tbl_summary(by = `Referral Status`,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 2,
              missing_text = "Missing"
  ) %>% 
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Referral Status**") %>%
  add_p()

# Insurance Group
readmiss_data %>% 
  select(`Referral Status`, `Insurance Product`) %>% 
  tbl_summary(by = `Referral Status`,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 2,
              missing_text = "Missing"
  ) %>% 
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Referral Status**") %>%
  add_p()

# Virtual or In Person
readmiss_data %>% 
  select(`Referral Status`, `Type of Visit`) %>% 
  filter(`Referral Status` != "Not Referred") %>% 
  tbl_summary(by = `Referral Status`,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 2,
              missing_text = "Missing"
  ) %>% 
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Referral Status**") %>%
  add_p()



# Scheduling/Readmit Vars
readmiss_data %>% 
  select(`Referral Status`, "Readmit Days after Discharge", "Readmit w/in 10", 
         "Readmit w/in 20", "Readmit w/in 30")  %>% 
  tbl_summary(by = `Referral Status`,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 2,
              missing_text = "No Readmit in 30 Days"
  ) %>% 
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Referral Status**") %>%
  add_p()

# Completed vs Not Completed
readmiss_data %>% filter(`Referral Status` != "Not Referred") %>% 
  select(`Referral Status`, "Readmit Days after Discharge", "Readmit w/in 10", 
         "Readmit w/in 20", "Readmit w/in 30")  %>% 
  tbl_summary(by = `Referral Status`,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 2,
              missing_text = "No Readmit in 30 Days"
  ) %>% 
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Referral Status**") %>%
  add_p()

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
