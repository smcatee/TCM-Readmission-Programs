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

# Plot scatter readmit days after dc, with percent readmissions
readmit_percent <- readmiss_data %>% group_by(`Referral Status`) %>% summarise(readmitted = mean(!is.na(`Readmit Days after Discharge`)))

ggplot(data = readmiss_data, aes(x = `Referral Status`, y = `Readmit Days after Discharge`)) +
  geom_boxplot() +
  geom_text(data = readmit_percent, aes(x = `Referral Status`, label = paste0(signif(readmitted, 4)*100, "%"), y = 32)) +
  geom_text(aes(x = 0.62, y = 32, label = "(Readmit %)"))

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
library(zipcode)
library(maps)
library(viridis)
library(ggthemes)

map_data()

maps::state()