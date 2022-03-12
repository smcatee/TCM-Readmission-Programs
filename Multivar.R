# Authors: Sean D McAtee, Sandy Carrillo-Argueta
# Version: 2022-03-09



library(gtsummary)
library(tidyverse)
library(ggExtra)

filename <- file.choose()
readmiss_data <- read_csv(filename)

# Set Theme
theme_gtsummary_journal(journal = "jama")


## Multinomial to find difference of `Referral Status` fit on covars
# `Not Referred` as baseline
library(nnet)
readmiss_data$`Referral Status` <- relevel(as.factor(readmiss_data$`Referral Status`), ref = "Not Referred")

multinom_referral <- nnet::multinom(`Referral Status` ~ `LOS` + `Enc - Age` + `Enc - Gender` + `Ethnicity Desc`, data = readmiss_data)
summary(multinom_referral)

logit_readmit10 <- glm(`Readmit w/in 10` ~ `Referral Status` + `LOS` + `Enc - Age` + `Enc - Gender` + `CMI Fed`, data = readmiss_data, family = binomial)
summary(logit_readmit10)

linmod_readmitdays <- lm(`Readmit Days after Discharge` ~ `Referral Status` + `LOS` + `Enc - Age` + `Enc - Gender` + `CMI Fed`, data = readmiss_data)
summary(linmod_readmitdays)



# `Patient Age`, gender, LOS, `Type of Visit`, `No PCP`, `Insurance Product`
glm(`Readmit w/in 10` ~ `Referral Status` + `Patient Age` + LOS + `Type of Visit` + `No PCP` + `Insurance Product`, data = readmiss_data, family = binomial) %>% summary()
lm(`Readmit w/in 10` ~ `Referral Status` + `Patient Age` + LOS + `Type of Visit` + `No PCP` + `Insurance Product`, data = readmiss_data) %>% summary()
