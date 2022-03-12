# Authors: Sean D McAtee, Sandy Carrillo-Argueta
# Version: 2022-03-09



library(gtsummary)
library(tidyverse)
library(ggExtra)

filename <- file.choose()
readmiss_data <- read_csv(filename)

# Set Theme
theme_gtsummary_journal(journal = "jama")

## Insurance with status
table(readmiss_data$`Referral Status`, readmiss_data$`Insurance Product`)
chisq.test(readmiss_data$`Referral Status`, readmiss_data$`Insurance Product`, correct=FALSE)


## Multinomial to find difference of `Referral Status` fit on covars
# `Not Referred` as baseline
## readmit 10,20, 30
library(nnet)

readmiss_data$`Patient Race`
readmiss_data$`Referral Status` <- relevel(as.factor(readmiss_data$`Referral Status`), ref = "Not Referred")

multinom_referral <- nnet::multinom(`Readmit w/in 10` ~`Referral Status` + LOS + `Patient Age` +`Insurance Product`, data = readmiss_data)
summary(multinom_referral)

logit_readmit10 <- glm(`Readmit w/in 10` ~ `Referral Status` + LOS + `Patient Age` + `Insurance Product` + `Patient Race`, data = readmiss_data, family = binomial)
summary(logit_readmit10)

logit_readmit20 <-glm(`Readmit w/in 20` ~ `Referral Status` + LOS + `Patient Age` + `Insurance Product` + `Patient Race`, data = readmiss_data, family = binomial)
summary(logit_readmit20)

logit_readmit30 <-glm(`Readmit w/in 30` ~ `Referral Status` + LOS + `Patient Age` + `Insurance Product` + `Patient Race`, data = readmiss_data, family = binomial)
summary(logit_readmit30)


linmod_readmitdays <- lm(`Readmit Days after Discharge` ~ `Referral Status` + `LOS` + `Enc - Age` + `Enc - Gender` + `CMI Fed`, data = readmiss_data)
summary(linmod_readmitdays)



# `Patient Age`, gender, LOS, `Type of Visit`, `No PCP`, `Insurance Product`
glm(`Readmit w/in 10` ~ `Referral Status` + `Patient Age` + LOS + `Type of Visit` + `No PCP` + `Insurance Product`, data = readmiss_data, family = binomial) %>% summary()
lm(`Readmit w/in 10` ~ `Referral Status` + `Patient Age` + LOS + `Type of Visit` + `No PCP` + `Insurance Product`, data = readmiss_data) %>% summary()
