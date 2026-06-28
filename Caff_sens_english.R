library(tidyverse)

data <- read_csv("Caffeine sensitivity ENGLISH.csv")

glimpse(data)

data_clean <- data %>%
  slice(-(1:2)) %>%
  mutate(across(
    .cols = c(gender, occupation, caff_sens, starts_with("caff_intox")),
    .fns = parse_number
  ))

install.packages("lavaan")
install.packages("semTools")
library(lavaan)
library(semTools)

model <- 'factor1 =~ caff_intox_revised_1 + caff_intox_revised_2 + caff_intox_revised_3 + 
                    caff_intox_revised_4 + caff_intox_revised_5 + caff_intox_revised_6 + caff_intox_revised_7
                    + caff_intox_revised_8 + caff_intox_revised_9 + caff_intox_revised_10
                    + caff_intox_revised_11 + caff_intox_revised_12'

fit <- cfa(model,
  data= data_clean,
  estimator= "WLSMV",
  ordered= c("caff_intox_revised_1", "caff_intox_revised_2", "caff_intox_revised_3", "caff_intox_revised_4",
             "caff_intox_revised_5", "caff_intox_revised_6", "caff_intox_revised_7", "caff_intox_revised_8", 
             "caff_intox_revised_9", "caff_intox_revised_10", "caff_intox_revised_11", "caff_intox_revised_12")
  )

summary(fit, fit.measures= TRUE, standardized= TRUE)
standardizedsolution(fit)
reliability(fit)

#Remove item 7 to evaluate model fit

items_full <- paste0("caff_intox_revised_", 1:12)
items1 <- items_full[items_full != "caff_intox_revised_7"]

model2 <- paste0("factor1 =~ ", paste(items1, collapse = " + "))
fit2 <- cfa(model2,
            data= data_clean,
            estimator= "WLSMV",
            ordered = items1)
summary(fit2, fit.measures= TRUE, standardized= TRUE)
standardizedsolution(fit2)
reliability(fit2)

#Remove item 7 and 4

items2 <- items1[items1 != "caff_intox_revised_4"]
model3 <- paste0("factor1 =~ ", paste(items2, collapse = " + "))
fit3 <- cfa(model3,
            data= data_clean,
            estimator= "WLSMV",
            ordered = items2)
summary(fit3, fit.measures= TRUE, standardized= TRUE)
standardizedsolution(fit3)
reliability(fit3)

fitMeasures(fit, c("cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr"))
fitMeasures(fit2, c("cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr"))
fitMeasures(fit3, c("cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr"))

