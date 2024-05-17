# Packages ----

library(haven)
library(dagitty)
library(mice)
library(lavaan)
library(survival)
library(riskRegression)
library(prodlim)
library(rms)
library(ggplot2)

# Data ----

...

# Subsets ----

## Participants without diabetes ----

data.no_diabetes <- subset(data, GTS.diabetes_all_types_1 == 0)
nrow(data.no_diabetes)

## Participants with diabetes ----

data.diabetes <- subset(data, GTS.diabetes_all_types_1 == 1)
nrow(data.diabetes)

# Descriptive analysis ----

## Participants without diabetes ----

table(data.no_diabetes$event, exclude = NULL)
prop.table(table(data.no_diabetes$event, exclude = NULL))

summary(data.no_diabetes$time)
sd(data.no_diabetes$time, na.rm = TRUE)

summary(data.no_diabetes$max_time)
sd(data.no_diabetes$max_time, na.rm = TRUE)

table(data.no_diabetes$sex.male_1, exclude = NULL)
prop.table(table(data.no_diabetes$sex.male_1, exclude = NULL))

summary(data.no_diabetes$age)
sd(data.no_diabetes$age, na.rm = TRUE)

table(data.no_diabetes$pre_existing_ASCVD.yes_1, exclude = FALSE)
prop.table(table(data.no_diabetes$pre_existing_ASCVD.yes_1, exclude = FALSE))

summary(data.no_diabetes$SES)
sd(data.no_diabetes$SES, na.rm = TRUE)
sum(is.na(data.no_diabetes$SES)) / nrow(data.no_diabetes)

table(data.no_diabetes$smoking.current_1, exclude = FALSE)
prop.table(table(data.no_diabetes$smoking.current_1, exclude = FALSE))

summary(data.no_diabetes$MVPA)
sd(data.no_diabetes$MVPA, na.rm = TRUE)
sum(is.na(data.no_diabetes$MVPA)) / nrow(data.no_diabetes)

summary(data.no_diabetes$DHD)
sd(data.no_diabetes$DHD, na.rm = TRUE)
sum(is.na(data.no_diabetes$DHD)) / nrow(data.no_diabetes)

## Participants with diabetes ----

table(data.diabetes$event, exclude = NULL)
prop.table(table(data.diabetes$event, exclude = NULL))

summary(data.diabetes$time)
sd(data.diabetes$time, na.rm = TRUE)

summary(data.diabetes$max_time)
sd(data.diabetes$max_time, na.rm = TRUE)

table(data.diabetes$sex.male_1, exclude = NULL)
prop.table(table(data.diabetes$sex.male_1, exclude = NULL))

summary(data.diabetes$age)
sd(data.diabetes$age, na.rm = TRUE)

table(data.diabetes$pre_existing_ASCVD.yes_1, exclude = FALSE)
prop.table(table(data.diabetes$pre_existing_ASCVD.yes_1, exclude = FALSE))

summary(data.diabetes$SES)
sd(data.diabetes$SES, na.rm = TRUE)
sum(is.na(data.diabetes$SES)) / nrow(data.diabetes)

table(data.diabetes$smoking.current_1, exclude = FALSE)
prop.table(table(data.diabetes$smoking.current_1, exclude = FALSE))

summary(data.diabetes$MVPA)
sd(data.diabetes$MVPA, na.rm = TRUE)
sum(is.na(data.diabetes$MVPA)) / nrow(data.diabetes)

summary(data.diabetes$DHD)
sd(data.diabetes$DHD, na.rm = TRUE)
sum(is.na(data.diabetes$DHD)) / nrow(data.diabetes)

# Imputations ----

## Number of imputations ----

n_imp <- 15

## Participants without diabetes ----

imp.no_diabetes <- data.no_diabetes[, c( "event",
                                         "time",
                                         "sex.male_1",
                                         "age",
                                         "pre_existing_ASCVD.yes_1",
                                         "SES",
                                         "smoking.current_1",
                                         "MVPA",
                                         "DHD")]
imp.no_diabetes <- sapply(imp.no_diabetes, zap_labels)
imp.no_diabetes <- mice(imp.no_diabetes, method = "pmm", m = n_imp, seed = ...)
imp.no_diabetes.stacked <- list()
for (i in 1:n_imp) {
  imp.no_diabetes.stacked[[i]] <- complete(imp.no_diabetes, i)
  imp.no_diabetes.stacked[[i]][["ASCVD_composite_event"]] <- ifelse(imp.no_diabetes.stacked[[i]][["event"]] == 1,
                                                                    1,
                                                                    0) 
  imp.no_diabetes.stacked[[i]][["overall_mortality_excl_ASCVD_mortality_event"]] <- ifelse(imp.no_diabetes.stacked[[i]][["event"]] == 2,
                                                                                           1,
                                                                                           0) 
}

for (i in 1:n_imp) {
  print(paste("imputation", i))
  
  print(table(imp.no_diabetes.stacked[[i]]$pre_existing_ASCVD.yes_1, exclude = FALSE))
  print(prop.table(table(imp.no_diabetes.stacked[[i]]$pre_existing_ASCVD.yes_1, exclude = FALSE)))
  
  print(summary(imp.no_diabetes.stacked[[i]]$SES))
  print(sd(imp.no_diabetes.stacked[[i]]$SES))
  
  print(table(imp.no_diabetes.stacked[[i]]$smoking.current_1, exclude = FALSE))
  print(prop.table(table(imp.no_diabetes.stacked[[i]]$smoking.current_1, exclude = FALSE)))
  
  print(summary(imp.no_diabetes.stacked[[i]]$MVPA))  
  print(sd(imp.no_diabetes.stacked[[i]]$MVPA))  
  
  print(summary(imp.no_diabetes.stacked[[i]]$DHD))
  print(sd(imp.no_diabetes.stacked[[i]]$DHD))
}

## Participants with diabetes ----

imp.diabetes <- data.diabetes[, c( "event",
                                   "time",
                                   "sex.male_1",
                                   "age",
                                   "pre_existing_ASCVD.yes_1",
                                   "SES",
                                   "smoking.current_1",
                                   "MVPA",
                                   "DHD")]
imp.diabetes <- sapply(imp.diabetes, zap_labels)
imp.diabetes <- mice(imp.diabetes, method = "pmm", m = n_imp, seed = ...)
imp.diabetes.stacked <- list()
for (i in 1:n_imp) {
  imp.diabetes.stacked[[i]] <- complete(imp.diabetes, i)
  imp.diabetes.stacked[[i]][["ASCVD_composite_event"]] <- ifelse(imp.diabetes.stacked[[i]][["event"]] == 1,
                                                                 1,
                                                                 0) 
  imp.diabetes.stacked[[i]][["overall_mortality_excl_ASCVD_mortality_event"]] <- ifelse(imp.diabetes.stacked[[i]][["event"]] == 2,
                                                                                        1,
                                                                                        0) 
}

for (i in 1:n_imp) {
  print(paste("imputation", i))
  
  print(table(imp.diabetes.stacked[[i]]$pre_existing_ASCVD.yes_1, exclude = FALSE))
  print(prop.table(table(imp.diabetes.stacked[[i]]$pre_existing_ASCVD.yes_1, exclude = FALSE)))
  
  print(summary(imp.diabetes.stacked[[i]]$SES))
  print(sd(imp.diabetes.stacked[[i]]$SES))

  print(table(imp.diabetes.stacked[[i]]$smoking.current_1, exclude = FALSE))
  print(prop.table(table(imp.diabetes.stacked[[i]]$smoking.current_1, exclude = FALSE)))
  
  print(summary(imp.diabetes.stacked[[i]]$MVPA))  
  print(sd(imp.diabetes.stacked[[i]]$MVPA))  
  
  print(summary(imp.diabetes.stacked[[i]]$DHD))
  print(sd(imp.diabetes.stacked[[i]]$DHD))
}

# Causal structure ----

## DAG ----

dag <- dagitty("dag {
  sex.male_1 -> age.ord
  sex.male_1 -> SES.ord
  sex.male_1 -> pre_existing_ASCVD.yes_1
  sex.male_1 -> smoking.current_1
  sex.male_1 -> MVPA.ord
  sex.male_1 -> DHD.ord
  sex.male_1 -> overall_mortality_excl_ASCVD_mortality_event
  sex.male_1 -> ASCVD_composite_event
  age.ord -> SES.ord
  age.ord -> pre_existing_ASCVD.yes_1
  age.ord -> smoking.current_1
  age.ord -> MVPA.ord
  age.ord -> DHD.ord
  age.ord -> overall_mortality_excl_ASCVD_mortality_event
  age.ord -> ASCVD_composite_event
  SES.ord -> pre_existing_ASCVD.yes_1
  SES.ord -> smoking.current_1
  SES.ord -> MVPA.ord
  SES.ord -> DHD.ord
  SES.ord -> overall_mortality_excl_ASCVD_mortality_event
  SES.ord -> ASCVD_composite_event
  pre_existing_ASCVD.yes_1 -> smoking.current_1
  pre_existing_ASCVD.yes_1 -> MVPA.ord
  pre_existing_ASCVD.yes_1 -> DHD.ord
  pre_existing_ASCVD.yes_1 -> ASCVD_composite_event
  smoking.current_1 -> overall_mortality_excl_ASCVD_mortality_event
  smoking.current_1 -> ASCVD_composite_event
  MVPA.ord -> overall_mortality_excl_ASCVD_mortality_event
  MVPA.ord -> ASCVD_composite_event
  DHD.ord -> overall_mortality_excl_ASCVD_mortality_event
  DHD.ord -> ASCVD_composite_event
  overall_mortality_excl_ASCVD_mortality_event -> ASCVD_composite_event
}")

## Investigating conditional independencies ----

impliedConditionalIndependencies(dag)

### Participants without diabetes ----

#### Point estimates ----

no_diabetes.cond_ind.stacked <- list()
for (i in 1:n_imp) {
  
  # Data 
  
  temp.data <- imp.no_diabetes.stacked[[i]]
  
  # Define ordinal categorical variables 
  
  temp.data$age.ord <- ifelse(temp.data$age <= quantile(temp.data$age, 0.25)[[1]],
                              1,
                              0)
  temp.data$age.ord <- ifelse((temp.data$age > quantile(temp.data$age, 0.25)[[1]])
                              &
                              (temp.data$age <= quantile(temp.data$age, 0.50)[[1]]),
                              2,
                              temp.data$age.ord)
  temp.data$age.ord <- ifelse((temp.data$age > quantile(temp.data$age, 0.50)[[1]])
                              &
                              (temp.data$age <= quantile(temp.data$age, 0.75)[[1]]),
                              3,
                              temp.data$age.ord)
  temp.data$age.ord <- ifelse(temp.data$age > quantile(temp.data$age, 0.75)[[1]],
                              4,
                              temp.data$age.ord)
  temp.data$age.ord <- ordered(temp.data$age.ord)
  
  temp.data$SES.ord <- ifelse(temp.data$SES <= quantile(temp.data$SES, 0.25)[[1]],
                              1,
                              0)
  temp.data$SES.ord <- ifelse((temp.data$SES > quantile(temp.data$SES, 0.25)[[1]])
                              &
                              (temp.data$SES <= quantile(temp.data$SES, 0.50)[[1]]),
                              2,
                              temp.data$SES.ord)
  temp.data$SES.ord <- ifelse((temp.data$SES > quantile(temp.data$SES, 0.50)[[1]])
                              &
                              (temp.data$SES <= quantile(temp.data$SES, 0.75)[[1]]),
                              3,
                              temp.data$SES.ord)
  temp.data$SES.ord <- ifelse(temp.data$SES > quantile(temp.data$SES, 0.75)[[1]],
                              4,
                              temp.data$SES.ord)
  temp.data$SES.ord <- ordered(temp.data$SES.ord)
  
  temp.data$MVPA.ord <- ifelse(temp.data$MVPA <= quantile(temp.data$MVPA, 0.25)[[1]],
                               1,
                               0)
  temp.data$MVPA.ord <- ifelse((temp.data$MVPA > quantile(temp.data$MVPA, 0.25)[[1]])
                               &
                               (temp.data$MVPA <= quantile(temp.data$MVPA, 0.50)[[1]]),
                               2,
                               temp.data$MVPA.ord)
  temp.data$MVPA.ord <- ifelse((temp.data$MVPA > quantile(temp.data$MVPA, 0.50)[[1]])
                               &
                               (temp.data$MVPA <= quantile(temp.data$MVPA, 0.75)[[1]]),
                               3,
                               temp.data$MVPA.ord)
  temp.data$MVPA.ord <- ifelse(temp.data$MVPA > quantile(temp.data$MVPA, 0.75)[[1]],
                               4,
                               temp.data$MVPA.ord)
  temp.data$MVPA.ord <- ordered(temp.data$MVPA.ord)
  
  temp.data$DHD.ord <- ifelse(temp.data$DHD <= quantile(temp.data$DHD, 0.25)[[1]],
                              1,
                              0)
  temp.data$DHD.ord <- ifelse((temp.data$DHD > quantile(temp.data$DHD, 0.25)[[1]])
                              &
                              (temp.data$DHD <= quantile(temp.data$DHD, 0.50)[[1]]),
                              2,
                              temp.data$DHD.ord)
  temp.data$DHD.ord <- ifelse((temp.data$DHD > quantile(temp.data$DHD, 0.50)[[1]])
                              &
                              (temp.data$DHD <= quantile(temp.data$DHD, 0.75)[[1]]),
                              3,
                              temp.data$DHD.ord)
  temp.data$DHD.ord <- ifelse(temp.data$DHD > quantile(temp.data$DHD, 0.75)[[1]],
                              4,
                              temp.data$DHD.ord)
  temp.data$DHD.ord <- ordered(temp.data$DHD.ord)
  
  # Define binary variables
  
  temp.data$sex.male_1 <- as.integer(temp.data$sex.male_1)
  
  temp.data$pre_existing_ASCVD.yes_1 <- as.integer(temp.data$pre_existing_ASCVD.yes_1)
  
  temp.data$smoking.current_1 <- as.integer(temp.data$smoking.current_1)
  
  temp.data$ASCVD_composite_event <- as.integer(temp.data$ASCVD_composite_event)
  
  temp.data$overall_mortality_excl_ASCVD_mortality_event <- as.integer(temp.data$overall_mortality_excl_ASCVD_mortality_event)
  
  # Unadjusted correlations
  
  temp.cor <- lavCor(temp.data[, c("sex.male_1",
                                   "age.ord",
                                   "pre_existing_ASCVD.yes_1",
                                   "SES.ord",
                                   "smoking.current_1",
                                   "MVPA.ord",
                                   "DHD.ord",
                                   "ASCVD_composite_event",
                                   "overall_mortality_excl_ASCVD_mortality_event")])
  
  # Adjusted correlations 
  
  no_diabetes.cond_ind.stacked[[i]] <-  localTests(dag,
                                                   sample.cov = temp.cor,
                                                   sample.nobs = nrow(temp.data))
}

no_diabetes.cond_ind.1 <- c()
for (i in 1:n_imp) {
  no_diabetes.cond_ind.1 <- append(no_diabetes.cond_ind.1, no_diabetes.cond_ind.stacked[[i]][[1]][[1]])
}
mean.no_diabetes.cond_ind.1 <- mean(no_diabetes.cond_ind.1)
mean.no_diabetes.cond_ind.1

no_diabetes.cond_ind.2 <- c()
for (i in 1:n_imp) {
  no_diabetes.cond_ind.2 <- append(no_diabetes.cond_ind.2, no_diabetes.cond_ind.stacked[[i]][[1]][[2]])
}
mean.no_diabetes.cond_ind.2 <- mean(no_diabetes.cond_ind.2)
mean.no_diabetes.cond_ind.2

no_diabetes.cond_ind.3 <- c()
for (i in 1:n_imp) {
  no_diabetes.cond_ind.3 <- append(no_diabetes.cond_ind.3, no_diabetes.cond_ind.stacked[[i]][[1]][[3]])
}
mean.no_diabetes.cond_ind.3 <- mean(no_diabetes.cond_ind.3)
mean.no_diabetes.cond_ind.3

no_diabetes.cond_ind.4 <- c()
for (i in 1:n_imp) {
  no_diabetes.cond_ind.4 <- append(no_diabetes.cond_ind.4, no_diabetes.cond_ind.stacked[[i]][[1]][[4]])
}
mean.no_diabetes.cond_ind.4 <- mean(no_diabetes.cond_ind.4)
mean.no_diabetes.cond_ind.4

#### Bootstrapped confidence intervals ----

# Seed 

set.seed(...)

# Number of bootstraps 

n_b <- 150

# Creating object to capture output 

boot.mean.no_diabetes.cond_ind <- as.data.frame(matrix(nrow = n_b,
                                                       ncol = 5))
colnames(boot.mean.no_diabetes.cond_ind) <- c("b",
                                              "boot.mean.no_diabetes.cond_ind.1",
                                              "boot.mean.no_diabetes.cond_ind.2",
                                              "boot.mean.no_diabetes.cond_ind.3",
                                              "boot.mean.no_diabetes.cond_ind.4")
boot.mean.no_diabetes.cond_ind$b <- 1:n_b

# Loop 

for (b in 1:n_b) {
  boot.index <- sample(1:nrow(data.no_diabetes), size = nrow(data.no_diabetes), replace = TRUE)
  
  boot.data.no_diabetes <- data.no_diabetes[boot.index, ]
  
  boot.imp.no_diabetes <- boot.data.no_diabetes[, c( "event",
                                                     "time",
                                                     "sex.male_1",
                                                     "age",
                                                     "pre_existing_ASCVD.yes_1",
                                                     "SES",
                                                     "smoking.current_1",
                                                     "MVPA",
                                                     "DHD")]
  boot.imp.no_diabetes <- sapply(boot.imp.no_diabetes, zap_labels)
  boot.imp.no_diabetes <- mice(boot.imp.no_diabetes, method = "pmm", m = n_imp, print = FALSE)
  boot.imp.no_diabetes.stacked <- list()
  for (i in 1:n_imp) {
    boot.imp.no_diabetes.stacked[[i]] <- complete(boot.imp.no_diabetes, i)
    boot.imp.no_diabetes.stacked[[i]][["ASCVD_composite_event"]] <- ifelse(boot.imp.no_diabetes.stacked[[i]][["event"]] == 1,
                                                                           1,
                                                                           0) 
    boot.imp.no_diabetes.stacked[[i]][["overall_mortality_excl_ASCVD_mortality_event"]] <- ifelse(boot.imp.no_diabetes.stacked[[i]][["event"]] == 2,
                                                                                                  1,
                                                                                                  0) 
  }
  
  boot.no_diabetes.cond_ind.stacked <- list()
  for (i in 1:n_imp) {
    
    # Data 
    
    boot.temp.data <- boot.imp.no_diabetes.stacked[[i]]
    
    # Define ordinal categorical variables 
    
    boot.temp.data$age.ord <- ifelse(boot.temp.data$age <= quantile(boot.temp.data$age, 0.25)[[1]],
                                     1,
                                     0)
    boot.temp.data$age.ord <- ifelse((boot.temp.data$age > quantile(boot.temp.data$age, 0.25)[[1]])
                                     &
                                     (boot.temp.data$age <= quantile(boot.temp.data$age, 0.50)[[1]]),
                                     2,
                                     boot.temp.data$age.ord)
    boot.temp.data$age.ord <- ifelse((boot.temp.data$age > quantile(boot.temp.data$age, 0.50)[[1]])
                                     &
                                     (boot.temp.data$age <= quantile(boot.temp.data$age, 0.75)[[1]]),
                                     3,
                                     boot.temp.data$age.ord)
    boot.temp.data$age.ord <- ifelse(boot.temp.data$age > quantile(boot.temp.data$age, 0.75)[[1]],
                                     4,
                                     boot.temp.data$age.ord)
    boot.temp.data$age.ord <- ordered(boot.temp.data$age.ord)
    
    boot.temp.data$SES.ord <- ifelse(boot.temp.data$SES <= quantile(boot.temp.data$SES, 0.25)[[1]],
                                     1,
                                     0)
    boot.temp.data$SES.ord <- ifelse((boot.temp.data$SES > quantile(boot.temp.data$SES, 0.25)[[1]])
                                     &
                                     (boot.temp.data$SES <= quantile(boot.temp.data$SES, 0.50)[[1]]),
                                     2,
                                     boot.temp.data$SES.ord)
    boot.temp.data$SES.ord <- ifelse((boot.temp.data$SES > quantile(boot.temp.data$SES, 0.50)[[1]])
                                     &
                                     (boot.temp.data$SES <= quantile(boot.temp.data$SES, 0.75)[[1]]),
                                     3,
                                     boot.temp.data$SES.ord)
    boot.temp.data$SES.ord <- ifelse(boot.temp.data$SES > quantile(boot.temp.data$SES, 0.75)[[1]],
                                     4,
                                     boot.temp.data$SES.ord)
    boot.temp.data$SES.ord <- ordered(boot.temp.data$SES.ord)
    
    boot.temp.data$MVPA.ord <- ifelse(boot.temp.data$MVPA <= quantile(boot.temp.data$MVPA, 0.25)[[1]],
                                      1,
                                      0)
    boot.temp.data$MVPA.ord <- ifelse((boot.temp.data$MVPA > quantile(boot.temp.data$MVPA, 0.25)[[1]])
                                      &
                                      (boot.temp.data$MVPA <= quantile(boot.temp.data$MVPA, 0.50)[[1]]),
                                      2,
                                      boot.temp.data$MVPA.ord)
    boot.temp.data$MVPA.ord <- ifelse((boot.temp.data$MVPA > quantile(boot.temp.data$MVPA, 0.50)[[1]])
                                      &
                                      (boot.temp.data$MVPA <= quantile(boot.temp.data$MVPA, 0.75)[[1]]),
                                      3,
                                      boot.temp.data$MVPA.ord)
    boot.temp.data$MVPA.ord <- ifelse(boot.temp.data$MVPA > quantile(boot.temp.data$MVPA, 0.75)[[1]],
                                      4,
                                      boot.temp.data$MVPA.ord)
    boot.temp.data$MVPA.ord <- ordered(boot.temp.data$MVPA.ord)
    
    boot.temp.data$DHD.ord <- ifelse(boot.temp.data$DHD <= quantile(boot.temp.data$DHD, 0.25)[[1]],
                                     1,
                                     0)
    boot.temp.data$DHD.ord <- ifelse((boot.temp.data$DHD > quantile(boot.temp.data$DHD, 0.25)[[1]])
                                     &
                                     (boot.temp.data$DHD <= quantile(boot.temp.data$DHD, 0.50)[[1]]),
                                     2,
                                     boot.temp.data$DHD.ord)
    boot.temp.data$DHD.ord <- ifelse((boot.temp.data$DHD > quantile(boot.temp.data$DHD, 0.50)[[1]])
                                     &
                                     (boot.temp.data$DHD <= quantile(boot.temp.data$DHD, 0.75)[[1]]),
                                     3,
                                     boot.temp.data$DHD.ord)
    boot.temp.data$DHD.ord <- ifelse(boot.temp.data$DHD > quantile(boot.temp.data$DHD, 0.75)[[1]],
                                     4,
                                     boot.temp.data$DHD.ord)
    boot.temp.data$DHD.ord <- ordered(boot.temp.data$DHD.ord)
    
    # Define binary variables
    
    boot.temp.data$sex.male_1 <- as.integer(boot.temp.data$sex.male_1)
    
    boot.temp.data$pre_existing_ASCVD.yes_1 <- as.integer(boot.temp.data$pre_existing_ASCVD.yes_1)
    
    boot.temp.data$smoking.current_1 <- as.integer(boot.temp.data$smoking.current_1)
    
    boot.temp.data$ASCVD_composite_event <- as.integer(boot.temp.data$ASCVD_composite_event)
    
    boot.temp.data$overall_mortality_excl_ASCVD_mortality_event <- as.integer(boot.temp.data$overall_mortality_excl_ASCVD_mortality_event)
    
    # Unadjusted correlations 
    
    boot.temp.cor <- lavCor(boot.temp.data[, c("sex.male_1",
                                               "age.ord",
                                               "pre_existing_ASCVD.yes_1",
                                               "SES.ord",
                                               "smoking.current_1",
                                               "MVPA.ord",
                                               "DHD.ord",
                                               "ASCVD_composite_event",
                                               "overall_mortality_excl_ASCVD_mortality_event")])
    
    # Adjusted correlations
    
    boot.no_diabetes.cond_ind.stacked[[i]] <-  localTests(dag,
                                                          sample.cov = boot.temp.cor,
                                                          sample.nobs = nrow(boot.temp.data))
  }
  
  boot.no_diabetes.cond_ind.1 <- c()
  for (i in 1:n_imp) {
    boot.no_diabetes.cond_ind.1 <- append(boot.no_diabetes.cond_ind.1, boot.no_diabetes.cond_ind.stacked[[i]][[1]][[1]])
  }
  boot.mean.no_diabetes.cond_ind.1 <- mean(boot.no_diabetes.cond_ind.1)
  boot.mean.no_diabetes.cond_ind[b, "boot.mean.no_diabetes.cond_ind.1"] <- boot.mean.no_diabetes.cond_ind.1
  
  boot.no_diabetes.cond_ind.2 <- c()
  for (i in 1:n_imp) {
    boot.no_diabetes.cond_ind.2 <- append(boot.no_diabetes.cond_ind.2, boot.no_diabetes.cond_ind.stacked[[i]][[1]][[2]])
  }
  boot.mean.no_diabetes.cond_ind.2 <- mean(boot.no_diabetes.cond_ind.2)
  boot.mean.no_diabetes.cond_ind[b, "boot.mean.no_diabetes.cond_ind.2"] <- boot.mean.no_diabetes.cond_ind.2
  
  boot.no_diabetes.cond_ind.3 <- c()
  for (i in 1:n_imp) {
    boot.no_diabetes.cond_ind.3 <- append(boot.no_diabetes.cond_ind.3, boot.no_diabetes.cond_ind.stacked[[i]][[1]][[3]])
  }
  boot.mean.no_diabetes.cond_ind.3 <- mean(boot.no_diabetes.cond_ind.3)
  boot.mean.no_diabetes.cond_ind[b, "boot.mean.no_diabetes.cond_ind.3"] <- boot.mean.no_diabetes.cond_ind.3
  
  boot.no_diabetes.cond_ind.4 <- c()
  for (i in 1:n_imp) {
    boot.no_diabetes.cond_ind.4 <- append(boot.no_diabetes.cond_ind.4, boot.no_diabetes.cond_ind.stacked[[i]][[1]][[4]])
  }
  boot.mean.no_diabetes.cond_ind.4 <- mean(boot.no_diabetes.cond_ind.4)
  boot.mean.no_diabetes.cond_ind[b, "boot.mean.no_diabetes.cond_ind.4"] <- boot.mean.no_diabetes.cond_ind.4
}

# Output

for (b in 10:n_b) {
  boot.mean.no_diabetes.cond_ind[b, "boot.mean.no_diabetes.cond_ind.1.lb"] <- quantile(boot.mean.no_diabetes.cond_ind[1:b, "boot.mean.no_diabetes.cond_ind.1"], 0.025)
  boot.mean.no_diabetes.cond_ind[b, "boot.mean.no_diabetes.cond_ind.1.ub"] <- quantile(boot.mean.no_diabetes.cond_ind[1:b, "boot.mean.no_diabetes.cond_ind.1"], 0.975)
}
ggplot() +
  geom_point(data = boot.mean.no_diabetes.cond_ind, aes(x = b, y = boot.mean.no_diabetes.cond_ind.1.lb), colour = "skyblue") +
  geom_point(data = boot.mean.no_diabetes.cond_ind, aes(x = b, y = boot.mean.no_diabetes.cond_ind.1.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap") +
  scale_y_continuous(name = "cond_ind.1") +
  theme_minimal()
quantile(boot.mean.no_diabetes.cond_ind$boot.mean.no_diabetes.cond_ind.1, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.mean.no_diabetes.cond_ind[b, "boot.mean.no_diabetes.cond_ind.2.lb"] <- quantile(boot.mean.no_diabetes.cond_ind[1:b, "boot.mean.no_diabetes.cond_ind.2"], 0.025)
  boot.mean.no_diabetes.cond_ind[b, "boot.mean.no_diabetes.cond_ind.2.ub"] <- quantile(boot.mean.no_diabetes.cond_ind[1:b, "boot.mean.no_diabetes.cond_ind.2"], 0.975)
}
ggplot() +
  geom_point(data = boot.mean.no_diabetes.cond_ind, aes(x = b, y = boot.mean.no_diabetes.cond_ind.2.lb), colour = "skyblue") +
  geom_point(data = boot.mean.no_diabetes.cond_ind, aes(x = b, y = boot.mean.no_diabetes.cond_ind.2.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap") +
  scale_y_continuous(name = "cond_ind.2") +
  theme_minimal()
quantile(boot.mean.no_diabetes.cond_ind$boot.mean.no_diabetes.cond_ind.2, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.mean.no_diabetes.cond_ind[b, "boot.mean.no_diabetes.cond_ind.3.lb"] <- quantile(boot.mean.no_diabetes.cond_ind[1:b, "boot.mean.no_diabetes.cond_ind.3"], 0.025)
  boot.mean.no_diabetes.cond_ind[b, "boot.mean.no_diabetes.cond_ind.3.ub"] <- quantile(boot.mean.no_diabetes.cond_ind[1:b, "boot.mean.no_diabetes.cond_ind.3"], 0.975)
}
ggplot() +
  geom_point(data = boot.mean.no_diabetes.cond_ind, aes(x = b, y = boot.mean.no_diabetes.cond_ind.3.lb), colour = "skyblue") +
  geom_point(data = boot.mean.no_diabetes.cond_ind, aes(x = b, y = boot.mean.no_diabetes.cond_ind.3.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap") +
  scale_y_continuous(name = "cond_ind.3") +
  theme_minimal()
quantile(boot.mean.no_diabetes.cond_ind$boot.mean.no_diabetes.cond_ind.3, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.mean.no_diabetes.cond_ind[b, "boot.mean.no_diabetes.cond_ind.4.lb"] <- quantile(boot.mean.no_diabetes.cond_ind[1:b, "boot.mean.no_diabetes.cond_ind.4"], 0.025)
  boot.mean.no_diabetes.cond_ind[b, "boot.mean.no_diabetes.cond_ind.4.ub"] <- quantile(boot.mean.no_diabetes.cond_ind[1:b, "boot.mean.no_diabetes.cond_ind.4"], 0.975)
}
ggplot() +
  geom_point(data = boot.mean.no_diabetes.cond_ind, aes(x = b, y = boot.mean.no_diabetes.cond_ind.4.lb), colour = "skyblue") +
  geom_point(data = boot.mean.no_diabetes.cond_ind, aes(x = b, y = boot.mean.no_diabetes.cond_ind.4.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap") +
  scale_y_continuous(name = "cond_ind.4") +
  theme_minimal()
quantile(boot.mean.no_diabetes.cond_ind$boot.mean.no_diabetes.cond_ind.4, c(0.025, 0.975))

### Participants with diabetes ----

#### Point estimates ----

diabetes.cond_ind.stacked <- list()
for (i in 1:n_imp) {
  
  # Data 
  
  temp.data <- imp.diabetes.stacked[[i]]
  
  # Define ordinal categorical variables 
  
  temp.data$age.ord <- ifelse(temp.data$age <= quantile(temp.data$age, 0.25)[[1]],
                              1,
                              0)
  temp.data$age.ord <- ifelse((temp.data$age > quantile(temp.data$age, 0.25)[[1]])
                              &
                              (temp.data$age <= quantile(temp.data$age, 0.50)[[1]]),
                              2,
                              temp.data$age.ord)
  temp.data$age.ord <- ifelse((temp.data$age > quantile(temp.data$age, 0.50)[[1]])
                              &
                              (temp.data$age <= quantile(temp.data$age, 0.75)[[1]]),
                              3,
                              temp.data$age.ord)
  temp.data$age.ord <- ifelse(temp.data$age > quantile(temp.data$age, 0.75)[[1]],
                              4,
                              temp.data$age.ord)
  temp.data$age.ord <- ordered(temp.data$age.ord)
  
  temp.data$SES.ord <- ifelse(temp.data$SES <= quantile(temp.data$SES, 0.25)[[1]],
                              1,
                              0)
  temp.data$SES.ord <- ifelse((temp.data$SES > quantile(temp.data$SES, 0.25)[[1]])
                              &
                              (temp.data$SES <= quantile(temp.data$SES, 0.50)[[1]]),
                              2,
                              temp.data$SES.ord)
  temp.data$SES.ord <- ifelse((temp.data$SES > quantile(temp.data$SES, 0.50)[[1]])
                              &
                              (temp.data$SES <= quantile(temp.data$SES, 0.75)[[1]]),
                              3,
                              temp.data$SES.ord)
  temp.data$SES.ord <- ifelse(temp.data$SES > quantile(temp.data$SES, 0.75)[[1]],
                              4,
                              temp.data$SES.ord)
  temp.data$SES.ord <- ordered(temp.data$SES.ord)
  
  temp.data$MVPA.ord <- ifelse(temp.data$MVPA <= quantile(temp.data$MVPA, 0.25)[[1]],
                               1,
                               0)
  temp.data$MVPA.ord <- ifelse((temp.data$MVPA > quantile(temp.data$MVPA, 0.25)[[1]])
                               &
                               (temp.data$MVPA <= quantile(temp.data$MVPA, 0.50)[[1]]),
                               2,
                               temp.data$MVPA.ord)
  temp.data$MVPA.ord <- ifelse((temp.data$MVPA > quantile(temp.data$MVPA, 0.50)[[1]])
                               &
                               (temp.data$MVPA <= quantile(temp.data$MVPA, 0.75)[[1]]),
                               3,
                               temp.data$MVPA.ord)
  temp.data$MVPA.ord <- ifelse(temp.data$MVPA > quantile(temp.data$MVPA, 0.75)[[1]],
                               4,
                               temp.data$MVPA.ord)
  temp.data$MVPA.ord <- ordered(temp.data$MVPA.ord)
  
  temp.data$DHD.ord <- ifelse(temp.data$DHD <= quantile(temp.data$DHD, 0.25)[[1]],
                              1,
                              0)
  temp.data$DHD.ord <- ifelse((temp.data$DHD > quantile(temp.data$DHD, 0.25)[[1]])
                              &
                              (temp.data$DHD <= quantile(temp.data$DHD, 0.50)[[1]]),
                              2,
                              temp.data$DHD.ord)
  temp.data$DHD.ord <- ifelse((temp.data$DHD > quantile(temp.data$DHD, 0.50)[[1]])
                              &
                              (temp.data$DHD <= quantile(temp.data$DHD, 0.75)[[1]]),
                              3,
                              temp.data$DHD.ord)
  temp.data$DHD.ord <- ifelse(temp.data$DHD > quantile(temp.data$DHD, 0.75)[[1]],
                              4,
                              temp.data$DHD.ord)
  temp.data$DHD.ord <- ordered(temp.data$DHD.ord)
  
  # Define binary variables
  
  temp.data$sex.male_1 <- as.integer(temp.data$sex.male_1)
  
  temp.data$pre_existing_ASCVD.yes_1 <- as.integer(temp.data$pre_existing_ASCVD.yes_1)
  
  temp.data$smoking.current_1 <- as.integer(temp.data$smoking.current_1)
  
  temp.data$ASCVD_composite_event <- as.integer(temp.data$ASCVD_composite_event)
  
  temp.data$overall_mortality_excl_ASCVD_mortality_event <- as.integer(temp.data$overall_mortality_excl_ASCVD_mortality_event)
  
  # Unadjusted correlations
  
  temp.cor <- lavCor(temp.data[, c("sex.male_1",
                                   "age.ord",
                                   "pre_existing_ASCVD.yes_1",
                                   "SES.ord",
                                   "smoking.current_1",
                                   "MVPA.ord",
                                   "DHD.ord",
                                   "ASCVD_composite_event",
                                   "overall_mortality_excl_ASCVD_mortality_event")])
  
  # Adjusted correlations 
  
  diabetes.cond_ind.stacked[[i]] <-  localTests(dag,
                                                sample.cov = temp.cor,
                                                sample.nobs = nrow(temp.data))
}

diabetes.cond_ind.1 <- c()
for (i in 1:n_imp) {
  diabetes.cond_ind.1 <- append(diabetes.cond_ind.1, diabetes.cond_ind.stacked[[i]][[1]][[1]])
}
mean.diabetes.cond_ind.1 <- mean(diabetes.cond_ind.1)
mean.diabetes.cond_ind.1

diabetes.cond_ind.2 <- c()
for (i in 1:n_imp) {
  diabetes.cond_ind.2 <- append(diabetes.cond_ind.2, diabetes.cond_ind.stacked[[i]][[1]][[2]])
}
mean.diabetes.cond_ind.2 <- mean(diabetes.cond_ind.2)
mean.diabetes.cond_ind.2

diabetes.cond_ind.3 <- c()
for (i in 1:n_imp) {
  diabetes.cond_ind.3 <- append(diabetes.cond_ind.3, diabetes.cond_ind.stacked[[i]][[1]][[3]])
}
mean.diabetes.cond_ind.3 <- mean(diabetes.cond_ind.3)
mean.diabetes.cond_ind.3

diabetes.cond_ind.4 <- c()
for (i in 1:n_imp) {
  diabetes.cond_ind.4 <- append(diabetes.cond_ind.4, diabetes.cond_ind.stacked[[i]][[1]][[4]])
}
mean.diabetes.cond_ind.4 <- mean(diabetes.cond_ind.4)
mean.diabetes.cond_ind.4

#### Bootstrapped confidence intervals ----

# Seed 

set.seed(...)

# Number of bootstraps 

n_b <- 150

# Creating object to capture output 

boot.mean.diabetes.cond_ind <- as.data.frame(matrix(nrow = n_b,
                                                    ncol = 5))
colnames(boot.mean.diabetes.cond_ind) <- c("b",
                                           "boot.mean.diabetes.cond_ind.1",
                                           "boot.mean.diabetes.cond_ind.2",
                                           "boot.mean.diabetes.cond_ind.3",
                                           "boot.mean.diabetes.cond_ind.4")
boot.mean.diabetes.cond_ind$b <- 1:n_b

# Loop 

for (b in 1:n_b) {
  boot.index <- sample(1:nrow(data.diabetes), size = nrow(data.diabetes), replace = TRUE)
  
  boot.data.diabetes <- data.diabetes[boot.index, ]
  
  boot.imp.diabetes <- boot.data.diabetes[, c( "event",
                                               "time",
                                               "sex.male_1",
                                               "age",
                                               "pre_existing_ASCVD.yes_1",
                                               "SES",
                                               "smoking.current_1",
                                               "MVPA",
                                               "DHD")]
  boot.imp.diabetes <- sapply(boot.imp.diabetes, zap_labels)
  boot.imp.diabetes <- mice(boot.imp.diabetes, method = "pmm", m = n_imp, print = FALSE)
  boot.imp.diabetes.stacked <- list()
  for (i in 1:n_imp) {
    boot.imp.diabetes.stacked[[i]] <- complete(boot.imp.diabetes, i)
    boot.imp.diabetes.stacked[[i]][["ASCVD_composite_event"]] <- ifelse(boot.imp.diabetes.stacked[[i]][["event"]] == 1,
                                                                        1,
                                                                        0) 
    boot.imp.diabetes.stacked[[i]][["overall_mortality_excl_ASCVD_mortality_event"]] <- ifelse(boot.imp.diabetes.stacked[[i]][["event"]] == 2,
                                                                                               1,
                                                                                               0) 
  }
  
  boot.diabetes.cond_ind.stacked <- list()
  for (i in 1:n_imp) {
    
    # Data 
    
    boot.temp.data <- boot.imp.diabetes.stacked[[i]]
    
    # Define ordinal categorical variables 
    
    boot.temp.data$age.ord <- ifelse(boot.temp.data$age <= quantile(boot.temp.data$age, 0.25)[[1]],
                                     1,
                                     0)
    boot.temp.data$age.ord <- ifelse((boot.temp.data$age > quantile(boot.temp.data$age, 0.25)[[1]])
                                     &
                                     (boot.temp.data$age <= quantile(boot.temp.data$age, 0.50)[[1]]),
                                     2,
                                     boot.temp.data$age.ord)
    boot.temp.data$age.ord <- ifelse((boot.temp.data$age > quantile(boot.temp.data$age, 0.50)[[1]])
                                     &
                                     (boot.temp.data$age <= quantile(boot.temp.data$age, 0.75)[[1]]),
                                     3,
                                     boot.temp.data$age.ord)
    boot.temp.data$age.ord <- ifelse(boot.temp.data$age > quantile(boot.temp.data$age, 0.75)[[1]],
                                     4,
                                     boot.temp.data$age.ord)
    boot.temp.data$age.ord <- ordered(boot.temp.data$age.ord)
    
    boot.temp.data$SES.ord <- ifelse(boot.temp.data$SES <= quantile(boot.temp.data$SES, 0.25)[[1]],
                                     1,
                                     0)
    boot.temp.data$SES.ord <- ifelse((boot.temp.data$SES > quantile(boot.temp.data$SES, 0.25)[[1]])
                                     &
                                     (boot.temp.data$SES <= quantile(boot.temp.data$SES, 0.50)[[1]]),
                                     2,
                                     boot.temp.data$SES.ord)
    boot.temp.data$SES.ord <- ifelse((boot.temp.data$SES > quantile(boot.temp.data$SES, 0.50)[[1]])
                                     &
                                     (boot.temp.data$SES <= quantile(boot.temp.data$SES, 0.75)[[1]]),
                                     3,
                                     boot.temp.data$SES.ord)
    boot.temp.data$SES.ord <- ifelse(boot.temp.data$SES > quantile(boot.temp.data$SES, 0.75)[[1]],
                                     4,
                                     boot.temp.data$SES.ord)
    boot.temp.data$SES.ord <- ordered(boot.temp.data$SES.ord)
    
    boot.temp.data$MVPA.ord <- ifelse(boot.temp.data$MVPA <= quantile(boot.temp.data$MVPA, 0.25)[[1]],
                                      1,
                                      0)
    boot.temp.data$MVPA.ord <- ifelse((boot.temp.data$MVPA > quantile(boot.temp.data$MVPA, 0.25)[[1]])
                                      &
                                      (boot.temp.data$MVPA <= quantile(boot.temp.data$MVPA, 0.50)[[1]]),
                                      2,
                                      boot.temp.data$MVPA.ord)
    boot.temp.data$MVPA.ord <- ifelse((boot.temp.data$MVPA > quantile(boot.temp.data$MVPA, 0.50)[[1]])
                                      &
                                      (boot.temp.data$MVPA <= quantile(boot.temp.data$MVPA, 0.75)[[1]]),
                                      3,
                                      boot.temp.data$MVPA.ord)
    boot.temp.data$MVPA.ord <- ifelse(boot.temp.data$MVPA > quantile(boot.temp.data$MVPA, 0.75)[[1]],
                                      4,
                                      boot.temp.data$MVPA.ord)
    boot.temp.data$MVPA.ord <- ordered(boot.temp.data$MVPA.ord)
    
    boot.temp.data$DHD.ord <- ifelse(boot.temp.data$DHD <= quantile(boot.temp.data$DHD, 0.25)[[1]],
                                     1,
                                     0)
    boot.temp.data$DHD.ord <- ifelse((boot.temp.data$DHD > quantile(boot.temp.data$DHD, 0.25)[[1]])
                                     &
                                     (boot.temp.data$DHD <= quantile(boot.temp.data$DHD, 0.50)[[1]]),
                                     2,
                                     boot.temp.data$DHD.ord)
    boot.temp.data$DHD.ord <- ifelse((boot.temp.data$DHD > quantile(boot.temp.data$DHD, 0.50)[[1]])
                                     &
                                     (boot.temp.data$DHD <= quantile(boot.temp.data$DHD, 0.75)[[1]]),
                                     3,
                                     boot.temp.data$DHD.ord)
    boot.temp.data$DHD.ord <- ifelse(boot.temp.data$DHD > quantile(boot.temp.data$DHD, 0.75)[[1]],
                                     4,
                                     boot.temp.data$DHD.ord)
    boot.temp.data$DHD.ord <- ordered(boot.temp.data$DHD.ord)
    
    # Define binary variables
    
    boot.temp.data$sex.male_1 <- as.integer(boot.temp.data$sex.male_1)
    
    boot.temp.data$pre_existing_ASCVD.yes_1 <- as.integer(boot.temp.data$pre_existing_ASCVD.yes_1)
    
    boot.temp.data$smoking.current_1 <- as.integer(boot.temp.data$smoking.current_1)
    
    boot.temp.data$ASCVD_composite_event <- as.integer(boot.temp.data$ASCVD_composite_event)
    
    boot.temp.data$overall_mortality_excl_ASCVD_mortality_event <- as.integer(boot.temp.data$overall_mortality_excl_ASCVD_mortality_event)
    
    # Unadjusted correlations 
    
    boot.temp.cor <- lavCor(boot.temp.data[, c("sex.male_1",
                                               "age.ord",
                                               "pre_existing_ASCVD.yes_1",
                                               "SES.ord",
                                               "smoking.current_1",
                                               "MVPA.ord",
                                               "DHD.ord",
                                               "ASCVD_composite_event",
                                               "overall_mortality_excl_ASCVD_mortality_event")])
    
    # Adjusted correlations
    
    boot.diabetes.cond_ind.stacked[[i]] <-  localTests(dag,
                                                       sample.cov = boot.temp.cor,
                                                       sample.nobs = nrow(boot.temp.data))
  }
  
  boot.diabetes.cond_ind.1 <- c()
  for (i in 1:n_imp) {
    boot.diabetes.cond_ind.1 <- append(boot.diabetes.cond_ind.1, boot.diabetes.cond_ind.stacked[[i]][[1]][[1]])
  }
  boot.mean.diabetes.cond_ind.1 <- mean(boot.diabetes.cond_ind.1)
  boot.mean.diabetes.cond_ind[b, "boot.mean.diabetes.cond_ind.1"] <- boot.mean.diabetes.cond_ind.1
  
  boot.diabetes.cond_ind.2 <- c()
  for (i in 1:n_imp) {
    boot.diabetes.cond_ind.2 <- append(boot.diabetes.cond_ind.2, boot.diabetes.cond_ind.stacked[[i]][[1]][[2]])
  }
  boot.mean.diabetes.cond_ind.2 <- mean(boot.diabetes.cond_ind.2)
  boot.mean.diabetes.cond_ind[b, "boot.mean.diabetes.cond_ind.2"] <- boot.mean.diabetes.cond_ind.2
  
  boot.diabetes.cond_ind.3 <- c()
  for (i in 1:n_imp) {
    boot.diabetes.cond_ind.3 <- append(boot.diabetes.cond_ind.3, boot.diabetes.cond_ind.stacked[[i]][[1]][[3]])
  }
  boot.mean.diabetes.cond_ind.3 <- mean(boot.diabetes.cond_ind.3)
  boot.mean.diabetes.cond_ind[b, "boot.mean.diabetes.cond_ind.3"] <- boot.mean.diabetes.cond_ind.3
  
  boot.diabetes.cond_ind.4 <- c()
  for (i in 1:n_imp) {
    boot.diabetes.cond_ind.4 <- append(boot.diabetes.cond_ind.4, boot.diabetes.cond_ind.stacked[[i]][[1]][[4]])
  }
  boot.mean.diabetes.cond_ind.4 <- mean(boot.diabetes.cond_ind.4)
  boot.mean.diabetes.cond_ind[b, "boot.mean.diabetes.cond_ind.4"] <- boot.mean.diabetes.cond_ind.4
}

# Output

for (b in 10:n_b) {
  boot.mean.diabetes.cond_ind[b, "boot.mean.diabetes.cond_ind.1.lb"] <- quantile(boot.mean.diabetes.cond_ind[1:b, "boot.mean.diabetes.cond_ind.1"], 0.025)
  boot.mean.diabetes.cond_ind[b, "boot.mean.diabetes.cond_ind.1.ub"] <- quantile(boot.mean.diabetes.cond_ind[1:b, "boot.mean.diabetes.cond_ind.1"], 0.975)
}
ggplot() +
  geom_point(data = boot.mean.diabetes.cond_ind, aes(x = b, y = boot.mean.diabetes.cond_ind.1.lb), colour = "skyblue") +
  geom_point(data = boot.mean.diabetes.cond_ind, aes(x = b, y = boot.mean.diabetes.cond_ind.1.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap") +
  scale_y_continuous(name = "cond_ind.1") +
  theme_minimal()
quantile(boot.mean.diabetes.cond_ind$boot.mean.diabetes.cond_ind.1, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.mean.diabetes.cond_ind[b, "boot.mean.diabetes.cond_ind.2.lb"] <- quantile(boot.mean.diabetes.cond_ind[1:b, "boot.mean.diabetes.cond_ind.2"], 0.025)
  boot.mean.diabetes.cond_ind[b, "boot.mean.diabetes.cond_ind.2.ub"] <- quantile(boot.mean.diabetes.cond_ind[1:b, "boot.mean.diabetes.cond_ind.2"], 0.975)
}
ggplot() +
  geom_point(data = boot.mean.diabetes.cond_ind, aes(x = b, y = boot.mean.diabetes.cond_ind.2.lb), colour = "skyblue") +
  geom_point(data = boot.mean.diabetes.cond_ind, aes(x = b, y = boot.mean.diabetes.cond_ind.2.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap") +
  scale_y_continuous(name = "cond_ind.2") +
  theme_minimal()
quantile(boot.mean.diabetes.cond_ind$boot.mean.diabetes.cond_ind.2, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.mean.diabetes.cond_ind[b, "boot.mean.diabetes.cond_ind.3.lb"] <- quantile(boot.mean.diabetes.cond_ind[1:b, "boot.mean.diabetes.cond_ind.3"], 0.025)
  boot.mean.diabetes.cond_ind[b, "boot.mean.diabetes.cond_ind.3.ub"] <- quantile(boot.mean.diabetes.cond_ind[1:b, "boot.mean.diabetes.cond_ind.3"], 0.975)
}
ggplot() +
  geom_point(data = boot.mean.diabetes.cond_ind, aes(x = b, y = boot.mean.diabetes.cond_ind.3.lb), colour = "skyblue") +
  geom_point(data = boot.mean.diabetes.cond_ind, aes(x = b, y = boot.mean.diabetes.cond_ind.3.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap") +
  scale_y_continuous(name = "cond_ind.3") +
  theme_minimal()
quantile(boot.mean.diabetes.cond_ind$boot.mean.diabetes.cond_ind.3, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.mean.diabetes.cond_ind[b, "boot.mean.diabetes.cond_ind.4.lb"] <- quantile(boot.mean.diabetes.cond_ind[1:b, "boot.mean.diabetes.cond_ind.4"], 0.025)
  boot.mean.diabetes.cond_ind[b, "boot.mean.diabetes.cond_ind.4.ub"] <- quantile(boot.mean.diabetes.cond_ind[1:b, "boot.mean.diabetes.cond_ind.4"], 0.975)
}
ggplot() +
  geom_point(data = boot.mean.diabetes.cond_ind, aes(x = b, y = boot.mean.diabetes.cond_ind.4.lb), colour = "skyblue") +
  geom_point(data = boot.mean.diabetes.cond_ind, aes(x = b, y = boot.mean.diabetes.cond_ind.4.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap") +
  scale_y_continuous(name = "cond_ind.4") +
  theme_minimal()
quantile(boot.mean.diabetes.cond_ind$boot.mean.diabetes.cond_ind.4, c(0.025, 0.975))

## Adjustment sets ----

### smoking.current_1 & ASCVD_composite_event ----

adjustmentSets(dag,
               exposure = "smoking.current_1",
               outcome = "ASCVD_composite_event",
               type = "all",
               effect = "total")

adjustmentSets(dag,
               exposure = "smoking.current_1",
               outcome = "ASCVD_composite_event",
               type = "minimal",
               effect = "direct")

### MVPA.ord & ASCVD_composite_event ----

adjustmentSets(dag,
               exposure = "MVPA.ord",
               outcome = "ASCVD_composite_event",
               type = "all",
               effect = "total")

adjustmentSets(dag,
               exposure = "MVPA.ord",
               outcome = "ASCVD_composite_event",
               type = "minimal",
               effect = "direct")

### DHD.ord & ASCVD_composite_event ----

adjustmentSets(dag,
               exposure = "DHD.ord",
               outcome = "ASCVD_composite_event",
               type = "all",
               effect = "total")

adjustmentSets(dag,
               exposure = "DHD.ord",
               outcome = "ASCVD_composite_event",
               type = "minimal",
               effect = "direct")

# Average causal effect estimation ----

## Participants without diabetes ----

### Not smoking versus smoking ----

#### Point estimates ----

# Step 1: data expansion 

block_1 <- imp.no_diabetes.stacked

block_2 <- imp.no_diabetes.stacked
for (i in 1:n_imp) {
  block_2[[i]][["smoking.current_1"]] <- 0
}

block_3 <- imp.no_diabetes.stacked
for (i in 1:n_imp) {
  block_3[[i]][["smoking.current_1"]] <- 1
}

# Step 2: outcome modelling 

smoking.fit_1.stacked <- list()
for (i in 1:n_imp) {
  smoking.fit_1.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                        smoking.current_1,
                                      data = block_1[[i]],
                                      x = TRUE)
}

smoking.fit_2.stacked <- list()
for (i in 1:n_imp) {
  smoking.fit_2.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                        smoking.current_1 +
                                        sex.male_1 +
                                        age,
                                      data = block_1[[i]],
                                      x = TRUE)
}

smoking.fit_3.stacked <- list()
for (i in 1:n_imp) {
  smoking.fit_3.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                      smoking.current_1 +
                                      sex.male_1 +
                                      age + 
                                      SES +
                                      pre_existing_ASCVD.yes_1,
                                    data = block_1[[i]],
                                    x = TRUE)
}

smoking.fit_4.stacked <- list()
for (i in 1:n_imp) {
  smoking.fit_4.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                        smoking.current_1 +
                                        sex.male_1 +
                                        rcs(age, 3) + 
                                        rcs(SES, 3) +
                                        pre_existing_ASCVD.yes_1 +
                                        smoking.current_1:sex.male_1 +
                                        age:sex.male_1 +
                                        SES:sex.male_1 +
                                        pre_existing_ASCVD.yes_1:sex.male_1,
                                      data = block_1[[i]],
                                      x = TRUE)
}

smoking.fit_5.stacked <- list()
for (i in 1:n_imp) {
  smoking.fit_5.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                        smoking.current_1 +
                                        sex.male_1 +
                                        age +
                                        SES +
                                        pre_existing_ASCVD.yes_1 +
                                        MVPA +
                                        DHD,
                                      data = block_1[[i]],
                                      x = TRUE)
}

smoking.fit_6.stacked <- list()
for (i in 1:n_imp) {
  smoking.fit_6.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                        smoking.current_1 +
                                        sex.male_1 +
                                        rcs(age, 3) +
                                        rcs(SES, 3) +
                                        pre_existing_ASCVD.yes_1 +
                                        rcs(MVPA, 3) +
                                        rcs(DHD, 3) +
                                        smoking.current_1:sex.male_1 +
                                        age:sex.male_1 +
                                        SES:sex.male_1 +
                                        pre_existing_ASCVD.yes_1:sex.male_1 +
                                        MVPA:sex.male_1 +
                                        DHD:sex.male_1,
                                      data = block_1[[i]],
                                      x = TRUE)
}

smoking.fit_7.stacked <- list()
for (i in 1:n_imp) {
  smoking.fit_7.stacked[[i]] <- CSC(Hist(time, event) ~ 
                                      smoking.current_1 +
                                      sex.male_1 +
                                      age +
                                      SES +
                                      pre_existing_ASCVD.yes_1 +
                                      MVPA +
                                      DHD,
                                    data = block_1[[i]])
}

smoking.fit_8.stacked <- list()
for (i in 1:n_imp) {
  smoking.fit_8.stacked[[i]] <- CSC(Hist(time, event) ~ 
                                       smoking.current_1 +
                                       sex.male_1 +
                                       rcs(age, 3) +
                                       rcs(SES, 3) +
                                       pre_existing_ASCVD.yes_1 +
                                       rcs(MVPA, 3) +
                                       rcs(DHD, 3) +
                                       smoking.current_1:sex.male_1 +
                                       age:sex.male_1 +
                                       SES:sex.male_1 +
                                       pre_existing_ASCVD.yes_1:sex.male_1 +
                                       MVPA:sex.male_1 +
                                       DHD:sex.male_1,
                                    data = block_1[[i]])
}

# Step 3: prediction 

block_2.pred_risk.smoking.fit_1.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.smoking.fit_1.stacked[[i]] <- predictRisk(smoking.fit_1.stacked[[i]], 
                                                              block_2[[i]], 
                                                              times = 5) 
}

block_2.pred_risk.smoking.fit_2.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.smoking.fit_2.stacked[[i]] <- predictRisk(smoking.fit_2.stacked[[i]], 
                                                              block_2[[i]], 
                                                              times = 5) 
}

block_2.pred_risk.smoking.fit_3.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.smoking.fit_3.stacked[[i]] <- predictRisk(smoking.fit_3.stacked[[i]], 
                                                              block_2[[i]], 
                                                              times = 5) 
}

block_2.pred_risk.smoking.fit_4.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.smoking.fit_4.stacked[[i]] <- predictRisk(smoking.fit_4.stacked[[i]], 
                                                              block_2[[i]], 
                                                              times = 5) 
}

block_2.pred_risk.smoking.fit_5.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.smoking.fit_5.stacked[[i]] <- predictRisk(smoking.fit_5.stacked[[i]], 
                                                              block_2[[i]], 
                                                              times = 5) 
}

block_2.pred_risk.smoking.fit_6.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.smoking.fit_6.stacked[[i]] <- predictRisk(smoking.fit_6.stacked[[i]], 
                                                              block_2[[i]], 
                                                              times = 5) 
}

block_2.pred_risk.smoking.fit_7.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.smoking.fit_7.stacked[[i]] <- predictRisk(smoking.fit_7.stacked[[i]], 
                                                              block_2[[i]], 
                                                              times = 5) 
}

block_2.pred_risk.smoking.fit_8.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.smoking.fit_8.stacked[[i]] <- predictRisk(smoking.fit_8.stacked[[i]], 
                                                              block_2[[i]], 
                                                              times = 5) 
}

block_3.pred_risk.smoking.fit_1.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.smoking.fit_1.stacked[[i]] <- predictRisk(smoking.fit_1.stacked[[i]], 
                                                              block_3[[i]], 
                                                              times = 5) 
}

block_3.pred_risk.smoking.fit_2.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.smoking.fit_2.stacked[[i]] <- predictRisk(smoking.fit_2.stacked[[i]], 
                                                              block_3[[i]], 
                                                              times = 5) 
}

block_3.pred_risk.smoking.fit_3.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.smoking.fit_3.stacked[[i]] <- predictRisk(smoking.fit_3.stacked[[i]], 
                                                              block_3[[i]], 
                                                              times = 5) 
}

block_3.pred_risk.smoking.fit_4.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.smoking.fit_4.stacked[[i]] <- predictRisk(smoking.fit_4.stacked[[i]], 
                                                              block_3[[i]], 
                                                              times = 5) 
}

block_3.pred_risk.smoking.fit_5.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.smoking.fit_5.stacked[[i]] <- predictRisk(smoking.fit_5.stacked[[i]], 
                                                              block_3[[i]], 
                                                              times = 5) 
}

block_3.pred_risk.smoking.fit_6.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.smoking.fit_6.stacked[[i]] <- predictRisk(smoking.fit_6.stacked[[i]], 
                                                              block_3[[i]], 
                                                              times = 5) 
}

block_3.pred_risk.smoking.fit_7.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.smoking.fit_7.stacked[[i]] <- predictRisk(smoking.fit_7.stacked[[i]], 
                                                              block_3[[i]], 
                                                              times = 5) 
}

block_3.pred_risk.smoking.fit_8.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.smoking.fit_8.stacked[[i]] <- predictRisk(smoking.fit_8.stacked[[i]], 
                                                              block_3[[i]], 
                                                              times = 5) 
}

# Step 4: standardization 

no_diabetes.risk_ratio.smoking.fit_1.stacked <- mean(unlist(block_2.pred_risk.smoking.fit_1.stacked)) / mean(unlist(block_3.pred_risk.smoking.fit_1.stacked)) 
no_diabetes.risk_ratio.smoking.fit_1.stacked

no_diabetes.risk_ratio.smoking.fit_2.stacked <- mean(unlist(block_2.pred_risk.smoking.fit_2.stacked)) / mean(unlist(block_3.pred_risk.smoking.fit_2.stacked)) 
no_diabetes.risk_ratio.smoking.fit_2.stacked

no_diabetes.risk_ratio.smoking.fit_3.stacked <- mean(unlist(block_2.pred_risk.smoking.fit_3.stacked)) / mean(unlist(block_3.pred_risk.smoking.fit_3.stacked)) 
no_diabetes.risk_ratio.smoking.fit_3.stacked

no_diabetes.risk_ratio.smoking.fit_4.stacked <- mean(unlist(block_2.pred_risk.smoking.fit_4.stacked)) / mean(unlist(block_3.pred_risk.smoking.fit_4.stacked)) 
no_diabetes.risk_ratio.smoking.fit_4.stacked

no_diabetes.risk_ratio.smoking.fit_5.stacked <- mean(unlist(block_2.pred_risk.smoking.fit_5.stacked)) / mean(unlist(block_3.pred_risk.smoking.fit_5.stacked)) 
no_diabetes.risk_ratio.smoking.fit_5.stacked

no_diabetes.risk_ratio.smoking.fit_6.stacked <- mean(unlist(block_2.pred_risk.smoking.fit_6.stacked)) / mean(unlist(block_3.pred_risk.smoking.fit_6.stacked)) 
no_diabetes.risk_ratio.smoking.fit_6.stacked

no_diabetes.risk_ratio.smoking.fit_7.stacked <- mean(unlist(block_2.pred_risk.smoking.fit_7.stacked)) / mean(unlist(block_3.pred_risk.smoking.fit_7.stacked)) 
no_diabetes.risk_ratio.smoking.fit_7.stacked

no_diabetes.risk_ratio.smoking.fit_8.stacked <- mean(unlist(block_2.pred_risk.smoking.fit_8.stacked)) / mean(unlist(block_3.pred_risk.smoking.fit_8.stacked)) 
no_diabetes.risk_ratio.smoking.fit_8.stacked

#### Bootstrapped confidence intervals ----

# Seed 

set.seed(...)

# Number of bootstraps

n_b <- 150

# Creating objects to capture output

boot.no_diabetes.risk_ratio.smoking.fit_1.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                          ncol = 2))
colnames(boot.no_diabetes.risk_ratio.smoking.fit_1.stacked) <- c("b",
                                                                 "boot.no_diabetes.risk_ratio.smoking.fit_1.stacked")
boot.no_diabetes.risk_ratio.smoking.fit_1.stacked$b <- 1:n_b

boot.no_diabetes.risk_ratio.smoking.fit_2.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                          ncol = 2))
colnames(boot.no_diabetes.risk_ratio.smoking.fit_2.stacked) <- c("b",
                                                                 "boot.no_diabetes.risk_ratio.smoking.fit_2.stacked")
boot.no_diabetes.risk_ratio.smoking.fit_2.stacked$b <- 1:n_b

boot.no_diabetes.risk_ratio.smoking.fit_3.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                          ncol = 2))
colnames(boot.no_diabetes.risk_ratio.smoking.fit_3.stacked) <- c("b",
                                                                 "boot.no_diabetes.risk_ratio.smoking.fit_3.stacked")
boot.no_diabetes.risk_ratio.smoking.fit_3.stacked$b <- 1:n_b

boot.no_diabetes.risk_ratio.smoking.fit_4.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                          ncol = 2))
colnames(boot.no_diabetes.risk_ratio.smoking.fit_4.stacked) <- c("b",
                                                                 "boot.no_diabetes.risk_ratio.smoking.fit_4.stacked")
boot.no_diabetes.risk_ratio.smoking.fit_4.stacked$b <- 1:n_b

boot.no_diabetes.risk_ratio.smoking.fit_5.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                          ncol = 2))
colnames(boot.no_diabetes.risk_ratio.smoking.fit_5.stacked) <- c("b",
                                                                 "boot.no_diabetes.risk_ratio.smoking.fit_5.stacked")
boot.no_diabetes.risk_ratio.smoking.fit_5.stacked$b <- 1:n_b

boot.no_diabetes.risk_ratio.smoking.fit_6.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                          ncol = 2))
colnames(boot.no_diabetes.risk_ratio.smoking.fit_6.stacked) <- c("b",
                                                                 "boot.no_diabetes.risk_ratio.smoking.fit_6.stacked")
boot.no_diabetes.risk_ratio.smoking.fit_6.stacked$b <- 1:n_b

boot.no_diabetes.risk_ratio.smoking.fit_7.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                          ncol = 2))
colnames(boot.no_diabetes.risk_ratio.smoking.fit_7.stacked) <- c("b",
                                                                 "boot.no_diabetes.risk_ratio.smoking.fit_7.stacked")
boot.no_diabetes.risk_ratio.smoking.fit_7.stacked$b <- 1:n_b

boot.no_diabetes.risk_ratio.smoking.fit_8.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                          ncol = 2))
colnames(boot.no_diabetes.risk_ratio.smoking.fit_8.stacked) <- c("b",
                                                                 "boot.no_diabetes.risk_ratio.smoking.fit_8.stacked")
boot.no_diabetes.risk_ratio.smoking.fit_8.stacked$b <- 1:n_b

# Loop

for (b in 1:n_b) {
  
  # Bootstrapped data
  
  boot.index <- sample(1:nrow(data.no_diabetes), size = nrow(data.no_diabetes), replace = TRUE)
  
  boot.data.no_diabetes <- data.no_diabetes[boot.index, ]
  
  boot.imp.no_diabetes <- boot.data.no_diabetes[, c( "event",
                                                     "time",
                                                     "sex.male_1",
                                                     "age",
                                                     "pre_existing_ASCVD.yes_1",
                                                     "SES",
                                                     "smoking.current_1",
                                                     "MVPA",
                                                     "DHD")]
  boot.imp.no_diabetes <- sapply(boot.imp.no_diabetes, zap_labels)
  boot.imp.no_diabetes <- mice(boot.imp.no_diabetes, method = "pmm", m = n_imp, print = FALSE)
  boot.imp.no_diabetes.stacked <- list()
  for (i in 1:n_imp) {
    boot.imp.no_diabetes.stacked[[i]] <- complete(boot.imp.no_diabetes, i)
    boot.imp.no_diabetes.stacked[[i]][["ASCVD_composite_event"]] <- ifelse(boot.imp.no_diabetes.stacked[[i]][["event"]] == 1,
                                                                           1,
                                                                           0) 
    boot.imp.no_diabetes.stacked[[i]][["overall_mortality_excl_ASCVD_mortality_event"]] <- ifelse(boot.imp.no_diabetes.stacked[[i]][["event"]] == 2,
                                                                                                  1,
                                                                                                  0) 
  }
  
  # Step 1: data expansion
  
  boot.block_1 <- boot.imp.no_diabetes.stacked
  
  boot.block_2 <- boot.imp.no_diabetes.stacked
  for (i in 1:n_imp) {
    boot.block_2[[i]][["smoking.current_1"]] <- 0
  }
  
  boot.block_3 <- boot.imp.no_diabetes.stacked
  for (i in 1:n_imp) {
    boot.block_3[[i]][["smoking.current_1"]] <- 1
  }
  
  # Step 2: outcome modelling
  
  boot.smoking.fit_1.stacked <- list()
  for (i in 1:n_imp) {
    boot.smoking.fit_1.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                               smoking.current_1,
                                             data = boot.block_1[[i]],
                                             x = TRUE)
  }
  
  boot.smoking.fit_2.stacked <- list()
  for (i in 1:n_imp) {
    boot.smoking.fit_2.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                               smoking.current_1 +
                                               sex.male_1 +
                                               age,
                                             data = boot.block_1[[i]],
                                             x = TRUE)
  }
  
  boot.smoking.fit_3.stacked <- list()
  for (i in 1:n_imp) {
    boot.smoking.fit_3.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                               smoking.current_1 +
                                               sex.male_1 +
                                               age + 
                                               SES +
                                               pre_existing_ASCVD.yes_1,
                                             data = boot.block_1[[i]],
                                             x = TRUE)
  }
  
  boot.smoking.fit_4.stacked <- list()
  for (i in 1:n_imp) {
    boot.smoking.fit_4.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                               smoking.current_1 +
                                               sex.male_1 +
                                               rcs(age, 3) + 
                                               rcs(SES, 3) +
                                               pre_existing_ASCVD.yes_1 +
                                               smoking.current_1:sex.male_1 +
                                               age:sex.male_1 + 
                                               SES:sex.male_1 + 
                                               pre_existing_ASCVD.yes_1:sex.male_1,
                                             data = boot.block_1[[i]],
                                             x = TRUE)
  }
  
  boot.smoking.fit_5.stacked <- list()
  for (i in 1:n_imp) {
    boot.smoking.fit_5.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                               smoking.current_1 +
                                               sex.male_1 +
                                               age +
                                               SES +
                                               pre_existing_ASCVD.yes_1 +
                                               MVPA +
                                               DHD,
                                             data = boot.block_1[[i]],
                                             x = TRUE)
  }
  
  boot.smoking.fit_6.stacked <- list()
  for (i in 1:n_imp) {
    boot.smoking.fit_6.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                               smoking.current_1 +
                                               sex.male_1 +
                                               rcs(age, 3) +
                                               rcs(SES, 3) +
                                               pre_existing_ASCVD.yes_1 +
                                               rcs(MVPA, 3) +
                                               rcs(DHD, 3) +
                                               smoking.current_1:sex.male_1 + 
                                               age:sex.male_1 +
                                               SES:sex.male_1 +
                                               pre_existing_ASCVD.yes_1:sex.male_1 +
                                               MVPA:sex.male_1 +
                                               DHD:sex.male_1,
                                             data = boot.block_1[[i]],
                                             x = TRUE)
  }
  
  boot.smoking.fit_7.stacked <- list()
  for (i in 1:n_imp) {
    boot.smoking.fit_7.stacked[[i]] <- CSC(Hist(time, event) ~ 
                                             smoking.current_1 +
                                             sex.male_1 +
                                             age +
                                             SES +
                                             pre_existing_ASCVD.yes_1 +
                                             MVPA +
                                             DHD,
                                           data = boot.block_1[[i]])
  }
  
  boot.smoking.fit_8.stacked <- list()
  for (i in 1:n_imp) {
    boot.smoking.fit_8.stacked[[i]] <- CSC(Hist(time, event) ~ 
                                              smoking.current_1 +
                                              sex.male_1 +
                                              rcs(age, 3) +
                                              rcs(SES, 3) +
                                              pre_existing_ASCVD.yes_1 +
                                              rcs(MVPA, 3) +
                                              rcs(DHD, 3) +
                                              smoking.current_1:sex.male_1 + 
                                              age:sex.male_1 +
                                              SES:sex.male_1 +
                                              pre_existing_ASCVD.yes_1:sex.male_1 +
                                              MVPA:sex.male_1 +
                                              DHD:sex.male_1,
                                           data = boot.block_1[[i]])
  }
  
  # Step 3: prediction
  
  boot.block_2.pred_risk.smoking.fit_1.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.smoking.fit_1.stacked[[i]] <- predictRisk(boot.smoking.fit_1.stacked[[i]], 
                                                                     boot.block_2[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_2.pred_risk.smoking.fit_2.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.smoking.fit_2.stacked[[i]] <- predictRisk(boot.smoking.fit_2.stacked[[i]], 
                                                                     boot.block_2[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_2.pred_risk.smoking.fit_3.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.smoking.fit_3.stacked[[i]] <- predictRisk(boot.smoking.fit_3.stacked[[i]], 
                                                                     boot.block_2[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_2.pred_risk.smoking.fit_4.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.smoking.fit_4.stacked[[i]] <- predictRisk(boot.smoking.fit_4.stacked[[i]], 
                                                                     boot.block_2[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_2.pred_risk.smoking.fit_5.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.smoking.fit_5.stacked[[i]] <- predictRisk(boot.smoking.fit_5.stacked[[i]], 
                                                                     boot.block_2[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_2.pred_risk.smoking.fit_6.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.smoking.fit_6.stacked[[i]] <- predictRisk(boot.smoking.fit_6.stacked[[i]], 
                                                                     boot.block_2[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_2.pred_risk.smoking.fit_7.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.smoking.fit_7.stacked[[i]] <- predictRisk(boot.smoking.fit_7.stacked[[i]], 
                                                                     boot.block_2[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_2.pred_risk.smoking.fit_8.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.smoking.fit_8.stacked[[i]] <- predictRisk(boot.smoking.fit_8.stacked[[i]], 
                                                                     boot.block_2[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_3.pred_risk.smoking.fit_1.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.smoking.fit_1.stacked[[i]] <- predictRisk(boot.smoking.fit_1.stacked[[i]], 
                                                                     boot.block_3[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_3.pred_risk.smoking.fit_2.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.smoking.fit_2.stacked[[i]] <- predictRisk(boot.smoking.fit_2.stacked[[i]], 
                                                                     boot.block_3[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_3.pred_risk.smoking.fit_3.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.smoking.fit_3.stacked[[i]] <- predictRisk(boot.smoking.fit_3.stacked[[i]], 
                                                                     boot.block_3[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_3.pred_risk.smoking.fit_4.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.smoking.fit_4.stacked[[i]] <- predictRisk(boot.smoking.fit_4.stacked[[i]], 
                                                                     boot.block_3[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_3.pred_risk.smoking.fit_5.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.smoking.fit_5.stacked[[i]] <- predictRisk(boot.smoking.fit_5.stacked[[i]], 
                                                                     boot.block_3[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_3.pred_risk.smoking.fit_6.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.smoking.fit_6.stacked[[i]] <- predictRisk(boot.smoking.fit_6.stacked[[i]], 
                                                                     boot.block_3[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_3.pred_risk.smoking.fit_7.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.smoking.fit_7.stacked[[i]] <- predictRisk(boot.smoking.fit_7.stacked[[i]], 
                                                                     boot.block_3[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_3.pred_risk.smoking.fit_8.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.smoking.fit_8.stacked[[i]] <- predictRisk(boot.smoking.fit_8.stacked[[i]], 
                                                                     boot.block_3[[i]], 
                                                                     times = 5) 
  }
  
  # Step 4: standardization 
  
  boot.no_diabetes.risk_ratio.smoking.fit_1.stacked[b, "boot.no_diabetes.risk_ratio.smoking.fit_1.stacked"] <- mean(unlist(boot.block_2.pred_risk.smoking.fit_1.stacked)) / mean(unlist(boot.block_3.pred_risk.smoking.fit_1.stacked)) 
  
  boot.no_diabetes.risk_ratio.smoking.fit_2.stacked[b, "boot.no_diabetes.risk_ratio.smoking.fit_2.stacked"] <- mean(unlist(boot.block_2.pred_risk.smoking.fit_2.stacked)) / mean(unlist(boot.block_3.pred_risk.smoking.fit_2.stacked)) 
  
  boot.no_diabetes.risk_ratio.smoking.fit_3.stacked[b, "boot.no_diabetes.risk_ratio.smoking.fit_3.stacked"] <- mean(unlist(boot.block_2.pred_risk.smoking.fit_3.stacked)) / mean(unlist(boot.block_3.pred_risk.smoking.fit_3.stacked)) 
  
  boot.no_diabetes.risk_ratio.smoking.fit_4.stacked[b, "boot.no_diabetes.risk_ratio.smoking.fit_4.stacked"] <- mean(unlist(boot.block_2.pred_risk.smoking.fit_4.stacked)) / mean(unlist(boot.block_3.pred_risk.smoking.fit_4.stacked)) 
  
  boot.no_diabetes.risk_ratio.smoking.fit_5.stacked[b, "boot.no_diabetes.risk_ratio.smoking.fit_5.stacked"] <- mean(unlist(boot.block_2.pred_risk.smoking.fit_5.stacked)) / mean(unlist(boot.block_3.pred_risk.smoking.fit_5.stacked)) 
  
  boot.no_diabetes.risk_ratio.smoking.fit_6.stacked[b, "boot.no_diabetes.risk_ratio.smoking.fit_6.stacked"] <- mean(unlist(boot.block_2.pred_risk.smoking.fit_6.stacked)) / mean(unlist(boot.block_3.pred_risk.smoking.fit_6.stacked)) 
  
  boot.no_diabetes.risk_ratio.smoking.fit_7.stacked[b, "boot.no_diabetes.risk_ratio.smoking.fit_7.stacked"] <- mean(unlist(boot.block_2.pred_risk.smoking.fit_7.stacked)) / mean(unlist(boot.block_3.pred_risk.smoking.fit_7.stacked)) 
  
  boot.no_diabetes.risk_ratio.smoking.fit_8.stacked[b, "boot.no_diabetes.risk_ratio.smoking.fit_8.stacked"] <- mean(unlist(boot.block_2.pred_risk.smoking.fit_8.stacked)) / mean(unlist(boot.block_3.pred_risk.smoking.fit_8.stacked)) 
}

# Output

for (b in 10:n_b) {
  boot.no_diabetes.risk_ratio.smoking.fit_1.stacked[b, "boot.no_diabetes.risk_ratio.smoking.fit_1.stacked.lb"] <- quantile(boot.no_diabetes.risk_ratio.smoking.fit_1.stacked[1:b, "boot.no_diabetes.risk_ratio.smoking.fit_1.stacked"], 0.025)
  boot.no_diabetes.risk_ratio.smoking.fit_1.stacked[b, "boot.no_diabetes.risk_ratio.smoking.fit_1.stacked.ub"] <- quantile(boot.no_diabetes.risk_ratio.smoking.fit_1.stacked[1:b, "boot.no_diabetes.risk_ratio.smoking.fit_1.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.no_diabetes.risk_ratio.smoking.fit_1.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.smoking.fit_1.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.no_diabetes.risk_ratio.smoking.fit_1.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.smoking.fit_1.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_1",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.no_diabetes.risk_ratio.smoking.fit_1.stacked$boot.no_diabetes.risk_ratio.smoking.fit_1.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.no_diabetes.risk_ratio.smoking.fit_2.stacked[b, "boot.no_diabetes.risk_ratio.smoking.fit_2.stacked.lb"] <- quantile(boot.no_diabetes.risk_ratio.smoking.fit_2.stacked[1:b, "boot.no_diabetes.risk_ratio.smoking.fit_2.stacked"], 0.025)
  boot.no_diabetes.risk_ratio.smoking.fit_2.stacked[b, "boot.no_diabetes.risk_ratio.smoking.fit_2.stacked.ub"] <- quantile(boot.no_diabetes.risk_ratio.smoking.fit_2.stacked[1:b, "boot.no_diabetes.risk_ratio.smoking.fit_2.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.no_diabetes.risk_ratio.smoking.fit_2.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.smoking.fit_2.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.no_diabetes.risk_ratio.smoking.fit_2.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.smoking.fit_2.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_2",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.no_diabetes.risk_ratio.smoking.fit_2.stacked$boot.no_diabetes.risk_ratio.smoking.fit_2.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.no_diabetes.risk_ratio.smoking.fit_3.stacked[b, "boot.no_diabetes.risk_ratio.smoking.fit_3.stacked.lb"] <- quantile(boot.no_diabetes.risk_ratio.smoking.fit_3.stacked[1:b, "boot.no_diabetes.risk_ratio.smoking.fit_3.stacked"], 0.025)
  boot.no_diabetes.risk_ratio.smoking.fit_3.stacked[b, "boot.no_diabetes.risk_ratio.smoking.fit_3.stacked.ub"] <- quantile(boot.no_diabetes.risk_ratio.smoking.fit_3.stacked[1:b, "boot.no_diabetes.risk_ratio.smoking.fit_3.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.no_diabetes.risk_ratio.smoking.fit_3.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.smoking.fit_3.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.no_diabetes.risk_ratio.smoking.fit_3.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.smoking.fit_3.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_3",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.no_diabetes.risk_ratio.smoking.fit_3.stacked$boot.no_diabetes.risk_ratio.smoking.fit_3.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.no_diabetes.risk_ratio.smoking.fit_4.stacked[b, "boot.no_diabetes.risk_ratio.smoking.fit_4.stacked.lb"] <- quantile(boot.no_diabetes.risk_ratio.smoking.fit_4.stacked[1:b, "boot.no_diabetes.risk_ratio.smoking.fit_4.stacked"], 0.025)
  boot.no_diabetes.risk_ratio.smoking.fit_4.stacked[b, "boot.no_diabetes.risk_ratio.smoking.fit_4.stacked.ub"] <- quantile(boot.no_diabetes.risk_ratio.smoking.fit_4.stacked[1:b, "boot.no_diabetes.risk_ratio.smoking.fit_4.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.no_diabetes.risk_ratio.smoking.fit_4.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.smoking.fit_4.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.no_diabetes.risk_ratio.smoking.fit_4.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.smoking.fit_4.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_4",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.no_diabetes.risk_ratio.smoking.fit_4.stacked$boot.no_diabetes.risk_ratio.smoking.fit_4.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.no_diabetes.risk_ratio.smoking.fit_5.stacked[b, "boot.no_diabetes.risk_ratio.smoking.fit_5.stacked.lb"] <- quantile(boot.no_diabetes.risk_ratio.smoking.fit_5.stacked[1:b, "boot.no_diabetes.risk_ratio.smoking.fit_5.stacked"], 0.025)
  boot.no_diabetes.risk_ratio.smoking.fit_5.stacked[b, "boot.no_diabetes.risk_ratio.smoking.fit_5.stacked.ub"] <- quantile(boot.no_diabetes.risk_ratio.smoking.fit_5.stacked[1:b, "boot.no_diabetes.risk_ratio.smoking.fit_5.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.no_diabetes.risk_ratio.smoking.fit_5.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.smoking.fit_5.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.no_diabetes.risk_ratio.smoking.fit_5.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.smoking.fit_5.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_5",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.no_diabetes.risk_ratio.smoking.fit_5.stacked$boot.no_diabetes.risk_ratio.smoking.fit_5.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.no_diabetes.risk_ratio.smoking.fit_6.stacked[b, "boot.no_diabetes.risk_ratio.smoking.fit_6.stacked.lb"] <- quantile(boot.no_diabetes.risk_ratio.smoking.fit_6.stacked[1:b, "boot.no_diabetes.risk_ratio.smoking.fit_6.stacked"], 0.025)
  boot.no_diabetes.risk_ratio.smoking.fit_6.stacked[b, "boot.no_diabetes.risk_ratio.smoking.fit_6.stacked.ub"] <- quantile(boot.no_diabetes.risk_ratio.smoking.fit_6.stacked[1:b, "boot.no_diabetes.risk_ratio.smoking.fit_6.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.no_diabetes.risk_ratio.smoking.fit_6.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.smoking.fit_6.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.no_diabetes.risk_ratio.smoking.fit_6.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.smoking.fit_6.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_6",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.no_diabetes.risk_ratio.smoking.fit_6.stacked$boot.no_diabetes.risk_ratio.smoking.fit_6.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.no_diabetes.risk_ratio.smoking.fit_7.stacked[b, "boot.no_diabetes.risk_ratio.smoking.fit_7.stacked.lb"] <- quantile(boot.no_diabetes.risk_ratio.smoking.fit_7.stacked[1:b, "boot.no_diabetes.risk_ratio.smoking.fit_7.stacked"], 0.025)
  boot.no_diabetes.risk_ratio.smoking.fit_7.stacked[b, "boot.no_diabetes.risk_ratio.smoking.fit_7.stacked.ub"] <- quantile(boot.no_diabetes.risk_ratio.smoking.fit_7.stacked[1:b, "boot.no_diabetes.risk_ratio.smoking.fit_7.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.no_diabetes.risk_ratio.smoking.fit_7.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.smoking.fit_7.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.no_diabetes.risk_ratio.smoking.fit_7.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.smoking.fit_7.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_7",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.no_diabetes.risk_ratio.smoking.fit_7.stacked$boot.no_diabetes.risk_ratio.smoking.fit_7.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.no_diabetes.risk_ratio.smoking.fit_8.stacked[b, "boot.no_diabetes.risk_ratio.smoking.fit_8.stacked.lb"] <- quantile(boot.no_diabetes.risk_ratio.smoking.fit_8.stacked[1:b, "boot.no_diabetes.risk_ratio.smoking.fit_8.stacked"], 0.025)
  boot.no_diabetes.risk_ratio.smoking.fit_8.stacked[b, "boot.no_diabetes.risk_ratio.smoking.fit_8.stacked.ub"] <- quantile(boot.no_diabetes.risk_ratio.smoking.fit_8.stacked[1:b, "boot.no_diabetes.risk_ratio.smoking.fit_8.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.no_diabetes.risk_ratio.smoking.fit_8.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.smoking.fit_8.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.no_diabetes.risk_ratio.smoking.fit_8.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.smoking.fit_8.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_8",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.no_diabetes.risk_ratio.smoking.fit_8.stacked$boot.no_diabetes.risk_ratio.smoking.fit_8.stacked, c(0.025, 0.975))

### MVPA + 1 SD versus no change ----

#### Point estimates ----

# Step 1: data expansion 

block_1 <- imp.no_diabetes.stacked

block_2 <- imp.no_diabetes.stacked
for (i in 1:n_imp) {
  for (j in 1:nrow(data.no_diabetes)) {
    block_2[[i]][["MVPA"]][[j]] <- block_2[[i]][["MVPA"]][[j]] + sd(block_2[[i]][["MVPA"]])
  }
}

block_3 <- imp.no_diabetes.stacked

# Step 2: outcome modelling 

MVPA.fit_1.stacked <- list()
for (i in 1:n_imp) {
  MVPA.fit_1.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                     MVPA,
                                   data = block_1[[i]],
                                   x = TRUE)
}

MVPA.fit_2.stacked <- list()
for (i in 1:n_imp) {
  MVPA.fit_2.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                     MVPA +
                                     sex.male_1 +
                                     age,
                                   data = block_1[[i]],
                                   x = TRUE)
}

MVPA.fit_3.stacked <- list()
for (i in 1:n_imp) {
  MVPA.fit_3.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                        MVPA +
                                        sex.male_1 +
                                        age + 
                                        SES +
                                        pre_existing_ASCVD.yes_1,
                                      data = block_1[[i]],
                                      x = TRUE)
}

MVPA.fit_4.stacked <- list()
for (i in 1:n_imp) {
  MVPA.fit_4.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                     rcs(MVPA, 3) +
                                     sex.male_1 +
                                     rcs(age, 3) + 
                                     rcs(SES, 3) +
                                     pre_existing_ASCVD.yes_1 + 
                                     MVPA:sex.male_1 +
                                     age:sex.male_1 + 
                                     SES:sex.male_1 + 
                                     pre_existing_ASCVD.yes_1:sex.male_1,
                                   data = block_1[[i]],
                                   x = TRUE)
}

MVPA.fit_5.stacked <- list()
for (i in 1:n_imp) {
  MVPA.fit_5.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~
                                     MVPA +
                                     sex.male_1 +
                                     age +
                                     SES +
                                     pre_existing_ASCVD.yes_1 +
                                     smoking.current_1 +
                                     DHD,
                                   data = block_1[[i]],
                                   x = TRUE)
}

MVPA.fit_6.stacked <- list()
for (i in 1:n_imp) {
  MVPA.fit_6.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~
                                     rcs(MVPA, 3) +
                                     sex.male_1 +
                                     rcs(age, 3) +
                                     rcs(SES, 3) +
                                     pre_existing_ASCVD.yes_1 +
                                     smoking.current_1 +
                                     rcs(DHD, 3) +
                                     MVPA:sex.male_1 +
                                     age:sex.male_1 +
                                     SES:sex.male_1 + 
                                     pre_existing_ASCVD.yes_1:sex.male_1 + 
                                     smoking.current_1:sex.male_1 + 
                                     DHD:sex.male_1,
                                   data = block_1[[i]],
                                   x = TRUE)
}

MVPA.fit_7.stacked <- list()
for (i in 1:n_imp) {
  MVPA.fit_7.stacked[[i]] <- CSC(Hist(time, event) ~ 
                                      MVPA +
                                      sex.male_1 +
                                      age +
                                      SES +
                                      pre_existing_ASCVD.yes_1 +
                                      smoking.current_1 +
                                      DHD,
                                    data = block_1[[i]])
}

MVPA.fit_8.stacked <- list()
for (i in 1:n_imp) {
  MVPA.fit_8.stacked[[i]] <- CSC(Hist(time, event) ~ 
                                    rcs(MVPA, 3) +
                                    sex.male_1 +
                                    rcs(age, 3) +
                                    rcs(SES, 3) +
                                    pre_existing_ASCVD.yes_1 +
                                    smoking.current_1 +
                                    rcs(DHD, 3) +
                                    MVPA:sex.male_1 +
                                    age:sex.male_1 +
                                    SES:sex.male_1 + 
                                    pre_existing_ASCVD.yes_1:sex.male_1 + 
                                    smoking.current_1:sex.male_1 + 
                                    DHD:sex.male_1,
                                  data = block_1[[i]])
}

# Step 3: prediction 

block_2.pred_risk.MVPA.fit_1.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.MVPA.fit_1.stacked[[i]] <- predictRisk(MVPA.fit_1.stacked[[i]], 
                                                           block_2[[i]], 
                                                           times = 5) 
}

block_2.pred_risk.MVPA.fit_2.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.MVPA.fit_2.stacked[[i]] <- predictRisk(MVPA.fit_2.stacked[[i]], 
                                                           block_2[[i]], 
                                                           times = 5) 
}

block_2.pred_risk.MVPA.fit_3.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.MVPA.fit_3.stacked[[i]] <- predictRisk(MVPA.fit_3.stacked[[i]], 
                                                           block_2[[i]], 
                                                           times = 5) 
}

block_2.pred_risk.MVPA.fit_4.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.MVPA.fit_4.stacked[[i]] <- predictRisk(MVPA.fit_4.stacked[[i]], 
                                                           block_2[[i]], 
                                                           times = 5) 
}

block_2.pred_risk.MVPA.fit_5.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.MVPA.fit_5.stacked[[i]] <- predictRisk(MVPA.fit_5.stacked[[i]], 
                                                           block_2[[i]], 
                                                           times = 5) 
}

block_2.pred_risk.MVPA.fit_6.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.MVPA.fit_6.stacked[[i]] <- predictRisk(MVPA.fit_6.stacked[[i]], 
                                                           block_2[[i]], 
                                                           times = 5) 
}

block_2.pred_risk.MVPA.fit_7.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.MVPA.fit_7.stacked[[i]] <- predictRisk(MVPA.fit_7.stacked[[i]], 
                                                           block_2[[i]], 
                                                           times = 5) 
}

block_2.pred_risk.MVPA.fit_8.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.MVPA.fit_8.stacked[[i]] <- predictRisk(MVPA.fit_8.stacked[[i]], 
                                                           block_2[[i]], 
                                                           times = 5) 
}

block_3.pred_risk.MVPA.fit_1.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.MVPA.fit_1.stacked[[i]] <- predictRisk(MVPA.fit_1.stacked[[i]], 
                                                           block_3[[i]], 
                                                           times = 5) 
}

block_3.pred_risk.MVPA.fit_2.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.MVPA.fit_2.stacked[[i]] <- predictRisk(MVPA.fit_2.stacked[[i]], 
                                                           block_3[[i]], 
                                                           times = 5) 
}

block_3.pred_risk.MVPA.fit_3.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.MVPA.fit_3.stacked[[i]] <- predictRisk(MVPA.fit_3.stacked[[i]], 
                                                           block_3[[i]], 
                                                           times = 5) 
}

block_3.pred_risk.MVPA.fit_4.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.MVPA.fit_4.stacked[[i]] <- predictRisk(MVPA.fit_4.stacked[[i]], 
                                                           block_3[[i]], 
                                                           times = 5) 
}

block_3.pred_risk.MVPA.fit_5.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.MVPA.fit_5.stacked[[i]] <- predictRisk(MVPA.fit_5.stacked[[i]], 
                                                           block_3[[i]], 
                                                           times = 5) 
}

block_3.pred_risk.MVPA.fit_6.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.MVPA.fit_6.stacked[[i]] <- predictRisk(MVPA.fit_6.stacked[[i]], 
                                                           block_3[[i]], 
                                                           times = 5) 
}

block_3.pred_risk.MVPA.fit_7.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.MVPA.fit_7.stacked[[i]] <- predictRisk(MVPA.fit_7.stacked[[i]], 
                                                           block_3[[i]], 
                                                           times = 5) 
}

block_3.pred_risk.MVPA.fit_8.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.MVPA.fit_8.stacked[[i]] <- predictRisk(MVPA.fit_8.stacked[[i]], 
                                                           block_3[[i]], 
                                                           times = 5) 
}

# Step 4: standardization 

no_diabetes.risk_ratio.MVPA.fit_1.stacked <- mean(unlist(block_2.pred_risk.MVPA.fit_1.stacked)) / mean(unlist(block_3.pred_risk.MVPA.fit_1.stacked)) 
no_diabetes.risk_ratio.MVPA.fit_1.stacked

no_diabetes.risk_ratio.MVPA.fit_2.stacked <- mean(unlist(block_2.pred_risk.MVPA.fit_2.stacked)) / mean(unlist(block_3.pred_risk.MVPA.fit_2.stacked)) 
no_diabetes.risk_ratio.MVPA.fit_2.stacked

no_diabetes.risk_ratio.MVPA.fit_3.stacked <- mean(unlist(block_2.pred_risk.MVPA.fit_3.stacked)) / mean(unlist(block_3.pred_risk.MVPA.fit_3.stacked)) 
no_diabetes.risk_ratio.MVPA.fit_3.stacked

no_diabetes.risk_ratio.MVPA.fit_4.stacked <- mean(unlist(block_2.pred_risk.MVPA.fit_4.stacked)) / mean(unlist(block_3.pred_risk.MVPA.fit_4.stacked)) 
no_diabetes.risk_ratio.MVPA.fit_4.stacked

no_diabetes.risk_ratio.MVPA.fit_5.stacked <- mean(unlist(block_2.pred_risk.MVPA.fit_5.stacked)) / mean(unlist(block_3.pred_risk.MVPA.fit_5.stacked)) 
no_diabetes.risk_ratio.MVPA.fit_5.stacked

no_diabetes.risk_ratio.MVPA.fit_6.stacked <- mean(unlist(block_2.pred_risk.MVPA.fit_6.stacked)) / mean(unlist(block_3.pred_risk.MVPA.fit_6.stacked)) 
no_diabetes.risk_ratio.MVPA.fit_6.stacked

no_diabetes.risk_ratio.MVPA.fit_7.stacked <- mean(unlist(block_2.pred_risk.MVPA.fit_7.stacked)) / mean(unlist(block_3.pred_risk.MVPA.fit_7.stacked)) 
no_diabetes.risk_ratio.MVPA.fit_7.stacked

no_diabetes.risk_ratio.MVPA.fit_8.stacked <- mean(unlist(block_2.pred_risk.MVPA.fit_8.stacked)) / mean(unlist(block_3.pred_risk.MVPA.fit_8.stacked)) 
no_diabetes.risk_ratio.MVPA.fit_8.stacked

#### Bootstrapped confidence intervals ----

# Seed 

set.seed(...)

# Number of bootstraps

n_b <- 150

# Creating objects to capture output

boot.no_diabetes.risk_ratio.MVPA.fit_1.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                       ncol = 2))
colnames(boot.no_diabetes.risk_ratio.MVPA.fit_1.stacked) <- c("b",
                                                              "boot.no_diabetes.risk_ratio.MVPA.fit_1.stacked")
boot.no_diabetes.risk_ratio.MVPA.fit_1.stacked$b <- 1:n_b

boot.no_diabetes.risk_ratio.MVPA.fit_2.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                       ncol = 2))
colnames(boot.no_diabetes.risk_ratio.MVPA.fit_2.stacked) <- c("b",
                                                              "boot.no_diabetes.risk_ratio.MVPA.fit_2.stacked")
boot.no_diabetes.risk_ratio.MVPA.fit_2.stacked$b <- 1:n_b

boot.no_diabetes.risk_ratio.MVPA.fit_3.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                       ncol = 2))
colnames(boot.no_diabetes.risk_ratio.MVPA.fit_3.stacked) <- c("b",
                                                              "boot.no_diabetes.risk_ratio.MVPA.fit_3.stacked")
boot.no_diabetes.risk_ratio.MVPA.fit_3.stacked$b <- 1:n_b

boot.no_diabetes.risk_ratio.MVPA.fit_4.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                       ncol = 2))
colnames(boot.no_diabetes.risk_ratio.MVPA.fit_4.stacked) <- c("b",
                                                              "boot.no_diabetes.risk_ratio.MVPA.fit_4.stacked")
boot.no_diabetes.risk_ratio.MVPA.fit_4.stacked$b <- 1:n_b

boot.no_diabetes.risk_ratio.MVPA.fit_5.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                       ncol = 2))
colnames(boot.no_diabetes.risk_ratio.MVPA.fit_5.stacked) <- c("b",
                                                              "boot.no_diabetes.risk_ratio.MVPA.fit_5.stacked")
boot.no_diabetes.risk_ratio.MVPA.fit_5.stacked$b <- 1:n_b

boot.no_diabetes.risk_ratio.MVPA.fit_6.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                       ncol = 2))
colnames(boot.no_diabetes.risk_ratio.MVPA.fit_6.stacked) <- c("b",
                                                              "boot.no_diabetes.risk_ratio.MVPA.fit_6.stacked")
boot.no_diabetes.risk_ratio.MVPA.fit_6.stacked$b <- 1:n_b

boot.no_diabetes.risk_ratio.MVPA.fit_7.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                       ncol = 2))
colnames(boot.no_diabetes.risk_ratio.MVPA.fit_7.stacked) <- c("b",
                                                              "boot.no_diabetes.risk_ratio.MVPA.fit_7.stacked")
boot.no_diabetes.risk_ratio.MVPA.fit_7.stacked$b <- 1:n_b

boot.no_diabetes.risk_ratio.MVPA.fit_8.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                       ncol = 2))
colnames(boot.no_diabetes.risk_ratio.MVPA.fit_8.stacked) <- c("b",
                                                              "boot.no_diabetes.risk_ratio.MVPA.fit_8.stacked")
boot.no_diabetes.risk_ratio.MVPA.fit_8.stacked$b <- 1:n_b

# Loop

for (b in 1:n_b) {
  
  # Bootstrapped data
  
  boot.index <- sample(1:nrow(data.no_diabetes), size = nrow(data.no_diabetes), replace = TRUE)
  
  boot.data.no_diabetes <- data.no_diabetes[boot.index, ]
  
  boot.imp.no_diabetes <- boot.data.no_diabetes[, c( "event",
                                                     "time",
                                                     "sex.male_1",
                                                     "age",
                                                     "pre_existing_ASCVD.yes_1",
                                                     "SES",
                                                     "smoking.current_1",
                                                     "MVPA",
                                                     "DHD")]
  boot.imp.no_diabetes <- sapply(boot.imp.no_diabetes, zap_labels)
  boot.imp.no_diabetes <- mice(boot.imp.no_diabetes, method = "pmm", m = n_imp, print = FALSE)
  boot.imp.no_diabetes.stacked <- list()
  for (i in 1:n_imp) {
    boot.imp.no_diabetes.stacked[[i]] <- complete(boot.imp.no_diabetes, i)
    boot.imp.no_diabetes.stacked[[i]][["ASCVD_composite_event"]] <- ifelse(boot.imp.no_diabetes.stacked[[i]][["event"]] == 1,
                                                                           1,
                                                                           0) 
    boot.imp.no_diabetes.stacked[[i]][["overall_mortality_excl_ASCVD_mortality_event"]] <- ifelse(boot.imp.no_diabetes.stacked[[i]][["event"]] == 2,
                                                                                                  1,
                                                                                                  0) 
  }
  
  # Step 1: data expansion
  
  boot.block_1 <- boot.imp.no_diabetes.stacked
  
  boot.block_2 <- boot.imp.no_diabetes.stacked
  for (i in 1:n_imp) {
    for (j in 1:nrow(data.no_diabetes)) {
      boot.block_2[[i]][["MVPA"]][[j]] <- boot.block_2[[i]][["MVPA"]][[j]] + sd(boot.block_2[[i]][["MVPA"]])
    }
  }
  
  boot.block_3 <- boot.imp.no_diabetes.stacked
  
  # Step 2: outcome modelling
  
  boot.MVPA.fit_1.stacked <- list()
  for (i in 1:n_imp) {
    boot.MVPA.fit_1.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                            MVPA,
                                          data = boot.block_1[[i]],
                                          x = TRUE)
  }
  
  boot.MVPA.fit_2.stacked <- list()
  for (i in 1:n_imp) {
    boot.MVPA.fit_2.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                               MVPA +
                                               sex.male_1 +
                                               age,
                                             data = boot.block_1[[i]],
                                             x = TRUE)
  }
  
  boot.MVPA.fit_3.stacked <- list()
  for (i in 1:n_imp) {
    boot.MVPA.fit_3.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                               MVPA +
                                               sex.male_1 +
                                               age + 
                                               SES +
                                               pre_existing_ASCVD.yes_1,
                                             data = boot.block_1[[i]],
                                             x = TRUE)
  }
  
  boot.MVPA.fit_4.stacked <- list()
  for (i in 1:n_imp) {
    boot.MVPA.fit_4.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                            rcs(MVPA, 3) +
                                            sex.male_1 +
                                            rcs(age, 3) +
                                            rcs(SES, 3) +
                                            pre_existing_ASCVD.yes_1 +
                                            MVPA:sex.male_1 +
                                            age:sex.male_1 +
                                            SES:sex.male_1 + 
                                            pre_existing_ASCVD.yes_1:sex.male_1,
                                          data = boot.block_1[[i]],
                                          x = TRUE)
  }
  
  boot.MVPA.fit_5.stacked <- list()
  for (i in 1:n_imp) {
    boot.MVPA.fit_5.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~
                                          MVPA +
                                          sex.male_1 +
                                          age +
                                          SES +
                                          pre_existing_ASCVD.yes_1 +
                                          smoking.current_1 +
                                          DHD,
                                          data = boot.block_1[[i]],
                                          x = TRUE)
  }
  
  boot.MVPA.fit_6.stacked <- list()
  for (i in 1:n_imp) {
    boot.MVPA.fit_6.stacked[[i]] <-  coxph(Surv(time, ASCVD_composite_event) ~
                                             rcs(MVPA, 3) +
                                             sex.male_1 +
                                             rcs(age, 3) +
                                             rcs(SES, 3) +
                                             pre_existing_ASCVD.yes_1 +
                                             smoking.current_1 +
                                             rcs(DHD, 3) +
                                             MVPA:sex.male_1 +
                                             age:sex.male_1 +
                                             SES:sex.male_1 + 
                                             pre_existing_ASCVD.yes_1:sex.male_1 + 
                                             smoking.current_1:sex.male_1 + 
                                             DHD:sex.male_1,
                                        data = boot.block_1[[i]],
                                        x = TRUE)
  }
  
  boot.MVPA.fit_7.stacked <- list()
  for (i in 1:n_imp) {
    boot.MVPA.fit_7.stacked[[i]] <- CSC(Hist(time, event) ~ 
                                             MVPA +
                                             sex.male_1 +
                                             age +
                                             SES +
                                             pre_existing_ASCVD.yes_1 +
                                             smoking.current_1 +
                                             DHD,
                                           data = boot.block_1[[i]])
  }
  
  boot.MVPA.fit_8.stacked <- list()
  for (i in 1:n_imp) {
    boot.MVPA.fit_8.stacked[[i]] <- CSC(Hist(time, event) ~ 
                                           rcs(MVPA, 3) +
                                           sex.male_1 +
                                           rcs(age, 3) +
                                           rcs(SES, 3) +
                                           pre_existing_ASCVD.yes_1 +
                                           smoking.current_1 +
                                           rcs(DHD, 3) +
                                           MVPA:sex.male_1 +
                                           age:sex.male_1 +
                                           SES:sex.male_1 + 
                                           pre_existing_ASCVD.yes_1:sex.male_1 + 
                                           smoking.current_1:sex.male_1 + 
                                           DHD:sex.male_1,
                                           data = boot.block_1[[i]])
  }
  
  # Step 3: prediction
  
  boot.block_2.pred_risk.MVPA.fit_1.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.MVPA.fit_1.stacked[[i]] <- predictRisk(boot.MVPA.fit_1.stacked[[i]], 
                                                                  boot.block_2[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_2.pred_risk.MVPA.fit_2.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.MVPA.fit_2.stacked[[i]] <- predictRisk(boot.MVPA.fit_2.stacked[[i]], 
                                                                  boot.block_2[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_2.pred_risk.MVPA.fit_3.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.MVPA.fit_3.stacked[[i]] <- predictRisk(boot.MVPA.fit_3.stacked[[i]], 
                                                                  boot.block_2[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_2.pred_risk.MVPA.fit_4.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.MVPA.fit_4.stacked[[i]] <- predictRisk(boot.MVPA.fit_4.stacked[[i]], 
                                                                  boot.block_2[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_2.pred_risk.MVPA.fit_5.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.MVPA.fit_5.stacked[[i]] <- predictRisk(boot.MVPA.fit_5.stacked[[i]], 
                                                                  boot.block_2[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_2.pred_risk.MVPA.fit_6.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.MVPA.fit_6.stacked[[i]] <- predictRisk(boot.MVPA.fit_6.stacked[[i]], 
                                                                  boot.block_2[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_2.pred_risk.MVPA.fit_7.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.MVPA.fit_7.stacked[[i]] <- predictRisk(boot.MVPA.fit_7.stacked[[i]], 
                                                                  boot.block_2[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_2.pred_risk.MVPA.fit_8.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.MVPA.fit_8.stacked[[i]] <- predictRisk(boot.MVPA.fit_8.stacked[[i]], 
                                                                  boot.block_2[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_3.pred_risk.MVPA.fit_1.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.MVPA.fit_1.stacked[[i]] <- predictRisk(boot.MVPA.fit_1.stacked[[i]], 
                                                                  boot.block_3[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_3.pred_risk.MVPA.fit_2.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.MVPA.fit_2.stacked[[i]] <- predictRisk(boot.MVPA.fit_2.stacked[[i]], 
                                                                  boot.block_3[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_3.pred_risk.MVPA.fit_3.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.MVPA.fit_3.stacked[[i]] <- predictRisk(boot.MVPA.fit_3.stacked[[i]], 
                                                                  boot.block_3[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_3.pred_risk.MVPA.fit_4.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.MVPA.fit_4.stacked[[i]] <- predictRisk(boot.MVPA.fit_4.stacked[[i]], 
                                                                  boot.block_3[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_3.pred_risk.MVPA.fit_5.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.MVPA.fit_5.stacked[[i]] <- predictRisk(boot.MVPA.fit_5.stacked[[i]], 
                                                                  boot.block_3[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_3.pred_risk.MVPA.fit_6.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.MVPA.fit_6.stacked[[i]] <- predictRisk(boot.MVPA.fit_6.stacked[[i]], 
                                                                  boot.block_3[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_3.pred_risk.MVPA.fit_7.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.MVPA.fit_7.stacked[[i]] <- predictRisk(boot.MVPA.fit_7.stacked[[i]], 
                                                                  boot.block_3[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_3.pred_risk.MVPA.fit_8.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.MVPA.fit_8.stacked[[i]] <- predictRisk(boot.MVPA.fit_8.stacked[[i]], 
                                                                  boot.block_3[[i]], 
                                                                  times = 5) 
  }
  
  # Step 4: standardization 
  
  boot.no_diabetes.risk_ratio.MVPA.fit_1.stacked[b, "boot.no_diabetes.risk_ratio.MVPA.fit_1.stacked"] <- mean(unlist(boot.block_2.pred_risk.MVPA.fit_1.stacked)) / mean(unlist(boot.block_3.pred_risk.MVPA.fit_1.stacked)) 
  
  boot.no_diabetes.risk_ratio.MVPA.fit_2.stacked[b, "boot.no_diabetes.risk_ratio.MVPA.fit_2.stacked"] <- mean(unlist(boot.block_2.pred_risk.MVPA.fit_2.stacked)) / mean(unlist(boot.block_3.pred_risk.MVPA.fit_2.stacked)) 
  
  boot.no_diabetes.risk_ratio.MVPA.fit_3.stacked[b, "boot.no_diabetes.risk_ratio.MVPA.fit_3.stacked"] <- mean(unlist(boot.block_2.pred_risk.MVPA.fit_3.stacked)) / mean(unlist(boot.block_3.pred_risk.MVPA.fit_3.stacked)) 
  
  boot.no_diabetes.risk_ratio.MVPA.fit_4.stacked[b, "boot.no_diabetes.risk_ratio.MVPA.fit_4.stacked"] <- mean(unlist(boot.block_2.pred_risk.MVPA.fit_4.stacked)) / mean(unlist(boot.block_3.pred_risk.MVPA.fit_4.stacked)) 
  
  boot.no_diabetes.risk_ratio.MVPA.fit_5.stacked[b, "boot.no_diabetes.risk_ratio.MVPA.fit_5.stacked"] <- mean(unlist(boot.block_2.pred_risk.MVPA.fit_5.stacked)) / mean(unlist(boot.block_3.pred_risk.MVPA.fit_5.stacked)) 
  
  boot.no_diabetes.risk_ratio.MVPA.fit_6.stacked[b, "boot.no_diabetes.risk_ratio.MVPA.fit_6.stacked"] <- mean(unlist(boot.block_2.pred_risk.MVPA.fit_6.stacked)) / mean(unlist(boot.block_3.pred_risk.MVPA.fit_6.stacked)) 
  
  boot.no_diabetes.risk_ratio.MVPA.fit_7.stacked[b, "boot.no_diabetes.risk_ratio.MVPA.fit_7.stacked"] <- mean(unlist(boot.block_2.pred_risk.MVPA.fit_7.stacked)) / mean(unlist(boot.block_3.pred_risk.MVPA.fit_7.stacked)) 
  
  boot.no_diabetes.risk_ratio.MVPA.fit_8.stacked[b, "boot.no_diabetes.risk_ratio.MVPA.fit_8.stacked"] <- mean(unlist(boot.block_2.pred_risk.MVPA.fit_8.stacked)) / mean(unlist(boot.block_3.pred_risk.MVPA.fit_8.stacked)) 
}

# Output

for (b in 10:n_b) {
  boot.no_diabetes.risk_ratio.MVPA.fit_1.stacked[b, "boot.no_diabetes.risk_ratio.MVPA.fit_1.stacked.lb"] <- quantile(boot.no_diabetes.risk_ratio.MVPA.fit_1.stacked[1:b, "boot.no_diabetes.risk_ratio.MVPA.fit_1.stacked"], 0.025)
  boot.no_diabetes.risk_ratio.MVPA.fit_1.stacked[b, "boot.no_diabetes.risk_ratio.MVPA.fit_1.stacked.ub"] <- quantile(boot.no_diabetes.risk_ratio.MVPA.fit_1.stacked[1:b, "boot.no_diabetes.risk_ratio.MVPA.fit_1.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.no_diabetes.risk_ratio.MVPA.fit_1.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.MVPA.fit_1.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.no_diabetes.risk_ratio.MVPA.fit_1.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.MVPA.fit_1.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_1",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.no_diabetes.risk_ratio.MVPA.fit_1.stacked$boot.no_diabetes.risk_ratio.MVPA.fit_1.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.no_diabetes.risk_ratio.MVPA.fit_2.stacked[b, "boot.no_diabetes.risk_ratio.MVPA.fit_2.stacked.lb"] <- quantile(boot.no_diabetes.risk_ratio.MVPA.fit_2.stacked[1:b, "boot.no_diabetes.risk_ratio.MVPA.fit_2.stacked"], 0.025)
  boot.no_diabetes.risk_ratio.MVPA.fit_2.stacked[b, "boot.no_diabetes.risk_ratio.MVPA.fit_2.stacked.ub"] <- quantile(boot.no_diabetes.risk_ratio.MVPA.fit_2.stacked[1:b, "boot.no_diabetes.risk_ratio.MVPA.fit_2.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.no_diabetes.risk_ratio.MVPA.fit_2.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.MVPA.fit_2.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.no_diabetes.risk_ratio.MVPA.fit_2.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.MVPA.fit_2.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_2",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.no_diabetes.risk_ratio.MVPA.fit_2.stacked$boot.no_diabetes.risk_ratio.MVPA.fit_2.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.no_diabetes.risk_ratio.MVPA.fit_3.stacked[b, "boot.no_diabetes.risk_ratio.MVPA.fit_3.stacked.lb"] <- quantile(boot.no_diabetes.risk_ratio.MVPA.fit_3.stacked[1:b, "boot.no_diabetes.risk_ratio.MVPA.fit_3.stacked"], 0.025)
  boot.no_diabetes.risk_ratio.MVPA.fit_3.stacked[b, "boot.no_diabetes.risk_ratio.MVPA.fit_3.stacked.ub"] <- quantile(boot.no_diabetes.risk_ratio.MVPA.fit_3.stacked[1:b, "boot.no_diabetes.risk_ratio.MVPA.fit_3.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.no_diabetes.risk_ratio.MVPA.fit_3.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.MVPA.fit_3.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.no_diabetes.risk_ratio.MVPA.fit_3.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.MVPA.fit_3.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_3",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.no_diabetes.risk_ratio.MVPA.fit_3.stacked$boot.no_diabetes.risk_ratio.MVPA.fit_3.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.no_diabetes.risk_ratio.MVPA.fit_4.stacked[b, "boot.no_diabetes.risk_ratio.MVPA.fit_4.stacked.lb"] <- quantile(boot.no_diabetes.risk_ratio.MVPA.fit_4.stacked[1:b, "boot.no_diabetes.risk_ratio.MVPA.fit_4.stacked"], 0.025)
  boot.no_diabetes.risk_ratio.MVPA.fit_4.stacked[b, "boot.no_diabetes.risk_ratio.MVPA.fit_4.stacked.ub"] <- quantile(boot.no_diabetes.risk_ratio.MVPA.fit_4.stacked[1:b, "boot.no_diabetes.risk_ratio.MVPA.fit_4.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.no_diabetes.risk_ratio.MVPA.fit_4.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.MVPA.fit_4.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.no_diabetes.risk_ratio.MVPA.fit_4.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.MVPA.fit_4.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_4",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.no_diabetes.risk_ratio.MVPA.fit_4.stacked$boot.no_diabetes.risk_ratio.MVPA.fit_4.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.no_diabetes.risk_ratio.MVPA.fit_5.stacked[b, "boot.no_diabetes.risk_ratio.MVPA.fit_5.stacked.lb"] <- quantile(boot.no_diabetes.risk_ratio.MVPA.fit_5.stacked[1:b, "boot.no_diabetes.risk_ratio.MVPA.fit_5.stacked"], 0.025)
  boot.no_diabetes.risk_ratio.MVPA.fit_5.stacked[b, "boot.no_diabetes.risk_ratio.MVPA.fit_5.stacked.ub"] <- quantile(boot.no_diabetes.risk_ratio.MVPA.fit_5.stacked[1:b, "boot.no_diabetes.risk_ratio.MVPA.fit_5.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.no_diabetes.risk_ratio.MVPA.fit_5.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.MVPA.fit_5.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.no_diabetes.risk_ratio.MVPA.fit_5.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.MVPA.fit_5.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_5",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.no_diabetes.risk_ratio.MVPA.fit_5.stacked$boot.no_diabetes.risk_ratio.MVPA.fit_5.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.no_diabetes.risk_ratio.MVPA.fit_6.stacked[b, "boot.no_diabetes.risk_ratio.MVPA.fit_6.stacked.lb"] <- quantile(boot.no_diabetes.risk_ratio.MVPA.fit_6.stacked[1:b, "boot.no_diabetes.risk_ratio.MVPA.fit_6.stacked"], 0.025)
  boot.no_diabetes.risk_ratio.MVPA.fit_6.stacked[b, "boot.no_diabetes.risk_ratio.MVPA.fit_6.stacked.ub"] <- quantile(boot.no_diabetes.risk_ratio.MVPA.fit_6.stacked[1:b, "boot.no_diabetes.risk_ratio.MVPA.fit_6.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.no_diabetes.risk_ratio.MVPA.fit_6.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.MVPA.fit_6.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.no_diabetes.risk_ratio.MVPA.fit_6.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.MVPA.fit_6.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_6",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.no_diabetes.risk_ratio.MVPA.fit_6.stacked$boot.no_diabetes.risk_ratio.MVPA.fit_6.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.no_diabetes.risk_ratio.MVPA.fit_7.stacked[b, "boot.no_diabetes.risk_ratio.MVPA.fit_7.stacked.lb"] <- quantile(boot.no_diabetes.risk_ratio.MVPA.fit_7.stacked[1:b, "boot.no_diabetes.risk_ratio.MVPA.fit_7.stacked"], 0.025)
  boot.no_diabetes.risk_ratio.MVPA.fit_7.stacked[b, "boot.no_diabetes.risk_ratio.MVPA.fit_7.stacked.ub"] <- quantile(boot.no_diabetes.risk_ratio.MVPA.fit_7.stacked[1:b, "boot.no_diabetes.risk_ratio.MVPA.fit_7.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.no_diabetes.risk_ratio.MVPA.fit_7.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.MVPA.fit_7.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.no_diabetes.risk_ratio.MVPA.fit_7.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.MVPA.fit_7.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_7",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.no_diabetes.risk_ratio.MVPA.fit_7.stacked$boot.no_diabetes.risk_ratio.MVPA.fit_7.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.no_diabetes.risk_ratio.MVPA.fit_8.stacked[b, "boot.no_diabetes.risk_ratio.MVPA.fit_8.stacked.lb"] <- quantile(boot.no_diabetes.risk_ratio.MVPA.fit_8.stacked[1:b, "boot.no_diabetes.risk_ratio.MVPA.fit_8.stacked"], 0.025)
  boot.no_diabetes.risk_ratio.MVPA.fit_8.stacked[b, "boot.no_diabetes.risk_ratio.MVPA.fit_8.stacked.ub"] <- quantile(boot.no_diabetes.risk_ratio.MVPA.fit_8.stacked[1:b, "boot.no_diabetes.risk_ratio.MVPA.fit_8.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.no_diabetes.risk_ratio.MVPA.fit_8.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.MVPA.fit_8.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.no_diabetes.risk_ratio.MVPA.fit_8.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.MVPA.fit_8.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_8",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.no_diabetes.risk_ratio.MVPA.fit_8.stacked$boot.no_diabetes.risk_ratio.MVPA.fit_8.stacked, c(0.025, 0.975))

### DHD + 1 SD versus no change ----

#### Point estimates ----

# Step 1: data expansion 

block_1 <- imp.no_diabetes.stacked

block_2 <- imp.no_diabetes.stacked
for (i in 1:n_imp) {
  for (j in 1:nrow(data.no_diabetes)) {
    block_2[[i]][["DHD"]][[j]] <- block_2[[i]][["DHD"]][[j]] + sd(block_2[[i]][["DHD"]])
  }
}

block_3 <- imp.no_diabetes.stacked

# Step 2: outcome modelling 

DHD.fit_1.stacked <- list()
for (i in 1:n_imp) {
  DHD.fit_1.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                     DHD,
                                   data = block_1[[i]],
                                   x = TRUE)
}

DHD.fit_2.stacked <- list()
for (i in 1:n_imp) {
  DHD.fit_2.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                     DHD +
                                     sex.male_1 +
                                     age,
                                   data = block_1[[i]],
                                   x = TRUE)
}

DHD.fit_3.stacked <- list()
for (i in 1:n_imp) {
  DHD.fit_3.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                     DHD +
                                     sex.male_1 +
                                     age + 
                                     SES +
                                     pre_existing_ASCVD.yes_1,
                                   data = block_1[[i]],
                                   x = TRUE)
}

DHD.fit_4.stacked <- list()
for (i in 1:n_imp) {
  DHD.fit_4.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                    rcs(DHD, 3) +
                                    sex.male_1 +
                                    rcs(age, 3) + 
                                    rcs(SES, 3) +
                                    pre_existing_ASCVD.yes_1 +
                                    DHD:sex.male_1 + 
                                    age:sex.male_1 + 
                                    SES:sex.male_1 + 
                                    pre_existing_ASCVD.yes_1:sex.male_1,
                                   data = block_1[[i]],
                                   x = TRUE)
}

DHD.fit_5.stacked <- list()
for (i in 1:n_imp) {
  DHD.fit_5.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~
                                     DHD +
                                     sex.male_1 +
                                     age +
                                     SES +
                                     pre_existing_ASCVD.yes_1 +
                                     smoking.current_1 +
                                     MVPA,
                                   data = block_1[[i]],
                                   x = TRUE)
}

DHD.fit_6.stacked <- list()
for (i in 1:n_imp) {
  DHD.fit_6.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~
                                    rcs(DHD, 3) +
                                    sex.male_1 +
                                    rcs(age, 3) + 
                                    rcs(SES, 3) +
                                    pre_existing_ASCVD.yes_1 +
                                    smoking.current_1 +
                                    rcs(MVPA, 3) +
                                    DHD:sex.male_1 + 
                                    age:sex.male_1 + 
                                    SES:sex.male_1 + 
                                    pre_existing_ASCVD.yes_1:sex.male_1 +
                                    smoking.current_1:sex.male_1 +
                                    MVPA:sex.male_1,
                                   data = block_1[[i]],
                                   x = TRUE)
}

DHD.fit_7.stacked <- list()
for (i in 1:n_imp) {
  DHD.fit_7.stacked[[i]] <- CSC(Hist(time, event) ~ 
                                   DHD +
                                   sex.male_1 +
                                   age +
                                   SES +
                                   pre_existing_ASCVD.yes_1 +
                                   smoking.current_1 +
                                   MVPA,
                                 data = block_1[[i]])
}

DHD.fit_8.stacked <- list()
for (i in 1:n_imp) {
  DHD.fit_8.stacked[[i]] <- CSC(Hist(time, event) ~ 
                                   rcs(DHD, 3) +
                                   sex.male_1 +
                                   rcs(age, 3) + 
                                   rcs(SES, 3) +
                                   pre_existing_ASCVD.yes_1 +
                                   smoking.current_1 +
                                   rcs(MVPA, 3) +
                                   DHD:sex.male_1 + 
                                   age:sex.male_1 + 
                                   SES:sex.male_1 + 
                                   pre_existing_ASCVD.yes_1:sex.male_1 +
                                   smoking.current_1:sex.male_1 +
                                   MVPA:sex.male_1,
                                  data = block_1[[i]])
}

# Step 3: prediction 

block_2.pred_risk.DHD.fit_1.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.DHD.fit_1.stacked[[i]] <- predictRisk(DHD.fit_1.stacked[[i]], 
                                                          block_2[[i]], 
                                                          times = 5) 
}

block_2.pred_risk.DHD.fit_2.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.DHD.fit_2.stacked[[i]] <- predictRisk(DHD.fit_2.stacked[[i]], 
                                                          block_2[[i]], 
                                                          times = 5) 
}

block_2.pred_risk.DHD.fit_3.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.DHD.fit_3.stacked[[i]] <- predictRisk(DHD.fit_3.stacked[[i]], 
                                                          block_2[[i]], 
                                                          times = 5) 
}

block_2.pred_risk.DHD.fit_4.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.DHD.fit_4.stacked[[i]] <- predictRisk(DHD.fit_4.stacked[[i]], 
                                                          block_2[[i]], 
                                                          times = 5) 
}

block_2.pred_risk.DHD.fit_5.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.DHD.fit_5.stacked[[i]] <- predictRisk(DHD.fit_5.stacked[[i]], 
                                                          block_2[[i]], 
                                                          times = 5) 
}

block_2.pred_risk.DHD.fit_6.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.DHD.fit_6.stacked[[i]] <- predictRisk(DHD.fit_6.stacked[[i]], 
                                                          block_2[[i]], 
                                                          times = 5) 
}

block_2.pred_risk.DHD.fit_7.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.DHD.fit_7.stacked[[i]] <- predictRisk(DHD.fit_7.stacked[[i]], 
                                                          block_2[[i]], 
                                                          times = 5) 
}

block_2.pred_risk.DHD.fit_8.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.DHD.fit_8.stacked[[i]] <- predictRisk(DHD.fit_8.stacked[[i]], 
                                                          block_2[[i]], 
                                                          times = 5) 
}

block_3.pred_risk.DHD.fit_1.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.DHD.fit_1.stacked[[i]] <- predictRisk(DHD.fit_1.stacked[[i]], 
                                                          block_3[[i]], 
                                                          times = 5) 
}

block_3.pred_risk.DHD.fit_2.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.DHD.fit_2.stacked[[i]] <- predictRisk(DHD.fit_2.stacked[[i]], 
                                                          block_3[[i]], 
                                                          times = 5) 
}

block_3.pred_risk.DHD.fit_3.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.DHD.fit_3.stacked[[i]] <- predictRisk(DHD.fit_3.stacked[[i]], 
                                                          block_3[[i]], 
                                                          times = 5) 
}

block_3.pred_risk.DHD.fit_4.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.DHD.fit_4.stacked[[i]] <- predictRisk(DHD.fit_4.stacked[[i]], 
                                                          block_3[[i]], 
                                                          times = 5) 
}

block_3.pred_risk.DHD.fit_5.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.DHD.fit_5.stacked[[i]] <- predictRisk(DHD.fit_5.stacked[[i]], 
                                                          block_3[[i]], 
                                                          times = 5) 
}

block_3.pred_risk.DHD.fit_6.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.DHD.fit_6.stacked[[i]] <- predictRisk(DHD.fit_6.stacked[[i]], 
                                                          block_3[[i]], 
                                                          times = 5) 
}

block_3.pred_risk.DHD.fit_7.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.DHD.fit_7.stacked[[i]] <- predictRisk(DHD.fit_7.stacked[[i]], 
                                                          block_3[[i]], 
                                                          times = 5) 
}

block_3.pred_risk.DHD.fit_8.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.DHD.fit_8.stacked[[i]] <- predictRisk(DHD.fit_8.stacked[[i]], 
                                                          block_3[[i]], 
                                                          times = 5) 
}

# Step 4: standardization 

no_diabetes.risk_ratio.DHD.fit_1.stacked <- mean(unlist(block_2.pred_risk.DHD.fit_1.stacked)) / mean(unlist(block_3.pred_risk.DHD.fit_1.stacked)) 
no_diabetes.risk_ratio.DHD.fit_1.stacked

no_diabetes.risk_ratio.DHD.fit_2.stacked <- mean(unlist(block_2.pred_risk.DHD.fit_2.stacked)) / mean(unlist(block_3.pred_risk.DHD.fit_2.stacked)) 
no_diabetes.risk_ratio.DHD.fit_2.stacked

no_diabetes.risk_ratio.DHD.fit_3.stacked <- mean(unlist(block_2.pred_risk.DHD.fit_3.stacked)) / mean(unlist(block_3.pred_risk.DHD.fit_3.stacked)) 
no_diabetes.risk_ratio.DHD.fit_3.stacked

no_diabetes.risk_ratio.DHD.fit_4.stacked <- mean(unlist(block_2.pred_risk.DHD.fit_4.stacked)) / mean(unlist(block_3.pred_risk.DHD.fit_4.stacked)) 
no_diabetes.risk_ratio.DHD.fit_4.stacked

no_diabetes.risk_ratio.DHD.fit_5.stacked <- mean(unlist(block_2.pred_risk.DHD.fit_5.stacked)) / mean(unlist(block_3.pred_risk.DHD.fit_5.stacked)) 
no_diabetes.risk_ratio.DHD.fit_5.stacked

no_diabetes.risk_ratio.DHD.fit_6.stacked <- mean(unlist(block_2.pred_risk.DHD.fit_6.stacked)) / mean(unlist(block_3.pred_risk.DHD.fit_6.stacked)) 
no_diabetes.risk_ratio.DHD.fit_6.stacked

no_diabetes.risk_ratio.DHD.fit_7.stacked <- mean(unlist(block_2.pred_risk.DHD.fit_7.stacked)) / mean(unlist(block_3.pred_risk.DHD.fit_7.stacked)) 
no_diabetes.risk_ratio.DHD.fit_7.stacked

no_diabetes.risk_ratio.DHD.fit_8.stacked <- mean(unlist(block_2.pred_risk.DHD.fit_8.stacked)) / mean(unlist(block_3.pred_risk.DHD.fit_8.stacked)) 
no_diabetes.risk_ratio.DHD.fit_8.stacked

#### Bootstrapped confidence intervals ----

# Seed 

set.seed(...)

# Number of bootstraps

n_b <- 150

# Creating objects to capture output

boot.no_diabetes.risk_ratio.DHD.fit_1.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                      ncol = 2))
colnames(boot.no_diabetes.risk_ratio.DHD.fit_1.stacked) <- c("b",
                                                             "boot.no_diabetes.risk_ratio.DHD.fit_1.stacked")
boot.no_diabetes.risk_ratio.DHD.fit_1.stacked$b <- 1:n_b

boot.no_diabetes.risk_ratio.DHD.fit_2.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                      ncol = 2))
colnames(boot.no_diabetes.risk_ratio.DHD.fit_2.stacked) <- c("b",
                                                             "boot.no_diabetes.risk_ratio.DHD.fit_2.stacked")
boot.no_diabetes.risk_ratio.DHD.fit_2.stacked$b <- 1:n_b

boot.no_diabetes.risk_ratio.DHD.fit_3.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                      ncol = 2))
colnames(boot.no_diabetes.risk_ratio.DHD.fit_3.stacked) <- c("b",
                                                             "boot.no_diabetes.risk_ratio.DHD.fit_3.stacked")
boot.no_diabetes.risk_ratio.DHD.fit_3.stacked$b <- 1:n_b

boot.no_diabetes.risk_ratio.DHD.fit_4.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                      ncol = 2))
colnames(boot.no_diabetes.risk_ratio.DHD.fit_4.stacked) <- c("b",
                                                             "boot.no_diabetes.risk_ratio.DHD.fit_4.stacked")
boot.no_diabetes.risk_ratio.DHD.fit_4.stacked$b <- 1:n_b

boot.no_diabetes.risk_ratio.DHD.fit_5.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                      ncol = 2))
colnames(boot.no_diabetes.risk_ratio.DHD.fit_5.stacked) <- c("b",
                                                             "boot.no_diabetes.risk_ratio.DHD.fit_5.stacked")
boot.no_diabetes.risk_ratio.DHD.fit_5.stacked$b <- 1:n_b

boot.no_diabetes.risk_ratio.DHD.fit_6.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                      ncol = 2))
colnames(boot.no_diabetes.risk_ratio.DHD.fit_6.stacked) <- c("b",
                                                             "boot.no_diabetes.risk_ratio.DHD.fit_6.stacked")
boot.no_diabetes.risk_ratio.DHD.fit_6.stacked$b <- 1:n_b

boot.no_diabetes.risk_ratio.DHD.fit_7.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                      ncol = 2))
colnames(boot.no_diabetes.risk_ratio.DHD.fit_7.stacked) <- c("b",
                                                             "boot.no_diabetes.risk_ratio.DHD.fit_7.stacked")
boot.no_diabetes.risk_ratio.DHD.fit_7.stacked$b <- 1:n_b

boot.no_diabetes.risk_ratio.DHD.fit_8.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                      ncol = 2))
colnames(boot.no_diabetes.risk_ratio.DHD.fit_8.stacked) <- c("b",
                                                             "boot.no_diabetes.risk_ratio.DHD.fit_8.stacked")
boot.no_diabetes.risk_ratio.DHD.fit_8.stacked$b <- 1:n_b

# Loop

for (b in 1:n_b) {
  
  # Bootstrapped data
  
  boot.index <- sample(1:nrow(data.no_diabetes), size = nrow(data.no_diabetes), replace = TRUE)
  
  boot.data.no_diabetes <- data.no_diabetes[boot.index, ]
  
  boot.imp.no_diabetes <- boot.data.no_diabetes[, c( "event",
                                                     "time",
                                                     "sex.male_1",
                                                     "age",
                                                     "pre_existing_ASCVD.yes_1",
                                                     "SES",
                                                     "smoking.current_1",
                                                     "DHD",
                                                     "MVPA")]
  boot.imp.no_diabetes <- sapply(boot.imp.no_diabetes, zap_labels)
  boot.imp.no_diabetes <- mice(boot.imp.no_diabetes, method = "pmm", m = n_imp, print = FALSE)
  boot.imp.no_diabetes.stacked <- list()
  for (i in 1:n_imp) {
    boot.imp.no_diabetes.stacked[[i]] <- complete(boot.imp.no_diabetes, i)
    boot.imp.no_diabetes.stacked[[i]][["ASCVD_composite_event"]] <- ifelse(boot.imp.no_diabetes.stacked[[i]][["event"]] == 1,
                                                                           1,
                                                                           0) 
    boot.imp.no_diabetes.stacked[[i]][["overall_mortality_excl_ASCVD_mortality_event"]] <- ifelse(boot.imp.no_diabetes.stacked[[i]][["event"]] == 2,
                                                                                                  1,
                                                                                                  0) 
  }
  
  # Step 1: data expansion
  
  boot.block_1 <- boot.imp.no_diabetes.stacked
  
  boot.block_2 <- boot.imp.no_diabetes.stacked
  for (i in 1:n_imp) {
    for (j in 1:nrow(data.no_diabetes)) {
      boot.block_2[[i]][["DHD"]][[j]] <- boot.block_2[[i]][["DHD"]][[j]] + sd(boot.block_2[[i]][["DHD"]])
    }
  }
  
  boot.block_3 <- boot.imp.no_diabetes.stacked
  
  # Step 2: outcome modelling
  
  boot.DHD.fit_1.stacked <- list()
  for (i in 1:n_imp) {
    boot.DHD.fit_1.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                            DHD,
                                          data = boot.block_1[[i]],
                                          x = TRUE)
  }
  
  boot.DHD.fit_2.stacked <- list()
  for (i in 1:n_imp) {
    boot.DHD.fit_2.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                            DHD +
                                            sex.male_1 +
                                            age,
                                          data = boot.block_1[[i]],
                                          x = TRUE)
  }
  
  boot.DHD.fit_3.stacked <- list()
  for (i in 1:n_imp) {
    boot.DHD.fit_3.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                            DHD +
                                            sex.male_1 +
                                            age + 
                                            SES +
                                            pre_existing_ASCVD.yes_1,
                                          data = boot.block_1[[i]],
                                          x = TRUE)
  }
  
  boot.DHD.fit_4.stacked <- list()
  for (i in 1:n_imp) {
    boot.DHD.fit_4.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                           rcs(DHD, 3) +
                                           sex.male_1 +
                                           rcs(age, 3) + 
                                           rcs(SES, 3) +
                                           pre_existing_ASCVD.yes_1 +
                                           DHD:sex.male_1 + 
                                           age:sex.male_1 + 
                                           SES:sex.male_1 + 
                                           pre_existing_ASCVD.yes_1:sex.male_1,
                                          data = boot.block_1[[i]],
                                          x = TRUE)
  }
  
  boot.DHD.fit_5.stacked <- list()
  for (i in 1:n_imp) {
    boot.DHD.fit_5.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~
                                            DHD +
                                            sex.male_1 +
                                            age +
                                            SES +
                                            pre_existing_ASCVD.yes_1 +
                                            smoking.current_1 +
                                            MVPA,
                                          data = boot.block_1[[i]],
                                          x = TRUE)
  }
  
  boot.DHD.fit_6.stacked <- list()
  for (i in 1:n_imp) {
    boot.DHD.fit_6.stacked[[i]] <-  coxph(Surv(time, ASCVD_composite_event) ~
                                            rcs(DHD, 3) +
                                            sex.male_1 +
                                            rcs(age, 3) + 
                                            rcs(SES, 3) +
                                            pre_existing_ASCVD.yes_1 +
                                            smoking.current_1 + 
                                            rcs(MVPA, 3) +
                                            DHD:sex.male_1 + 
                                            age:sex.male_1 + 
                                            SES:sex.male_1 + 
                                            pre_existing_ASCVD.yes_1:sex.male_1 +
                                            smoking.current_1:sex.male_1 +
                                            MVPA:sex.male_1,
                                           data = boot.block_1[[i]],
                                           x = TRUE)
  }
  
  boot.DHD.fit_7.stacked <- list()
  for (i in 1:n_imp) {
    boot.DHD.fit_7.stacked[[i]] <- CSC(Hist(time, event) ~ 
                                          DHD +
                                          sex.male_1 +
                                          age +
                                          SES +
                                          pre_existing_ASCVD.yes_1 +
                                          smoking.current_1 +
                                          MVPA,
                                        data = boot.block_1[[i]])
  }
  
  boot.DHD.fit_8.stacked <- list()
  for (i in 1:n_imp) {
    boot.DHD.fit_8.stacked[[i]] <- CSC(Hist(time, event) ~ 
                                          rcs(DHD, 3) +
                                          sex.male_1 +
                                          rcs(age, 3) + 
                                          rcs(SES, 3) +
                                          pre_existing_ASCVD.yes_1 +
                                          smoking.current_1 + 
                                          rcs(MVPA, 3) +
                                          DHD:sex.male_1 + 
                                          age:sex.male_1 + 
                                          SES:sex.male_1 + 
                                          pre_existing_ASCVD.yes_1:sex.male_1 +
                                          smoking.current_1:sex.male_1 +
                                          MVPA:sex.male_1,
                                         data = boot.block_1[[i]])
  }
  
  # Step 3: prediction
  
  boot.block_2.pred_risk.DHD.fit_1.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.DHD.fit_1.stacked[[i]] <- predictRisk(boot.DHD.fit_1.stacked[[i]], 
                                                                 boot.block_2[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_2.pred_risk.DHD.fit_2.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.DHD.fit_2.stacked[[i]] <- predictRisk(boot.DHD.fit_2.stacked[[i]], 
                                                                 boot.block_2[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_2.pred_risk.DHD.fit_3.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.DHD.fit_3.stacked[[i]] <- predictRisk(boot.DHD.fit_3.stacked[[i]], 
                                                                 boot.block_2[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_2.pred_risk.DHD.fit_4.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.DHD.fit_4.stacked[[i]] <- predictRisk(boot.DHD.fit_4.stacked[[i]], 
                                                                 boot.block_2[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_2.pred_risk.DHD.fit_5.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.DHD.fit_5.stacked[[i]] <- predictRisk(boot.DHD.fit_5.stacked[[i]], 
                                                                 boot.block_2[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_2.pred_risk.DHD.fit_6.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.DHD.fit_6.stacked[[i]] <- predictRisk(boot.DHD.fit_6.stacked[[i]], 
                                                                 boot.block_2[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_2.pred_risk.DHD.fit_7.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.DHD.fit_7.stacked[[i]] <- predictRisk(boot.DHD.fit_7.stacked[[i]], 
                                                                 boot.block_2[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_2.pred_risk.DHD.fit_8.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.DHD.fit_8.stacked[[i]] <- predictRisk(boot.DHD.fit_8.stacked[[i]], 
                                                                 boot.block_2[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_3.pred_risk.DHD.fit_1.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.DHD.fit_1.stacked[[i]] <- predictRisk(boot.DHD.fit_1.stacked[[i]], 
                                                                 boot.block_3[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_3.pred_risk.DHD.fit_2.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.DHD.fit_2.stacked[[i]] <- predictRisk(boot.DHD.fit_2.stacked[[i]], 
                                                                 boot.block_3[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_3.pred_risk.DHD.fit_3.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.DHD.fit_3.stacked[[i]] <- predictRisk(boot.DHD.fit_3.stacked[[i]], 
                                                                 boot.block_3[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_3.pred_risk.DHD.fit_4.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.DHD.fit_4.stacked[[i]] <- predictRisk(boot.DHD.fit_4.stacked[[i]], 
                                                                 boot.block_3[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_3.pred_risk.DHD.fit_5.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.DHD.fit_5.stacked[[i]] <- predictRisk(boot.DHD.fit_5.stacked[[i]], 
                                                                 boot.block_3[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_3.pred_risk.DHD.fit_6.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.DHD.fit_6.stacked[[i]] <- predictRisk(boot.DHD.fit_6.stacked[[i]], 
                                                                 boot.block_3[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_3.pred_risk.DHD.fit_7.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.DHD.fit_7.stacked[[i]] <- predictRisk(boot.DHD.fit_7.stacked[[i]], 
                                                                 boot.block_3[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_3.pred_risk.DHD.fit_8.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.DHD.fit_8.stacked[[i]] <- predictRisk(boot.DHD.fit_8.stacked[[i]], 
                                                                 boot.block_3[[i]], 
                                                                 times = 5) 
  }
  
  # Step 4: standardization 
  
  boot.no_diabetes.risk_ratio.DHD.fit_1.stacked[b, "boot.no_diabetes.risk_ratio.DHD.fit_1.stacked"] <- mean(unlist(boot.block_2.pred_risk.DHD.fit_1.stacked)) / mean(unlist(boot.block_3.pred_risk.DHD.fit_1.stacked)) 
  
  boot.no_diabetes.risk_ratio.DHD.fit_2.stacked[b, "boot.no_diabetes.risk_ratio.DHD.fit_2.stacked"] <- mean(unlist(boot.block_2.pred_risk.DHD.fit_2.stacked)) / mean(unlist(boot.block_3.pred_risk.DHD.fit_2.stacked)) 
  
  boot.no_diabetes.risk_ratio.DHD.fit_3.stacked[b, "boot.no_diabetes.risk_ratio.DHD.fit_3.stacked"] <- mean(unlist(boot.block_2.pred_risk.DHD.fit_3.stacked)) / mean(unlist(boot.block_3.pred_risk.DHD.fit_3.stacked)) 
  
  boot.no_diabetes.risk_ratio.DHD.fit_4.stacked[b, "boot.no_diabetes.risk_ratio.DHD.fit_4.stacked"] <- mean(unlist(boot.block_2.pred_risk.DHD.fit_4.stacked)) / mean(unlist(boot.block_3.pred_risk.DHD.fit_4.stacked)) 
  
  boot.no_diabetes.risk_ratio.DHD.fit_5.stacked[b, "boot.no_diabetes.risk_ratio.DHD.fit_5.stacked"] <- mean(unlist(boot.block_2.pred_risk.DHD.fit_5.stacked)) / mean(unlist(boot.block_3.pred_risk.DHD.fit_5.stacked)) 
  
  boot.no_diabetes.risk_ratio.DHD.fit_6.stacked[b, "boot.no_diabetes.risk_ratio.DHD.fit_6.stacked"] <- mean(unlist(boot.block_2.pred_risk.DHD.fit_6.stacked)) / mean(unlist(boot.block_3.pred_risk.DHD.fit_6.stacked)) 
  
  boot.no_diabetes.risk_ratio.DHD.fit_7.stacked[b, "boot.no_diabetes.risk_ratio.DHD.fit_7.stacked"] <- mean(unlist(boot.block_2.pred_risk.DHD.fit_7.stacked)) / mean(unlist(boot.block_3.pred_risk.DHD.fit_7.stacked)) 
  
  boot.no_diabetes.risk_ratio.DHD.fit_8.stacked[b, "boot.no_diabetes.risk_ratio.DHD.fit_8.stacked"] <- mean(unlist(boot.block_2.pred_risk.DHD.fit_8.stacked)) / mean(unlist(boot.block_3.pred_risk.DHD.fit_8.stacked)) 
}

# Output

for (b in 10:n_b) {
  boot.no_diabetes.risk_ratio.DHD.fit_1.stacked[b, "boot.no_diabetes.risk_ratio.DHD.fit_1.stacked.lb"] <- quantile(boot.no_diabetes.risk_ratio.DHD.fit_1.stacked[1:b, "boot.no_diabetes.risk_ratio.DHD.fit_1.stacked"], 0.025)
  boot.no_diabetes.risk_ratio.DHD.fit_1.stacked[b, "boot.no_diabetes.risk_ratio.DHD.fit_1.stacked.ub"] <- quantile(boot.no_diabetes.risk_ratio.DHD.fit_1.stacked[1:b, "boot.no_diabetes.risk_ratio.DHD.fit_1.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.no_diabetes.risk_ratio.DHD.fit_1.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.DHD.fit_1.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.no_diabetes.risk_ratio.DHD.fit_1.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.DHD.fit_1.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_1",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.no_diabetes.risk_ratio.DHD.fit_1.stacked$boot.no_diabetes.risk_ratio.DHD.fit_1.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.no_diabetes.risk_ratio.DHD.fit_2.stacked[b, "boot.no_diabetes.risk_ratio.DHD.fit_2.stacked.lb"] <- quantile(boot.no_diabetes.risk_ratio.DHD.fit_2.stacked[1:b, "boot.no_diabetes.risk_ratio.DHD.fit_2.stacked"], 0.025)
  boot.no_diabetes.risk_ratio.DHD.fit_2.stacked[b, "boot.no_diabetes.risk_ratio.DHD.fit_2.stacked.ub"] <- quantile(boot.no_diabetes.risk_ratio.DHD.fit_2.stacked[1:b, "boot.no_diabetes.risk_ratio.DHD.fit_2.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.no_diabetes.risk_ratio.DHD.fit_2.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.DHD.fit_2.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.no_diabetes.risk_ratio.DHD.fit_2.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.DHD.fit_2.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_2",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.no_diabetes.risk_ratio.DHD.fit_2.stacked$boot.no_diabetes.risk_ratio.DHD.fit_2.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.no_diabetes.risk_ratio.DHD.fit_3.stacked[b, "boot.no_diabetes.risk_ratio.DHD.fit_3.stacked.lb"] <- quantile(boot.no_diabetes.risk_ratio.DHD.fit_3.stacked[1:b, "boot.no_diabetes.risk_ratio.DHD.fit_3.stacked"], 0.025)
  boot.no_diabetes.risk_ratio.DHD.fit_3.stacked[b, "boot.no_diabetes.risk_ratio.DHD.fit_3.stacked.ub"] <- quantile(boot.no_diabetes.risk_ratio.DHD.fit_3.stacked[1:b, "boot.no_diabetes.risk_ratio.DHD.fit_3.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.no_diabetes.risk_ratio.DHD.fit_3.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.DHD.fit_3.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.no_diabetes.risk_ratio.DHD.fit_3.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.DHD.fit_3.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_3",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.no_diabetes.risk_ratio.DHD.fit_3.stacked$boot.no_diabetes.risk_ratio.DHD.fit_3.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.no_diabetes.risk_ratio.DHD.fit_4.stacked[b, "boot.no_diabetes.risk_ratio.DHD.fit_4.stacked.lb"] <- quantile(boot.no_diabetes.risk_ratio.DHD.fit_4.stacked[1:b, "boot.no_diabetes.risk_ratio.DHD.fit_4.stacked"], 0.025)
  boot.no_diabetes.risk_ratio.DHD.fit_4.stacked[b, "boot.no_diabetes.risk_ratio.DHD.fit_4.stacked.ub"] <- quantile(boot.no_diabetes.risk_ratio.DHD.fit_4.stacked[1:b, "boot.no_diabetes.risk_ratio.DHD.fit_4.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.no_diabetes.risk_ratio.DHD.fit_4.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.DHD.fit_4.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.no_diabetes.risk_ratio.DHD.fit_4.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.DHD.fit_4.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_4",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.no_diabetes.risk_ratio.DHD.fit_4.stacked$boot.no_diabetes.risk_ratio.DHD.fit_4.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.no_diabetes.risk_ratio.DHD.fit_5.stacked[b, "boot.no_diabetes.risk_ratio.DHD.fit_5.stacked.lb"] <- quantile(boot.no_diabetes.risk_ratio.DHD.fit_5.stacked[1:b, "boot.no_diabetes.risk_ratio.DHD.fit_5.stacked"], 0.025)
  boot.no_diabetes.risk_ratio.DHD.fit_5.stacked[b, "boot.no_diabetes.risk_ratio.DHD.fit_5.stacked.ub"] <- quantile(boot.no_diabetes.risk_ratio.DHD.fit_5.stacked[1:b, "boot.no_diabetes.risk_ratio.DHD.fit_5.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.no_diabetes.risk_ratio.DHD.fit_5.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.DHD.fit_5.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.no_diabetes.risk_ratio.DHD.fit_5.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.DHD.fit_5.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_5",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.no_diabetes.risk_ratio.DHD.fit_5.stacked$boot.no_diabetes.risk_ratio.DHD.fit_5.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.no_diabetes.risk_ratio.DHD.fit_6.stacked[b, "boot.no_diabetes.risk_ratio.DHD.fit_6.stacked.lb"] <- quantile(boot.no_diabetes.risk_ratio.DHD.fit_6.stacked[1:b, "boot.no_diabetes.risk_ratio.DHD.fit_6.stacked"], 0.025)
  boot.no_diabetes.risk_ratio.DHD.fit_6.stacked[b, "boot.no_diabetes.risk_ratio.DHD.fit_6.stacked.ub"] <- quantile(boot.no_diabetes.risk_ratio.DHD.fit_6.stacked[1:b, "boot.no_diabetes.risk_ratio.DHD.fit_6.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.no_diabetes.risk_ratio.DHD.fit_6.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.DHD.fit_6.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.no_diabetes.risk_ratio.DHD.fit_6.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.DHD.fit_6.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_6",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.no_diabetes.risk_ratio.DHD.fit_6.stacked$boot.no_diabetes.risk_ratio.DHD.fit_6.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.no_diabetes.risk_ratio.DHD.fit_7.stacked[b, "boot.no_diabetes.risk_ratio.DHD.fit_7.stacked.lb"] <- quantile(boot.no_diabetes.risk_ratio.DHD.fit_7.stacked[1:b, "boot.no_diabetes.risk_ratio.DHD.fit_7.stacked"], 0.025)
  boot.no_diabetes.risk_ratio.DHD.fit_7.stacked[b, "boot.no_diabetes.risk_ratio.DHD.fit_7.stacked.ub"] <- quantile(boot.no_diabetes.risk_ratio.DHD.fit_7.stacked[1:b, "boot.no_diabetes.risk_ratio.DHD.fit_7.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.no_diabetes.risk_ratio.DHD.fit_7.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.DHD.fit_7.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.no_diabetes.risk_ratio.DHD.fit_7.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.DHD.fit_7.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_7",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.no_diabetes.risk_ratio.DHD.fit_7.stacked$boot.no_diabetes.risk_ratio.DHD.fit_7.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.no_diabetes.risk_ratio.DHD.fit_8.stacked[b, "boot.no_diabetes.risk_ratio.DHD.fit_8.stacked.lb"] <- quantile(boot.no_diabetes.risk_ratio.DHD.fit_8.stacked[1:b, "boot.no_diabetes.risk_ratio.DHD.fit_8.stacked"], 0.025)
  boot.no_diabetes.risk_ratio.DHD.fit_8.stacked[b, "boot.no_diabetes.risk_ratio.DHD.fit_8.stacked.ub"] <- quantile(boot.no_diabetes.risk_ratio.DHD.fit_8.stacked[1:b, "boot.no_diabetes.risk_ratio.DHD.fit_8.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.no_diabetes.risk_ratio.DHD.fit_8.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.DHD.fit_8.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.no_diabetes.risk_ratio.DHD.fit_8.stacked, aes(x = b, y = boot.no_diabetes.risk_ratio.DHD.fit_8.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_8",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.no_diabetes.risk_ratio.DHD.fit_8.stacked$boot.no_diabetes.risk_ratio.DHD.fit_8.stacked, c(0.025, 0.975))

## Participants with diabetes ----

### Not smoking versus smoking ----

#### Point estimates ----

# Step 1: data expansion 

block_1 <- imp.diabetes.stacked

block_2 <- imp.diabetes.stacked
for (i in 1:n_imp) {
  block_2[[i]][["smoking.current_1"]] <- 0
}

block_3 <- imp.diabetes.stacked
for (i in 1:n_imp) {
  block_3[[i]][["smoking.current_1"]] <- 1
}

# Step 2: outcome modelling 

smoking.fit_1.stacked <- list()
for (i in 1:n_imp) {
  smoking.fit_1.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                        smoking.current_1,
                                      data = block_1[[i]],
                                      x = TRUE)
}

smoking.fit_2.stacked <- list()
for (i in 1:n_imp) {
  smoking.fit_2.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                        smoking.current_1 +
                                        sex.male_1 +
                                        age,
                                      data = block_1[[i]],
                                      x = TRUE)
}

smoking.fit_3.stacked <- list()
for (i in 1:n_imp) {
  smoking.fit_3.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                        smoking.current_1 +
                                        sex.male_1 +
                                        age + 
                                        SES +
                                        pre_existing_ASCVD.yes_1,
                                      data = block_1[[i]],
                                      x = TRUE)
}

smoking.fit_4.stacked <- list()
for (i in 1:n_imp) {
  smoking.fit_4.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                        smoking.current_1 +
                                        sex.male_1 +
                                        rcs(age, 3) + 
                                        rcs(SES, 3) +
                                        pre_existing_ASCVD.yes_1 +
                                        smoking.current_1:sex.male_1 + 
                                        age:sex.male_1 +
                                        SES:sex.male_1 +
                                        pre_existing_ASCVD.yes_1:sex.male_1,
                                      data = block_1[[i]],
                                      x = TRUE)
}

smoking.fit_5.stacked <- list()
for (i in 1:n_imp) {
  smoking.fit_5.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                        smoking.current_1 +
                                        sex.male_1 +
                                        age +
                                        SES +
                                        pre_existing_ASCVD.yes_1 +
                                        MVPA +
                                        DHD,
                                      data = block_1[[i]],
                                      x = TRUE)
}

smoking.fit_6.stacked <- list()
for (i in 1:n_imp) {
  smoking.fit_6.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                        smoking.current_1 +
                                        sex.male_1 +
                                        rcs(age, 3) + 
                                        rcs(SES, 3) +
                                        pre_existing_ASCVD.yes_1 +
                                        rcs(MVPA, 3) +
                                        rcs(DHD, 3) +
                                        smoking.current_1:sex.male_1 + 
                                        age:sex.male_1 +
                                        SES:sex.male_1 +
                                        pre_existing_ASCVD.yes_1:sex.male_1 +
                                        MVPA:sex.male_1 + 
                                        DHD:sex.male_1,
                                      data = block_1[[i]],
                                      x = TRUE)
}

smoking.fit_7.stacked <- list()
for (i in 1:n_imp) {
  smoking.fit_7.stacked[[i]] <- CSC(Hist(time, event) ~ 
                                      smoking.current_1 +
                                      sex.male_1 +
                                      age +
                                      SES +
                                      pre_existing_ASCVD.yes_1 +
                                      MVPA +
                                      DHD,
                                    data = block_1[[i]])
}

smoking.fit_8.stacked <- list()
for (i in 1:n_imp) {
  smoking.fit_8.stacked[[i]] <- CSC(Hist(time, event) ~ 
                                       smoking.current_1 +
                                       sex.male_1 +
                                       rcs(age, 3) + 
                                       rcs(SES, 3) +
                                       pre_existing_ASCVD.yes_1 +
                                       rcs(MVPA, 3) +
                                       rcs(DHD, 3) +
                                       smoking.current_1:sex.male_1 + 
                                       age:sex.male_1 +
                                       SES:sex.male_1 +
                                       pre_existing_ASCVD.yes_1:sex.male_1 +
                                       MVPA:sex.male_1 + 
                                       DHD:sex.male_1,
                                     data = block_1[[i]])
}

# Step 3: prediction 

block_2.pred_risk.smoking.fit_1.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.smoking.fit_1.stacked[[i]] <- predictRisk(smoking.fit_1.stacked[[i]], 
                                                              block_2[[i]], 
                                                              times = 5) 
}

block_2.pred_risk.smoking.fit_2.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.smoking.fit_2.stacked[[i]] <- predictRisk(smoking.fit_2.stacked[[i]], 
                                                              block_2[[i]], 
                                                              times = 5) 
}

block_2.pred_risk.smoking.fit_3.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.smoking.fit_3.stacked[[i]] <- predictRisk(smoking.fit_3.stacked[[i]], 
                                                              block_2[[i]], 
                                                              times = 5) 
}

block_2.pred_risk.smoking.fit_4.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.smoking.fit_4.stacked[[i]] <- predictRisk(smoking.fit_4.stacked[[i]], 
                                                              block_2[[i]], 
                                                              times = 5) 
}

block_2.pred_risk.smoking.fit_5.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.smoking.fit_5.stacked[[i]] <- predictRisk(smoking.fit_5.stacked[[i]], 
                                                              block_2[[i]], 
                                                              times = 5) 
}

block_2.pred_risk.smoking.fit_6.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.smoking.fit_6.stacked[[i]] <- predictRisk(smoking.fit_6.stacked[[i]], 
                                                              block_2[[i]], 
                                                              times = 5) 
}

block_2.pred_risk.smoking.fit_7.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.smoking.fit_7.stacked[[i]] <- predictRisk(smoking.fit_7.stacked[[i]], 
                                                              block_2[[i]], 
                                                              times = 5) 
}

block_2.pred_risk.smoking.fit_8.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.smoking.fit_8.stacked[[i]] <- predictRisk(smoking.fit_8.stacked[[i]], 
                                                              block_2[[i]], 
                                                              times = 5) 
}

block_3.pred_risk.smoking.fit_1.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.smoking.fit_1.stacked[[i]] <- predictRisk(smoking.fit_1.stacked[[i]], 
                                                              block_3[[i]], 
                                                              times = 5) 
}

block_3.pred_risk.smoking.fit_2.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.smoking.fit_2.stacked[[i]] <- predictRisk(smoking.fit_2.stacked[[i]], 
                                                              block_3[[i]], 
                                                              times = 5) 
}

block_3.pred_risk.smoking.fit_3.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.smoking.fit_3.stacked[[i]] <- predictRisk(smoking.fit_3.stacked[[i]], 
                                                              block_3[[i]], 
                                                              times = 5) 
}

block_3.pred_risk.smoking.fit_4.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.smoking.fit_4.stacked[[i]] <- predictRisk(smoking.fit_4.stacked[[i]], 
                                                              block_3[[i]], 
                                                              times = 5) 
}

block_3.pred_risk.smoking.fit_5.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.smoking.fit_5.stacked[[i]] <- predictRisk(smoking.fit_5.stacked[[i]], 
                                                              block_3[[i]], 
                                                              times = 5) 
}

block_3.pred_risk.smoking.fit_6.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.smoking.fit_6.stacked[[i]] <- predictRisk(smoking.fit_6.stacked[[i]], 
                                                              block_3[[i]], 
                                                              times = 5) 
}

block_3.pred_risk.smoking.fit_7.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.smoking.fit_7.stacked[[i]] <- predictRisk(smoking.fit_7.stacked[[i]], 
                                                              block_3[[i]], 
                                                              times = 5) 
}

block_3.pred_risk.smoking.fit_8.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.smoking.fit_8.stacked[[i]] <- predictRisk(smoking.fit_8.stacked[[i]], 
                                                              block_3[[i]], 
                                                              times = 5) 
}

# Step 4: standardization 

diabetes.risk_ratio.smoking.fit_1.stacked <- mean(unlist(block_2.pred_risk.smoking.fit_1.stacked)) / mean(unlist(block_3.pred_risk.smoking.fit_1.stacked)) 
diabetes.risk_ratio.smoking.fit_1.stacked

diabetes.risk_ratio.smoking.fit_2.stacked <- mean(unlist(block_2.pred_risk.smoking.fit_2.stacked)) / mean(unlist(block_3.pred_risk.smoking.fit_2.stacked)) 
diabetes.risk_ratio.smoking.fit_2.stacked

diabetes.risk_ratio.smoking.fit_3.stacked <- mean(unlist(block_2.pred_risk.smoking.fit_3.stacked)) / mean(unlist(block_3.pred_risk.smoking.fit_3.stacked)) 
diabetes.risk_ratio.smoking.fit_3.stacked

diabetes.risk_ratio.smoking.fit_4.stacked <- mean(unlist(block_2.pred_risk.smoking.fit_4.stacked)) / mean(unlist(block_3.pred_risk.smoking.fit_4.stacked)) 
diabetes.risk_ratio.smoking.fit_4.stacked

diabetes.risk_ratio.smoking.fit_5.stacked <- mean(unlist(block_2.pred_risk.smoking.fit_5.stacked)) / mean(unlist(block_3.pred_risk.smoking.fit_5.stacked)) 
diabetes.risk_ratio.smoking.fit_5.stacked

diabetes.risk_ratio.smoking.fit_6.stacked <- mean(unlist(block_2.pred_risk.smoking.fit_6.stacked)) / mean(unlist(block_3.pred_risk.smoking.fit_6.stacked)) 
diabetes.risk_ratio.smoking.fit_6.stacked

diabetes.risk_ratio.smoking.fit_7.stacked <- mean(unlist(block_2.pred_risk.smoking.fit_7.stacked)) / mean(unlist(block_3.pred_risk.smoking.fit_7.stacked)) 
diabetes.risk_ratio.smoking.fit_7.stacked

diabetes.risk_ratio.smoking.fit_8.stacked <- mean(unlist(block_2.pred_risk.smoking.fit_8.stacked)) / mean(unlist(block_3.pred_risk.smoking.fit_8.stacked)) 
diabetes.risk_ratio.smoking.fit_8.stacked

#### Bootstrapped confidence intervals ----

# Seed 

set.seed(...)

# Number of bootstraps

n_b <- 150

# Creating objects to capture output

boot.diabetes.risk_ratio.smoking.fit_1.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                       ncol = 2))
colnames(boot.diabetes.risk_ratio.smoking.fit_1.stacked) <- c("b",
                                                              "boot.diabetes.risk_ratio.smoking.fit_1.stacked")
boot.diabetes.risk_ratio.smoking.fit_1.stacked$b <- 1:n_b

boot.diabetes.risk_ratio.smoking.fit_2.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                       ncol = 2))
colnames(boot.diabetes.risk_ratio.smoking.fit_2.stacked) <- c("b",
                                                              "boot.diabetes.risk_ratio.smoking.fit_2.stacked")
boot.diabetes.risk_ratio.smoking.fit_2.stacked$b <- 1:n_b

boot.diabetes.risk_ratio.smoking.fit_3.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                       ncol = 2))
colnames(boot.diabetes.risk_ratio.smoking.fit_3.stacked) <- c("b",
                                                              "boot.diabetes.risk_ratio.smoking.fit_3.stacked")
boot.diabetes.risk_ratio.smoking.fit_3.stacked$b <- 1:n_b

boot.diabetes.risk_ratio.smoking.fit_4.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                       ncol = 2))
colnames(boot.diabetes.risk_ratio.smoking.fit_4.stacked) <- c("b",
                                                              "boot.diabetes.risk_ratio.smoking.fit_4.stacked")
boot.diabetes.risk_ratio.smoking.fit_4.stacked$b <- 1:n_b

boot.diabetes.risk_ratio.smoking.fit_5.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                       ncol = 2))
colnames(boot.diabetes.risk_ratio.smoking.fit_5.stacked) <- c("b",
                                                              "boot.diabetes.risk_ratio.smoking.fit_5.stacked")
boot.diabetes.risk_ratio.smoking.fit_5.stacked$b <- 1:n_b

boot.diabetes.risk_ratio.smoking.fit_6.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                       ncol = 2))
colnames(boot.diabetes.risk_ratio.smoking.fit_6.stacked) <- c("b",
                                                              "boot.diabetes.risk_ratio.smoking.fit_6.stacked")
boot.diabetes.risk_ratio.smoking.fit_6.stacked$b <- 1:n_b

boot.diabetes.risk_ratio.smoking.fit_7.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                       ncol = 2))
colnames(boot.diabetes.risk_ratio.smoking.fit_7.stacked) <- c("b",
                                                              "boot.diabetes.risk_ratio.smoking.fit_7.stacked")
boot.diabetes.risk_ratio.smoking.fit_7.stacked$b <- 1:n_b

boot.diabetes.risk_ratio.smoking.fit_8.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                       ncol = 2))
colnames(boot.diabetes.risk_ratio.smoking.fit_8.stacked) <- c("b",
                                                              "boot.diabetes.risk_ratio.smoking.fit_8.stacked")
boot.diabetes.risk_ratio.smoking.fit_8.stacked$b <- 1:n_b

# Loop

for (b in 1:n_b) {
  
  # Bootstrapped data
  
  boot.index <- sample(1:nrow(data.diabetes), size = nrow(data.diabetes), replace = TRUE)
  
  boot.data.diabetes <- data.diabetes[boot.index, ]
  
  boot.imp.diabetes <- boot.data.diabetes[, c( "event",
                                               "time",
                                               "sex.male_1",
                                               "age",
                                               "pre_existing_ASCVD.yes_1",
                                               "SES",
                                               "smoking.current_1",
                                               "MVPA",
                                               "DHD")]
  boot.imp.diabetes <- sapply(boot.imp.diabetes, zap_labels)
  boot.imp.diabetes <- mice(boot.imp.diabetes, method = "pmm", m = n_imp, print = FALSE)
  boot.imp.diabetes.stacked <- list()
  for (i in 1:n_imp) {
    boot.imp.diabetes.stacked[[i]] <- complete(boot.imp.diabetes, i)
    boot.imp.diabetes.stacked[[i]][["ASCVD_composite_event"]] <- ifelse(boot.imp.diabetes.stacked[[i]][["event"]] == 1,
                                                                        1,
                                                                        0) 
    boot.imp.diabetes.stacked[[i]][["overall_mortality_excl_ASCVD_mortality_event"]] <- ifelse(boot.imp.diabetes.stacked[[i]][["event"]] == 2,
                                                                                               1,
                                                                                               0) 
  }
  
  # Step 1: data expansion
  
  boot.block_1 <- boot.imp.diabetes.stacked
  
  boot.block_2 <- boot.imp.diabetes.stacked
  for (i in 1:n_imp) {
    boot.block_2[[i]][["smoking.current_1"]] <- 0
  }
  
  boot.block_3 <- boot.imp.diabetes.stacked
  for (i in 1:n_imp) {
    boot.block_3[[i]][["smoking.current_1"]] <- 1
  }
  
  # Step 2: outcome modelling
  
  boot.smoking.fit_1.stacked <- list()
  for (i in 1:n_imp) {
    boot.smoking.fit_1.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                               smoking.current_1,
                                             data = boot.block_1[[i]],
                                             x = TRUE)
  }
  
  boot.smoking.fit_2.stacked <- list()
  for (i in 1:n_imp) {
    boot.smoking.fit_2.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                               smoking.current_1 +
                                               sex.male_1 +
                                               age,
                                             data = boot.block_1[[i]],
                                             x = TRUE)
  }
  
  boot.smoking.fit_3.stacked <- list()
  for (i in 1:n_imp) {
    boot.smoking.fit_3.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                               smoking.current_1 +
                                               sex.male_1 +
                                               age + 
                                               SES +
                                               pre_existing_ASCVD.yes_1,
                                             data = boot.block_1[[i]],
                                             x = TRUE)
  }
  
  boot.smoking.fit_4.stacked <- list()
  for (i in 1:n_imp) {
    boot.smoking.fit_4.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                               smoking.current_1 +
                                               sex.male_1 +
                                               rcs(age, 3) + 
                                               rcs(SES, 3) +
                                               pre_existing_ASCVD.yes_1 +
                                               smoking.current_1:sex.male_1 + 
                                               age:sex.male_1 +
                                               SES:sex.male_1 +
                                               pre_existing_ASCVD.yes_1:sex.male_1,
                                             data = boot.block_1[[i]],
                                             x = TRUE)
  }
  
  boot.smoking.fit_5.stacked <- list()
  for (i in 1:n_imp) {
    boot.smoking.fit_5.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                               smoking.current_1 +
                                               sex.male_1 +
                                               age +
                                               SES +
                                               pre_existing_ASCVD.yes_1 +
                                               MVPA +
                                               DHD,
                                             data = boot.block_1[[i]],
                                             x = TRUE)
  }
  
  boot.smoking.fit_6.stacked <- list()
  for (i in 1:n_imp) {
    boot.smoking.fit_6.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                               smoking.current_1 +
                                               sex.male_1 +
                                               rcs(age, 3) + 
                                               rcs(SES, 3) +
                                               pre_existing_ASCVD.yes_1 +
                                               rcs(MVPA, 3) +
                                               rcs(DHD, 3) +
                                               smoking.current_1:sex.male_1 + 
                                               age:sex.male_1 +
                                               SES:sex.male_1 +
                                               pre_existing_ASCVD.yes_1:sex.male_1 +
                                               MVPA:sex.male_1 + 
                                               DHD:sex.male_1,
                                             data = boot.block_1[[i]],
                                             x = TRUE)
  }
  
  boot.smoking.fit_7.stacked <- list()
  for (i in 1:n_imp) {
    boot.smoking.fit_7.stacked[[i]] <- CSC(Hist(time, event) ~ 
                                             smoking.current_1 +
                                             sex.male_1 +
                                             age +
                                             SES +
                                             pre_existing_ASCVD.yes_1 +
                                             MVPA +
                                             DHD,
                                           data = boot.block_1[[i]])
  }
  
  boot.smoking.fit_8.stacked <- list()
  for (i in 1:n_imp) {
    boot.smoking.fit_8.stacked[[i]] <- CSC(Hist(time, event) ~ 
                                              smoking.current_1 +
                                              sex.male_1 +
                                              rcs(age, 3) + 
                                              rcs(SES, 3) +
                                              pre_existing_ASCVD.yes_1 +
                                              rcs(MVPA, 3) +
                                              rcs(DHD, 3) +
                                              smoking.current_1:sex.male_1 + 
                                              age:sex.male_1 +
                                              SES:sex.male_1 +
                                              pre_existing_ASCVD.yes_1:sex.male_1 +
                                              MVPA:sex.male_1 + 
                                              DHD:sex.male_1,
                                            data = boot.block_1[[i]])
  }
  
  # Step 3: prediction
  
  boot.block_2.pred_risk.smoking.fit_1.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.smoking.fit_1.stacked[[i]] <- predictRisk(boot.smoking.fit_1.stacked[[i]], 
                                                                     boot.block_2[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_2.pred_risk.smoking.fit_2.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.smoking.fit_2.stacked[[i]] <- predictRisk(boot.smoking.fit_2.stacked[[i]], 
                                                                     boot.block_2[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_2.pred_risk.smoking.fit_3.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.smoking.fit_3.stacked[[i]] <- predictRisk(boot.smoking.fit_3.stacked[[i]], 
                                                                     boot.block_2[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_2.pred_risk.smoking.fit_4.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.smoking.fit_4.stacked[[i]] <- predictRisk(boot.smoking.fit_4.stacked[[i]], 
                                                                     boot.block_2[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_2.pred_risk.smoking.fit_5.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.smoking.fit_5.stacked[[i]] <- predictRisk(boot.smoking.fit_5.stacked[[i]], 
                                                                     boot.block_2[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_2.pred_risk.smoking.fit_6.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.smoking.fit_6.stacked[[i]] <- predictRisk(boot.smoking.fit_6.stacked[[i]], 
                                                                     boot.block_2[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_2.pred_risk.smoking.fit_7.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.smoking.fit_7.stacked[[i]] <- predictRisk(boot.smoking.fit_7.stacked[[i]], 
                                                                     boot.block_2[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_2.pred_risk.smoking.fit_8.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.smoking.fit_8.stacked[[i]] <- predictRisk(boot.smoking.fit_8.stacked[[i]], 
                                                                     boot.block_2[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_3.pred_risk.smoking.fit_1.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.smoking.fit_1.stacked[[i]] <- predictRisk(boot.smoking.fit_1.stacked[[i]], 
                                                                     boot.block_3[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_3.pred_risk.smoking.fit_2.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.smoking.fit_2.stacked[[i]] <- predictRisk(boot.smoking.fit_2.stacked[[i]], 
                                                                     boot.block_3[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_3.pred_risk.smoking.fit_3.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.smoking.fit_3.stacked[[i]] <- predictRisk(boot.smoking.fit_3.stacked[[i]], 
                                                                     boot.block_3[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_3.pred_risk.smoking.fit_4.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.smoking.fit_4.stacked[[i]] <- predictRisk(boot.smoking.fit_4.stacked[[i]], 
                                                                     boot.block_3[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_3.pred_risk.smoking.fit_5.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.smoking.fit_5.stacked[[i]] <- predictRisk(boot.smoking.fit_5.stacked[[i]], 
                                                                     boot.block_3[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_3.pred_risk.smoking.fit_6.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.smoking.fit_6.stacked[[i]] <- predictRisk(boot.smoking.fit_6.stacked[[i]], 
                                                                     boot.block_3[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_3.pred_risk.smoking.fit_7.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.smoking.fit_7.stacked[[i]] <- predictRisk(boot.smoking.fit_7.stacked[[i]], 
                                                                     boot.block_3[[i]], 
                                                                     times = 5) 
  }
  
  boot.block_3.pred_risk.smoking.fit_8.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.smoking.fit_8.stacked[[i]] <- predictRisk(boot.smoking.fit_8.stacked[[i]], 
                                                                     boot.block_3[[i]], 
                                                                     times = 5) 
  }
  
  # Step 4: standardization 
  
  boot.diabetes.risk_ratio.smoking.fit_1.stacked[b, "boot.diabetes.risk_ratio.smoking.fit_1.stacked"] <- mean(unlist(boot.block_2.pred_risk.smoking.fit_1.stacked)) / mean(unlist(boot.block_3.pred_risk.smoking.fit_1.stacked)) 
  
  boot.diabetes.risk_ratio.smoking.fit_2.stacked[b, "boot.diabetes.risk_ratio.smoking.fit_2.stacked"] <- mean(unlist(boot.block_2.pred_risk.smoking.fit_2.stacked)) / mean(unlist(boot.block_3.pred_risk.smoking.fit_2.stacked)) 
  
  boot.diabetes.risk_ratio.smoking.fit_3.stacked[b, "boot.diabetes.risk_ratio.smoking.fit_3.stacked"] <- mean(unlist(boot.block_2.pred_risk.smoking.fit_3.stacked)) / mean(unlist(boot.block_3.pred_risk.smoking.fit_3.stacked)) 
  
  boot.diabetes.risk_ratio.smoking.fit_4.stacked[b, "boot.diabetes.risk_ratio.smoking.fit_4.stacked"] <- mean(unlist(boot.block_2.pred_risk.smoking.fit_4.stacked)) / mean(unlist(boot.block_3.pred_risk.smoking.fit_4.stacked)) 
  
  boot.diabetes.risk_ratio.smoking.fit_5.stacked[b, "boot.diabetes.risk_ratio.smoking.fit_5.stacked"] <- mean(unlist(boot.block_2.pred_risk.smoking.fit_5.stacked)) / mean(unlist(boot.block_3.pred_risk.smoking.fit_5.stacked)) 
  
  boot.diabetes.risk_ratio.smoking.fit_6.stacked[b, "boot.diabetes.risk_ratio.smoking.fit_6.stacked"] <- mean(unlist(boot.block_2.pred_risk.smoking.fit_6.stacked)) / mean(unlist(boot.block_3.pred_risk.smoking.fit_6.stacked)) 
  
  boot.diabetes.risk_ratio.smoking.fit_7.stacked[b, "boot.diabetes.risk_ratio.smoking.fit_7.stacked"] <- mean(unlist(boot.block_2.pred_risk.smoking.fit_7.stacked)) / mean(unlist(boot.block_3.pred_risk.smoking.fit_7.stacked)) 
  
  boot.diabetes.risk_ratio.smoking.fit_8.stacked[b, "boot.diabetes.risk_ratio.smoking.fit_8.stacked"] <- mean(unlist(boot.block_2.pred_risk.smoking.fit_8.stacked)) / mean(unlist(boot.block_3.pred_risk.smoking.fit_8.stacked)) 
}

# Output

for (b in 10:n_b) {
  boot.diabetes.risk_ratio.smoking.fit_1.stacked[b, "boot.diabetes.risk_ratio.smoking.fit_1.stacked.lb"] <- quantile(boot.diabetes.risk_ratio.smoking.fit_1.stacked[1:b, "boot.diabetes.risk_ratio.smoking.fit_1.stacked"], 0.025)
  boot.diabetes.risk_ratio.smoking.fit_1.stacked[b, "boot.diabetes.risk_ratio.smoking.fit_1.stacked.ub"] <- quantile(boot.diabetes.risk_ratio.smoking.fit_1.stacked[1:b, "boot.diabetes.risk_ratio.smoking.fit_1.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.diabetes.risk_ratio.smoking.fit_1.stacked, aes(x = b, y = boot.diabetes.risk_ratio.smoking.fit_1.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.diabetes.risk_ratio.smoking.fit_1.stacked, aes(x = b, y = boot.diabetes.risk_ratio.smoking.fit_1.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_1",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.diabetes.risk_ratio.smoking.fit_1.stacked$boot.diabetes.risk_ratio.smoking.fit_1.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.diabetes.risk_ratio.smoking.fit_2.stacked[b, "boot.diabetes.risk_ratio.smoking.fit_2.stacked.lb"] <- quantile(boot.diabetes.risk_ratio.smoking.fit_2.stacked[1:b, "boot.diabetes.risk_ratio.smoking.fit_2.stacked"], 0.025)
  boot.diabetes.risk_ratio.smoking.fit_2.stacked[b, "boot.diabetes.risk_ratio.smoking.fit_2.stacked.ub"] <- quantile(boot.diabetes.risk_ratio.smoking.fit_2.stacked[1:b, "boot.diabetes.risk_ratio.smoking.fit_2.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.diabetes.risk_ratio.smoking.fit_2.stacked, aes(x = b, y = boot.diabetes.risk_ratio.smoking.fit_2.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.diabetes.risk_ratio.smoking.fit_2.stacked, aes(x = b, y = boot.diabetes.risk_ratio.smoking.fit_2.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_2",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.diabetes.risk_ratio.smoking.fit_2.stacked$boot.diabetes.risk_ratio.smoking.fit_2.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.diabetes.risk_ratio.smoking.fit_3.stacked[b, "boot.diabetes.risk_ratio.smoking.fit_3.stacked.lb"] <- quantile(boot.diabetes.risk_ratio.smoking.fit_3.stacked[1:b, "boot.diabetes.risk_ratio.smoking.fit_3.stacked"], 0.025)
  boot.diabetes.risk_ratio.smoking.fit_3.stacked[b, "boot.diabetes.risk_ratio.smoking.fit_3.stacked.ub"] <- quantile(boot.diabetes.risk_ratio.smoking.fit_3.stacked[1:b, "boot.diabetes.risk_ratio.smoking.fit_3.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.diabetes.risk_ratio.smoking.fit_3.stacked, aes(x = b, y = boot.diabetes.risk_ratio.smoking.fit_3.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.diabetes.risk_ratio.smoking.fit_3.stacked, aes(x = b, y = boot.diabetes.risk_ratio.smoking.fit_3.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_3",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.diabetes.risk_ratio.smoking.fit_3.stacked$boot.diabetes.risk_ratio.smoking.fit_3.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.diabetes.risk_ratio.smoking.fit_4.stacked[b, "boot.diabetes.risk_ratio.smoking.fit_4.stacked.lb"] <- quantile(boot.diabetes.risk_ratio.smoking.fit_4.stacked[1:b, "boot.diabetes.risk_ratio.smoking.fit_4.stacked"], 0.025)
  boot.diabetes.risk_ratio.smoking.fit_4.stacked[b, "boot.diabetes.risk_ratio.smoking.fit_4.stacked.ub"] <- quantile(boot.diabetes.risk_ratio.smoking.fit_4.stacked[1:b, "boot.diabetes.risk_ratio.smoking.fit_4.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.diabetes.risk_ratio.smoking.fit_4.stacked, aes(x = b, y = boot.diabetes.risk_ratio.smoking.fit_4.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.diabetes.risk_ratio.smoking.fit_4.stacked, aes(x = b, y = boot.diabetes.risk_ratio.smoking.fit_4.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_4",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.diabetes.risk_ratio.smoking.fit_4.stacked$boot.diabetes.risk_ratio.smoking.fit_4.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.diabetes.risk_ratio.smoking.fit_5.stacked[b, "boot.diabetes.risk_ratio.smoking.fit_5.stacked.lb"] <- quantile(boot.diabetes.risk_ratio.smoking.fit_5.stacked[1:b, "boot.diabetes.risk_ratio.smoking.fit_5.stacked"], 0.025)
  boot.diabetes.risk_ratio.smoking.fit_5.stacked[b, "boot.diabetes.risk_ratio.smoking.fit_5.stacked.ub"] <- quantile(boot.diabetes.risk_ratio.smoking.fit_5.stacked[1:b, "boot.diabetes.risk_ratio.smoking.fit_5.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.diabetes.risk_ratio.smoking.fit_5.stacked, aes(x = b, y = boot.diabetes.risk_ratio.smoking.fit_5.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.diabetes.risk_ratio.smoking.fit_5.stacked, aes(x = b, y = boot.diabetes.risk_ratio.smoking.fit_5.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_5",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.diabetes.risk_ratio.smoking.fit_5.stacked$boot.diabetes.risk_ratio.smoking.fit_5.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.diabetes.risk_ratio.smoking.fit_6.stacked[b, "boot.diabetes.risk_ratio.smoking.fit_6.stacked.lb"] <- quantile(boot.diabetes.risk_ratio.smoking.fit_6.stacked[1:b, "boot.diabetes.risk_ratio.smoking.fit_6.stacked"], 0.025)
  boot.diabetes.risk_ratio.smoking.fit_6.stacked[b, "boot.diabetes.risk_ratio.smoking.fit_6.stacked.ub"] <- quantile(boot.diabetes.risk_ratio.smoking.fit_6.stacked[1:b, "boot.diabetes.risk_ratio.smoking.fit_6.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.diabetes.risk_ratio.smoking.fit_6.stacked, aes(x = b, y = boot.diabetes.risk_ratio.smoking.fit_6.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.diabetes.risk_ratio.smoking.fit_6.stacked, aes(x = b, y = boot.diabetes.risk_ratio.smoking.fit_6.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_6",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.diabetes.risk_ratio.smoking.fit_6.stacked$boot.diabetes.risk_ratio.smoking.fit_6.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.diabetes.risk_ratio.smoking.fit_7.stacked[b, "boot.diabetes.risk_ratio.smoking.fit_7.stacked.lb"] <- quantile(boot.diabetes.risk_ratio.smoking.fit_7.stacked[1:b, "boot.diabetes.risk_ratio.smoking.fit_7.stacked"], 0.025)
  boot.diabetes.risk_ratio.smoking.fit_7.stacked[b, "boot.diabetes.risk_ratio.smoking.fit_7.stacked.ub"] <- quantile(boot.diabetes.risk_ratio.smoking.fit_7.stacked[1:b, "boot.diabetes.risk_ratio.smoking.fit_7.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.diabetes.risk_ratio.smoking.fit_7.stacked, aes(x = b, y = boot.diabetes.risk_ratio.smoking.fit_7.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.diabetes.risk_ratio.smoking.fit_7.stacked, aes(x = b, y = boot.diabetes.risk_ratio.smoking.fit_7.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_7",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.diabetes.risk_ratio.smoking.fit_7.stacked$boot.diabetes.risk_ratio.smoking.fit_7.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.diabetes.risk_ratio.smoking.fit_8.stacked[b, "boot.diabetes.risk_ratio.smoking.fit_8.stacked.lb"] <- quantile(boot.diabetes.risk_ratio.smoking.fit_8.stacked[1:b, "boot.diabetes.risk_ratio.smoking.fit_8.stacked"], 0.025)
  boot.diabetes.risk_ratio.smoking.fit_8.stacked[b, "boot.diabetes.risk_ratio.smoking.fit_8.stacked.ub"] <- quantile(boot.diabetes.risk_ratio.smoking.fit_8.stacked[1:b, "boot.diabetes.risk_ratio.smoking.fit_8.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.diabetes.risk_ratio.smoking.fit_8.stacked, aes(x = b, y = boot.diabetes.risk_ratio.smoking.fit_8.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.diabetes.risk_ratio.smoking.fit_8.stacked, aes(x = b, y = boot.diabetes.risk_ratio.smoking.fit_8.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_8",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.diabetes.risk_ratio.smoking.fit_8.stacked$boot.diabetes.risk_ratio.smoking.fit_8.stacked, c(0.025, 0.975))

### MVPA + 1 SD versus no change ----

#### Point estimates ----

# Step 1: data expansion 

block_1 <- imp.diabetes.stacked

block_2 <- imp.diabetes.stacked
for (i in 1:n_imp) {
  for (j in 1:nrow(data.diabetes)) {
    block_2[[i]][["MVPA"]][[j]] <- block_2[[i]][["MVPA"]][[j]] + sd(block_2[[i]][["MVPA"]])
  }
}

block_3 <- imp.diabetes.stacked

# Step 2: outcome modelling 

MVPA.fit_1.stacked <- list()
for (i in 1:n_imp) {
  MVPA.fit_1.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                     MVPA,
                                   data = block_1[[i]],
                                   x = TRUE)
}

MVPA.fit_2.stacked <- list()
for (i in 1:n_imp) {
  MVPA.fit_2.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                     MVPA +
                                     sex.male_1 +
                                     age,
                                   data = block_1[[i]],
                                   x = TRUE)
}

MVPA.fit_3.stacked <- list()
for (i in 1:n_imp) {
  MVPA.fit_3.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                     MVPA +
                                     sex.male_1 +
                                     age + 
                                     SES +
                                     pre_existing_ASCVD.yes_1,
                                   data = block_1[[i]],
                                   x = TRUE)
}

MVPA.fit_4.stacked <- list()
for (i in 1:n_imp) {
  MVPA.fit_4.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                     rcs(MVPA, 3) +
                                     sex.male_1 +
                                     rcs(age, 3) + 
                                     rcs(SES, 3) +
                                     pre_existing_ASCVD.yes_1 +
                                     MVPA:sex.male_1 + 
                                     age:sex.male_1 + 
                                     SES:sex.male_1 +
                                     pre_existing_ASCVD.yes_1:sex.male_1,
                                   data = block_1[[i]],
                                   x = TRUE)
}

MVPA.fit_5.stacked <- list()
for (i in 1:n_imp) {
  MVPA.fit_5.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~
                                     MVPA +
                                     sex.male_1 +
                                     age +
                                     SES +
                                     pre_existing_ASCVD.yes_1 +
                                     smoking.current_1 +
                                     DHD,
                                   data = block_1[[i]],
                                   x = TRUE)
}

MVPA.fit_6.stacked <- list()
for (i in 1:n_imp) {
  MVPA.fit_6.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~
                                     rcs(MVPA, 3) +
                                     sex.male_1 +
                                     rcs(age, 3) + 
                                     rcs(SES, 3) +
                                     pre_existing_ASCVD.yes_1 +
                                     smoking.current_1 +
                                     rcs(DHD, 3) +
                                     MVPA:sex.male_1 + 
                                     age:sex.male_1 + 
                                     SES:sex.male_1 +
                                     pre_existing_ASCVD.yes_1:sex.male_1 + 
                                     smoking.current_1:sex.male_1 +
                                     DHD:sex.male_1,
                                   data = block_1[[i]],
                                   x = TRUE)
}

MVPA.fit_7.stacked <- list()
for (i in 1:n_imp) {
  MVPA.fit_7.stacked[[i]] <- CSC(Hist(time, event) ~ 
                                   MVPA +
                                   sex.male_1 +
                                   age +
                                   SES +
                                   pre_existing_ASCVD.yes_1 +
                                   smoking.current_1 +
                                   DHD,
                                 data = block_1[[i]])
}

MVPA.fit_8.stacked <- list()
for (i in 1:n_imp) {
  MVPA.fit_8.stacked[[i]] <- CSC(Hist(time, event) ~ 
                                    rcs(MVPA, 3) +
                                    sex.male_1 +
                                    rcs(age, 3) + 
                                    rcs(SES, 3) +
                                    pre_existing_ASCVD.yes_1 +
                                    smoking.current_1 +
                                    rcs(DHD, 3) +
                                    MVPA:sex.male_1 + 
                                    age:sex.male_1 + 
                                    SES:sex.male_1 +
                                    pre_existing_ASCVD.yes_1:sex.male_1 + 
                                    smoking.current_1:sex.male_1 +
                                    DHD:sex.male_1,
                                  data = block_1[[i]])
}

# Step 3: prediction 

block_2.pred_risk.MVPA.fit_1.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.MVPA.fit_1.stacked[[i]] <- predictRisk(MVPA.fit_1.stacked[[i]], 
                                                           block_2[[i]], 
                                                           times = 5) 
}

block_2.pred_risk.MVPA.fit_2.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.MVPA.fit_2.stacked[[i]] <- predictRisk(MVPA.fit_2.stacked[[i]], 
                                                           block_2[[i]], 
                                                           times = 5) 
}

block_2.pred_risk.MVPA.fit_3.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.MVPA.fit_3.stacked[[i]] <- predictRisk(MVPA.fit_3.stacked[[i]], 
                                                           block_2[[i]], 
                                                           times = 5) 
}

block_2.pred_risk.MVPA.fit_4.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.MVPA.fit_4.stacked[[i]] <- predictRisk(MVPA.fit_4.stacked[[i]], 
                                                           block_2[[i]], 
                                                           times = 5) 
}

block_2.pred_risk.MVPA.fit_5.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.MVPA.fit_5.stacked[[i]] <- predictRisk(MVPA.fit_5.stacked[[i]], 
                                                           block_2[[i]], 
                                                           times = 5) 
}

block_2.pred_risk.MVPA.fit_6.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.MVPA.fit_6.stacked[[i]] <- predictRisk(MVPA.fit_6.stacked[[i]], 
                                                           block_2[[i]], 
                                                           times = 5) 
}

block_2.pred_risk.MVPA.fit_7.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.MVPA.fit_7.stacked[[i]] <- predictRisk(MVPA.fit_7.stacked[[i]], 
                                                           block_2[[i]], 
                                                           times = 5) 
}

block_2.pred_risk.MVPA.fit_8.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.MVPA.fit_8.stacked[[i]] <- predictRisk(MVPA.fit_8.stacked[[i]], 
                                                           block_2[[i]], 
                                                           times = 5) 
}

block_3.pred_risk.MVPA.fit_1.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.MVPA.fit_1.stacked[[i]] <- predictRisk(MVPA.fit_1.stacked[[i]], 
                                                           block_3[[i]], 
                                                           times = 5) 
}

block_3.pred_risk.MVPA.fit_2.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.MVPA.fit_2.stacked[[i]] <- predictRisk(MVPA.fit_2.stacked[[i]], 
                                                           block_3[[i]], 
                                                           times = 5) 
}

block_3.pred_risk.MVPA.fit_3.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.MVPA.fit_3.stacked[[i]] <- predictRisk(MVPA.fit_3.stacked[[i]], 
                                                           block_3[[i]], 
                                                           times = 5) 
}

block_3.pred_risk.MVPA.fit_4.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.MVPA.fit_4.stacked[[i]] <- predictRisk(MVPA.fit_4.stacked[[i]], 
                                                           block_3[[i]], 
                                                           times = 5) 
}

block_3.pred_risk.MVPA.fit_5.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.MVPA.fit_5.stacked[[i]] <- predictRisk(MVPA.fit_5.stacked[[i]], 
                                                           block_3[[i]], 
                                                           times = 5) 
}

block_3.pred_risk.MVPA.fit_6.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.MVPA.fit_6.stacked[[i]] <- predictRisk(MVPA.fit_6.stacked[[i]], 
                                                           block_3[[i]], 
                                                           times = 5) 
}

block_3.pred_risk.MVPA.fit_7.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.MVPA.fit_7.stacked[[i]] <- predictRisk(MVPA.fit_7.stacked[[i]], 
                                                           block_3[[i]], 
                                                           times = 5) 
}

block_3.pred_risk.MVPA.fit_8.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.MVPA.fit_8.stacked[[i]] <- predictRisk(MVPA.fit_8.stacked[[i]], 
                                                           block_3[[i]], 
                                                           times = 5) 
}

# Step 4: standardization 

diabetes.risk_ratio.MVPA.fit_1.stacked <- mean(unlist(block_2.pred_risk.MVPA.fit_1.stacked)) / mean(unlist(block_3.pred_risk.MVPA.fit_1.stacked)) 
diabetes.risk_ratio.MVPA.fit_1.stacked

diabetes.risk_ratio.MVPA.fit_2.stacked <- mean(unlist(block_2.pred_risk.MVPA.fit_2.stacked)) / mean(unlist(block_3.pred_risk.MVPA.fit_2.stacked)) 
diabetes.risk_ratio.MVPA.fit_2.stacked

diabetes.risk_ratio.MVPA.fit_3.stacked <- mean(unlist(block_2.pred_risk.MVPA.fit_3.stacked)) / mean(unlist(block_3.pred_risk.MVPA.fit_3.stacked)) 
diabetes.risk_ratio.MVPA.fit_3.stacked

diabetes.risk_ratio.MVPA.fit_4.stacked <- mean(unlist(block_2.pred_risk.MVPA.fit_4.stacked)) / mean(unlist(block_3.pred_risk.MVPA.fit_4.stacked)) 
diabetes.risk_ratio.MVPA.fit_4.stacked

diabetes.risk_ratio.MVPA.fit_5.stacked <- mean(unlist(block_2.pred_risk.MVPA.fit_5.stacked)) / mean(unlist(block_3.pred_risk.MVPA.fit_5.stacked)) 
diabetes.risk_ratio.MVPA.fit_5.stacked

diabetes.risk_ratio.MVPA.fit_6.stacked <- mean(unlist(block_2.pred_risk.MVPA.fit_6.stacked)) / mean(unlist(block_3.pred_risk.MVPA.fit_6.stacked)) 
diabetes.risk_ratio.MVPA.fit_6.stacked

diabetes.risk_ratio.MVPA.fit_7.stacked <- mean(unlist(block_2.pred_risk.MVPA.fit_7.stacked)) / mean(unlist(block_3.pred_risk.MVPA.fit_7.stacked)) 
diabetes.risk_ratio.MVPA.fit_7.stacked

diabetes.risk_ratio.MVPA.fit_8.stacked <- mean(unlist(block_2.pred_risk.MVPA.fit_8.stacked)) / mean(unlist(block_3.pred_risk.MVPA.fit_8.stacked)) 
diabetes.risk_ratio.MVPA.fit_8.stacked

#### Bootstrapped confidence intervals ----

# Seed 

set.seed(...)

# Number of bootstraps

n_b <- 150

# Creating objects to capture output

boot.diabetes.risk_ratio.MVPA.fit_1.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                    ncol = 2))
colnames(boot.diabetes.risk_ratio.MVPA.fit_1.stacked) <- c("b",
                                                           "boot.diabetes.risk_ratio.MVPA.fit_1.stacked")
boot.diabetes.risk_ratio.MVPA.fit_1.stacked$b <- 1:n_b

boot.diabetes.risk_ratio.MVPA.fit_2.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                    ncol = 2))
colnames(boot.diabetes.risk_ratio.MVPA.fit_2.stacked) <- c("b",
                                                           "boot.diabetes.risk_ratio.MVPA.fit_2.stacked")
boot.diabetes.risk_ratio.MVPA.fit_2.stacked$b <- 1:n_b

boot.diabetes.risk_ratio.MVPA.fit_3.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                    ncol = 2))
colnames(boot.diabetes.risk_ratio.MVPA.fit_3.stacked) <- c("b",
                                                           "boot.diabetes.risk_ratio.MVPA.fit_3.stacked")
boot.diabetes.risk_ratio.MVPA.fit_3.stacked$b <- 1:n_b

boot.diabetes.risk_ratio.MVPA.fit_4.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                    ncol = 2))
colnames(boot.diabetes.risk_ratio.MVPA.fit_4.stacked) <- c("b",
                                                           "boot.diabetes.risk_ratio.MVPA.fit_4.stacked")
boot.diabetes.risk_ratio.MVPA.fit_4.stacked$b <- 1:n_b

boot.diabetes.risk_ratio.MVPA.fit_5.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                    ncol = 2))
colnames(boot.diabetes.risk_ratio.MVPA.fit_5.stacked) <- c("b",
                                                           "boot.diabetes.risk_ratio.MVPA.fit_5.stacked")
boot.diabetes.risk_ratio.MVPA.fit_5.stacked$b <- 1:n_b

boot.diabetes.risk_ratio.MVPA.fit_6.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                    ncol = 2))
colnames(boot.diabetes.risk_ratio.MVPA.fit_6.stacked) <- c("b",
                                                           "boot.diabetes.risk_ratio.MVPA.fit_6.stacked")
boot.diabetes.risk_ratio.MVPA.fit_6.stacked$b <- 1:n_b

boot.diabetes.risk_ratio.MVPA.fit_7.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                    ncol = 2))
colnames(boot.diabetes.risk_ratio.MVPA.fit_7.stacked) <- c("b",
                                                           "boot.diabetes.risk_ratio.MVPA.fit_7.stacked")
boot.diabetes.risk_ratio.MVPA.fit_7.stacked$b <- 1:n_b

boot.diabetes.risk_ratio.MVPA.fit_8.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                    ncol = 2))
colnames(boot.diabetes.risk_ratio.MVPA.fit_8.stacked) <- c("b",
                                                           "boot.diabetes.risk_ratio.MVPA.fit_8.stacked")
boot.diabetes.risk_ratio.MVPA.fit_8.stacked$b <- 1:n_b

# Loop

for (b in 1:n_b) {
  
  # Bootstrapped data
  
  boot.index <- sample(1:nrow(data.diabetes), size = nrow(data.diabetes), replace = TRUE)
  
  boot.data.diabetes <- data.diabetes[boot.index, ]
  
  boot.imp.diabetes <- boot.data.diabetes[, c( "event",
                                               "time",
                                               "sex.male_1",
                                               "age",
                                               "pre_existing_ASCVD.yes_1",
                                               "SES",
                                               "smoking.current_1",
                                               "MVPA",
                                               "DHD")]
  boot.imp.diabetes <- sapply(boot.imp.diabetes, zap_labels)
  boot.imp.diabetes <- mice(boot.imp.diabetes, method = "pmm", m = n_imp, print = FALSE)
  boot.imp.diabetes.stacked <- list()
  for (i in 1:n_imp) {
    boot.imp.diabetes.stacked[[i]] <- complete(boot.imp.diabetes, i)
    boot.imp.diabetes.stacked[[i]][["ASCVD_composite_event"]] <- ifelse(boot.imp.diabetes.stacked[[i]][["event"]] == 1,
                                                                        1,
                                                                        0) 
    boot.imp.diabetes.stacked[[i]][["overall_mortality_excl_ASCVD_mortality_event"]] <- ifelse(boot.imp.diabetes.stacked[[i]][["event"]] == 2,
                                                                                               1,
                                                                                               0) 
  }
  
  # Step 1: data expansion
  
  boot.block_1 <- boot.imp.diabetes.stacked
  
  boot.block_2 <- boot.imp.diabetes.stacked
  for (i in 1:n_imp) {
    for (j in 1:nrow(data.diabetes)) {
      boot.block_2[[i]][["MVPA"]][[j]] <- boot.block_2[[i]][["MVPA"]][[j]] + sd(boot.block_2[[i]][["MVPA"]])
    }
  }
  
  boot.block_3 <- boot.imp.diabetes.stacked
  
  # Step 2: outcome modelling
  
  boot.MVPA.fit_1.stacked <- list()
  for (i in 1:n_imp) {
    boot.MVPA.fit_1.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                            MVPA,
                                          data = boot.block_1[[i]],
                                          x = TRUE)
  }
  
  boot.MVPA.fit_2.stacked <- list()
  for (i in 1:n_imp) {
    boot.MVPA.fit_2.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                            MVPA +
                                            sex.male_1 +
                                            age,
                                          data = boot.block_1[[i]],
                                          x = TRUE)
  }
  
  boot.MVPA.fit_3.stacked <- list()
  for (i in 1:n_imp) {
    boot.MVPA.fit_3.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                            MVPA +
                                            sex.male_1 +
                                            age + 
                                            SES +
                                            pre_existing_ASCVD.yes_1,
                                          data = boot.block_1[[i]],
                                          x = TRUE)
  }
  
  boot.MVPA.fit_4.stacked <- list()
  for (i in 1:n_imp) {
    boot.MVPA.fit_4.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                            rcs(MVPA, 3) +
                                            sex.male_1 +
                                            rcs(age, 3) + 
                                            rcs(SES, 3) +
                                            pre_existing_ASCVD.yes_1 +
                                            MVPA:sex.male_1 + 
                                            age:sex.male_1 + 
                                            SES:sex.male_1 +
                                            pre_existing_ASCVD.yes_1:sex.male_1,
                                          data = boot.block_1[[i]],
                                          x = TRUE)
  }
  
  boot.MVPA.fit_5.stacked <- list()
  for (i in 1:n_imp) {
    boot.MVPA.fit_5.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~
                                            MVPA +
                                            sex.male_1 +
                                            age +
                                            SES +
                                            pre_existing_ASCVD.yes_1 +
                                            smoking.current_1 +
                                            DHD,
                                          data = boot.block_1[[i]],
                                          x = TRUE)
  }
  
  boot.MVPA.fit_6.stacked <- list()
  for (i in 1:n_imp) {
    boot.MVPA.fit_6.stacked[[i]] <-  coxph(Surv(time, ASCVD_composite_event) ~
                                             rcs(MVPA, 3) +
                                             sex.male_1 +
                                             rcs(age, 3) + 
                                             rcs(SES, 3) +
                                             pre_existing_ASCVD.yes_1 +
                                             smoking.current_1 +
                                             rcs(DHD, 3) +
                                             MVPA:sex.male_1 + 
                                             age:sex.male_1 + 
                                             SES:sex.male_1 +
                                             pre_existing_ASCVD.yes_1:sex.male_1 + 
                                             smoking.current_1:sex.male_1 +
                                             DHD:sex.male_1,
                                           data = boot.block_1[[i]],
                                           x = TRUE)
  }
  
  boot.MVPA.fit_7.stacked <- list()
  for (i in 1:n_imp) {
    boot.MVPA.fit_7.stacked[[i]] <- CSC(Hist(time, event) ~ 
                                          MVPA +
                                          sex.male_1 +
                                          age +
                                          SES +
                                          pre_existing_ASCVD.yes_1 +
                                          smoking.current_1 +
                                          DHD,
                                        data = boot.block_1[[i]])
  }
  
  boot.MVPA.fit_8.stacked <- list()
  for (i in 1:n_imp) {
    boot.MVPA.fit_8.stacked[[i]] <- CSC(Hist(time, event) ~ 
                                           rcs(MVPA, 3) +
                                           sex.male_1 +
                                           rcs(age, 3) + 
                                           rcs(SES, 3) +
                                           pre_existing_ASCVD.yes_1 +
                                           smoking.current_1 +
                                           rcs(DHD, 3) +
                                           MVPA:sex.male_1 + 
                                           age:sex.male_1 + 
                                           SES:sex.male_1 +
                                           pre_existing_ASCVD.yes_1:sex.male_1 + 
                                           smoking.current_1:sex.male_1 +
                                           DHD:sex.male_1,
                                         data = boot.block_1[[i]])
  }
  
  # Step 3: prediction
  
  boot.block_2.pred_risk.MVPA.fit_1.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.MVPA.fit_1.stacked[[i]] <- predictRisk(boot.MVPA.fit_1.stacked[[i]], 
                                                                  boot.block_2[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_2.pred_risk.MVPA.fit_2.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.MVPA.fit_2.stacked[[i]] <- predictRisk(boot.MVPA.fit_2.stacked[[i]], 
                                                                  boot.block_2[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_2.pred_risk.MVPA.fit_3.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.MVPA.fit_3.stacked[[i]] <- predictRisk(boot.MVPA.fit_3.stacked[[i]], 
                                                                  boot.block_2[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_2.pred_risk.MVPA.fit_4.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.MVPA.fit_4.stacked[[i]] <- predictRisk(boot.MVPA.fit_4.stacked[[i]], 
                                                                  boot.block_2[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_2.pred_risk.MVPA.fit_5.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.MVPA.fit_5.stacked[[i]] <- predictRisk(boot.MVPA.fit_5.stacked[[i]], 
                                                                  boot.block_2[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_2.pred_risk.MVPA.fit_6.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.MVPA.fit_6.stacked[[i]] <- predictRisk(boot.MVPA.fit_6.stacked[[i]], 
                                                                  boot.block_2[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_2.pred_risk.MVPA.fit_7.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.MVPA.fit_7.stacked[[i]] <- predictRisk(boot.MVPA.fit_7.stacked[[i]], 
                                                                  boot.block_2[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_2.pred_risk.MVPA.fit_8.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.MVPA.fit_8.stacked[[i]] <- predictRisk(boot.MVPA.fit_8.stacked[[i]], 
                                                                  boot.block_2[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_3.pred_risk.MVPA.fit_1.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.MVPA.fit_1.stacked[[i]] <- predictRisk(boot.MVPA.fit_1.stacked[[i]], 
                                                                  boot.block_3[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_3.pred_risk.MVPA.fit_2.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.MVPA.fit_2.stacked[[i]] <- predictRisk(boot.MVPA.fit_2.stacked[[i]], 
                                                                  boot.block_3[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_3.pred_risk.MVPA.fit_3.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.MVPA.fit_3.stacked[[i]] <- predictRisk(boot.MVPA.fit_3.stacked[[i]], 
                                                                  boot.block_3[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_3.pred_risk.MVPA.fit_4.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.MVPA.fit_4.stacked[[i]] <- predictRisk(boot.MVPA.fit_4.stacked[[i]], 
                                                                  boot.block_3[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_3.pred_risk.MVPA.fit_5.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.MVPA.fit_5.stacked[[i]] <- predictRisk(boot.MVPA.fit_5.stacked[[i]], 
                                                                  boot.block_3[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_3.pred_risk.MVPA.fit_6.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.MVPA.fit_6.stacked[[i]] <- predictRisk(boot.MVPA.fit_6.stacked[[i]], 
                                                                  boot.block_3[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_3.pred_risk.MVPA.fit_7.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.MVPA.fit_7.stacked[[i]] <- predictRisk(boot.MVPA.fit_7.stacked[[i]], 
                                                                  boot.block_3[[i]], 
                                                                  times = 5) 
  }
  
  boot.block_3.pred_risk.MVPA.fit_8.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.MVPA.fit_8.stacked[[i]] <- predictRisk(boot.MVPA.fit_8.stacked[[i]], 
                                                                  boot.block_3[[i]], 
                                                                  times = 5) 
  }
  
  # Step 4: standardization 
  
  boot.diabetes.risk_ratio.MVPA.fit_1.stacked[b, "boot.diabetes.risk_ratio.MVPA.fit_1.stacked"] <- mean(unlist(boot.block_2.pred_risk.MVPA.fit_1.stacked)) / mean(unlist(boot.block_3.pred_risk.MVPA.fit_1.stacked)) 
  
  boot.diabetes.risk_ratio.MVPA.fit_2.stacked[b, "boot.diabetes.risk_ratio.MVPA.fit_2.stacked"] <- mean(unlist(boot.block_2.pred_risk.MVPA.fit_2.stacked)) / mean(unlist(boot.block_3.pred_risk.MVPA.fit_2.stacked)) 
  
  boot.diabetes.risk_ratio.MVPA.fit_3.stacked[b, "boot.diabetes.risk_ratio.MVPA.fit_3.stacked"] <- mean(unlist(boot.block_2.pred_risk.MVPA.fit_3.stacked)) / mean(unlist(boot.block_3.pred_risk.MVPA.fit_3.stacked)) 
  
  boot.diabetes.risk_ratio.MVPA.fit_4.stacked[b, "boot.diabetes.risk_ratio.MVPA.fit_4.stacked"] <- mean(unlist(boot.block_2.pred_risk.MVPA.fit_4.stacked)) / mean(unlist(boot.block_3.pred_risk.MVPA.fit_4.stacked)) 
  
  boot.diabetes.risk_ratio.MVPA.fit_5.stacked[b, "boot.diabetes.risk_ratio.MVPA.fit_5.stacked"] <- mean(unlist(boot.block_2.pred_risk.MVPA.fit_5.stacked)) / mean(unlist(boot.block_3.pred_risk.MVPA.fit_5.stacked)) 
  
  boot.diabetes.risk_ratio.MVPA.fit_6.stacked[b, "boot.diabetes.risk_ratio.MVPA.fit_6.stacked"] <- mean(unlist(boot.block_2.pred_risk.MVPA.fit_6.stacked)) / mean(unlist(boot.block_3.pred_risk.MVPA.fit_6.stacked)) 
  
  boot.diabetes.risk_ratio.MVPA.fit_7.stacked[b, "boot.diabetes.risk_ratio.MVPA.fit_7.stacked"] <- mean(unlist(boot.block_2.pred_risk.MVPA.fit_7.stacked)) / mean(unlist(boot.block_3.pred_risk.MVPA.fit_7.stacked)) 
  
  boot.diabetes.risk_ratio.MVPA.fit_8.stacked[b, "boot.diabetes.risk_ratio.MVPA.fit_8.stacked"] <- mean(unlist(boot.block_2.pred_risk.MVPA.fit_8.stacked)) / mean(unlist(boot.block_3.pred_risk.MVPA.fit_8.stacked)) 
}

# Output

for (b in 10:n_b) {
  boot.diabetes.risk_ratio.MVPA.fit_1.stacked[b, "boot.diabetes.risk_ratio.MVPA.fit_1.stacked.lb"] <- quantile(boot.diabetes.risk_ratio.MVPA.fit_1.stacked[1:b, "boot.diabetes.risk_ratio.MVPA.fit_1.stacked"], 0.025)
  boot.diabetes.risk_ratio.MVPA.fit_1.stacked[b, "boot.diabetes.risk_ratio.MVPA.fit_1.stacked.ub"] <- quantile(boot.diabetes.risk_ratio.MVPA.fit_1.stacked[1:b, "boot.diabetes.risk_ratio.MVPA.fit_1.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.diabetes.risk_ratio.MVPA.fit_1.stacked, aes(x = b, y = boot.diabetes.risk_ratio.MVPA.fit_1.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.diabetes.risk_ratio.MVPA.fit_1.stacked, aes(x = b, y = boot.diabetes.risk_ratio.MVPA.fit_1.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_1",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.diabetes.risk_ratio.MVPA.fit_1.stacked$boot.diabetes.risk_ratio.MVPA.fit_1.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.diabetes.risk_ratio.MVPA.fit_2.stacked[b, "boot.diabetes.risk_ratio.MVPA.fit_2.stacked.lb"] <- quantile(boot.diabetes.risk_ratio.MVPA.fit_2.stacked[1:b, "boot.diabetes.risk_ratio.MVPA.fit_2.stacked"], 0.025)
  boot.diabetes.risk_ratio.MVPA.fit_2.stacked[b, "boot.diabetes.risk_ratio.MVPA.fit_2.stacked.ub"] <- quantile(boot.diabetes.risk_ratio.MVPA.fit_2.stacked[1:b, "boot.diabetes.risk_ratio.MVPA.fit_2.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.diabetes.risk_ratio.MVPA.fit_2.stacked, aes(x = b, y = boot.diabetes.risk_ratio.MVPA.fit_2.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.diabetes.risk_ratio.MVPA.fit_2.stacked, aes(x = b, y = boot.diabetes.risk_ratio.MVPA.fit_2.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_2",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.diabetes.risk_ratio.MVPA.fit_2.stacked$boot.diabetes.risk_ratio.MVPA.fit_2.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.diabetes.risk_ratio.MVPA.fit_3.stacked[b, "boot.diabetes.risk_ratio.MVPA.fit_3.stacked.lb"] <- quantile(boot.diabetes.risk_ratio.MVPA.fit_3.stacked[1:b, "boot.diabetes.risk_ratio.MVPA.fit_3.stacked"], 0.025)
  boot.diabetes.risk_ratio.MVPA.fit_3.stacked[b, "boot.diabetes.risk_ratio.MVPA.fit_3.stacked.ub"] <- quantile(boot.diabetes.risk_ratio.MVPA.fit_3.stacked[1:b, "boot.diabetes.risk_ratio.MVPA.fit_3.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.diabetes.risk_ratio.MVPA.fit_3.stacked, aes(x = b, y = boot.diabetes.risk_ratio.MVPA.fit_3.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.diabetes.risk_ratio.MVPA.fit_3.stacked, aes(x = b, y = boot.diabetes.risk_ratio.MVPA.fit_3.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_3",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.diabetes.risk_ratio.MVPA.fit_3.stacked$boot.diabetes.risk_ratio.MVPA.fit_3.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.diabetes.risk_ratio.MVPA.fit_4.stacked[b, "boot.diabetes.risk_ratio.MVPA.fit_4.stacked.lb"] <- quantile(boot.diabetes.risk_ratio.MVPA.fit_4.stacked[1:b, "boot.diabetes.risk_ratio.MVPA.fit_4.stacked"], 0.025)
  boot.diabetes.risk_ratio.MVPA.fit_4.stacked[b, "boot.diabetes.risk_ratio.MVPA.fit_4.stacked.ub"] <- quantile(boot.diabetes.risk_ratio.MVPA.fit_4.stacked[1:b, "boot.diabetes.risk_ratio.MVPA.fit_4.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.diabetes.risk_ratio.MVPA.fit_4.stacked, aes(x = b, y = boot.diabetes.risk_ratio.MVPA.fit_4.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.diabetes.risk_ratio.MVPA.fit_4.stacked, aes(x = b, y = boot.diabetes.risk_ratio.MVPA.fit_4.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_4",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) + 
  theme_minimal()
quantile(boot.diabetes.risk_ratio.MVPA.fit_4.stacked$boot.diabetes.risk_ratio.MVPA.fit_4.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.diabetes.risk_ratio.MVPA.fit_5.stacked[b, "boot.diabetes.risk_ratio.MVPA.fit_5.stacked.lb"] <- quantile(boot.diabetes.risk_ratio.MVPA.fit_5.stacked[1:b, "boot.diabetes.risk_ratio.MVPA.fit_5.stacked"], 0.025)
  boot.diabetes.risk_ratio.MVPA.fit_5.stacked[b, "boot.diabetes.risk_ratio.MVPA.fit_5.stacked.ub"] <- quantile(boot.diabetes.risk_ratio.MVPA.fit_5.stacked[1:b, "boot.diabetes.risk_ratio.MVPA.fit_5.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.diabetes.risk_ratio.MVPA.fit_5.stacked, aes(x = b, y = boot.diabetes.risk_ratio.MVPA.fit_5.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.diabetes.risk_ratio.MVPA.fit_5.stacked, aes(x = b, y = boot.diabetes.risk_ratio.MVPA.fit_5.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_5",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.diabetes.risk_ratio.MVPA.fit_5.stacked$boot.diabetes.risk_ratio.MVPA.fit_5.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.diabetes.risk_ratio.MVPA.fit_6.stacked[b, "boot.diabetes.risk_ratio.MVPA.fit_6.stacked.lb"] <- quantile(boot.diabetes.risk_ratio.MVPA.fit_6.stacked[1:b, "boot.diabetes.risk_ratio.MVPA.fit_6.stacked"], 0.025)
  boot.diabetes.risk_ratio.MVPA.fit_6.stacked[b, "boot.diabetes.risk_ratio.MVPA.fit_6.stacked.ub"] <- quantile(boot.diabetes.risk_ratio.MVPA.fit_6.stacked[1:b, "boot.diabetes.risk_ratio.MVPA.fit_6.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.diabetes.risk_ratio.MVPA.fit_6.stacked, aes(x = b, y = boot.diabetes.risk_ratio.MVPA.fit_6.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.diabetes.risk_ratio.MVPA.fit_6.stacked, aes(x = b, y = boot.diabetes.risk_ratio.MVPA.fit_6.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_6",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.diabetes.risk_ratio.MVPA.fit_6.stacked$boot.diabetes.risk_ratio.MVPA.fit_6.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.diabetes.risk_ratio.MVPA.fit_7.stacked[b, "boot.diabetes.risk_ratio.MVPA.fit_7.stacked.lb"] <- quantile(boot.diabetes.risk_ratio.MVPA.fit_7.stacked[1:b, "boot.diabetes.risk_ratio.MVPA.fit_7.stacked"], 0.025)
  boot.diabetes.risk_ratio.MVPA.fit_7.stacked[b, "boot.diabetes.risk_ratio.MVPA.fit_7.stacked.ub"] <- quantile(boot.diabetes.risk_ratio.MVPA.fit_7.stacked[1:b, "boot.diabetes.risk_ratio.MVPA.fit_7.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.diabetes.risk_ratio.MVPA.fit_7.stacked, aes(x = b, y = boot.diabetes.risk_ratio.MVPA.fit_7.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.diabetes.risk_ratio.MVPA.fit_7.stacked, aes(x = b, y = boot.diabetes.risk_ratio.MVPA.fit_7.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_7",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.diabetes.risk_ratio.MVPA.fit_7.stacked$boot.diabetes.risk_ratio.MVPA.fit_7.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.diabetes.risk_ratio.MVPA.fit_8.stacked[b, "boot.diabetes.risk_ratio.MVPA.fit_8.stacked.lb"] <- quantile(boot.diabetes.risk_ratio.MVPA.fit_8.stacked[1:b, "boot.diabetes.risk_ratio.MVPA.fit_8.stacked"], 0.025)
  boot.diabetes.risk_ratio.MVPA.fit_8.stacked[b, "boot.diabetes.risk_ratio.MVPA.fit_8.stacked.ub"] <- quantile(boot.diabetes.risk_ratio.MVPA.fit_8.stacked[1:b, "boot.diabetes.risk_ratio.MVPA.fit_8.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.diabetes.risk_ratio.MVPA.fit_8.stacked, aes(x = b, y = boot.diabetes.risk_ratio.MVPA.fit_8.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.diabetes.risk_ratio.MVPA.fit_8.stacked, aes(x = b, y = boot.diabetes.risk_ratio.MVPA.fit_8.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_8",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.diabetes.risk_ratio.MVPA.fit_8.stacked$boot.diabetes.risk_ratio.MVPA.fit_8.stacked, c(0.025, 0.975))

### DHD + 1 SD versus no change ----

#### Point estimates ----

# Step 1: data expansion 

block_1 <- imp.diabetes.stacked

block_2 <- imp.diabetes.stacked
for (i in 1:n_imp) {
  for (j in 1:nrow(data.diabetes)) {
    block_2[[i]][["DHD"]][[j]] <- block_2[[i]][["DHD"]][[j]] + sd(block_2[[i]][["DHD"]])
  }
}

block_3 <- imp.diabetes.stacked

# Step 2: outcome modelling 

DHD.fit_1.stacked <- list()
for (i in 1:n_imp) {
  DHD.fit_1.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                    DHD,
                                  data = block_1[[i]],
                                  x = TRUE)
}

DHD.fit_2.stacked <- list()
for (i in 1:n_imp) {
  DHD.fit_2.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                    DHD +
                                    sex.male_1 +
                                    age,
                                  data = block_1[[i]],
                                  x = TRUE)
}

DHD.fit_3.stacked <- list()
for (i in 1:n_imp) {
  DHD.fit_3.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                    DHD +
                                    sex.male_1 +
                                    age + 
                                    SES +
                                    pre_existing_ASCVD.yes_1,
                                  data = block_1[[i]],
                                  x = TRUE)
}

DHD.fit_4.stacked <- list()
for (i in 1:n_imp) {
  DHD.fit_4.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                    rcs(DHD, 3) +
                                    sex.male_1 +
                                    rcs(age, 3) + 
                                    rcs(SES, 3) +
                                    pre_existing_ASCVD.yes_1 +
                                    DHD:sex.male_1 + 
                                    age:sex.male_1 + 
                                    SES:sex.male_1 + 
                                    pre_existing_ASCVD.yes_1:sex.male_1,
                                  data = block_1[[i]],
                                  x = TRUE)
}

DHD.fit_5.stacked <- list()
for (i in 1:n_imp) {
  DHD.fit_5.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~
                                    DHD +
                                    sex.male_1 +
                                    age +
                                    SES +
                                    pre_existing_ASCVD.yes_1 +
                                    smoking.current_1 +
                                    MVPA,
                                  data = block_1[[i]],
                                  x = TRUE)
}

DHD.fit_6.stacked <- list()
for (i in 1:n_imp) {
  DHD.fit_6.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~
                                    rcs(DHD, 3) +
                                    sex.male_1 +
                                    rcs(age, 3) + 
                                    rcs(SES, 3) +
                                    pre_existing_ASCVD.yes_1 + 
                                    smoking.current_1 +
                                    rcs(MVPA, 3) +
                                    DHD:sex.male_1 + 
                                    age:sex.male_1 + 
                                    SES:sex.male_1 + 
                                    pre_existing_ASCVD.yes_1:sex.male_1 +
                                    smoking.current_1:sex.male_1 +
                                    MVPA:sex.male_1,
                                  data = block_1[[i]],
                                  x = TRUE)
}

DHD.fit_7.stacked <- list()
for (i in 1:n_imp) {
  DHD.fit_7.stacked[[i]] <- CSC(Hist(time, event) ~ 
                                  DHD +
                                  sex.male_1 +
                                  age +
                                  SES +
                                  pre_existing_ASCVD.yes_1 +
                                  smoking.current_1 +
                                  MVPA,
                                data = block_1[[i]])
}

DHD.fit_8.stacked <- list()
for (i in 1:n_imp) {
  DHD.fit_8.stacked[[i]] <- CSC(Hist(time, event) ~ 
                                   rcs(DHD, 3) +
                                   sex.male_1 +
                                   rcs(age, 3) + 
                                   rcs(SES, 3) +
                                   pre_existing_ASCVD.yes_1 + 
                                   smoking.current_1 +
                                   rcs(MVPA, 3) +
                                   DHD:sex.male_1 + 
                                   age:sex.male_1 + 
                                   SES:sex.male_1 + 
                                   pre_existing_ASCVD.yes_1:sex.male_1 +
                                   smoking.current_1:sex.male_1 +
                                   MVPA:sex.male_1,
                                 data = block_1[[i]])
}

# Step 3: prediction 

block_2.pred_risk.DHD.fit_1.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.DHD.fit_1.stacked[[i]] <- predictRisk(DHD.fit_1.stacked[[i]], 
                                                          block_2[[i]], 
                                                          times = 5) 
}

block_2.pred_risk.DHD.fit_2.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.DHD.fit_2.stacked[[i]] <- predictRisk(DHD.fit_2.stacked[[i]], 
                                                          block_2[[i]], 
                                                          times = 5) 
}

block_2.pred_risk.DHD.fit_3.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.DHD.fit_3.stacked[[i]] <- predictRisk(DHD.fit_3.stacked[[i]], 
                                                          block_2[[i]], 
                                                          times = 5) 
}

block_2.pred_risk.DHD.fit_4.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.DHD.fit_4.stacked[[i]] <- predictRisk(DHD.fit_4.stacked[[i]], 
                                                          block_2[[i]], 
                                                          times = 5) 
}

block_2.pred_risk.DHD.fit_5.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.DHD.fit_5.stacked[[i]] <- predictRisk(DHD.fit_5.stacked[[i]], 
                                                          block_2[[i]], 
                                                          times = 5) 
}

block_2.pred_risk.DHD.fit_6.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.DHD.fit_6.stacked[[i]] <- predictRisk(DHD.fit_6.stacked[[i]], 
                                                          block_2[[i]], 
                                                          times = 5) 
}

block_2.pred_risk.DHD.fit_7.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.DHD.fit_7.stacked[[i]] <- predictRisk(DHD.fit_7.stacked[[i]], 
                                                          block_2[[i]], 
                                                          times = 5) 
}

block_2.pred_risk.DHD.fit_8.stacked <- list()
for (i in 1:n_imp) {
  block_2.pred_risk.DHD.fit_8.stacked[[i]] <- predictRisk(DHD.fit_8.stacked[[i]], 
                                                          block_2[[i]], 
                                                          times = 5) 
}

block_3.pred_risk.DHD.fit_1.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.DHD.fit_1.stacked[[i]] <- predictRisk(DHD.fit_1.stacked[[i]], 
                                                          block_3[[i]], 
                                                          times = 5) 
}

block_3.pred_risk.DHD.fit_2.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.DHD.fit_2.stacked[[i]] <- predictRisk(DHD.fit_2.stacked[[i]], 
                                                          block_3[[i]], 
                                                          times = 5) 
}

block_3.pred_risk.DHD.fit_3.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.DHD.fit_3.stacked[[i]] <- predictRisk(DHD.fit_3.stacked[[i]], 
                                                          block_3[[i]], 
                                                          times = 5) 
}

block_3.pred_risk.DHD.fit_4.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.DHD.fit_4.stacked[[i]] <- predictRisk(DHD.fit_4.stacked[[i]], 
                                                          block_3[[i]], 
                                                          times = 5) 
}

block_3.pred_risk.DHD.fit_5.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.DHD.fit_5.stacked[[i]] <- predictRisk(DHD.fit_5.stacked[[i]], 
                                                          block_3[[i]], 
                                                          times = 5) 
}

block_3.pred_risk.DHD.fit_6.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.DHD.fit_6.stacked[[i]] <- predictRisk(DHD.fit_6.stacked[[i]], 
                                                          block_3[[i]], 
                                                          times = 5) 
}

block_3.pred_risk.DHD.fit_7.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.DHD.fit_7.stacked[[i]] <- predictRisk(DHD.fit_7.stacked[[i]], 
                                                          block_3[[i]], 
                                                          times = 5) 
}

block_3.pred_risk.DHD.fit_8.stacked <- list()
for (i in 1:n_imp) {
  block_3.pred_risk.DHD.fit_8.stacked[[i]] <- predictRisk(DHD.fit_8.stacked[[i]], 
                                                          block_3[[i]], 
                                                          times = 5) 
}

# Step 4: standardization 

diabetes.risk_ratio.DHD.fit_1.stacked <- mean(unlist(block_2.pred_risk.DHD.fit_1.stacked)) / mean(unlist(block_3.pred_risk.DHD.fit_1.stacked)) 
diabetes.risk_ratio.DHD.fit_1.stacked

diabetes.risk_ratio.DHD.fit_2.stacked <- mean(unlist(block_2.pred_risk.DHD.fit_2.stacked)) / mean(unlist(block_3.pred_risk.DHD.fit_2.stacked)) 
diabetes.risk_ratio.DHD.fit_2.stacked

diabetes.risk_ratio.DHD.fit_3.stacked <- mean(unlist(block_2.pred_risk.DHD.fit_3.stacked)) / mean(unlist(block_3.pred_risk.DHD.fit_3.stacked)) 
diabetes.risk_ratio.DHD.fit_3.stacked

diabetes.risk_ratio.DHD.fit_4.stacked <- mean(unlist(block_2.pred_risk.DHD.fit_4.stacked)) / mean(unlist(block_3.pred_risk.DHD.fit_4.stacked)) 
diabetes.risk_ratio.DHD.fit_4.stacked

diabetes.risk_ratio.DHD.fit_5.stacked <- mean(unlist(block_2.pred_risk.DHD.fit_5.stacked)) / mean(unlist(block_3.pred_risk.DHD.fit_5.stacked)) 
diabetes.risk_ratio.DHD.fit_5.stacked

diabetes.risk_ratio.DHD.fit_6.stacked <- mean(unlist(block_2.pred_risk.DHD.fit_6.stacked)) / mean(unlist(block_3.pred_risk.DHD.fit_6.stacked)) 
diabetes.risk_ratio.DHD.fit_6.stacked

diabetes.risk_ratio.DHD.fit_7.stacked <- mean(unlist(block_2.pred_risk.DHD.fit_7.stacked)) / mean(unlist(block_3.pred_risk.DHD.fit_7.stacked)) 
diabetes.risk_ratio.DHD.fit_7.stacked

diabetes.risk_ratio.DHD.fit_8.stacked <- mean(unlist(block_2.pred_risk.DHD.fit_8.stacked)) / mean(unlist(block_3.pred_risk.DHD.fit_8.stacked)) 
diabetes.risk_ratio.DHD.fit_8.stacked

#### Bootstrapped confidence intervals ----

# Seed 

set.seed(...)

# Number of bootstraps

n_b <- 150

# Creating objects to capture output

boot.diabetes.risk_ratio.DHD.fit_1.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                   ncol = 2))
colnames(boot.diabetes.risk_ratio.DHD.fit_1.stacked) <- c("b",
                                                          "boot.diabetes.risk_ratio.DHD.fit_1.stacked")
boot.diabetes.risk_ratio.DHD.fit_1.stacked$b <- 1:n_b

boot.diabetes.risk_ratio.DHD.fit_2.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                   ncol = 2))
colnames(boot.diabetes.risk_ratio.DHD.fit_2.stacked) <- c("b",
                                                          "boot.diabetes.risk_ratio.DHD.fit_2.stacked")
boot.diabetes.risk_ratio.DHD.fit_2.stacked$b <- 1:n_b

boot.diabetes.risk_ratio.DHD.fit_3.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                   ncol = 2))
colnames(boot.diabetes.risk_ratio.DHD.fit_3.stacked) <- c("b",
                                                          "boot.diabetes.risk_ratio.DHD.fit_3.stacked")
boot.diabetes.risk_ratio.DHD.fit_3.stacked$b <- 1:n_b

boot.diabetes.risk_ratio.DHD.fit_4.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                   ncol = 2))
colnames(boot.diabetes.risk_ratio.DHD.fit_4.stacked) <- c("b",
                                                          "boot.diabetes.risk_ratio.DHD.fit_4.stacked")
boot.diabetes.risk_ratio.DHD.fit_4.stacked$b <- 1:n_b

boot.diabetes.risk_ratio.DHD.fit_5.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                   ncol = 2))
colnames(boot.diabetes.risk_ratio.DHD.fit_5.stacked) <- c("b",
                                                          "boot.diabetes.risk_ratio.DHD.fit_5.stacked")
boot.diabetes.risk_ratio.DHD.fit_5.stacked$b <- 1:n_b

boot.diabetes.risk_ratio.DHD.fit_6.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                   ncol = 2))
colnames(boot.diabetes.risk_ratio.DHD.fit_6.stacked) <- c("b",
                                                          "boot.diabetes.risk_ratio.DHD.fit_6.stacked")
boot.diabetes.risk_ratio.DHD.fit_6.stacked$b <- 1:n_b

boot.diabetes.risk_ratio.DHD.fit_7.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                   ncol = 2))
colnames(boot.diabetes.risk_ratio.DHD.fit_7.stacked) <- c("b",
                                                          "boot.diabetes.risk_ratio.DHD.fit_7.stacked")
boot.diabetes.risk_ratio.DHD.fit_7.stacked$b <- 1:n_b

boot.diabetes.risk_ratio.DHD.fit_8.stacked <- as.data.frame(matrix(nrow = n_b,
                                                                   ncol = 2))
colnames(boot.diabetes.risk_ratio.DHD.fit_8.stacked) <- c("b",
                                                          "boot.diabetes.risk_ratio.DHD.fit_8.stacked")
boot.diabetes.risk_ratio.DHD.fit_8.stacked$b <- 1:n_b

# Loop

for (b in 1:n_b) {
  
  # Bootstrapped data
  
  boot.index <- sample(1:nrow(data.diabetes), size = nrow(data.diabetes), replace = TRUE)
  
  boot.data.diabetes <- data.diabetes[boot.index, ]
  
  boot.imp.diabetes <- boot.data.diabetes[, c( "event",
                                               "time",
                                               "sex.male_1",
                                               "age",
                                               "pre_existing_ASCVD.yes_1",
                                               "SES",
                                               "smoking.current_1",
                                               "DHD",
                                               "MVPA")]
  boot.imp.diabetes <- sapply(boot.imp.diabetes, zap_labels)
  boot.imp.diabetes <- mice(boot.imp.diabetes, method = "pmm", m = n_imp, print = FALSE)
  boot.imp.diabetes.stacked <- list()
  for (i in 1:n_imp) {
    boot.imp.diabetes.stacked[[i]] <- complete(boot.imp.diabetes, i)
    boot.imp.diabetes.stacked[[i]][["ASCVD_composite_event"]] <- ifelse(boot.imp.diabetes.stacked[[i]][["event"]] == 1,
                                                                        1,
                                                                        0) 
    boot.imp.diabetes.stacked[[i]][["overall_mortality_excl_ASCVD_mortality_event"]] <- ifelse(boot.imp.diabetes.stacked[[i]][["event"]] == 2,
                                                                                               1,
                                                                                               0) 
  }
  
  # Step 1: data expansion
  
  boot.block_1 <- boot.imp.diabetes.stacked
  
  boot.block_2 <- boot.imp.diabetes.stacked
  for (i in 1:n_imp) {
    for (j in 1:nrow(data.diabetes)) {
      boot.block_2[[i]][["DHD"]][[j]] <- boot.block_2[[i]][["DHD"]][[j]] + sd(boot.block_2[[i]][["DHD"]])
    }
  }
  
  boot.block_3 <- boot.imp.diabetes.stacked
  
  # Step 2: outcome modelling
  
  boot.DHD.fit_1.stacked <- list()
  for (i in 1:n_imp) {
    boot.DHD.fit_1.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                           DHD,
                                         data = boot.block_1[[i]],
                                         x = TRUE)
  }
  
  boot.DHD.fit_2.stacked <- list()
  for (i in 1:n_imp) {
    boot.DHD.fit_2.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                           DHD +
                                           sex.male_1 +
                                           age,
                                         data = boot.block_1[[i]],
                                         x = TRUE)
  }
  
  boot.DHD.fit_3.stacked <- list()
  for (i in 1:n_imp) {
    boot.DHD.fit_3.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                           DHD +
                                           sex.male_1 +
                                           age + 
                                           SES +
                                           pre_existing_ASCVD.yes_1,
                                         data = boot.block_1[[i]],
                                         x = TRUE)
  }
  
  boot.DHD.fit_4.stacked <- list()
  for (i in 1:n_imp) {
    boot.DHD.fit_4.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~ 
                                           rcs(DHD, 3) +
                                           sex.male_1 +
                                           rcs(age, 3) + 
                                           rcs(SES, 3) +
                                           pre_existing_ASCVD.yes_1 + 
                                           DHD:sex.male_1 + 
                                           age:sex.male_1 + 
                                           SES:sex.male_1 + 
                                           pre_existing_ASCVD.yes_1:sex.male_1,
                                         data = boot.block_1[[i]],
                                         x = TRUE)
  }
  
  boot.DHD.fit_5.stacked <- list()
  for (i in 1:n_imp) {
    boot.DHD.fit_5.stacked[[i]] <- coxph(Surv(time, ASCVD_composite_event) ~
                                           DHD +
                                           sex.male_1 +
                                           age +
                                           SES +
                                           pre_existing_ASCVD.yes_1 +
                                           smoking.current_1 +
                                           MVPA,
                                         data = boot.block_1[[i]],
                                         x = TRUE)
  }
  
  boot.DHD.fit_6.stacked <- list()
  for (i in 1:n_imp) {
    boot.DHD.fit_6.stacked[[i]] <-  coxph(Surv(time, ASCVD_composite_event) ~
                                            rcs(DHD, 3) +
                                            sex.male_1 +
                                            rcs(age, 3) + 
                                            rcs(SES, 3) +
                                            pre_existing_ASCVD.yes_1 + 
                                            smoking.current_1 +
                                            rcs(MVPA, 3) +
                                            DHD:sex.male_1 + 
                                            age:sex.male_1 + 
                                            SES:sex.male_1 + 
                                            pre_existing_ASCVD.yes_1:sex.male_1 +
                                            smoking.current_1:sex.male_1 +
                                            MVPA:sex.male_1,
                                          data = boot.block_1[[i]],
                                          x = TRUE)
  }
  
  boot.DHD.fit_7.stacked <- list()
  for (i in 1:n_imp) {
    boot.DHD.fit_7.stacked[[i]] <- CSC(Hist(time, event) ~ 
                                         DHD +
                                         sex.male_1 +
                                         age +
                                         SES +
                                         pre_existing_ASCVD.yes_1 +
                                         smoking.current_1 +
                                         MVPA,
                                       data = boot.block_1[[i]])
  }
  
  boot.DHD.fit_8.stacked <- list()
  for (i in 1:n_imp) {
    boot.DHD.fit_8.stacked[[i]] <- CSC(Hist(time, event) ~ 
                                          rcs(DHD, 3) +
                                          sex.male_1 +
                                          rcs(age, 3) + 
                                          rcs(SES, 3) +
                                          pre_existing_ASCVD.yes_1 + 
                                          smoking.current_1 +
                                          rcs(MVPA, 3) +
                                          DHD:sex.male_1 + 
                                          age:sex.male_1 + 
                                          SES:sex.male_1 + 
                                          pre_existing_ASCVD.yes_1:sex.male_1 +
                                          smoking.current_1:sex.male_1 +
                                          MVPA:sex.male_1,
                                        data = boot.block_1[[i]])
  }
  
  # Step 3: prediction
  
  boot.block_2.pred_risk.DHD.fit_1.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.DHD.fit_1.stacked[[i]] <- predictRisk(boot.DHD.fit_1.stacked[[i]], 
                                                                 boot.block_2[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_2.pred_risk.DHD.fit_2.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.DHD.fit_2.stacked[[i]] <- predictRisk(boot.DHD.fit_2.stacked[[i]], 
                                                                 boot.block_2[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_2.pred_risk.DHD.fit_3.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.DHD.fit_3.stacked[[i]] <- predictRisk(boot.DHD.fit_3.stacked[[i]], 
                                                                 boot.block_2[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_2.pred_risk.DHD.fit_4.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.DHD.fit_4.stacked[[i]] <- predictRisk(boot.DHD.fit_4.stacked[[i]], 
                                                                 boot.block_2[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_2.pred_risk.DHD.fit_5.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.DHD.fit_5.stacked[[i]] <- predictRisk(boot.DHD.fit_5.stacked[[i]], 
                                                                 boot.block_2[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_2.pred_risk.DHD.fit_6.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.DHD.fit_6.stacked[[i]] <- predictRisk(boot.DHD.fit_6.stacked[[i]], 
                                                                 boot.block_2[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_2.pred_risk.DHD.fit_7.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.DHD.fit_7.stacked[[i]] <- predictRisk(boot.DHD.fit_7.stacked[[i]], 
                                                                 boot.block_2[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_2.pred_risk.DHD.fit_8.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_2.pred_risk.DHD.fit_8.stacked[[i]] <- predictRisk(boot.DHD.fit_8.stacked[[i]], 
                                                                 boot.block_2[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_3.pred_risk.DHD.fit_1.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.DHD.fit_1.stacked[[i]] <- predictRisk(boot.DHD.fit_1.stacked[[i]], 
                                                                 boot.block_3[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_3.pred_risk.DHD.fit_2.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.DHD.fit_2.stacked[[i]] <- predictRisk(boot.DHD.fit_2.stacked[[i]], 
                                                                 boot.block_3[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_3.pred_risk.DHD.fit_3.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.DHD.fit_3.stacked[[i]] <- predictRisk(boot.DHD.fit_3.stacked[[i]], 
                                                                 boot.block_3[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_3.pred_risk.DHD.fit_4.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.DHD.fit_4.stacked[[i]] <- predictRisk(boot.DHD.fit_4.stacked[[i]], 
                                                                 boot.block_3[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_3.pred_risk.DHD.fit_5.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.DHD.fit_5.stacked[[i]] <- predictRisk(boot.DHD.fit_5.stacked[[i]], 
                                                                 boot.block_3[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_3.pred_risk.DHD.fit_6.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.DHD.fit_6.stacked[[i]] <- predictRisk(boot.DHD.fit_6.stacked[[i]], 
                                                                 boot.block_3[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_3.pred_risk.DHD.fit_7.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.DHD.fit_7.stacked[[i]] <- predictRisk(boot.DHD.fit_7.stacked[[i]], 
                                                                 boot.block_3[[i]], 
                                                                 times = 5) 
  }
  
  boot.block_3.pred_risk.DHD.fit_8.stacked <- list()
  for (i in 1:n_imp) {
    boot.block_3.pred_risk.DHD.fit_8.stacked[[i]] <- predictRisk(boot.DHD.fit_8.stacked[[i]], 
                                                                 boot.block_3[[i]], 
                                                                 times = 5) 
  }
  
  # Step 4: standardization 
  
  boot.diabetes.risk_ratio.DHD.fit_1.stacked[b, "boot.diabetes.risk_ratio.DHD.fit_1.stacked"] <- mean(unlist(boot.block_2.pred_risk.DHD.fit_1.stacked)) / mean(unlist(boot.block_3.pred_risk.DHD.fit_1.stacked)) 
  
  boot.diabetes.risk_ratio.DHD.fit_2.stacked[b, "boot.diabetes.risk_ratio.DHD.fit_2.stacked"] <- mean(unlist(boot.block_2.pred_risk.DHD.fit_2.stacked)) / mean(unlist(boot.block_3.pred_risk.DHD.fit_2.stacked)) 
  
  boot.diabetes.risk_ratio.DHD.fit_3.stacked[b, "boot.diabetes.risk_ratio.DHD.fit_3.stacked"] <- mean(unlist(boot.block_2.pred_risk.DHD.fit_3.stacked)) / mean(unlist(boot.block_3.pred_risk.DHD.fit_3.stacked)) 
  
  boot.diabetes.risk_ratio.DHD.fit_4.stacked[b, "boot.diabetes.risk_ratio.DHD.fit_4.stacked"] <- mean(unlist(boot.block_2.pred_risk.DHD.fit_4.stacked)) / mean(unlist(boot.block_3.pred_risk.DHD.fit_4.stacked)) 
  
  boot.diabetes.risk_ratio.DHD.fit_5.stacked[b, "boot.diabetes.risk_ratio.DHD.fit_5.stacked"] <- mean(unlist(boot.block_2.pred_risk.DHD.fit_5.stacked)) / mean(unlist(boot.block_3.pred_risk.DHD.fit_5.stacked)) 
  
  boot.diabetes.risk_ratio.DHD.fit_6.stacked[b, "boot.diabetes.risk_ratio.DHD.fit_6.stacked"] <- mean(unlist(boot.block_2.pred_risk.DHD.fit_6.stacked)) / mean(unlist(boot.block_3.pred_risk.DHD.fit_6.stacked)) 
  
  boot.diabetes.risk_ratio.DHD.fit_7.stacked[b, "boot.diabetes.risk_ratio.DHD.fit_7.stacked"] <- mean(unlist(boot.block_2.pred_risk.DHD.fit_7.stacked)) / mean(unlist(boot.block_3.pred_risk.DHD.fit_7.stacked)) 
  
  boot.diabetes.risk_ratio.DHD.fit_8.stacked[b, "boot.diabetes.risk_ratio.DHD.fit_8.stacked"] <- mean(unlist(boot.block_2.pred_risk.DHD.fit_8.stacked)) / mean(unlist(boot.block_3.pred_risk.DHD.fit_8.stacked)) 
}

# Output

for (b in 10:n_b) {
  boot.diabetes.risk_ratio.DHD.fit_1.stacked[b, "boot.diabetes.risk_ratio.DHD.fit_1.stacked.lb"] <- quantile(boot.diabetes.risk_ratio.DHD.fit_1.stacked[1:b, "boot.diabetes.risk_ratio.DHD.fit_1.stacked"], 0.025)
  boot.diabetes.risk_ratio.DHD.fit_1.stacked[b, "boot.diabetes.risk_ratio.DHD.fit_1.stacked.ub"] <- quantile(boot.diabetes.risk_ratio.DHD.fit_1.stacked[1:b, "boot.diabetes.risk_ratio.DHD.fit_1.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.diabetes.risk_ratio.DHD.fit_1.stacked, aes(x = b, y = boot.diabetes.risk_ratio.DHD.fit_1.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.diabetes.risk_ratio.DHD.fit_1.stacked, aes(x = b, y = boot.diabetes.risk_ratio.DHD.fit_1.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_1",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.diabetes.risk_ratio.DHD.fit_1.stacked$boot.diabetes.risk_ratio.DHD.fit_1.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.diabetes.risk_ratio.DHD.fit_2.stacked[b, "boot.diabetes.risk_ratio.DHD.fit_2.stacked.lb"] <- quantile(boot.diabetes.risk_ratio.DHD.fit_2.stacked[1:b, "boot.diabetes.risk_ratio.DHD.fit_2.stacked"], 0.025)
  boot.diabetes.risk_ratio.DHD.fit_2.stacked[b, "boot.diabetes.risk_ratio.DHD.fit_2.stacked.ub"] <- quantile(boot.diabetes.risk_ratio.DHD.fit_2.stacked[1:b, "boot.diabetes.risk_ratio.DHD.fit_2.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.diabetes.risk_ratio.DHD.fit_2.stacked, aes(x = b, y = boot.diabetes.risk_ratio.DHD.fit_2.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.diabetes.risk_ratio.DHD.fit_2.stacked, aes(x = b, y = boot.diabetes.risk_ratio.DHD.fit_2.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_2",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.diabetes.risk_ratio.DHD.fit_2.stacked$boot.diabetes.risk_ratio.DHD.fit_2.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.diabetes.risk_ratio.DHD.fit_3.stacked[b, "boot.diabetes.risk_ratio.DHD.fit_3.stacked.lb"] <- quantile(boot.diabetes.risk_ratio.DHD.fit_3.stacked[1:b, "boot.diabetes.risk_ratio.DHD.fit_3.stacked"], 0.025)
  boot.diabetes.risk_ratio.DHD.fit_3.stacked[b, "boot.diabetes.risk_ratio.DHD.fit_3.stacked.ub"] <- quantile(boot.diabetes.risk_ratio.DHD.fit_3.stacked[1:b, "boot.diabetes.risk_ratio.DHD.fit_3.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.diabetes.risk_ratio.DHD.fit_3.stacked, aes(x = b, y = boot.diabetes.risk_ratio.DHD.fit_3.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.diabetes.risk_ratio.DHD.fit_3.stacked, aes(x = b, y = boot.diabetes.risk_ratio.DHD.fit_3.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_3",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.diabetes.risk_ratio.DHD.fit_3.stacked$boot.diabetes.risk_ratio.DHD.fit_3.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.diabetes.risk_ratio.DHD.fit_4.stacked[b, "boot.diabetes.risk_ratio.DHD.fit_4.stacked.lb"] <- quantile(boot.diabetes.risk_ratio.DHD.fit_4.stacked[1:b, "boot.diabetes.risk_ratio.DHD.fit_4.stacked"], 0.025)
  boot.diabetes.risk_ratio.DHD.fit_4.stacked[b, "boot.diabetes.risk_ratio.DHD.fit_4.stacked.ub"] <- quantile(boot.diabetes.risk_ratio.DHD.fit_4.stacked[1:b, "boot.diabetes.risk_ratio.DHD.fit_4.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.diabetes.risk_ratio.DHD.fit_4.stacked, aes(x = b, y = boot.diabetes.risk_ratio.DHD.fit_4.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.diabetes.risk_ratio.DHD.fit_4.stacked, aes(x = b, y = boot.diabetes.risk_ratio.DHD.fit_4.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_4",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.diabetes.risk_ratio.DHD.fit_4.stacked$boot.diabetes.risk_ratio.DHD.fit_4.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.diabetes.risk_ratio.DHD.fit_5.stacked[b, "boot.diabetes.risk_ratio.DHD.fit_5.stacked.lb"] <- quantile(boot.diabetes.risk_ratio.DHD.fit_5.stacked[1:b, "boot.diabetes.risk_ratio.DHD.fit_5.stacked"], 0.025)
  boot.diabetes.risk_ratio.DHD.fit_5.stacked[b, "boot.diabetes.risk_ratio.DHD.fit_5.stacked.ub"] <- quantile(boot.diabetes.risk_ratio.DHD.fit_5.stacked[1:b, "boot.diabetes.risk_ratio.DHD.fit_5.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.diabetes.risk_ratio.DHD.fit_5.stacked, aes(x = b, y = boot.diabetes.risk_ratio.DHD.fit_5.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.diabetes.risk_ratio.DHD.fit_5.stacked, aes(x = b, y = boot.diabetes.risk_ratio.DHD.fit_5.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_5",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.diabetes.risk_ratio.DHD.fit_5.stacked$boot.diabetes.risk_ratio.DHD.fit_5.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.diabetes.risk_ratio.DHD.fit_6.stacked[b, "boot.diabetes.risk_ratio.DHD.fit_6.stacked.lb"] <- quantile(boot.diabetes.risk_ratio.DHD.fit_6.stacked[1:b, "boot.diabetes.risk_ratio.DHD.fit_6.stacked"], 0.025)
  boot.diabetes.risk_ratio.DHD.fit_6.stacked[b, "boot.diabetes.risk_ratio.DHD.fit_6.stacked.ub"] <- quantile(boot.diabetes.risk_ratio.DHD.fit_6.stacked[1:b, "boot.diabetes.risk_ratio.DHD.fit_6.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.diabetes.risk_ratio.DHD.fit_6.stacked, aes(x = b, y = boot.diabetes.risk_ratio.DHD.fit_6.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.diabetes.risk_ratio.DHD.fit_6.stacked, aes(x = b, y = boot.diabetes.risk_ratio.DHD.fit_6.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_6",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.diabetes.risk_ratio.DHD.fit_6.stacked$boot.diabetes.risk_ratio.DHD.fit_6.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.diabetes.risk_ratio.DHD.fit_7.stacked[b, "boot.diabetes.risk_ratio.DHD.fit_7.stacked.lb"] <- quantile(boot.diabetes.risk_ratio.DHD.fit_7.stacked[1:b, "boot.diabetes.risk_ratio.DHD.fit_7.stacked"], 0.025)
  boot.diabetes.risk_ratio.DHD.fit_7.stacked[b, "boot.diabetes.risk_ratio.DHD.fit_7.stacked.ub"] <- quantile(boot.diabetes.risk_ratio.DHD.fit_7.stacked[1:b, "boot.diabetes.risk_ratio.DHD.fit_7.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.diabetes.risk_ratio.DHD.fit_7.stacked, aes(x = b, y = boot.diabetes.risk_ratio.DHD.fit_7.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.diabetes.risk_ratio.DHD.fit_7.stacked, aes(x = b, y = boot.diabetes.risk_ratio.DHD.fit_7.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_7",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.diabetes.risk_ratio.DHD.fit_7.stacked$boot.diabetes.risk_ratio.DHD.fit_7.stacked, c(0.025, 0.975))

for (b in 10:n_b) {
  boot.diabetes.risk_ratio.DHD.fit_8.stacked[b, "boot.diabetes.risk_ratio.DHD.fit_8.stacked.lb"] <- quantile(boot.diabetes.risk_ratio.DHD.fit_8.stacked[1:b, "boot.diabetes.risk_ratio.DHD.fit_8.stacked"], 0.025)
  boot.diabetes.risk_ratio.DHD.fit_8.stacked[b, "boot.diabetes.risk_ratio.DHD.fit_8.stacked.ub"] <- quantile(boot.diabetes.risk_ratio.DHD.fit_8.stacked[1:b, "boot.diabetes.risk_ratio.DHD.fit_8.stacked"], 0.975)
}
ggplot() +
  geom_point(data = boot.diabetes.risk_ratio.DHD.fit_8.stacked, aes(x = b, y = boot.diabetes.risk_ratio.DHD.fit_8.stacked.lb), colour = "skyblue") +
  geom_point(data = boot.diabetes.risk_ratio.DHD.fit_8.stacked, aes(x = b, y = boot.diabetes.risk_ratio.DHD.fit_8.stacked.ub), colour = "skyblue4") +
  scale_x_discrete(name = "last bootstrap",
                   breaks = seq(0, 200, 50),
                   labels = seq(0, 200, 50)) +
  scale_y_continuous(name = "fit_8",
                     breaks = seq(0.3, 1.3, 0.1),
                     labels = seq(0.3, 1.3, 0.1)) +
  theme_minimal()
quantile(boot.diabetes.risk_ratio.DHD.fit_8.stacked$boot.diabetes.risk_ratio.DHD.fit_8.stacked, c(0.025, 0.975))

# Session info ----

sessionInfo()
