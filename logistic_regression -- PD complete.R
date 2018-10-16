## Regression with binary outcomes
## ═════════════════════════════════

## Logistic regression
## ───────────────────────

##   This far we have used the `lm' function to fit our regression models.
##   `lm' is great, but limited–in particular it only fits models for
##   continuous dependent variables. For categorical dependent variables we
##   can use the `glm()' function.

##   For these models we will use a different dataset, drawn from the
##   National Health Interview Survey. From the [CDC website]:

##         The National Health Interview Survey (NHIS) has monitored
##         the health of the nation since 1957. NHIS data on a broad
##         range of health topics are collected through personal
##         household interviews. For over 50 years, the U.S. Census
##         Bureau has been the data collection agent for the National
##         Health Interview Survey. Survey results have been
##         instrumental in providing data to track health status,
##         health care access, and progress toward achieving national
##         health objectives.

##   Load the National Health Interview Survey data:

NH11 <- readRDS("dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels

##   [CDC website] http://www.cdc.gov/nchs/nhis.htm

## Logistic regression example
## ───────────────────────────────

##   Let's predict the probability of being diagnosed with hypertension
##   based on age, sex, sleep, and bmi

str(NH11$hypev) # check stucture of hypev
levels(NH11$hypev) # check levels of hypev
# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
# run our regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
              data=NH11, family="binomial")
coef(summary(hyp.out))

## Logistic regression coefficients
## ────────────────────────────────────

##   Generalized linear models use link functions, so raw coefficients are
##   difficult to interpret. For example, the age coefficient of .06 in the
##   previous model tells us that for every one unit increase in age, the
##   log odds of hypertension diagnosis increases by 0.06. Since most of us
##   are not used to thinking in log odds this is not too helpful!

##   One solution is to transform the coefficients to make them easier to
##   interpret

hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab

## Generating predicted values
## ───────────────────────────────

##   In addition to transforming the log-odds produced by `glm' to odds, we
##   can use the `predict()' function to make direct statements about the
##   predictors in our model. For example, we can ask "How much more likely
##   is a 63 year old female to have hypertension compared to a 33 year old
##   female?".

# Create a dataset with predictors set at desired levels
predDat <- with(NH11,
                expand.grid(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
# predict hypertension at those levels
cbind(predDat, predict(hyp.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))

##   This tells us that a 33 year old female has a 13% probability of
##   having been diagnosed with hypertension, while and 63 year old female
##   has a 48% probability of having been diagnosed.

## Packages for  computing and graphing predicted values
## ─────────────────────────────────────────────────────────

##   Instead of doing all this ourselves, we can use the effects package to
##   compute quantities of interest for us (cf. the Zelig package).

library(effects)
plot(allEffects(hyp.out))

## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
##   2. Predict the probability of working for each level of marital
##      status.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.



#################################### PD ####################################
#################################### #1 ####################################
############################################################################
############################################################################



#### re-reading the NH11 dataset, from line 27 above

NH11 <- readRDS("dataSets/NatHealth2011.rds")

#### let's first see if there are any NA's

sum(as.numeric(is.na(NH11$r_maritl)))    ######## 0
sum(as.numeric(is.na(NH11$age_p)))    ######## 0
sum(as.numeric(is.na(NH11$everwrk)))    ######## 18949

#### remove the observations where everwrk is NA

NH11$everwrk_NA <- ifelse(is.na(NH11$everwrk), 1, 0)
NH11 <- subset(NH11, everwrk_NA == 0)

#### also, only hold onto variables that are needed

NH11 <- data.frame(NH11$everwrk, NH11$age_p, NH11$r_maritl)
colnames(NH11) <- c("everwrk", "age_p", "r_maritl")

#### check the structure of everwrk

str(NH11$everwrk)

#### adjust everwrk to binary: "1 Yes" = 1, else = 0

NH11$everwrk <- ifelse(NH11$everwrk == "1 Yes", 1, 0)

#### now call glm to predict everwrk via age_p and r_maritl

model <- glm(everwrk ~ age_p + r_maritl, data = NH11, family = "binomial")
coef(summary(model)) ######## all but "married," "separated," and "unknown"

#### exponentiate model estimates

model_coeffs <- coef(summary(model))
model_coeffs[, "Estimate"] <- exp(coef(model))



#################################### PD ####################################
#################################### #2 ####################################
############################################################################
############################################################################



#### create dataset with age set to average age and marital status set at
#### each level

predDat <- with(NH11,
                expand.grid(age_p = mean(NH11$age_p, na.rm = TRUE),
                            r_maritl = c(levels(NH11$r_maritl))))

#### check to make sure that all marital levels are in the dataset
subset(data.frame(table(NH11$r_maritl)), Freq == 0) # "0" and "3" are not

#### remove "0" and "3" levels from the new data
predDat <- subset(predDat, r_maritl != c("0 Under 14 years",
                             "3 Married - spouse in household unknown"))

#### now predict

results <- cbind(predDat, predict(model, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))

min(results$fit)
max(results$fit)

#### at an average age of almost 56, for each level of marital status,
#### the probability that the subject has ever worked ranges between
#### 80% and 95%