#################################################################################################
# BCCDC BIOSTATS 2019 - Modeling building 1
#
# Purpose: To illustrate concepts and methods related to modeling building, using an example
#           STI dataset provided by CPS.  Concepts are covered in the corresponding slides on 
#           model building (BCCDC biostats - model building 1 and 2)
#           The following code was written using Microsoft R Open 3.4.1 and 
#           is meant to be run on the BCCDC Analytics Platform.
#
# Authors: Michael Otterstatter
#
# Created: July 4, 2019
#
# Last modified: Aug 16, 2019 
#                 
#               
#################################################################################################


# The following illustrates components of regression model building using an example dataset
#   from Clinical Prevention Services (CPS) at BCCDC.  This material is not meant to be comprehensive,
#   but rather to highlight important considerations in model building and provide example code.

# Prior to modeling building, there are necessarily steps for importing the data, cleaning the data,
#   and general data exploration.  These steps can be found below, followed by the core section on
#   model building.



#### SETUP ####

# set folder paths and filenames for import
indir <- "//phsabc/root/BCCDC/Groups/Data_Projects/STI_HIV_Projects/SURVEILLANCE/STI Sentinel Surveillance/NOT_SENTINAL/2019_05_Testingmodels"
indat <- "ctgcisv3.csv"

# set working directory and define checkpoint to ensure reproducibility (date is matched to analytics platform version)
setwd("//phsabc/root/BCCDC/Groups/Analytics_Resources/Training/Biostats/Sessions/Aug 16 2019 - model building 3") 
library(checkpoint) 
checkpoint("2017-09-01") 

# load tidy data and model building tools
library(tidyverse)
library(broom)
library(modelr)


options(scipen = 100) # forces R to show 'regular' numeric output, not scientific notation 




#### DATA DESCRIPTION ####

# As an example of model building, we analyze the following data:

# Individual-level clinic data from STI sentinel surveillance (provided by Clinical Prevention Services, BCCDC)
# Chlamydia and gonorrhea diagnoses (BC, 2006-17) linked at patient level to infectious syphilis diagnoses (up to 12-months after)
# Patient-level information is based on case report forms and linkage to HIV surveillance data

# Variables available for modeling building:
#   syph_dx - Did the patient have (1) or not have (0) a syphilis diagnosis during the study period?
#   earliest_age_grp - patient age groups (15-19, 20-24, 25-29, 30-39, 40-59, 60+ years)
#   hiv_atoc - Did the patient have HIV at the time of their syphilis diagnosis?
#   everlgv - 
#   gender_bin - Sex categories (M, F, NA)
#   surveillance_region_ha - Patient's Health Authority of residence
#   ctgc_cat - Number of chlamyida or gonorrhea diagnoses patient had during study period (1 or 2, 3 or 4, 5+)
#   post2011 - Was the earliest syphilis diagnosis before (0) or after (1) 2011?
#   any_risk - Risk category assigned to patient (MSM, Street/STW, Heterosexual, Outside, Other/Unknow, Missing)




#### IMPORT DATA ####

# NOTE: Given that these are real individual-level surveillance data, access is only available to those with permission 
#   to the CPS STI sentinel surveillance folders on the BCCDC network. Nevertheless, the script includes frequent comments
#   and pointers that help clarify the general approach to model building even if that data cannot be directly used.

# import data from csv file
raw_data <- read_csv(paste(indir, indat, sep = "/"))

# quick summary of dataset and column formats
glimpse(raw_data)

# or view as spreadsheet
View(raw_data)





#### CLEAN DATA ####

# set variable formats to match previous analyses in SAS
clean_data <- raw_data %>%
  mutate(everlgv = recode(everlgv, "0" = "No", "1" = "Yes"),
         hiv_atoc = recode(hiv_atoc, "0" = "No", "1" = "Yes"),
         any_risk = recode(any_risk, "1" = "MSM", "2" = "Street/STW", "3" = "Heterosexual", 
                           "4" = "Outside", "5" = "Other/Unknown", .missing = "missing"),
         dxwhen_bin = recode(dxwhen_bin, "0" = "1 or 2", "1" = ">2"),
         post2011 = recode(post2011, "0" = "<=2011", "1" = ">2011"))




#### EXPLORE DATA ####

# number of data rows and number of unique patients
clean_data %>% count()
clean_data %>% summarise(unique.patients = n_distinct(patient_master_key))

# numbers of cases and non-cases
clean_data %>% group_by(syph_dx) %>% summarise(unique.patients = n_distinct(patient_master_key))


# univariate frequency tables (case counts) by key covariates
clean_data %>% count(syph_dx) %>% mutate(proportion = prop.table(n)) 
clean_data %>% count(gender) %>% mutate(proportion = prop.table(n))
clean_data %>% count(earliest_age_grp) %>% mutate(proportion = prop.table(n))
clean_data %>% count(everlgv) %>% mutate(proportion = prop.table(n))
clean_data %>% count(any_risk) %>% mutate(proportion = prop.table(n))
clean_data %>% count(hiv_atoc) %>% mutate(proportion = prop.table(n))
clean_data %>% count(ctgc_cat) %>% mutate(proportion = prop.table(n))
clean_data %>% count(post2011) %>% mutate(proportion = prop.table(n))


# similarly, could generate bar charts for frequencies or percentages
clean_data %>% count(gender) %>% mutate(proportion = prop.table(n)) %>%
  ggplot(aes(x = gender, y = proportion)) + geom_bar(stat = "identity")

clean_data %>% count(earliest_age_grp) %>% mutate(proportion = prop.table(n)) %>%
  ggplot(aes(x = earliest_age_grp, y = proportion)) + geom_bar(stat = "identity")

clean_data %>% count(any_risk) %>% mutate(proportion = prop.table(n)) %>%
  ggplot(aes(x = any_risk, y = proportion)) + geom_bar(stat = "identity")


# bivariate frequency tables (case counts) by key covariates
clean_data %>% count(syph_dx, any_risk) %>% group_by(syph_dx) %>% mutate(proportion = prop.table(n))
clean_data %>% count(syph_dx, any_risk, gender) %>% group_by(syph_dx) %>% mutate(proportion = prop.table(n))

# similarly, could generate bar charts for frequencies or percentages
clean_data %>% count(syph_dx, any_risk, gender) %>% group_by(syph_dx) %>% mutate(proportion = prop.table(n)) %>%
  ggplot(aes(x = any_risk, y = proportion, fill = gender)) + geom_bar(stat = "identity") + facet_wrap(~ syph_dx)


# summary statistics for numeric covariates
clean_data %>% group_by(syph_dx) %>% summarise_at("earliest_age_years_num", 
                            funs(mean=mean, sd=sd, min=min, median=median, max=max, n=sum(!is.na(.))), na.rm = TRUE)



# run chi-square tests of independence on selected covariates
var_list <- c("hiv_atoc", "everlgv", "surveillance_region_ha", "earliest_age_grp", "gender", "ctgc_cat", "post2011")

# syphilis dx
clean_data %>%
  select(var_list) %>%
  summarise_all(funs(chisq.test(., clean_data$syph_dx, simulate.p.value = TRUE)$p.value))

# hiv at time of case
clean_data %>%
  select(var_list) %>%
  summarise_all(funs(chisq.test(., clean_data$hiv_atoc, simulate.p.value = TRUE)$p.value))

# everlgv
clean_data %>%
  select(var_list) %>%
  summarise_all(funs(chisq.test(., clean_data$everlgv, simulate.p.value = TRUE)$p.value))

# health authority
clean_data %>%
  select(var_list) %>%
  summarise_all(funs(chisq.test(., clean_data$surveillance_region_ha, simulate.p.value = TRUE)$p.value))

# age group
clean_data %>%
  select(var_list) %>%
  summarise_all(funs(chisq.test(., clean_data$earliest_age_grp, simulate.p.value = TRUE)$p.value))

# previous chlamydia/gonorrhea dx
clean_data %>%
  select(var_list) %>%
  summarise_all(funs(chisq.test(., clean_data$ctgc_cat, simulate.p.value = TRUE)$p.value))

# before or after 2011 (STOP HIV)
clean_data %>%
  select(var_list) %>%
  summarise_all(funs(chisq.test(., clean_data$post2011, simulate.p.value = TRUE)$p.value))






#### MODEL BUILDING ####

# Building a regression model requires careful thought throughout and is not simply the following 
#   predefined steps.  In general, you must consider and decide:

#     - What is the purpose of my model (to describe, explain, or predict)?
#     - What type of model is appropriate for my purpose and data (e.g., ordinary linear, generalized linear, etc.)?
#     - What is the best fit model for my data?
 

# There are various approaches to model building, including

#     - begin with 'full model', containing all relevant covariates, then possibly remove covariates to achieve a better fit
#     - begin with simple model (e.g., only one covariate) and build by iteratively adding covariates and assessing model fit


# Regardless of the approach, it is essential to assess the fit of candidate models against the observed data and against
#   each other, which can be done by, for example, 

#     - generating predicted ('fitted') values from the model and comparing them against the data, and/or 
#     - generating residuals ('errors') from the model and using various diagnostics to assess fit, and/or
#     - examining goodness-of-fit statistics (AIC, BIC, dispersion)



# In this example, the objective is to describe those factors associated with syphilis diagnosis.  
#   In other words, we are attempting generate hypotheses, not test them.  Thus we are building 
#   a *descriptive model* (as opposed to an explanatory or predictive model).

# Our dependent variable, syphilis diagnosis (syph_dx), is a binary yes/no indicator; thus, we will be using
#   a logistic (binomial) regression model and exploring several covariates of interest.  



# To simplify our example, we will first remove patients missing gender or age information
analysis_data <- clean_data %>%
  filter(!is.na(gender_bin),
         !is.na(earliest_age_yrs))



# How do we determine the best fit model for these data?
# As a starting point, one could begin with a simple model (e.g., only 1 covariate) and add covariates as appropriate



#### FIT CANDIDATE MODEL ####

#   For illustration purposes, let's being with an age-only model
age_only_model <- glm(syph_dx ~ earliest_age_yrs, family = "binomial", data = analysis_data)


#   Visualise the model fit: plot probability of syph_dx as a function of patient age
ggplot(analysis_data, aes(x = earliest_age_yrs, y = syph_dx)) + 
  geom_point(alpha = 0.15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  xlab("Age") + ylab("Probability of Syphilis Dx")



#### ASSESS MODEL FIT ####

#   summary of the model ouput includes helpful information on model fit, including residuals and 
#   deviance (measure of goodness of fit in generalized linear models)
summary(age_only_model)
anova(age_only_model, test="Chisq")


# The age term appears highly significant, with a reduction in deviance of 553.54 
#   compared to the NULL (intercept-only) model:

      # Coefficients:
      #                     Estimate Std. Error z value            Pr(>|z|)    
      #   (Intercept)      -7.090126   0.095191  -74.48 <0.0000000000000002 ***
      #   earliest_age_yrs  0.064993   0.002465   26.37 <0.0000000000000002 ***
      
        
      #                   Df Deviance Resid. Df Resid. Dev              Pr(>Chi)    
      # NULL                            132901     9938.7                          
      # earliest_age_yrs  1   553.54    132900     9385.1 < 0.00000000000000022 ***


# Similarly, the odds ratio shows increasing probability of syphilis diagnosis with age (6.7% increase per year of age)
exp(cbind(OR = coef(age_only_model), confint.default(age_only_model)))


# Similarly, we can see how the model predicted probabilties of syphilis diagnosis varies with age 
pred_age_only <- analysis_data %>%
  data_grid(earliest_age_yrs) %>%
  add_predictions(age_only_model) %>%
  mutate(probability = exp(pred))

pred_age_only



# But in terms of model fit, note the skewed residuals (min residual = -0.6727 and max residual = 3.4793) 
#   perhaps suggesting the age-only model is not a good fit to these data

      # Deviance Residuals: 
      #     Min       1Q   Median       3Q      Max  
      # -0.6727  -0.1081  -0.0890  -0.0781   3.4793  
      # 
      # Residual deviance: 9385.1  on 132900  degrees of freedom
      # AIC: 9389.1


# It is useful to visualise the residuals as an assessment of model fit

# First, we add the residuals and the predicted (fitted) values from the model to our analysis
#   dataset.  This allows us to inspect which individuals in the dataset fit well or fit poorly
age_only_residuals <- analysis_data %>%
  mutate(resid = residuals(age_only_model),
         pred = predict(age_only_model, type = "link"))


# plot residuals by predicted (fitted values)
ggplot(age_only_residuals) +
  geom_point(aes(x = pred, y = resid, colour = as.factor(syph_dx))) +
  xlab("Model fitted values") + ylab("Deviance residuals") + labs(colour = "Syphilis Dx (1=Yes, 0=No)") +
  theme(legend.position="bottom")



# Also, we can assess model fit using formal goodness-of-fit statistics

# One basic element of goodness-of-fit is to determine how much of observed variation in our outcome
#   is explained by our model.  In simple linear models, this is measured as R-squared.

# Although traditional R-squared is not directly available in logistic regression, we can get a rough
#   equivalent based on a comparison of the deviance in the null model (intercept only) to that in 
#   our candidate model (age-only)

#   this calculation shows that our age-only model only explains roughly 6% of the variation in
#   the probability of syphilis diagnosis
(1-exp( ((age_only_model$deviance) - (age_only_model$null.deviance)) / (age_only_model$df.null+1) )) / 
  (1-exp( -(age_only_model$null.deviance) / (age_only_model$df.null+1) ))





#### ASSESS MODEL ASSUMPTIONS ####

# check assumption of linearity for model

# for logistic regression, we can calculate observed proportions and log odds of syphilis dx from the 
#   data and plot these against our variable of interest to assess whether or not the relationship
#   is approximately linear

obs.probs <- analysis_data %>% 
  mutate( age_grp = cut(earliest_age_yrs, breaks = 10),
          age_grp = gsub('\\(|\\]', '', age_grp)) %>% 
  group_by(age_grp, syph_dx) %>% 
  summarise(count = n()) %>%
  group_by(age_grp) %>%
  mutate(prop = count/sum(count),
         log.odds = log (prop / (1-prop))) %>%
  filter(syph_dx == 1) %>%
  separate(age_grp, c("lower_age","upper_age"), sep = ",", remove = FALSE) %>%
  mutate(mid_age = as.numeric(lower_age) + 0.5*(as.numeric(upper_age) - as.numeric(lower_age))) # create mid age for plotting

model.pred.line = tibble(syph_pred = predict(age_only_model), age = analysis_data$earliest_age_yrs)


# plot observed probabilities against model fit
ggplot() + 
  geom_point(data = obs.probs, aes(x = mid_age, y = log.odds)) +
  geom_line(data = model.pred.line, aes(x = age, y = syph_pred), colour = "red", size = 1) +
  xlab("Age") + ylab("Prob. syphilis dx (log odds)")



# check for outliers

# In looking for unusual observations, it is helpful to calculate various  'influence measures' 
#   for each data point, which indicate impact of that point on the overall model fit.  Cook's Distance
#   is one such measure that may be useful.

cooks.d <- tibble(cooks.distance(age_only_model)) %>%
  rowid_to_column() %>%
  rename(cooks.d = `cooks.distance(age_only_model)`)

# create 'index' plot to visualise distribution of Cook's D
ggplot(cooks.d) +
  geom_point(aes(x = rowid, y = cooks.d)) +
  xlab("Observation number") + ylab("Cook's Distance")




# Another aspect of model assumptions/fit is the appropriateness of our link function.  In binomial and Poisson
#   models, variation is assumed to increase in proportion to the mean; hence, the dispersion parameter is assumed
#   to be 1.0.

# Notice, however, that in our summary statistics the model deviance is much less than the model degrees-of-freedom
#   suggesting underdispersed data (deviance/degrees-of-freedom = 9385.1/132900  = 0.07 -- much less than 1.0)  

summary(age_only_model)


# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 9938.7  on 132901  degrees of freedom
# Residual deviance: 9385.1  on 132900  degrees of freedom
# AIC: 9389.1


# We could fix this by allowing the dispersion parameter to be estimated, rather than assuming a fixed value of 1.0.
#   We do this by fitting a quasibinomial model
age_only_model_quasi <- glm(syph_dx ~ earliest_age_yrs, family = "quasibinomial", data = analysis_data)

# Note, however, that this change affects only the estimated standard errors of the model coefficients, but does
#   nothing to improve the fit to the data per se
summary(age_only_model_quasi)









#### COMPARE CANDIDATE MODELS #### 

# Given that age alone does not predict well the probability of syphilis diagnosis, we need to 
#   build additional covariates into our model

# For example, we could add sex into our model to see if this improves fit
age_sex_model <- glm(syph_dx ~ earliest_age_yrs + gender_bin, family = "binomial", data = analysis_data)
age_sex_int_model <- glm(syph_dx ~ earliest_age_yrs + gender_bin + earliest_age_yrs:gender_bin, family = "binomial", data = analysis_data)

# Visualise the model fit: plot probability of syph_dx as a function of patient age and sex
ggplot(analysis_data, aes(x = earliest_age_yrs, y = syph_dx)) + 
  geom_point(alpha = 0.15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  facet_grid(~gender_bin) +
  xlab("Age") + ylab("Probability of Syphilis Dx")


# model summary (both main effects of age and sex are significant, but interaction term is not)
summary(age_sex_int_model)
anova(age_sex_int_model, test = "Chisq")


# residual diagnostic plot (with just main effects for age and sex)
age_sex_residuals <- analysis_data %>%
  mutate(resid = residuals(age_sex_model),
         pred = predict(age_sex_model, type = "link"))


# plot residuals by predicted (fitted values)
ggplot(age_sex_residuals) +
  geom_point(aes(x = pred, y = resid, colour = as.factor(syph_dx))) +
  xlab("Model fitted values") + ylab("Deviance residuals") + labs(colour = "Syphilis Dx (1=Yes, 0=No)") 


# compared goodness-of-fit statistics for age-only model vs age + sex model
age_only_model$aic
age_sex_model$aic
age_sex_int_model$aic

# calculate approximate proportion of deviance explained: 14%
(1-exp( ((age_sex_model$deviance) - (age_sex_model$null.deviance)) / (age_sex_model$df.null+1) )) / 
  (1-exp( -(age_sex_model$null.deviance) / (age_sex_model$df.null+1) ))



# checking assumption of linearity: generate observed probabilities by age and sex
obs.probs <- analysis_data %>% 
  mutate( age_grp = cut(earliest_age_yrs, breaks = 10),
          age_grp = gsub('\\(|\\]', '', age_grp)) %>% 
  group_by(gender_bin, age_grp, syph_dx) %>% 
  summarise(count = n()) %>%
  group_by(gender_bin, age_grp) %>%
  mutate(prop = count/sum(count),
         log.odds = log (prop / (1-prop))) %>%
  filter(syph_dx == 1) %>%
  separate(age_grp, c("lower_age","upper_age"), sep = ",", remove = FALSE) %>%
  mutate(mid_age = as.numeric(lower_age) + 0.5*(as.numeric(upper_age) - as.numeric(lower_age))) # create mid age for plotting


model.pred.line = tibble(syph_pred = predict(age_sex_model), sex = analysis_data$gender_bin, age = analysis_data$earliest_age_yrs)


# plot observed probabilities against model fit
ggplot() + 
  geom_point(data = obs.probs, aes(x = mid_age, y = log.odds, colour = gender_bin)) +
  geom_line(data = model.pred.line, aes(x = age, y = syph_pred, colour = sex), size = 1) +
  xlab("Age") + ylab("Prob. syphilis dx (log odds)")


# the relation between syphilis dx and age appears non-linear, especially for males
#   we can try a model including a quadratic term for age to see if that is a better fit
analysis_data <- analysis_data %>% mutate(age_squared = earliest_age_yrs^2)

age_sq_sex_model <- glm(syph_dx ~ earliest_age_yrs + age_squared + gender_bin, family = "binomial", data = analysis_data)


# model summary: age and age-squared are significant, as is sex
summary(age_sq_sex_model)
anova(age_sq_sex_model, test = "Chisq")


# examine fit including quadratic term for age -- looks much better agreement with observed probabilities, especially for males
model.pred.line = tibble(syph_pred = predict(age_sq_sex_model), sex = analysis_data$gender_bin, age = analysis_data$earliest_age_yrs)

ggplot() + 
  geom_point(data = obs.probs, aes(x = mid_age, y = log.odds, colour = gender_bin)) +
  geom_line(data = model.pred.line, aes(x = age, y = syph_pred, colour = sex), size = 1) +
  xlab("Age") + ylab("Prob. syphilis dx (log odds)")



# compared goodness-of-fit statistics
age_only_model$aic
age_sex_model$aic
age_sq_sex_model$aic


# calculate approximate proportion of deviance explained: 16%
(1-exp( ((age_sq_sex_model$deviance) - (age_sq_sex_model$null.deviance)) / (age_sq_sex_model$df.null+1) )) / 
  (1-exp( -(age_sq_sex_model$null.deviance) / (age_sq_sex_model$df.null+1) ))



# residual diagnostic plot (with just main effects for age and sex)
age_sq_sex_residuals <- analysis_data %>%
  mutate(resid = residuals(age_sq_sex_model),
         pred = predict(age_sq_sex_model, type = "link"))


# plot residuals by predicted (fitted values)
ggplot(age_sq_sex_residuals) +
  geom_point(aes(x = pred, y = resid, colour = as.factor(syph_dx))) +
  facet_grid(~gender_bin) +
  xlab("Model fitted values") + ylab("Deviance residuals") + labs(colour = "Syphilis Dx (1=Yes, 0=No)") 


# as above, we note that the data are under-dispersed (deviance/df << 1),thus the SE values are likely too narrow 
#   and a quasibinomial model would be a better choice (although here it does not qualitatively change our model)
age_sq_sex_model_quasi <- glm(syph_dx ~ earliest_age_yrs + age_squared + gender_bin, family = "quasibinomial", data = analysis_data)
summary(age_sq_sex_model_quasi)




# Continuing with this iterative process, we can build and compare a series of models - for each, we
  # - assess fit to the data (generating model summaries and plots of predicted values and residuals)
  # - compare to simpler models (using deviance reduction, % of variation explained, and goodness-of-fit statistics)
  # - examine model assumptions and adjust as appropriate (e.g., quadratic terms with relations are non-linear)

# For example, we might next add the number of chlamydia/gonorrhea dx to our model (and examine a quadratic term as well)
analysis_data <- analysis_data %>% mutate(CTGC_squared = TotalCTGC^2)

age_sq_sex_ctgc_model <- glm(syph_dx ~ earliest_age_yrs + age_squared + gender_bin + TotalCTGC, 
                        family = "binomial", data = analysis_data)

age_sq_sex_ctgc_sq_model <- glm(syph_dx ~ earliest_age_yrs + age_squared + gender_bin + TotalCTGC + CTGC_squared, 
                             family = "binomial", data = analysis_data)


# model summary: age and age-squared are significant, as is sex
summary(age_sq_sex_ctgc_model)
anova(age_sq_sex_ctgc_model, test = "Chisq")

summary(age_sq_sex_ctgc_sq_model)
anova(age_sq_sex_ctgc_sq_model, test = "Chisq")


# checking assumption of linearity: generate observed probabilities by age and sex
obs.probs <- analysis_data %>% 
  mutate( ctgc_grp = cut(TotalCTGC, breaks = 5),
          ctgc_grp = gsub('\\(|\\]', '', ctgc_grp)) %>% 
  mutate( age_grp = cut(earliest_age_yrs, breaks = 10),
          age_grp = gsub('\\(|\\]', '', age_grp)) %>% 
  group_by(gender_bin, age_grp, ctgc_grp, syph_dx) %>% 
  summarise(count = n()) %>%
  group_by(gender_bin, age_grp, ctgc_grp) %>%
  mutate(prop = count/sum(count),
         log.odds = log (prop / (1-prop))) %>%
  filter(syph_dx == 1) %>%
  separate(ctgc_grp, c("lower_ctgc","upper_ctgc"), sep = ",", remove = FALSE) %>%
  mutate(mid_ctgc = as.numeric(lower_ctgc) + 0.5*(as.numeric(upper_ctgc) - as.numeric(lower_ctgc))) %>% # create mid age for plotting
  separate(age_grp, c("lower_age","upper_age"), sep = ",", remove = FALSE) %>%
  mutate(mid_age = as.numeric(lower_age) + 0.5*(as.numeric(upper_age) - as.numeric(lower_age))) %>% # create mid age for plotting
  arrange(gender_bin, as.numeric(lower_age), as.numeric(lower_ctgc))


model.pred.line = tibble(syph_pred = predict(age_sq_sex_ctgc_sq_model), 
                         ctgc = analysis_data$TotalCTGC, 
                         age = analysis_data$earliest_age_yrs,
                         sex = analysis_data$gender_bin)


# plot observed probabilities against model fit
ggplot() + 
  geom_point(data = obs.probs, aes(x = mid_ctgc, y = log.odds)) +
  geom_line(data = model.pred.line, aes(x = ctgc, y = syph_pred, colour = sex), size = 1) +
  facet_grid(mid_age ~ .) +
  xlab("Total CTGC") + ylab("Prob. syphilis dx (log odds)")








# Alternatively, one could begin with a full ('staturated') model including all relevant covariates and
#   interaction terms

full_model <- glm(syph_dx ~ earliest_age_yrs + hiv_atoc + everlgv + gender_bin + 
                    surveillance_region_ha + TotalCTGC + post2011 +
                    
                    age_squared + CTGC_squared +
                    
                    earliest_age_yrs:hiv_atoc + earliest_age_yrs:everlgv + earliest_age_yrs:gender_bin + 
                    earliest_age_yrs:surveillance_region_ha + earliest_age_yrs:TotalCTGC + earliest_age_yrs:post2011 +
                    
                    hiv_atoc:everlgv + hiv_atoc:gender_bin + 
                    hiv_atoc:surveillance_region_ha + hiv_atoc:TotalCTGC + hiv_atoc:post2011 +
                    
                    everlgv:gender_bin + 
                    everlgv:surveillance_region_ha + everlgv:TotalCTGC + everlgv:post2011 +
                    
                    gender_bin:surveillance_region_ha + gender_bin:TotalCTGC + gender_bin:post2011 +
                    
                    surveillance_region_ha:TotalCTGC + surveillance_region_ha:post2011 +
                    
                    TotalCTGC:post2011 
                    
                    , family = "binomial", data = analysis_data)


# summary of model ouput
summary(full_model)
anova(full_model, test = "Chisq")


# examine diagnostic plot of residuals from model fit
plot(full_model)



# calculation of approximate deviance explained: 49%
(1-exp( ((full_model$deviance) - (full_model$null.deviance)) / (full_model$df.null+1) )) / 
  (1-exp( -(full_model$null.deviance) / (full_model$df.null+1) ))

full_model$aic





# We might start with a full model but then iteratively reduce it to one with only those
#   covariates that significantly improve model fit.  The reduced version of the full
#   model might look something like this:
reduced_model <- glm(syph_dx ~ earliest_age_yrs + hiv_atoc + everlgv + gender_bin + 
                    surveillance_region_ha + TotalCTGC + post2011 +
                    
                    age_squared + CTGC_squared +
                      
                    earliest_age_yrs:hiv_atoc + earliest_age_yrs:gender_bin + 
                    
                    hiv_atoc:everlgv +
                    hiv_atoc:surveillance_region_ha + hiv_atoc:TotalCTGC +
                    
                    everlgv:surveillance_region_ha +
                    
                    gender_bin:post2011 +
                    
                    
                    TotalCTGC:post2011 
                  
                  , family = "binomial", data = analysis_data)


anova(reduced_model, test = "Chisq")


full_model$aic
reduced_model$aic

plot(reduced_model)

# calculation of approximate deviance explained: 48% (almost exactly the same as the full model)
(1-exp( ((reduced_model$deviance) - (reduced_model$null.deviance)) / (reduced_model$df.null+1) )) / 
  (1-exp( -(reduced_model$null.deviance) / (reduced_model$df.null+1) ))


# residual diagnostic plot (with just main effects for age and sex)
reduced_model_residuals <- tibble(resid = residuals(reduced_model),
                                  pred = predict(reduced_model, type = "link"))


# plot residuals by predicted (fitted values)
ggplot(reduced_model_residuals) +
  geom_point(aes(x = pred, y = resid)) +
  xlab("Model fitted values") + ylab("Deviance residuals") 




# What if we had used automated stepwise selection to choose a reduced model?
library(MASS)

reduced_model_step <- full_model %>%
  stepAIC(trace = FALSE)


# summary of final model from stepwise selection
summary(reduced_model_step)











# Below code not used
##############################################################################################################
library(rms)
test.fit <- lrm(syph_dx ~ earliest_age_yrs, data = analysis_data, x = TRUE, y = TRUE)

test.fit
anova(test.fit)

fit.dat <- analysis_data %>%
  mutate(pearson.resid = resid(test.fit, "pearson"))

ggplot(fit.dat) +
  geom_point(aes(x = patient_master_key, y = pearson.resid))
