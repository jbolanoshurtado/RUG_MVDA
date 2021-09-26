### set working directory
setwd('C:/Users/Jean Pierre/Documents/0-Local Workstation/0-RUG/0-courses/1a courses/Multivariate/Assignments/Assignment1/assignment1_r')
getwd()

set.seed(2021) # to reproduce results when using random numbers

### libraries
library(moments) # to assess skewness and kurtosis
library(Hmisc) # to use the describe fx
library(pastecs) # to test for normality w/ shapiro-wilk
library(olsrr) # to calculate Mallow's Cp (not used after all)
library(car) # durbin-watson test for autocorrelation of errors + VIF
library(MASS)

### import csv ddbb to R
ddbb <- read.csv('~/0-Local Workstation/0-RUG/0-courses/1a courses/Multivariate/Assignments/Assignment1/assignment1_r/Camera_BDM.csv')

### preserve all output numbers as decimals
options(scipen = 0)

### Check for data misentries (out of expected range)

### Check for data misentry (out of expected range)
#****************************************************

### Broad view of database
summary(ddbb) # no missing data, need to check ranges!

### Section I: Data screening (ranges, outliers, skewness, normality) 
## 1) gender var
summary(ddbb$Gender) # range ok
table(ddbb$Gender)   # note fair even distr. M/F

## 2) Age var
summary(ddbb$Age)   # range ok
table(ddbb$Age)     # only 4 students < 18 y.o. 
# to use in regression, recode into dummy (group <18 & 18-24)

## 3) in a Relationship var
summary(ddbb$Relationship)   # range ok
table(ddbb$Relationship)     # 9 students did not disclose relat.
# to use in regression, recode to dummy (group non disclose & yes in relat)

## 4) quantity of smartphones var
summary(ddbb$Nr_smartphones)  # Bad range: 'zero' smartphones!
table(ddbb$Nr_smartphones)    # 0: either misinput or does not qualify 

## (dependent vars screened last)

## 8) Consequentiality composite var
summary(ddbb$Consequentiality)                # ranges ok
describe(ddbb$Consequentiality)               # seems balanced
dens_conseq <- density(ddbb$Consequentiality) # gen var for k.density plot
plot(dens_conseq)                             # nice normal distribution!!!
stat.desc(ddbb$Consequentiality , norm = TRUE , p=0.95) # norm. distrib. indeed

## 9) Avg of photos taken per week var
summary(ddbb$Pictures_week)     # wide range[0-300] vs. IQ range [5-25]. Some 0's
describe(ddbb$Pictures_week)    # median: 15, mean: 25. Positive skewness
sd(ddbb$Pictures_week)          # sd: 41 with outliers, def. skewed
skewness(ddbb$Pictures_week)    # long right tailed skewed.
dens_photos <- density(ddbb$Pictures_week)
plot(dens_photos)               # tail: 100, 150, 200 and 300
table(ddbb$Pictures_week)       # 16 obs >= 100 photos
# as range includes zero, solve skewness with inverse hyperbolic sine

## 10) Avg videos taken per week var
summary(ddbb$Videos_week)       # range ok, mind the 0's
describe(ddbb$Videos_week)      # mean = 3xMedian. right skewed: 75% people do 0-2
skewness(ddbb$Videos_week)      # Positive skewness
dens_video <- density(ddbb$Videos_week)
plot(dens_video)                # weird long right tail
# as range includes zero, solve skewness with inverse hyperbolic sine


## 11) Used a DSLR in the past year
summary(ddbb$Substitute_DSLR)   # only 18% 
table(ddbb$Substitute_DSLR)     # that's 45 subjects vs 201! noted

## 12) Used a compact camera in the past year
summary(ddbb$Substitute_Compact)  # even less! 16% has used a compact one in the past
table(ddbb$Substitute_Compact)    # that's 41 subjects vs 205

## 11) & 12) cross tabs to see overlap
table <- table(ddbb$Substitute_Compact , ddbb$Substitute_DSLR) # all cells filled
chisq.test(table)                                              # no association
cor(ddbb$Substitute_Compact , ddbb$Substitute_DSLR , method = c("spearman")) # no association

## 13) Hedonic use of the camera
summary(ddbb$Camera_hedonic)      # range ok. most obs. highly hedonic!
describe(ddbb$Camera_hedonic)     # only 5.7% has a negative score, negative skew
skewness(ddbb$Camera_hedonic)     # negative skew 
dens_hedonic <- density(ddbb$Camera_hedonic) # non-normal
plot(dens_hedonic)                # indeed, left skewness

## 14) Utilitarian use of camera
summary(ddbb$Camera_utilitiarian)  # range ok, most obs. highly utilitarian!
describe(ddbb$Camera_utilitiarian) # only 11.4% are non-utilitarians (-3,-1)
skewness(ddbb$Camera_utilitiarian) # left skewness, non-normal distribution
dens_utilitarian <- density(ddbb$Camera_utilitiarian)
plot(dens_utilitarian)                        # non-normal, similar to hedonic

## 13) & 14) association btw hedonic & utilitarian
cor(ddbb$Camera_hedonic, ddbb$Camera_utilitiarian, method = c('spearman'))
plot(ddbb$Camera_hedonic, ddbb$Camera_utilitiarian) # weak association/noisy

## 15) Depletion var
summary(ddbb$Depletion)   # range ok, not that normal
describe(ddbb$Depletion)  # slight skewness to the right
stat.desc(ddbb$Depletion, norm = TRUE) # median & mean close, check kurtosis
histogram(ddbb$Depletion) # too much obs on 0, platikurtic
dens_depletion <- density(ddbb$Depletion)
plot(dens_depletion)      # slight right assymetry and platikurtic

## 16) Used Facebook in the past year
summary(ddbb$Services_Facebook)   # 98%, not useful

## 17) Used instagram in the past year
summary(ddbb$Services_Instagram)  # 82.5%, more useful

## 18) Used Snapchat in the past year
summary(ddbb$Services_Snapchat)   # 71.5%, more useful

## 17) & 18) association
table <- table(ddbb$Services_Instagram , ddbb$Services_Snapchat)
chisq.test(table)  # sig. association
cor(ddbb$Services_Instagram,ddbb$Services_Snapchat, method  =c('spearman')) # ~0.4 weak

## 19) Used Twitter
summary(ddbb$Services_Twitter)   # 31.3%

## 20) Used LinkedIn
summary(ddbb$Services_LinkedIn)  # 49.6%

## 21) Used Whatsapp
summary(ddbb$Services_WhatsApp)  #98.4%, not useful

## Dependent variables
# 5) Bid for selfie camera x1 month
summary(ddbb$BDM_front)  # right skewness: IQ range is 75 & range is 10k. Note the zero
describe(ddbb$BDM_front) # median: 50 & mean: 198, right skewness
table(ddbb$BDM_front)    # 1 bid ZERO, 2 bid 1 euro. Note the pref. to end bid in zero
dens_BDM_front <- density(ddbb$BDM_front)
plot(dens_BDM_front)  # 9decile: 240-250, last values: 1k, 2k and 10k. right skewness
sd(ddbb$BDM_front)    # extreme high values inflate sd.
# this variable for the selfie cam will not be used

## 6) Bid for back camera x1 month
summary(ddbb$BDM_main)  # here median is higher.IQ range: 105, min is 1, max 10k.
describe(ddbb$BDM_main) # main camera is more valued than the selfie cam
table(ddbb$BDM_main)    # no one bid zero. High values: 1k, 2k, 1.5k, 10k
dens_BDM_main <- density(ddbb$BDM_main)
plot(dens_BDM_main)     # same, right skewness, few high values
# this variable for main cam will not be used

## 7) Bid for both camera x1 month
summary(ddbb$BDM_both)  # min value is 2. max 20k, IQ range 346.8
describe(ddbb$BDM_both) # p90: 500, p95: 800, & then rest: 1k to 20k! (12 subjects)
table(ddbb$BDM_both)    # 12 subjects: 1k up to 20k
dens_BDM_both <- density(ddbb$BDM_both)
plot(dens_BDM_both)     # indeed right skewed
# This severe right skewness will cause problems.
# Solution: nat. log transformation as all obs are strict. positive


### Section II: Figuring out inconsistencies:

## A) there are 4 minors in our sample
#inspecting their obs.
minors <- ddbb[which(ddbb$Age == 0) , ]
minors  # nothng wrong, no reason to believe their are 10 y.o.
# --> all ok: recode and treat them like bach's students (same as age group 18-24) 

## B) Relationships not disclosed in 9 cases
# inspecting their observations
not_disclosing <- ddbb[which(ddbb$Relationship == 2) , ]
not_disclosing  # nothing wrong, good variability, here: case of 0 bid for selfie cam
# --> all ok, no further actions

## C) 1 subject has zero smartphones
no_smartphone <- ddbb[which(ddbb$Nr_smartphones == 0) , ]
no_smartphone # male, single, >=25yo, no smartphone, bids 10 forall, no pics/vids, 
#               no dslr/compact, no affinity x cams, only LinkedIn
#hypothesis 1, his answer was inputted incorrectly (1 smartphone instead of 0)
#hypothesis 2, he really does not have a smartphone. Not eligible for the study (DROP)
# --> solution: drop from analysis and then add him in sensitivity analysis.

## D) 4 subjects do not take any photo per week on average
no_photos <- ddbb[which(ddbb$Pictures_week == 0) , ]
no_photos
# 1 is the previous case (ok), 2 obs only use FB and WAPP (ok) and wagers are normal
# but 1 case is weird: has 3 smartphones, uses instagram and snapchat but takes 0 photos
# This case is suspected to be a misentry in the dataset. Very unlikely value.
# hyp. 1: we have lost the real response (datum misentry), we have to treat it as missing
# hyp. 2.1: we have the right response, its just her behaviour, use her datum as is
# hyp. 2.2: She takes photos with the other phones so response is correct, use as is.
# --> solution: drop from analysis and then add her in sensitivity analysis

## E) 1 subject does not bid about the front camera
no_BDM_selfie <- ddbb[which(ddbb$BDM_front == 0) , ]
no_BDM_selfie
# no instragram or snapchat, no value for the front cam, doesn't take pictures. 
# This is not unusual but rather there is no value for it. No further actions.








### Section III: Subset without two problem cases (244 out of 246 obs in this subset)
db1 <- data.frame(ddbb[which(ddbb$Nr_smartphones != 0 & ddbb$Resp_id != 82 ), ])

## Generate subsample of 200 observations  (set seed is "2021") left out are 44 obs
db1_1 <- db1[sample(1:nrow(db1), 200 , replace=FALSE),]
id_obs <- db1_1$Resp_id
db1$holdout <- 0
db1$holdout[which(is.element(db1$Resp_id , id_obs) == FALSE)] <- 1 
summary(db1$holdout)
table(db1$holdout) ## holdout var contains the holdout subsample









#########################################
# First model: directly from the theory #
#########################################
lm0 <- lm(db1_1$BDM_both ~      db1_1$Services_Instagram + # hypothesis 1: non-substit
                                db1_1$Services_Snapchat +  # hypothesis 1: non-substit
                                db1_1$Nr_smartphones +     # hypothesis 1: non-substit
                                db1_1$Substitute_Compact + # hypothesis 2: loss aversion
                                db1_1$Substitute_DSLR +    # hypothesis 2: loss aversion
                                db1_1$Pictures_week +      # hypothesis 3: prev. exp.
                                db1_1$Videos_week +        # hypothesis 3: prev. exp.
                                db1_1$Consequentiality)    # hypothesis 7: real conseq.

summary(lm0) ## bad fit, low r2 and non-sig F test
plot(lm0)
plot(lm0,4)
plot(lm0,5)
## worry: positive skewness on errors (severe non-normality).
## worry: too much far away standardised residuals
## worry: too much influential data points on hhat>0.11 [3*(k+1)/n]
ncvTest(lm0) # heteroskedastic


## obtain hhat stats
db1_1$hat_lm0 <- hatvalues(lm0)
summary(db1_1$hat_lm0)
# theoretical max is: (3 * (9/200)) = 0.135

# Test for independence of errors
durbinWatsonTest(lm0)

# Obtain Cook's Distance
db1_1$cook_lm0 <- cooks.distance(lm0)
summary(db1_1$cook_lm0)

#VIF
car::vif(lm0)


# let's see the residuals
db1_1$predict_lm0 <- predict(lm0)
db1_1$stres_lm0 <- rstandard(lm0)
summary(db1_1$stres_lm0)


plot(db1_1$BDM_both ~ db1_1$predict_lm0)  ## the skewness of y wreacks the relationship
plot(db1_1$stres_lm0 ~ db1_1$predict_lm0)
summary(db1_1$stres_lm0) ## skewed distribution of stand. residuals and outliers
describe(db1$BDM_both) ## as noted before, it is positively skewed


## Solution: log transform the dependent variable 
#------------------------------------------------


##############################################
# Second model: log transformed response var #
##############################################

# adding the BDM log transform
db1_1$ln_bdm_both <- log(db1_1$BDM_both)

# model 1 
lm1 <- lm(db1_1$ln_bdm_both ~   db1_1$Services_Instagram + # hypothesis 1: non-substit
                              db1_1$Services_Snapchat +  # hypothesis 1: non-substit
                              db1_1$Nr_smartphones +     # hypothesis 1: non-substit
                              db1_1$Substitute_Compact + # hypothesis 2: loss aversion
                              db1_1$Substitute_DSLR +    # hypothesis 2: loss aversion
                              db1_1$Pictures_week +      # hypothesis 3: prev. exp.
                              db1_1$Videos_week +        # hypothesis 3: prev. exp.
                              db1_1$Consequentiality)    # hypothesis 7: real conseq.

summary(lm1) ## defo better fit than previous model
plot(lm1)    ## worry: the model is worst at predicting middle values of y than the extremes
             ## worry: high values and low values of residuals differ from theoret. normal d.
             ## worry: too much far away standardised residuals
             ## worry: too much influential data points on CD>0.11 [3*(k+1)/n]
plot(lm1,4)
ncvTest(lm1) # homosceda




# Test for independence of errors
durbinWatsonTest(lm1)

## obtain hhat stats
db1_1$hat_lm1 <- hatvalues(lm1)
summary(db1_1$hat_lm1)
# theoretical max is: (3 * (9/200)) = 0.135

# Obtain Cook's Distance
db1_1$cook_lm1 <- cooks.distance(lm1)
summary(db1_1$cook_lm1)

#VIF
car::vif(lm1)








db1_1$predict_lm1 <- predict(lm1)
db1_1$stresid_lm1 <- rstandard(lm1)   
summary(db1_1$stresid_lm1)
plot(db1_1$predict_lm1 , db1_1$ln_bdm_both , xlab = 'predicted' , ylab = 'actual' ) ## outliers
abline(a=0,b=1)
plot(db1_1$stresid_lm1 ~ db1_1$predict_lm1)  ## indeed, too much outliers 
abline(a=0 , b=0)

den_stresid_lm1 <- density(db1_1$stresid_lm1)
plot(den_stresid_lm1)   ## too much long tails

stat.desc(stresid_lm1, norm=TRUE,  p = 0.95) ## non-normal residuals

#
## Tentative solution: transform right skewed photos and videos
## variables' values include zero: log not appropriate
## alternative: inverse hyperbolic sine transformation (IHS)
## test this with rescaled variable
#--------------------------------------------------------------------------------------------

# adding the pictures per month and videos per month vars
db1_1$pics_month <- db1_1$Pictures_week*4
db1_1$vids_month <- db1_1$Videos_week*4

# creating inverse hyperbolic sine transform of pics x week / month & videos x week / month
# we do this rescaling because the ihs is sensitive to scaling of original var. 
# based on (Aihounton & Henningsen, 2021) Oxford: The Econometrics Journal
db1_1$ihs_pics_week <- log(db1_1$Pictures_week + sqrt((db1_1$Pictures_week**2) + 1))
db1_1$ihs_pics_month <- log(db1_1$pics_month + sqrt((db1_1$pics_month**2) + 1))
db1_1$ihs_vids_week <- log(db1_1$Videos_week + sqrt((db1_1$Videos_week**2) + 1))
db1_1$ihs_vids_month <- log(db1_1$vids_month + sqrt((db1_1$vids_month**2) + 1))


describe(db1_1$ihs_pics_week)
describe(db1_1$ihs_pics_month)
describe(db1_1$ihs_vids_week)
describe(db1_1$ihs_vids_month)

dens_ihs_pics_week <- density(db1_1$ihs_pics_week)
plot(dens_ihs_pics_week)
plot(dens_photos)

## completely better behaved distributions. minimum difference between decision on scale

####################################################################################
# Third model: same as model 2 with IHS transformation on photos and videos x week #
####################################################################################

# model 2
lm2 <- lm(db1_1$ln_bdm_both ~   db1_1$Services_Instagram + # hypothesis 1: non-substit
                              db1_1$Services_Snapchat +  # hypothesis 1: non-substit
                              db1_1$Nr_smartphones +     # hypothesis 1: non-substit
                              db1_1$Substitute_Compact + # hypothesis 2: loss aversion
                              db1_1$Substitute_DSLR +    # hypothesis 2: loss aversion
                              db1_1$ihs_pics_week +      # hypothesis 3: prev. exp.
                              db1_1$ihs_vids_week +        # hypothesis 3: prev. exp.
                              db1_1$Consequentiality)    # hypothesis 7: real conseq.

summary(lm2) ## Even better fit than previous model!!!!
plot(lm2)    ## worry: slight heteroskedasticity
             ##  worry: some extreme residuals
             ## worry: few far away residuals, slight heteroskedasticity
             ## leverage problem solved
ncvTest(lm2) #not homoscedastic



# Test for independence of errors
durbinWatsonTest(lm2)

## obtain hhat stats
db1_1$hat_lm2 <- hatvalues(lm2)
summary(db1_1$hat_lm2)
# theoretical max is: (3 * (9/200)) = 0.135

# Obtain Cook's Distance
db1_1$cook_lm2 <- cooks.distance(lm2)
summary(db1_1$cook_lm2)
plot(lm2,4)

#VIF
car::vif(lm2)




db1_1$predict_lm2 <- predict(lm2)
db1_1$stresid_lm2 <- rstandard(lm2)  
summary(db1_1$stresid_lm2)
plot(db1_1$predict_lm2 , db1_1$ln_bdm_both , xlab = 'predicted' , ylab = 'actual' ) ## outliers
abline(a=0,b=1)
plot(db1_1$stresid_lm2 ~ db1_1$predict_lm2)  ## indeed, too much outliers 
abline(a=0 , b=0)
den_stresid_lm2 <- density(db1_1$stresid_lm2)
plot(den_stresid_lm2)   ## too much long tails but better looking

stat.desc(db1_1$stresid_lm2, norm=TRUE,  p = 0.95) ## non-normal residuals but better looking


# Last step is to inspect the outliers, even though, they have no influence on the betas now
# pinpointing the obs that we are interested on (st. dev. of errors > |3|)

plot(db1_1$stresid_lm2 ~ db1_1$predict_lm2, 
     xlab = 'Predicted IHS(photos x week)' , 
     ylab = 'Standardised error', 
     main = 'Standardised residuals vs predicted values')  ## indeed, too much outliers 
axis(2,seq(-4,4,1))
abline(a=2, b=0)
abline(a=-2, b=0)
abline(a=3,b=0)
abline(a=-3,b=0)






#######################################
# Final model: same but with controls #
#######################################

#re-coding 'Relationship' variable assuming that 2: prefer not to say = 0: Yes in a relationship
db1_1$in_a_relation <- db1_1$Relationship
db1_1$in_a_relation[which(db1_1$in_a_relation == 2)] <- 0
summary(db1_1$in_a_relation)
check <- table(db1_1$Relationship, db1_1$in_a_relation)
check # subject from 'prefer not to say' are now with 'yes in a relationship' responses
db1_1$in_a_relation[which(db1_1$in_a_relation == 1)] <- 88 # moving 'no' from 1 to 88 temporarily
db1_1$in_a_relation[which(db1_1$in_a_relation == 0)] <- 1  # moving 'yes' from 0 to 1
db1_1$in_a_relation[which(db1_1$in_a_relation == 88)] <- 0 # moving 'no' from 88 to 1 
describe(db1_1$in_a_relation) 
check <- table(db1_1$in_a_relation,db1_1$Relationship)
check # all subjects in a relationship are now coded as 1, and subject not in one are coded as 0.



#recoding age
summary(db1_1$Age)
table(db1_1$Age)
#making minors be together with 18-24 y.o. group
db1_1$Age[which(db1_1$Age == 0)] <- 1
table(db1_1$Age) ## moved from 0 to 1
# now recode age minors - 24 y.o. as zero and 25+ as one
db1_1$Age[which(db1_1$Age == 1)] <- 0
db1_1$Age[which(db1_1$Age == 2)] <- 1
table(db1_1$Age) # dummy 1 means higher y.o. group







####################################
# Final model = model 2 + controls #
####################################
lm_final <- lm(db1_1$ln_bdm_both ~   db1_1$Services_Instagram + # hypothesis 1: non-substit
                              db1_1$Services_Snapchat +  # hypothesis 1: non-substit
                              db1_1$Nr_smartphones +     # hypothesis 1: non-substit
                              db1_1$Substitute_Compact + # hypothesis 2: loss aversion
                              db1_1$Substitute_DSLR +    # hypothesis 2: loss aversion
                              db1_1$ihs_pics_week +      # hypothesis 3: prev. exp.
                              db1_1$ihs_vids_week +      # hypothesis 3: prev. exp.
                              db1_1$Consequentiality +   # hypothesis 7: real conseq.
                              db1_1$Gender + db1_1$Age + db1_1$in_a_relation) # control variables

summary(lm_final) ## no contribution whatsoever from the controls
plot(lm_final)
plot(lm_final,4) ## CD are ok
db1_1$hatval_lm_final <- hatvalues(lm_final)
summary(db1_1$hatval_lm_final)
plot(hatvalues(lm_final), type = 'h')
#inspecting the value closest to the max leverage admitted
high_leverage <- db1[which(db1_1$hatval_lm_final > 0.18),]
high_leverage ## understandable, no photos but wages the average. got it.




# Test for independence of errors
durbinWatsonTest(lm_final)


# Obtain Cook's Distance
db1_1$cook_lm_final <- cooks.distance(lm_final)
summary(db1_1$cook_lm_final)
plot(lm2,4)

#VIF
car::vif(lm_final)










plot(lm_final)    ## worry: slight heteroskedasticity
## same worry: too extreme residuals
## worry: few far away residuals, slight heteroskedasticity


db1_1$predict_lm_final <- predict(lm_final)
db1_1$stresid_lm_final <- rstandard(lm_final)   
summary(db1_1$stresid_lm_final)
plot(db1_1$predict_lm_final , db1_1$ln_bdm_both , xlab = 'predicted' , ylab = 'actual' ) ## outliers
abline(a=0,b=1)
plot(db1_1$stresid_lm_final ~ db1_1$predict_lm_final)  ## indeed, too much outliers 
abline(a=0 , b=0)
den_stresid_lm_final <- density(db1_1$stresid_lm_final)
plot(den_stresid_lm2)   ## too much long tails but better looking

stat.desc(db1_1$stresid_lm_final, norm=TRUE,  p = 0.95) ## non-normal residuals but better looking


# Last step is to inspect the outliers, even though, they have no influence on the betas now
# pinpointing the obs that we are interested on (st. dev. of errors > |3|)

plot(db1_1$stresid_lm_final ~ db1_1$predict_lm_final, 
     xlab = 'Predicted IHS(photos x week)' , 
     ylab = 'Standardised error', 
     main = 'Standardised residuals vs predicted values') 
axis(2,seq(-4,4,1))
abline(a=2, b=0)
abline(a=-2, b=0)
abline(a=3,b=0)
abline(a=-3,b=0)
ncvTest(lm_final) #approx normal


### Testing the cross-validity of the model on the holdout sample

# Need to transform BDM_Both, pictures, videos, age y in_a_relationship in TOTAL SAMPLE

#ln(BDM_Both)
db1$ln_bdm_both <- log(db1$BDM_both)
#IHS of pictures
db1$ihs_pics_week <- log(db1$Pictures_week + sqrt((db1$Pictures_week**2) + 1))
#IHS of videos
db1$ihs_vids_week <- log(db1$Videos_week + sqrt((db1$Videos_week**2) + 1))
# age
#recoding age
summary(db1$Age)
table(db1$Age)
#making minors be together with 18-24 y.o. group
db1$Age[which(db1$Age == 0)] <- 1
table(db1$Age) ## moved from 0 to 1
# now recode age minors - 24 y.o. as zero and 25+ as one
db1$Age[which(db1$Age == 1)] <- 0
db1$Age[which(db1$Age == 2)] <- 1
table(db1$Age) # dummy 1 means higher y.o. group


# in a relationship
#re-coding 'Relationship' variable assuming that 2: prefer not to say = 0: Yes in a relationship
db1$in_a_relation <- db1$Relationship
db1$in_a_relation[which(db1$in_a_relation == 2)] <- 0
summary(db1$in_a_relation)
check <- table(db1$Relationship, db1$in_a_relation)
check # subject from 'prefer not to say' are now with 'yes in a relationship' responses
db1$in_a_relation[which(db1$in_a_relation == 1)] <- 88 # moving 'no' from 1 to 88 temporarily
db1$in_a_relation[which(db1$in_a_relation == 0)] <- 1  # moving 'yes' from 0 to 1
db1$in_a_relation[which(db1$in_a_relation == 88)] <- 0 # moving 'no' from 88 to 1 
describe(db1$in_a_relation) 
check <- table(db1$in_a_relation,db1$Relationship)
check # all subjects in a relationship are now coded as 1, and subject not in one are coded as 0.


# Validation test

db1$ln_bdm_both_prediction <-   3.235132 + 
                                0.463674 * db1$Services_Instagram +
                                0.082678 * db1$Services_Snapchat + 
                                0.003184 * db1$Nr_smartphones + 
                                -0.060477 * db1$Substitute_Compact + 
                                -0.141863 * db1$Substitute_DSLR + 
                                0.354836 * db1$ihs_pics_week + 
                                -0.131334 * db1$ihs_vids_week + 
                                -0.212190 * db1$Consequentiality + 
                                0.051768 * db1$Gender + 
                                0.668886 * db1$Age + 
                                0.321672 * db1$in_a_relation

table(db1$holdout)                                
db1_holdout <- db1[which(db1$holdout == 1) , ]                               
plot(db1_holdout$ln_bdm_both_prediction , db1_holdout$ln_bdm_both )
abline(a=0,b=1)
cor.test(db1_holdout$ln_bdm_both_prediction , db1_holdout$ln_bdm_both)
cor(db1_holdout$ln_bdm_both_prediction , db1_holdout$ln_bdm_both)**2

db1_holdout$residuals <- db1_holdout$ln_bdm_both - db1_holdout$ln_bdm_both_prediction

plot(db1_holdout$ln_bdm_both_prediction , db1_holdout$residuals)
abline(a=0,b=0)
dens_residuals <- density(db1_holdout$residuals)
plot(dens_residuals)
histogram(db1_holdout$residuals)


plot(db1_holdout$ln_bdm_both_prediction , db1_holdout$ln_bdm_both )
abline(a=0,b=1)


plot(db1_holdout$ln_bdm_both_prediction , db1_holdout$ln_bdm_both, 
     xlab = 'Predicted value of ln(reservation price)' , 
     ylab = 'Actual ln(reservation price) asked by subjects', 
     main = 'Out-of-sample prediction capability of the model') 
abline(a=0,b=1)



plot(dens_residuals,
     xlab = 'Predicted value of ln(reservation price)' , 
     main = "Residuals' distribution for out-of-sample prediction" )

### Annex C: descriptive statistics for the variables included in the model

## Total sample (n=244)

options(scipen = 9)

stat.desc(db1[,c(8, 24, 18, 19, 5, 12, 13, 10, 25, 11, 26, 9, 2, 3, 27)] , basic = TRUE , desc = TRUE , norm = FALSE)
cor(db1[,c(8, 24, 18, 19, 5, 12, 13, 10, 25, 11, 26, 9, 2, 3, 27)])


## Estimation sample (n=200)

stat.desc(db1_1[,c(8, 27, 18, 19, 5, 12, 13, 10, 34, 11, 36, 9, 2, 3, 42)] , basic = TRUE , desc = TRUE , norm = FALSE)
cor(db1_1[,c(8, 27, 18, 19, 5, 12, 13, 10, 34, 11, 36, 9, 2, 3, 42)])

## Holdout sample (n=44)

stat.desc(db1_holdout[,c(8, 24, 18, 19, 5, 12, 13, 10, 25, 11, 26, 9, 2, 3, 27)] , basic = TRUE , desc = TRUE , norm = FALSE)
cor(db1_holdout[,c(8, 24, 18, 19, 5, 12, 13, 10, 25, 11, 26, 9, 2, 3, 27)])


## export databases to csv
#db1
write.csv(db1, "C:\\Users\\Jean Pierre\\Documents\\0-Local Workstation\\0-RUG\\0-courses\\1a courses\\Multivariate\\Assignments\\Assignment1\\assignment1_r\\db1.csv" , row.names = FALSE )
#db1_1
write.csv(db1_1, "C:\\Users\\Jean Pierre\\Documents\\0-Local Workstation\\0-RUG\\0-courses\\1a courses\\Multivariate\\Assignments\\Assignment1\\assignment1_r\\db1_1.csv" , row.names = FALSE )
#db1_holdout
write.csv(db1_holdout, "C:\\Users\\Jean Pierre\\Documents\\0-Local Workstation\\0-RUG\\0-courses\\1a courses\\Multivariate\\Assignments\\Assignment1\\assignment1_r\\db1_holdout.csv" , row.names = FALSE )
