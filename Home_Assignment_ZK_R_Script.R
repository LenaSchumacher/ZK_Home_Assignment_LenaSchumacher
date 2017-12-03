### HOME ASSIGNMENT ZK No. 1###

###Load Data
data_sample_1 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_1.csv")
who(TRUE)

###Load packages (psych, lm.beta, dplyr, gsheet, car, ggplot2, lsr, scatterplot3d)
library(psych) # for describe
library(lm.beta) # for lm.beta
library(dplyr)
library(gsheet)
library(car) # for scatter3d, residualPlots, vif, pairs.panels, ncvTest
library(ggplot2) # for ggplot
library(lsr)
library(scatterplot3d)

require(psych)
require(lm.beta)
require(dplyr)
require(gsheet)
require(car)
require(ggplot2)
require(lsr)
require(scatterplot3d)

###Load error_plotter
error_plotter <- function(mod, col = "black"){
  mod_vars = as.character(mod$call[2])
  data = eval(parse(text = as.character(mod$call[3])))
  y = substr(mod_vars, 1, as.numeric(gregexpr(pattern ='~',mod_vars))-2)
  x = substr(mod_vars, as.numeric(gregexpr(pattern ='~',mod_vars))+2, nchar(mod_vars))
  
  data$pred = predict(mod)
  
  if(x == "1"){x = "response_ID"
  data$response_ID = 1:nrow(data)}
  
  plot(data[,y] ~ data[,x], ylab = y, xlab = x)
  abline(mod)
  
  for(i in 1:nrow(data)){
    clip(min(data[,x]), max(data[,x]), min(data[i,c(y,"pred")]), max(data[i,c(y,"pred")]))
    abline(v = data[i,x], lty = 2, col = col)
  }
  
}

###Descriptive statistics / Check data set for invalid data (e.g. coding errors)

#Descriptive Statistics
who(TRUE)
describe(data_sample_1)
summary(data_sample_1)

#Decision to exclude ID_15 from dataset (sex=3)
data_sample_new1 <- data_sample_1 #copy dataset and create new dataset "data_sample_new"
data_sample_new1 <- data_sample_1[!data_sample_1$sex==3,] #exclude cases where sex=3

#Decision to exclude ID_24, ID_25, ID_66 (because mindfulness < 1)
describe(data_sample_1$mindfulness)
data_sample_new <- data_sample_new1
data_sample_new <- data_sample_new1[!data_sample_new1$mindfulness<=1, ] #exclude cases where mindfulness<1
summary(data_sample_new)
describe(data_sample_new)

#Histograms
hist(data_sample_new$pain, breaks = 20)
hist(data_sample_new$cortisol_serum, breaks = 20)
hist(data_sample_new$STAI_trait, breaks = 20)
hist(data_sample_new$cortisol_saliva, breaks = 20)
hist(data_sample_new$mindfulness, breaks = 20)
hist(data_sample_new$weight, breaks = 20)

#Boxplots
boxplot(data_sample_new$pain)
boxplot(data_sample_new$cortisol_serum)
boxplot(data_sample_new$cortisol_saliva)
boxplot(data_sample_new$STAI_trait)
boxplot(data_sample_new$mindfulness)
boxplot(data_sample_new$weight)

#Scatterplots with lines
plot(pain ~ age, data = data_sample_new)
abline(lm(pain ~ age, data = data_sample_new))
plot(pain ~ STAI_trait, data = data_sample_new)
abline(lm(pain ~ STAI_trait, data = data_sample_new))
plot(pain ~ pain_cat, data = data_sample_new)
abline(lm(pain ~ pain_cat, data = data_sample_new))
plot(pain ~ cortisol_serum, data = data_sample_new)
abline(lm(pain ~ cortisol_serum, data = data_sample_new))
plot(pain ~ cortisol_saliva, data = data_sample_new)
abline(lm(pain ~ cortisol_saliva, data = data_sample_new))
plot(pain ~ mindfulness, data = data_sample_new)
abline(lm(pain ~ mindfulness, data = data_sample_new))
plot(pain ~ weight, data = data_sample_new)
abline(lm(pain ~ weight, data = data_sample_new))

###Regression Model 1
# run a regression analysis
mod1 <- lm(pain ~ age + sex, data = data_sample_new) #create regression model 1
summary(mod1) # adjusted R-squared:  0.10, F = 9.75 (2,153), p = 0.0001
mod1 # Intercept = 8.35, age = -0.08, sex = 0.06
AIC(mod1) # 527.6
lm.beta(mod1) # Intercept = 0.00, age = -0.33, sex = 0.02
confint(mod1) # Intercept = 6.71 - 9.99, age = -0.12 - -0.05, sex = -0.36- 0.48
cooks.distance(mod1)


###Regression Model 2
mod2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_sample_new) #create regression model 2
summary(mod2) # adjusted R-squared:  0.47, F = 20.58 (7,148), p < 0.001
mod2 # Intercept = 2.63, age = -0.06, sex = 0.09, STAI_trait = -0.01, pain_cat = 0.08, mindfulness = -0.18, cortisol_serum = -0.01, cortisoL_saliva = 0.66
AIC(mod2) # 450.28
lm.beta(mod2) # Intercept = 0.00, age = -0.23, sex = 0.03, STAI_trait = -0.04, pain_cat = 0.27, mindfulness = -0.12, cortisol_serum = -0.01, cortisol_saliva = 0.47
confint(mod2) # Intercept = 0.03 - 5.23, age = -0.1 - -0.02, sex = -0.26 - 0.44, STAI_trait = -0.05 - 0.03, pain_cat = 0.03 - 0.13, mindfulness = -0.39 - 0.04, cortisol_serum = -0.4 - 0.38, cortisol_saliva = 0.28 - 1.04
cooks.distance(mod2)
standardCoefs(mod2) # age: b=-0.06, beta=-0.23, sex: b=0.09, beta=0.03, STAI_trait: b=-0.01, beta=-0.04, pain_cat: b=0.08, beta=0.27, mindfulness: b=-0.18, beta=-0.12, cortisol_serum: b=-0.01, beta=-0.01, cortisol_saliva: b=0.66, beta=0.47

###Compare Model 1 & 2, see that adjusted R^2 is better with Model 2 and decide to go with Model 2
anova(mod1, mod2)

###Check assumptions for Model 2

#Check collinearity first as a high correlation between cortisol_serum and cortisol_saliva can be assumed
#Nr.1
vif( mod = mod2 ) # VIF cortisol_serum = 5.36, VIF cortisol_saliva = 5.64
#Nr.2
cor(data_sample_new$cortisol_serum, data_sample_new$cortisol_saliva)

#As VIF is >5, we need to exclude one of the factors (either cortisol_serum or cortisol_saliva)

#Probably decision based on theory, so for cortisol_serum
#Nevertheless, compare model with only cortisol_serum to model with only cortisol_saliva

###Create model 3: with cortisol_saliva
mod3 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = data_sample_new)
summary(mod3) # adjusted R-squared:  0.47, F = 24.17 (6,149), p < 0.001
mod3 # Intercept = 2.65, age = -0.06, sex = 0.09, STAI_trait = -0.01, pain_cat = 0.08, mindfulness = -0.18, cortisoL_saliva = 0.65
confint(mod3)
lm.beta(mod3)
AIC(mod3) # 448.2853

###Create model 4: with cortisol_serum
mod4 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_new)
summary(mod4) # adjusted R-squared:  0.43, F = 20.61 (6,149), p < 0.001
mod4 # Intercept = 4.96, age = -0.09, sex = 0.12, STAI_trait = 0.002, pain_cat = 0.05, mindfulness = -0.28, cortisol_serum = 0.57
confint(mod4)
lm.beta(mod4)
AIC(mod4) # 460.0452

#Compare standardized coefficients and adjusted R^2
lm.beta(mod2) # adj. R^s (mod3) = 0.47, adj. R^s (mod3) = 0.43; beta cortisol_saliva: 0.47, beta cortisol_serum: -0.006

#Both options indicate that we should exclude cortisol_serum,
#as the factor cortisol_saliva explains more variance according to standardized coefficients
#and adjusted R^2 is higher with mod3
#Nevertheless, decision for mod4 as everything else would be datafitting

###Check assumptions for Model 4

#Check Model 4 for normality
#Nr. 1
hist(x=residuals(mod4),xlab= "Value of residual", breaks=20)
#Nr. 2
shapiro.test(x=residuals(mod4)) # p =0.76
#Nr. 3
plot(x=mod4,which = 2)

#Check Model 4 for influential outliers
#Nr. 1
cooks.distance( model = mod4 ) # decide that assumption is fulfilled according to Navarro (Cook's distance <1)
#Nr. 2
plot(x=mod4, which = 4)
#Nr. 3
plot(x=mod4, which = 5)
#Nr. 4
cooksd_mod4 <- cooks.distance(mod4)
plot(cooksd_mod4, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd_mod4, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd_mod4)+1, y=cooksd_mod4, 
     labels=ifelse(cooksd_mod4>4*mean(cooksd_mod4, na.rm=T),names(cooksd_mod4),""), col="red")  # add labels

#Check Model 4 for linearity of the relationship
#Nr. 1
yhat.2 <- fitted.values( object = mod4 )
plot( x = yhat.2,
      y = data_sample_new$pain,
      xlab = "Fitted Values",
      ylab = "Observed Values")
#Nr. 2
plot(x=mod4, which=1)
#Nr. 3
residualPlots(model=mod4)


#Check Model 4 for homoscedasticity
#Nr. 1
plot(x=mod4, which=3)
#Nr. 2
ncvTest(mod4) # p = 0.11, df = 1, Chi-Square = 2.48

#Check Model 4 for uncorrelated predictors, collinearity
#Nr. 1
vif( mod = mod4 ) # all VIF <5
#Nr. 2
numeric_variables1 = c("age", "pain",	"STAI_trait", "pain_cat", "weight",	"mindfulness",	"cortisol_serum")
cor(data_sample_new[,numeric_variables1])

###Decide that all assumptions are fulfilled, Model 4 is the final model

#Confidence interval of the regression coefficients
confint(mod4) # Intercept = 2.66 - 7.26, age = -0.12 - -0.05, sex = -0.24 - 0.48, STAI_trait = -0.04 - 0.05, pain_cat = 0.00 - 0.1, mindfulness = -0.5 - -0.07, cortisol_serum = 0.36 - 0.77

#Calculating B and Beta for Model 4
lm.beta(mod4) # Intercept = 0.00, age = -0.34, sex = 0.04, STAI_trait = 0.01, pain_cat = 0.17, mindfulness = -0.19, cortisol_serum = 0.39

#Compare Model 4 with Model 1
#AIC
AIC(mod1) # 527.6
AIC(mod4) # 460.05

#ANOVA
anova(mod1, mod4) # p < 0.001***, df = 4, F = 23.21


### HOME ASSIGNMENT ZK No. 2###

###Descriptive Statistics
who(TRUE)
describe(data_sample_new)
summary(data_sample_new)

###Create Model 5 with factors age, sex, weight, STAI, pain catastrophizing, mindfulness, serum cortisol with data_sample_new
mod5 <- lm(pain ~ age + sex + weight + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_new)
mod5
summary(mod5)
confint(mod5)
lm.beta(mod5)
AIC(mod5) # 461.1939

###Check assumptions for Model 5
#Check Model 5 for normality
#Nr. 1
hist(x=residuals(mod5),xlab= "Value of residual", breaks=20)
#Nr. 2
shapiro.test(x=residuals(mod5)) # p = 0.7
#Nr. 3
plot(x=mod5,which = 2)

#Check Model 5 for influential outliers
#Nr. 1
cooks.distance( model = mod5 ) # decide that assumption is fulfilled according to Navarro (Cook's distance <1)
#Nr. 2
plot(x=mod5, which = 4)
#Nr. 3
plot(x=mod5, which = 5)
# Nr. 4
cooksd_mod5 <- cooks.distance(mod5)
plot(cooksd_mod5, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd_mod5, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd_mod4)+1, y=cooksd_mod5, 
     labels=ifelse(cooksd_mod5>4*mean(cooksd_mod5, na.rm=T),names(cooksd_mod5),""), col="red")  # add labels

#Check Model 5 for linearity of the relationship
#Nr. 1
yhat.2 <- fitted.values( object = mod5 )
plot( x = yhat.2,
      y = data_sample_new$pain,
      xlab = "Fitted Values",
      ylab = "Observed Values")
#Nr. 2
plot(x=mod5, which=1)
#Nr. 3
residualPlots(model=mod5)

#Check Model 5 for homoscedasticity
#Nr. 1
plot(x=mod5, which=3)
#Nr. 2
ncvTest(mod5) # p = 0.1

#Check Model 5 for uncorrelated predictors, collinearity
#Nr. 1
vif( mod = mod5 ) # all VIF <5
#Nr. 2
numeric_variables2 = c("pain", "age",	"weight", "STAI_trait", "pain_cat",	"mindfulness",	"cortisol_serum")
cor(data_sample_new[,numeric_variables2])


###Decide that all assumptions are fulfilled for Model 5

###Run backward regression for Model 5
mod5 <- lm( formula = pain ~ age + sex + weight + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_new)
stats::step( object = mod5, direction = "backward") # pain ~ age + pain_cat + mindfulness + cortisol_serum

###New regression model only using the predictors that were retained at the end of the backward regression

###Save Model in a new R object ("backward model")
backward_model <- lm(pain ~ age + pain_cat + mindfulness + cortisol_serum, data = data_sample_new)
summary(backward_model) # adjusted R-squared:  0.44, F = 31.13 (4,151), p < 0.001
backward_model # Intercept = 4.99, age = -0.09, pain_cat = 0.05, mindfulness = -0.27, cortisol_serum  = 0.57
lm.beta(backward_model) # age = -0.35, pain_cat = 0.18, mindfulness = -0.19, cortisol_serum = 0.39
confint(backward_model) # Intercept = 2.74 - 7.25, age = -0.12 - -0.06, pain_cat = 0.01 - 0.09, mindfulness = -0.47 - -0.07, cortisol_serum = 0.39 - 0.75
cooks.distance(backward_model)

###Run full regression model from the end of assignment 1; Save model in another R object ("theorybased_model")
theorybased_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_new)
summary(theorybased_model) # adjusted R-squared:  0.43, F = 20.61 (6,149), p < 0.001
theorybased_model # Intercept = 4.96, age = -0.09, sex = 0.12, STAI_trait = 0.002, pain_cat = 0.05, mindfulness = -0.28, cortisol_serum = 0.57

###Compare backward model and theory-based model using AIC and ANOVA

AIC(backward_model) # 456.51
AIC(theorybased_model) # 460.05

anova(backward_model,theorybased_model) # p = 0.8, df = 2, F = 0.22

###Compare backward model and initial model (mod5) using AIC and ANOVA
AIC(backward_model) # 456.51
AIC(mod5) # 461.19

anova(backward_model, mod5) # p = 0.74, df = 3, F = 0.42

###Put the two models to the test on some new data

###Load Data
data_sample_2 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_2.csv")

###Check Data
who(TRUE)
summary(data_sample_2)
describe(data_sample_2)

#Decision to exclude ID_26, ID_30, ID_43, ID_78, ID_93, ID_158 (because mindfulness < 1)
data_sample_new2 <- data_sample_2
data_sample_new2 <- data_sample_2[!data_sample_2$mindfulness<=1, ] #exclude cases where mindfulness<1

#Histograms
hist(data_sample_new2$pain, breaks = 20)
hist(data_sample_new2$cortisol_serum, breaks = 20)
hist(data_sample_new2$STAI_trait, breaks = 20)
hist(data_sample_new2$cortisol_saliva, breaks = 20)
hist(data_sample_new2$mindfulness, breaks = 20)
hist(data_sample_new2$weight, breaks = 20)

#Boxplots
boxplot(data_sample_new2$pain)
boxplot(data_sample_new2$cortisol_serum)
boxplot(data_sample_new2$cortisol_saliva)
boxplot(data_sample_new2$STAI_trait)
boxplot(data_sample_new2$mindfulness)
boxplot(data_sample_new2$weight)

###Make predictions on pain using the regression models or equations of the backward model and the theory-based model
predict(backward_model, newdata = data_sample_new2)
predict(theorybased_model, newdata = data_sample_new2)
points(predict(backward_model) ~ pain, newdata = data_sample_new2, col = "red")
points(predict(theorybased_model) ~ pain, newdata = data_sample_new2, col = "red")

#Predict values of new data set based on models
pred.theorybased_model <- predict(theorybased_model, data_sample_new2)
pred.backward_model <- predict(backward_model, data_sample_new2)

###Compare predicted values with actual pain ratings
RSS_theorybased_model <- sum((data_sample_new2$pain - pred.theorybased_model)^2)
RSS_theorybased_model # 230.0634

RSS_backward_model <- sum((data_sample_new2$pain - pred.backward_model)^2)
RSS_backward_model # 232.2393

pred1 <- predict( object = theorybased_model, newdata = data_sample_new2)
plot( x = pred1, y = data_sample_new2$pain, 
      xlab = "Fitted Values", ylab = "Observed Values")

pred2 <- predict( object = backward_model, newdata = data_sample_new2)
plot( x = pred2, y = data_sample_new2$pain, 
      xlab = "Fitted Values", ylab = "Observed Values")


###Which model was able to predict the actual pain ratings in data file 2 better?
AIC(backward_model) # 456.5106
AIC(theorybased_model) # 460.0452

anova(backward_model,theorybased_model) # # p = 0.8, df = 2, F = 0.2226



### HOME ASSIGNMENT ZK No. 3###

###Load Data
data_sample_3 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_3.csv")
who(TRUE)
describe(data_sample_3)
summary(data_sample_3)

###Load packages (...)
library(psych)
library(car)
library(ggplot2)
library(cAIC4)
library(r2glmm)
library(influence.ME)
library(lattice)
library(reshape2)

require(psych)
require(car)
require(ggplot2)
require(cAIC4)
require(r2glmm)
require(influence.ME)
require(lattice)
require(reshape2)

###Load function to extract standardized beta coefficients
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

# asign ID and location as factors
data_sample_3$ID <- factor(data_sample_3$ID)
data_sample_3$sex <- factor(data_sample_3$sex)

# variables
names(data_sample_3)

# designate which are the repeated variables
repeated_variables <- c("pain1",	"pain2", "pain3",	"pain4")

###########################################################
#                       Explore data                      #
###########################################################

# descriptives
describe(data_sample_3)
table(data_sample_3[,"sex"])

# histograms
hist(data_sample_3$pain1)
hist(data_sample_3$pain2)
hist(data_sample_3$pain3)
hist(data_sample_3$pain4)

# correlation of repeated variables
cor(data_sample_3[,repeated_variables]) #correlation gets lower from pain1 to pain4

###Prepare dataset and convert from wide to long format

# id.vars should be all non-repeated variables
data_sample_3_long <- melt(data_sample_3, measure.vars=repeated_variables, variable.name = "time", value.name = "pain_rating")
# order data frame by participant ID
data_sample_3_long <- data_sample_3_long[order(data_sample_3_long[,"ID"]),]
# change the time variable to a numerical vector
data_sample_3_long$time <- as.numeric(data_sample_3_long$time)

# Take a look at the data in long format
data_sample_3_long

###########################################################
#                        Analysis                         #
###########################################################

###Load packages
library(lme4)
library(lmerTest)

require(lme4)
require(lmerTest)

# Model including the fixed effects of age, sex, weight, STAI_trait, pain_cat, mindfulness, cortisol_serum, time and a random intercept for participant_ID random, but no random slope
mod_rep_int <- lmer(pain_rating ~ age + sex + weight + STAI_trait + pain_cat + mindfulness + cortisol_serum + time + (1|ID), data = data_sample_3_long)

# Model containing the same fixed effects, but also including a random slope over time for each participant and a random intercept 
mod_rep_slope <- lmer(pain_rating ~ age + sex + weight + STAI_trait + pain_cat + mindfulness + cortisol_serum + time + (time|ID), data = data_sample_3_long)

#Summarize both models
summary(mod_rep_int)
summary(mod_rep_slope) 
r2beta(mod_rep_int)
r2beta(mod_rep_slope)
mod_rep_int
mod_rep_slope

### Compare models to decide which one to use
## Plot the regression line (prediction)
# Save predictions of models to variables
data_sample_3_long$pred_int <- predict(mod_rep_int)
data_sample_3_long$pred_slope <- predict(mod_rep_slope)
# Random intercept model
ggplot(data_sample_3_long, aes(y = pain_rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_int, x=time))+
  facet_wrap( ~ ID, ncol = 5)
# Random slope and intercept model
ggplot(data_sample_3_long, aes(y = pain_rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='blue', aes(y=pred_slope, x=time))+
  facet_wrap( ~ ID, ncol = 5)
# Relationship could be curved instead of linear 
# Slope and intercept model looks a bit better

# Comparing models
AIC(mod_rep_int) # 254.0994
AIC(mod_rep_slope) # 240.2401

# Use of cAIC to compare mixed effects models to each other
cAIC(mod_rep_int)$caic # 211.3871
cAIC(mod_rep_slope)$caic # 175.8321

#Decide to go with mod_rep_slope, because cAIC is lower

# Use of Anova to compare mixed effects models to each other
anova(mod_rep_int, mod_rep_slope) # significant

# Plot regression lines and see which approach results in better fit
data_sample_3_long_toplot <- mod_rep_slope
data_sample_3_long_toplot$pred_int <- predict(mod_rep_int)
data_sample_3_long_toplot$pred_slope <- predict(mod_rep_slope)

###New model, because relationship looks rather curved than linear on plots
mod_rep_slope_quadr <- lmer(pain_rating ~ age + sex + weight + STAI_trait + pain_cat + mindfulness + cortisol_serum + time + (time|ID) + I(time^2), data = data_sample_3_long)
summary(mod_rep_slope_quadr)
mod_rep_slope_quadr

# Calculate standard coefficients
stdCoef.merMod(mod_rep_slope_quadr) # Intercept: 0.00 - 0.00, age: -0.09 - 0.11, sex = -0.03 - 0.08, weight = -0.15 - 0.1, STAI_trait: 0.03 - 0.13, pain_cat: 0.27 - 0.12, mindfulness: -0.23 - 0.11, cortisol_serum: 0.22 - 0.13, time: -1.39 - 0.17, I(time^2) = 1.03 - 0.16

# Calculate marginal R^2 with 95% CI
r2beta(mod_rep_slope_quadr)

## Plot the results
# Save prediction to a new variable
data_sample_3_long$pred_slope_quadr <- predict(mod_rep_slope_quadr)
# Plotting
ggplot(data_sample_3_long, aes(y = pain_rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='green', aes(y=pred_slope_quadr, x=time))+
  facet_wrap( ~ ID, ncol = 5)
# Looks like a better fit than before

## Compare models
# cAIC
cAIC(mod_rep_slope)$caic # 175.8321
cAIC(mod_rep_slope_quadr)$caic # 121.9584
# mod_slope_quadr has the better cAIC value

# Use anova
anova(mod_rep_slope, mod_rep_slope_quadr)
# There's a significant difference between the two models

### Based on the results the random slope model including the quadratic term of time is the best choice

### Model diagnostics 
## Checking for influential outliers
influence(mod_rep_slope_quadr, group = "ID")$alt.fixed # When are effects influential? 
influence(mod_rep_slope_quadr, obs = T)$alt.fixed

## Checking assumptions
# Normality
qqmath(mod_rep_slope_quadr, id=0.05) # QQ plot
# Output looks quite normal

# Linearity
# Linearity of prediction and standardized residuals
plot(mod_rep_slope_quadr)
# Linearity of each predictor and the standardized residual
# visualize the linearity of connection of each predictor
predictors <- c("sex", "age", "STAI_trait", "weight", "pain_cat", "mindfulness", "cortisol_serum")
for(i in 1:length(predictors)){
  predictor_to_test = data_sample_3[,predictors[i]]
  print(
    ggplot(data.frame(x = predictor_to_test,pearson=residuals(mod_rep_slope_quadr,type="pearson")),
           aes(x=x,y=pearson)) +
      geom_point() +
      geom_smooth(method = 'loess') +
      theme_bw()
  )
}

# Homoscedasticty 
# Look for funnel shape on this graph
plot(mod_rep_slope_quadr) # No funnel shape
# Levene testing for Heteroscedasticity
summary(lm(residuals(mod_rep_slope_quadr)^2 ~ data_sample_3_long[,"ID"]))
# p>.05, not significant so there is no heteroscedasticity

# Multicollinearity
pairs.panels(data_sample_3_long, col = "red", lm = T)
# Time and Sex look quite linear to all other variables