# PACKAGES NEEDED
library(dplyr);library(readxl);library(lme4)

##########################################################################################################################################################################################
# LOAD DATA
setwd(dir = '')
data_glm<- read_excel("ESM_Territorial-thermoregulation-tradeoff-in-a-dry-forest.xlsx", sheet='glm')
# The prunning of registers are explained in the column "NUM_LIN" of the data frame "Trade_off_S_ochoterenae_2023"

#############

# RELATIONSHIPS OF THE CAUSAL MODEL "F"

# Number of intervals during which the focal male occupied shady spots as a function of Number of intervals during which the focal male performed push-ups
#   i.e., SHADE~ PUSH-UP
summary(glm(data=data_glm, NUM_INT_SHADE_YES~ NUM_INT_PUSH_YES, offset=log(NUM_INT_TOTAL), family=quasipoisson(link='log')))
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    -0.07287    0.09974  -0.731     0.47    
# NUM_INT_PUSH_YES -0.14444    0.02931  -4.928 2.13e-05 ***

# Number of intervals during which the focal male performed push-ups as a function of Number of intervals during which an intruder was within a 3m radius of the focal male
#   i.e., PUSH-UP~ INTRUDER
summary(glm(data=data_glm, NUM_INT_PUSH_YES~ NUM_INT_INTRUDER_YES, offset=log(NUM_INT_TOTAL), family=quasipoisson(link='log')))
# (Intercept)           -2.2138     0.2311  -9.578 3.48e-11 ***
# NUM_INT_INTRUDER_YES   0.3174     0.1119   2.836  0.00765 **  

# Number of intervals during which the focal male performed push-ups as a function of Microclimate temperature
#   i.e., PUSH-UP~ TEMPERATURE
summary(glm(data=data_glm, NUM_INT_PUSH_YES~ as.numeric(TEMP_C), offset=log(NUM_INT_TOTAL), family=quasipoisson(link='log')))
# (Intercept)        -12.0852     3.8411  -3.146  0.00343 **
# TEMP_C               0.3016     0.1131   2.666  0.01165 *

# Number of intervals during which the focal male performed push-ups as a function of Vegetation cover
# i.e., PUSH-UP~ VC
summary(glm(data=data_glm, NUM_INT_PUSH_YES~ VC, offset=log(NUM_INT_TOTAL), family=quasipoisson(link='log')))
# (Intercept) -0.09596    0.58518  -0.164  0.87072   
# CV          -0.05522    0.01786  -3.092  0.00395 **

# Number of intervals during which an intruder was within a 3m radius of the focal male as a function of Microclimate temperature
#   i.e., INTRUDER~ TEMPERATURE
summary(glm(data=data_glm, NUM_INT_INTRUDER_YES~ as.numeric(TEMP_C), offset=log(NUM_INT_TOTAL), family=quasipoisson(link='log')))
#   (Intercept)        -43.4015     8.6746  -5.003 1.70e-05 ***
#   TEMP_C               1.1400     0.2447   4.659 4.73e-05 *** 

# Number of intervals during which an intruder was within a 3m radius of the focal male as a function of Vegetation cover
#   i.e., INTRUDER~ VC
summary(glm(data=data_glm, NUM_INT_INTRUDER_YES~ VC, offset=log(NUM_INT_TOTAL), family=quasipoisson(link='log')))
# (Intercept) -1.09532    1.14836  -0.954   0.3469  
# CV          -0.09338    0.03926  -2.379   0.0231 * 

# Microclimate temperature as a function of Vegetation cover
#   i.e., TEMP~ VC
summary(lm(data=data_glm, TEMP_C~ VC, offset=NUM_INT_TOTAL))
# (Intercept) 12.79542    2.38120   5.374 5.62e-06 ***
# CV          -0.18575    0.06061  -3.064  0.00425 **  

##########################################################################################################################################################################################

# LOAD DATA FOR THE GLMM
data_glm_m<- read_excel("ESM_Territorial-thermoregulation-tradeoff-in-a-dry-forest.xlsx", sheet='glm_m')
#############

# glmm to test the assumption that males were more likely to perform push-ups when in sunny spots versus when in shady spots
summary(aa<- glmer(data=data_glm_m, as.factor(PUSH_YES_1_NOT_0)~ as.factor(SHADE_YES_1_NO_0) + (1|NUM_LAG), family= binomial(link='logit'), na.action = na.omit))
#                               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                   -1.5482     0.3532  -4.383 1.17e-05 ***
# as.factor(SHADE_YES_1_NO_0)1  -1.8206     0.3248  -5.606 2.07e-08 ***
