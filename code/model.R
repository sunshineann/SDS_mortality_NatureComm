# Author: Can Zhang
# Date: July 19, 2023
# Purpose: model used in analysis for 
#         Zhang et al. (submitted to Nature Communications)

# NOTE: this code is a guide for transparency and 
#       reproducibility and is not able to be run

#key packages used in this model
#"stats" package is used for stage1 analysis and is the base package for R 
#It does not need to be loaded separately
library("metafor")#for stage2 analysis

#1. Full model formula for the primary analysis
# NOTE: the primary analysis was conducted using data 
#       for the sand and dust storms (SDS) period (1 February–31 May),
#       which had a high frequency of SDS events from 2013 and 2018

#stage1. Time series analysis for each county and each mortality outcome
model1 <- glm(
               #indicating the county-level daily mortality from a cause 
               deathcause ~ 
                
                #a categorical variable with “1” for identified SDS event days, 
                #“2” for non-SDS event days with PM2.5 pollution, 
                #and “0” for reference days
                as.factor(SDSexposure)+
                
                #adjust for long time trend per SDS period, 
                #with the degrees of freedom (df) of 2 during 6 years
                ns(time,df=2*6)+
                
                #adjust for daily mean temperature (df=3) and 
                #daily relative humidity (df=3)
                ns(Tm, df=3)+ns(rh,df=3)+
                
                #adjust for day of the week
                as.factor(dow), 
                
                #data1 is time-series data of each county for a deathcause
                data = data1,
                family = quasipoisson(link = "log"), 
                control = glm.control(epsilon = 10E-8, maxit = 5000))

#stage2. Random-effects meta-analysis based on all the study counties 
#        for each mortality outcome
# NOTE: meta-analyses were conducted for SDS event days (SDSexposure=1) and 
#       non-SDS event days with PM2.5 pollution (SDSexposure=2), separately
model2 <- rma(
            #coefficients vector for variable "SDSexposure" estimated in the stage1
            yi,
            
            #variances vector calculated by squaring the standard errors of the coefficients
            vi,
            
            #data2 contains data for "yi" and "vi"
            data=data2,
            
            #use "restricted maximum-likelihood estimator (REML)" to estimate model coefficients
            method = "REML")

#the pooled coefficient
Beta_meta <- model2$beta
#the pooled standard error of coefficient
Se_meta <- model2$se

#2. Equation for excess risk (ER) of mortality calculation
# NOTE: excess deaths for a cause were estimated for SDS event days (SDSexposure=1) and 
#       non-SDS event days with PM2.5 pollution (SDSexposure=2), separately
ER <- (exp(Beta_meta*1)-1)
ER_lower <- (exp(Beta_meta*1-1.96*Se_meta)-1)
ER_upper <- (exp(Beta_meta*1+1.96*Se_meta)-1)


