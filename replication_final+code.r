####################################################
## GOV 2001 REPLICATION PAPER: MATCHING ANALYSIS ###
##   Last updated 12:35pm 5th May 2014 by AET    ###
####################################################

## THIS CODE IMPLEMENTS MAHALANOBIS NEAREST NEIGHBOUR MATCHING AND CREATES A MATCHED DATA SET
## CALLED 'data.mahal' (12,764 OBSERVATIONS) WHICH SHOULD BE USED FOR OUR ANALYSIS.
## THE CODE ALSO CREATES A BALANCE TABLE SHOWING MEAN VALUES FOR OBESE AND NON-OBESE GROUPS
## BEFORE AND AFTER MATCHING.


############################################
### INSTALL AND CALL NECESSARY LIBRARIES ###
############################################
install.packages("sandwich")
install.packages("foreign")
install.packages("stargazer")
install.packages("stats")
install.packages("lmtest")
install.packages("weights")
install.packages("cem")
install.packages("MatchIt")
install.packages("optmatch")
install.packages("plyr")


library(sandwich)
library(foreign)
library(stargazer)
library(stats)
library(lmtest)
library(weights)
library(cem)
library(MatchIt)
library(optmatch)
library(plyr)


###################
### IMPORT DATA ###
###################
setwd("/Users/psaynisch/Documents/Harvard/Spring 2014/Gov2001/replication/nlsy obesity")
nlsy<-read.dta("maintmp.dta")

########################
### DATA PREPARATION ###
########################
#create list of control variables
controls<-c("d_sexf","childany","childf","d_race_b","d_race_o","d_marrnever","d_marroth","age","age2","d_educ9_12",
            "d_educ13_up","d_AFQT_0_25","d_AFQT_25_50","d_AFQT_50_75","d_AFQT_75_100",
            "d_urban_res","d_tenure0_1","d_tenure1_3","d_tenure3_6","d_tenure6_up",
            "d_emp0_9","d_emp10_24","d_emp25_49","d_emp50_999","d_emp1000_up",
            "d_year1989","d_year1990","d_year1992","d_year1993","d_year1994","d_year1996","d_year1998",
            "d_year2000","d_year2002","d_ind_ag","d_ind_for","d_ind_mining","d_ind_const","d_ind_mfrg",
            "d_ind_transp","d_ind_wtrade","d_ind_rtrade","d_ind_finance","d_ind_bus_svc","d_ind_pers_svc",
            "d_ind_entert","d_ind_prof_svc","d_ind_pub_ad","d_occ_mgmt","d_occ_tech","d_occ_admin","d_occ_svc",
            "d_occ_farming","d_occ_prodxn","d_occ_operators","d_occ_military")

#subset data to include only variables used in analyses
varlist<-c("person_id","ln_smwage","CPS_hourly_rec","d_hinsEMP","d_hinsEMPOTH","d_hinsINDCOV","d_hinsPUBLIC","d_hinsNONE",
           "d_hinsUNKNOWN","overwt","d_obese","d_obese1","d_obese2","d_overwt","overwtinsEMP",
           "d_obese1insEMP","d_obese2insEMP","d_obesinsEMP", controls)
nlsy<-nlsy[,c(varlist,"sample_wt","srvy_yr","birth_year","AFQTrevised",
              "educ","tenure","BMI","NumberEmp")]

#######################################
### IMPLEMENT PROPRIETARY FUNCTIONS ###
#######################################

## Calculate cluster-robust standard errors (CRSEs)
# Implement a linear model CRSE function - via Mahmood Arai @ Univ. Stockholm
clx <- function(fm, cluster){
  M <- length(unique(cluster))
  N <- length(cluster)
  dfc <- (M/(M-1))*((N-1)/(N-fm$rank))
  u <- apply(estfun(fm),2,function(x) tapply(x, cluster, sum))
  vcovCL <- dfc*sandwich(fm, meat=crossprod(u)/N)
  coeftest(fm, vcovCL) }


## Implement a weighted Welch 2 sample t-test function
welch <- function(nx,ny,mx,my,vx,vy){
  # mx = weighted mean of x, nx = number of observations in x vector, vx = weighted variance of x
  # http://en.wikipedia.org/wiki/Student's_t-test#Equal_or_Unequal_sample_sizes.2C_unequal_variances
  
  t.stat <- (mx - my)/(sqrt(vx/nx+vy/ny))
  df <- (vx/nx + vy/ny)^2/
    ((vx/nx)^2/(nx-1))+((vy/ny)^2/(ny-1)) ##NB may need to round this for it to work
  dt(t.stat, df=df)
}

# Implement a function to calculate weighted SD 
weighted.var <- function(x, w) {
  sum.w <- sum(w)
  sum.w2 <- sum(w^2)
  mean.w <- sum(x * w) / sum(w)
  (sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2)
}

################################################################
## DATA PRE-PROCESSING: MAHALNOBIS NEAREST NEIGHBOR MATCHING ###
################################################################
mahal <- matchit(formula = d_obese~d_sexf+childany+childf+age+d_urban_res+
                   srvy_yr+AFQTrevised+educ+tenure+NumberEmp+
                   d_race_b+d_race_o+d_marrnever+d_marroth+
                   d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+d_ind_mfrg+
                   d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+
                   d_ind_bus_svc+d_ind_pers_svc+d_ind_entert+d_ind_prof_svc+
                   d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+
                   d_occ_farming+d_occ_prodxn+d_occ_operators,
                 data = nlsy,
                 method = "nearest",
                 distance = "mahalanobis",
                 discard="control")

data.mahal<-match.data(mahal)

mahal.imb<-imbalance(data.mahal$d_obese,data=data.mahal,drop=c("d_obese","CPS_hourly_rec","distance","weights",
                                                               "sample_wt","person_id","ln_smwage","CPS_hourly_rec",
                                                               "d_educ9_12","d_educ13_up",
                                                               "d_AFQT_0_25","d_AFQT_25_50","d_AFQT_50_75","d_AFQT_75_100",
                                                               "d_tenure0_1","d_tenure1_3","d_tenure3_6","d_tenure6_up",
                                                               "d_emp0_9","d_emp10_24","d_emp24_49","d_emp50_999","d_emp1000_up",
                                                               "d_hinsEMP","d_hinsEMPOTH","d_hinsINDCOV",
                                                               "d_hinsPUBLIC","d_hinsNONE","d_hinsUNKNOWN","overwt",
                                                               "d_obese","d_obese1","d_obese2","d_overwt",
                                                               "overwtinsEMP","d_obese1insEMP","d_obese2insEMP",
                                                               "d_obesinsEMP","BMI","age2"))

mahal.imb

################################################
## DATA PRE-PROCESSING: CREATE BALANCE TABLE ###
################################################
# ABBREVIATIONS KEY:
# oo - original obese; on - original non-obese; mo - matched obese; mn - matched non-obese


### Create balance tables pre- and post-matching
## Create vector of weighted mean value of variables for obese and non-obese subsets in pre- & post-match data
oo <- apply(nlsy[which (nlsy$d_obese==1),],2,weighted.mean,w=nlsy$sample_wt[nlsy$d_obese==1])
on <- apply(nlsy[which (nlsy$d_obese==0),],2,weighted.mean,w=nlsy$sample_wt[nlsy$d_obese==0])
mo <- apply(data.mahal[which (data.mahal$d_obese==1),],2,weighted.mean,w=data.mahal$sample_wt[data.mahal$d_obese==1])
mn <- apply(data.mahal[which (data.mahal$d_obese==0),],2,weighted.mean,w=data.mahal$sample_wt[data.mahal$d_obese==0])

## Calculate number of observations in each grouop
oo.n <- length(nlsy$person_id[nlsy$d_obese==1])
on.n <- length(nlsy$person_id[nlsy$d_obese==0])
mo.n <- length(data.mahal$person_id[data.mahal$d_obese==1])
mn.n <- length(data.mahal$person_id[data.mahal$d_obese==0])

## Calculate weighted variances for vectors of means
oo.var <- apply(nlsy[which (nlsy$d_obese==1),],2,weighted.var,w=nlsy$sample_wt[nlsy$d_obese==1])
on.var <- apply(nlsy[which (nlsy$d_obese==0),],2,weighted.var,w=nlsy$sample_wt[nlsy$d_obese==0])
mo.var <- apply(data.mahal[which (data.mahal$d_obese==1),],2,weighted.var,w=data.mahal$sample_wt[data.mahal$d_obese==1])
mn.var <- apply(data.mahal[which (data.mahal$d_obese==0),],2,weighted.var,w=data.mahal$sample_wt[data.mahal$d_obese==0])

## Trim last 2 columns of post-match means and variances (weights and distance)
mo <- mo[1:82]
mn <- mn[1:82]
mo.var <- mo.var[1:82]
mn.var <- mn.var[1:82]

## Calculate p-values for weighted Welch t-test of difference in means between obese and non-obese
o.pvalues <- welch(on.n,oo.n,on,oo,on.var,oo.var)
m.pvalues <- welch(mn.n,mo.n,mn,mo,mn.var,mo.var)

## Compile into a table
match.table <- matrix(nrow=length(colnames(nlsy)), ncol=7)
match.table[,1] <- round(t(on),2)
match.table[,2] <- round(t(oo),2)
match.table[,3] <- round(t(o.pvalues),3)
match.table[,4] <- round(t(mn),2)
match.table[,5] <- round(t(mo),2)
match.table[,6] <- round(t(m.pvalues),3)
match.table[,7] <- ifelse(match.table[,3]<0.05 & match.table[,6]>0.05, "Yes", "No")
# Drop variables not of interest
match.table <- match.table[c(3:4, 8, 11:14, 18, 19:26, 28:74,81),]
# Label table
var.mt <- c("Hourly wage*", "Employer coverage in own name", "Uninsured", "Obese (BMI>30)", 
            "Mildly obese (BMI>30 and BMI<35)", "Morbidly obese (BMI>35)", "Overweight",
            "Obese * employer coverage (own)", "Female", "Any children in household", 
            "Female with children in household","Race - Black", "Race - Other", "Never married", 
            "Formerly married","Age", "Education: 9-12", "Education: 13 and over", "AFQT: 0-25", 
            "AFQT: 25-50", "AFQT: 50-75", "AFQT: 75-100", "Urban residence", "Job tenure: 0-1 years", 
            "Job tenure: 1-3 years", "Job tenure: 3-6 years", "Job tenure: 6+ years","Employer size: 0-9",
            "Employer size: 10-24","Employer size: 25-49", "Employer size: 50-999","Employer size: 1000+",
            "Survey year: 1989", "Survey year: 1990", "Survey year: 1992", "Survey year: 1993",
            "Survey year: 1994", "Survey year: 1996", "Survey year: 1998", "Survey year: 2000", 
            "Survey year: 2002", "Industry: Agriculture", "Industry: Forestry", "Industry: Mining",
            "Industry: Construction", "Industry: Manufacturing", "Industry: Transport", 
            "Industry: Wholesale trade", "Industry: Retail trade", "Industry: Finance", 
            "Industry: Business services", "Industry: Personal services", "Industry: Entertainment",
            "Industry: Professional services", "Industry: Public administration", 
            "Occupation: Management", "Occupation: Technical", "Occupation: Administrative", 
            "Occupation: Service", "Occupation: Farming", "Occupation: Production", 
            "Occupation: Operators", "Occupation: Military", "BMI")
rownames(match.table) <- var.mt
colnames(match.table) <- c("Pre-match non-obese", "Pre-match obese", "Pre-match p-value","Post-match non-obese", 
                           "Post-match obese", "Matched p-value", "Improved balance with matching")
View(match.table)      # Inspect table
count(match.table[,7]) # Count number of variables where balance improved (NB - needs 'plyr' package)
## Matching significantly improved balance on 23 out of 67 variables

stargazer(match.table)

### CREATE A TABLE THAT SUMMARIZES EFFECT OF BALANCING
improve.table <- matrix(nrow=nrow(match.table), ncol=3)
rownames(improve.table) <- rownames(match.table)
colnames(improve.table) <- c("Balanced before matching", "Balanced after matching", 
                             "Balance improved by matching")
improve.table[,1] <- ifelse(match.table[,3]>0.05, "Yes", "No")
improve.table[,2] <- ifelse(match.table[,6]>0.05, "Yes", "No")
improve.table[,3] <- match.table[,7]
improve.table <- improve.table[c(1:3,9:63),] #Drop weight-related variables
View(improve.table)
count(improve.table)

stargazer(improve.table, title="Summary Table, Balance Before and After Matching", out="improvetable.tex")

## 15 variables remained unbalanced after matching
## 23 variables became balanced after matching
## 2 variables became unbalanced after matching
## 18 variables remained balanced after matching

################################################################
####  **PHIL - COULD YOU OUTPUT THE ABOVE TABLE TO LaTeX?**  ###
#### Also, any formatting changes you think. This table will ###
#### be an appendix so we can keep it in long form I think.  ###
################################################################

## Clean up
rm(on.n)
rm(oo.n)
rm(on)
rm(oo)
rm(on.var)
rm(oo.var)
rm(mn.n)
rm(mo.n)
rm(mn)
rm(mo)
rm(mn.var)
rm(mo.var)
rm(var.mt)

#### Original results for comparison - Table 3 Model 1 ####
## Create model 1: Main study sample
t3.m1.orig<-lm(CPS_hourly_rec~d_obese+d_hinsEMP+d_obesinsEMP+d_sexf+childany+childf+d_race_b+d_race_o+
            d_marrnever+d_marroth+age+age2+d_educ9_12+d_educ13_up+d_AFQT_0_25+d_AFQT_25_50+d_AFQT_50_75+
            d_AFQT_75_100+d_urban_res+d_tenure0_1+d_tenure1_3+d_tenure3_6+d_tenure6_up+d_emp0_9+d_emp10_24+
            d_emp25_49+d_emp50_999+d_emp1000_up+d_year1989+d_year1990+d_year1992+d_year1993+d_year1994+
            d_year1996+d_year1998+d_year2000+d_year2002+d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+
            d_ind_mfrg+d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+d_ind_bus_svc+d_ind_pers_svc+
            d_ind_entert+d_ind_prof_svc+d_ind_pub_ad+d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+d_occ_farming+
            d_occ_prodxn+d_occ_operators+d_occ_military, weight=sample_wt, data=nlsy)

# Calculate cluster-robust SEs for model 1
m1.orig<-clx(t3.m1.orig,nlsy$person_id)

#### Recreate main results with matched sample ####

## Create model 1: Main study sample
t3.m1<-lm(CPS_hourly_rec~d_obese+d_hinsEMP+d_obesinsEMP+d_sexf+childany+childf+d_race_b+d_race_o+
            d_marrnever+d_marroth+age+age2+d_educ9_12+d_educ13_up+d_AFQT_0_25+d_AFQT_25_50+d_AFQT_50_75+
            d_AFQT_75_100+d_urban_res+d_tenure0_1+d_tenure1_3+d_tenure3_6+d_tenure6_up+d_emp0_9+d_emp10_24+
            d_emp25_49+d_emp50_999+d_emp1000_up+d_year1989+d_year1990+d_year1992+d_year1993+d_year1994+
            d_year1996+d_year1998+d_year2000+d_year2002+d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+
            d_ind_mfrg+d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+d_ind_bus_svc+d_ind_pers_svc+
            d_ind_entert+d_ind_prof_svc+d_ind_pub_ad+d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+d_occ_farming+
            d_occ_prodxn+d_occ_operators+d_occ_military, weight=sample_wt, data=data.mahal)

# Calculate cluster-robust SEs for model 1
m1<-clx(t3.m1,data.mahal$person_id)

## Compile into table 3 and save output at LaTeX file

stargazer(m1.orig, m1, title="Estimates of the obesity wage offset for health insurance, Original and Matched Samples", 
          align=TRUE,  no.space=TRUE,column.labels=c("Original", "Matched"),
          covariate.labels=c("Obese","Insured","Obese x Insured"),
          keep=c("d_obese", "d_hinsEMP","d_obesinsEMP","Constant"),
          out = "Table3comp.tex")


#### New Analyses - By Race and Gender, Wage and Log Wage as DV ####
#add indicator for white respondent back to data
nlsy$d_race_w=0
nlsy[nlsy$d_race_b==0 & nlsy$d_race_o==0,]$d_race_w=1

### Untransformed Wage ###
#####
modelb<-lm(CPS_hourly_rec~d_obese+d_hinsEMP+d_obesinsEMP+d_sexf+childany+childf+
             d_marrnever+d_marroth+age+age2+d_educ9_12+d_educ13_up+d_AFQT_0_25+d_AFQT_25_50+d_AFQT_50_75+
             d_AFQT_75_100+d_urban_res+d_tenure0_1+d_tenure1_3+d_tenure3_6+d_tenure6_up+d_emp0_9+d_emp10_24+
             d_emp25_49+d_emp50_999+d_emp1000_up+d_year1989+d_year1990+d_year1992+d_year1993+d_year1994+
             d_year1996+d_year1998+d_year2000+d_year2002+d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+
             d_ind_mfrg+d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+d_ind_bus_svc+d_ind_pers_svc+
             d_ind_entert+d_ind_prof_svc+d_ind_pub_ad+d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+d_occ_farming+
             d_occ_prodxn+d_occ_operators+d_occ_military, weight=sample_wt, data=nlsy[nlsy$d_race_b==1,])

summary(modelb)
mb<-clx(modelb,nlsy[nlsy$d_race_b==1,]$person_id)

modelw<-lm(CPS_hourly_rec~d_obese+d_hinsEMP+d_obesinsEMP+d_sexf+childany+childf+
             d_marrnever+d_marroth+age+age2+d_educ9_12+d_educ13_up+d_AFQT_0_25+d_AFQT_25_50+d_AFQT_50_75+
             d_AFQT_75_100+d_urban_res+d_tenure0_1+d_tenure1_3+d_tenure3_6+d_tenure6_up+d_emp0_9+d_emp10_24+
             d_emp25_49+d_emp50_999+d_emp1000_up+d_year1989+d_year1990+d_year1992+d_year1993+d_year1994+
             d_year1996+d_year1998+d_year2000+d_year2002+d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+
             d_ind_mfrg+d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+d_ind_bus_svc+d_ind_pers_svc+
             d_ind_entert+d_ind_prof_svc+d_ind_pub_ad+d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+d_occ_farming+
             d_occ_prodxn+d_occ_operators+d_occ_military, weight=sample_wt, data=nlsy[nlsy$d_race_w==1,])

summary(modelw)
mw<-clx(modelw,nlsy[nlsy$d_race_w==1,]$person_id)

#####

#####
modelbmale<-lm(CPS_hourly_rec~d_obese+d_hinsEMP+d_obesinsEMP+childany+
                 d_marrnever+d_marroth+age+age2+d_educ9_12+d_educ13_up+d_AFQT_0_25+d_AFQT_25_50+d_AFQT_50_75+
                 d_AFQT_75_100+d_urban_res+d_tenure0_1+d_tenure1_3+d_tenure3_6+d_tenure6_up+d_emp0_9+d_emp10_24+
                 d_emp25_49+d_emp50_999+d_emp1000_up+d_year1989+d_year1990+d_year1992+d_year1993+d_year1994+
                 d_year1996+d_year1998+d_year2000+d_year2002+d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+
                 d_ind_mfrg+d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+d_ind_bus_svc+d_ind_pers_svc+
                 d_ind_entert+d_ind_prof_svc+d_ind_pub_ad+d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+d_occ_farming+
                 d_occ_prodxn+d_occ_operators+d_occ_military, weight=sample_wt, data=nlsy[nlsy$d_race_b==1 & nlsy$d_sexf==0,])
summary(modelbmale)
mbmale<-clx(modelbmale,nlsy[nlsy$d_race_b==1 & nlsy$d_sexf==0,]$person_id)

modelbfemale<-lm(CPS_hourly_rec~d_obese+d_hinsEMP+d_obesinsEMP+childany+
                   d_marrnever+d_marroth+age+age2+d_educ9_12+d_educ13_up+d_AFQT_0_25+d_AFQT_25_50+d_AFQT_50_75+
                   d_AFQT_75_100+d_urban_res+d_tenure0_1+d_tenure1_3+d_tenure3_6+d_tenure6_up+d_emp0_9+d_emp10_24+
                   d_emp25_49+d_emp50_999+d_emp1000_up+d_year1989+d_year1990+d_year1992+d_year1993+d_year1994+
                   d_year1996+d_year1998+d_year2000+d_year2002+d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+
                   d_ind_mfrg+d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+d_ind_bus_svc+d_ind_pers_svc+
                   d_ind_entert+d_ind_prof_svc+d_ind_pub_ad+d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+d_occ_farming+
                   d_occ_prodxn+d_occ_operators+d_occ_military, weight=sample_wt, data=nlsy[nlsy$d_race_b==1 & nlsy$d_sexf==1,])
summary(modelbfemale)
mbfemale<-clx(modelbfemale,nlsy[nlsy$d_race_b==1 & nlsy$d_sexf==1,]$person_id)
#####

#####
modelwmale<-lm(CPS_hourly_rec~d_obese+d_hinsEMP+d_obesinsEMP+childany+
                 d_marrnever+d_marroth+age+age2+d_educ9_12+d_educ13_up+d_AFQT_0_25+d_AFQT_25_50+d_AFQT_50_75+
                 d_AFQT_75_100+d_urban_res+d_tenure0_1+d_tenure1_3+d_tenure3_6+d_tenure6_up+d_emp0_9+d_emp10_24+
                 d_emp25_49+d_emp50_999+d_emp1000_up+d_year1989+d_year1990+d_year1992+d_year1993+d_year1994+
                 d_year1996+d_year1998+d_year2000+d_year2002+d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+
                 d_ind_mfrg+d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+d_ind_bus_svc+d_ind_pers_svc+
                 d_ind_entert+d_ind_prof_svc+d_ind_pub_ad+d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+d_occ_farming+
                 d_occ_prodxn+d_occ_operators+d_occ_military, weight=sample_wt, data=nlsy[nlsy$d_race_w==1 & nlsy$d_sexf==0,])
summary(modelwmale)
mwmale<-clx(modelwmale,nlsy[nlsy$d_race_w==1 & nlsy$d_sexf==0,]$person_id)

modelwfemale<-lm(CPS_hourly_rec~d_obese+d_hinsEMP+d_obesinsEMP+childany+
                   d_marrnever+d_marroth+age+age2+d_educ9_12+d_educ13_up+d_AFQT_0_25+d_AFQT_25_50+d_AFQT_50_75+
                   d_AFQT_75_100+d_urban_res+d_tenure0_1+d_tenure1_3+d_tenure3_6+d_tenure6_up+d_emp0_9+d_emp10_24+
                   d_emp25_49+d_emp50_999+d_emp1000_up+d_year1989+d_year1990+d_year1992+d_year1993+d_year1994+
                   d_year1996+d_year1998+d_year2000+d_year2002+d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+
                   d_ind_mfrg+d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+d_ind_bus_svc+d_ind_pers_svc+
                   d_ind_entert+d_ind_prof_svc+d_ind_pub_ad+d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+d_occ_farming+
                   d_occ_prodxn+d_occ_operators+d_occ_military, weight=sample_wt, data=nlsy[nlsy$d_race_w==1 & nlsy$d_sexf==1,])
summary(modelwfemale)
mwfemale<-clx(modelwfemale,nlsy[nlsy$d_race_w==1 & nlsy$d_sexf==1,]$person_id)

#####

#output
stargazer(mb, mw, title="Estimates of the obesity wage offset by race", 
          align=TRUE,  no.space=TRUE, column.labels=c("Black", "White"),
          covariate.labels=c("Obese","Insured","Obese x Insured"),
          keep=c("d_obese", "d_hinsEMP","d_obesinsEMP","Constant"),
          out = "race.tex")

stargazer(mbmale, mwmale, title="Estimates of the obesity wage offset by race - males", 
          align=TRUE,  no.space=TRUE, column.labels=c("Black", "White"),
          covariate.labels=c("Obese","Insured","Obese x Insured"),
          keep=c("d_obese", "d_hinsEMP","d_obesinsEMP","Constant"),
          out = "race2.tex")

stargazer(mbfemale, mwfemale, title="Estimates of the obesity wage offset by race - females", 
          align=TRUE,  no.space=TRUE, column.labels=c("Black", "White"),
          covariate.labels=c("Obese","Insured","Obese x Insured"),
          keep=c("d_obese", "d_hinsEMP","d_obesinsEMP","Constant"),
          out = "race3.tex")

### Log Transformed Wages ###

#####
modelb<-lm(ln_smwage~d_obese+d_hinsEMP+d_obesinsEMP+d_sexf+childany+childf+
             d_marrnever+d_marroth+age+age2+d_educ9_12+d_educ13_up+d_AFQT_0_25+d_AFQT_25_50+d_AFQT_50_75+
             d_AFQT_75_100+d_urban_res+d_tenure0_1+d_tenure1_3+d_tenure3_6+d_tenure6_up+d_emp0_9+d_emp10_24+
             d_emp25_49+d_emp50_999+d_emp1000_up+d_year1989+d_year1990+d_year1992+d_year1993+d_year1994+
             d_year1996+d_year1998+d_year2000+d_year2002+d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+
             d_ind_mfrg+d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+d_ind_bus_svc+d_ind_pers_svc+
             d_ind_entert+d_ind_prof_svc+d_ind_pub_ad+d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+d_occ_farming+
             d_occ_prodxn+d_occ_operators+d_occ_military, weight=sample_wt, data=nlsy[nlsy$d_race_b==1,])

summary(modelb)
mb<-clx(modelb,nlsy[nlsy$d_race_b==1,]$person_id)

modelw<-lm(ln_smwage~d_obese+d_hinsEMP+d_obesinsEMP+d_sexf+childany+childf+
             d_marrnever+d_marroth+age+age2+d_educ9_12+d_educ13_up+d_AFQT_0_25+d_AFQT_25_50+d_AFQT_50_75+
             d_AFQT_75_100+d_urban_res+d_tenure0_1+d_tenure1_3+d_tenure3_6+d_tenure6_up+d_emp0_9+d_emp10_24+
             d_emp25_49+d_emp50_999+d_emp1000_up+d_year1989+d_year1990+d_year1992+d_year1993+d_year1994+
             d_year1996+d_year1998+d_year2000+d_year2002+d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+
             d_ind_mfrg+d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+d_ind_bus_svc+d_ind_pers_svc+
             d_ind_entert+d_ind_prof_svc+d_ind_pub_ad+d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+d_occ_farming+
             d_occ_prodxn+d_occ_operators+d_occ_military, weight=sample_wt, data=nlsy[nlsy$d_race_w==1,])

summary(modelw)
mw<-clx(modelw,nlsy[nlsy$d_race_w==1,]$person_id)


#####


#####
modelbmale<-lm(ln_smwage~d_obese+d_hinsEMP+d_obesinsEMP+childany+
                 d_marrnever+d_marroth+age+age2+d_educ9_12+d_educ13_up+d_AFQT_0_25+d_AFQT_25_50+d_AFQT_50_75+
                 d_AFQT_75_100+d_urban_res+d_tenure0_1+d_tenure1_3+d_tenure3_6+d_tenure6_up+d_emp0_9+d_emp10_24+
                 d_emp25_49+d_emp50_999+d_emp1000_up+d_year1989+d_year1990+d_year1992+d_year1993+d_year1994+
                 d_year1996+d_year1998+d_year2000+d_year2002+d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+
                 d_ind_mfrg+d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+d_ind_bus_svc+d_ind_pers_svc+
                 d_ind_entert+d_ind_prof_svc+d_ind_pub_ad+d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+d_occ_farming+
                 d_occ_prodxn+d_occ_operators+d_occ_military, weight=sample_wt, data=nlsy[nlsy$d_race_b==1 & nlsy$d_sexf==0,])
summary(modelbmale)
mbmale<-clx(modelbmale,nlsy[nlsy$d_race_b==1 & nlsy$d_sexf==0,]$person_id)

modelbfemale<-lm(ln_smwage~d_obese+d_hinsEMP+d_obesinsEMP+childany+
                   d_marrnever+d_marroth+age+age2+d_educ9_12+d_educ13_up+d_AFQT_0_25+d_AFQT_25_50+d_AFQT_50_75+
                   d_AFQT_75_100+d_urban_res+d_tenure0_1+d_tenure1_3+d_tenure3_6+d_tenure6_up+d_emp0_9+d_emp10_24+
                   d_emp25_49+d_emp50_999+d_emp1000_up+d_year1989+d_year1990+d_year1992+d_year1993+d_year1994+
                   d_year1996+d_year1998+d_year2000+d_year2002+d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+
                   d_ind_mfrg+d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+d_ind_bus_svc+d_ind_pers_svc+
                   d_ind_entert+d_ind_prof_svc+d_ind_pub_ad+d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+d_occ_farming+
                   d_occ_prodxn+d_occ_operators+d_occ_military, weight=sample_wt, data=nlsy[nlsy$d_race_b==1 & nlsy$d_sexf==1,])
summary(modelbfemale)
mbfemale<-clx(modelbfemale,nlsy[nlsy$d_race_b==1 & nlsy$d_sexf==1,]$person_id)
#####

#####
modelwmale<-lm(ln_smwage~d_obese+d_hinsEMP+d_obesinsEMP+childany+
                 d_marrnever+d_marroth+age+age2+d_educ9_12+d_educ13_up+d_AFQT_0_25+d_AFQT_25_50+d_AFQT_50_75+
                 d_AFQT_75_100+d_urban_res+d_tenure0_1+d_tenure1_3+d_tenure3_6+d_tenure6_up+d_emp0_9+d_emp10_24+
                 d_emp25_49+d_emp50_999+d_emp1000_up+d_year1989+d_year1990+d_year1992+d_year1993+d_year1994+
                 d_year1996+d_year1998+d_year2000+d_year2002+d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+
                 d_ind_mfrg+d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+d_ind_bus_svc+d_ind_pers_svc+
                 d_ind_entert+d_ind_prof_svc+d_ind_pub_ad+d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+d_occ_farming+
                 d_occ_prodxn+d_occ_operators+d_occ_military, weight=sample_wt, data=nlsy[nlsy$d_race_w==1 & nlsy$d_sexf==0,])
summary(modelwmale)
mwmale<-clx(modelwmale,nlsy[nlsy$d_race_w==1 & nlsy$d_sexf==0,]$person_id)

modelwfemale<-lm(ln_smwage~d_obese+d_hinsEMP+d_obesinsEMP+childany+
                   d_marrnever+d_marroth+age+age2+d_educ9_12+d_educ13_up+d_AFQT_0_25+d_AFQT_25_50+d_AFQT_50_75+
                   d_AFQT_75_100+d_urban_res+d_tenure0_1+d_tenure1_3+d_tenure3_6+d_tenure6_up+d_emp0_9+d_emp10_24+
                   d_emp25_49+d_emp50_999+d_emp1000_up+d_year1989+d_year1990+d_year1992+d_year1993+d_year1994+
                   d_year1996+d_year1998+d_year2000+d_year2002+d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+
                   d_ind_mfrg+d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+d_ind_bus_svc+d_ind_pers_svc+
                   d_ind_entert+d_ind_prof_svc+d_ind_pub_ad+d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+d_occ_farming+
                   d_occ_prodxn+d_occ_operators+d_occ_military, weight=sample_wt, data=nlsy[nlsy$d_race_w==1 & nlsy$d_sexf==1,])
summary(modelwfemale)
mwfemale<-clx(modelwfemale,nlsy[nlsy$d_race_w==1 & nlsy$d_sexf==1,]$person_id)

#####

stargazer(mb, mw, title="Estimates of the log obesity wage offset by race", 
          align=TRUE,  no.space=TRUE, column.labels=c("Black", "White"),
          covariate.labels=c("Obese","Insured","Obese x Insured"),
          keep=c("d_obese", "d_hinsEMP","d_obesinsEMP","Constant"),
          out = "lograce.tex")

stargazer(mbmale, mwmale, title="Estimates of the log obesity wage offset by race - males", 
          align=TRUE,  no.space=TRUE, column.labels=c("Black", "White"),
          covariate.labels=c("Obese","Insured","Obese x Insured"),
          keep=c("d_obese", "d_hinsEMP","d_obesinsEMP","Constant"),
          out = "lograce2.tex")

stargazer(mbfemale, mwfemale, title="Estimates of the log obesity wage offset by race - females", 
          align=TRUE,  no.space=TRUE, column.labels=c("Black", "White"),
          covariate.labels=c("Obese","Insured","Obese x Insured"),
          keep=c("d_obese", "d_hinsEMP","d_obesinsEMP","Constant"),
          out = "lograce3.tex")