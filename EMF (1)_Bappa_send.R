rm(list=ls())

##Packages
library(dplyr)
library(skimr)
library(MatchIt)
library(ggplot2)
library(stargazer)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(caret)
library(car)


##2014 Microdata
#data_14 <- read.csv("/Users/bappadityamukhopadhyay/Desktop/Research/DATA/WorldBank_WDI/FINDEX_ hhld/WLD_2014_FINDEX_v01_M_CSV/micro_world.csv")

#data_14 <- read.csv("/Users/Tizian/Downloads/WLD_2014_FINDEX_v01_M_CSV/micro_world.csv")
##2017 Microdata
#data_17 <- read.csv("/Users/bappadityamukhopadhyay/Desktop/Research/DATA/WorldBank_WDI/FINDEX_ hhld/WLD_2017_FINDEX_v02_M_CSV/micro_world.csv")

#data_17 <- read.csv("/Users/Tizian/Downloads/WLD_2017_FINDEX_v02_M_CSV/micro_world.csv")

##2021 Microdata
#data_21 <- read.csv("/Users/bappadityamukhopadhyay/Desktop/Research/DATA/WorldBank_WDI/FINDEX_ hhld/micro_world_139countries.csv")

#data_21 <- read.csv("/Users/Tizian/Downloads/micro_world_139countries.csv")






#######2014 Data

###MSCI Emerging countries: 
#Brazil, Chile, China, Colombia, Czech Republic, Egypt, Greece, Hungary, India, Indonesia, 
##Korea, Malaysia, Mexico, Peru, Philippines, Poland, Russia, South Africa, Taiwan, Thailand, and Turkey.
##Additional: Kenya, Nigeria, Argentina
data_14$Year<-2014

data_14$EM<-ifelse(data_14$economycode=="BRA"|data_14$economycode=="RUS"|data_14$economycode== "IND" | data_14$economycode=="CHN"| 
                     data_14$economycode=="IDN"| data_14$economycode== "ZAF" | data_14$economycode=="TUR"| data_14$economycode== "COL"
                   |data_14$economycode=="MEX" | data_14$economycode== "TUN"
                   |data_14$economycode=="KEN"  |data_14$economycode=="ARG"  |data_14$economycode=="MAR"
                   |data_14$economycode=="PHL" |data_14$economycode=="THA" |data_14$economycode=="TWN" |data_14$economycode=="POL" |data_14$economycode=="PER"
                   |data_14$economycode=="MYS"|data_14$economycode=="KOR" |data_14$economycode=="HUN"|data_14$economycode=="TUN"|data_14$economycode=="VNM"
                   |data_14$economycode=="GRC" |data_14$economycode=="EGY" |data_14$economycode=="CZE",1,0)

data_14$Low<-ifelse(data_14$economycode=="AFG"| data_14$economycode=="BGD"| data_14$economycode=="BEN"|data_14$economycode=="BFA" |data_14$economycode=="BDI"
                    |data_14$economycode=="TCD"| data_14$economycode=="ZAR" |data_14$economycode=="COG" |data_14$economycode=="CIV" | data_14$economycode=="ETH"
             | data_14$economycode=="GHA"| data_14$economycode=="GIN" |data_14$economycode=="HTI" |data_14$economycode=="HND"
                    |data_14$economycode=="KGZ" |data_14$economycode=="MDG" | data_14$economycode=="MWI"|data_14$economycode=="MLI"| data_14$economycode=="MRT"
                    |data_14$economycode=="MMR"| data_14$economycode=="NPL"|data_14$economycode=="NER"|data_14$economycode=="NGA"|data_14$economycode=="PAK" 
                    |data_14$economycode=="RWA"|data_14$economycode=="SEN"|data_14$economycode=="SLE" |data_14$economycode=="SOM" |data_14$economycode=="LKA"
                    |data_14$economycode=="SDN"| data_14$economycode=="TJK" |data_14$economycode=="TZA"|data_14$economycode=="TGO"|data_14$economycode=="UGA"
                    |data_14$economycode=="WBG"|data_14$economycode=="YEM"|data_14$economycode=="ZMB"|data_14$economycode=="ZWE",1,0)





### Define Treatment and Control: Treatment anyone with bank account; Control reasons of non account is either "too far, no documents, religious reasons,”

data_14$treatment<-ifelse(data_14$account==1,1,0)
data_14$no_account<-ifelse(data_14$q8a==1 | data_14$q8c==1|data_14$q8e==1,1,0)
data_14$control<-ifelse(data_14$treatment==1,0,data_14$no_account)
data_14$Flag<-ifelse(data_14$treatment==1| data_14$control==1,1,0)

summary(data_14$treatment)
summary(data_14$control)
####Subset of only Treatment and Control

data_14 <-subset(data_14,Flag==1)
summary(data_14$treatment)

######Other Controls
data_14$edu_primary<-ifelse(data_14$educ==1,1,0)
data_14$edu_secondary<-ifelse(data_14$educ==2,1,0)
data_14$inc_q<-data_14$inc_q

#####Wage and other receipts
data_14$received_wage<-ifelse(data_14$q34==1,1,0)
data_14$received_wage_account<-ifelse(data_14$q36bc==1 | data_14$q36d==1,1,0)
data_14$received_govt<-ifelse(data_14$q36bc==1 | data_14$q36d==1,1,0)
data_14$received_govt_account<-ifelse(data_14$q40bc==1 | data_14$q40d==1,1,0)
data_14$received_agri<-ifelse(data_14$q43==1,1,0)
data_14$received_agri_account<-ifelse(data_14$q44b==1 | data_14$q44c==1,1,0)

#####Financial Planning

data_14$Saved<- data_14$saved
data_14$Borrowed<- data_14$borowed

data_14$emergency_possible<-ifelse(data_14$q24==1 | data_14$q24==2,1,0)


data_14$emergency_savings <- ifelse(data_14$q25==1,1,0)
data_14$emergency_family <- ifelse(data_14$q25==2,1,0)
data_14$emergency_working<- ifelse(data_14$q25==3,1,0)
data_14$emergency_local<- ifelse(data_14$q25==5,1,0  )
data_14$emergency_others<- ifelse(data_14$q25==6,1,0  )
data_14$emergency_formal_fi<- ifelse(data_14$q25==4 ,1,0  )

data_14$emergency_unplanned= data_14$emergency_local +data_14$emergency_family +data_14$emergency_others+data_14$emergency_formal_fi 

data_14$emergency_planned=data_14$emergency_savings+data_14$emergency_working

data_14$Keep<-ifelse(data_14$EM +data_14$Low>0,1,0)
data_14_Selected<-subset(data_14, Keep==1)



data_14_work<-data_14_Selected[,c(2,4, 10,85:90,92,94,95, 96, 97:101,103,110,111)]
#######2017 Data

###MSCI Emerging countries: 
#Brazil, Chile, China, Colombia, Czech Republic, Egypt, Greece, Hungary, India, Indonesia, 
##Korea, Malaysia, Mexico, Peru, Philippines, Poland, Russia, South Africa, Taiwan, Thailand, and Turkey.
##Additional: Kenya, Nigeria, Argentina
data_17$Year<-2017


data_17$EM<-ifelse(data_17$economycode=="BRA"|data_17$economycode=="RUS"|data_17$economycode== "IND" | data_17$economycode=="CHN"| 
                     data_17$economycode=="IDN"| data_17$economycode== "ZAF" | data_17$economycode=="TUR"| data_17$economycode== "COL"
                   |data_17$economycode=="MEX" | data_17$economycode== "TUN"
                   |data_17$economycode=="KEN"  |data_17$economycode=="ARG"  |data_17$economycode=="MAR"
                   |data_17$economycode=="PHL" |data_17$economycode=="THA" |data_17$economycode=="TWN" |data_17$economycode=="POL" |data_17$economycode=="PER"
                   |data_17$economycode=="MYS"|data_17$economycode=="KOR" |data_17$economycode=="HUN"|data_17$economycode=="TUN"|data_17$economycode=="VNM"
                   |data_17$economycode=="GRC" |data_17$economycode=="EGY" |data_17$economycode=="CZE",1,0)

data_17$Low<-ifelse(data_17$economycode=="AFG"| data_17$economycode=="BGD"| data_17$economycode=="BEN"|data_17$economycode=="BFA" |data_17$economycode=="BDI"
                    |data_17$economycode=="TCD"| data_17$economycode=="ZAR" |data_17$economycode=="COG" |data_17$economycode=="CIV" | data_17$economycode=="ETH"
               | data_17$economycode=="GHA"| data_17$economycode=="GIN" |data_17$economycode=="HTI" |data_17$economycode=="HND"
                    |data_17$economycode=="KGZ" |data_17$economycode=="MDG" | data_17$economycode=="MWI"|data_17$economycode=="MLI"| data_17$economycode=="MRT"
                    |data_17$economycode=="MMR"| data_17$economycode=="NPL"|data_17$economycode=="NER"|data_17$economycode=="NGA"|data_17$economycode=="PAK" 
                    |data_17$economycode=="RWA"|data_17$economycode=="SEN"|data_17$economycode=="SLE" |data_17$economycode=="SOM" |data_17$economycode=="LKA"
                    |data_17$economycode=="SDN"| data_17$economycode=="TJK" |data_17$economycode=="TZA"|data_17$economycode=="TGO"|data_17$economycode=="UGA"
                    |data_17$economycode=="WBG"|data_17$economycode=="YEM"|data_17$economycode=="ZMB"|data_17$economycode=="ZWE",1,0)



### Define Treatment and Control: Treatment anyone with bank account; Control reasons of non account is either "too far, no documents, religious reasons,”

data_17$treatment<-ifelse(data_17$account==1,1,0)
data_17$no_account<-ifelse(data_17$fin11a==1|data_17$fin11c==1|data_17$fin11e==1,1,0)
data_17$control<-ifelse(data_17$treatment==1,0, data_17$no_account)
data_17$Flag<-ifelse(data_17$treatment==1 | data_17$control==1,1,0)

####Subset of only Treatment and Control

data_17 <-data_17[data_17$Flag==1,]


######Other Controls
data_17$edu_primary<-ifelse(data_17$educ==1,1,0)
data_17$edu_secondary<-ifelse(data_17$educ==2,1,0)
data_17$inc_q<-data_17$inc_q

#####Wage and other receipts
data_17$received_wage<-ifelse(data_17$fin32==1,1,0)
data_17$received_wage_account<-ifelse(data_17$fin34a==1 | data_17$fin34b==1,1,0)
data_17$received_govt<-ifelse(data_17$fin37==1,1,0)
data_17$received_govt_account<-ifelse(data_17$fin39a==1 | data_17$fin39b==1,1,0)
data_17$received_agri<-ifelse(data_17$fin42==1,1,0)
data_17$received_agri_account<-ifelse(data_17$fin43a==1 | data_17$fin43b==1| data_17$fin43c2==1,1,0)

#####Financial Planning

data_17$Saved<- data_17$saved
data_17$Borrowed<- data_17$borowed

data_17$emergency_possible<-ifelse(data_17$fin24==1,1,0)


data_17$emergency_savings <- ifelse(data_17$fin25==1,1,0)
data_17$emergency_family <- ifelse(data_17$fin25==2,1,0)
data_17$emergency_working<- ifelse(data_17$fin25==3,1,0)
data_17$emergency_borrowing<- ifelse(data_17$fin25==4,1,0)
data_17$emergency_assetsale<- ifelse(data_17$fin25==5,1,0)
data_17$emergency_others<- ifelse(data_17$fin25==6,1,0  )

data_17$emergency_unplanned= data_17$emergency_family +data_17$emergency_others+data_17$emergency_borrowing+ data_17$emergency_assetsale

data_17$emergency_planned=data_17$emergency_savings+data_17$emergency_working

data_17$Keep<-ifelse(data_17$EM +data_17$Low>0,1,0)
data_17_Selected<-subset(data_17, Keep==1)



data_17_work<-data_17_Selected[,c(2,4,10,94,95,106,107, 108, 109, 111, 113,114,115:120,  122,129,130)]
#########


#######2021 Data

###MSCI Emerging countries: 
#Brazil, Chile, China, Colombia, Czech Republic, Egypt, Greece, Hungary, India, Indonesia, 
##Korea, Malaysia, Mexico, Peru, Philippines, Poland, Russia, South Africa, Taiwan, Thailand, and Turkey.
##Additional: Kenya, Nigeria, Argentina
data_21$Year<-2021
data_21$EM<-ifelse(data_21$economycode=="BRA"|data_21$economycode=="RUS"|data_21$economycode== "IND" | data_21$economycode=="CHN"| 
                     data_21$economycode=="IDN"| data_21$economycode== "ZAF" | data_21$economycode=="TUR"| data_21$economycode== "COL"
                   |data_21$economycode=="MEX" | data_21$economycode== "TUN"
                   |data_21$economycode=="KEN"  |data_21$economycode=="ARG"  |data_21$economycode=="MAR"
                   |data_21$economycode=="PHL" |data_21$economycode=="THA" |data_21$economycode=="TWN" |data_21$economycode=="POL" |data_21$economycode=="PER"
                   |data_21$economycode=="MYS"|data_21$economycode=="KOR" |data_21$economycode=="HUN"|data_21$economycode=="TUN"|data_21$economycode=="VNM"
                   |data_21$economycode=="GRC" |data_21$economycode=="EGY" |data_21$economycode=="CZE",1,0)

data_21$Low<-ifelse(data_21$economycode=="AFG"| data_21$economycode=="BGD"| data_21$economycode=="BEN"|data_21$economycode=="BFA" |data_21$economycode=="BDI"
                    |data_21$economycode=="TCD"| data_21$economycode=="ZAR" |data_21$economycode=="COG" |data_21$economycode=="CIV" | data_21$economycode=="ETH"
                     | data_21$economycode=="GHA"| data_21$economycode=="GIN" |data_21$economycode=="HTI" |data_21$economycode=="HND"
                    |data_21$economycode=="KGZ" |data_21$economycode=="MDG" | data_21$economycode=="MWI"|data_21$economycode=="MLI"| data_21$economycode=="MRT"
                    |data_21$economycode=="MMR"| data_21$economycode=="NPL"|data_21$economycode=="NER"|data_21$economycode=="NGA"|data_21$economycode=="PAK" 
                    |data_21$economycode=="RWA"|data_21$economycode=="SEN"|data_21$economycode=="SLE" |data_21$economycode=="SOM" |data_21$economycode=="LKA"
                    |data_21$economycode=="SDN"| data_21$economycode=="TJK" |data_21$economycode=="TZA"|data_21$economycode=="TGO"|data_21$economycode=="UGA"
                    |data_21$economycode=="WBG"|data_21$economycode=="YEM"|data_21$economycode=="ZMB"|data_21$economycode=="ZWE",1,0)


### Define Treatment and Control: Treatment anyone with bank account; Control reasons of non account is either "too far, no documents, religious reasons,”

data_21$treatment<-ifelse(data_21$account==1,1,0)
data_21$no_account<-ifelse(data_21$fin11a==1|data_21$fin11c==1 |data_21$fin11e==1,1,0)
data_21$control<-ifelse(data_21$treatment==1,0,data_21$no_account)
data_21$Flag<-ifelse(data_21$treatment==1 | data_21$control==1,1,0)

####Subset of only Treatment and Control

data_21 <-data_21[data_21$Flag==1,]


######Other Controls
data_21$edu_primary<-ifelse(data_21$educ==1,1,0)
data_21$edu_secondary<-ifelse(data_21$educ==2,1,0)
data_21$inc_q<-data_21$inc_q

#####Wage and other receipts
data_21$received_wage<-ifelse(data_21$fin32==1,1,0)
data_21$received_wage_account<-ifelse(data_21$fin34a==1 | data_21$fin34b==1 | data_21$fin34e==1,1,0)
data_21$received_govt<-ifelse(data_21$fin37==1,1,0)
data_21$received_govt_account<-ifelse(data_21$fin39a==1 | data_21$fin39b==1 | data_21$fin39e==1,1,0)
data_21$received_agri<-ifelse(data_21$fin42==1,1,0)
data_21$received_agri_account<-ifelse(data_21$fin43a==1 | data_21$fin43b==1| data_21$fin43e==1,1,0)

#####Financial Planning

data_21$Saved<- data_21$saved
data_21$Borrowed<- data_21$borowed

data_21$emergency_possible<-ifelse(data_21$fin24a==3,1,0)


data_21$emergency_savings <- ifelse(data_21$fin24==1,1,0)
data_21$emergency_family <- ifelse(data_21$fin24==2,1,0)
data_21$emergency_working<- ifelse(data_21$fin24==3,1,0)
data_21$emergency_borrowing<- ifelse(data_21$fin24==4,1,0)
data_21$emergency_assetsale<- ifelse(data_21$fin24==5,1,0)
data_21$emergency_none<- ifelse(data_21$fin24==6,1,0)
data_21$emergency_others<- ifelse(data_21$fin24==7,1,0  )

data_21$emergency_unplanned= data_21$emergency_none+data_21$emergency_family +data_21$emergency_others+data_21$emergency_borrowing+ data_21$emergency_assetsale 

data_21$emergency_planned=data_21$emergency_savings+data_21$emergency_working

data_21$Keep<-ifelse(data_21$EM +data_21$Low>0,1,0)
data_21_Selected<-subset(data_21, Keep==1)


data_21_work<-data_21_Selected[,c(2,4,10,116,117,129,130,131,132,134, 136:143,145,153,154)]



#####ALL ROUND DATA merged
All_data<-rbind(data_14_work,data_17_work,data_21_work)
All_data$logpop<-log(All_data$pop_adult)
All_data$saved<-ifelse(All_data$saved==1,1,0)
All_data$borrowed<-ifelse(All_data$borrowed==1,1,0)

summary(All_data$emergency_planned)
summary(All_data$emergency_unplanned)
summary(All_data$emergency_possible)
#########
#install.packages("fastDummies")
library(fastDummies)
results <- fastDummies::dummy_cols(All_data$economycode)
All<-cbind(All_data,results)

######RESULTS
emergency.unplanned <- emergency_unplanned~ logpop+Year+treatment + edu_primary+  inc_q+ received_agri+received_govt+received_wage+  EM 
emergency.planned<- emergency_planned~ logpop+Year+treatment + edu_primary+  inc_q+ received_agri+received_govt+received_wage+  EM
emergency.possible<- emergency_possible~ logpop+Year+treatment + edu_primary+  inc_q+ received_agri+received_govt+received_wage+  EM


reg_unplanned_all<- glm(emergency.unplanned ,family=binomial, data = All_data)
vif(reg_unplanned_all)
summary(reg_unplanned_all)

reg_planned_all<- glm(emergency.planned ,family=binomial, data = All_data)
vif(reg_planned_all)
summary(reg_planned_all)

reg_possible_all<- glm(emergency.unplanned ,family=binomial, data = All_data)
vif(reg_possible_all)
summary(reg_possible_all)



########
###EM Country Dummies
# .data_ARG+ .data_BRA+.data_HUN+.data_EGY+.data_COL+.data_IND+.data_IDN+.data_PHL+.data_VNM+.data_MEX+.data_KEN+.data_MAR+.data_POL+.data_TUN+.data_ZAF+.data_PER+.data_TUR+.data_RUS




#####Individual Countries



###IND
data.IND<-subset(All_data, All_data$economycode=="IND")
summary(data.IND$treatment)
summary(data.IND$control)
summary(data.IND$emergency_unplanned)
summary(data.IND$emergency_planned)
summary(data.IND$emergency_possible)



emergency.unplanned.IND<- emergency_unplanned~ Year+treatment + edu_primary+  inc_q+ received_agri+received_govt+received_wage 
emergency.planned.IND<- emergency_planned~ Year+treatment + edu_primary+  inc_q+ received_agri+received_govt+received_wage
emergency.possible.IND<- emergency_possible~ Year+treatment + edu_primary+  inc_q+ received_agri+received_govt+received_wage

reg_possible_IND<- glm(emergency.possible.IND,family=binomial, data = data.IND)
vif(reg_possible_IND)
summary(reg_possible_IND)
    
reg_planned_IND<- glm(emergency.planned.IND ,family=binomial, data = data.IND)
vif(reg_planned_IND)
summary(reg_planned_IND)

coef<- unname(coef(reg_planned_IND)[3])
        
reg_unplanned_IND<- glm(emergency.unplanned.IND ,family=binomial, data = data.IND)
vif(reg_unplanned_IND)
summary(reg_unplanned_IND)


### PSM
country <- results[,-1]
colnames(country)


match_it <- matchit(treatment ~ edu_primary + edu_secondary + inc_q,
                    data = All_data,
                    method = "nearest", 
                    distance = "logit",
                    ratio = 1,
                    exact = ~ economycode + Year)

summary(match_it)
summary(match_it, standardize = TRUE)
#Extracted Matched Data Set
matched_data <- match.data(match_it)





#####Individual Countries


country_codes <- unique(matched_data$economycode)

data <- data.frame("country" = c(""),
                   "unplanned" = c(""),
                   "p_unplanned" = c(""),
                   "planned" = c(""),
                   "p_planned" = c(""),
                   "possible" = c(""),
                   "p_possible"= c(""))


for(c in country_codes){
    dat <- subset(matched_data, matched_data$economycode == c)
 
    emergency.unplanned <- emergency_unplanned~   treatment + edu_primary+  inc_q+ received_agri+received_govt+received_wage 
    emergency.planned <- emergency_planned~  treatment + edu_primary+  inc_q+ received_agri+received_govt+received_wage
    emergency.possible <- emergency_possible~ +treatment + edu_primary+  inc_q+ received_agri+received_govt+received_wage
  
    reg_possible <- glm(emergency.possible ,family=binomial, data = dat)
    reg_planned<- glm(emergency.planned ,family=binomial, data = dat)
    reg_unplanned<- glm(emergency.unplanned ,family=binomial, data = dat)
    
    coef_possible <- unname(coef(reg_possible)[2])
    coef_planned <- unname(coef(reg_planned)[2])
    coef_unplanned <- unname(coef(reg_unplanned)[2])
    
    pvalue_pos <- unname(ifelse(summary(reg_possible)$coef[,4][2] < 0.05,1,0))
    pvalue_plan <- unname(ifelse(summary(reg_planned)$coef[,4][2] < 0.05,1,0))
    pvalue_unpl <- unname(ifelse(summary(reg_unplanned)$coef[,4][2] < 0.05,1,0))
    
    help <- data.frame("country" = c,
                       "unplanned" = exp(coef_unplanned)/(1+ exp(coef_unplanned)),
                       "p_unplanned" = pvalue_unpl,
                       "planned" = exp(coef_planned)/(1+ exp(coef_planned)),
                       "p_planned" = pvalue_plan,
                       "possible" = exp(coef_possible)/(1+ exp(coef_possible)),
                       "p_possible"= pvalue_pos)
    
    data <- rbind(data,help)
    
}

data <- data[-1,]


