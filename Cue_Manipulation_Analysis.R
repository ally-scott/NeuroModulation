################
## Cue Manipulation
## 2023-02-23
## Bailey Newell
##
################

## Set Working Directory
  setwd("C:/Users/ally/OneDrive/Desktop/VPopto/")

## Read in necessary packages #####
  library(ggplot2)
  library(reshape2)
  library(dplyr)
  library(gridExtra)
  library(faraway)
  library(tidyverse)
  library(plotrix)
  library(stats)
  library(readxl) ## for M/N data conversion
  library(openxlsx) ## Converting things back into excel files
  library(Hmisc)
  library(extrafont)

#### select working data frame ####
  library(readxl)
  lab_meeting_cue_manipulation <- read_excel("lab_meeting_cue_manipulation.xlsx", 
                                           sheet = "all_data")
  info<- read_excel("GVO Full Group.xlsx")

  daybefore<-read_excel("daybeforestim.xlsx")
  
VPnoopto<-merge(x = daybefore, y = info[ , c("subject","virus", "Exclude")], by = "subject", all.x=TRUE)  
VPoto<- merge(x = lab_meeting_cue_manipulation, y = info[ , c("subject","virus", "Exclude")], by = "subject", all.x=TRUE)
names<-colnames(VPoto[116:175])



DSNSnostimstatus_trial=select(VPnoopto,1:5,2915)#(m=ds,n=ns)
DSNSnostimstatus_trial$stimulationtype<-'none'
DSNSnostimstatus_trial$duration<-'0'
DSNSnostimstatus_trial[,names] <-'0'


DSNSnostimlatarray_trial=select(VPnoopto,1:5,2915,1603:1632,1635:1664)#(m=ds,n=ns)
DSNSnostimlatarray_trial$stimulationtype<-'none'
DSNSnostimlatarray_trial$duration<-'0'
##add stim and dur columns


DSNSstimstatusarray_trial=select(VPoto,1:7,2925,116:175)#(E=ds,F=ns)
DSNSlatarray_trial=select(VPoto,1:7,2925,1746:1775,1778:1807)#(m=ds,n=ns)

DSNSstimstatusarray_trial=rbind(x =DSNSstimstatusarray_trial , y = DSNSnostimstatus_trial)

DSNSlatarray_trial<-rbind(x =DSNSlatarray_trial , y = DSNSnostimlatarray_trial)
#need to find a way to get DS ratio of stim vs. non-stim trials 
#### grouping all data #### 
#arrays for 10sec ratio###
stimarray<- DSNSstimstatusarray_trial%>%
  mutate(subject = `subject`,
         session=`startdate`,
         virus = `virus.x`,
         sex = sex,
         stim_type = `stimulationtype`,
         stim_dur= `duration`) %>% 
  select(subject,session, virus, sex, stim_type,stim_dur,Exclude)

stimarray<-DSNSstimstatusarray_trial|>
  pivot_longer(cols=9:68,
               names_to='trial',
               values_to='stim') %>%
  mutate(cuetype = case_when(
    grepl("E", trial) ~ "DS",
    grepl("F", trial) ~ "NS",
    TRUE ~ "WEEOOOOWEEEEOOOOO"
  ),
  trial
  )

stimframe<- stimarray %>%
  mutate(trial= parse_number(`trial`))

latarray<- DSNSlatarray_trial%>%
  mutate(subject = `subject`,
         session=`startdate`,
         virus = `virus.x`,
         sex = sex,
         stim_type = `stimulationtype`,
         stim_dur= `duration`) %>% 
  select(subject,session, virus, sex, stim_type,stim_dur,Exclude)

latarray<-DSNSlatarray_trial|>
  pivot_longer(cols = 9:68,
               names_to='trial',
               values_to='lat') %>%
  mutate(cuetype = case_when(
    grepl("M", trial) ~ "DS",
    grepl("N", trial) ~ "NS",
    TRUE ~ "WEEOOOOWEEEEOOOOO"
  ),
  trial
  )

latframe<- latarray %>%
  mutate(trial= parse_number(`trial`))
latframe$PE <- ifelse(latframe$lat >= 0,"1", "0")
latframe$PE <- ifelse(latframe$lat < 10,"1", "0")

 VPopto<-merge(x = stimframe, y = latframe, by = c("subject", "trial","startdate","box","sex","virus.x","stimulationtype","duration","cuetype","Exclude"), all.x=TRUE)%>%
   mutate(virus = `virus.x`,
          session=`startdate`,
          stim_type=`stimulationtype`,
          stim_dur=`duration`,
          cue=`cuetype`) %>% 
   filter(stim_type!='post-test',stim_dur!='post-test',is.na(Exclude))%>%
   select(subject,trial,session, virus, sex, stim_type,stim_dur,cue,stim,lat,PE)
 
 
 #arrays for 1sec ratio###
 
 latframe1sec<- latarray %>%
   mutate(trial= parse_number(`trial`))
 latframe1sec$PE <- ifelse(latframe1sec$lat >= 0,"1", "0")
 latframe1sec$PE <- ifelse(latframe1sec$lat < 1,"1", "0")
 
 
 
 ### grouping arrays for full data set####
 VPopto<-merge(x = stimframe, y = latframe, by = c("subject", "trial","startdate","box","sex","virus.x","stimulationtype","duration","cuetype","Exclude"), all.x=TRUE)%>%
   mutate(virus = `virus.x`,
          session=`startdate`,
          stim_type=`stimulationtype`,
          stim_dur=`duration`,
          cue=`cuetype`) %>% 
   filter(stim_type!='post-test',stim_dur!='post-test',is.na(Exclude))%>%
   select(subject,trial,session, virus, sex, stim_type,stim_dur,cue,stim,lat,PE)
 
 VPopto1secPE<-merge(x = stimframe, y = latframe1sec, by = c("subject", "trial","startdate","box","sex","virus.x","stimulationtype","duration","cuetype","Exclude"), all.x=TRUE)%>%
   mutate(virus = `virus.x`,
          session=`startdate`,
          stim_type=`stimulationtype`,
          stim_dur=`duration`,
          cue=`cuetype`) %>% 
   filter(stim_type!='post-test',stim_dur!='post-test',is.na(Exclude))%>%
   select(subject,trial,session, virus, sex, stim_type,stim_dur,cue,stim,lat,PE)
 

## Sub-setting data by virus type for graph making, finding means and sem for PEratio and PElatency
##### INHIBITION GROUP AVG DATA ########    
 inhibition_group<- VPopto[VPopto$virus=='stgtacr',]
 inhibition_group$totaltrial<-ifelse(inhibition_group$stim_dur>0,"15","30")
 
 inhibition_group1secPE<-  VPopto1secPE[VPopto1secPE$virus=='stgtacr',]
 inhibition_group1secPE$totaltrial<-ifelse(inhibition_group1secPE$stim_dur>0,"15","30")
###########################PEratio 
inhib_avgPE<-inhibition_group%>% 
  group_by(subject,as.factor(session),cue,stim_dur,stim) %>%
 summarise(trialPE=sum(as.numeric(PE)),totaltrial=mean(as.numeric(totaltrial))) 

inhib_avgPE1sec<-inhibition_group1secPE%>% 
  group_by(subject,as.factor(session),cue,stim_dur,stim) %>%
  summarise(trialPE=sum(as.numeric(PE)),totaltrial=mean(as.numeric(totaltrial))) 

#avgPE
inhib_avgPE<-inhib_avgPE%>% 
  group_by(subject,`as.factor(session)`,cue,stim_dur,stim) %>%
  summarise(PEratio=trialPE/totaltrial)

inhib_avgPE1sec<-inhib_avgPE1sec%>% 
  group_by(subject,`as.factor(session)`,cue,stim_dur,stim) %>%
  summarise(PEratio=trialPE/totaltrial)

#across sessions
inhib_avgPE<-inhib_avgPE%>% 
  group_by(subject,cue,stim_dur,stim) %>% 
summarise(meanPEratio=mean(PEratio))

inhib_avgPE1sec<-inhib_avgPE1sec%>% 
  group_by(subject,cue,stim_dur,stim) %>% 
  summarise(meanPEratio=mean(PEratio))
##bargraph values
inhib_avgPEbar<-inhib_avgPE%>% 
  group_by(cue,stim_dur,stim) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())),meanPEratio)

inhib_avgPE1secbar<-inhib_avgPE1sec%>% 
  group_by(cue,stim_dur,stim) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())),meanPEratio)

############################latency
inhib_avglat<-inhibition_group%>% 
  group_by(subject,as.factor(session),cue,stim_dur,stim) %>%
  summarise(lat=mean(as.numeric(lat))) %>%
  arrange(subject)  

inhib_avglat1sec<-inhibition_group1secPE%>% 
  group_by(subject,as.factor(session),cue,stim_dur,stim) %>%
  summarise(lat=mean(as.numeric(lat))) %>%
  arrange(subject)  

#across session
inhib_avglat<-inhib_avglat%>% 
  group_by(subject,cue,stim_dur,stim) %>%
  summarise(latmean=mean(lat))

inhib_avglat1sec<-inhib_avglat1sec%>% 
  group_by(subject,cue,stim_dur,stim) %>%
  summarise(latmean=mean(lat))

#across subject
inhib_avglat_bar<-inhib_avglat%>% 
  group_by(cue,stim_dur,stim) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())),latmean)

inhib_avglat1sec_bar<-inhib_avglat1sec%>% 
  group_by(cue,stim_dur,stim) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())),latmean)
####CONTROL AVG DATA######
  
control_group <- VPopto[VPopto$virus=='DIO-mCherry',]
control_group$totaltrial<-ifelse(control_group$stim_dur>0,"15","30")

control_group1secPE<-  VPopto1secPE[VPopto1secPE$virus=='DIO-mCherry',]
control_group1secPE$totaltrial<-ifelse(control_group1secPE$stim_dur>0,"15","30")
###########################PEratio 
con_avgPE<-control_group%>% 
  group_by(subject,as.factor(session),cue,stim_dur,stim) %>%
  summarise(trialPE=sum(as.numeric(PE)),totaltrial=mean(as.numeric(totaltrial)))

con_avgPE1sec<-control_group1secPE%>% 
  group_by(subject,as.factor(session),cue,stim_dur,stim) %>%
  summarise(trialPE=sum(as.numeric(PE)),totaltrial=mean(as.numeric(totaltrial))) 

#avgPE session
con_avgPE<-con_avgPE%>% 
  group_by(subject,cue,stim_dur,stim) %>%
  summarise(PEratio=trialPE/totaltrial)

con_avgPE1sec<-con_avgPE1sec%>% 
  group_by(subject,cue,stim_dur,stim) %>%
  summarise(PEratio=trialPE/totaltrial)

#across sessions
con_avgPE<-con_avgPE%>% 
  group_by(subject,cue,stim_dur,stim) %>% 
  summarise(meanPEratio=mean(PEratio))

con_avgPE1sec<-con_avgPE1sec%>% 
  group_by(subject,cue,stim_dur,stim) %>% 
  summarise(meanPEratio=mean(PEratio))

#across subject
con_avgPE_bar<-con_avgPE%>% 
  group_by(cue,stim_dur,stim) %>% 
  summarise_each(funs(mean,se=sd(.)/sqrt(n())),meanPEratio)

con_avgPE1sec_bar<-con_avgPE1sec%>% 
  group_by(cue,stim_dur,stim) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())),meanPEratio)

############################latency
con_avglat<-control_group%>% 
  group_by(subject,as.factor(session),cue,stim_dur,stim) %>%
  summarise(lat=mean(as.numeric(lat))) %>%
  arrange(subject)

con_avglat1sec<-control_group1secPE%>% 
  group_by(subject,as.factor(session),cue,stim_dur,stim) %>%
  summarise(lat=mean(as.numeric(lat))) %>%
  arrange(subject)  

#across session
con_avglat<-con_avglat%>% 
  group_by(subject,cue,stim_dur,stim) %>%
  summarise(latmean=mean(lat))

con_avglat1sec<-con_avglat1sec%>% 
  group_by(subject,cue,stim_dur,stim) %>%
  summarise(latmean=mean(lat))

#across subject
con_avglat_bar<-con_avglat%>% 
  group_by(cue,stim_dur,stim) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())),latmean)

con_avglat1sec_bar<-con_avglat1sec%>% 
  group_by(cue,stim_dur,stim) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())),latmean)

#### STATS ####
library(lme4)
library(lmerTest)
library(emmeans)
### combined lmes ####
##inhibiton lme PE
inhibPE_model <- lmer(meanPEratio ~ as.factor(stim_dur) * as.factor(stim) *cue + (1 | subject), data = inhib_avgPE%>% filter(stim_dur == 1 | stim_dur==10))
inhibPE_model_anova <-anova(inhibPE_model)
capture.output(inhibPE_model_anova, file = "inhibPE_model_anova_results.doc")

inhibPEin1sec_model <- lmer(meanPEratio ~ as.factor(stim_dur) * as.factor(stim) *cue + (1 | subject), data = inhib_avgPE1sec%>% filter(stim_dur == 1 | stim_dur==10))
inhibPEin1sec_model_anova <-anova(inhibPEin1sec_model)
capture.output(inhibPEin1sec_model_anova, file = "inhibPEin1sec_model_anova_results.doc")

##control lme PE
conPE_model <- lmer(meanPEratio ~ as.factor(stim_dur) * as.factor(stim) *cue + (1 | subject), data = con_avgPE%>% filter(stim_dur == 1 | stim_dur==10))
conPE_model_anova <-anova(conPE_model)
capture.output(conPE_model_anova, file = "conPE_model_anova_results.doc")

## control lme lat
conlat_model <- lmer(latmean~ as.factor(stim_dur) * as.factor(stim) *cue + (1 | subject), data = con_avglat%>% filter(stim_dur == 1 | stim_dur==10))
conlat_model_anova <-anova(conlat_model)
capture.output(conlat_model_anova, file = "conlat_model_anova_results.doc")




#### separate lmes for 1 and 10 sec duration ####
### INHIBITION ####
  ##PE in 1 sec
inhibPEin1sec_1secstimdur_model <- lmer(meanPEratio ~ as.factor(stim) *cue + (1 | subject), data = inhib_avgPE1sec%>% filter(stim_dur == 1 ))
inhibPEin1sec_1secstimdur_model_anova <-anova(inhibPEin1sec_1secstimdur_model)
capture.output(inhibPEin1sec_1secstimdur_model_anova, file = "inhibPEin1sec_1secstimdur_model_anova_results.doc")

inhibPEin1sec_10secstimdur_model <- lmer(meanPEratio ~ as.factor(stim) *cue + (1 | subject), data = inhib_avgPE1sec%>% filter(stim_dur == 10 ))
inhibPEin1sec_10secstimdur_model_anova <-anova(inhibPEin1sec_10secstimdur_model)
capture.output(inhibPEin1sec_10secstimdur_model_anova, file = "inhibPEin1sec_10secstimdur_model_anova_results.doc")
  ##PE in 10 sec
inhibPEin10sec_1secstimdur_model <- lmer(meanPEratio ~ as.factor(stim) *cue + (1 | subject), data = inhib_avgPE%>% filter(stim_dur == 1 ))
inhibPEin10sec_1secstimdur_model_anova <-anova(inhibPEin10sec_1secstimdur_model)
capture.output(inhibPEin10sec_1secstimdur_model_anova, file = "inhibPEin10sec_1secstimdur_model_anova_results.doc")

inhibPEin10sec_10secstimdur_model <- lmer(meanPEratio ~ as.factor(stim) *cue + (1 | subject), data = inhib_avgPE%>% filter(stim_dur == 10 ))
inhibPEin10sec_10secstimdur_model_anova <-anova(inhibPEin10sec_10secstimdur_model)
capture.output(inhibPEin10sec_10secstimdur_model_anova, file = "inhibPEin10sec_10secstimdur_model_anova_results.doc")

##inhibition lme lat
  ##PE in 1 sec
inhibPEin1sec_1secstimdur_latmodel <- lmer(latmean ~ as.factor(stim) *cue + (1 | subject), data = inhib_avglat1sec%>% filter(stim_dur == 1 ))
inhibPEin1sec_1secstimdur_latmodel_anova <-anova(inhibPEin1sec_1secstimdur_latmodel)
capture.output(inhibPEin1sec_1secstimdur_latmodel_anova, file = "inhibPEin1sec_1secstimdur_latmodel_anova_results.doc")

inhibPEin1sec_10secstimdur_latmodel <- lmer(latmean ~ as.factor(stim) *cue + (1 | subject), data = inhib_avglat1sec%>% filter(stim_dur == 10 ))
inhibPEin1sec_10secstimdur_latmodel_anova <-anova(inhibPEin1sec_10secstimdur_latmodel)
capture.output(inhibPEin1sec_10secstimdur_latmodel_anova, file = "inhibPEin1sec_10secstimdur_latmodel_anova_results.doc")
  ##PE in 10 sec
inhibPEin10sec_1secstimdur_latmodel <- lmer(latmean ~ as.factor(stim) *cue + (1 | subject), data = inhib_avglat%>% filter(stim_dur == 1 ))
inhibPEin10sec_1secstimdur_latmodel_anova <-anova(inhibPEin10sec_1secstimdur_latmodel)
capture.output(inhibPEin10sec_1secstimdur_latmodel_anova, file = "inhibPEin10sec_1secstimdur_latmodel_anova_results.doc")

inhibPEin10sec_10secstimdur_latmodel <- lmer(latmean ~ as.factor(stim) *cue + (1 | subject), data = inhib_avglat%>% filter(stim_dur == 10 ))
inhibPEin10sec_10secstimdur_latmodel_anova <-anova(inhibPEin10sec_10secstimdur_latmodel)
capture.output(inhibPEin10sec_10secstimdur_latmodel_anova, file = "inhibPEin10sec_10secstimdur_latmodel_anova_results.doc")

### CONTROL ####
##PE in 1 sec
conPEin1sec_1secstimdur_model <- lmer(meanPEratio ~ as.factor(stim) *cue + (1 | subject), data = con_avgPE1sec%>% filter(stim_dur == 1 ))
conPEin1sec_1secstimdur_model_anova <-anova(conPEin1sec_1secstimdur_model)
capture.output(conPEin1sec_1secstimdur_model_anova, file = "control_PEin1sec_1secstimdur_model_anova_results.doc")

conPEin1sec_10secstimdur_model <- lmer(meanPEratio ~ as.factor(stim) *cue + (1 | subject), data = con_avgPE1sec%>% filter(stim_dur == 10 ))
conPEin1sec_10secstimdur_model_anova <-anova(conPEin1sec_10secstimdur_model)
capture.output(conPEin1sec_10secstimdur_model_anova, file = "control_PEin1sec_10secstimdur_model_anova_results.doc")
##PE in 10 sec
conPEin10sec_1secstimdur_model <- lmer(meanPEratio ~ as.factor(stim) *cue + (1 | subject), data = con_avgPE%>% filter(stim_dur == 1 ))
conPEin10sec_1secstimdur_model_anova <-anova(conPEin10sec_1secstimdur_model)
capture.output(conPEin10sec_1secstimdur_model_anova, file = "control_PEin10sec_1secstimdur_model_anova_results.doc")

conPEin10sec_10secstimdur_model <- lmer(meanPEratio ~ as.factor(stim) *cue + (1 | subject), data = con_avgPE%>% filter(stim_dur == 10 ))
conPEin10sec_10secstimdur_model_anova <-anova(conPEin10sec_10secstimdur_model)
capture.output(conPEin10sec_10secstimdur_model_anova, file = "control_PEin10sec_10secstimdur_model_anova_results.doc")

##lme lat
##PE in 1 sec
conPEin1sec_1secstimdur_latmodel <- lmer(latmean ~ as.factor(stim) *cue + (1 | subject), data = con_avglat1sec%>% filter(stim_dur == 1 ))
conPEin1sec_1secstimdur_latmodel_anova <-anova(conPEin1sec_1secstimdur_latmodel)
capture.output(conPEin1sec_1secstimdur_latmodel_anova, file = "control_PEin1sec_1secstimdur_latmodel_anova_results.doc")

conPEin1sec_10secstimdur_latmodel <- lmer(latmean ~ as.factor(stim) *cue + (1 | subject), data = con_avglat1sec%>% filter(stim_dur == 10 ))
conPEin1sec_10secstimdur_latmodel_anova <-anova(conPEin1sec_10secstimdur_latmodel)
capture.output(conPEin1sec_10secstimdur_latmodel_anova, file = "control_PEin1sec_10secstimdur_latmodel_anova_results.doc")
##PE in 10 sec
conPEin10sec_1secstimdur_latmodel <- lmer(latmean ~ as.factor(stim) *cue + (1 | subject), data = con_avglat%>% filter(stim_dur == 1 ))
conPEin10sec_1secstimdur_latmodel_anova <-anova(conPEin10sec_1secstimdur_latmodel)
capture.output(conPEin10sec_1secstimdur_latmodel_anova, file = "control_PEin10sec_1secstimdur_latmodel_anova_results.doc")

conPEin10sec_10secstimdur_latmodel <- lmer(latmean ~ as.factor(stim) *cue + (1 | subject), data = con_avglat%>% filter(stim_dur == 10 ))
conPEin10sec_10secstimdur_latmodel_anova <-anova(conPEin10sec_10secstimdur_latmodel)
capture.output(conPEin10sec_10secstimdur_latmodel_anova, file = "control_PEin10sec_10secstimdur_latmodel_anova_results.doc")



##follow up comparisons on inhibition data
## within stim_dur groups 1st


# 1 and 10 sec stim
#DS-DS and NS-NS
#PEratio lme####

###1secPEratio 1sec stim duration lme###
eminhibPE1secratio_1secstimdur <- emmeans(inhibPEin1sec_1secstimdur_model,specs =  ~  as.factor(stim) *cue )
# from grid in emmeans 
DS1secPEoff_1secdur<- c(1,0,0,0)
DS1secPEon_1secdur<- c(0,1,0,0)

NS1secPEoff_1secdur<- c(0,0,1,0)
NS1secPEon_1secdur<- c(0,0,0,1)

inhib1secPEratiocon<-contrast(eminhibPE1secratio_1secstimdur ,method = list("DS1secPE stim off - DS1secPE stim on" = DS1secPEoff_1secdur - DS1secPEon_1secdur,
                                                        "NS1secPE stim off - NS1secPE stim on" = NS1secPEoff_1secdur -NS1secPEon_1secdur))
capture.output(inhib1secPEratiocon, file = "inhib1secPEratio_1secdur_pairwise_results.doc")

###10secPEratio 1sec stim duration lme###
eminhibPE10secratio_1secstimdur <- emmeans(inhibPEin10sec_1secstimdur_model,specs =  ~  as.factor(stim) *cue )
# from grid in emmeans 
DS10secPEoff_1secdur<- c(1,0,0,0)
DS10secPEon_1secdur<- c(0,1,0,0)

NS10secPEoff_1secdur<- c(0,0,1,0)
NS10secPEon_1secdur<- c(0,0,0,1)

inhib10secPEratiocon<-contrast(eminhibPE10secratio_1secstimdur ,method = list("DS10secPE stim off - DS10secPE stim on" = DS10secPEoff_1secdur - DS10secPEon_1secdur,
                                                                            "NS10secPE stim off - NS10secPE stim on" = NS10secPEoff_1secdur -NS10secPEon_1secdur))
capture.output(inhib10secPEratiocon, file = "inhib10secPEratio_1secdur_pairwise_results.doc")
###10secPEratio 10sec stim duration lme###
eminhibPE10secratio_10secstimdur <- emmeans(inhibPEin10sec_10secstimdur_model,specs =  ~  as.factor(stim) *cue )
# from grid in emmeans 
DS10secPEoff_10secdur<- c(1,0,0,0)
DS10secPEon_10secdur<- c(0,1,0,0)

NS10secPEoff_10secdur<- c(0,0,1,0)
NS10secPEon_10secdur<- c(0,0,0,1)

inhib10secPEratiocon<-contrast(eminhibPE10secratio_10secstimdur ,method = list("DS10secPE stim off - DS10secPE stim on" = DS10secPEoff_10secdur - DS10secPEon_10secdur,
                                                                            "NS10secPE stim off - NS10secPE stim on" = NS10secPEoff_10secdur -NS10secPEon_10secdur))
capture.output(inhib10secPEratiocon, file = "inhib10secPEratio_10secdur_pairwise_results.doc")

#lat lme####  
###1secPEratio 1sec stim duration lme###
eminhibPE1secratio_1secstimdur_lat <- emmeans(inhibPEin1sec_1secstimdur_latmodel,specs =  ~  as.factor(stim) *cue )
# from grid in emmeans 
DS1secPEoff_1secdur<- c(1,0,0,0)
DS1secPEon_1secdur<- c(0,1,0,0)

NS1secPEoff_1secdur<- c(0,0,1,0)
NS1secPEon_1secdur<- c(0,0,0,1)

inhib1secPEratiocon_lat<-contrast(eminhibPE1secratio_1secstimdur_lat ,method = list("DS1secPE stim off - DS1secPE stim on" = DS1secPEoff_1secdur - DS1secPEon_1secdur,
                                                                            "NS1secPE stim off - NS1secPE stim on" = NS1secPEoff_1secdur -NS1secPEon_1secdur))
capture.output(inhib1secPEratiocon_lat, file = "inhib1secPEratio_1secdur_lat_pairwise_results.doc")

###10secPEratio 1sec stim duration lme###
eminhibPE10secratio_1secstimdur_lat <- emmeans(inhibPEin10sec_1secstimdur_latmodel,specs =  ~  as.factor(stim) *cue )
# from grid in emmeans 
DS10secPEoff_1secdur<- c(1,0,0,0)
DS10secPEon_1secdur<- c(0,1,0,0)

NS10secPEoff_1secdur<- c(0,0,1,0)
NS10secPEon_1secdur<- c(0,0,0,1)

inhib10secPEratiocon_lat<-contrast(eminhibPE10secratio_1secstimdur_lat ,method = list("DS10secPE stim off - DS10secPE stim on" = DS10secPEoff_1secdur - DS10secPEon_1secdur,
                                                                                    "NS10secPE stim off - NS10secPE stim on" = NS10secPEoff_1secdur -NS10secPEon_1secdur))
capture.output(inhib10secPEratiocon_lat, file = "inhib10secPEratio_1secdur_lat_pairwise_results.doc")

###10secPEratio 10sec stim duration lme###
eminhibPE10secratio_10secstimdur_lat <- emmeans(inhibPEin10sec_10secstimdur_latmodel,specs =  ~  as.factor(stim) *cue )
# from grid in emmeans 
DS10secPEoff_10secdur<- c(1,0,0,0)
DS10secPEon_10secdur<- c(0,1,0,0)

NS10secPEoff_10secdur<- c(0,0,1,0)
NS10secPEon_10secdur<- c(0,0,0,1)

inhib10secPEratiocon_lat<-contrast(eminhibPE10secratio_10secstimdur_lat ,method = list("DS10secPE stim off - DS10secPE stim on" = DS10secPEoff_10secdur - DS10secPEon_10secdur,
                                                                             "NS10secPE stim off - NS10secPE stim on" = NS10secPEoff_10secdur -NS10secPEon_10secdur))
capture.output(inhib10secPEratiocon_lat, file = "inhib10secPEratio_10secdur_lat_pairwise_results.doc")

#### Making graphs (by virus) to convey mean response ratios and PE latency####
 
 library(ggplot2)
 
 ggplot(inhib_avgPE, aes(x=reorder(factor(cue), stim), y=meanPEratio,color=as.factor(stim),fill=as.factor(cue)))+
   stat_summary(fun = "mean", geom = "bar",position = position_dodge(width=1.00),size=1)+
   scale_fill_manual(values=c("#3182bd","grey"))+ 
   scale_color_manual(values=c("black","red"))+ 
   #geom_line(size = 1,alpha=0.2,position = position_dodge(width=0.90))+
   geom_point(size = 3,alpha=0.2,position = position_jitterdodge(
     jitter.width = 0.25,
     jitter.height = 0,
     dodge.width = 0.90,
     seed = NA))+
   stat_summary(fun.data = mean_se,geom = "errorbar", size = 1,position = position_dodge(width=1.00))+
   labs(title="Inhibiton PE Ratio",x ="stim_duration", y = "PE ratio ")+ 
   theme_classic()+ 
   theme( axis.line = element_line(colour = "black", size = 1, linetype = "solid"))+
   theme( axis.ticks = element_line(colour = "black", size =1, linetype = "solid")) +
   facet_grid(.~stim_dur)
 ggsave("Inhibition_PEratio.pdf")
 
 ggplot(inhib_avglat, aes(x=reorder(factor(cue), stim), y=latmean,color=as.factor(stim),fill=as.factor(cue)))+
   stat_summary(fun = "mean", geom = "bar",position = position_dodge(width=1.00),size=1)+
   scale_fill_manual(values=c("#3182bd","grey"))+ 
   scale_color_manual(values=c("black","red"))+ 
   #geom_line(size = 1,alpha=0.2,position = position_dodge(width=0.90))+
   geom_point(size = 3,alpha=0.2,position = position_jitterdodge(
     jitter.width = 0.25,
     jitter.height = 0,
     dodge.width = 0.90,
     seed = NA))+
   stat_summary(fun.data = mean_se,geom = "errorbar", size = 1,position = position_dodge(width=1.00))+
   labs(title="Inhibiton PE latency",x ="stim_duration", y = "PE latency(sec) ")+ 
   theme_classic()+ 
   theme( axis.line = element_line(colour = "black", size = 1, linetype = "solid"))+
   theme( axis.ticks = element_line(colour = "black", size =1, linetype = "solid")) +
   facet_grid(.~stim_dur)
 ggsave("Inhibition_lat.pdf")
 
 ## CONTROL GRAPH
 
 ggplot(con_avgPE, aes(x=reorder(factor(cue), stim), y=meanPEratio,color=as.factor(stim),fill=as.factor(cue)))+
   stat_summary(fun = "mean", geom = "bar",position = position_dodge(width=1.00),size=1)+
   scale_fill_manual(values=c("#3182bd","grey"))+ 
   scale_color_manual(values=c("black","red"))+ 
   #geom_line(size = 1,alpha=0.2,position = position_dodge(width=0.90))+
   geom_point(size = 3,alpha=0.2,position = position_jitterdodge(
     jitter.width = 0.25,
     jitter.height = 0,
     dodge.width = 0.90,
     seed = NA))+
   stat_summary(fun.data = mean_se,geom = "errorbar", size = 1,position = position_dodge(width=1.00))+
   labs(title="Control PE Ratio",x ="stim_duration", y = "PE ratio ")+ 
   theme_classic()+ 
   theme( axis.line = element_line(colour = "black", size = 1, linetype = "solid"))+
   theme( axis.ticks = element_line(colour = "black", size =1, linetype = "solid")) +
   facet_grid(.~stim_dur)
 ggsave("Control_PEratio.pdf")
 
 ggplot(con_avglat, aes(x=reorder(factor(cue), stim), y=latmean,color=as.factor(stim),fill=as.factor(cue)))+
   stat_summary(fun = "mean", geom = "bar",position = position_dodge(width=1.00),size=1)+
   scale_fill_manual(values=c("#3182bd","grey"))+ 
   scale_color_manual(values=c("black","red"))+ 
   #geom_line(size = 1,alpha=0.2,position = position_dodge(width=0.90))+
   geom_point(size = 3,alpha=0.2,position = position_jitterdodge(
     jitter.width = 0.25,
     jitter.height = 0,
     dodge.width = 0.90,
     seed = NA))+
   stat_summary(fun.data = mean_se,geom = "errorbar", size = 1,position = position_dodge(width=1.00))+
   labs(title="Control PE latency",x ="stim_duration", y = "PE latency(sec) ")+ 
   theme_classic()+ 
   theme( axis.line = element_line(colour = "black", size = 1, linetype = "solid"))+
   theme( axis.ticks = element_line(colour = "black", size =1, linetype = "solid")) +
   facet_grid(.~stim_dur)
 ggsave("Control_lat.pdf")
 
 
  
