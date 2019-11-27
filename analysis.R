#EFiT data analysis - 25/11/2019

#load packages and data
library(effectsize)
library(skimr)
library(bayestestR)
library(ggdag)
library(readxl)
library(tidyverse)

efit <- read_xlsx("EFiT_Main data set_05NOV2019.xlsx") %>% 
  filter(!partNum == 3, !partNum ==30, !partNum == 79) #remove participants 3, 30 
efit <- efit %>% rename(participant = partNum)

stroop <- read_csv("stroopTrimmed.csv")
ruleSwitchCost <- read_csv("switchCostMeans.csv") 
efit <- left_join(efit, ruleSwitchCost, by = "participant")#join switch cost data
efit <- left_join(efit, stroop, by = "participant")#join stroop data

#Descriptives
#Create categorical variable (THI = 0 = none, THI <= 37 = mild, THI >= 38 = modsev)
efit <- efit %>% 
  mutate(
    THI_cat = ifelse(
      THI >= 38 ,"modsev", ifelse(
        THI > 0 & THI <= 37, "mild", "none"
      ))) #add a categorical variable with the groups split by THI scores
efit %>% select(participant, THI, THI_cat)#check it's worked

efit$THI_cat <- as.factor(efit$THI_cat) %>% ordered(levels = c("none", "mild", "modsev"))#factorise tinnitus groups

#filter by groups for statistical tests
controls <- efit %>% 
  filter(THI_cat == "none")
mild_tin <- efit %>% 
  filter(THI_cat == "mild")
modsev_tin <- efit %>% 
  filter(THI_cat == "modsev")

#group_by tinnitus groups and count
efit %>% 
  group_by(THI_cat) %>% 
  summarise(group_count = n()) %>% 
  arrange(THI_cat)

###RIF 

#create new RIF variable (proportion of correct respones and RIFdif (i.e. a measure of the RIF effect)
efit <- efit %>% 
mutate(pRIF_RpPlus = (RIF_RpPlus/12) * 100,
       pRIF_RpMinus = (RIF_RpMinus/12) * 100,
       pRIF_Np = (RIF_Np/24) * 100,
       RIF_Diff = pRIF_Np - pRIF_RpMinus)

#summarise RIF scores between groups
efit %>% 
  group_by(THI_cat) %>% 
  summarise(Diff = mean(RIF_Diff),
            Plus = mean(pRIF_RpPlus),
            Minus = mean(pRIF_RpMinus),
            Np = mean(pRIF_Np))

#t.tests for RIF effect
t.test(controls$RIF_Diff, modsev_tin$RIF_Diff, alternative = "greater",var.equal = F)#controls greater than modsev
t.test(mild_tin$RIF_Diff, modsev_tin$RIF_Diff, alternative = "greater",var.equal = F)#mild greater than modsev

#effect size for RIF effect
cohens_d(controls$RIF_Diff, modsev_tin$RIF_Diff) %>% interpret_d(rules = "funder2019")
cohens_d(modsev_tin$RIF_Diff, controls$RIF_Diff) %>% interpret_d(rules = "funder2019")

###Number-letter sequencing
#summarise NLS scores between groups
efit %>% 
  group_by(THI_cat) %>% 
  summarise(numLetSeqMean = mean(numLetSeq))

#t.tests for NLS scores
t.test(controls$numLetSeq, modsev_tin$numLetSeq, alternative = "greater",var.equal = F)#controls greater than modsev
t.test(mild_tin$numLetSeq, modsev_tin$numLetSeq, alternative = "greater",var.equal = F)#mild greater than modsev

#effect size for NLS scores
cohens_d(controls$numLetSeq, modsev_tin$numLetSeq) %>% interpret_d(rules = "funder2019")
cohens_d(modsev_tin$numLetSeq, mild_tin$numLetSeq) %>% interpret_d(rules = "funder2019")
