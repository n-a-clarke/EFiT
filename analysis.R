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

#filter by groups for statistical tests
controls <- efit %>% 
  filter(THI_cat == "none")
mild_tin <- efit %>% 
  filter(THI_cat == "mild")
modsev_tin <- efit %>% 
  filter(THI_cat == "modsev")

#t.tests for RIF effect
t.test(controls$RIF_Diff, modsev_tin$RIF_Diff, alternative = "greater",var.equal = F)#controls greater than modsev
t.test(controls$RIF_Diff, mild_tin$RIF_Diff, alternative = "greater",var.equal = F)#controls greater than mild
t.test(mild_tin$RIF_Diff, modsev_tin$RIF_Diff, alternative = "greater",var.equal = F)#mild greater than modsev

#effect size for RIF effect
rif_cd_control_modsev <- cohens_d(controls$RIF_Diff, modsev_tin$RIF_Diff)#controls vs modsev
rif_cd_control_mild <- cohens_d(controls$RIF_Diff, mild_tin$RIF_Diff)#controls vs mild
rif_cd_mild_modsev <- cohens_d(mild_tin$RIF_Diff, modsev_tin$RIF_Diff) #mild vs modsev

###Number-letter sequencing
#summarise NLS scores between groups
efit %>% 
  group_by(THI_cat) %>% 
  summarise(numLetSeqMean = mean(numLetSeq))

#t.tests for NLS scores
t.test(controls$numLetSeq, modsev_tin$numLetSeq, alternative = "greater",var.equal = F)#controls greater than modsev
t.test(controls$numLetSeq, mild_tin$numLetSeq, alternative = "greater",var.equal = F)#controls greater than mild
t.test(mild_tin$numLetSeq, modsev_tin$numLetSeq, alternative = "greater",var.equal = F)#mild greater than modsev

#effect size for NLS scores
nls_cd_control_modsev <- cohens_d(controls$numLetSeq, modsev_tin$numLetSeq)#controls vs modsev
nls_cd_control_mild <- cohens_d(controls$numLetSeq, mild_tin$numLetSeq)#controls vs mild
nls_cd_mild_modsev <- cohens_d(mild_tin$numLetSeq, modsev_tin$numLetSeq)#mild vs modsev

###Keep track
#summarise keep track scores between groups
efit %>% 
  group_by(THI_cat) %>% 
  summarise(keepTrackMean = mean(keepTrack))

#t.tests for keep track scores
t.test(controls$keepTrack, modsev_tin$keepTrack, alternative = "greater",var.equal = F)#controls greater than modsev
t.test(controls$keepTrack, mild_tin$keepTrack, alternative = "greater",var.equal = F)#controls greater than mild
t.test(mild_tin$keepTrack, modsev_tin$keepTrack, alternative = "greater",var.equal = F)#mild greater than modsev

#effect size for keep track scores
keep_cd_control_modsev <- cohens_d(controls$keepTrack, modsev_tin$keepTrack)#controls vs modsev
keep_cd_control_mild <- cohens_d(controls$keepTrack, mild_tin$keepTrack)#controls vs mild
keep_cd_mild_modsev <- cohens_d(mild_tin$keepTrack, modsev_tin$keepTrack)#mild vs modsev

###Matrix reasoning
#summarise matrix reasoning scores between groups
efit %>% 
  group_by(THI_cat) %>% 
  summarise(matrixReasonMean = mean(matrixReason))

#t.tests for matrix reasoning scores
t.test(controls$matrixReason, modsev_tin$matrixReason, alternative = "greater",var.equal = F)#controls greater than modsev
t.test(controls$matrixReason, mild_tin$matrixReason, alternative = "greater",var.equal = F)#controls greater than mild
t.test(mild_tin$matrixReason, modsev_tin$matrixReason, alternative = "greater",var.equal = F)#mild greater than modsev

#effect size for matrix reasoning scores
matrix_cd_control_modsev <- cohens_d(controls$matrixReason, modsev_tin$matrixReason)#controls vs modsev
matrix_cd_control_mild <- cohens_d(controls$matrixReason, mild_tin$matrixReason)#controls vs mild
matrix_cd_mild_modsev <- cohens_d(mild_tin$matrixReason, modsev_tin$matrixReason)#mild vs modsev

###Stroop
#summarise stroop response time and error scores between groups
efit %>% 
  group_by(THI_cat) %>% 
  summarise(congruentMean = mean(congruent),
            neutralMean = mean(neutral),
            incongruentMean = mean(incongruent),
            stroopErrorMean = mean(stroopErrors))

#t.tests for stroop response times
t.test(controls$incongruent, modsev_tin$incongruent, alternative = "less",var.equal = F)#controls less than modsev
t.test(controls$incongruent, mild_tin$incongruent, alternative = "less",var.equal = F)#controls less than mild
t.test(mild_tin$incongruent, modsev_tin$incongruent, alternative = "less",var.equal = F)#mild less than modsev

#effect size for stroop response times
stroopRT_cd_control_modsev <- cohens_d(controls$incongruent, modsev_tin$incongruent)#controls vs modsev
stroopRT_cd_control_mild <- cohens_d(controls$incongruent, mild_tin$incongruent)#controls vs mild
stroopRT_cd_mild_modsev <- cohens_d(mild_tin$incongruent, modsev_tin$incongruent)#mild vs modsev

#t.tests for stroop error counts
t.test(controls$stroopErrors, modsev_tin$stroopErrors, alternative = "less",var.equal = F)#controls less than modsev
t.test(controls$stroopErrors, mild_tin$stroopErrors, alternative = "less",var.equal = F)#controls less than mild
t.test(mild_tin$stroopErrors, modsev_tin$stroopErrors, alternative = "less",var.equal = F)#mild less than modsev

#effect size for stroop error counts
stroopERR_cd_control_modsev <- cohens_d(controls$stroopErrors, modsev_tin$stroopErrors)#controls vs modsev
stroopERR_cd_control_mild <- cohens_d(controls$stroopErrors, mild_tin$stroopErrors)#controls vs mild
stroopERR_cd_mild_modsev <- cohens_d(mild_tin$stroopErrors, modsev_tin$stroopErrors)#mild vs modsev

###Rule switching
#summarise rule switching response times and error scores between groups
efit %>% 
  group_by(THI_cat) %>% 
  summarise(switchCostMean = mean(switchCost),
            nonSwitchMean = mean(nonSwitch),
            switchMean = mean(switch),
            ruleErrorMean = mean(ruleErrors))

#t.tests for switch cost
t.test(controls$switchCost, modsev_tin$switchCost, alternative = "less",var.equal = F)#controls less than modsev
t.test(controls$switchCost, mild_tin$switchCost, alternative = "less",var.equal = F)#controls less than mild
t.test(mild_tin$switchCost, modsev_tin$switchCost, alternative = "less",var.equal = F)#mild less than modsev

#effect size for switch cost
switchCostRT_cd_control_modsev <- cohens_d(controls$switchCost, modsev_tin$switchCost)#controls vs modsev
switchCostRT_cd_control_mild <- cohens_d(controls$switchCost, mild_tin$switchCost)#controls vs mild
switchCostRT_cd_mild_modsev <- cohens_d(mild_tin$switchCost, modsev_tin$switchCost)#mild vs modsev

#t.tests for rule switch error counts
t.test(controls$ruleErrors, modsev_tin$ruleErrors, alternative = "less",var.equal = F)#controls less than modsev
t.test(controls$ruleErrors, mild_tin$ruleErrors, alternative = "less",var.equal = F)#controls less than mild
t.test(mild_tin$ruleErrors, modsev_tin$ruleErrors, alternative = "less",var.equal = F)#mild less than modsev

#effect size for rule switch errors
switchErr_cd_control_modsev <- cohens_d(controls$ruleErrors, modsev_tin$ruleErrors)#controls vs modsev
switchErr_cd_control_mild <- cohens_d(controls$ruleErrors, mild_tin$ruleErrors)#controls vs mild
switchErr_cd_mild_modsev <- cohens_d(mild_tin$ruleErrors, modsev_tin$ruleErrors)#mild vs modsev

###Dual task
#summarise dual task scores between groups
efit %>% 
  group_by(THI_cat) %>% 
  summarise(numbersCorrectMean = mean(dualTask_num),
            easySIN = mean(dualTask_wordsEasy),
            difficultSIN = mean(dualTask_wordsEasy))

#t.tests for dual task scores
t.test(controls$dualTask_num, modsev_tin$dualTask_num, alternative = "greater",var.equal = F)#controls greater than modsev
t.test(controls$dualTask_num, mild_tin$dualTask_num, alternative = "greater",var.equal = F)#controls greater than mild
t.test(mild_tin$dualTask_num, modsev_tin$dualTask_num, alternative = "greater",var.equal = F)#mild greater than modsev

#effect size for dual task numbers score
dual_cd_control_modsev <- cohens_d(controls$dualTask_num, modsev_tin$dualTask_num)#controls vs modsev
dual_cd_control_mild <- cohens_d(controls$dualTask_num, mild_tin$dualTask_num)#controls vs mild
dual_cd_mild_modsev <- cohens_d(mild_tin$dualTask_num, modsev_tin$dualTask_num)#mild vs modsev

#create vectors of cohens_d scores for control vs modsev
control_modsev_cd <- data.frame(
  c("Retrieval-induced forgetting", 
    "Number letter sequencing", 
    "Keep track", 
    "Matrix reasoning",
    "Stroop-RT", 
    "Stroop-errors", 
    "Switch cost", 
    "Switch errors", 
    "Dual task"),
  c(rif_cd_control_modsev, 
    nls_cd_control_modsev, 
    keep_cd_control_modsev, 
    matrix_cd_control_modsev, 
    stroopRT_cd_control_modsev, 
    stroopERR_cd_control_modsev, 
    switchCostRT_cd_control_modsev,
    switchErr_cd_control_modsev, 
    dual_cd_control_modsev),
  c(rep(1, 9))
  )
colnames(control_modsev_cd) <- c("task", "cohens_d", "group")

#create vectors of cohens_d scores for control vs mild
control_mild_cd <- data.frame(
  c("Retrieval-induced forgetting", 
    "Number letter sequencing", 
    "Keep track", 
    "Matrix reasoning",
    "Stroop-RT", 
    "Stroop-errors", 
    "Switch cost", 
    "Switch errors", 
    "Dual task"),
  c(rif_cd_control_mild, 
    nls_cd_control_mild, 
    keep_cd_control_mild, 
    matrix_cd_control_mild, 
    stroopRT_cd_control_mild, 
    stroopERR_cd_control_mild, 
    switchCostRT_cd_control_mild,
    switchErr_cd_control_mild, 
    dual_cd_control_mild),
  c(rep(2, 9))
)
colnames(control_mild_cd) <- c("task", "cohens_d", "group")

#create vectors of cohens_d scores for control vs mild
mild_modsev_cd <- data.frame(
  c("Retrieval-induced forgetting", 
    "Number letter sequencing", 
    "Keep track", 
    "Matrix reasoning",
    "Stroop-RT", 
    "Stroop-errors", 
    "Switch cost", 
    "Switch errors", 
    "Dual task"),
  c(rif_cd_mild_modsev, 
    nls_cd_mild_modsev, 
    keep_cd_mild_modsev, 
    matrix_cd_mild_modsev, 
    stroopRT_cd_mild_modsev, 
    stroopERR_cd_mild_modsev, 
    switchCostRT_cd_mild_modsev,
    switchErr_cd_mild_modsev, 
    dual_cd_mild_modsev),
  c(rep(3, 9))
)
colnames(mild_modsev_cd) <- c("task", "cohens_d", "group")

#join tables together
all_cohens_d <- bind_rows(control_modsev_cd, control_mild_cd, mild_modsev_cd)
#change minus signs to positives for plotting
all_cohens_d <- all_cohens_d %>% arrange(cohens_d)#arrange for easier transform
all_cohens_d[1:17,2] <- all_cohens_d[1:17,2]*-1
#change group membership column to factor
all_cohens_d[,3] <- as.factor(all_cohens_d[,3])

control_modsev_control_mild_cd <- all_cohens_d %>% 
  filter(group == 1 | group == 2)


#plot of cohens_d for each task
ggplot(control_modsev_control_mild_cd , aes(x = cohens_d, y = task, colour = group)) +
  geom_point(position = "jitter",
             shape = 15, size = 3) +
  xlim(c(0,1))
  
