#EFiT data analysis - 25/11/2019

#load packages and data
library(effectsize)
library(psych)
library(skimr)
library(readxl)
library(tidyverse)

efit <- read_xlsx("EFiT_Main data set_29NOV2019.xlsx") %>% 
  filter(!partNum == 3, !partNum ==30, !partNum == 79, !partNum == 121) #remove participants 3, 30 
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

#divide CRIq score by age
efit <- efit %>% mutate(CRIq_aged = CRIq/age)

#group_by tinnitus groups and count
groupCount <- efit %>% 
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

#cohens_d confidence intervals for controls vs modsev
rif_cd_control_modsev_CI <- d.ci(rif_cd_control_modsev, n1=groupCount$group_count[1], n2=groupCount$group_count[3])#confidence interval for RIF
nls_cd_control_modsev_CI <- d.ci(nls_cd_control_modsev, n1=groupCount$group_count[1], n2=groupCount$group_count[3])#confidence interval for nls
keep_cd_control_modsev_CI <- d.ci(keep_cd_control_modsev, n1=groupCount$group_count[1], n2=groupCount$group_count[3])#confidence interval for keep track
matrix_cd_control_modsev_CI <- d.ci(matrix_cd_control_modsev, n1=groupCount$group_count[1], n2=groupCount$group_count[3])#confidence interval for matrix reasoning
stroopRT_cd_control_modsev_CI <- d.ci(stroopRT_cd_control_modsev , n1=groupCount$group_count[1], n2=groupCount$group_count[3])#confidence interval for Stroop RT
stroopERR_cd_control_modsev_CI <- d.ci(stroopERR_cd_control_modsev , n1=groupCount$group_count[1], n2=groupCount$group_count[3])#confidence interval for Stroop errors
switchCostRT_cd_control_modsev_CI <- d.ci(switchCostRT_cd_control_modsev , n1=groupCount$group_count[1], n2=groupCount$group_count[3])#confidence interval for switch cost
switchErr_cd_control_modsev_CI <- d.ci(switchErr_cd_control_modsev , n1=groupCount$group_count[1], n2=groupCount$group_count[3])#confidence interval for switch errors
dual_cd_control_modsevCI <- d.ci(dual_cd_control_modsev , n1=groupCount$group_count[1], n2=groupCount$group_count[3])#confidence interval for dual task


#create dataframe of cohens_d scores for control vs modsev
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
  c(rif_cd_control_modsev_CI[1], 
    nls_cd_control_modsev_CI[1], 
    keep_cd_control_modsev_CI[1], 
    matrix_cd_control_modsev_CI[1], 
    stroopRT_cd_control_modsev_CI[1], 
    stroopERR_cd_control_modsev_CI[1], 
    switchCostRT_cd_control_modsev_CI[1],
    switchErr_cd_control_modsev_CI[1], 
    dual_cd_control_modsevCI[3]),
  c(rif_cd_control_modsev_CI[3], 
    nls_cd_control_modsev_CI[3], 
    keep_cd_control_modsev_CI[3], 
    matrix_cd_control_modsev_CI[3], 
    stroopRT_cd_control_modsev_CI[3], 
    stroopERR_cd_control_modsev_CI[3], 
    switchCostRT_cd_control_modsev_CI[3],
    switchErr_cd_control_modsev_CI[3], 
    dual_cd_control_modsevCI[3]),
  c(rep(1, 9))
  )
colnames(control_modsev_cd) <- c("task", "cohens_d", "CI_lower","CI_upper", "group")

#cohens_d confidence intervals for controls vs mild
rif_cd_control_mild_CI <- d.ci(rif_cd_control_mild, n1=groupCount$group_count[1], n2=groupCount$group_count[3])#confidence interval for RIF
nls_cd_control_mild_CI <- d.ci(nls_cd_control_mild, n1=groupCount$group_count[1], n2=groupCount$group_count[3])#confidence interval for nls
keep_cd_control_mild_CI <- d.ci(keep_cd_control_mild, n1=groupCount$group_count[1], n2=groupCount$group_count[3])#confidence interval for keep track
matrix_cd_control_mild_CI <- d.ci(matrix_cd_control_mild, n1=groupCount$group_count[1], n2=groupCount$group_count[3])#confidence interval for matrix reasoning
stroopRT_cd_control_mild_CI <- d.ci(stroopRT_cd_control_mild , n1=groupCount$group_count[1], n2=groupCount$group_count[3])#confidence interval for Stroop RT
stroopERR_cd_control_mild_CI <- d.ci(stroopERR_cd_control_mild , n1=groupCount$group_count[1], n2=groupCount$group_count[3])#confidence interval for Stroop errors
switchCostRT_cd_control_mild_CI <- d.ci(switchCostRT_cd_control_mild , n1=groupCount$group_count[1], n2=groupCount$group_count[3])#confidence interval for switch cost
switchErr_cd_control_mild_CI <- d.ci(switchErr_cd_control_mild , n1=groupCount$group_count[1], n2=groupCount$group_count[3])#confidence interval for switch errors
dual_cd_control_mildCI <- d.ci(dual_cd_control_mild , n1=groupCount$group_count[1], n2=groupCount$group_count[3])#confidence interval for dual task

#create dataframe of cohens_d scores for control vs mild
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
  c(rif_cd_control_mild_CI[1], 
    nls_cd_control_mild_CI[1], 
    keep_cd_control_mild_CI[1], 
    matrix_cd_control_mild_CI[1], 
    stroopRT_cd_control_mild_CI[1], 
    stroopERR_cd_control_mild_CI[1], 
    switchCostRT_cd_control_mild_CI[1],
    switchErr_cd_control_mild_CI[1], 
    dual_cd_control_mildCI[1]),
  c(rif_cd_control_mild_CI[3], 
    nls_cd_control_mild_CI[3], 
    keep_cd_control_mild_CI[3], 
    matrix_cd_control_mild_CI[3], 
    stroopRT_cd_control_mild_CI[3], 
    stroopERR_cd_control_mild_CI[3], 
    switchCostRT_cd_control_mild_CI[3],
    switchErr_cd_control_mild_CI[3], 
    dual_cd_control_mildCI[3]),
  c(rep(2, 9))
)
colnames(control_mild_cd) <- c("task", "cohens_d", "CI_lower","CI_upper", "group")

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

#join two control tables together
controlVSmodsev_controlVSmild_cohens_d <- bind_rows(control_modsev_cd, control_mild_cd)
#change minus signs to positives for plotting
controlVSmodsev_controlVSmild_cohens_d <- controlVSmodsev_controlVSmild_cohens_d %>% arrange(cohens_d)#arrange for easier transform
controlVSmodsev_controlVSmild_cohens_d[1:12,2:4] <- controlVSmodsev_controlVSmild_cohens_d[1:12,2:4]*-1#transofrm direction of negative effects for easier interpretation
#change group membership column to factor
controlVSmodsev_controlVSmild_cohens_d[,5] <- as.factor(controlVSmodsev_controlVSmild_cohens_d[,5])

#plot of cohens_d for control vs modsev and control vs mild for each task
pos <- position_jitter(width = NULL, height = 1, seed = 123)

ggplot(controlVSmodsev_controlVSmild_cohens_d , aes(x = cohens_d, y = task, colour = group)) +
  geom_point(alpha = 0.9, shape = 15, size = 3, position = pos) +
  xlim(c(-1,1)) +
  geom_errorbarh(alpha = 0.7, aes(xmin=CI_lower, xmax=CI_upper), position = pos) +
  geom_vline(xintercept = 0, linetype = "longdash")

####################################  
###Bayesian between groups analysis
####################################
library(rstanarm)
library(bayestestR)
library(see)
library(insight)

#Controls vs modsev - drop mild observations for bayesian t-test
bayes_control_modsev <- efit %>% 
  filter(THI_cat != "mild") %>% 
  droplevels()

#violin-dot plot to visualise distibution and sample size for rule switch cost - control vs modsev
bayes_control_modsev %>% 
  ggplot(aes(x = THI_cat, y = switchCost, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for rule switch task - control vs modsev
control_modsev_switch_cost_BF <- BayesFactor::ttestBF(formula = switchCost ~ THI_cat, data = bayes_control_modsev)  
BF_control_modsev_switchCost <- describe_posterior(control_modsev_switch_cost_BF)

#violin-dot plot to visualise distibution and sample size for rule switch error count - control vs modsev
bayes_control_modsev %>% 
  ggplot(aes(x = THI_cat, y = ruleErrors, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for rule switch error - control vs modsev
control_modsev_switch_error_BF <- BayesFactor::ttestBF(formula = ruleErrors ~ THI_cat, data = bayes_control_modsev)
BF_control_modsev_switchError <- describe_posterior(control_modsev_switch_error_BF)

#violin-dot plot to visualise distibution and sample size for stroop response time - control vs modsev
bayes_control_modsev %>% 
  ggplot(aes(x = THI_cat, y = incongruent, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for stroop response time - control vs modsev
control_modsev_stroop_rt_BF <- BayesFactor::ttestBF(formula = incongruent ~ THI_cat, data = bayes_control_modsev)
BF_control_modsev_stroopRT <- describe_posterior(control_modsev_stroop_rt_BF)

#violin-dot plot to visualise distibution and sample size for stroop error count - control vs modsev
bayes_control_modsev %>% 
  ggplot(aes(x = THI_cat, y = stroopErrors, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for stroop error count - control vs modsev
control_modsev_stroop_error_BF <- BayesFactor::ttestBF(formula = incongruent ~ THI_cat, data = bayes_control_modsev)
BF_control_modsev_stroopError <- describe_posterior(control_modsev_stroop_error_BF)

#violin-dot plot to visualise distibution and sample size for number letter sequencing - control vs modsev
bayes_control_modsev %>% 
  ggplot(aes(x = THI_cat, y = numLetSeq, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for number letter sequencing - control vs modsev
control_modsev_nls_BF <- BayesFactor::ttestBF(formula = numLetSeq ~ THI_cat, data = bayes_control_modsev)
BF_control_modsev_nls <- describe_posterior(control_modsev_nls_BF)

#violin-dot plot to visualise distibution and sample size for keep track task - control vs modsev
bayes_control_modsev %>% 
  ggplot(aes(x = THI_cat, y = keepTrack, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for keep track task - control vs modsev
control_modsev_keep_BF <- BayesFactor::ttestBF(formula = keepTrack ~ THI_cat, data = bayes_control_modsev)
BF_control_modsev_keep <- describe_posterior(control_modsev_keep_BF)

#violin-dot plot to visualise distibution and sample size for RIF - control vs modsev
bayes_control_modsev %>% 
  ggplot(aes(x = THI_cat, y = RIF_Diff, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for RIF - control vs modsev
control_modsev_RIF_BF <- BayesFactor::ttestBF(formula = RIF_Diff ~ THI_cat, data = bayes_control_modsev)
BF_control_modsev_RIF <- describe_posterior(control_modsev_RIF_BF)

#violin-dot plot to visualise distibution and sample size for matrix reasoning - control vs modsev
bayes_control_modsev %>% 
  ggplot(aes(x = THI_cat, y = matrixReason, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for matrix reasoning - control vs modsev
control_modsev_matrix_BF <- BayesFactor::ttestBF(formula = matrixReason ~ THI_cat, data = bayes_control_modsev)
BF_control_modsev_matrix <- describe_posterior(control_modsev_matrix_BF)

#violin-dot plot to visualise distibution and sample size for dual task - control vs modsev
bayes_control_modsev %>% 
  ggplot(aes(x = THI_cat, y = dualTask_num, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for dual task - control vs modsev
control_modsev_dual_BF <- BayesFactor::ttestBF(formula = dualTask_num ~ THI_cat, data = bayes_control_modsev)
BF_control_modsev_dual <- describe_posterior(control_modsev_dual_BF)

#violin-dot plot to visualise distibution and sample size for CFQ- control vs modsev
bayes_control_modsev %>% 
  ggplot(aes(x = THI_cat, y = CFQ, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for CFQ - control vs modsev
control_modsev_CFQ_BF <- BayesFactor::ttestBF(formula = CFQ ~ THI_cat, data = bayes_control_modsev)
BF_control_modsev_CFQ <- describe_posterior(control_modsev_CFQ_BF)

#violin-dot plot to visualise distibution and sample size for RRS- control vs modsev
bayes_control_modsev %>% 
  ggplot(aes(x = THI_cat, y = RRS, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for RRS - control vs modsev
control_modsev_RRS_BF <- BayesFactor::ttestBF(formula = RRS ~ THI_cat, data = bayes_control_modsev)
BF_control_modsev_RRS <- describe_posterior(control_modsev_RRS_BF)

#violin-dot plot to visualise distibution and sample size for CRIq_aged - control vs modsev
bayes_control_modsev %>% 
  ggplot(aes(x = THI_cat, y = CRIq_aged, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for CRIq_aged - control vs modsev
control_modsev_CRIq_aged_BF <- BayesFactor::ttestBF(formula = CRIq_aged ~ THI_cat, data = bayes_control_modsev)
BF_control_modsev_CRIq_aged <- describe_posterior(control_modsev_CRIq_aged_BF)

#violin-dot plot to visualise distibution and sample size for depression - control vs modsev
bayes_control_modsev %>% 
  ggplot(aes(x = THI_cat, y = DASS_D, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for depression - control vs modsev
control_modsev_depression_BF <- BayesFactor::ttestBF(formula = CRIq_aged ~ THI_cat, data = bayes_control_modsev)
BF_control_modsev_depression <- describe_posterior(control_modsev_depression_BF)

#violin-dot plot to visualise distibution and sample size for anxiety - control vs modsev
bayes_control_modsev %>% 
  ggplot(aes(x = THI_cat, y = DASS_A, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for anxiety - control vs modsev
control_modsev_anxiety_BF <- BayesFactor::ttestBF(formula = DASS_A ~ THI_cat, data = bayes_control_modsev)
BF_control_modsev_anxiety <- describe_posterior(control_modsev_anxiety_BF)

#violin-dot plot to visualise distibution and sample size for stress - control vs modsev
bayes_control_modsev %>% 
  ggplot(aes(x = THI_cat, y = DASS_S, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for stress - control vs modsev
control_modsev_stress_BF <- BayesFactor::ttestBF(formula = DASS_S ~ THI_cat, data = bayes_control_modsev)
BF_control_modsev_stress <- describe_posterior(control_modsev_stress_BF)
###############################
#Controls vs mild - drop modsev observations for bayesian t-test
bayes_control_mild <- efit %>% 
  filter(THI_cat != "modsev") %>% 
  droplevels()

#violin-dot plot to visualise distibution and sample size for rule switch cost - control vs mild
bayes_control_mild %>% 
  ggplot(aes(x = THI_cat, y = switchCost, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for rule switch task - control vs mild
control_mild_switch_cost_BF <- BayesFactor::ttestBF(formula = switchCost ~ THI_cat, data = bayes_control_mild)
BF_control_mild_switchCost <- describe_posterior(control_mild_switch_cost_BF)

#violin-dot plot to visualise distibution and sample size for rule switch error count - control vs mild
bayes_control_mild %>% 
  ggplot(aes(x = THI_cat, y = ruleErrors, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for rule switch error - control vs mild
control_mod_mild_switch_error_BF <- BayesFactor::ttestBF(formula = ruleErrors ~ THI_cat, data = bayes_control_mild)
BF_control_mild_switchError <- describe_posterior(control_mod_mild_switch_error_BF)

#violin-dot plot to visualise distibution and sample size for stroop response time - control vs mild
bayes_control_mild %>% 
  ggplot(aes(x = THI_cat, y = incongruent, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for stroop response time - control vs mild
control_mild_stroop_rt_BF <- BayesFactor::ttestBF(formula = incongruent ~ THI_cat, data = bayes_control_mild)
BF_control_mild_stroopRT <- describe_posterior(control_mild_stroop_rt_BF)

#violin-dot plot to visualise distibution and sample size for stroop error count - control vs mild
bayes_control_mild %>% 
  ggplot(aes(x = THI_cat, y = stroopErrors, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for stroop error count - control vs mild
control_mild_stroop_error_BF <- BayesFactor::ttestBF(formula = incongruent ~ THI_cat, data = bayes_control_mild)
BF_control_mild_stroopError <- describe_posterior(control_mild_stroop_error_BF)

#violin-dot plot to visualise distibution and sample size for number letter sequencing - control vs mild
bayes_control_mild %>% 
  ggplot(aes(x = THI_cat, y = numLetSeq, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for number letter sequencing - control vs mild
control_mild_nls_BF <- BayesFactor::ttestBF(formula = numLetSeq ~ THI_cat, data = bayes_control_mild)
BF_control_mild_nls <- describe_posterior(control_mild_nls_BF)

#violin-dot plot to visualise distibution and sample size for keep track task - control vs mild
bayes_control_mild %>% 
  ggplot(aes(x = THI_cat, y = keepTrack, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for keep track task - control vs mild
control_mild_keep_BF <- BayesFactor::ttestBF(formula = keepTrack ~ THI_cat, data = bayes_control_modsev)
BF_control_mild_keep <- describe_posterior(control_mild_keep_BF)

#violin-dot plot to visualise distibution and sample size for RIF - control vs mild
bayes_control_mild %>% 
  ggplot(aes(x = THI_cat, y = RIF_Diff, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for RIF - control vs mild
control_mild_RIF_BF <- BayesFactor::ttestBF(formula = RIF_Diff ~ THI_cat, data = bayes_control_mild)
BF_control_mild_RIF <- describe_posterior(control_mild_RIF_BF)

#violin-dot plot to visualise distibution and sample size for matrix reasoning - control vs mild
bayes_control_mild %>% 
  ggplot(aes(x = THI_cat, y = matrixReason, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for matrix reasoning - control vs mild
control_mild_matrix_BF <- BayesFactor::ttestBF(formula = matrixReason ~ THI_cat, data = bayes_control_mild)
BF_control_mild_matrix <- describe_posterior(control_mild_matrix_BF)

#violin-dot plot to visualise distibution and sample size for dual task - control vs mild
bayes_control_mild %>% 
  ggplot(aes(x = THI_cat, y = dualTask_num, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for dual task - control vs mild
control_mild_dual_BF <- BayesFactor::ttestBF(formula = dualTask_num ~ THI_cat, data = bayes_control_mild)
BF_control_mild_dual <- describe_posterior(control_mild_dual_BF)

#violin-dot plot to visualise distibution and sample size for CFQ- control vs mild
bayes_control_mild %>% 
  ggplot(aes(x = THI_cat, y = CFQ, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for CFQ - control vs mild
control_mild_CFQ_BF <- BayesFactor::ttestBF(formula = CFQ ~ THI_cat, data = bayes_control_mild)
BF_control_mild_CFQ <- describe_posterior(control_mild_CFQ_BF)

#violin-dot plot to visualise distibution and sample size for RRS- control vs mild
bayes_control_mild %>% 
  ggplot(aes(x = THI_cat, y = RRS, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for RRS - control vs mild
control_mild_RRS_BF <- BayesFactor::ttestBF(formula = RRS ~ THI_cat, data = bayes_control_mild)
BF_control_mild_RRS<- describe_posterior(control_mild_RRS_BF)

#violin-dot plot to visualise distibution and sample size for CRIq_aged - control vs mild
bayes_control_mild %>% 
  ggplot(aes(x = THI_cat, y = CRIq_aged, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for CRIq_aged - control vs mild
control_mild_CRIq_aged_BF <- BayesFactor::ttestBF(formula = CRIq_aged ~ THI_cat, data = bayes_control_mild)
BF_control_mild_CRIq_aged <- describe_posterior(control_mild_CRIq_aged_BF)

#violin-dot plot to visualise distibution and sample size for depression - control vs mild
bayes_control_mild %>% 
  ggplot(aes(x = THI_cat, y = DASS_D, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for depression - control vs mild
control_mild_depression_BF <- BayesFactor::ttestBF(formula = CRIq_aged ~ THI_cat, data = bayes_control_mild)
BF_control_mild_depression <- describe_posterior(control_mild_depression_BF)

#violin-dot plot to visualise distibution and sample size for anxiety - control vs mild
bayes_control_mild %>% 
  ggplot(aes(x = THI_cat, y = DASS_A, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for anxiety - control vs mild
control_mild_anxiety_BF <- BayesFactor::ttestBF(formula = DASS_A ~ THI_cat, data = bayes_control_mild)
BF_control_mild_anxiety <- describe_posterior(control_mild_anxiety_BF)

#violin-dot plot to visualise distibution and sample size for stress - control vs mild
bayes_control_mild %>% 
  ggplot(aes(x = THI_cat, y = DASS_S, fill = THI_cat)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

#bayesian ttest for stress - control vs mild
control_mild_stress_BF <- BayesFactor::ttestBF(formula = DASS_S ~ THI_cat, data = bayes_control_mild)
BF_control_mild_stress <- describe_posterior(control_mild_stress_BF)
#################
#create table of bayes factors for control vs mild
control_mild_BF <- data.frame(
  c("Retrieval-induced forgetting", 
    "Number letter sequencing", 
    "Keep track", 
    "Matrix reasoning",
    "Stroop-RT", 
    "Stroop-errors", 
    "Switch cost", 
    "Switch errors", 
    "Dual task",
    "CFQ",
    "RRS",
    "CRIq_aged",
    "Depression",
    "Anxiety",
    "Stress"),
  c(BF_control_mild_RIF$BF, 
    BF_control_mild_nls$BF, 
    BF_control_mild_keep$BF,
    BF_control_mild_matrix$BF,
    BF_control_mild_stroopRT$BF,
    BF_control_mild_stroopError$BF,
    BF_control_mild_switchCost$BF,
    BF_control_mild_switchError$BF,
    BF_control_mild_dual$BF,
    BF_control_mild_CFQ$BF,
    BF_control_mild_RRS$BF,
    BF_control_mild_CRIq_aged$BF,
    BF_control_mild_depression$BF,
    BF_control_mild_anxiety$BF,
    BF_control_mild_stress$BF)
)
colnames(control_mild_BF) <- c("name", "bayes_factor")
control_mild_BF$bayes_factor <- signif(control_mild_BF$bayes_factor, 2)

control_mild_BF <- control_mild_BF %>% arrange(desc(bayes_factor))

#bayes factor for control vs modsev
control_modsev_BF <- data.frame(
  c("Retrieval-induced forgetting", 
    "Number letter sequencing", 
    "Keep track", 
    "Matrix reasoning",
    "Stroop-RT", 
    "Stroop-errors", 
    "Switch cost", 
    "Switch errors", 
    "Dual task",
    "CFQ",
    "RRS",
    "CRIq_aged",
    "Depression",
    "Anxiety",
    "Stress"),
  c(BF_control_modsev_RIF$BF, 
    BF_control_modsev_nls$BF, 
    BF_control_modsev_keep$BF,
    BF_control_modsev_matrix$BF,
    BF_control_modsev_stroopRT$BF,
    BF_control_modsev_stroopError$BF,
    BF_control_modsev_switchCost$BF,
    BF_control_modsev_switchError$BF,
    BF_control_modsev_dual$BF,
    BF_control_modsev_CFQ$BF,
    BF_control_modsev_RRS$BF,
    BF_control_modsev_CRIq_aged$BF,
    BF_control_modsev_depression$BF,
    BF_control_modsev_anxiety$BF,
    BF_control_modsev_stress$BF)
)
colnames(control_modsev_BF) <- c("name", "bayes_factor")
control_modsev_BF$bayes_factor <- signif(control_modsev_BF$bayes_factor, 2)

control_modsev_BF <- control_modsev_BF %>% arrange(desc(bayes_factor))

#filter BF tables for outcomes that support H1
control_mild_BF %>% filter(bayes_factor > 1)
control_modsev_BF %>% filter(bayes_factor > 1)

#filter BF tables for outcomes that support H0
control_mild_BF %>% filter(bayes_factor < 1)
control_modsev_BF %>% filter(bayes_factor < 1)
