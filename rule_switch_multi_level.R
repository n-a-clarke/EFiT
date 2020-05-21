library(brms)
library(readxl)
library(tidyverse)

files <- dir(pattern = "*.csv")
mainSet <- read_xlsx("EFiT_Main data set_5MARCH2020.xlsx") %>%
  filter(partNum != 3, partNum != 30) %>% 
  rename(participant = partNum)

df <- files %>%  map_df(read_csv)

#change character formatting for easier manipulation        

df$participant <- gsub(pattern = "EFiT_", x = df$participant, replacement = "")
df$participant <- gsub(pattern = "EFiT-", x = df$participant, replacement = "")
df$participant <- as.integer(df$participant)#remove EFiT sting and coerse to an integer for filtering

df <- left_join(df, mainSet, by = "participant")

df$participant <- as.factor(df$participant)

df <- df %>% 
  filter(is.na(trials.thisTrialN ))#filter out practise trials

df <- df %>% 
  mutate(
    THI_cat = ifelse(
      THI >= 38 ,"modsev", ifelse(
        THI > 0 & THI <= 37, "mild", "none"
      ))) #add a categorical variable with the groups split by THI scores

testing <- df %>% filter(participant ==2)

#create function that prints 0 for non-switch and 1 for switch

is_switch <- function(z) {
  ifelse(df$rule[z] == df$rule[z-1],0,1)
}

is_switch(21)
x <- c(2:length(df$rule))
switch_trial <- map(x, is_switch) %>% unlist()
switch_trial <- prepend(switch_trial, 0, before =  1)

df <- df %>% mutate(is.switch = switch_trial)

validate <- df %>% select(rule, is.switch)

first_trial <- seq(1, length(df$rule), 180)#create vector of first trials for each participant

first_gone_switch <- df[-first_trial,]#get rid of first trial for each participant as it can't be a switch trial

switch_full <- first_gone_switch %>% filter(is.switch == 1,
                             mainResp.corr == 1)

switch_full$THI_cat <- as.factor(switch_full$THI_cat)
switch_full$THI_cat <- ordered(switch_full$THI_cat, levels = c("none", "mild", "modsev"))

switch_full <- switch_full %>% mutate(switch_response_time = mainResp.rt * 1000)

switch_trials_mod <- brm(data = switch_full,
                         switch_response_time ~ 1 + THI_cat + scale(age) + scale(hearThresh) + scale(DASS_D) + scale(DASS_A) + scale(DASS_S) + (1|participant),
            file = "switch_trials",
            family = shifted_lognormal(),
            iter = 6000, warmup = 500, chains = 4, cores = 4,
            seed = 1234)

summary(switch_trials_mod)

marginal_effects(switch_trials_mod)

pp_check(switch_trials_mod)

abc <- plot(marginal_effects(switch_trials_mod))

abc$THI_cat +
  ylab("response time") +
  xlab("group")+
  cowplot::theme_minimal_grid()

hypothesis(switch_trials_mod, 'THI_cat.Q  > Intercept')  # Numerically

summary(switch_trials_mod)

116.93 + exp(7.62)

bother_switch_estimate <- 116.93 + exp(7.62+0.01)

2175.98-2155.492

cde <- marginal_effects(switch_trials_mod, method='predict')

###same model for non-switch
non_switch_full <- first_gone_switch %>% filter(is.switch == 0,
                                            mainResp.corr == 1)

non_switch_full$THI_cat <- as.factor(non_switch_full$THI_cat)
non_switch_full$THI_cat <- ordered(non_switch_full$THI_cat, levels = c("none", "mild", "modsev"))

non_switch_full <- non_switch_full %>% mutate(switch_response_time = mainResp.rt * 1000)

non-switch_trials_mod <- brm(data = non_switch_full,
                         switch_response_time ~ 1 + THI_cat + scale(age) + scale(hearThresh) + scale(DASS_D) + scale(DASS_A) + scale(DASS_S) + (1|participant),
                         file = "non_switch_trials",
                         family = shifted_lognormal(),
                         iter = 6000, warmup = 500, chains = 4, cores = 4,
                         seed = 1234)

marginal_effects(sure)

summary(sure)

sure <- readRDS("non_switch_trials.rds")

bother_non_switch_estimate <- 354.93 + exp(7.10-0.02)

control_non_switch_estimate <- 354.93 + exp(7.10)

control_switch_estimate <- 116.93 + exp(7.62)

bother_switch_cost <- bother_switch_estimate - bother_non_switch_estimate

control_switch_cost <- control_switch_estimate - control_non_switch_estimate

bother_switch_cost -control_switch_cost
