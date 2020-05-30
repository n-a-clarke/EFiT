library(brms)
library(readxl)
library(tidyverse)
library(cowplot)
library(patchwork)

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

df <- df %>% filter(participant != 79)#remove participant 79

df <- df %>% 
  mutate(
    THI_cat = ifelse(
      THI >= 38 ,"3", ifelse(
        THI > 0 & THI <= 37, "2", "1"
      ))) #add a categorical variable with the groups split by THI scores

#create function that prints 0 for non-switch and 1 for switch
is_switch <- function(z) {
  ifelse(df$rule[z] == df$rule[z-1],0,1)
}
#vector of values to iterate over - function works by subtracting 1 from current position, so needs to start at 2nd row
x <- c(2:length(df$rule))
#use function over above vector
switch_trial <- map(x, is_switch) %>% unlist()
#prepend 0 to vector as this couldn't have been a switch trial
switch_trial <- prepend(switch_trial, 0, before =  1)
#add this vector to main dataframe
df <- df %>% mutate(is.switch = switch_trial)
#create vector that corresponds to first trial for each participant by sequencing from 1 to length of df counting in 180
first_trial <- seq(1, length(df$rule), 180)

first_gone_switch <- df[-first_trial,]#get rid of first trial for each participant as it can't be a switch trial
#filter only correct and switch trials
switch_full <- first_gone_switch %>% filter(is.switch == 1,
                             mainResp.corr == 1)
#create ordered tinnitus category factor
switch_full$THI_cat <- as.factor(switch_full$THI_cat)
switch_full$THI_cat <- ordered(switch_full$THI_cat, levels = c("none", "mild", "modsev"))
#rescale rts to milliseconds
switch_full <- switch_full %>% mutate(switch_response_time = mainResp.rt * 1000)

#histogram of remaining rts
switch_full %>% ggplot(aes(x = switch_response_time)) +
  geom_histogram(bins = 50)


outliers_200_4000 <- switch_full %>% 
  filter(switch_response_time < 200 |
         switch_response_time > 4000)

switch_200_4000_cut <- switch_full %>% 
  filter(switch_response_time > 200 &
           switch_response_time < 4000)

#calculate proportion of outliers removed
(nrow(outliers_200_4000)/nrow(switch_full)) * 100

#run 200ms - 4000ms models, scaled and unscaled predictors
switch_trials_scaled_mod <- brm(data = switch_200_4000_cut,
                         switch_response_time ~ 1 +
                           THI_cat + 
                           scale(age) +
                           scale(hearThresh) +
                           scale(DASS_D) +
                           scale(DASS_A) +
                           scale(DASS_S) +
                           (1|participant),
            file = "switch_trials_scaled_preds_200_4000ms",
            family = shifted_lognormal(),
            iter = 6000, warmup = 500, chains = 4, cores = 4,
            seed = 1234)
#unscaled model
switch_trials_mod <- brm(data = switch_200_4000_cut,
                                switch_response_time ~ 1 +
                                  THI_cat + 
                           age +
                                  hearThresh +
                                  DASS_D +
                                  DASS_A +
                                  DASS_S +
                                  (1|participant),
                                file = "switch_trials_200_4000ms",
                                family = shifted_lognormal(),
                                iter = 6000, warmup = 500, chains = 4, cores = 4,
                                seed = 1234)

#load models from RDS objects
switch_trials_scaled_mod <- readRDS("switch_trials_scaled_preds_200_4000ms.rds")
switch_trials_mod <- readRDS("switch_trials_200_4000ms.rds")
#model summarires
summary(switch_trials_scaled_mod)
summary(switch_trials_mod)
#pp checks - contrast this with the normal fit
pp_check(switch_trials_mod)
pp_check(switch_trials_scaled_mod)
#marginal effects
marginal_effects(switch_trials_scaled_mod)

#save marginal effects as ggplot objects
abc <- plot(marginal_effects(switch_trials_mod))
#manipulate plot code
abc$THI_cat +
  ylab("response time") +
  xlab("group")+
  cowplot::theme_minimal_grid()

#Bayes factor for paramter differences
hypothesis(switch_trials_mod, 'THI_cat.L  > 0')  # Numerically

#code for transforming paramter values back: ndt + exp(predictor)
12.21 + exp(7.08)

bother_switch_estimate <- 116.93 + exp(7.62+0.01)


outliers_200_5000 <- switch_full %>% 
  filter(switch_response_time < 200 |
           switch_response_time > 5000)

switch_200_5000_cut <- switch_full %>% 
  filter(switch_response_time > 200 &
           switch_response_time < 5000)

#calculate proportion of outliers removed
(nrow(outliers_200_5000)/nrow(switch_full)) * 100

#run 200ms - 5000ms models, scaled and unscaled predictors
switch_trials_scaled_mod_5000 <- brm(data = switch_200_5000_cut,
                                switch_response_time ~ 1 +
                                  THI_cat + 
                                  scale(age) +
                                  scale(hearThresh) +
                                  scale(DASS_D) +
                                  scale(DASS_A) +
                                  scale(DASS_S) +
                                  (1|participant),
                                file = "switch_trials_scaled_preds_200_5000ms",
                                family = shifted_lognormal(),
                                iter = 6000, warmup = 500, chains = 4, cores = 4,
                                seed = 1234)

switch_trials_scaled_mod_5000 <- readRDS("switch_trials_scaled_preds_200_5000ms.rds")
#model summarires
summary(switch_trials_scaled_mod_5000)
#pp checks - contrast this with the normal fit
pp_check(switch_trials_scaled_mod_5000)
#marginal effects
conditional_effects(switch_trials_scaled_mod_5000)

#now with 15% outliers removed
outliers_200_3500 <- switch_full %>% 
  filter(switch_response_time < 200 |
           switch_response_time > 3500)

switch_200_3500_cut <- switch_full %>% 
  filter(switch_response_time > 200 &
           switch_response_time < 3500)

#calculate proportion of outliers removed
(nrow(outliers_200_3500)/nrow(switch_full)) * 100

#run 200ms - 3500ms models, scaled and unscaled predictors
switch_trials_scaled_mod_3500 <- brm(data = switch_200_3500_cut,
                                     switch_response_time ~ 1 +
                                       THI_cat + 
                                       scale(age) +
                                       scale(hearThresh) +
                                       scale(DASS_D) +
                                       scale(DASS_A) +
                                       scale(DASS_S) +
                                       (1|participant),
                                     file = "switch_trials_scaled_preds_200_3500ms",
                                     family = shifted_lognormal(),
                                     iter = 6000, warmup = 500, chains = 4, cores = 4,
                                     seed = 1234)

switch_trials_scaled_mod_3500 <- readRDS("switch_trials_scaled_preds_200_3500ms.rds")

conditional_effects(switch_trials_scaled_mod_3500)

summary(switch_trials_scaled_mod_3500)
#run 200 - 4000ms model again with normal prior for ppc illustration

#run 200ms - 4000ms models, scaled and unscaled predictors
switch_trials_scaled_5000_norm <- brm(data = switch_200_5000_cut,
                                switch_response_time ~ 1 +
                                  THI_cat + 
                                  scale(age) +
                                  scale(hearThresh) +
                                  scale(DASS_D) +
                                  scale(DASS_A) +
                                  scale(DASS_S) +
                                  (1|participant),
                                file = "normal_switch_trials_scaled_preds_200_5000ms",
                                family = gaussian(),
                                iter = 6000, warmup = 500, chains = 4, cores = 4,
                                seed = 1234)

switch_trials_scaled_5000_norm <- readRDS("normal_switch_trials_scaled_preds_200_5000ms.rds")

switch_ppcheck_5000_norm <- pp_check(switch_trials_scaled_5000_norm)
switch_ppcheck_5000 <- pp_check(switch_trials_scaled_mod_5000)

switch_ppcheck_5000_norm <- switch_ppcheck_5000_norm +
  xlim(0,6500) +
  theme_cowplot()

switch_ppcheck_5000 <- switch_ppcheck_5000 +
  xlim(0,6500) +
  theme_cowplot()

switch_ppcheck_comp <- switch_ppcheck_5000_norm + switch_ppcheck_5000

ggsave("switch_5000_ppcheck_comp.tiff", switch_ppcheck_comp)

switch_5000_condeff <- plot(conditional_effects(switch_trials_scaled_mod_5000))

#tinnitus condeff plot
tinnitus_switch_condeff <- switch_5000_condeff$THI_cat +
  theme_cowplot()
ggsave("switch_condeff_tinnitus.tiff", tinnitus_switch_condeff)

#age condeff plot
age_switch_condeff <- switch_5000_condeff$age +
  theme_cowplot()
ggsave("switch_condeff_age.tiff", age_switch_condeff)

#hearing condeff plot
hearthresh_switch_condeff <- switch_5000_condeff$hearThresh +
  theme_cowplot()
ggsave("switch_condeff_hearthresh.tiff", hearthresh_switch_condeff)

#depression condeff plot
depression_switch_condeff <- switch_5000_condeff$DASS_D +
  theme_cowplot()
ggsave("switch_condeff_depression.tiff", depression_switch_condeff)
#anxiety condeff plot
anxiety_switch_condeff <- switch_5000_condeff$DASS_A +
  theme_cowplot()
ggsave("switch_condeff_anxiety.tiff", anxiety_switch_condeff)
#stress condeff plot
stress_switch_condeff <- switch_5000_condeff$DASS_S +
  theme_cowplot()
ggsave("switch_condeff_stress.tiff", stress_switch_condeff)
#make table of group-level parameter estimates
mod_200_5000_parameters <- broom::tidy(switch_trials_scaled_mod_5000)
mod_200_5000_parameters <- mod_200_5000_parameters %>% mutate_at(2:5, round, 2) %>% 
  slice(1:11)
#export parameters as table
write.csv(mod_200_5000_parameters, file = "mod_200_5000_parameters.csv", row.names = F)
