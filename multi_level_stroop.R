library(brms)
library(readxl)
library(tidyverse)

files <- dir(pattern = "*.csv")
mainSet <- read_xlsx("EFiT_Main data set_5MARCH2020.xlsx") %>%
  filter(partNum != 3, partNum != 30) %>% 
  rename(participant = partNum)
  
df <- files %>%  map_df(read_csv, col_types = cols_only("word" = "?",                     
                                                        "colour" = "?",              
                                                        "congruent" = "f",                
                                                        "corrAns" = "?",                  
                                                        "practiseTrials.thisRepN" = "?",  
                                                        "practiseTrials.thisTrialN" = "?",
                                                        "practiseTrials.thisN" = "?",   
                                                        "practiseTrials.thisIndex" = "?", 
                                                        "mainTrialsLoop.thisRepN" = "?",  
                                                        "mainTrialsLoop.thisTrialN" = "?",
                                                        "mainTrialsLoop.thisN" = "?",     
                                                        "mainTrialsLoop.thisIndex" = "?", 
                                                        "b_key_resp_6_.keys" = "?",       
                                                        "b_key_resp_6_.corr" = "?",      
                                                        "b_key_resp_6_.rt" = "?",         
                                                        "key_resp_5.keys" = "?",          
                                                        "key_resp_5.corr" = "?",          
                                                        "key_resp_5.rt" = "?",            
                                                        "date" = "?",                     
                                                        "frameRate" = "?",                
                                                        "expName" = "?",                  
                                                        "session" = "c",                  
                                                        "participant" = "?"))


#change character formatting for easier manipulation        

df$participant <- gsub(pattern = "EFiT_", x = df$participant, replacement = "")
df$participant <- as.integer(df$participant)#remove EFiT sting and coerse to an integer for filtering
df$congruent <- gsub(pattern = "1.0", x = df$congruent, replacement = "1")
df$congruent <- gsub(pattern = "0.0", x = df$congruent, replacement = "0")

df <- left_join(df, mainSet, by = "participant")

df <- df %>% 
  filter(is.na(practiseTrials.thisTrialN ))#filter out practise trials

df$participant <- as.factor(df$participant)

df <- df %>% 
  mutate(
    THI_cat = ifelse(
      THI >= 38 ,"modsev", ifelse(
        THI > 0 & THI <= 37, "mild", "none"
      ))) #add a categorical variable with the groups split by THI scores

incongruentDF <- df %>% filter(congruent == 0, key_resp_5.corr == 1)#filter to obtain incongruent, correct responses

incongruentDF$THI_cat <- as.factor(incongruentDF$THI_cat)
incongruentDF$THI_cat <- ordered(incongruentDF$THI_cat, levels = c("none", "mild", "modsev"))

test <- brm(data = incongruentDF,
    key_resp_5.rt ~ 1 + THI_cat + scale(age) + scale(hearThresh) + scale(DASS_D) + scale(DASS_A) + scale(DASS_S) + (1|participant),
    file = "stroop_incon",
    family = shifted_lognormal(),
        iter = 6000, warmup = 500, chains = 4, cores = 4,
    seed = 1234)

test2 <- readRDS(file = 'stroop_incon.rds')

marginal_effects(test2)

hypothesis(test, 'THI_cat.L  > Intercept')  # Numerically

tidy(test)
summary(test)
plot(test)

pp_check(test)
ranef(test)
abc <- plot(marginal_effects(test2))

abc$THI_cat +
  ylab("response time") +
  xlab("group")+
  cowplot::theme_minimal_grid()
