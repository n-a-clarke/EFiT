library(trimr)
library(tidyverse)

files <- dir(pattern = "*.csv")
z <- setdiff(1:122, c(3, 30))#change 2nd number this to current participant number!
z <- as.list(z)


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


autoTrim <- function(x) {
  stroop <- df %>% 
  filter(participant == x) %>% #replace 1 with x
  slice(-c(1:15)) %>% 
  select(-participant, key_resp_5.corr, key_resp_5.rt)
  

q <- rep(x, length(stroop$key_resp_5.rt)) #replace 1 with x
necessary <- rep("TrimmedRT", length(stroop$key_resp_5.rt))#create "condition" column of same length as rts
stroop <- cbind(stroop, q)
stroop <- cbind(stroop, necessary)
stroop2 <- stroop %>% select("rt" = key_resp_5.rt, "accuracy" = key_resp_5.corr, "participant" = q, "condition" = necessary, congruent)

#group by congruency (incongruent, congruent, and neutral)
#incongruent
stroopIncon <- stroop2 %>% 
  filter(congruent == "0")
  
incon_trimmed <- sdTrim(data = stroopIncon, minRT = .200, sd = 2.5, omitErrors = F, returnType = "mean",digits = 3, perCondition = T)#sd trimming)

incon_trimmed <- incon_trimmed %>% 
  mutate(congruency = "incongruent")
#congruent
stroopCon <- stroop2 %>% 
  filter(congruent == "1")

con_trimmed <- sdTrim(data = stroopCon, minRT = .200, sd = 2.5, omitErrors = F, returnType = "mean",digits = 3, perCondition = T)#sd trimming)

con_trimmed <- con_trimmed %>% 
  mutate(congruency = "congruent")
#neutral
stroopNeutral <- stroop2 %>% 
  filter(congruent == "None")

neutral_trimmed <- sdTrim(data = stroopNeutral, minRT = .200, sd = 2.5, omitErrors = F, returnType = "mean",digits = 3, perCondition = T)#sd trimming)

neutral_trimmed <- neutral_trimmed %>% 
  mutate(congruency = "neutral")


trimmed <- rbind(
  incon_trimmed, con_trimmed, neutral_trimmed)  

   
trimmed
  }

stroopErrors <- function(x) {
errorStroop <- df %>% 
  filter(participant == x) %>% #replace 1 with x
  slice(-c(1:15)) %>% 
  select(-participant, key_resp_5.corr, key_resp_5.rt)

#errors
stroopErrors <- 150 - sum(errorStroop$key_resp_5.corr)

stroopErrors <- tibble(participant = x, stroopErrors = stroopErrors)

stroopErrors
}

stroopErrors(6)




stroopRT <- map_df(z, autoTrim)
stroopRT <- stroopRT %>% spread(congruency,TrimmedRT)

errors <- map_df(z, stroopErrors)

stroopRTError <- stroopRT %>% left_join(errors, by = "participant")

#rename for joing later
stroopRTError <- stroopRTError %>% 
  rename(stroop_congruent = congruent,
         stroop_incongruent = incongruent,
         stroop_neutral = neutral,
         stroop_errors = stroopErrors)


write.csv(stroopRTError, "processedStroop.csv",
          row.names = FALSE)
