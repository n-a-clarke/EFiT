library(tidyverse)
###DON'T IGNORE THIS FIRST LINE###
obs <- setdiff(1:122, c(3, 30))#change this to current participant number!

#read in all the files individually using read_csv then reduce with rbind into a single data frame
files <- dir(pattern = "*.csv")
data <- files %>% 
  map(read_csv) %>% 
  reduce(rbind) 
#remove EFiT sting and coerse to an integer for filtering
data$participant <- gsub(pattern = "EFiT_", x = data$participant, replacement = "")
data$participant <- gsub(pattern = "EFiT-", x = data$participant, replacement = "")
data$participant <- as.integer(data$participant)

#this function prints all the rule switch RTs: if the cell above is different to the cell being compared then print the response time for that row
switchCost <- function(z) {
  
  df <<- df
  if(df$rule[z] != df$rule[z-1]) {
    df$mainResp.rt[z]
  }
} 
nonSwitch <- function(z) {
  
  df <<- df
  if(df$rule[z] == df$rule[z-1]) {
    df$mainResp.rt[z]
  }
} 

#create a vector for switchCost function to iterate over
df <- data %>% 
  filter(participant == 1) %>% 
  slice(19:198)
x <- c(2:length(df$rule))

### try and make from scratch

switchMean <- function(q) {
df <<- data %>% 
  filter(participant == q) %>% 
  slice(19:198)

allSwitch <- map(x, switchCost)#iterate over
allSwitch <- unlist(allSwitch)#unlist to vector for mean
  allMean <- mean(allSwitch)
  comMean <- tibble(participant = q, allMean)
  as.tibble(comMean)
comMean
}


test <- switchMean(122)#test the function

#iterate over obs and bind each observation
allSwitchMeanOut <- obs %>%  map_df(switchMean) %>% rbind()

nonSwitchMean <- function(q) {
  df <<- data %>% 
    filter(participant == q) %>% 
    slice(19:198)
  
  allNonSwitch <- map(x, nonSwitch)#iterate over
  allNonSwitch <- unlist(allNonSwitch)#unlist to vector for mean
  allNonMean <- mean(allNonSwitch)
  comNonMean <- tibble(participant = q, allNonMean)
  as.tibble(comNonMean)
  comNonMean
}

test2 <- nonSwitchMean(1)#test the function

allNonSwitchMeanOut <- obs %>%  map_df(nonSwitchMean) %>% rbind()

#join together
switchCostMeans <- left_join(allSwitchMeanOut, allNonSwitchMeanOut, by = "participant") %>% 
  rename(switch = allMean, nonSwitch = allNonMean) %>% 
  mutate(switchCost = switch - nonSwitch)

#create function for sum of errors for individual
switchErrors <- function(q) {
  df <- data %>% 
    filter(participant == q) %>% 
    slice(19:198) %>% 
    select(mainResp.corr, participant)
  
errors <- sum(df$mainResp.corr)
comNonMean <- tibble(participant = q, ruleErrors = 180 - errors)
comNonMean
}


switchErrors(6)


allSwitchErrors <- obs %>%  map_df(switchErrors) %>% rbind()

switchCostMeansErrors <- left_join(switchCostMeans, allSwitchErrors, by = "participant")

#rename for joining later
switchCostMeansErrors <- switchCostMeansErrors %>% 
  rename(switch_trials = switch,
         none_switchtrials = nonSwitch,
         switch_cost = switchCost,
         rule_errors = ruleErrors)

write.csv(switchCostMeansErrors, "processedRule.csv",
          row.names = FALSE)

