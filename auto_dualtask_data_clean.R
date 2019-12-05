###automated preparation of raw dual task data
##04 December 2019
##########################################
library("tidyverse")
obs <- setdiff(1:122, c(3, 30))#change this to current participant number!

#load data
files <- dir(pattern = "*.csv")

df <- files %>% 
  map_df(read_csv, col_types = cols_only(
  "stimList" = "?",
  "stimString" = "?",	
  "EasyLoop.thisRepN" = "?",
  "EasyLoop.thisTrialN" ="?",	
  "EasyLoop.thisN" = "?",
  "EasyLoop.thisIndex" = "?",
  "HardLoop.thisRepN" = "?",
  "HardLoop.thisTrialN" = "?",
  "HardLoop.thisN" = "?",
  "HardLoop.thisIndex" = "?",
  "key_resp_2.keys" = "?",
  "key_resp_2.rt" = "?",
  "inputText" = "c",
  "key_resp_3.keys" = "?",	
  "key_resp_3.rt" = "?",	
  "date" = "?",
  "frameRate" = "?",	
  "expName" = "?",
  "session" = "c",
  "participant" = "c"))


#remove EFiT string and coerse to an integer for filtering
df$participant <- gsub(pattern = "EFiT_", x = df$participant, replacement = "")
df$participant <- gsub(pattern = "EFiT-", x = df$participant, replacement = "")
df$participant <- gsub(pattern = "EFIT_", x = df$participant, replacement = "")
df$participant <- as.integer(df$participant)

#load data
autoDual <- function(x) {
  temp <- df %>% 
  filter(participant == x) %>% 
  select(stimString, inputText)
#compare the columns
temp2 <- temp %>% mutate(
  compare = stimString == inputText)
#add together
temp3 <- temp2 %>% summarise(
  total = sum(compare, na.rm = TRUE)
)
data.frame(participant = x, temp3)
}

allDual <- obs %>% map_df(autoDual)

#rename to something sensible for identification when joining later

allDual <- allDual %>% rename(dual_task_total= total)

#write .csv
write.csv(allDual, "processedDual.csv", row.names = FALSE)
