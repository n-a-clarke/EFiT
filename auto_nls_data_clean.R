###automated preparation of raw nls data
##04 December 2019
##########################################
library("tidyverse")
obs <- setdiff(1:122, c(3, 30))#change this to current participant number!

#load data
files <- dir(pattern = "*.csv")
data <- files %>% 
  map_df(read_csv, col_types = cols_only(
    "stimString" = "c",	
    "corrAns" = "c",
    "inputText" = "c",
    "key_resp_3.keys" = "?",
    "key_resp_3.rt" = "?",
    "date" = "?",
    "participant" = "?"
  ))  

#remove EFiT string and coerse to an integer for filtering
data$participant <- gsub(pattern = "EFiT_", x = data$participant, replacement = "")
data$participant <- gsub(pattern = "EFiT-", x = data$participant, replacement = "")
data$participant <- gsub(pattern = "EFIT_", x = data$participant, replacement = "")
data$participant <- gsub(pattern = "ESiT_", x = data$participant, replacement = "")
data$participant <- as.integer(data$participant)

#remove practise trials and select correct answer and input text
auto_nls <- function(x) {
temp <-  data %>% 
  filter(participant == x) %>% 
  slice(-c(1:5)) %>% 
  select(corrAns, inputText)
temp$corrAns <- str_to_lower(temp$corrAns)
#compare the two columns
temp2 <- temp %>% 
  mutate(
  compare = corrAns == inputText) %>% 
  drop_na()
#count the correct answers
temp3 <- temp2 %>% 
  summarise(nls_correct = sum(compare))

data.frame(participant = x, temp3)
}

#iterate auto_nls summing function over vector of participant numbers
all_nls <- obs %>% 
  map_df(auto_nls)

#write a .csv file
write.csv(all_nls, "processedNLS.csv", row.names = FALSE)
