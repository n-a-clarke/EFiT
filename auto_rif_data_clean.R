###automated preparation of raw RIF data
##29 November 2019
##########################################
library(tidyverse)

obs <- setdiff(1:122, c(3, 30))#change this to current participant number!

#mass import files and select the relevant information
files <- dir(pattern = "*.csv")
data <- files %>% 
  map(read_csv) %>% 
  reduce(rbind) %>% 
  select(participant, pracCat, key_resp_2.keys) %>%
  filter(pracCat == "Rp+" |
           pracCat == "Rp-" |
           pracCat == "Np")
data$key_resp_2.keys <- as.numeric(data$key_resp_2.keys)#change key presses to numeric for summing

#remove EFiT string and coerse to an integer for filtering
data$participant <- gsub(pattern = "EFiT_", x = data$participant, replacement = "")
data$participant <- gsub(pattern = "EFiT-", x = data$participant, replacement = "")
data$participant <- gsub(pattern = "EFIT_", x = data$participant, replacement = "")
data$participant <- as.integer(data$participant)

#function to sum different categories
categorySum <- function(x) {
  df <- data %>% filter(participant == x) %>% 
  group_by(pracCat) %>% 
  summarise(total = sum(key_resp_2.keys, na.rm = TRUE))
  
  data.frame(df, participant = c(rep(x, 3)))
  }

#iterate autocategory summing function over vector of participant numbers
processed_RIF <- obs %>% 
  map_df(categorySum) %>%  
  pivot_wider(names_from = pracCat, values_from = total) 

#rename last two colums
processed_RIF <- processed_RIF %>% 
  rename(Rp_minus =`Rp-`, Rp_plus =`Rp+`)

#write a .csv file
write.csv(processed_RIF, "processedRIF.csv", row.names = FALSE)
