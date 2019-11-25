#EFiT DAGs - outlining variables for causal models

#load packages
library(dagitty)
library(ggdag)
library(readxl)
library(tidyverse)

d <- read_xlsx("EFiT_Main data set_31OCT2019.xlsx") %>% 
  filter(partNum != c(3, 30))

#This code can set the coordinates for the nodes in our DAG
#coords <- data.frame(matrix(c("age",0,0,
                              "hearThresh",0,1,
                              "tinOnset",0,2,
                              "tinDur",0,3,
                              "tinLat",0,4,
                              "THI",1,1,
                              "CFQ",1,2,
                              "RRS",1,4,
                              "CRIq",1,4,
                              "depression",1,0.4,
                              "anxiety",0.9,0.4,
                              "stress",0.8,0.4,
                              "EF",0,0), nrow=13, ncol=3, byrow=T))
#colnames(coords) <- c("name","x","y")

#Create DAG for EF -> t models
# Initialize the DAG. "y ~ x" means that x is a parent of y (y <- x)
EF_t_m1 <- dagify(
            THI ~ T,
            THI ~ EF,
            C2 ~ C1,
            T ~ C2,
            EF ~ C3 + C4 + C5,
            THI ~ C6 + C7,
            EF ~ C8,
            EF ~ C1,
             labels =
               c("EF" = "Executive \n function",
                 "THI" = "Tinnitus \n severity",
                 "T" = "Onset",
                 "TD" = "Duration",
                 "C1" = "Age",
                 "C2" = "Hearing \n thresholds",
                 "C3" = "Depression",
                 "C4" = "Anxiety",
                 "C5" = "Stress",
                 "C6" = "Rumination",
                 "C7" = "Cognitive \n failures",
                 "C8" = "Cognitive \n reserve"), 
              exposure = "T",
              outcome = "THI") %>% 
  tidy_dagitty()


# Plot the DAG
EF_t_m1 %>% ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point() +
  geom_dag_text() +
  scale_adjusted() +
  geom_dag_label_repel(aes(label = label),
                       nudge_y = -.1,
                       nudge_x = .25) +
  theme_void()
  

