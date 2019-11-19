#EFiT DAGs - outlining variables for causal models

#load packages
library(dagitty)
library(ggdag)
library(readxl)
library(tidyverse)

d <- read_xlsx("EFiT_Main data set_31OCT2019.xlsx") %>% 
  filter(partNum != c(3, 30))

# Set the coordinates for the nodes in our DAG, this step is only for 
coords <- data.frame(matrix(c("age",0,0,
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
colnames(coords) <- c("name","x","y")

#Create DAG for EF -> t models
# Initialize the DAG. "y ~ x" means that x is a parent of y (y <- x)
m1 <- dagify(EF ~ THI,
             labels =
               c("EF" = "Executive \n function",
                 "THI" = "Tinnitus \n severity"), 
              latent = 
               c("EF, THI"),
              exposure = "EF",
              outcome = "THI") %>% 
  tidy_dagitty()



# Plot the DAG
m1 %>% ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point() +
  geom_dag_text() +
  scale_adjusted() +
  geom_dag_label_repel(aes(label = label, fill = label),
                       col = "white", show.legend = FALSE) 

 ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_node(
    color = "black",
    alpha = 0.8
  ) +
  geom_dag_label_repel(aes(label = label),
                       col = "white",
                       label.size = .4,
                       fill = "#20a486ff",
                       alpha = 0.8,
                       show.legend = FALSE,
                       nudge_x = .7,
                       nudge_y = .3
  ) +
  theme_void()
