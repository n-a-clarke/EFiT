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
m1 <- dagify(THI ~ EF,
             EF ~ stress, EF ~ anxiety, EF ~ depression,
              exposure = "EF",
              outcome = "THI",
              coords=coords)

# Plot the DAG
ggdag(m1)
