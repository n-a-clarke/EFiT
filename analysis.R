#EFiT data analysis - 25/11/2019

#load packages and data
library(dagitty)
library(skimr)
library(bayestestR)
library(ggdag)
library(readxl)
library(tidyverse)

d <-read_xlsx("EFiT_Main data set_31OCT2019.xlsx") # load data

skim(d)
