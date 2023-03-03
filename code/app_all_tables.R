# This file replicates tables 4-8 in the appendix.

rm(list = ls())
library(rstudioapi)
library(tidyverse)
library(broom.mixed)
library(stargazer)
library(lme4)
library(sjstats)
library(optimx)
library(fixest) # cluster-robust standard errors
library(data.table)
library(lubridate)
library(modelsummary) # stargazer does not work for models with the fixest package

setwd(dirname(getActiveDocumentContext()$path))

source("code/tables/tab_rob_alldecisions.R", echo=TRUE)
source("code/tables/tab_rob_popdum.R", echo=TRUE)
source("code/tables/tab_rob_popdumall.R", echo=TRUE)
source("code/tables/tab_rob_fixedeffects.R", echo=TRUE)
source("code/tables/tab_rob_2000-21.R", echo=TRUE)
