# This file replicates tables 1-3 in the main text.

rm(list = ls())
library(rstudioapi)
library(tidyverse)
library(broom.mixed)
library(stargazer)
library(lme4)
library(sjstats)
library(optimx)
library(data.table)
library(lubridate)
library(knitr)

setwd(dirname(getActiveDocumentContext()$path))

source("code/tables/tab_regressionresults.R", echo=TRUE)
source("code/tables/tab_suedlaws.R", echo=TRUE)
