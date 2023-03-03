# This file replicates figures 2-5 in the main text.

rm(list = ls())
library(rstudioapi)
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(vdemdata)

setwd(dirname(getActiveDocumentContext()$path))

source("code/figures/fig_totalcomplaints.R", echo=TRUE)
source("code/figures/fig_lawsovertime.R", echo=TRUE)
source("code/figures/fig_lawsreviewed_bypop.R", echo=TRUE)
source("code/figures/fig_lawsreviewed_byplaintiff.R", echo=TRUE)

