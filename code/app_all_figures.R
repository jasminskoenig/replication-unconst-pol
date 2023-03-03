# APPENDIX ----

# This file replicates figures 2-5 in the main text.

rm(list = ls())

library(rstudioapi)
library(tidyverse)
library(vdemdata)
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(readxl)

setwd(dirname(getActiveDocumentContext()$path))

source("code/figures/fig_vdem.R", echo=TRUE)
source("code/figures/fig_trust.R", echo=TRUE)
source("code/figures/fig_vparty.R", echo=TRUE)
source("code/figures/fig_review1.R", echo=TRUE)
source("code/figures/fig_review2.R", echo=TRUE)
source("code/figures/fig_review3.R", echo=TRUE)

