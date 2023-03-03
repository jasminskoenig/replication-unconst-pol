rm(list=ls())


##############
# DATA
##############

# import data 
vfgh_decisions <- readRDS("data/vfgh_decisions.rds") # only cases that were decided on the merits

vfgh_decisions <- vfgh_decisions %>%
  mutate(populist = as.integer(ifelse(populist == "populist", 1, 0))) %>%
  mutate(finances = as.integer(ifelse(policyarea == "Finances", 1, 0))) %>%
  mutate(security = as.integer(ifelse(policyarea == "Security", 1, 0))) %>%
  mutate(judicature = as.integer(ifelse(policyarea == "Judicature" ,1, 0))) %>%
  mutate(migration = as.integer(ifelse(policyarea == "Migration", 1, 0))) %>%
  mutate(mobility = as.integer(ifelse(policyarea == "Mobility", 1, 0))) %>%
  mutate(economy = as.integer(ifelse(policyarea == "Economy", 1, 0))) %>%
  mutate(taxation = as.integer(ifelse(policyarea == "Taxation", 1, 0))) %>%
  mutate(social = as.integer(ifelse(policyarea == "Social", 1, 0))) %>%
  mutate(decision = as.integer(ifelse(decision == "sustained", 1, 0))) %>%
  mutate(court = as.integer(ifelse(plaintiff_category == "Court", 1, 0))) %>%
  mutate(private = as.integer(ifelse(plaintiff_category == "Person", 1, 0))) %>%
  mutate(politician = as.integer(ifelse(plaintiff_category == "Politician", 1, 0))) %>%
  mutate(erkenntnis = as.integer(ifelse(type == "erkenntnis", 1, 0))) %>%
  mutate(beschluss = as.integer(ifelse(type == "beschluss", 1, 0))) %>%
  mutate(date = year(date_2)) %>%
  mutate(lawstotal_cent = (lawstotal - mean(lawstotal,na.rm=T))/100) %>%
  filter(erkenntnis == 1) %>%
  filter(norm != "NA") 

vfgh_decisions$date <- as.factor(vfgh_decisions$date)
vfgh_decisions$populistdummy[vfgh_decisions$year<1990] <- 0 # recode FPÖ in 1980s as 'non-populist'

##############
# MODELS
##############

# only key IV + cabinet FE, se clustered by cabinet
m1fe <- feglm(decision ~ pop_origin | government, data = vfgh_decisions,family = binomial)
summary(m1fe)

# also add year FE, se clustered by cabinet
m2fe <- feglm(decision ~ pop_origin | government + date, data = vfgh_decisions,family = binomial)
summary(m2fe)

# + controls
m3fe <- feglm(decision ~ pop_origin + gov_brief+twothirds+lawstotal_cent + security+migration+social+court + politician | government + date, data = vfgh_decisions,family = binomial)
summary(m3fe) 

# create variable that indicates either security, migration or social policies
vfgh_decisions %>% mutate(pop_policy = ifelse(security==1|migration==1|social==1,1,0)) -> vfgh_decisions

# add interaction
m4fe <- feglm(decision ~ pop_origin + gov_brief+twothirds+lawstotal_cent + pop_policy + pop_origin*pop_policy +court + politician | government + date, data = vfgh_decisions,family = binomial)
summary(m4fe) 


##############
# TABLE
##############

allfemodels <- list()
allfemodels[['(1)']] <- m1fe
allfemodels[['(2)']] <- m2fe
allfemodels[['(3)']] <- m3fe
allfemodels[['(4)']] <- m4fe

msummary(allfemodels,stars=TRUE, output="results/tab_rob_fixedeffects.tex") 
