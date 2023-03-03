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
  #filter(erkenntnis == 1) %>%
  filter(norm != "NA") 

vfgh_decisions$date <- as.factor(vfgh_decisions$date)
vfgh_decisions$populistdummy[vfgh_decisions$year<1990] <- 0 # recode FPÖ in 1980s as 'non-populist'



##############
# MODELS
##############

# only key IV
m1all <- glm(decision ~ pop_origin, data = vfgh_decisions,family = binomial)

# random intercepts for year and cabinet
m2all <- glmer(formula=decision ~ 1 + (1|date) + (1|government) + pop_origin,
                family=binomial(link="logit"),
                data=vfgh_decisions,
                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

# include control variables
m3all <- glmer(formula=decision ~ 1 + (1|date) + (1|government) +
pop_origin + lawstotal_cent + gov_brief+twothirds+security+migration+social+court + politician,
                family=binomial(link="logit"),
                data=vfgh_decisions,
                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))


# create variable that indicates either security, migration or social policies
vfgh_decisions %>% mutate(pop_policy = ifelse(security==1|migration==1|social==1,1,0)) -> vfgh_decisions

# add interaction term
m5all <- glmer(formula=decision ~ 1 + (1|date) + (1|government) +
pop_origin + lawstotal_cent + gov_brief+twothirds+pop_policy+
pop_origin*pop_policy +court + politician,
                family=binomial(link="logit"),
                data=vfgh_decisions,
                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))


##############
# TABLE
##############

tidym1all <- broom::tidy(m1all)
tidym2all <- broom::tidy(m2all) %>% filter(effect=="fixed")
tidym3all <- broom::tidy(m3all) %>% filter(effect=="fixed")
tidym5all <- broom::tidy(m5all) %>% filter(effect=="fixed")

# counts of random effects
num_date <- nrow(ranef(m3all)$date)
num_gov <- nrow(ranef(m3all)$government)

# standard deviation of random effects
sd_datem5 <- round(attributes(VarCorr(m5all)$"date")$stddev, 3)
sd_govm5 <- round(attributes(VarCorr(m5all)$"government")$stddev, 3)
sd_datem3 <- round(attributes(VarCorr(m3all)$"date")$stddev, 3)
sd_govm3 <- round(attributes(VarCorr(m3all)$"government")$stddev, 3)
sd_datem2 <- round(attributes(VarCorr(m2all)$"date")$stddev, 3)
sd_govm2 <- round(attributes(VarCorr(m2all)$"government")$stddev, 3)

tribble(~stat, ~m1all, ~m2all, ~m3all, ~m5all, 
        "Number of Years", NA, num_date, num_date, num_date, 
        "Number of Governments", NA, num_gov, num_gov, num_gov,
        "sd(Year)", NA, sd_datem2, sd_datem3, sd_datem5,
        "sd(Government)", NA, sd_govm2, sd_govm3, sd_govm5,
        "", NA, NA, NA, NA,
        "N", nobs(m1all), nobs(m2all), nobs(m3all), nobs(m5all)) -> mod_stats


# create table
stargazer(m1all, m2all, m3all, m5all, type="latex", 
          # Below: manually supply tidied coefficients and standard errors
          coef = list(tidym1all$estimate, tidym2all$estimate, tidym3all$estimate, tidym5all$estimate),
          se = list(tidym1all$std.error, tidym2all$std.error, tidym3all$std.error, tidym5all$std.error),
          omit=c("date","government"),
          # Omit model statistics by default...
          omit.table.layout = "s",
          # ...but supply your own that you created (with random effects)
          add.lines = lapply(1:nrow(mod_stats), function(i) unlist(mod_stats[i, ])),
          #covariate.labels = c("Populist","Gov Brief","2/3 Majority", "Total Laws", "Security", "Migration","Social","Plaintiff (Court)","Abstract Review"),
          #notes="<small>Data: ESS, Round 9 (United Kingdom)</small>",
          dep.var.labels="Decision",
          model.names = FALSE,
          out="results/tab_rob_alldecisions.tex"
          )