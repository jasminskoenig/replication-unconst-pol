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
m1dumall <- glm(decision ~ populistdummy, data = vfgh_decisions,family = binomial)


# random intercepts for year and cabinet
m2dumall <- glmer(formula=decision ~ 1 + (1|date) + (1|government) + populistdummy,
                family=binomial(link="logit"),
                data=vfgh_decisions,
                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

# include control variables
m3dumall <- glmer(formula=decision ~ 1 + (1|date)  + (1|government) +
populistdummy + lawstotal_cent + gov_brief+twothirds+security+migration+social+court + politician,
                family=binomial(link="logit"),
                data=vfgh_decisions,
                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

# create variable that indicates either security, migration or social policies
vfgh_decisions %>% mutate(pop_policy = ifelse(security==1|migration==1|social==1,1,0)) -> vfgh_decisions

# add interaction term
m5dumall <- glmer(formula=decision ~ 1 + (1|date)  + (1|government) +
populistdummy + lawstotal_cent + gov_brief+twothirds+pop_policy+
populistdummy*pop_policy +court + politician,
                family=binomial(link="logit"),
                data=vfgh_decisions,
                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))


##############
# TABLE
##############

tidym1dumall <- broom::tidy(m1dumall)
tidym2dumall <- broom::tidy(m2dumall)%>% filter(effect=="fixed")
tidym3dumall <- broom::tidy(m3dumall)%>% filter(effect=="fixed")
tidym5dumall <- broom::tidy(m5dumall)%>% filter(effect=="fixed")

# counts of random effects
num_date <- nrow(ranef(m3dumall)$date)
num_gov <- nrow(ranef(m3dumall)$government)

# standard deviation of random effects
sd_datem5dumall <- round(attributes(VarCorr(m5dumall)$"date")$stddev, 3)
sd_govm5dumall <- round(attributes(VarCorr(m5dumall)$"government")$stddev, 3)
sd_datem3dumall <- round(attributes(VarCorr(m3dumall)$"date")$stddev, 3)
sd_govm3dumall <- round(attributes(VarCorr(m3dumall)$"government")$stddev, 3)
sd_datem2dumall <- round(attributes(VarCorr(m2dumall)$"date")$stddev, 3)
sd_govm2dumall <- round(attributes(VarCorr(m2dumall)$"government")$stddev, 3)



tribble(~stat, ~m1dumall, ~m2dumall, ~m3dumall, ~m5dumall, 
        "Number of Years", NA, num_date, num_date, num_date, 
        "Number of Governments", NA, num_gov, num_gov, num_gov, 
        "sd(Year)", NA, sd_datem2dumall, sd_datem3dumall, sd_datem5dumall,
        "sd(Government)", NA, sd_govm2dumall, sd_govm3dumall, sd_govm5dumall,
        "", NA, NA, NA, NA,
        "N", nobs(m1dumall), nobs(m2dumall), nobs(m3dumall), nobs(m5dumall)) -> mod_stats


# create table
stargazer(m1dumall, m2dumall, m3dumall, m5dumall, type="latex", 
          coef = list(tidym1dumall$estimate, tidym2dumall$estimate, tidym3dumall$estimate, tidym5dumall$estimate),
          se = list(tidym1dumall$std.error, tidym2dumall$std.error, tidym3dumall$std.error, tidym5dumall$std.error),
          omit=c("date","government"),
          omit.table.layout = "s",
          add.lines = lapply(1:nrow(mod_stats), function(i) unlist(mod_stats[i, ])),
          dep.var.labels="Decision",
          model.names = FALSE,
          out="results/tab_rob_popdumall.tex"
          )