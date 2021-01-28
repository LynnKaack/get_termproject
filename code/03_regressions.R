# Regression analysis

library(betareg)
library(stargazer)
library(tidyverse)
options(scipen = 999)

load("data/similarity_tfidf.RData")
load("data/entropy.RData")

merge <- similarity_df_tfidf %>% 
  left_join(entropy)
  
reg_df <- merge %>% 
  mutate(cosine_sim_inv = 1-cosine_sim,
         after_cph = ifelse(year > 2009, 1, 0),
         after_paris_lag = ifelse(year > 2016, 1, 0)) %>% 
  group_by(year) %>% 
  add_count() %>% 
  rename(doc_count = n)

# calculate cumulative sum
cum_sum <- merge %>% 
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(log_cum_sum = log(cumsum(n))) %>% 
  select(-n) %>% 
  glimpse()

# add cumulative sum
reg_df <- reg_df %>% 
  left_join(cum_sum)

# restrict to years after 2010
reg_df_short <- reg_df %>% 
  filter(year > 2010)

# whole period, controls included / beta regression
reg1 <- betareg(cosine_sim_inv ~ afterparis + articles_tot + article_ref_tot + entropy + log_cum_sum, data = reg_df)
summary(reg1)

# after 2010, controls included / beta regression
reg2 <- betareg(cosine_sim_inv ~ afterparis + articles_tot + article_ref_tot + entropy + log_cum_sum, data = reg_df_short)
summary(reg2)

stargazer(reg1, reg2, 
          type = "latex",
          title = "Regression models",
          covariate.labels = c("After COP-21", "Structural complexity (n articles)", "Relational complexity (n references)", "Entropy", "Number of previous policies (log)"),
          dep.var.labels = "Regulatory novelty (MaxSim)",
          omit.stat = c("rsq"))
