
remotes::install_github("b-rodrigues/diffindiff")

library(diffindiff)
library(tidyverse)

####################
#    Load data
####################

ck1994 <- njmin

ck1994 %>%
  select(sheet,chain,state,observation,empft,emppt) %>% 
  head()

####################
#      Task 1
####################


#Q1

str(ck1994)

#Q2

ck1994 %>% group_by(state, observation) %>% summarise(n_stores = n_distinct(sheet))

#Q3

ck1994 = ck1994 %>% ungroup %>% mutate(empfte = empft + 0.5*emppt + nmgrs)

#Q4

did = ck1994 %>% group_by(state, observation) %>% summarise(avg_fte = mean(empfte, na.rm = T),
                                                            pc_ft = mean(empft/empfte, na.rm = T)*100,
                                                            avg_w = mean(wage_st, na.rm = T))

####################
#      Task 2
####################

#Q1

ck1994 = ck1994 %>% mutate(treat = case_when(state == "Pennsylvania" ~ F,
                                            state == "New Jersey" ~ T))

#Q2

ck1994 = ck1994 %>% mutate(post = case_when(observation == "February 1992" ~ F,
                                             observation == "November 1992" ~ T))
#Q3

did_reg = lm(empfte ~ treat*post, data = ck1994)

summary(did_reg)


