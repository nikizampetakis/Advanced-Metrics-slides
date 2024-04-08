library(tidyverse)
library(skimr)
library(fixest)


####################
#     Task 1
####################

#Q1 Load data

otc = read_csv(url("https://raw.githubusercontent.com/nikizampetakis/Advanced-Metrics-slides/master/lectures/11-structural/otc_data_merged.csv"))

#Q2 skim the data

skim(otc)

# #mg

unique(otc$mg)

# Q3 Price differentitaion

otc %>% group_by(brand_name, mg) %>% 
  filter(prom_ == 0) %>%
  summarise(avg_price = mean(price_))

####################
#     Task 2
####################

#Q1

otc <- otc %>% mutate(share = sales_/count) %>% group_by(store, week) %>%
  mutate(ssh = sum(share)) %>% mutate(outside_good = 1 - ssh)

#Q2

otc = otc  %>% 
  group_by(store, week) %>%
  mutate(delta = log(share) - log(outside_good))

#Q3

ols <- lm(delta ~ price_ + prom_ + size, data = otc)

summary(ols)

#Q4

otc = otc  %>% 
  group_by(store, week) %>%
  mutate(market_id = cur_group_id())

#Q5

fe = feols(delta ~ price_ + prom_ + size | brand_name + market_id, data = otc)

etable(fe, tex = F)


#Q6

iv = feols(delta ~ prom_ + size | price_ ~ avoutprice, data = otc)

#Q7

ivfe = feols(delta ~ prom_ | brand_name + market_id | price_ ~ avoutprice, data = otc)

etable(iv, ivfe, tex = F)

#Q8

otc = otc %>% 
  mutate(own_elast = ivfe$coeftable[1,1]*price_*(1 - share))

ggplot(otc, aes(x = own_elast)) + 
  geom_histogram(binwidth=0.1, color="black", fill="green") +
  geom_vline(xintercept = 0.0, linetype="dashed", color = "red", size = 1.5) +
  theme_bw()
  
####################
#     Task 3
####################

#Q1

otc = otc %>%
  mutate(pcm = (price_ - cost_)/price_)

#Q2

otc = otc %>%
  mutate(pcm_b = 1/abs(own_elast))

#Q3

otc %>% 
  group_by(brand_name) %>% 
  summarise(mean_pcm = mean(pcm), mean_pcm_b = mean(pcm_b))














  