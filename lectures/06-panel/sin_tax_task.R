library(AER)
library(tidyverse)
library(skimr)
library(fixest)


####################
#    Load data
####################

data(Fatalities)

####################
# Skim the dataset
####################

skim(Fatalities)

########################################
#  Create the fatality rate variable
########################################

Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000

Fatalities = Fatalities %>% mutate(fatal_rate = fatal/pop*1000)

####################
# Pooled regression
####################

lm1 = lm(fatal_rate ~ beertax, Fatalities)

summary(lm1)

#Plot the regression line

# Higher beer taxes mean higher fatalitites rate. This is counter intuitive.

ggplot(Fatalities, aes(x = beertax, y = fatal_rate)) +
  geom_point() +
  geom_abline(slope = lm1$coefficients[2],
              intercept = lm1$coefficients[1],
              color = "red",
              size = 1)

ggplot(Fatalities, aes(x = beertax, y = fatal_rate)) +
  geom_point() +
  geom_smooth(method='lm', se = F)

####################
# Add more controls
####################

#Create a dummy var punish when there is jail or community work time when busted drunk driving

Fatalities = Fatalities %>% mutate(punish = if_else((jail == "yes" | service == "yes"), 1, 0))

#Create a categorical variable for the minimum drinking age at each state

Fatalities = Fatalities %>% 
  mutate(drinkagec = cut(drinkage,breaks = 18:22, include.lowest = TRUE, right = FALSE))

Fatalities$drinkagec = relevel(Fatalities$drinkagec, "[21,22]")

#Regression

lm2 = lm(fatal_rate ~ beertax + drinkagec + punish + miles + unemp + log(income), Fatalities)

summary(lm2)

#Again the "effect" of beertax is positive even if we control for observable characteristics of the states.
#We can control for unobserved charactersitics that might be constant through time or in a state e.g.
#populations’ attitude towards drunk driving.

########################################
#     Fixed effects regression
########################################

lm3 = feols(fatal_rate ~ beertax + drinkagec + punish + miles + unemp + log(income)|
              year + state, Fatalities, vcov = "hetero")

lm4 = feols(fatal_rate ~ beertax + drinkagec + punish + miles + unemp + log(income)|
              year + state, Fatalities, vcov = ~state)

lm5 = feols(fatal_rate ~ beertax + drinkagec + punish + miles + unemp + log(income)|
                    year + state, Fatalities)

lm6 = feols(fatal_rate ~ beertax + drinkagec + punish + miles + unemp + log(income)|
              year + state, Fatalities, cluster = c("year", "state"))

etable(lm3, lm4, lm5, lm6)









