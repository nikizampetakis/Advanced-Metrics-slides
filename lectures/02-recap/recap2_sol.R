
library(tidyverse)
library(skimr)
library(wooldridge)

#Task 1

data("wage1")

wage1 %>% skim()

summary(lm(wage~educ + tenure, data = wage1))

#Task 2

summary(lm(wage ~ female, data = wage1))

summary(lm(wage ~ female + married, data = wage1))

#Task 3

lm1 = lm(wage ~ female*exper, data = wage1)

summary(lm1)

summary(lm(wage ~ exper + female + female:exper, data = wage1))

summary(lm(wage ~ female:exper, data = wage1))

male5 = tibble(female = 0, exper = 5)

predict(lm1, male5)

#Task 4

summary(lm(lwage ~ educ + tenure, data = wage1))

(exp(0.086528) - 1)*100

#Task 5

data("hprice1")

summary(lm(lprice ~ lsqrft, data = hprice1))


















