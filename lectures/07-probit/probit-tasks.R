library(tidyverse)
library(stargazer)
library(skimr)
library(ROCR)

#TASK 1

data(mroz, package = "wooldridge")


#Q1/ Q2

str(mroz)

#Q3

sum(mroz$inlf[which(mroz$inlf == 1)])/nrow(mroz)

length(which(mroz$inlf == 1))/nrow(mroz)

#Q4

mean(mroz$inlf)

#Q5

sum(mroz$inlf[which(mroz$inlf == 1 & mroz$kidslt6 == 1)])/nrow(mroz)


#TASK 2

#Q1

mroz = mroz %>%
  mutate(age_lt_50 = case_when(age < 50 ~ 1,
                               age >= 50 ~ 0)) %>%
  mutate(husage_lt_50 = case_when(husage < 50 ~ 1,
                                  husage >= 50 ~ 0))


#Q2

lpm = lm(inlf ~ age_lt_50*husage_lt_50, mroz)

stargazer(lpm, type = "text")

#Q3

mroz$pred = predict(lpm)

#Q4

pred_df = tibble(age_lt_50 = 1, husage_lt_50 = 1)

predict(lpm, pred_df)

#Q5

ggplot(mroz[order(mroz$pred),], aes(x = 1:nrow(mroz),y = pred, color = age_lt_50)) + 
  geom_point() + 
  theme_bw() + 
  scale_y_continuous(limits = c(0,1), name = "p(inlf)")

#TASK 3

#Q1
data(SwissLabor, package = "AER")

#Q2
skim(SwissLabor)

#Q3

f <- "participation ~ income + education + youngkids + oldkids + foreign + age + I(age^2)"

glms <- list()

glms$probit <- glm(formula = f, 
                   data = SwissLabor, 
                   family = binomial(link = "probit"))

stargazer(glms$probit, type = "text")


#Q4

glms$probitMean <- mfx::probitmfx(formula = f, 
                                  data = SwissLabor, atmean = TRUE)

glms$probitMean

#Q5

pred <- ROCR::prediction(fitted(glms$probit), SwissLabor$participation)
plot(performance(pred,"tpr","fpr"))
abline(0,1,lty = 2, col = "red")






