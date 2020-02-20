
library("tidyverse")
library("readxl")

din <- read_excel("Table2_updated.xlsx", skip = 1)
colnames(din)

dw <- din %>% 
  select(Lab, 
         `Total Included...8`, `Correct...9`, 
         `Total Included...16`, `Correct...17`) %>% 
  rename(n_c = `Total Included...8`, 
         s_c = `Correct...9`, 
         n_e = `Total Included...16`, 
         s_e = `Correct...17`) 
ns <- dw %>% 
  select(Lab, starts_with("n")) %>% 
  pivot_longer(cols = starts_with("n"), 
               names_to = "condition", 
               values_to = "n") %>% 
  mutate(condition = substr(condition, 3, 3))

ss <- dw %>% 
  select(Lab, starts_with("s")) %>% 
  pivot_longer(cols = starts_with("s"), 
               names_to = "condition", 
               values_to = "s") %>% 
  mutate(condition = substr(condition, 3, 3))

d2 <- left_join(ns, ss) %>% 
  mutate(condition = factor(condition, levels = c("c", "e")), 
         Lab = factor(Lab, levels = unique(d2$Lab)))
levels(d2$Lab) <- LETTERS[1:24]

library("rstanarm")

m1 <- stan_glmer(cbind(s, n - s) ~ condition + (condition|Lab), d2, 
                 family = binomial(link = "probit"))

m1
summary(m1)
VarCorr(m1)

pars <- names(m1$coefficients)[1:2]
round(posterior_interval(m1, pars = pars, prob = 0.5), 2)

posterior_interval(m1, pars = "Sigma[Lab:conditione,conditione]", prob = 0.95)

plot(m1, pars = pars)
plot(m1, regex_pars = "b\\[conditione Lab")
plot(m1, regex_pars = "b\\[\\(Intercept\\) Lab")

post <- as.matrix(m1)
str(post)

ci <- names(m1$coefficients)[str_detect(names(m1$coefficients), "b\\[conditione Lab")]

np <- post[,ci] + post[,2]
str(np)

## shows: every study shows effect!
bayesplot::mcmc_intervals(np, prob_outer = 0.99, prob = 0.8)


### not needed and stupid 
# m2 <- stan_glmer(cbind(s, n - s) ~ 0 + condition + (0 + condition|Lab), d2, 
#                  family = binomial(link = "probit"))
# m2
# 
# 
# names(m2$coefficients)
