# Loading tidyverse library
library(tidyverse)

## T-Test

# Load data from MASS into a tibble
birthwt <- as_tibble(MASS::birthwt)

birthwt

# Rename variables
birthwt <- birthwt %>%
  rename(birthwt.below.2500 = low, 
         mother.age = age,
         mother.weight = lwt,
         mother.smokes = smoke,
         previous.prem.labor = ptl,
         hypertension = ht,
         uterine.irr = ui,
         physician.visits = ftv,
         birthwt.grams = bwt)

# Change factor level names
birthwt <- birthwt %>%
  mutate(race = recode_factor(race, `1` = "white", `2` = "black", `3` = "other")) %>%
  mutate_at(c("mother.smokes", "hypertension", "uterine.irr", "birthwt.below.2500"),
            ~ recode_factor(.x, `0` = "no", `1` = "yes"))

# Check one-sided 
t.test( x = birthwt$birthwt.grams, mu = 2500)
?t.test

# Create boxplot showing how birthwt.grams varies between
# smoking status
qplot(x = mother.smokes, y = birthwt.grams,
      geom = "boxplot", data = birthwt,
      xlab = "Mother smokes", 
      ylab = "Birthweight (grams)",
      fill = I("lightblue"))

# How can we assess whether this difference is statistically significant?
birthwt %>%
  group_by(mother.smokes) %>%
  summarize(num.obs = n(),
            mean.birthwt = round(mean(birthwt.grams), 0),
            sd.birthwt = round(sd(birthwt.grams), 0),
            se.birthwt = round(sd(birthwt.grams) / sqrt(num.obs), 0))

# Use t,test 
birthwt.t.test <- t.test(birthwt.grams ~ mother.smokes, data = birthwt)
birthwt.t.test
names(birthwt.t.test)
birthwt.t.test$p.value   # p-value
birthwt.t.test$estimate  # group means
birthwt.t.test$conf.int  # confidence interval for difference

birthwt.smoke.diff <- birthwt.t.test$estimate[1] - birthwt.t.test$estimate[2]

# Density plots
qplot(fill = mother.smokes, x = birthwt.grams, data = birthwt, geom = "density", 
      alpha = I(0.5),
      adjust = 1.5)

#check normality

# qq plot
p.birthwt <- ggplot(data = birthwt, aes(sample = birthwt.grams))

p.birthwt + stat_qq() + stat_qq_line()

# Separate plots for different values of smoking status
p.birthwt + stat_qq() + stat_qq_line() + facet_grid(. ~ mother.smokes)

shapiro.test( x = birthwt$birthwt.grams )
# The W statistic has a maximum value of 1, which arises when the data look “perfectly normal”. 
# The smaller the value of W, the less normal the data are

## T-Test for non-normal distribution (Wilcox)
birthwt.wilcox.test <- wilcox.test(birthwt.grams ~ mother.smokes, data=birthwt)
birthwt.wilcox.test

## T-Test for paired
# Load Data
getwd()
setwd("/Users/hamidah/Desktop")
load( "chico.Rdata" )
chico <- chico %>% mutate(improvement = grade_test2-grade_test1)
t.test( x = chico$improvement, mu = 0)

t.test( x = chico$grade_test2, # variable 1 is the "test2" scores
        y = chico$grade_test1, # variable 2 is the "test1" scores
        paired = TRUE # paired test
        )

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
## Chi-square
#chi-square only takes input in table
  
weight.smoke.tbl <- xtabs( ~ birthwt.below.2500 + mother.smokes, data = birthwt)
weight.smoke.tbl
chisq.test(weight.smoke.tbl)


#chi-square for small sample size

salem <- load("salem.Rdata")
salem.tabs <- table( trial )  
salem.tabs

chisq.test( salem.tabs )
fisher.test( salem.tabs )

#chi-square for dependent
chico <- chico %>%
  mutate(group_test1 = ifelse(grade_test1>60,"Yes","No"),
         group_test2 = ifelse(grade_test2>60,"Yes","No"))

chico.table <- xtabs( ~ group_test1 + group_test2, data = chico)
chico.table

chisq.test( chico.table )


mcnemar.test( chico.table )


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
## ANOVA
  
birthwt %>%
  group_by(race) %>%
  summarize(mean.bwt = round(mean(birthwt.grams), 0), 
            se.bwt = round(sd(birthwt.grams) / sqrt(n()), 0))

anova <- aov(birthwt.grams ~ race, data = birthwt)
summary(anova)