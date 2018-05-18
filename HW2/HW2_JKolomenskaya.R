library("tidyverse")
library("readr")
library("dplyr")

install.packages("lingtypology")
library(lingtypology)
install.packages('lme4')
library(lme4)
library(vcd)
install.packages("rcompanion")
library("rcompanion")
library("ggplot2")

#1.0 Read the data from file to the variable shva

shva <- read.csv("https://raw.githubusercontent.com/agricolamz/2018-MAG_R_course/master/data/duryagin_ReductionRussian.txt", sep = '\t')

head(shva)

#1.1 Scatterplot f1 and f2 using ggplot()

ggplot(shva, aes(x=f2, y=f1, colour = vowel)) +
  geom_point() +
  scale_y_continuous(trans = "reverse") +
  scale_x_continuous(trans = "reverse") +
  theme(legend.position="none") +
  ggtitle("f2 and f1 of the redused and stressed vowels")

#1.2 Plot the boxplots of f1 and f2 for each vowel using ggplot()

p1 <- ggplot(shva, aes(x=vowel, y=f1, fill = vowel)) +
    geom_boxplot() +
    coord_flip() +
    theme(legend.position="none",
          axis.title.y = element_blank()) +
    ggtitle("f1 distribution in each vowel")

p1

p2 <- ggplot(shva, aes(x=vowel, y=f2, fill = vowel)) +
    geom_boxplot() +
    coord_flip() +
    theme(legend.position="none",
          axis.title.y = element_blank()) +
    ggtitle("f2 distribution in each vowel")

p2


#1.3 Which f1 can be considered outliers in a vowel?

ggplot_build(p1)$data

    # f1 = 826, f1 = 679 for a

#1.4 Calculate Pearson’s correlation of f1 and f2 (all data)

cor_all <- cor(shva$f1, shva$f2, method = "pearson")
print(cor_all)

#1.5 Calculate Pearson’s correlation of f1 and f2 for each vowel

a <- shva %>%
      filter(vowel == "a")

A <- shva %>%
  filter(vowel == "A")

y <- shva %>%
  filter(vowel == "y")

cor_a <- cor(a$f1, a$f2, method = "pearson")
print(cor_a)

cor_A <- cor(A$f1, A$f2, method = "pearson")
print(cor_A)

cor_y <- cor(y$f1, y$f2, method = "pearson")
print(cor_y)

#1.6 Use the linear regression model to predict f2 by f1
#1.6.1 Provide the result regression formula

fit1 <- lm(f2~f1, data = shva)
summary(fit1)

#1.6.2 Provide the adjusted R2

  #Adjusted R-squared:  0.3319

#1.6.3 Add the regression line in scatterplot 1.1

ggplot(shva, aes(x=f2, y=f1, colour = vowel)) +
  geom_point() +
  scale_y_continuous(trans = "reverse") +
  scale_x_continuous(trans = "reverse") +
  geom_line(mapping = aes(x = shva$f2, y = shva$f1),
            data = shva,
            stat= "smooth",
            method = "lm",
            color = "grey") +
  theme(legend.position="none") +
  ggtitle("f2 and f1 of the redused and stressed vowels")

#1.7 Use the mixed-efects model to predict f2 by f1 using vowel intercept as a random effect
#1.7.1 Provide the fixed effects formula

fit2 <- lmer(f2 ~ f1 + (1|vowel), data = shva)
summary(fit2)

#1.7.2 Provide the variance for intercept argument for vowel random effects

  #variance = 6374

#1.7.3 Add the regression line in scatterplot 1.1

ggplot(shva, aes(x=f2, y=f1, colour = vowel)) +
  geom_point() +
  scale_y_continuous(trans = "reverse") +
  scale_x_continuous(trans = "reverse") +
  geom_smooth(method = "lm", se = FALSE) +
  theme(legend.position="none") +
  ggtitle("f2 and f1 of the redused and stressed vowels")



#2.0 Read the data from file to the variable elp

elp <- read.csv("https://raw.githubusercontent.com/agricolamz/2018-MAG_R_course/master/data/ELP.csv")
head(elp)

#2.1 Which two variables have the highest Pearson’s correlaton value

L_vs_S <- cor(elp$Length, elp$SUBTLWF, method = "pearson")
print(L_vs_S)

L_vs_M <- cor(elp$Length, elp$Mean_RT, method = "pearson")
print(L_vs_M)

M_vs_S <- cor(elp$Mean_RT, elp$SUBTLWF, method = "pearson")
print(M_vs_S)

  # Length and Mean_RT: 0.5276826

#2.2 Group your data by parts of speech and make a scatterplot of SUBTLWF and Mean_RT.

ggplot(data = elp, aes(x = log(SUBTLWF), y = Mean_RT, color = Length)) +
  geom_point() +
  facet_wrap( ~ POS)+
  scale_color_continuous(low = "lightblue", high = "red")+
  theme_bw()+
  labs(x = "SUBTLWF")

#2.3 Use the linear regression model to predict Mean_RT by log(SUBTLWF) and POS.
#2.3.1 Provide the result regression formula

fit3 <- lm(log(SUBTLWF)~Mean_RT, data = elp)
summary(fit3)

#2.3.2 Provide the adjusted R2

  #Adjusted R-squared:  0.3271

#2.3.3 Add the regression line in scatterplot 1.1

ggplot(data = elp, aes(x = log(SUBTLWF), y = Mean_RT, color = Length)) +
  geom_point() +
  scale_color_continuous(low = "lightblue", high = "red")+
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE, color = "black")

#2.4 Use the mixed-efects model to predict Mean_RT by log(SUBTLWF) using POS intercept as a random effect
#2.4.1 Provide the fixed effects formula

fit4 <- lmer(Mean_RT ~ log(SUBTLWF) + (1|POS), data = elp)
summary(fit4)

#2.4.2 Provide the variance for intercept argument for POS random effects

  #POS intercept variance = 414.4

#2.4.3 Add the regression line to scatterplot

ggplot(data = elp, aes(x = log(SUBTLWF), y = Mean_RT, color = POS)) +
  geom_point() +
  facet_wrap( ~ POS)+
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  theme(legend.position="none")


#3. Dutch causative constructions
#3.0 Read the data from file to the variable d_caus

d_caus <- read.csv('https://raw.githubusercontent.com/agricolamz/2018-MAG_R_course/master/data/dutch_causatives.csv')
head(d_caus)

#3.1 We are going to test whether the association between Aux and other categorical variables (Aux ~ CrSem, Aux ~ CeSem, etc) is statistically significant. The assiciation with which variable should be analysed using Fisher’s Exact Test and not using Pearson’s Chi-squared Test? Is this association statistically significant?

fisher.test(x = d_caus$Aux, y = d_caus$CrSem)

fisher.test(x = d_caus$Aux, y = d_caus$CeSem)

fisher.test(x = d_caus$Aux, y = d_caus$CdEvSem)

fisher.test(x = d_caus$Aux, y = d_caus$CeSynt)

fisher.test(x = d_caus$Aux, y = d_caus$EPTrans)

fisher.test(x = d_caus$Aux, y = d_caus$Country)

fisher.test(x = d_caus$Aux, y = d_caus$Domain)

#3.2. Test the hypothesis that Aux and EPTrans are not independent with the help of 

AE <- table(d_caus$Aux, d_caus$EPTrans)
AE

chisq.test(AE)

#3.3 Provide expected values for Pearson’s Chi-squared Test of Aux and EPTrans variables.

  #X-squared = 14.307, df = 1, p-value = 0.0001553

#3.4. Calculate the odds ratio.

summary(lodds(AE))

  #doen:laten|Intr -1.1609
  #doen:laten|Tr -2.11883   

#3.5 Calculate effect size for this test using Cramer’s V (phi)

cramerV(AE)
  #0.1745

#3.7 Visualize the distribution using mosaic plot.

mosaic(~ Aux + CrSem + Country, data=d_caus, shade=TRUE, legend=TRUE)

mosaic(~ Aux + EPTrans, data=d_caus, shade=TRUE, legend=TRUE)


