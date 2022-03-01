library(readxl)

# import datasets 

problem_1 <- read_excel("CT4_problems.xlsx", sheet = "problem 1")
View(problem_1)
problem_2 <- read_excel("CT4_problems.xlsx", sheet = "problem 2", col_types = c("text","text", "numeric"))
View(problem_2)
problem_2.1 <- read_excel("CT4_problems.xlsx",sheet= "problem 2.1", col_types= c("text","text", "numeric"))
View(problem_2.1)
problem_3 <- read_excel("CT4_problems.xlsx", sheet = "problem 3")
View(problem_3)

# problem 1 
model_1 <- chisq.test(problem_1$`Number of Deaths`)
model_1

# problem 2

#part one
model_2<- aov(problem_2$Mileage ~ problem_2$Gasoline + problem_2$Additive)
anova(model_2)


#part two

"point a:"
aggregate(problem_2.1[, 3], list(problem_2.1$Gasoline), mean)

"point b:"
aggregate(problem_2.1[, 3], list(problem_2.1$Additive), mean)

"point c:"
aggregate(problem_2.1[, 3], list(problem_2.1$Gasoline,problem_2.1$Additive), mean)

model_3<- aov(problem_2.1$Mileage ~ problem_2.1$Gasoline*problem_2.1$Additive)
anova(model_3)


# problem 3 

"point f: How many females are expected to be in party A, B, C respectively"

expected_partA_women <- (156*120)/300
expected_partB_women <- (156*128)/300
expected_partC_women <- (156*52)/300

"point g: How many males are expected to be in party A, B, C respectively"

expected_partA_men <- (144*120)/300
expected_partB_men <- (144*128)/300
expected_partC_men <- (144*52)/300

"Point i:"

problem_3_TS <- ((68-62.4)^2/62.4)+ ((56-66.56)^2/66.56)+ ((32-27.04)^2/27.04)+((52-57.6)^2/57.6)+
  ((72-61.44)^2/61.44) +((20-24.96)^2/24.96)

"Since (r − 1)(s − 1) the degree of freedom = 2."

problem_3_Pvalue <- 1-pchisq(problem_3_TS,2)

"Since p-value (0.04)< 0.05, the null hypothesis is rejected at the 5 percent level of significance.
That is, the hypothesis that gender and political affiliation of members of the population
are independent is rejected at the 5 percent level of significance."