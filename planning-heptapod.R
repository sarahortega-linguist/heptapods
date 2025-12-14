#fake data creation for planningTime 
df1<- data.frame(planningTime = sample(250:450, 640, replace = TRUE))
df2<- data.frame(set = sample(1:32,32),set = sample(1:32,32),set = sample(1:32,32),set = sample(1:32,32),set = sample(1:32,32),set = sample(1:32,32),set = sample(1:32,32),set = sample(1:32,32),set = sample(1:32,32),set = sample(1:32,32),set = sample(1:32,32),set = sample(1:32,32),set = sample(1:32,32),set = sample(1:32,32),set = sample(1:32,32),set = sample(1:32,32),set = sample(1:32,32),set = sample(1:32,32),set = sample(1:32,32),set = sample(1:32,32))
write.csv(df1, file = 'C:/Users/bnwke/OneDrive/Desktop/ALx.csv', row.names = TRUE)
write.csv(df2, file = 'C:/Users/bnwke/OneDrive/Desktop/set_sample.csv', row.names = TRUE)

#used libraries
library(ggplot2)
library(ez)
heptapod <- read.csv('HeptapodA_updated.csv', stringsAsFactors = TRUE)

heptapod$subj <-as.factor(heptapod$subj)
heptapod$set <- as.factor(heptapod$set)

# by subject and by item 
xtabs(~subj + length, heptapod) #within subject
xtabs(~subj + first_clause_complexity, heptapod) #within subject
xtabs(~set + length, heptapod) #between item
xtabs(~set + first_clause_complexity, heptapod) #between item

#important metrics 
#one-clause means refer to simple and complex sentences
((mean(heptapod[heptapod$length == 'one-clause', 'planningTime'])))# 346.7156
((mean(heptapod[heptapod$first_clause_complexity == 'simple', 'planningTime'])))# 344.7375
((mean(heptapod[heptapod$first_clause_complexity == 'complex', 'planningTime'])))# 348.6938
# difference of simple and complex means = 3.95

#standard error simple and complex sentences
simple <- heptapod[heptapod$first_clause_complexity == 'simple',]
simple.sd <- sd(simple$planningTime)
simple.n <- nrow(simple)
(simple.se <- simple.sd/sqrt(simple.n)) # 4.56
complex <- heptapod[heptapod$first_clause_complexity == 'complex',]
complex.sd <- sd(complex$planningTime)
complex.n <- nrow(complex)
(complex.se <- complex.sd/sqrt(complex.n)) # 4.58

#two-clause means refer to all simple-complex and complex-simple sentences
((mean(heptapod[heptapod$length == 'two-clause', 'planningTime'])))# 347.6875
((mean(heptapod[heptapod$first_clause_complexity == 'simp-comp', 'planningTime'])))# 358.25
((mean(heptapod[heptapod$first_clause_complexity == 'comp-simp', 'planningTime'])))# 337.125
#difference of simp-comp and comp-simp means = 21.125

#standard error simp-comp and comp-simp sentences
simpcomp <- heptapod[heptapod$first_clause_complexity == 'simp-comp',]
simpcomp.sd <- sd(simpcomp$planningTime)
simpcomp.n <- nrow(simpcomp)
(simpcomp.se <- simpcomp.sd/sqrt(simpcomp.n)) # 4.57
compsimp <- heptapod[heptapod$first_clause_complexity == 'comp-simp',]
compsimp.sd <- sd(compsimp$planningTime)
compsimp.n <- nrow(compsimp)
(compsimp.se <- compsimp.sd/sqrt(compsimp.n)) # 4.33

#full visual
ggplot(heptapod, aes(x=first_clause_complexity, y=planningTime)) +
  stat_summary(fun='mean', geom='point') +
  stat_summary(fun.data='mean_cl_normal', geom='errorbar', width=.2)

#main effect visual
mainEffect <- heptapod[heptapod$length == "two-clause",]
ggplot(mainEffect, aes(x=first_clause_complexity, y=planningTime)) +
  stat_summary(fun='mean', geom='point') +
  stat_summary(fun.data='mean_cl_normal', geom='errorbar', width=.2)


## 2x2 factorial ANOVA
heptapod.aov <- aov(planningTime ~ length*first_clause_complexity, heptapod)
summary(heptapod.aov)

##follow up t-tests
bonferroni.p <- .05/2
nrow(simpcomp)==nrow(compsimp)
sum(simpcomp$subj == compsimp$subj)==nrow(compsimp)
t.test(simpcomp$planningTime, compsimp$planningTime, paired = TRUE)

nrow(simple)==nrow(complex)
sum(simple$subj == complex$subj)==nrow(complex)
t.test(simple$planningTime, complex$planningTime, paired = TRUE)






                              