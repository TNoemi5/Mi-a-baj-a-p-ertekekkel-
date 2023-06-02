library(ggplot2)
library(lme4)

treatment = rep(c("reduced", "enhanced"), each = 30)
femaleID = rep(c(1:12), each= 5)
eggmass = c(1.1, 1.11, 1.18, 1.24, 1.265,
             1.175, 1.19, 1.22, 1.235, 1.415,
             1.105, 1.13, 1.145, 1.18, 1.185,
             0.965, 0.98, 1.015, 1.02, 1.09,
             1.08, 1.095, 1.125, 1.28, 1.305,
             1.19, 1.2, 1.205, 1.24, 1.25,
             1.245, 1.3, 1.34, 1.435, 1.44,
             1.15, 1.305, 1.31, 1.325, 1.35,
             0.955, 1.06, 1.065, 1.09, 1.145,
             1.215, 1.23, 1.325, 1.42, 1.455,
             1.125, 1.17, 1.21, 1.24, 1.3,
             1.15, 1.185, 1.365, 1.53, 1.58)

egg = data.frame(Female_ID = femaleID, Egg_mass = eggmass, Treatment = treatment)
egg$Treatment = as.factor(egg$Treatment)
egg$Female_ID = as.factor(egg$Female_ID)

ggplot(egg, aes(Female_ID, Egg_mass)) +
  geom_point(size = 3, aes(col = Treatment)) +
  xlab("Nõstény azonosítója") + ylab("Tojástömeg") +
  scale_color_manual(values = c("darkorange1", "blue"), labels = c("díszített", "dísztelen"), name = "Hím") +
  scale_x_discrete(breaks = seq(1, 12, 1)) +
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 17),
        legend.text = element_text(size = 15), legend.title = element_text(size = 17))

egg["Mean_of_egg_mass"] = 0
for (j in 1:length(unique(egg$Female_ID))){
  egg$Mean_of_egg_mass[egg$Female_ID == j] = mean(egg$Egg_mass[egg$Female_ID == j])
}

eggmean = egg[row.names(unique(egg[,c("Mean_of_egg_mass", "Treatment")])), c("Mean_of_egg_mass", "Treatment")]
rownames(eggmean) = NULL

fg1 = glm(Egg_mass ~ Treatment, data = egg)
summary(fg1)$coef[2, 4]

fg2 = glm(Mean_of_egg_mass ~ Treatment, data = eggmean)
summary(fg2)$coef[2, 4]

fg3 = lmer(Egg_mass ~ Treatment + (1 | Female_ID), data = egg)
2*pt(q = summary(fg3)$coef[2,3], df = 59, lower.tail = TRUE)

