library(tibble)
library(ggplot2)
library(readxl)
library(tidyverse)
library(maps)
library(lmtest)
library(DescTools)

adatok = read.csv("C:\\Users\\User\\Desktop\\Szakdolgozat\\R\\Life Expectancy Data.csv", header = T, sep = ",")
terulet = read_xls("C:\\Users\\User\\Desktop\\Szakdolgozat\\R\\Country areas.xls")[,c("Country Name",2010)]

# A 2010-es adatok:

adatok2010 = data.frame(matrix(nrow = 0, ncol = ncol(adatok)))
colnames(adatok2010) = colnames(adatok)
for (i in 1:nrow(adatok))
{if (adatok[i,2] == 2010)
  adatok2010[nrow(adatok2010)+1,] = c(adatok[i,])
}

add_column(adatok2010, Area = 0)
for (i in 1:nrow(adatok2010)) 
  for (j in 1:nrow(terulet))
  {if (adatok2010[i,1] == terulet[j,1])
    adatok2010[i,"Area"] = terulet[j,2]
  }

adatok2010[151,"Schooling"] = 10^(-10)

# A várható élettartam ábrázolása térképen

vilag = map_data("world")
add_column(vilag, lifeexp = NA)
for (i in 1:nrow(vilag)) 
  for (j in 1:nrow(adatok2010))
  {if (vilag[i,"region"] == adatok2010[j,"Country"])
    vilag[i,"lifeexp"] = adatok2010[j,"Life.expectancy"]
  }
for (i in 1:nrow(vilag)){
  if (vilag[i,"region"] == "USA"){
    vilag[i,"lifeexp"] = adatok2010[175,"Life.expectancy"]
  } else if (vilag[i,"region"] == "UK"){
    vilag[i,"lifeexp"] = adatok2010[173,"Life.expectancy"]
  } else if (vilag[i,"region"] == "Russia"){
    vilag[i,"lifeexp"] = adatok2010[134,"Life.expectancy"]
  } else if (vilag[i,"region"] == "Czech Republic"){
    vilag[i,"lifeexp"] = adatok2010[44,"Life.expectancy"]
  } else if (vilag[i,"region"] == "Venezuela"){
    vilag[i,"lifeexp"] = adatok2010[179,"Life.expectancy"]
  } else if (vilag[i,"region"] == "Bolivia"){
    vilag[i,"lifeexp"] = adatok2010[20,"Life.expectancy"]
  } else if (vilag[i,"region"] == "Vietnam"){
    vilag[i,"lifeexp"] = adatok2010[180,"Life.expectancy"]
  } else if (vilag[i,"region"] == "Iran"){
    vilag[i,"lifeexp"] = adatok2010[77,"Life.expectancy"]
  } else if (vilag[i,"region"] == "Syria"){
    vilag[i,"lifeexp"] = adatok2010[159,"Life.expectancy"]
  } else if (vilag[i,"region"] == "Ivory Coast"){
    vilag[i,"lifeexp"] = adatok2010[28,"Life.expectancy"]
  } else if (vilag[i,"region"] == "Republic of Congo"){
    vilag[i,"lifeexp"] = adatok2010[39,"Life.expectancy"]
  } else if (vilag[i,"region"] == "North Macedonia"){
    vilag[i,"lifeexp"] = adatok2010[162,"Life.expectancy"]
  } else if (vilag[i,"region"] == "North Korea"){
    vilag[i,"lifeexp"] = adatok2010[45,"Life.expectancy"]
  } else if (vilag[i,"region"] == "Laos"){
    vilag[i,"lifeexp"] = adatok2010[90,"Life.expectancy"]
  }
}

ggplot() +
  geom_map(data = vilag, map = vilag, aes(long, lat, map_id = region, fill = lifeexp), color = "black") +
  scale_fill_viridis_c(option = "plasma") +
  xlab(NULL) + ylab(NULL) +
  guides(fill = guide_colourbar(title = "Várható\nélettartam"))

# A felhasznált változók hisztogramjai:

hist(adatok2010[,"Life.expectancy"], main = "Várható élettartam", xlab = "év", ylab = "Gyakoriság", col = "aquamarine", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.2)

par(mfrow=c(2,3))
hist(log10(adatok2010[,"Area"]), main = "Terület", xlab = "log_10 km^2", ylab = "Gyakoriság", col = "slateblue2", cex.lab = 1.1, cex.axis = 1.1)
hist(log10(adatok2010[,"Population"]), main = "Népesség", xlab = "log_10 fõ", ylab = "Gyakoriság", col = "slateblue2", cex.lab = 1.1, cex.axis = 1.1)
hist(log10(adatok2010[,"GDP"]), main = "GDP/fõ (bruttó hazai termék)", xlab = "log_10 USD", ylab = "Gyakoriság", col = "slateblue2", cex.lab = 1.1, cex.axis = 1.1)
hist(adatok2010[,"Alcohol"], main = "Alkoholfogyasztás", xlab = "liter/fõ (tiszta alkohol)", ylab = "Gyakoriság", col = "slateblue2", cex.lab = 1.1, cex.axis = 1.1)
hist(adatok2010[,"Schooling"], main = "Iskolázottság", xlab = "év (átlagosan iskolában töltött)", ylab = "Gyakoriság", col = "slateblue2", cex.lab = 1.1, cex.axis = 1.1)
hist(adatok2010[,"BMI"], main = "BMI (testtömegindex)", xlab = "kg/m^2", ylab = "Gyakoriság", col = "slateblue2", cex.lab = 1.1, cex.axis = 1.1)
par(mfrow=c(1,1))

set.seed(1)

# 1. Terület, népesség, GDP; adatok harmada

ter1 = c()
nep1 = c()
gdp1 = c()
for (i in 1:100)
{
  adatok3 = adatok2010[sample(nrow(adatok2010),floor(nrow(adatok2010)/3)),]
  reg1 = lm(Life.expectancy ~ log10(Area) + log10(Population) + log10(GDP), data = adatok3)
  ter1 = append(ter1,summary(reg1)$coefficients[2,4])
  nep1 = append(nep1,summary(reg1)$coefficients[3,4])
  gdp1 = append(gdp1,summary(reg1)$coefficients[4,4])
}

# 2. Terület, népesség, GDP; adatok kétharmada

ter2 = c()
nep2 = c()
gdp2 = c()
for (i in 1:100)
{
  adatok2 = adatok2010[sample(nrow(adatok2010),floor(2*nrow(adatok2010)/3)),]
  reg2 = lm(Life.expectancy ~ log10(Area) + log10(Population) + log10(GDP), data = adatok2)
  ter2 = append(ter2,summary(reg2)$coefficients[2,4])
  nep2 = append(nep2,summary(reg2)$coefficients[3,4])
  gdp2 = append(gdp2,summary(reg2)$coefficients[4,4])
}

# 3. Terület, népesség, GDP; összes adat

reg3 = lm(Life.expectancy ~ log10(Area) + log10(Population) + log10(GDP), data = adatok2010)
ter3 = summary(reg3)$coefficients[2,4]
nep3 = summary(reg3)$coefficients[3,4]
gdp3 = summary(reg3)$coefficients[4,4]

eredmeny3 = data.frame(Reziduálisok = reg3$residuals, Illesztett.értékek = reg3$fitted.values)

ggplot(data = eredmeny3, aes(x = Illesztett.értékek, y = Reziduálisok)) +
  geom_hline(yintercept = 0, col = "springgreen", lwd = 1.2) +
  geom_point(size = 2.2) +
  xlab("Illesztett értékek") +
  ggtitle("Reziduálisok az illesztett érték függvényében") +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 15), axis.title = element_text(size = 17))

ggplot(data = eredmeny3, aes(x = Reziduálisok)) +
  geom_histogram(fill = "springgreen", col = "black", bins = 20) +
  ylab("Gyakoriság") +
  ggtitle("A reziduálisok hisztogramja") +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 15), axis.title = element_text(size = 17))

DurbinWatsonTest(reg3)
VIF(reg3)
bptest(reg3)
shapiro.test(reg3$residuals)

# 4. Terület, népesség, GDP, alkohol; adatok harmada

ter4 = c()
nep4 = c()
gdp4 = c()
alk4 = c()
for (i in 1:100)
{
  adatok3 = adatok2010[sample(nrow(adatok2010),floor(nrow(adatok2010)/3)),]
  reg4 = lm(Life.expectancy ~ log10(Area) + log10(Population) + log10(GDP) + Alcohol, data = adatok3)
  ter4 = append(ter4,summary(reg4)$coefficients[2,4])
  nep4 = append(nep4,summary(reg4)$coefficients[3,4])
  gdp4 = append(gdp4,summary(reg4)$coefficients[4,4])
  alk4 = append(alk4,summary(reg4)$coefficients[5,4])
}

# 5. Terület, népesség, GDP, alkohol; adatok kétharmada

ter5 = c()
nep5 = c()
gdp5 = c()
alk5 = c()
for (i in 1:100)
{
  adatok2 = adatok2010[sample(nrow(adatok2010),floor(2*nrow(adatok2010)/3)),]
  reg5 = lm(Life.expectancy ~ log10(Area) + log10(Population) + log10(GDP) + Alcohol, data = adatok2)
  ter5 = append(ter5,summary(reg5)$coefficients[2,4])
  nep5 = append(nep5,summary(reg5)$coefficients[3,4])
  gdp5 = append(gdp5,summary(reg5)$coefficients[4,4])
  alk5 = append(alk5,summary(reg5)$coefficients[5,4])
}

# 6. Terület, népesség, GDP, alkohol; összes adat

reg6 = lm(Life.expectancy ~ log10(Area) + log10(Population) + log10(GDP) + Alcohol, data = adatok2010)
ter6 = summary(reg6)$coefficients[2,4]
nep6 = summary(reg6)$coefficients[3,4]
gdp6 = summary(reg6)$coefficients[4,4]
alk6 = summary(reg6)$coefficients[5,4]

eredmeny6 = data.frame(Reziduálisok = reg6$residuals, Illesztett.értékek = reg6$fitted.values)

ggplot(data = eredmeny6, aes(x = Illesztett.értékek, y = Reziduálisok)) +
  geom_hline(yintercept = 0, col = "springgreen", lwd = 1.2) +
  geom_point(size = 2.2) +
  xlab("Illesztett értékek") +
  ggtitle("Reziduálisok az illesztett érték függvényében") +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 15), axis.title = element_text(size = 17))

ggplot(data = eredmeny6, aes(x = Reziduálisok)) +
  geom_histogram(fill = "springgreen", col = "black", bins = 20) +
  ylab("Gyakoriság") +
  ggtitle("A reziduálisok hisztogramja") +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 15), axis.title = element_text(size = 17))

DurbinWatsonTest(reg6)
VIF(reg6)
bptest(reg6)
shapiro.test(reg6$residuals)

# 7. Terület, népesség, GDP, alkohol, iskolázottság, BMI; adatok harmada

ter7 = c()
nep7 = c()
gdp7 = c()
alk7 = c()
isk7 = c()
bmi7 = c()
for (i in 1:100)
{
  adatok3 = adatok2010[sample(nrow(adatok2010),floor(nrow(adatok2010)/3)),]
  reg7 = lm(Life.expectancy ~ log10(Area) + log10(Population) + log10(GDP) + Alcohol + Schooling + BMI, data = adatok3)
  ter7 = append(ter7,summary(reg7)$coefficients[2,4])
  nep7 = append(nep7,summary(reg7)$coefficients[3,4])
  gdp7 = append(gdp7,summary(reg7)$coefficients[4,4])
  alk7 = append(alk7,summary(reg7)$coefficients[5,4])
  isk7 = append(isk7,summary(reg7)$coefficients[6,4])
  bmi7 = append(bmi7,summary(reg7)$coefficients[7,4])
}

# 8. Terület, népesség, GDP, alkohol, iskolázottság, BMI; adatok kétharmada

ter8 = c()
nep8 = c()
gdp8 = c()
alk8 = c()
isk8 = c()
bmi8 = c()
for (i in 1:100)
{
  adatok2 = adatok2010[sample(nrow(adatok2010),floor(2*nrow(adatok2010)/3)),]
  reg8 = lm(Life.expectancy ~ log10(Area) + log10(Population) + log10(GDP) + Alcohol + Schooling + BMI, data = adatok2)
  ter8 = append(ter8,summary(reg8)$coefficients[2,4])
  nep8 = append(nep8,summary(reg8)$coefficients[3,4])
  gdp8 = append(gdp8,summary(reg8)$coefficients[4,4])
  alk8 = append(alk8,summary(reg8)$coefficients[5,4])
  isk8 = append(isk8,summary(reg8)$coefficients[6,4])
  bmi8 = append(bmi8,summary(reg8)$coefficients[7,4])
}

# 9. Terület, népesség, GDP, alkohol, iskolázottság, BMI; összes adat

reg9 = lm(Life.expectancy ~ log10(Area) + log10(Population) + log10(GDP) + Alcohol + Schooling + BMI, data = adatok2010)
ter9 = summary(reg9)$coefficient[2,4]
nep9 = summary(reg9)$coefficient[3,4]
gdp9 = summary(reg9)$coefficient[4,4]
alk9 = summary(reg9)$coefficient[5,4]
isk9 = summary(reg9)$coefficient[6,4]
bmi9 = summary(reg9)$coefficient[7,4]

eredmeny9 = data.frame(Reziduálisok = reg9$residuals, Illesztett.értékek = reg9$fitted.values)

ggplot(data = eredmeny9, aes(x = Illesztett.értékek, y = Reziduálisok)) +
  geom_hline(yintercept = 0, col = "springgreen", lwd = 1.2) +
  geom_point(size = 2.2) +
  xlab("Illesztett értékek") +
  ggtitle("Reziduálisok az illesztett érték függvényében") +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 15), axis.title = element_text(size = 17))

ggplot(data = eredmeny9, aes(x = Reziduálisok)) +
  geom_histogram(fill = "springgreen", col = "black", bins = 20) +
  ylab("Gyakoriság") +
  ggtitle("A reziduálisok hisztogramja") +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 15), axis.title = element_text(size = 17))

DurbinWatsonTest(reg9)
VIF(reg9)
bptest(reg9)
shapiro.test(reg9$residuals)

# A modell részletei:
summary(reg9)

# Jósolt értékek:

datas = data.frame("Area" = adatok2010$Area, "Population" = adatok2010$Population,
                   "GDP" = adatok2010$GDP, "Alcohol" = adatok2010$Alcohol,
                   "Schooling" = adatok2010$Schooling, "BMI" = adatok2010$BMI)
preds = predict(reg9, newdata = datas)
hist(adatok2010$Life.expectancy-preds, breaks = 15, xlab = "év", ylab = "Gyakoriság",
     main = "A valódi és a jósolt értékek közti eltérések", col = "aquamarine",
     cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.2)
sd(adatok2010$Life.expectancy - preds, na.rm = TRUE)

# legnagyobb eltérés:
k = order(abs(adatok2010$Life.expectancy - preds), decreasing = TRUE)[1]
adatok2010$Country[k]
(adatok2010$Life.expectancy - preds)[k]
adatok2010$Life.expectancy[k]
preds[k]

# Jósolt értékek térképen:

vilag2 = map_data("world")
add_column(vilag2, pred = NA)
for (i in 1:nrow(vilag2)) 
  for (j in 1:nrow(adatok2010))
  {if (vilag2[i,"region"] == adatok2010[j,"Country"])
    vilag2[i,"pred"] = preds[j]
  }
for (i in 1:nrow(vilag2)){
  if (vilag2[i,"region"] == "USA"){
    vilag2[i,"pred"] = preds[175]
  } else if (vilag2[i,"region"] == "UK"){
    vilag2[i,"pred"] = preds[173]
  } else if (vilag2[i,"region"] == "Russia"){
    vilag2[i,"pred"] = preds[134]
  } else if (vilag2[i,"region"] == "Czech Republic"){
    vilag2[i,"pred"] = preds[44]
  } else if (vilag2[i,"region"] == "Venezuela"){
    vilag2[i,"pred"] = preds[179]
  } else if (vilag2[i,"region"] == "Bolivia"){
    vilag2[i,"pred"] = preds[20]
  } else if (vilag2[i,"region"] == "Vietnam"){
    vilag2[i,"pred"] = preds[180]
  } else if (vilag2[i,"region"] == "Iran"){
    vilag2[i,"pred"] = preds[77]
  } else if (vilag2[i,"region"] == "Syria"){
    vilag2[i,"pred"] = preds[159]
  } else if (vilag2[i,"region"] == "Ivory Coast"){
    vilag2[i,"pred"] = preds[28]
  } else if (vilag2[i,"region"] == "Republic of Congo"){
    vilag2[i,"pred"] = preds[39]
  } else if (vilag2[i,"region"] == "North Macedonia"){
    vilag2[i,"pred"] = preds[162]
  } else if (vilag2[i,"region"] == "North Korea"){
    vilag2[i,"pred"] = preds[45]
  } else if (vilag2[i,"region"] == "Laos"){
    vilag2[i,"pred"] = preds[90]
  }
}

ggplot() +
  geom_map(data = vilag2, map = vilag, aes(long, lat, map_id = region, fill = pred), color = "black") +
  scale_fill_viridis_c(option = "plasma") +
  xlab(NULL) + ylab(NULL) +
  guides(fill = guide_colourbar(title = "Jósolt\nvárható\nélettartam"))


# A p értékek vizuális megjelenítése:

t1 = c(mean(ter1), mean(ter2), ter3)
t2 = c(mean(ter4), mean(ter5), ter6)
t3 = c(mean(ter7), mean(ter8), ter9)
n1 = c(mean(nep1), mean(nep2), nep3)
n2 = c(mean(nep4), mean(nep5), nep6)
n3 = c(mean(nep7), mean(nep8), nep9)
g1 = c(mean(gdp1), mean(gdp2), gdp3)
g2 = c(mean(gdp4), mean(gdp5), gdp6)
g3 = c(mean(gdp7), mean(gdp8), gdp9)
a2 = c(mean(alk4), mean(alk5), alk6)
a3 = c(mean(alk7), mean(alk8), alk9)
i3 = c(mean(isk7), mean(isk8), isk9)
b3 = c(mean(bmi7), mean(bmi8), bmi9)

p = data.frame(t1, t2, t3, n1, n2, n3, g1, g2, g3, a2, a3, i3, b3)
x = c(1/3, 2/3, 1)

# arany: 3 magyarázó változós futtatás
# narancs: 4 magyarázó változós futtatás
# piros: 6 magyarázó változós futtatás
# fekete: p = 0.05

ggplot(p) +
  geom_hline(yintercept = 0.05) +
  geom_line(aes(x, t1), col = "gold") + geom_point(aes(x, t1), size = 5, col = c("gold")) +
  geom_line(aes(x, t2), col = "orange") + geom_point(aes(x, t2), size = 5, col = c("orange")) +
  geom_line(aes(x, t3), col = "red") + geom_point(aes(x, t3), size = 5, col = c("red")) +
  ggtitle("Területhez tartozó p-értékek") +
  labs(x = "adatok hányadrészére lett futtatva", y = "p-értékek") +
  scale_x_continuous(breaks = seq(1/3, 1, 1/3), labels = c("1/3", "2/3", "1")) + scale_y_continuous(breaks = sort(c(seq(0, 1, 0.1), 0.05))) +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 17), axis.title = element_text(size = 17))

boxplot(ter1,ter2,ter3,ter4,ter5,ter6,ter7,ter8,ter9,
        col = c(rep("gold",3),rep("orange",3),rep("red",3)),
        names = c(rep(c("1/3","2/3","1"),3)),
        ylab = "p-értékek", xlab = "adatok hányadrészére lett futtatva",
        main = "A terület p-értékeinek elhelyezkedése az egyes szimulációk során",
        cex.lab = 1.5, cex.axis = 1.3)
abline(h = 0.05)

ggplot(p) +
  geom_hline(yintercept = 0.05) +
  geom_line(aes(x, n1), col = "gold") + geom_point(aes(x, n1), size = 5, col = c("gold")) +
  geom_line(aes(x, n2), col = "orange") + geom_point(aes(x, n2), size = 5, col = c("orange")) +
  geom_line(aes(x, n3), col = "red") + geom_point(aes(x, n3), size = 5, col = c("red")) +
  ggtitle("Népességhez tartozó p-értékek") +
  labs(x = "adatok hányadrészére lett futtatva", y = "p-értékek") +
  scale_x_continuous(breaks = seq(1/3, 1, 1/3), labels = c("1/3", "2/3", "1")) + scale_y_continuous(breaks = sort(c(seq(0, 1, 0.15), 0.05))) +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 17), axis.title = element_text(size = 17))

boxplot(nep1,nep2,nep3,nep4,nep5,nep6,nep7,nep8,nep9,
        col = c(rep("gold",3),rep("orange",3),rep("red",3)),
        names = c(rep(c("1/3","2/3","1"),3)),
        ylab = "p-értékek", xlab = "adatok hányadrészére lett futtatva",
        main = "A népesség p-értékeinek elhelyezkedése az egyes szimulációk során",
        cex.lab = 1.5, cex.axis = 1.3)
abline(h = 0.05)

ggplot(p) +
  geom_hline(yintercept = 0.05) +
  geom_line(aes(x, g1), col = "gold") + geom_point(aes(x, g1), size = 5, col = c("gold")) +
  geom_line(aes(x, g2), col = "orange") + geom_point(aes(x, g2), size = 5, col = c("orange")) +
  geom_line(aes(x, g3), col = "red") + geom_point(aes(x, g3), size = 5, col = c("red")) +
  ggtitle("Egy fõre jutó GDP-hez tartozó p-értékek") +
  labs(x = "adatok hányadrészére lett futtatva", y = "p-értékek") +
  scale_x_continuous(breaks = seq(1/3, 1, 1/3), labels = c("1/3", "2/3", "1")) + scale_y_continuous(breaks = sort(c(seq(0, 1, 0.1), 0.05))) +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 17), axis.title = element_text(size = 17))

par(mfrow = c(1,2))
boxplot(log10(gdp1),log10(gdp2),log10(gdp3),log10(gdp4),log10(gdp5),log10(gdp6),
        col = c(rep("gold",3),rep("orange",3)),
        names = c(rep(c("1/3","2/3","1"),2)),
        ylab = "log_10 p-értékek", xlab = "adatok hányadrészére lett futtatva",
        cex.lab = 1.5, cex.axis = 1.3)
abline(h = log10(0.05))
boxplot(gdp7,gdp8,gdp9,
        col = c(rep("red",3)),
        names = c("1/3","2/3","1"),
        ylab = "p-értékek", xlab = "adatok hányadrészére lett futtatva",
        cex.lab = 1.5, cex.axis = 1.3)
abline(h = 0.05)
mtext("A GDP/fõ p-értékeinek elhelyezkedése az egyes szimulációk során", outer = TRUE, line = -2.5, font = 2, cex = 1.2)

par(mfrow = c(1,1))

ggplot(p) +
  geom_hline(yintercept = 0.05) +
  geom_line(aes(x, a2), col = "orange") + geom_point(aes(x, a2), size = 5, col = c("orange")) +
  geom_line(aes(x, a3), col = "red") + geom_point(aes(x, a3), size = 5, col = c("red")) +
  ggtitle("Alkoholfogyasztáshoz tartozó p-értékek") +
  labs(x = "adatok hányadrészére lett futtatva", y = "p-értékek") +
  scale_x_continuous(breaks = seq(1/3, 1, 1/3), labels = c("1/3", "2/3", "1")) + scale_y_continuous(breaks = sort(c(seq(0, 0.5, 0.05), 0.05))) +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 17), axis.title = element_text(size = 17))

boxplot(alk4,alk5,alk6,alk7,alk8,alk9,
        col = c(rep("orange",3),rep("red",3)),
        names = c(rep(c("1/3","2/3","1"),2)),
        ylab = "p-értékek", xlab = "adatok hányadrészére lett futtatva",
        main = "Az alkoholfogyasztás p-értékeinek elhelyezkedése az egyes szimulációk során",
        cex.lab = 1.5, cex.axis = 1.3)
abline(h = 0.05)

ggplot(p) +
  geom_line(aes(x, log10(i3)), col = "red") + geom_point(aes(x, log10(i3)), size = 5, col = c("red")) +
  ggtitle("Iskolázottsághoz tartozó p-értékek") +
  labs(x = "adatok hányadrészére lett futtatva", y = "p-értékek log_10-es skálán") +
  scale_x_continuous(breaks = seq(1/3, 1, 1/3), labels = c("1/3", "2/3", "1")) +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 17), axis.title = element_text(size = 17))

boxplot(log10(isk7),log10(isk8),log10(isk9),
        col = c(rep("red",3)),
        names = c("1/3","2/3","1"),
        ylab = "log_10 p-értékek", xlab = "adatok hányadrészére lett futtatva",
        main = "Az iskolázottsághoz p-értékeinek elhelyezkedése az egyes szimulációk során",
        cex.lab = 1.5, cex.axis = 1.3)
abline(h = log10(0.05))

ggplot(p) +
  geom_hline(yintercept = 0.05) +
  geom_line(aes(x, b3), col = "red") + geom_point(aes(x, b3), size = 5, col = c("red")) +
  ggtitle("BMI értékhez tartozó p-értékek") +
  labs(x = "adatok hányadrészére lett futtatva", y = "p-értékek") +
  scale_x_continuous(breaks = seq(1/3, 1, 1/3), labels = c("1/3", "2/3", "1")) + scale_y_continuous(breaks = sort(c(seq(0.05, 0.5, 0.1), 0.05))) +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 17), axis.title = element_text(size = 17))

boxplot(bmi7,bmi8,bmi9,
        col = c(rep("red",3)),
        names = c("1/3","2/3","1"),
        ylab = "p-értékek", xlab = "adatok hányadrészére lett futtatva",
        main = "A BMI p-értékeinek elhelyezkedése az egyes szimulációk során",
        cex.lab = 1.5, cex.axis = 1.3)
abline(h = 0.05)