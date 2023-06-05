library(DescTools)
library(ggplot2)

# Szerkezeti multikollinearit�s

# Cikkb�li p�lda:

mc = read.csv("C:\\Users\\User\\Desktop\\Szakdolgozat\\R\\MulticollinearityExample.csv", header = T, sep = ",")

mglm = lm(Femoral.Neck ~ Activity + X.Fat + Weight.kg + X.Fat*Weight.kg, data = mc)
summary(mglm)
VIF(mglm)

 # centr�lt v�ltoz�kkal:

mglm2 = lm(Femoral.Neck ~ Activity + X.Fat.S + Weight.S + X.Fat.S*Weight.S, data = mc)
summary(mglm2)
VIF(mglm2)

# A modellek becsl�sei:

df = data.frame(Illesztett.�rt�kek.1 = mglm$fitted.values, Illesztett.�rt�kek.2 = mglm2$fitted.values,
                Rezidu�lisok.1 = mglm$residuals, Rezidu�lisok.2 = mglm2$residuals,
                Illesztett.�rt�kek.k = (mglm$fitted.values - mglm2$fitted.values),
                Rezidu�lisok.k = (mglm$residuals - mglm2$residuals))

ggplot(df, aes(Illesztett.�rt�kek.k)) +
  geom_histogram(fill = "lightblue1", color = "black") +
  labs(title = "A centr�l�s el�tti �s centr�l�s ut�ni becsl�sek k�zti k�l�nbs�gek") +
  xlab("A k�t �rt�k k�zti k�l�nbs�g") +
  ylab(NULL) +
  scale_x_continuous(breaks = seq(min(df$Illesztett.�rt�kek.k), max(df$Illesztett.�rt�kek.k), (max(df$Illesztett.�rt�kek.k)-min(df$Illesztett.�rt�kek.k))/5),
                     labels = signif(seq(min(df$Illesztett.�rt�kek.k), max(df$Illesztett.�rt�kek.k), (max(df$Illesztett.�rt�kek.k)-min(df$Illesztett.�rt�kek.k))/5), 3)) +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 17))

ggplot(df, aes(Rezidu�lisok.k)) +
  geom_histogram(fill = "lightblue1", color = "black") +
  labs(title = "A centr�l�s el�tti �s centr�l�s ut�ni rezidu�lisok k�zti k�l�nbs�gek") +
  xlab("A k�t �rt�k k�zti k�l�nbs�g") +
  ylab(NULL) +
  scale_x_continuous(breaks = seq(min(df$Rezidu�lisok.k), max(df$Rezidu�lisok.k), (max(df$Rezidu�lisok.k)-min(df$Rezidu�lisok.k))/5),
                     labels = signif(seq(min(df$Rezidu�lisok.k), max(df$Rezidu�lisok.k), (max(df$Rezidu�lisok.k)-min(df$Rezidu�lisok.k))/5), 3)) +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 17))


# Gener�lt adatokra:

set.seed(3)
ism = 5000
n = 1000

corx1 = c()
corx2 = c()
corcx1 = c()
corcx2 = c()

for (i in 1:ism){
  x1 = rnorm(n, mean = 10, sd = 1)
  x2 = rnorm(n, mean = 5, sd = 1)
  x1x2 = x1*x2
  cx1 = x1 - mean(x1)
  cx2 = x2 - mean(x2)
  cx1x2 = cx1*cx2
  
  corx1 = c(corx1, cor(x1, x1x2))
  corx2 = c(corx2, cor(x2, x1x2))
  corcx1 = c(corcx1, cor(cx1, cx1x2))
  corcx2 = c(corcx2, cor(cx2, cx1x2))
}

df = data.frame("�rt�kek" = c(corx1, corx2, corcx1, corcx2),
                "Centr�l�s" = rep(c("Centr�l�s el�tt", "Centr�l�s ut�n"), each = 2*ism),
                "V�ltoz�k" = rep(rep(c("X1 �s X1*X2 korre�ci�ja", "X2 �s X1*X2 korrel�ci�ja"), each = ism), times = 2))

ggplot(df, aes(x = �rt�kek, fill = Centr�l�s)) + 
  geom_histogram(color = "black", binwidth = 0.02) +
  facet_grid(rows = vars(V�ltoz�k),  scales = "free") +
  scale_fill_manual(values = c("indianred1", "seagreen1")) +
  labs(title = "A v�ltoz�k korrel�ci�ja") + ylab(NULL) +
  scale_x_continuous(breaks = round(seq(-0.3, 1, 0.1), 1))

# �tlag �s sz�r�s
mean(corx1)
mean(corx2)
sd(corx1)
sd(corx2)

mean(corcx1)
mean(corcx2)
sd(corcx1)
sd(corcx2)
