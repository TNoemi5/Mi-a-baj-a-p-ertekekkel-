library(DescTools)
library(ggplot2)

# Szerkezeti multikollinearitás

# Cikkbéli példa:

mc = read.csv("C:\\Users\\User\\Desktop\\Szakdolgozat\\R\\MulticollinearityExample.csv", header = T, sep = ",")

mglm = lm(Femoral.Neck ~ Activity + X.Fat + Weight.kg + X.Fat*Weight.kg, data = mc)
summary(mglm)
VIF(mglm)

 # centrált változókkal:

mglm2 = lm(Femoral.Neck ~ Activity + X.Fat.S + Weight.S + X.Fat.S*Weight.S, data = mc)
summary(mglm2)
VIF(mglm2)

# A modellek becslései:

df = data.frame(Illesztett.értékek.1 = mglm$fitted.values, Illesztett.értékek.2 = mglm2$fitted.values,
                Reziduálisok.1 = mglm$residuals, Reziduálisok.2 = mglm2$residuals,
                Illesztett.értékek.k = (mglm$fitted.values - mglm2$fitted.values),
                Reziduálisok.k = (mglm$residuals - mglm2$residuals))

ggplot(df, aes(Illesztett.értékek.k)) +
  geom_histogram(fill = "lightblue1", color = "black") +
  labs(title = "A centrálás elõtti és centrálás utáni becslések közti különbségek") +
  xlab("A két érték közti különbség") +
  ylab(NULL) +
  scale_x_continuous(breaks = seq(min(df$Illesztett.értékek.k), max(df$Illesztett.értékek.k), (max(df$Illesztett.értékek.k)-min(df$Illesztett.értékek.k))/5),
                     labels = signif(seq(min(df$Illesztett.értékek.k), max(df$Illesztett.értékek.k), (max(df$Illesztett.értékek.k)-min(df$Illesztett.értékek.k))/5), 3)) +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 17))

ggplot(df, aes(Reziduálisok.k)) +
  geom_histogram(fill = "lightblue1", color = "black") +
  labs(title = "A centrálás elõtti és centrálás utáni reziduálisok közti különbségek") +
  xlab("A két érték közti különbség") +
  ylab(NULL) +
  scale_x_continuous(breaks = seq(min(df$Reziduálisok.k), max(df$Reziduálisok.k), (max(df$Reziduálisok.k)-min(df$Reziduálisok.k))/5),
                     labels = signif(seq(min(df$Reziduálisok.k), max(df$Reziduálisok.k), (max(df$Reziduálisok.k)-min(df$Reziduálisok.k))/5), 3)) +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 17))


# Generált adatokra:

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

df = data.frame("Értékek" = c(corx1, corx2, corcx1, corcx2),
                "Centrálás" = rep(c("Centrálás elõtt", "Centrálás után"), each = 2*ism),
                "Változók" = rep(rep(c("X1 és X1*X2 korreációja", "X2 és X1*X2 korrelációja"), each = ism), times = 2))

ggplot(df, aes(x = Értékek, fill = Centrálás)) + 
  geom_histogram(color = "black", binwidth = 0.02) +
  facet_grid(rows = vars(Változók),  scales = "free") +
  scale_fill_manual(values = c("indianred1", "seagreen1")) +
  labs(title = "A változók korrelációja") + ylab(NULL) +
  scale_x_continuous(breaks = round(seq(-0.3, 1, 0.1), 1))

# Átlag és szórás
mean(corx1)
mean(corx2)
sd(corx1)
sd(corx2)

mean(corcx1)
mean(corcx2)
sd(corcx1)
sd(corcx2)
