library(ggplot2)
library(MASS)
library(lmtest)

df = read.csv("C:\\Users\\User\\Desktop\\Szakdolgozat\\R\\Heteroscedasticity.csv", header = T, sep = ",")

# Eredeti adatok

ggplot(df, aes(Population, Accidents)) +
  geom_smooth(method = lm, se = FALSE, lwd = 1.2) +
  geom_point(size = 2.5) +
  labs(y = "Balesetek száma", x = "Népesség") +
  ggtitle("Balesetek száma ~ Népesség") +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 15), axis.title = element_text(size = 17))

reg = lm(Accidents ~ Population, data = df)

df["Illesztett_érték"] = reg$fitted.values
df["Reziduálisok"] = reg$residuals

ggplot(df, aes(Illesztett_érték, Reziduálisok)) +
  geom_hline(yintercept = 0, col = "green", lwd = 1.2) +
  geom_point(size = 2.5) +
  labs(y = "Reziduálisok", x = "Illesztett értékek") +
  ggtitle("Balesetek száma ~ Népesség") +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 15), axis.title = element_text(size = 17))

summary(reg)$coefficients[2,4]


# Változók újradefiniálása

reg2 = lm(AccidentRate ~ Population, data = df)

df["Illesztett_érték2"] = reg2$fitted.values
df["Reziduálisok2"] = reg2$residuals

ggplot(df, aes(Illesztett_érték2, (Reziduálisok2-mean(Reziduálisok2))/sd(Reziduálisok2))) +
  geom_hline(yintercept = 0, col = "green", lwd = 1.2) +
  geom_point(size = 2.5) +
  labs(y = "Standardizált reziduálisok", x = "Illesztett értékek") +
  ggtitle("Baleseti arány ~ Népesség") +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 15), axis.title = element_text(size = 17))

summary(reg2)$coefficients[2,4]


# Súlyozott regresszió

df$Weight2 = df$Weight*dim(df)[1]/sum(df$Weight)

reg3 = lm(Accidents ~ Population, weights = Weight2, data = df)

df["Illesztett_érték3"] = reg3$fitted.values
df["Reziduálisok3"] = reg3$residuals

ggplot(df, aes(Illesztett_érték3, (Reziduálisok3-mean(Reziduálisok3))/sd(Reziduálisok3))) +
  geom_hline(yintercept = 0, col = "green", lwd = 1.2) +
  geom_point(size = 2.5) +
  labs(y = "Standardizált reziduálisok", x = "Illesztett értékek") +
  ggtitle("Balesetek száma ~ Népesség (súlyozott)") +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 15), axis.title = element_text(size = 17))

summary(reg3)$coefficients[2,4]


# A függõ változó Box-Cox transzformálása

bc = boxcox(df$Accidents ~ df$Population, plotit = FALSE)
lambda = bc$x[which.max(bc$y)]

reg4 = lm(((Accidents^lambda-1)/lambda) ~ Population, data = df)

df["Illesztett_érték4"] = reg4$fitted.values
df["Reziduálisok4"] = reg4$residuals

ggplot(df, aes(Illesztett_érték4, (Reziduálisok4-mean(Reziduálisok4))/sd(Reziduálisok4))) +
  geom_hline(yintercept = 0, col = "green", lwd = 1.2) +
  geom_point(size = 2.5) +
  labs(y = "Standardizált reziduálisok", x = "Illesztett értékek") +
  ggtitle("Balesetek száma ~ Népesség (Box-Cox transzformált)") +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 15), axis.title = element_text(size = 17))

summary(reg4)$coefficients[2,4]

hist(df$Accidents, main = "Balesetek számának eloszlása", xlab = "db", ylab = "Gyakoriság", col = "green",
     cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.2)
hist((df$Accidents^lambda-1)/lambda, main = "Box-Cox transzformálás után a balesetek számának eloszlása",
     xlab = "db", ylab = "Gyakoriság", col = "green", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.2)


# A homoszkedaszticitás tesztelése

bptest(reg)
bptest(reg2)
bptest(reg3)
bptest(reg4)

# A reziduálisok szórása visszatranszformálás után

sd(reg$residuals)
sd(reg2$residuals*df$Population)
sd(reg3$residuals)
trafy = (1+lambda*reg4$fitted.values)^(1/lambda)
sd(df$Accidents-trafy)


# A Box-Cox transzformáció és a súlyozott regresszió befolyásossága független generált adatok esetén

n = 1000
ism = 5000

x = sort(runif(n))
y = c(rnorm(n/5, mean = 30, sd = 1), rnorm(n/5, mean = 30, sd = 2), rnorm(n/5, mean = 30, sd = 3), rnorm(n/5, mean = 30, sd = 4), rnorm(n/5, mean = 30, sd = 5))

abra = data.frame(x = x, y = y)

ggplot(abra, aes(x, y)) +
  geom_point(size = 2.5) +
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 17))

p = c()
p2 = c()
p3 = c()
s = c()
s2 = c()
s3 = c()

set.seed(42)

for (i in 1:ism){
  x = sort(runif(n))
  y = c(rnorm(n/5, mean = 30, sd = 1), rnorm(n/5, mean = 30, sd = 2), rnorm(n/5, mean = 30, sd = 3), rnorm(n/5, mean = 30, sd = 4), rnorm(n/5, mean = 30, sd = 5))
  
  lr = lm(y ~ x)
  bc = boxcox(y ~ x, plotit = FALSE)
  lambda = bc$x[which.max(bc$y)]
  lr2 = lm(((y^lambda-1)/lambda) ~ x)
  W = c(rep(1, n/5), rep(1/4, n/5), rep(1/9, n/5), rep(1/16, n/5), rep(1/25, n/5))
  lr3 = lm(y ~ x, weights = W)
  
  p = c(p, summary(lr)$coefficients[2,4])
  p2 = c(p2, summary(lr2)$coefficients[2,4])
  p3 = c(p3, summary(lr3)$coefficients[2,4])
  s = c(s, sd(lr$residuals))
  tr = (1+lambda*lr2$fitted.values)^(1/lambda)
  s2 = c(s2, sd(y - tr))
  s3 = c(s3, sd(lr3$residuals))
}

mean(s)
mean(s2)
mean(s3)

P = data.frame(p = p, p2 = p2, p3 = p3, arány = sum(p < 0.05)/ism, arány2 = sum(p2 < 0.05)/ism, arány3 = sum(p3 < 0.05)/ism)

ggplot(P, aes(p)) +
  geom_histogram(fill = "peachpuff", colour = "black", breaks = seq(0, 1, 0.05)) +
  geom_vline(xintercept = 0.05, col = "red", lwd = 1.2) +
  labs(x = "p érték", y = NULL) +
  ggtitle("A p értékek eloszlása az alapadatok esetén") +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 15), axis.title = element_text(size = 17)) +
  geom_text(mapping = aes(x = 0.75, y = 6*ism/100), label = paste("Elsõf.hiba arány: ", round(P$arány, 4)), size = 6) +
  theme(plot.title = element_text(size = 18), axis.title = element_text(size = 18))

ggplot(P, aes(p2)) +
  geom_histogram(fill = "peachpuff", colour = "black", breaks = seq(0, 1, 0.05)) +
  geom_vline(xintercept = 0.05, col = "red", lwd = 1.2) +
  labs(x = "p érték", y = NULL) +
  ggtitle("A p értékek eloszlása a transzformált adatok esetén") +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 15), axis.title = element_text(size = 17)) +
  geom_text(mapping = aes(x = 0.75, y = 6*ism/100), label = paste("Elsõf.hiba arány: ", round(P$arány2, 4)), size = 6) +
  theme(plot.title = element_text(size = 18), axis.title = element_text(size = 18))

ggplot(P, aes(p3)) +
  geom_histogram(fill = "peachpuff", colour = "black", breaks = seq(0, 1, 0.05)) +
  geom_vline(xintercept = 0.05, col = "red", lwd = 1.2) +
  labs(x = "p érték", y = NULL) +
  ggtitle("A p értékek eloszlása súlyozott regresszió esetén") +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 15), axis.title = element_text(size = 17)) +
  geom_text(mapping = aes(x = 0.75, y = 6*ism/100), label = paste("Elsõf.hiba arány: ", round(P$arány3, 4)), size = 6) +
  theme(plot.title = element_text(size = 18), axis.title = element_text(size = 18))

ka = 0.05 - qnorm(0.975)*sd(p)/sqrt(5000)
kf = 0.05 + qnorm(0.975)*sd(p)/sqrt(5000)
  
# p értékek egyenletes eloszlás tesztelése

ks.test(p, punif)
ks.test(p2, punif)
ks.test(p3, punif)