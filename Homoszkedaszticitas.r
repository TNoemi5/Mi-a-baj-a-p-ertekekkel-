library(ggplot2)
library(MASS)
library(lmtest)

df = read.csv("C:\\Users\\User\\Desktop\\Szakdolgozat\\R\\Heteroscedasticity.csv", header = T, sep = ",")

# Eredeti adatok

ggplot(df, aes(Population, Accidents)) +
  geom_smooth(method = lm, se = FALSE, lwd = 1.2) +
  geom_point(size = 2.5) +
  labs(y = "Balesetek sz�ma", x = "N�pess�g") +
  ggtitle("Balesetek sz�ma ~ N�pess�g") +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 15), axis.title = element_text(size = 17))

reg = lm(Accidents ~ Population, data = df)

df["Illesztett_�rt�k"] = reg$fitted.values
df["Rezidu�lisok"] = reg$residuals

ggplot(df, aes(Illesztett_�rt�k, Rezidu�lisok)) +
  geom_hline(yintercept = 0, col = "green", lwd = 1.2) +
  geom_point(size = 2.5) +
  labs(y = "Rezidu�lisok", x = "Illesztett �rt�kek") +
  ggtitle("Balesetek sz�ma ~ N�pess�g") +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 15), axis.title = element_text(size = 17))

summary(reg)$coefficients[2,4]


# V�ltoz�k �jradefini�l�sa

reg2 = lm(AccidentRate ~ Population, data = df)

df["Illesztett_�rt�k2"] = reg2$fitted.values
df["Rezidu�lisok2"] = reg2$residuals

ggplot(df, aes(Illesztett_�rt�k2, (Rezidu�lisok2-mean(Rezidu�lisok2))/sd(Rezidu�lisok2))) +
  geom_hline(yintercept = 0, col = "green", lwd = 1.2) +
  geom_point(size = 2.5) +
  labs(y = "Standardiz�lt rezidu�lisok", x = "Illesztett �rt�kek") +
  ggtitle("Baleseti ar�ny ~ N�pess�g") +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 15), axis.title = element_text(size = 17))

summary(reg2)$coefficients[2,4]


# S�lyozott regresszi�

df$Weight2 = df$Weight*dim(df)[1]/sum(df$Weight)

reg3 = lm(Accidents ~ Population, weights = Weight2, data = df)

df["Illesztett_�rt�k3"] = reg3$fitted.values
df["Rezidu�lisok3"] = reg3$residuals

ggplot(df, aes(Illesztett_�rt�k3, (Rezidu�lisok3-mean(Rezidu�lisok3))/sd(Rezidu�lisok3))) +
  geom_hline(yintercept = 0, col = "green", lwd = 1.2) +
  geom_point(size = 2.5) +
  labs(y = "Standardiz�lt rezidu�lisok", x = "Illesztett �rt�kek") +
  ggtitle("Balesetek sz�ma ~ N�pess�g (s�lyozott)") +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 15), axis.title = element_text(size = 17))

summary(reg3)$coefficients[2,4]


# A f�gg� v�ltoz� Box-Cox transzform�l�sa

bc = boxcox(df$Accidents ~ df$Population, plotit = FALSE)
lambda = bc$x[which.max(bc$y)]

reg4 = lm(((Accidents^lambda-1)/lambda) ~ Population, data = df)

df["Illesztett_�rt�k4"] = reg4$fitted.values
df["Rezidu�lisok4"] = reg4$residuals

ggplot(df, aes(Illesztett_�rt�k4, (Rezidu�lisok4-mean(Rezidu�lisok4))/sd(Rezidu�lisok4))) +
  geom_hline(yintercept = 0, col = "green", lwd = 1.2) +
  geom_point(size = 2.5) +
  labs(y = "Standardiz�lt rezidu�lisok", x = "Illesztett �rt�kek") +
  ggtitle("Balesetek sz�ma ~ N�pess�g (Box-Cox transzform�lt)") +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 15), axis.title = element_text(size = 17))

summary(reg4)$coefficients[2,4]

hist(df$Accidents, main = "Balesetek sz�m�nak eloszl�sa", xlab = "db", ylab = "Gyakoris�g", col = "green",
     cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.2)
hist((df$Accidents^lambda-1)/lambda, main = "Box-Cox transzform�l�s ut�n a balesetek sz�m�nak eloszl�sa",
     xlab = "db", ylab = "Gyakoris�g", col = "green", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.2)


# A homoszkedaszticit�s tesztel�se

bptest(reg)
bptest(reg2)
bptest(reg3)
bptest(reg4)

# A rezidu�lisok sz�r�sa visszatranszform�l�s ut�n

sd(reg$residuals)
sd(reg2$residuals*df$Population)
sd(reg3$residuals)
trafy = (1+lambda*reg4$fitted.values)^(1/lambda)
sd(df$Accidents-trafy)


# A Box-Cox transzform�ci� �s a s�lyozott regresszi� befoly�soss�ga f�ggetlen gener�lt adatok eset�n

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

P = data.frame(p = p, p2 = p2, p3 = p3, ar�ny = sum(p < 0.05)/ism, ar�ny2 = sum(p2 < 0.05)/ism, ar�ny3 = sum(p3 < 0.05)/ism)

ggplot(P, aes(p)) +
  geom_histogram(fill = "peachpuff", colour = "black", breaks = seq(0, 1, 0.05)) +
  geom_vline(xintercept = 0.05, col = "red", lwd = 1.2) +
  labs(x = "p �rt�k", y = NULL) +
  ggtitle("A p �rt�kek eloszl�sa az alapadatok eset�n") +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 15), axis.title = element_text(size = 17)) +
  geom_text(mapping = aes(x = 0.75, y = 6*ism/100), label = paste("Els�f.hiba ar�ny: ", round(P$ar�ny, 4)), size = 6) +
  theme(plot.title = element_text(size = 18), axis.title = element_text(size = 18))

ggplot(P, aes(p2)) +
  geom_histogram(fill = "peachpuff", colour = "black", breaks = seq(0, 1, 0.05)) +
  geom_vline(xintercept = 0.05, col = "red", lwd = 1.2) +
  labs(x = "p �rt�k", y = NULL) +
  ggtitle("A p �rt�kek eloszl�sa a transzform�lt adatok eset�n") +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 15), axis.title = element_text(size = 17)) +
  geom_text(mapping = aes(x = 0.75, y = 6*ism/100), label = paste("Els�f.hiba ar�ny: ", round(P$ar�ny2, 4)), size = 6) +
  theme(plot.title = element_text(size = 18), axis.title = element_text(size = 18))

ggplot(P, aes(p3)) +
  geom_histogram(fill = "peachpuff", colour = "black", breaks = seq(0, 1, 0.05)) +
  geom_vline(xintercept = 0.05, col = "red", lwd = 1.2) +
  labs(x = "p �rt�k", y = NULL) +
  ggtitle("A p �rt�kek eloszl�sa s�lyozott regresszi� eset�n") +
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 15), axis.title = element_text(size = 17)) +
  geom_text(mapping = aes(x = 0.75, y = 6*ism/100), label = paste("Els�f.hiba ar�ny: ", round(P$ar�ny3, 4)), size = 6) +
  theme(plot.title = element_text(size = 18), axis.title = element_text(size = 18))

ka = 0.05 - qnorm(0.975)*sd(p)/sqrt(5000)
kf = 0.05 + qnorm(0.975)*sd(p)/sqrt(5000)
  
# p �rt�kek egyenletes eloszl�s tesztel�se

ks.test(p, punif)
ks.test(p2, punif)
ks.test(p3, punif)