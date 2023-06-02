library(DescTools)
library(TrustGauss)
library(ggplot2)

# A v�lasztott eloszl�sok (norm�lis, zero-inflated norm�lis kateg�ri�kkal,
# zero-inflated norm�lis, abszol�t norm�lis, Student-f�le t, negat�v binomi�lis,
# indik�tor, gamma) hisztogramjai
# 1000 mintaelemsz�m eset�n:

par(mfrow = c(2,4), mai = c(0.4,0.4,0.4,0.1))

n = rnorm(1000, mean = 0, sd = 1)
zink = c()
for (j in 1:1000){
  r = runif(1)
  if (r <= 0.5)
    zink = c(zink, 0)
  else
    zink = c(zink, rnorm(1, mean = 3, sd = 1))
}
for (j in 1:1000){
  if (0 < zink[j] & zink[j] <= (max(zink)-min(zink))/4){
    zink[j] = 1
  } else if ((max(zink)-min(zink))/4 < zink[j] & zink[j] <= 2*(max(zink)-min(zink))/4){
    zink[j] = 2
  } else if (2*(max(zink)-min(zink))/4 < zink[j] & zink[j] <= 3*(max(zink)-min(zink))/4){
    zink[j] = 3
  } else if (3*(max(zink)-min(zink))/4 < zink[j] & zink[j] <= 3*(max(zink)-min(zink))){
    zink[j] = 4
  }
}
zin = c()
for (j in 1:1000){
  r = runif(1)
  if (r <= 0.5)
    zin = c(zin, 0)
  else
    zin = c(zin, rnorm(1, mean = 3, sd = 1))
}
an = abs(rnorm(1000, mean = 0, sd = 1))
t = rt(1000, df = 4)
nb = rnbinom(1000, size = 1, prob = 1/11)
i = rbinom(1000, size = 1, prob = 0.9)
g = rgamma(1000, shape = 0.1, scale = 100)

hist(n, main = "Norm�lis", freq = FALSE, xlab = NULL, ylab = NULL, col = "chartreuse", cex.axis = 1.3, cex.main = 1.5)
hist(zink, main = "Zero-inflated norm�lis\nkateg�ri�kkal", freq = FALSE, xlab = NULL, ylab = NULL, col = "chartreuse", cex.axis = 1.3, cex.main = 1.5)
hist(zin, main = "Zero-inflated norm�lis", freq = FALSE, xlab = NULL, ylab = NULL, col = "chartreuse", cex.axis = 1.3, cex.main = 1.5)
hist(an, main = "Norm�lis abszol�t\n�rt�ke", freq = FALSE, xlab = NULL, ylab = NULL, col = "chartreuse", cex.axis = 1.3, cex.main = 1.5)
hist(t, main = "Student-f�le t", freq = FALSE, xlab = NULL, ylab = NULL, col = "chartreuse", cex.axis = 1.3, cex.main = 1.5)
hist(nb, main = "Negat�v binomi�lis", freq = FALSE, xlab = NULL, ylab = NULL, col = "chartreuse", cex.axis = 1.3, cex.main = 1.5)
hist(i, main = "Indik�tor", freq = FALSE, xlab = NULL, ylab = NULL, col = "chartreuse", cex.axis = 1.3, cex.main = 1.5)
hist(g, main = "Gamma", freq = FALSE, xlab = NULL, ylab = NULL, col = "chartreuse", cex.axis = 1.3, cex.main = 1.5)

# A szimul�ci�k:
  
fam = "gaussian"
  
sim = 5000
  
sam = 1000
  
# 1. Y: standard norm�lis eloszl�s� - m = 0, sigma = 1 param�terekkel
  
  Y1X1 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gaussian", DistributionXCov = "Gaussian", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  MeanX.gauss = 0, SDX.gauss = 1)
  Y1X2 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gaussian", DistributionXCov = "GaussianZeroCategorical", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  MeanX.gauss = 3, SDX.gauss = 1, nCategoriesX.cat=5)
  Y1X3 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gaussian", DistributionXCov = "GaussianZero", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  MeanX.gauss = 3, SDX.gauss = 1, zeroLevelX.zero=0.50)
  Y1X4 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gaussian", DistributionXCov = "AbsoluteGaussian", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  MeanX.gauss = 0, SDX.gauss = 1)
  Y1X5 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gaussian", DistributionXCov = "StudentsT", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  DFX.student = 4)
  Y1X6 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gaussian", DistributionXCov = "NegativeBinomial", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  ShapeX.gamma = 1, ScaleX.gamma = 10)
  Y1X7 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gaussian", DistributionXCov = "Binomial", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  zeroLevelX.zero = 0.90)
  Y1X8 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gaussian", DistributionXCov = "Gamma", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  ShapeX.gamma = 0.1, ScaleX.gamma = 100)
  
# 2. Y: zero-inflated norm�lis eloszl�s� kateg�ri�kkal - m = 3, sigma = 1, cat = 5 param�terekkel
  
  Y2X1 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZeroCategorical", DistributionXCov = "Gaussian", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1,  MeanX.gauss = 0, SDX.gauss = 1)
  Y2X2 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZeroCategorical", DistributionXCov = "GaussianZeroCategorical", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, nCategoriesY.cat=5,  MeanX.gauss = 3, SDX.gauss = 1, nCategoriesX.cat=5)
  Y2X3 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZeroCategorical", DistributionXCov = "GaussianZero", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, nCategoriesY.cat=5,  MeanX.gauss = 3, SDX.gauss = 1, zeroLevelX.zero=0.50)
  Y2X4 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZeroCategorical", DistributionXCov = "AbsoluteGaussian", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, nCategoriesY.cat=5,  MeanX.gauss = 0, SDX.gauss = 1)
  Y2X5 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZeroCategorical", DistributionXCov = "StudentsT", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, nCategoriesY.cat=5,  DFX.student = 4)
  Y2X6 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZeroCategorical", DistributionXCov = "NegativeBinomial", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, nCategoriesY.cat=5,  ShapeX.gamma = 1, ScaleX.gamma = 10)
  Y2X7 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZeroCategorical", DistributionXCov = "Binomial", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, nCategoriesY.cat=5,  zeroLevelX.zero = 0.90)
  Y2X8 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZeroCategorical", DistributionXCov = "Gamma", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, nCategoriesY.cat=5,  ShapeX.gamma = 0.1, ScaleX.gamma = 100)
  
# 3. Y: zero-inflated norm�lis eloszl�s� - m = 3, sigma = 1, P(Y=0) = 0.5 param�terekkel
  
  Y3X1 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZero", DistributionXCov = "Gaussian", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, zeroLevelY.zero=0.50,  MeanX.gauss = 0, SDX.gauss = 1)
  Y3X2 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZero", DistributionXCov = "GaussianZeroCategorical", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, zeroLevelY.zero=0.50,  MeanX.gauss = 3, SDX.gauss = 1, nCategoriesX.cat=5)
  Y3X3 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZero", DistributionXCov = "GaussianZero", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, zeroLevelY.zero=0.50,  MeanX.gauss = 3, SDX.gauss = 1, zeroLevelX.zero=0.50)
  Y3X4 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZero", DistributionXCov = "AbsoluteGaussian", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, zeroLevelY.zero=0.50,  MeanX.gauss = 0, SDX.gauss = 1)
  Y3X5 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZero", DistributionXCov = "StudentsT", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, zeroLevelY.zero=0.50,  DFX.student = 4)
  Y3X6 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZero", DistributionXCov = "NegativeBinomial", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, zeroLevelY.zero=0.50,  ShapeX.gamma = 1, ScaleX.gamma = 10)
  Y3X7 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZero", DistributionXCov = "Binomial", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, zeroLevelY.zero=0.50,  zeroLevelX.zero = 0.90)
  Y3X8 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZero", DistributionXCov = "Gamma", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, zeroLevelY.zero=0.50,  ShapeX.gamma = 0.1, ScaleX.gamma = 100)
  
# 4. Y: abszol�t norm�lis eloszl�s� - m = 0, sigma = 1 param�terekkel
  
  Y4X1 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="AbsoluteGaussian", DistributionXCov = "Gaussian", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  MeanX.gauss = 0, SDX.gauss = 1)
  Y4X2 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="AbsoluteGaussian", DistributionXCov = "GaussianZeroCategorical", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  MeanX.gauss = 3, SDX.gauss = 1, nCategoriesX.cat=5)
  Y4X3 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="AbsoluteGaussian", DistributionXCov = "GaussianZero", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  MeanX.gauss = 3, SDX.gauss = 1, zeroLevelX.zero=0.50)
  Y4X4 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="AbsoluteGaussian", DistributionXCov = "AbsoluteGaussian", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  MeanX.gauss = 0, SDX.gauss = 1)
  Y4X5 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="AbsoluteGaussian", DistributionXCov = "StudentsT", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  DFX.student = 4)
  Y4X6 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="AbsoluteGaussian", DistributionXCov = "NegativeBinomial", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  ShapeX.gamma = 1, ScaleX.gamma = 10)
  Y4X7 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="AbsoluteGaussian", DistributionXCov = "Binomial", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  zeroLevelX.zero = 0.90)
  Y4X8 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="AbsoluteGaussian", DistributionXCov = "Gamma", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  ShapeX.gamma = 0.1, ScaleX.gamma = 100)
  
# 5. Y: Student-f�le t eloszl�s� - 4 szabads�gi fokkal
  
  Y5X1 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="StudentsT", DistributionXCov = "Gaussian", DistributionXFac = NULL,  DFY.student = 4,  MeanX.gauss = 0, SDX.gauss = 1)
  Y5X2 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="StudentsT", DistributionXCov = "GaussianZeroCategorical", DistributionXFac = NULL,  DFY.student = 4,  MeanX.gauss = 3, SDX.gauss = 1, nCategoriesX.cat=5)
  Y5X3 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="StudentsT", DistributionXCov = "GaussianZero", DistributionXFac = NULL,  DFY.student = 4,  MeanX.gauss = 3, SDX.gauss = 1, zeroLevelX.zero=0.50)
  Y5X4 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="StudentsT", DistributionXCov = "AbsoluteGaussian", DistributionXFac = NULL,  DFY.student = 4,  MeanX.gauss = 0, SDX.gauss = 1)
  Y5X5 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="StudentsT", DistributionXCov = "StudentsT", DistributionXFac = NULL,  DFY.student = 4,  DFX.student = 4)
  Y5X6 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="StudentsT", DistributionXCov = "NegativeBinomial", DistributionXFac = NULL,  DFY.student = 4,  ShapeX.gamma = 1, ScaleX.gamma = 10)
  Y5X7 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="StudentsT", DistributionXCov = "Binomial", DistributionXFac = NULL,  DFY.student = 4,  zeroLevelX.zero = 0.90)
  Y5X8 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="StudentsT", DistributionXCov = "Gamma", DistributionXFac = NULL,  DFY.student = 4,  ShapeX.gamma = 0.1, ScaleX.gamma = 100)
  
# 6. Y: negat�v binomi�lis eloszl�s� - n = 1, p = 1/11 param�terekkel
  
  Y6X1 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="NegativeBinomial", DistributionXCov = "Gaussian", DistributionXFac = NULL,  ShapeY.gamma = 1, ScaleY.gamma = 10,  MeanX.gauss = 0, SDX.gauss = 1)
  Y6X2 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="NegativeBinomial", DistributionXCov = "GaussianZeroCategorical", DistributionXFac = NULL,  ShapeY.gamma = 1, ScaleY.gamma = 10,  MeanX.gauss = 3, SDX.gauss = 1, nCategoriesX.cat=5)
  Y6X3 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="NegativeBinomial", DistributionXCov = "GaussianZero", DistributionXFac = NULL,  ShapeY.gamma = 1, ScaleY.gamma = 10,  MeanX.gauss = 3, SDX.gauss = 1, zeroLevelX.zero=0.50)
  Y6X4 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="NegativeBinomial", DistributionXCov = "AbsoluteGaussian", DistributionXFac = NULL,  ShapeY.gamma = 1, ScaleY.gamma = 10,  MeanX.gauss = 0, SDX.gauss = 1)
  Y6X5 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="NegativeBinomial", DistributionXCov = "StudentsT", DistributionXFac = NULL,  ShapeY.gamma = 1, ScaleY.gamma = 10,  DFX.student = 4)
  Y6X6 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="NegativeBinomial", DistributionXCov = "NegativeBinomial", DistributionXFac = NULL,  ShapeY.gamma = 1, ScaleY.gamma = 10,  ShapeX.gamma = 1, ScaleX.gamma = 10)
  Y6X7 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="NegativeBinomial", DistributionXCov = "Binomial", DistributionXFac = NULL,  ShapeY.gamma = 1, ScaleY.gamma = 10,  zeroLevelX.zero = 0.90)
  Y6X8 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="NegativeBinomial", DistributionXCov = "Gamma", DistributionXFac = NULL,  ShapeY.gamma = 1, ScaleY.gamma = 10,  ShapeX.gamma = 0.1, ScaleX.gamma = 100)
  
# 7. Y: indik�tor eloszl�s� - p = 0.9 param�terekkel
  
  Y7X1 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Binomial", DistributionXCov = "Gaussian", DistributionXFac = NULL,  zeroLevelY.zero = 0.90,  MeanX.gauss = 0, SDX.gauss = 1)
  Y7X2 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Binomial", DistributionXCov = "GaussianZeroCategorical", DistributionXFac = NULL,  zeroLevelY.zero = 0.90,  MeanX.gauss = 3, SDX.gauss = 1, nCategoriesX.cat=5)
  Y7X3 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Binomial", DistributionXCov = "GaussianZero", DistributionXFac = NULL,  zeroLevelY.zero = 0.90,  MeanX.gauss = 3, SDX.gauss = 1, zeroLevelX.zero=0.50)
  Y7X4 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Binomial", DistributionXCov = "AbsoluteGaussian", DistributionXFac = NULL,  zeroLevelY.zero = 0.90,  MeanX.gauss = 0, SDX.gauss = 1)
  Y7X5 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Binomial", DistributionXCov = "StudentsT", DistributionXFac = NULL,  zeroLevelY.zero = 0.90,  DFX.student = 4)
  Y7X6 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Binomial", DistributionXCov = "NegativeBinomial", DistributionXFac = NULL,  zeroLevelY.zero = 0.90,  ShapeX.gamma = 1, ScaleX.gamma = 10)
  Y7X7 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Binomial", DistributionXCov = "Binomial", DistributionXFac = NULL,  zeroLevelY.zero = 0.90,  zeroLevelX.zero = 0.90)
  Y7X8 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Binomial", DistributionXCov = "Gamma", DistributionXFac = NULL,  zeroLevelY.zero = 0.90,  ShapeX.gamma = 0.1, ScaleX.gamma = 100)
  
# 8. Y: gamma eloszl�s� - alpha = 0.1, lambda = 100 param�terekkel
  
  Y8X1 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gamma", DistributionXCov = "Gaussian", DistributionXFac = NULL,  ShapeY.gamma = 0.1, ScaleY.gamma = 100,  MeanX.gauss = 0, SDX.gauss = 1)
  Y8X2 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gamma", DistributionXCov = "GaussianZeroCategorical", DistributionXFac = NULL,  ShapeY.gamma = 0.1, ScaleY.gamma = 100,  MeanX.gauss = 3, SDX.gauss = 1, nCategoriesX.cat=5)
  Y8X3 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gamma", DistributionXCov = "GaussianZero", DistributionXFac = NULL,  ShapeY.gamma = 0.1, ScaleY.gamma = 100,  MeanX.gauss = 3, SDX.gauss = 1, zeroLevelX.zero=0.50)
  Y8X4 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gamma", DistributionXCov = "AbsoluteGaussian", DistributionXFac = NULL,  ShapeY.gamma = 0.1, ScaleY.gamma = 100,  MeanX.gauss = 0, SDX.gauss = 1)
  Y8X5 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gamma", DistributionXCov = "StudentsT", DistributionXFac = NULL,  ShapeY.gamma = 0.1, ScaleY.gamma = 100,  DFX.student = 4)
  Y8X6 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gamma", DistributionXCov = "NegativeBinomial", DistributionXFac = NULL,  ShapeY.gamma = 0.1, ScaleY.gamma = 100,  ShapeX.gamma = 1, ScaleX.gamma = 10)
  Y8X7 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gamma", DistributionXCov = "Binomial", DistributionXFac = NULL,  ShapeY.gamma = 0.1, ScaleY.gamma = 100,  zeroLevelX.zero = 0.90)
  Y8X8 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gamma", DistributionXCov = "Gamma", DistributionXFac = NULL,  ShapeY.gamma = 0.1, ScaleY.gamma = 100,  ShapeX.gamma = 0.1, ScaleX.gamma = 100)
  
  
# A p �rt�kek vizu�lis megjelen�t�se "h�t�rk�pen":
  
  par(mfrow = c(1,1))
  
  pY1 = c(Y1X1$Alphas.05, Y1X2$Alphas.05, Y1X3$Alphas.05, Y1X4$Alphas.05, Y1X5$Alphas.05, Y1X6$Alphas.05, Y1X7$Alphas.05, Y1X8$Alphas.05)
  pY2 = c(Y2X1$Alphas.05, Y2X2$Alphas.05, Y2X3$Alphas.05, Y2X4$Alphas.05, Y2X5$Alphas.05, Y2X6$Alphas.05, Y2X7$Alphas.05, Y2X8$Alphas.05)
  pY3 = c(Y3X1$Alphas.05, Y3X2$Alphas.05, Y3X3$Alphas.05, Y3X4$Alphas.05, Y3X5$Alphas.05, Y3X6$Alphas.05, Y3X7$Alphas.05, Y3X8$Alphas.05)
  pY4 = c(Y4X1$Alphas.05, Y4X2$Alphas.05, Y4X3$Alphas.05, Y4X4$Alphas.05, Y4X5$Alphas.05, Y4X6$Alphas.05, Y4X7$Alphas.05, Y4X8$Alphas.05)
  pY5 = c(Y5X1$Alphas.05, Y5X2$Alphas.05, Y5X3$Alphas.05, Y5X4$Alphas.05, Y5X5$Alphas.05, Y5X6$Alphas.05, Y5X7$Alphas.05, Y5X8$Alphas.05)
  pY6 = c(Y6X1$Alphas.05, Y6X2$Alphas.05, Y6X3$Alphas.05, Y6X4$Alphas.05, Y6X5$Alphas.05, Y6X6$Alphas.05, Y6X7$Alphas.05, Y6X8$Alphas.05)
  pY7 = c(Y7X1$Alphas.05, Y7X2$Alphas.05, Y7X3$Alphas.05, Y7X4$Alphas.05, Y7X5$Alphas.05, Y7X6$Alphas.05, Y7X7$Alphas.05, Y7X8$Alphas.05)
  pY8 = c(Y8X1$Alphas.05, Y8X2$Alphas.05, Y8X3$Alphas.05, Y8X4$Alphas.05, Y8X5$Alphas.05, Y8X6$Alphas.05, Y8X7$Alphas.05, Y8X8$Alphas.05)
  
  Y = c('1','2','3','4','5','6','7','8')
  X = c('1','2','3','4','5','6','7','8')
  p1000 = expand.grid(x = X, y = Y)
  p1000$p = c(pY1, pY2, pY3, pY4, pY5, pY6, pY7, pY8)
  
  # 95 %-os konfidencia intervallum als� �s fels� hat�ra a 0.05-�s v�rhat� �rt�kre:
  also1000 = round(0.05 - qnorm(0.975)*sd(p1000$p)/sqrt(64), digits = 3)
  felso1000 = round(0.05 + qnorm(0.975)*sd(p1000$p)/sqrt(64), digits = 3)
  p1000$m = c(rep(paste(" 95 %-os konf. int.: ", also1000, "-", felso1000, sep = ""),64))
  
P = rbind(p10, p100, p1000)
  
ggplot(P, aes(x, y, fill = p)) +
    geom_tile() +
    scale_fill_gradient(low = "turquoise1", high = "yellow", breaks = c(0.01,0.02,0.04,0.05,0.06,0.08,0.1)) +
    coord_fixed() +
    guides(fill = guide_colourbar(title = NULL, barheight = 11, breaks = 5)) +
    ggtitle("Elsofaj� hibaar�ny 10, 100 �s 1000 mintaelemsz�m eset�n (alpha = 0.05)") +
    facet_grid(~factor(m, levels = c(paste("95 %-os konf. int.: ", also10, "-", felso10, sep = ""),paste("95 %-os konf. int.: ", also100, "-", felso100, sep = ""),paste(" 95 %-os konf. int.: ", also1000, "-", felso1000, sep = ""))))
  
# p �rt�kek hisztogramjai Y = standard norm�lis eloszl�s� f�gg� v�ltoz� eset�ben:
    
par(mfrow = c(2,4), mai = c(0.4,0.4,0.4,0.1))
  
  hist(Y1X1$Pvals[,1], breaks = 20, main = "Norm�lis", col = "darkseagreen1", freq = FALSE, xlab = NULL, ylab = NULL)
  abline(v = 0.05, col = "red")
  hist(Y1X2$Pvals[,1], breaks = 20, main = "Zero-inflated norm�lis kateg�ri�kkal", col = "darkseagreen1", freq = FALSE, xlab = NULL, ylab = NULL)
  abline(v = 0.05, col = "red")
  hist(Y1X3$Pvals[,1], breaks = 20, main = "Zero-inflated norm�lis", col = "darkseagreen1", freq = FALSE, xlab = NULL, ylab = NULL)
  abline(v = 0.05, col = "red")
  hist(Y1X4$Pvals[,1], breaks = 20, main = "Abszol�t norm�lis", col = "darkseagreen1", freq = FALSE, xlab = NULL, ylab = NULL)
  abline(v = 0.05, col = "red")
  hist(Y1X5$Pvals[,1], breaks = 20, main = "Student-f�le t", col = "darkseagreen1", freq = FALSE, xlab = NULL, ylab = NULL)
  abline(v = 0.05, col = "red")
  hist(Y1X6$Pvals[,1], breaks = 20, main = "Negat�v binomi�lis", col = "darkseagreen1", freq = FALSE, xlab = NULL, ylab = NULL)
  abline(v = 0.05, col = "red")
  hist(Y1X7$Pvals[,1], breaks = 20, main = "Indik�tor", col = "darkseagreen1", freq = FALSE, xlab = NULL, ylab = NULL)
  abline(v = 0.05, col = "red")
  hist(Y1X8$Pvals[,1], breaks = 20, main = "Gamma", col = "darkseagreen1", freq = FALSE, xlab = NULL, ylab = NULL)
  abline(v = 0.05, col = "red")

  
# A hib�k eloszl�s�nak megs�rt�se Poisson eloszl�s eset�n
# A diszkr�t v�ltoz�kat vizsg�lva 100 mintaelemsz�m eset�n
  
  YzinkG = TrustGauss(Family = "gaussian", nSamples = 1000, nSimulations = 5000, SaveAllOutput = FALSE, DistributionY ="GaussianZeroCategorical", DistributionXCov = "GaussianZeroCategorical", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, nCategoriesY.cat=5,  MeanX.gauss = 3, SDX.gauss = 1, nCategoriesX.cat=5)
  YzinkP = TrustGauss(Family = "poisson", nSamples = 1000, nSimulations = 5000, SaveAllOutput = FALSE, DistributionY ="GaussianZeroCategorical", DistributionXCov = "GaussianZeroCategorical", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, nCategoriesY.cat=5,  MeanX.gauss = 3, SDX.gauss = 1, nCategoriesX.cat=5)
  YnbG = TrustGauss(Family = "gaussian", nSamples = 1000, nSimulations = 5000, SaveAllOutput = FALSE, DistributionY ="NegativeBinomial", DistributionXCov = "NegativeBinomial", DistributionXFac = NULL,  ShapeY.gamma = 1, ScaleY.gamma = 10,  ShapeX.gamma = 1, ScaleX.gamma = 10)
  YnbP = TrustGauss(Family = "poisson", nSamples = 1000, nSimulations = 5000, SaveAllOutput = FALSE, DistributionY ="NegativeBinomial", DistributionXCov = "NegativeBinomial", DistributionXFac = NULL,  ShapeY.gamma = 1, ScaleY.gamma = 10,  ShapeX.gamma = 1, ScaleX.gamma = 10)
  YiG = TrustGauss(Family = "gaussian", nSamples = 1000, nSimulations = 5000, SaveAllOutput = FALSE, DistributionY ="Binomial", DistributionXCov = "Binomial", DistributionXFac = NULL,  zeroLevelY.zero = 0.90,  zeroLevelX.zero = 0.90)
  YiP = TrustGauss(Family = "poisson", nSamples = 1000, nSimulations = 5000, SaveAllOutput = FALSE, DistributionY ="Binomial", DistributionXCov = "Binomial", DistributionXFac = NULL,  zeroLevelY.zero = 0.90,  zeroLevelX.zero = 0.90)
  
p_ertekek = c(YzinkG$Pvals[,1], YzinkP$Pvals[,1], YnbG$Pvals[,1], YnbP$Pvals[,1], YiG$Pvals[,1], YiP$Pvals[,1])
elsofh = c(rep(YzinkG$Alphas.05,5000), rep(YzinkP$Alphas.05,5000), rep(YnbG$Alphas.05,5000), rep(YnbP$Alphas.05,5000), rep(YiG$Alphas.05,5000), rep(YiP$Alphas.05,5000))
hiba = c(rep("Norm�lis",5000),rep("Poisson",5000),rep("Norm�lis",5000),rep("Poisson",5000),rep("Norm�lis",5000),rep("Poisson",5000))
eloszlas = c(rep("(2) Zero-inflated norm�lis\nkateg�ri�kkal",10000),rep("(6) Negat�v binomi�lis",10000),rep("(7) Indik�tor",10000))

ossz = data.frame(p_ertekek, elsofh, hiba, eloszlas)

ggplot(ossz, aes(x = p_ertekek, fill = factor(elsofh))) +
  geom_histogram(colour = "black", breaks = seq(0, 1, 0.05)) +
  geom_vline(xintercept = 0.05, col = "red", size = 0.8) +
  geom_text(ossz, mapping = aes(x = 0.5, y = rep(c(rep(290,5000),rep(2500,5000)),3), label = paste("Els�f.hiba: ", round(elsofh, 4)))) +
  ylab(NULL) + xlab("p �rt�kek") +
  facet_grid(vars(hiba), vars(eloszlas), scales = "free") +
  scale_fill_manual(values = c("turquoise1",rep("darkseagreen1",3),"#ccff33","yellow")) +
  theme(legend.position = "none")

# p �rt�kek egyenletes eloszl�s tesztel�se

ks.test(ossz[ossz$hiba == 'Norm�lis' & ossz$eloszlas == '(2) Zero-inflated norm�lis\nkateg�ri�kkal',]$p_ertekek, punif)
ks.test(ossz[ossz$hiba == 'Norm�lis' & ossz$eloszlas == '(6) Negat�v binomi�lis',]$p_ertekek, punif)
ks.test(ossz[ossz$hiba == 'Norm�lis' & ossz$eloszlas == '(7) Indik�tor',]$p_ertekek, punif)
ks.test(ossz[ossz$hiba == 'Poisson' & ossz$eloszlas == '(2) Zero-inflated norm�lis\nkateg�ri�kkal',]$p_ertekek, punif)
ks.test(ossz[ossz$hiba == 'Poisson' & ossz$eloszlas == '(6) Negat�v binomi�lis',]$p_ertekek, punif)
ks.test(ossz[ossz$hiba == 'Poisson' & ossz$eloszlas == '(7) Indik�tor',]$p_ertekek, punif)
