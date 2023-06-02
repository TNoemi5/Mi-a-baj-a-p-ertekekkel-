library(DescTools)
library(TrustGauss)
library(ggplot2)

# A választott eloszlások (normális, zero-inflated normális kategóriákkal,
# zero-inflated normális, abszolút normális, Student-féle t, negatív binomiális,
# indikátor, gamma) hisztogramjai
# 1000 mintaelemszám esetén:

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

hist(n, main = "Normális", freq = FALSE, xlab = NULL, ylab = NULL, col = "chartreuse", cex.axis = 1.3, cex.main = 1.5)
hist(zink, main = "Zero-inflated normális\nkategóriákkal", freq = FALSE, xlab = NULL, ylab = NULL, col = "chartreuse", cex.axis = 1.3, cex.main = 1.5)
hist(zin, main = "Zero-inflated normális", freq = FALSE, xlab = NULL, ylab = NULL, col = "chartreuse", cex.axis = 1.3, cex.main = 1.5)
hist(an, main = "Normális abszolút\nértéke", freq = FALSE, xlab = NULL, ylab = NULL, col = "chartreuse", cex.axis = 1.3, cex.main = 1.5)
hist(t, main = "Student-féle t", freq = FALSE, xlab = NULL, ylab = NULL, col = "chartreuse", cex.axis = 1.3, cex.main = 1.5)
hist(nb, main = "Negatív binomiális", freq = FALSE, xlab = NULL, ylab = NULL, col = "chartreuse", cex.axis = 1.3, cex.main = 1.5)
hist(i, main = "Indikátor", freq = FALSE, xlab = NULL, ylab = NULL, col = "chartreuse", cex.axis = 1.3, cex.main = 1.5)
hist(g, main = "Gamma", freq = FALSE, xlab = NULL, ylab = NULL, col = "chartreuse", cex.axis = 1.3, cex.main = 1.5)

# A szimulációk:
  
fam = "gaussian"
  
sim = 5000
  
sam = 1000
  
# 1. Y: standard normális eloszlású - m = 0, sigma = 1 paraméterekkel
  
  Y1X1 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gaussian", DistributionXCov = "Gaussian", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  MeanX.gauss = 0, SDX.gauss = 1)
  Y1X2 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gaussian", DistributionXCov = "GaussianZeroCategorical", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  MeanX.gauss = 3, SDX.gauss = 1, nCategoriesX.cat=5)
  Y1X3 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gaussian", DistributionXCov = "GaussianZero", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  MeanX.gauss = 3, SDX.gauss = 1, zeroLevelX.zero=0.50)
  Y1X4 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gaussian", DistributionXCov = "AbsoluteGaussian", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  MeanX.gauss = 0, SDX.gauss = 1)
  Y1X5 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gaussian", DistributionXCov = "StudentsT", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  DFX.student = 4)
  Y1X6 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gaussian", DistributionXCov = "NegativeBinomial", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  ShapeX.gamma = 1, ScaleX.gamma = 10)
  Y1X7 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gaussian", DistributionXCov = "Binomial", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  zeroLevelX.zero = 0.90)
  Y1X8 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gaussian", DistributionXCov = "Gamma", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  ShapeX.gamma = 0.1, ScaleX.gamma = 100)
  
# 2. Y: zero-inflated normális eloszlású kategóriákkal - m = 3, sigma = 1, cat = 5 paraméterekkel
  
  Y2X1 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZeroCategorical", DistributionXCov = "Gaussian", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1,  MeanX.gauss = 0, SDX.gauss = 1)
  Y2X2 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZeroCategorical", DistributionXCov = "GaussianZeroCategorical", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, nCategoriesY.cat=5,  MeanX.gauss = 3, SDX.gauss = 1, nCategoriesX.cat=5)
  Y2X3 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZeroCategorical", DistributionXCov = "GaussianZero", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, nCategoriesY.cat=5,  MeanX.gauss = 3, SDX.gauss = 1, zeroLevelX.zero=0.50)
  Y2X4 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZeroCategorical", DistributionXCov = "AbsoluteGaussian", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, nCategoriesY.cat=5,  MeanX.gauss = 0, SDX.gauss = 1)
  Y2X5 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZeroCategorical", DistributionXCov = "StudentsT", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, nCategoriesY.cat=5,  DFX.student = 4)
  Y2X6 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZeroCategorical", DistributionXCov = "NegativeBinomial", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, nCategoriesY.cat=5,  ShapeX.gamma = 1, ScaleX.gamma = 10)
  Y2X7 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZeroCategorical", DistributionXCov = "Binomial", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, nCategoriesY.cat=5,  zeroLevelX.zero = 0.90)
  Y2X8 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZeroCategorical", DistributionXCov = "Gamma", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, nCategoriesY.cat=5,  ShapeX.gamma = 0.1, ScaleX.gamma = 100)
  
# 3. Y: zero-inflated normális eloszlású - m = 3, sigma = 1, P(Y=0) = 0.5 paraméterekkel
  
  Y3X1 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZero", DistributionXCov = "Gaussian", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, zeroLevelY.zero=0.50,  MeanX.gauss = 0, SDX.gauss = 1)
  Y3X2 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZero", DistributionXCov = "GaussianZeroCategorical", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, zeroLevelY.zero=0.50,  MeanX.gauss = 3, SDX.gauss = 1, nCategoriesX.cat=5)
  Y3X3 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZero", DistributionXCov = "GaussianZero", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, zeroLevelY.zero=0.50,  MeanX.gauss = 3, SDX.gauss = 1, zeroLevelX.zero=0.50)
  Y3X4 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZero", DistributionXCov = "AbsoluteGaussian", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, zeroLevelY.zero=0.50,  MeanX.gauss = 0, SDX.gauss = 1)
  Y3X5 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZero", DistributionXCov = "StudentsT", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, zeroLevelY.zero=0.50,  DFX.student = 4)
  Y3X6 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZero", DistributionXCov = "NegativeBinomial", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, zeroLevelY.zero=0.50,  ShapeX.gamma = 1, ScaleX.gamma = 10)
  Y3X7 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZero", DistributionXCov = "Binomial", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, zeroLevelY.zero=0.50,  zeroLevelX.zero = 0.90)
  Y3X8 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="GaussianZero", DistributionXCov = "Gamma", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, zeroLevelY.zero=0.50,  ShapeX.gamma = 0.1, ScaleX.gamma = 100)
  
# 4. Y: abszolút normális eloszlású - m = 0, sigma = 1 paraméterekkel
  
  Y4X1 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="AbsoluteGaussian", DistributionXCov = "Gaussian", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  MeanX.gauss = 0, SDX.gauss = 1)
  Y4X2 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="AbsoluteGaussian", DistributionXCov = "GaussianZeroCategorical", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  MeanX.gauss = 3, SDX.gauss = 1, nCategoriesX.cat=5)
  Y4X3 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="AbsoluteGaussian", DistributionXCov = "GaussianZero", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  MeanX.gauss = 3, SDX.gauss = 1, zeroLevelX.zero=0.50)
  Y4X4 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="AbsoluteGaussian", DistributionXCov = "AbsoluteGaussian", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  MeanX.gauss = 0, SDX.gauss = 1)
  Y4X5 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="AbsoluteGaussian", DistributionXCov = "StudentsT", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  DFX.student = 4)
  Y4X6 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="AbsoluteGaussian", DistributionXCov = "NegativeBinomial", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  ShapeX.gamma = 1, ScaleX.gamma = 10)
  Y4X7 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="AbsoluteGaussian", DistributionXCov = "Binomial", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  zeroLevelX.zero = 0.90)
  Y4X8 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="AbsoluteGaussian", DistributionXCov = "Gamma", DistributionXFac = NULL,  MeanY.gauss = 0, SDY.gauss = 1,  ShapeX.gamma = 0.1, ScaleX.gamma = 100)
  
# 5. Y: Student-féle t eloszlású - 4 szabadsági fokkal
  
  Y5X1 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="StudentsT", DistributionXCov = "Gaussian", DistributionXFac = NULL,  DFY.student = 4,  MeanX.gauss = 0, SDX.gauss = 1)
  Y5X2 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="StudentsT", DistributionXCov = "GaussianZeroCategorical", DistributionXFac = NULL,  DFY.student = 4,  MeanX.gauss = 3, SDX.gauss = 1, nCategoriesX.cat=5)
  Y5X3 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="StudentsT", DistributionXCov = "GaussianZero", DistributionXFac = NULL,  DFY.student = 4,  MeanX.gauss = 3, SDX.gauss = 1, zeroLevelX.zero=0.50)
  Y5X4 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="StudentsT", DistributionXCov = "AbsoluteGaussian", DistributionXFac = NULL,  DFY.student = 4,  MeanX.gauss = 0, SDX.gauss = 1)
  Y5X5 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="StudentsT", DistributionXCov = "StudentsT", DistributionXFac = NULL,  DFY.student = 4,  DFX.student = 4)
  Y5X6 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="StudentsT", DistributionXCov = "NegativeBinomial", DistributionXFac = NULL,  DFY.student = 4,  ShapeX.gamma = 1, ScaleX.gamma = 10)
  Y5X7 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="StudentsT", DistributionXCov = "Binomial", DistributionXFac = NULL,  DFY.student = 4,  zeroLevelX.zero = 0.90)
  Y5X8 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="StudentsT", DistributionXCov = "Gamma", DistributionXFac = NULL,  DFY.student = 4,  ShapeX.gamma = 0.1, ScaleX.gamma = 100)
  
# 6. Y: negatív binomiális eloszlású - n = 1, p = 1/11 paraméterekkel
  
  Y6X1 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="NegativeBinomial", DistributionXCov = "Gaussian", DistributionXFac = NULL,  ShapeY.gamma = 1, ScaleY.gamma = 10,  MeanX.gauss = 0, SDX.gauss = 1)
  Y6X2 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="NegativeBinomial", DistributionXCov = "GaussianZeroCategorical", DistributionXFac = NULL,  ShapeY.gamma = 1, ScaleY.gamma = 10,  MeanX.gauss = 3, SDX.gauss = 1, nCategoriesX.cat=5)
  Y6X3 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="NegativeBinomial", DistributionXCov = "GaussianZero", DistributionXFac = NULL,  ShapeY.gamma = 1, ScaleY.gamma = 10,  MeanX.gauss = 3, SDX.gauss = 1, zeroLevelX.zero=0.50)
  Y6X4 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="NegativeBinomial", DistributionXCov = "AbsoluteGaussian", DistributionXFac = NULL,  ShapeY.gamma = 1, ScaleY.gamma = 10,  MeanX.gauss = 0, SDX.gauss = 1)
  Y6X5 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="NegativeBinomial", DistributionXCov = "StudentsT", DistributionXFac = NULL,  ShapeY.gamma = 1, ScaleY.gamma = 10,  DFX.student = 4)
  Y6X6 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="NegativeBinomial", DistributionXCov = "NegativeBinomial", DistributionXFac = NULL,  ShapeY.gamma = 1, ScaleY.gamma = 10,  ShapeX.gamma = 1, ScaleX.gamma = 10)
  Y6X7 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="NegativeBinomial", DistributionXCov = "Binomial", DistributionXFac = NULL,  ShapeY.gamma = 1, ScaleY.gamma = 10,  zeroLevelX.zero = 0.90)
  Y6X8 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="NegativeBinomial", DistributionXCov = "Gamma", DistributionXFac = NULL,  ShapeY.gamma = 1, ScaleY.gamma = 10,  ShapeX.gamma = 0.1, ScaleX.gamma = 100)
  
# 7. Y: indikátor eloszlású - p = 0.9 paraméterekkel
  
  Y7X1 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Binomial", DistributionXCov = "Gaussian", DistributionXFac = NULL,  zeroLevelY.zero = 0.90,  MeanX.gauss = 0, SDX.gauss = 1)
  Y7X2 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Binomial", DistributionXCov = "GaussianZeroCategorical", DistributionXFac = NULL,  zeroLevelY.zero = 0.90,  MeanX.gauss = 3, SDX.gauss = 1, nCategoriesX.cat=5)
  Y7X3 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Binomial", DistributionXCov = "GaussianZero", DistributionXFac = NULL,  zeroLevelY.zero = 0.90,  MeanX.gauss = 3, SDX.gauss = 1, zeroLevelX.zero=0.50)
  Y7X4 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Binomial", DistributionXCov = "AbsoluteGaussian", DistributionXFac = NULL,  zeroLevelY.zero = 0.90,  MeanX.gauss = 0, SDX.gauss = 1)
  Y7X5 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Binomial", DistributionXCov = "StudentsT", DistributionXFac = NULL,  zeroLevelY.zero = 0.90,  DFX.student = 4)
  Y7X6 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Binomial", DistributionXCov = "NegativeBinomial", DistributionXFac = NULL,  zeroLevelY.zero = 0.90,  ShapeX.gamma = 1, ScaleX.gamma = 10)
  Y7X7 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Binomial", DistributionXCov = "Binomial", DistributionXFac = NULL,  zeroLevelY.zero = 0.90,  zeroLevelX.zero = 0.90)
  Y7X8 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Binomial", DistributionXCov = "Gamma", DistributionXFac = NULL,  zeroLevelY.zero = 0.90,  ShapeX.gamma = 0.1, ScaleX.gamma = 100)
  
# 8. Y: gamma eloszlású - alpha = 0.1, lambda = 100 paraméterekkel
  
  Y8X1 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gamma", DistributionXCov = "Gaussian", DistributionXFac = NULL,  ShapeY.gamma = 0.1, ScaleY.gamma = 100,  MeanX.gauss = 0, SDX.gauss = 1)
  Y8X2 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gamma", DistributionXCov = "GaussianZeroCategorical", DistributionXFac = NULL,  ShapeY.gamma = 0.1, ScaleY.gamma = 100,  MeanX.gauss = 3, SDX.gauss = 1, nCategoriesX.cat=5)
  Y8X3 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gamma", DistributionXCov = "GaussianZero", DistributionXFac = NULL,  ShapeY.gamma = 0.1, ScaleY.gamma = 100,  MeanX.gauss = 3, SDX.gauss = 1, zeroLevelX.zero=0.50)
  Y8X4 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gamma", DistributionXCov = "AbsoluteGaussian", DistributionXFac = NULL,  ShapeY.gamma = 0.1, ScaleY.gamma = 100,  MeanX.gauss = 0, SDX.gauss = 1)
  Y8X5 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gamma", DistributionXCov = "StudentsT", DistributionXFac = NULL,  ShapeY.gamma = 0.1, ScaleY.gamma = 100,  DFX.student = 4)
  Y8X6 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gamma", DistributionXCov = "NegativeBinomial", DistributionXFac = NULL,  ShapeY.gamma = 0.1, ScaleY.gamma = 100,  ShapeX.gamma = 1, ScaleX.gamma = 10)
  Y8X7 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gamma", DistributionXCov = "Binomial", DistributionXFac = NULL,  ShapeY.gamma = 0.1, ScaleY.gamma = 100,  zeroLevelX.zero = 0.90)
  Y8X8 = TrustGauss(Family = fam, nSamples = sam, nSimulations = sim, SaveAllOutput = FALSE, DistributionY ="Gamma", DistributionXCov = "Gamma", DistributionXFac = NULL,  ShapeY.gamma = 0.1, ScaleY.gamma = 100,  ShapeX.gamma = 0.1, ScaleX.gamma = 100)
  
  
# A p értékek vizuális megjelenítése "hõtérképen":
  
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
  
  # 95 %-os konfidencia intervallum alsó és felsõ határa a 0.05-ös várható értékre:
  also1000 = round(0.05 - qnorm(0.975)*sd(p1000$p)/sqrt(64), digits = 3)
  felso1000 = round(0.05 + qnorm(0.975)*sd(p1000$p)/sqrt(64), digits = 3)
  p1000$m = c(rep(paste(" 95 %-os konf. int.: ", also1000, "-", felso1000, sep = ""),64))
  
P = rbind(p10, p100, p1000)
  
ggplot(P, aes(x, y, fill = p)) +
    geom_tile() +
    scale_fill_gradient(low = "turquoise1", high = "yellow", breaks = c(0.01,0.02,0.04,0.05,0.06,0.08,0.1)) +
    coord_fixed() +
    guides(fill = guide_colourbar(title = NULL, barheight = 11, breaks = 5)) +
    ggtitle("Elsofajú hibaarány 10, 100 és 1000 mintaelemszám esetén (alpha = 0.05)") +
    facet_grid(~factor(m, levels = c(paste("95 %-os konf. int.: ", also10, "-", felso10, sep = ""),paste("95 %-os konf. int.: ", also100, "-", felso100, sep = ""),paste(" 95 %-os konf. int.: ", also1000, "-", felso1000, sep = ""))))
  
# p értékek hisztogramjai Y = standard normális eloszlású függõ változó esetében:
    
par(mfrow = c(2,4), mai = c(0.4,0.4,0.4,0.1))
  
  hist(Y1X1$Pvals[,1], breaks = 20, main = "Normális", col = "darkseagreen1", freq = FALSE, xlab = NULL, ylab = NULL)
  abline(v = 0.05, col = "red")
  hist(Y1X2$Pvals[,1], breaks = 20, main = "Zero-inflated normális kategóriákkal", col = "darkseagreen1", freq = FALSE, xlab = NULL, ylab = NULL)
  abline(v = 0.05, col = "red")
  hist(Y1X3$Pvals[,1], breaks = 20, main = "Zero-inflated normális", col = "darkseagreen1", freq = FALSE, xlab = NULL, ylab = NULL)
  abline(v = 0.05, col = "red")
  hist(Y1X4$Pvals[,1], breaks = 20, main = "Abszolút normális", col = "darkseagreen1", freq = FALSE, xlab = NULL, ylab = NULL)
  abline(v = 0.05, col = "red")
  hist(Y1X5$Pvals[,1], breaks = 20, main = "Student-féle t", col = "darkseagreen1", freq = FALSE, xlab = NULL, ylab = NULL)
  abline(v = 0.05, col = "red")
  hist(Y1X6$Pvals[,1], breaks = 20, main = "Negatív binomiális", col = "darkseagreen1", freq = FALSE, xlab = NULL, ylab = NULL)
  abline(v = 0.05, col = "red")
  hist(Y1X7$Pvals[,1], breaks = 20, main = "Indikátor", col = "darkseagreen1", freq = FALSE, xlab = NULL, ylab = NULL)
  abline(v = 0.05, col = "red")
  hist(Y1X8$Pvals[,1], breaks = 20, main = "Gamma", col = "darkseagreen1", freq = FALSE, xlab = NULL, ylab = NULL)
  abline(v = 0.05, col = "red")

  
# A hibák eloszlásának megsértése Poisson eloszlás esetén
# A diszkrét változókat vizsgálva 100 mintaelemszám esetén
  
  YzinkG = TrustGauss(Family = "gaussian", nSamples = 1000, nSimulations = 5000, SaveAllOutput = FALSE, DistributionY ="GaussianZeroCategorical", DistributionXCov = "GaussianZeroCategorical", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, nCategoriesY.cat=5,  MeanX.gauss = 3, SDX.gauss = 1, nCategoriesX.cat=5)
  YzinkP = TrustGauss(Family = "poisson", nSamples = 1000, nSimulations = 5000, SaveAllOutput = FALSE, DistributionY ="GaussianZeroCategorical", DistributionXCov = "GaussianZeroCategorical", DistributionXFac = NULL,  MeanY.gauss = 3, SDY.gauss = 1, nCategoriesY.cat=5,  MeanX.gauss = 3, SDX.gauss = 1, nCategoriesX.cat=5)
  YnbG = TrustGauss(Family = "gaussian", nSamples = 1000, nSimulations = 5000, SaveAllOutput = FALSE, DistributionY ="NegativeBinomial", DistributionXCov = "NegativeBinomial", DistributionXFac = NULL,  ShapeY.gamma = 1, ScaleY.gamma = 10,  ShapeX.gamma = 1, ScaleX.gamma = 10)
  YnbP = TrustGauss(Family = "poisson", nSamples = 1000, nSimulations = 5000, SaveAllOutput = FALSE, DistributionY ="NegativeBinomial", DistributionXCov = "NegativeBinomial", DistributionXFac = NULL,  ShapeY.gamma = 1, ScaleY.gamma = 10,  ShapeX.gamma = 1, ScaleX.gamma = 10)
  YiG = TrustGauss(Family = "gaussian", nSamples = 1000, nSimulations = 5000, SaveAllOutput = FALSE, DistributionY ="Binomial", DistributionXCov = "Binomial", DistributionXFac = NULL,  zeroLevelY.zero = 0.90,  zeroLevelX.zero = 0.90)
  YiP = TrustGauss(Family = "poisson", nSamples = 1000, nSimulations = 5000, SaveAllOutput = FALSE, DistributionY ="Binomial", DistributionXCov = "Binomial", DistributionXFac = NULL,  zeroLevelY.zero = 0.90,  zeroLevelX.zero = 0.90)
  
p_ertekek = c(YzinkG$Pvals[,1], YzinkP$Pvals[,1], YnbG$Pvals[,1], YnbP$Pvals[,1], YiG$Pvals[,1], YiP$Pvals[,1])
elsofh = c(rep(YzinkG$Alphas.05,5000), rep(YzinkP$Alphas.05,5000), rep(YnbG$Alphas.05,5000), rep(YnbP$Alphas.05,5000), rep(YiG$Alphas.05,5000), rep(YiP$Alphas.05,5000))
hiba = c(rep("Normális",5000),rep("Poisson",5000),rep("Normális",5000),rep("Poisson",5000),rep("Normális",5000),rep("Poisson",5000))
eloszlas = c(rep("(2) Zero-inflated normális\nkategóriákkal",10000),rep("(6) Negatív binomiális",10000),rep("(7) Indikátor",10000))

ossz = data.frame(p_ertekek, elsofh, hiba, eloszlas)

ggplot(ossz, aes(x = p_ertekek, fill = factor(elsofh))) +
  geom_histogram(colour = "black", breaks = seq(0, 1, 0.05)) +
  geom_vline(xintercept = 0.05, col = "red", size = 0.8) +
  geom_text(ossz, mapping = aes(x = 0.5, y = rep(c(rep(290,5000),rep(2500,5000)),3), label = paste("Elsõf.hiba: ", round(elsofh, 4)))) +
  ylab(NULL) + xlab("p értékek") +
  facet_grid(vars(hiba), vars(eloszlas), scales = "free") +
  scale_fill_manual(values = c("turquoise1",rep("darkseagreen1",3),"#ccff33","yellow")) +
  theme(legend.position = "none")

# p értékek egyenletes eloszlás tesztelése

ks.test(ossz[ossz$hiba == 'Normális' & ossz$eloszlas == '(2) Zero-inflated normális\nkategóriákkal',]$p_ertekek, punif)
ks.test(ossz[ossz$hiba == 'Normális' & ossz$eloszlas == '(6) Negatív binomiális',]$p_ertekek, punif)
ks.test(ossz[ossz$hiba == 'Normális' & ossz$eloszlas == '(7) Indikátor',]$p_ertekek, punif)
ks.test(ossz[ossz$hiba == 'Poisson' & ossz$eloszlas == '(2) Zero-inflated normális\nkategóriákkal',]$p_ertekek, punif)
ks.test(ossz[ossz$hiba == 'Poisson' & ossz$eloszlas == '(6) Negatív binomiális',]$p_ertekek, punif)
ks.test(ossz[ossz$hiba == 'Poisson' & ossz$eloszlas == '(7) Indikátor',]$p_ertekek, punif)
