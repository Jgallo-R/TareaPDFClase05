rm(list = ls())
getwd()
setwd("C:/Users/JGALLO/Desktop/TareaPDF")
#### Ejemplo 01 Regresion Lineal Simple####

edad <- c(56,42,72,36,63,47,55,47,38,42)
presion <- c(148,126,159,118,149,130,151,142,114,141)


# Regresion lineal simple

r_lin <- lm(edad ~ presion) 

# Usamos summary para obtener informacion del modelo
summary(r_lin)

# Utilizamos plot para obtener la recta de regresion sobre el diagrama

plot(presion,edad)
abline(r_lin)

xmin <- 0.9 * min(presion)
xmax <- 1.1 * max(presion)
ymin <- 0.9 * min(edad)
ymax <- 1.1 * max(edad)

png(filename = "Pobl_10_mujeres.png")

plot(presion, edad, main="Edad ~ Presion Sanguinea",sub="Pobl_10_mujeres", xlab="Presion", ylab="Edad",
xlim=c(xmin,xmax),ylim=c(ymin,ymax))
abline(r_lin)
dev.off()  


plot(r_lin)

#### Apliacamos el Test de normalidad de Kolmogorov-Smirnov ####

ks.test(r_lin$residuals, "pnorm")

# Resultado

# One-sample Kolmogorov-Smirnov test
# 
# data:  r_lin$residuals
# D = 0.3935, p-value = 0.06608
# alternative hypothesis: two-sided

#### Aplicamos el Test de Durbin Watson ####

library(lmtest)
dwtest(edad~presion)

# Resultado
# Durbin-Watson test
# 
# data:  edad ~ presion
# DW = 1.9667, p-value = 0.5879
# alternative hypothesis: true autocorrelation is greater than 0

#### Ejemplo 02 de Regresion Lineal Multiple ####

gastos <- c(1000,580,520,500,600,550,400)
ingresos <- c(50000,2500,2000,1900,3000,4000,2000)
tamaño <- c(7,4,3,3,6,5,2)
hijosU <- c(3,1,1,0,1,2,0)

# Concentramos los datos en un data frame
datos <- data.frame(gastos,ingresos,tamaño,hijosU)

# Aplicamos el modelo a una Regresion lineal 
rlmul <- lm(gastos~ingresos + tamaño+hijosU)
summary(rlmul)

# Resultado
# Call:
#   lm(formula = gastos ~ ingresos + tamaño + hijosU)
# 
# Residuals:
#   1       2       3       4       5       6       7 
# 1.216  48.164  29.125  15.209 -10.134 -35.402 -48.178 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept) 3.590e+02  6.291e+01   5.706   0.0107 *
#   ingresos    7.247e-03  1.802e-03   4.021   0.0276 *
#   tamaño      3.734e+01  2.046e+01   1.825   0.1655  
# hijosU      5.359e+00  4.061e+01   0.132   0.9034  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 48.57 on 3 degrees of freedom
# Multiple R-squared:  0.9677,	Adjusted R-squared:  0.9353 
# F-statistic: 29.93 on 3 and 3 DF,  p-value: 0.009772

#### Utilizando PRESTIGE ####
library(carData)
library(car)
head(Prestige,5)
newdata=Prestige[,c(1:2)]
summary(newdata)
modelo=lm(income~education, data=newdata)
plot(newdata$education,newdata$income, main="Educacion ~ Income")
abline(modelo)
summary(modelo)

# Respuesta
# Call:
#   lm(formula = income ~ education, data = newdata)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5493.2 -2433.8   -41.9  1491.5 17713.1 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -2853.6     1407.0  -2.028   0.0452 *  
#   education      898.8      127.0   7.075 2.08e-10 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3483 on 100 degrees of freedom
# Multiple R-squared:  0.3336,	Adjusted R-squared:  0.3269 
# F-statistic: 50.06 on 1 and 100 DF,  p-value: 2.079e-10

# income=898.8(education)-2853.6

#### Regresion Logistica #### 

data <- read.csv("HumanResourcesAnalytics.csv",T)
muestra <- dim(data)[1]
data <- datos[sample(muestra,100,replace=TRUE),]
class(data)
str(data)
head(data)
View(data)
colnames(data)=c("nivel_satisfaccion","ultima_evaluacion","numero_proyectos","promedio_horas_mensuales","antiguedad"
                 ,"accidente","abandona","promocionado","departamento","salario")


data.modelo <- subset(data,select=c(abandona,nivel_satisfaccion,ultima_evaluacion))
data.modelo$abandona <- factor(data.modelo$abandona)
head(data.modelo)
plot(data.modelo$nivel_satisfaccion,data.modelo$abandona)

table(data.modelo$abandona)
summary(data.modelo$nivel_satisfaccion)
summary(data.modelo$ultima_evaluacion)

#### Utilizando la funcion GLM ####

modelo.logit <- glm(abandona ~ultima_evaluacion + nivel_satisfaccion, data=data.modelo, family="binomial")
summary(modelo.logit)

# Respuesta
# Call:
#   glm(formula = abandona ~ ultima_evaluacion + nivel_satisfaccion, 
#       family = "binomial", data = data.modelo)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.4619  -0.7050  -0.5015  -0.3359   2.2949  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         0.62697    0.09567   6.554 5.61e-11 ***
#   ultima_evaluacion   0.50871    0.12034   4.227 2.37e-05 ***
#   nivel_satisfaccion -3.85391    0.08752 -44.034  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 16465  on 14998  degrees of freedom
# Residual deviance: 14180  on 14996  degrees of freedom
# AIC: 14186
# 
# Number of Fisher Scoring iterations: 4

# Modelo Regresion Logistica

exp(coefficients(modelo.logit))

log.odds <- predict(modelo.logit, data.frame(nivel_satisfaccion=0.6,
                                             ultima_evaluacion=0.75))
log.odds
exp(log.odds)/(1-exp(log.odds))

q <- seq(from=0, to=20,by=0.1)
y <- 500+0.4*(q-10)*3
noise <- rnorm(length(q), mean=10, sd=80)
noisy.y <- y+noise
plot(q,noisy.y, col='deepskyblue4',xlab='q' , main='Observeddata')
lines(q,y,col='firebrick1',lwd=3)
model <- lm(noisy.y ~ poly(q,3))

confint(model, level=0.95)

#### Regresion de Poisson ####
NumResueltos <- c(0,1,2,1,5,3,2,5,7,8,12,13,12,11,10,12,10,15)
HorasClase <- c(1,3,4,1,3,5,1,3,5,2,3,5,0,3,5,4,3,5)

Nota <- c(0,2,3,0,4,3,1,3,4,3,4,5,4,5,4,5,5,5)
tabla <- data.frame(NumResueltos, HorasClase, Nota)

regPoisson <- glm(Nota~NumResueltos+HorasClase, data=tabla,family=poisson())
summary(regPoisson)
predict(regPoisson, type="response")








