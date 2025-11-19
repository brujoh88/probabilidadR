# TRABAJO PRÁCTICO INTEGRADOR - PUNTO 5,6 y 7
#Limpiamos el entorno
rm(list = ls())
# Cargamos la libreria necesaria para leer archivos Excel
if (!require(readxl)) install.packages("readxl")
library(readxl)
# Carga de archivo Excel
archivo <- file.choose()
datos <- read_excel(archivo)

prob_5a <- 1 - pbinom(9, size = 16, prob = 0.5654)
print(paste("5a. Probabilidad de más de 9:", round(prob_5a, 4)))

# 5b. Entre 4 y 8 estudiantes satisfechos (P(4 <= X <= 8))
# n = 16, p = 0.3084
# Se calcula como P(X <= 8) - P(X <= 3)
prob_5b <- pbinom(8, size = 16, prob = 0.3084) - pbinom(3, size = 16, prob = 0.3084)

print(paste("5b. Probabilidad entre 4 y 8:", round(prob_5b, 4)))

# 5c. Menos de 5 estudiantes insatisfechos (P(X < 5) => P(X <= 4))
# n = 16, p = 0.0654
prob_5c <- pbinom(4, size = 16, prob = 0.0654)

print(paste("5c. Probabilidad de menos de 5:", round(prob_5c, 4)))


# 5d. Exactamente 10 estudiantes muy insatisfechos (P(X = 10))
# n = 16, p = 0.0607
# Usamos dbinom para una probabilidad exacta
prob_5d <- dbinom(10, size = 16, prob = 0.0607)

print(paste("5d. Probabilidad de exactamente 10:", prob_5d))


#6 a
resul6a <- ppois(q = 5, lambda = 10, lower.tail = FALSE)
print(paste("La probabilidad del punto 6a es =", resul6a))
#6 b
resul6b <- ppois(q = 12, lambda = 20, lower.tail = TRUE)
print(paste("La probabilidad del punto 6b es =", resul6b))
# 6c: P(7 < X < 10) = P(X=8) + P(X=9)
prob_8 <- dpois(8,15)
print(paste("P(X=8)", prob_8))
prob_9 <- dpois(9,15)
print(paste("P(X=9)", prob_9))
resul6c <- prob_8 + prob_9
print(paste("La probabilidad del punto 6c es =", resul6c))

# 7
media7 <- mean(datos$`ESTATURA CM.`)
# Calcula la desviación estándar de la estatura
desvio7 <- sd(datos$`ESTATURA CM.`)

#7a
resul7a<-pnorm(179,media7,desvio7, lower.tail = FALSE)
print(paste("La probabilidad del punto 7a es =", resul7a))

#7b
# Calcula P(X <= 172)
prob_hasta_172 <- pnorm(172, mean = media7, sd = desvio7)
# Calcula P(X <= 147)
prob_hasta_147 <- pnorm(147, mean = media7, sd = desvio7)
# La probabilidad del intervalo es la diferencia: P(147 <= X <= 172)
probabilidad_intervalo <- prob_hasta_172 - prob_hasta_147

print(paste("La probabilidad del punto 7b es =",probabilidad_intervalo))

#7c
# Hallar el valor (X) que deja el 97.5% de los datos por debajo
valor_estatura <- qnorm(0.975, mean = media7, sd = desvio7)
print(paste("La probabilidad del punto 7c es =",valor_estatura))

