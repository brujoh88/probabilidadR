# Limpiamos el programa
rm(list = ls())

# Cargamos la libreria necesaria para leer archivos Excel
if (!require(readxl)) install.packages("readxl")
library(readxl)

# Carga de archivo Excel
archivo <- file.choose()
datos <- read_excel(archivo)

# ----- VARIABLE DISCRETA: Tickets_Soporte -------
variable_discreta <- "Tickets_Soporte"

print(summary(datos[[variable_discreta]]))

media_discreta <- mean(datos[[variable_discreta]], na.rm = TRUE)

mediana_discreta <- median(datos[[variable_discreta]], na.rm = TRUE)

if (!require(modeest)) install.packages("modeest")
library(modeest)
moda_discreta <- mlv(datos[[variable_discreta]], method = "mfv")

varianza_discreta <- var(datos[[variable_discreta]], na.rm = TRUE)

desvio_estandar_discreta <- sd(datos[[variable_discreta]], na.rm = TRUE)

coef_var_discreta <- (desvio_estandar_discreta/media_discreta)*100  # Se divide por la media, no por la mediana

message("\nEstadisticos descriptivos (Discreta):")
discreta_stats <- data.frame(
  Media = round(media_discreta, 4),
  Mediana = round(mediana_discreta, 4),
  Moda = moda_discreta,
  Varianza = round(varianza_discreta, 4),
  Desvio_Estandar = round(desvio_estandar_discreta, 4),  # Corregido: era varianza en lugar de desvio
  Coef_Variacion_pct = round(coef_var_discreta, 4)
)
print(discreta_stats, row.names = FALSE)

# Cuartiles y RIC
cuartiles <- quantile(datos[[variable_discreta]], probs = c(0.25,0.5,0.75), na.rm = TRUE)
rango_intercuartil <- IQR(datos[[variable_discreta]], na.rm = TRUE)
cuartiles
cat("RIC:", rango_intercuartil,"\n")

# ----- VARIABLE CONTINUA: Tiempo_Conexion_Min -------
variable_continua <- "Tiempo_Conexion_Min"
k <- ceiling(1 + 3.322 * log10(nrow(datos)))
min_val <- floor(min(datos[[variable_continua]], na.rm = TRUE))
max_val <- ceiling(max(datos[[variable_continua]], na.rm = TRUE))
amplitud <- ceiling((max_val - min_val)/k)
max_tope <- min_val + amplitud * k
cortes <- seq(min_val, max_tope, by = amplitud)

datos$clases <- cut(datos[[variable_continua]], breaks = cortes,
                    right = FALSE, include.lowest = TRUE)

# CORRECCIÓN IMPORTANTE: Cálculo correcto de marca de clase
marca_clase <- (cortes[-length(cortes)] + cortes[-1]) / 2  # Punto medio entre límites

tabla_clases <- table(datos$clases)
f_acum <- cumsum(tabla_clases)
f_rel <- prop.table(tabla_clases)
f_rel_acum <- cumsum(f_rel)

tabla_frecuencia <- data.frame(
  Intervalo = names(tabla_clases),
  Marca = as.vector(marca_clase),
  Frec_Abs = as.vector(tabla_clases),
  Frec_Acum = as.vector(f_acum),
  Frec_Rel = round(as.vector(f_rel), 4),
  Frec_Rel_Acum = round(as.vector(f_rel_acum), 4)
)

frecuencias <- as.vector(tabla_clases)
media_continua <- sum(marca_clase * frecuencias) / sum(frecuencias)

# Cálculo de moda corregido
i_modal <- which.max(frecuencias)
L_m <- cortes[i_modal]  # Límite inferior de la clase modal
f_m <- frecuencias[i_modal]
f_1 <- ifelse(i_modal == 1, 0, frecuencias[i_modal - 1])
f_2 <- ifelse(i_modal == length(frecuencias), 0, frecuencias[i_modal + 1])  # Corregido: i_modal + 1

moda_continua <- L_m + ((f_m - f_1) / ((f_m - f_1) + (f_m - f_2))) * amplitud

# Cálculo de mediana corregido
n_total <- sum(frecuencias)
n_2 <- n_total / 2
clase_mediana_index <- which(f_acum >= n_2)[1]
L <- cortes[clase_mediana_index]  # Límite inferior de la clase mediana
F_anterior <- ifelse(clase_mediana_index == 1, 0, f_acum[clase_mediana_index - 1])
f_mediana <- frecuencias[clase_mediana_index]
mediana_continua <- L + ((n_2 - F_anterior) / f_mediana) * amplitud

# Cálculo de varianza y desvío corregidos (usando marca de clase)
varianza_continua <- sum(frecuencias * (marca_clase - media_continua)^2) / (n_total - 1)  # Usar media_continua
desvio_continua <- sqrt(varianza_continua)
coef_var_continua <- (desvio_continua / media_continua) * 100  # Usar media, no mediana

continua_stats <- data.frame(
  Media = round(media_continua, 4),
  Mediana = round(mediana_continua, 4),
  Moda = round(moda_continua, 4),
  Varianza = round(varianza_continua, 4),
  Desvio_estandar = round(desvio_continua, 4),
  Coef_Variacion_pct = round(coef_var_continua, 4)
)

message("\nTabla de Frecuencias (Continua):")
print(tabla_frecuencia, row.names = FALSE)

message("\nEstadisticos descriptivos (Continua):")
print(continua_stats, row.names = FALSE)