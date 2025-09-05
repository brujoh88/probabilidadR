# Limpiamos el programa
rm(list = ls())

# Cargamos la libreria necesaria para leer archivos Excel
if (!require(readxl)) install.packages("readxl")
library(readxl)

# Carga de archivo Excel
archivo <- file.choose()
datos <- read_excel(archivo)

# ----- VARIABLE CONTINUA: TIEMPO SEMANAL en HS. DEDIC. EST. -------
variable_continua <- "TIEMPO SEMANAL en HS. DEDIC. EST."

faltantes_continua <- sum(is.na(datos[[variable_continua]]))
if (faltantes_continua > 0) {
  message(paste("Advertencia: La variable continua '", variable_continua,
                "' contiene ", faltantes_continua, " valores faltantes.", sep = ""))
}

k <- ceiling(1 + 3.322 * log10(nrow(datos)))
min_val <- floor(min(datos[[variable_continua]], na.rm = TRUE))
max_val <- ceiling(max(datos[[variable_continua]], na.rm = TRUE))
amplitud <- ceiling((max_val - min_val) / k)
max_tope <- min_val + amplitud * k
cortes <- seq(min_val, max_tope, by = amplitud)

datos$clases <- cut(datos[[variable_continua]], breaks = cortes,
                    right = FALSE, include.lowest = TRUE)

marca_clase <- (head(cortes, -1) + tail(cortes, -1)) / 2

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

#Calculo de media
frecuencias <- as.vector(tabla_clases) # Frecuencias absolutas
media_continua <- sum(marca_clase * frecuencias) / sum(frecuencias) # Media ponderada

# Cálculo de moda
i_modal <- which.max(frecuencias) #Indice de la clase modal
L_m <- cortes[i_modal]  # Límite inferior de la clase modal
f_m <- frecuencias[i_modal] # Frecuencia del intervalo modal
f_1 <- ifelse(i_modal == 1, 0, frecuencias[i_modal - 1]) # Frecuencia del intervalo anterior al modal
f_2 <- ifelse(i_modal == length(frecuencias), 0, frecuencias[i_modal + 1]) # Frecuencia del intervalo posterior al modal
moda_continua <- L_m + ((f_m - f_1) / ((f_m - f_1) + (f_m - f_2))) * amplitud # Fórmula de la moda para datos agrupados

# Cálculo de mediana
n_total <- sum(frecuencias) # Total de observaciones
n_2 <- n_total / 2 # Mitad del total de observaciones
clase_mediana_index <- which(f_acum >= n_2)[1] # Índice de la clase que contiene la mediana
L <- cortes[clase_mediana_index]  # Límite inferior de la clase mediana
F_anterior <- ifelse(clase_mediana_index == 1, 0, f_acum[clase_mediana_index - 1]) # Frecuencia acumulada antes de la clase mediana
f_mediana <- frecuencias[clase_mediana_index] # Frecuencia de la clase mediana
mediana_continua <- L + ((n_2 - F_anterior) / f_mediana) * amplitud # Fórmula de la mediana para datos agrupados

# Medidas de dispersion
varianza_continua <- sum(frecuencias * (marca_clase - media_continua)^2) / (n_total - 1)  # Varianza
desvio_continua <- sqrt(varianza_continua) # Desvío estándar
coef_var_continua <- (desvio_continua / media_continua) * 100  # Coeficiente de variación en porcentaje

# Estadísticos descriptivos
continua_stats <- data.frame(
  Media = round(media_continua, 4),
  Moda = round(moda_continua, 4),
  Mediana = round(mediana_continua, 4),
  Varianza = round(varianza_continua, 4),
  Desvio_estandar = round(desvio_continua, 4),
  Coef_Variacion_pct = round(coef_var_continua, 4)
)

# Cálculo de cuartiles
Q1_pos <- n_total * 0.25
Q3_pos <- n_total * 0.75

# Q1
clase_Q1_index <- which(f_acum >= Q1_pos)[1] # Índice de la clase que contiene Q1
L_Q1 <- cortes[clase_Q1_index] # Límite inferior de la clase Q1
F_anterior_Q1 <- ifelse(clase_Q1_index == 1, 0, f_acum[clase_Q1_index - 1]) # Frecuencia acumulada antes de la clase Q1
f_Q1 <- frecuencias[clase_Q1_index] # Frecuencia de la clase Q1
Q1 <- L_Q1 + ((Q1_pos - F_anterior_Q1) / f_Q1) * amplitud # Fórmula de Q1

# Q3 (similar proceso)
clase_Q3_index <- which(f_acum >= Q3_pos)[1] # Índice de la clase que contiene Q3
L_Q3 <- cortes[clase_Q3_index] # Límite inferior de la clase Q3
F_anterior_Q3 <- ifelse(clase_Q3_index == 1, 0, f_acum[clase_Q3_index - 1]) # Frecuencia acumulada antes de la clase Q3
f_Q3 <- frecuencias[clase_Q3_index] # Frecuencia de la clase Q3
Q3 <- L_Q3 + ((Q3_pos - F_anterior_Q3) / f_Q3) * amplitud # Fórmula de Q3
IQR <- Q3 - Q1

# Cuartiles en un data frame
cuartiles <- data.frame(
  Q1 = round(Q1, 4),
  Q2 = round(mediana_continua, 4),
  Q3 = round(Q3, 4),
  IQR = round(IQR, 4)
)

# Variable categórica - SATISFACCIÓN CON LA CARRERA
variable_categorica <- "SATISFACCIÓN CON LA CARRERA"

faltantes_categorica <- sum(is.na(datos[[variable_categorica]]))
if (faltantes_categorica > 0) {
  message(paste("Advertencia: La variable categórica '", variable_categorica,
                "' contiene ", faltantes_categorica, " valores faltantes.", sep = ""))
}

# Convertir códigos numéricos a etiquetas descriptivas Y establecer orden correcto
datos[[variable_categorica]] <- factor(datos[[variable_categorica]], 
                                        levels = c("4", "3", "2", "1"),
                                        labels = c("Muy insatisfecho", "Insatisfecho", "Satisfecho", "Muy satisfecho"),
                                        ordered = TRUE)

# Crear tablas de frecuencias
tabla_satisfaccion <- table(datos[[variable_categorica]])
f_rel_satisfaccion <- prop.table(tabla_satisfaccion)
f_acum_satisfaccion <- cumsum(tabla_satisfaccion)
f_rel_acum_satisfaccion <- cumsum(f_rel_satisfaccion)

tabla_satisfaccion_df <- data.frame(
  Categoria = names(tabla_satisfaccion),
  Frec_Abs = as.vector(tabla_satisfaccion),
  Frec_Acum = as.vector(f_acum_satisfaccion),
  Frec_Rel = round(as.vector(f_rel_satisfaccion), 4),
  Frec_Rel_Acum = round(as.vector(f_rel_acum_satisfaccion), 4)
)

tabla_satisfaccion_df$Categoria <- factor(
  tabla_satisfaccion_df$Categoria,
  levels = c("Muy insatisfecho", "Insatisfecho", "Satisfecho", "Muy satisfecho"),
  ordered = TRUE
)

# Cálculos estadísticos
n_total_cat <- sum(tabla_satisfaccion)
moda_categorica <- names(tabla_satisfaccion)[which.max(tabla_satisfaccion)]

# Mediana
posicion_mediana <- n_total_cat / 2
clase_mediana_cat <- which(f_acum_satisfaccion >= posicion_mediana)[1]
mediana_categorica <- names(tabla_satisfaccion)[clase_mediana_cat]

# Q1
posicion_Q1 <- n_total_cat * 0.25
clase_Q1_cat <- which(f_acum_satisfaccion >= posicion_Q1)[1]
Q1_categorica <- names(tabla_satisfaccion)[clase_Q1_cat]

# Q3
posicion_Q3 <- n_total_cat * 0.75
clase_Q3_cat <- which(f_acum_satisfaccion >= posicion_Q3)[1]
Q3_categorica <- names(tabla_satisfaccion)[clase_Q3_cat]

# Estadísticos descriptivos
categorica_stats <- data.frame(
  Moda = moda_categorica,
  Mediana = mediana_categorica,
  Q1 = Q1_categorica,
  Q3 = Q3_categorica  
)

# Resultados
message("----- RESULTADOS -----")
message("Punto 3: Análisis de las variables")
message("\n Tabla de frecuencias - VARIABLE CONTINUA (", variable_continua, ")")
print(tabla_frecuencia, row.names = FALSE)
message("\n Estadísticos descriptivos (Continua):")
print(continua_stats, row.names = FALSE)
message("\n Cuartiles:")
print(cuartiles, row.names = FALSE)
message("\n Tabla de frecuencias - VARIABLE CATEGÓRICA (", variable_categorica, ")")
print(tabla_satisfaccion_df, row.names = FALSE)
message("\n Estadísticos descriptivos (Categórica):")
print(categorica_stats, row.names = FALSE)

# ----- PUNTO 4: REPRESENTACIONES GRÁFICAS -----
# Cargar librería ggplot2
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
# 4a. Histograma
tabla_frecuencia$Intervalo <- factor(tabla_frecuencia$Intervalo, levels = tabla_frecuencia$Intervalo, ordered = TRUE)

ggplot(tabla_frecuencia, aes(x = Intervalo, y = Frec_Abs)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = Frec_Abs), vjust = -0.5, size = 3) +
  labs(title = paste("Histograma de", variable_continua),
       x = variable_continua,
       y = "Frecuencia Absoluta") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Diagrama Circular
ggplot(tabla_satisfaccion_df, aes(x = "", y = Frec_Abs, fill = Categoria)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(Frec_Rel * 100, "%")), 
            position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(values = c(
    "Muy insatisfecho" = "#E74C3C",   # rojo
    "Insatisfecho" = "#F7CA18",       # amarillo
    "Satisfecho" = "#3498DB",         # azul
    "Muy satisfecho" = "#27AE60"      # verde
  )) +
  labs(title = paste("Diagrama Circular de", variable_categorica),
       fill = "Nivel de Satisfacción") +
  theme_void() +
  theme(legend.position = "right")