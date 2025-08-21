# TRABAJO PRÁCTICO INTEGRADOR - PUNTO 2
# Análisis de datos de encuesta a estudiantes

# Cargamos la libreria necesaria para leer archivos Excel
if (!require(readxl)) install.packages("readxl")
library(readxl)

# Carga de archivo Excel
archivo <- file.choose()
datos <- read_excel(archivo)

# Verificar nombres de columnas
cat("Nombres de las columnas:\n")
print(colnames(datos))

# 2a. TABLA DE FRECUENCIAS - TIEMPO SEMANAL EN HS. DEDIC. EST.
cat("\n=== 2a. TABLA DE FRECUENCIAS - TIEMPO SEMANAL EN HS. DEDIC. EST. ===\n\n")

# Cantidad total de observaciones
n <- length(datos$`TIEMPO SEMANAL en HS. DEDIC. EST.`)
cat("Cantidad total de observaciones:", n, "\n")

# Número de clases según la regla de Sturges
k <- ceiling(1 + 3.322 * log10(n))
cat("Número de intervalos (Regla de Sturges):", k, "\n")

# Calcular rango y amplitud
rango <- range(datos$`TIEMPO SEMANAL en HS. DEDIC. EST.`)
amplitud <- ceiling((rango[2] - rango[1]) / k)
cat("Rango:", rango[1], "-", rango[2], "\n")
cat("Amplitud de intervalo:", amplitud, "\n")

# Crear intervalos
breaks <- seq(floor(rango[1]), ceiling(rango[2]) + amplitud, by = amplitud)
clases <- cut(datos$`TIEMPO SEMANAL en HS. DEDIC. EST.`, breaks = breaks, right = FALSE)

# Crear tabla de frecuencias
tabla_tiempos <- table(clases)
f_acum <- cumsum(tabla_tiempos)
f_rel <- prop.table(tabla_tiempos)
f_rel_acum <- cumsum(f_rel)

tabla_frecuencia_tiempos_estudio <- data.frame(
  Intervalo = levels(clases),
  Frecuencia = as.vector(tabla_tiempos),
  Frec_Acumulada = as.vector(f_acum),
  Frec_Relativa = round(as.vector(f_rel), 4),
  Frec_Rel_Acum = round(as.vector(f_rel_acum), 4)
)

print(tabla_frecuencia_tiempos_estudio, row.names = FALSE)

# 2b. TABLA DE FRECUENCIAS - SATISFACCIÓN CON LA CARRERA
cat("\n=== 2b. TABLA DE FRECUENCIAS - SATISFACCIÓN CON LA CARRERA ===\n\n")

# Convertir códigos numéricos a etiquetas descriptivas
satisfaccion_labels <- c("1" = "Muy satisfecho", 
                         "2" = "Satisfecho", 
                         "3" = "Insatisfecho", 
                         "4" = "Muy insatisfecho")

datos$satisfaccion_categoria <- factor(datos$`SATISFACCIÓN CON LA CARRERA`,
                                       levels = c(1, 2, 3, 4),
                                       labels = satisfaccion_labels)

tabla_satisfaccion <- table(datos$satisfaccion_categoria)
f_acum_satisfaccion <- cumsum(tabla_satisfaccion)
f_rel_satisfaccion <- prop.table(tabla_satisfaccion)
f_rel_acum_satisfaccion <- cumsum(f_rel_satisfaccion)

tabla_frecuencia_satisfaccion <- data.frame(
  Satisfaccion = names(tabla_satisfaccion),
  frec = as.vector(tabla_satisfaccion),
  frec_acum = as.vector(f_acum_satisfaccion),
  frec_rel = round(as.vector(f_rel_satisfaccion), 4),
  frec_rel_acum = round(as.vector(f_rel_acum_satisfaccion), 4)
)

print(tabla_frecuencia_satisfaccion, row.names = FALSE)