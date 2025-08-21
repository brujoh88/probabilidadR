# Cargamos la libreria necesaria para leer archivos Excel
if (!require(readxl)) install.packages("readxl")
library(readxl)

# Carga de archivo Excel
archivo <- file.choose()
datos <- read_excel(archivo)

tabla_experiencia <- table(datos$Nivel_Experiencia)
f_acum_experiencia <- cumsum(tabla_experiencia)
f_rel_experiencia <- prop.table(tabla_experiencia)
f_rel_acum_experiencia <- cumsum(f_rel_experiencia)

tabla_frecuencia_experiencia<- data.frame(
  Experiencia = names(tabla_experiencia),
  frec = as.vector(tabla_experiencia),
  frec_acum = as.vector(f_acum_experiencia),
  frec_rel = round(as.vector(f_rel_experiencia), 3),
  frec_rel_acum = round(as.vector(f_rel_acum_experiencia), 3)
)
print(tabla_frecuencia_experiencia, row.names = FALSE)



# Tabla tickets soporte
tabla_Tickets_Soporte <- table(datos$Tickets_Soporte)
f_acum_Tickets_Soporte <- cumsum(tabla_Tickets_Soporte)
f_rel_Tickets_Soporte <- prop.table(tabla_Tickets_Soporte)
f_rel_acum_Tickets_Soporte <- cumsum(f_rel_Tickets_Soporte)
tabla_Tickets<- data.frame(
  Tickets = names(tabla_Tickets_Soporte),
  frec = as.vector(tabla_Tickets_Soporte),
  frec_acum = as.vector(f_acum_Tickets_Soporte),
  frec_rel = round(as.vector(f_rel_Tickets_Soporte), 3),
  frec_rel_acum = round(as.vector(f_rel_acum_Tickets_Soporte), 3)
)
print(tabla_Tickets, row.names = FALSE)


# Tabla tiempo conexion
# Cantidad total de observaciones
n <- length(datos$Tiempo_Conexion)
n
# Número de clases segün la regla de Sturges
k <- ceiling(1+3.322 * log10(n))
k
rango <- range(datos$Tiempo_Conexion)
amplitud <- ceiling((rango[2]-rango[1])/k)
rango
amplitud

breaks <- seq(floor(rango[1]),ceiling(rango[2]) + amplitud, by = amplitud)
clases <- cut( datos$Tiempo_Conexion, breaks = breaks, right = FALSE)
head(clases)

tabla_tiempos <- table(clases)
f_acum <- cumsum(tabla_tiempos)
f_rel <- prop.table(tabla_tiempos)
f_rel_acum <- cumsum(f_rel)
tabla_frecuencia_tiempos_conexion <- data.frame(
  Intervalo = levels(clases),
  Frecuencia = as.vector(tabla_tiempos),
  Frec_Acumulada = as.vector(f_acum),
  Frec_Relativa = round(as.vector(f_rel), 3),
  Frec_Rel_Acum = round(as.vector(f_rel_acum), 3)
)
print(tabla_frecuencia_tiempos_conexion, row.names = FALSE)
