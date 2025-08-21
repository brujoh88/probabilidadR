# Cargamos la libreria necesaria para leer archivos Excel
if (!require(readxl)) install.packages("readxl")
library(readxl)

# Carga de archivo Excel
archivo <- file.choose()
datos <- read_excel(archivo)

# Vemos cuantas veces aparece cada plataforma
tabla_plataforma <- table(datos$Plataforma_Trabajo)
tabla_plataforma

# Calculamos la frecuencia relativa
f_rel_plataforma <- prop.table(tabla_plataforma)
f_rel_plataforma

# Construimos la tabla de frecuencia con los datos anteriores
tabla_frecuencia_plataforma <- data.frame(
  Plataforma = names(tabla_plataforma),
  frec = as.vector(tabla_plataforma),
  frec_rel = round(as.vector(f_rel_plataforma), 3)
)

#Imprimimos la tabla_frecuencia_plataforma
print(tabla_frecuencia_plataforma, row.names = FALSE)


# TICKETS
tabla_tickets <- table(datos$Tickets_Soporte)
tabla_tickets
f_acum_tickets <- cumsum(tabla_tickets)
f_rel_tickets <- prop.table(tabla_tickets)
f_rel_acum_tickets <- cumsum(f_rel_tickets)

tabla_frecuencia_tickets <- data.frame(
  Tickets = names(tabla_tickets),
  frec = as.vector(tabla_tickets),
  frec_acum = as.vector(f_acum_tickets),
  frec_rel = round(as.vector(f_rel_tickets), 3),
  frec_rel_acum = round(as.vector(f_rel_acum_tickets), 3)
)

print(tabla_frecuencia_tickets, row.names = FALSE)

# Tiempo de conexion minima
# Cantidad total de observaciones
n <- length(datos$Tiempo_Conexion_Min)
n
# Número de clases segün la regla de Sturges
k <- ceiling(1+3.322 * log10(n))
k
rango <- range(datos$Tiempo_Conexion_Min)
amplitud <- ceiling((rango[2]-rango[1])/k)
rango
amplitud

breaks <- seq(floor(rango[1]),ceiling(rango[2]) + amplitud, by = amplitud)
clases <- cut( datos$Tiempo_Conexion_Min, breaks = breaks, right = FALSE)
head(clases)

tabla_tiempos <- table(clases)
f_acum <- cumsum(tabla_tiempos)
f_rel <- prop.table(tabla_tiempos)
f_rel_acum <- cumsum(f_rel)

tabla_frecuencia_tiempos <- data.frame(
  Intervalo = levels(clases),
  Frecuencia = as.vector(tabla_tiempos),
  Frec_Acumulada = as.vector(f_acum),
  Frec_Relativa = round(as.vector(f_rel), 3),
  Frec_Rel_Acum = round(as.vector(f_rel_acum), 3)
)



tabla_frecuencia_tiempos