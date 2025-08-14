# Cargamos la libreria necesaria para leer archivos Excel
if (!require(readxl)) install.packages("readxl")
library(readxl)

# Carga de archivo Excel
archivo <- file.choose()
datos <- read_excel(archivo)

# Tabla de frecuencia para variables categórica
# Contamos cuántas veces aparece cada lenguaje favorito

tabla_lenguajes <- table(datos$Lenguaje_Favorito)

# Mostramos la tabla de frecuencia
tabla_lenguajes

# Tabla de frecuencia para variables discreta: Proyectos_Completados
tabla_proyectos_completados <- table(datos$Proyectos_Completados)
f_acum <- cumsum(tabla_proyectos_completados)
f_rel <- prop.table(tabla_proyectos_completados)
f_rel_acum <- cumsum(f_rel)

tabla_frecuencia <- data.frame(
  Proyectos = names(tabla_proyectos_completados),
  frec = as.vector(tabla_proyectos_completados),
  frec_acum = as.vector(f_acum),
  frec_rel = round(as.vector(f_rel), 3),
  frec_rel_acum = round(as.vector(f_rel_acum), 3)
)

tabla_frecuencia

print(tabla_frecuencia, row.names = FALSE)
