# Fijamos una semilla para obtener siempre los mismos resultados
set.seed(123)

# Generamos los datos: 47 tiempos simulados en minutos
tiempos <- round(rnorm(47, mean = 55, sd = 15), 1)

# Reemplazamos valores negativos por cero
tiempos <- ifelse(tiempos < 0, 0, tiempos)

# Mostramos los datos simulados
tiempos

# Cantidad total de observaciones
n <- length(tiempos)
n
# Número de clases segün la regla de Sturges
k <- ceiling(1+3.322 * log10(n))
k
rango <- range(tiempos)
amplitud <- ceiling((rango[2]-rango[1])/k)
rango
amplitud

breaks <- seq(floor(rango[1]),ceiling(rango[2]) + amplitud, by = amplitud)
clases <- cut( tiempos, breaks = breaks, right = FALSE)
head(clases)

tabla_tiempos <- table(clases)
f_acum <- cumsum(tabla_tiempos)
f_rel <- prop.table(tabla_tiempos)
f_rel_acum <- cumsum(f_rel)

tabla_final <- data.frame(
  Intervalo = levels(clases),
  Frecuencia = as.vector(tabla_tiempos),
  Frec_Acumulada = as.vector(f_acum),
  Frec_Relativa = round(as.vector(f_rel), 3),
  Frec_Rel_Acum = round(as.vector(f_rel_acum), 3)
)



tabla_final

