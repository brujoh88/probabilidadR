x <- 10.2 + 3i
class(x)
typeof(x)
y <- c(10.2 + 3i,"1.55 + 7i", TRUE)
y[1]
class(y[1])
mi_lista <- list(comp = 10.2 + 3i, string2 = "Casa", bool = TRUE)
mi_lista$comp
matriz <- matrix(c(y,y), nrow = 2, ncol = 3)
dataFrame <- data.frame(
  nombre = c("Gustavo","Enrique","David"),
  edad = c(37,54,23),
  isActiv = c(TRUE, FALSE, TRUE)
)
dataFrame$nombre
dataFrame[2,"edad"]
catg <- factor(c("F","F","M","M","M","M","F","X"))
levels(catg)
