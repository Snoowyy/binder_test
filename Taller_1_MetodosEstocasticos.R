#####TALLER 1 MÉTODOS ESTOCÁSTICOS#####
#####PRESENTADO POR GERALDINE VEGA MONTENEGRO#####
install.packages('moments')
install.packages('ggplot2')
library('moments')
library(ggplot2)
#Lectura de datos
setwd('H:/Mi unidad/UNAL/METEOROLOGÍA/MÉTODOS ESTOCÁSTICOS/TALLER 1')
datos<-read.table('SMwr_Ex_01.txt', header=TRUE)
print(datos)
#####PUNTO 1#####
# Crear el histograma
#Ancho de clase=1
ggplot(datos, aes(x = datos$A)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histograma de la columna A", x = "Valores de la columna A", y = "Frecuencia")


#Ancho de clase=2
ggplot(datos, aes(x = datos$A)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  labs(title = "Histograma de la columna A", x = "Valores de la columna A", y = "Frecuencia")

#Ancho de clase=5
ggplot(datos, aes(x = datos$A)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Histograma de la columna A", x = "Valores de la columna A", y = "Frecuencia")

#Conteo de datos entre 5 y 10 para la columna A
datos_5_10_A <- subset(datos, A >= 5 & A <= 10)
cantidad_datos_5_10_A <- nrow(datos_5_10_A)
print(paste('La cantidad de datos entre 5 y 10 para la columna A es', cantidad_datos_5_10_A))

#####PUNTO 2#####
#Ancho de clase=1
ggplot(datos, aes(x = datos$B)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histograma de la columna B", x = "Valores de la columna A", y = "Frecuencia")


#Ancho de clase=2
ggplot(datos, aes(x = datos$B)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  labs(title = "Histograma de la columna B", x = "Valores de la columna A", y = "Frecuencia")

#Ancho de clase=5
ggplot(datos, aes(x = datos$B)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Histograma de la columna B", x = "Valores de la columna A", y = "Frecuencia")

#Conteo de datos entre 10 y 15 para la columna B
datos_10_15_B <- subset(datos, B >= 10 & B <= 15)
cantidad_datos_10_15_B <- nrow(datos_10_15_B)
print(paste('La cantidad de datos entre 5 y 10 para la columna B es', cantidad_datos_10_15_B))
#PUNTO 3
#Columna A
# Ordenar los datos según la columna A
datos_ordenadosA <- datos[order(datos$A), ]

# Calcular las frecuencias acumuladas
frecuencias_A <- table(datos_ordenadosA$A)
frecuencias_acumuladas_A <- cumsum(frecuencias_A)

# Crear un DataFrame con los valores únicos de A, las frecuencias y las frecuencias acumuladas
datos_frecuencia_acumulada_A <- data.frame(
  A = unique(datos_ordenadosA$A),
  Frecuencia = frecuencias_A,
  Frecuencia_Acumulada = frecuencias_acumuladas_A
)

# Crear la ojiva y el diagrama de barras acumuladas
ggplot() +
  geom_bar(data = datos_frecuencia_acumulada_A, aes(x = A, y = Frecuencia_Acumulada), stat = "identity", fill = "skyblue", color = "black") +
  geom_line(data = datos_frecuencia_acumulada_A, aes(x = A, y = Frecuencia_Acumulada), color = "red") +
  labs(title = "Diagrama de barras acumuladas y ojiva de la columna A", x = "Valores de la columna A", y = "Frecuencia acumulada") +
  scale_y_continuous(sec.axis = sec_axis(~., breaks = NULL, name = "Frecuencia acumulada"))

#Columna B
# Ordenar los datos según la columna B
datos_ordenadosB <- datos[order(datos$B), ]

# Calcular las frecuencias acumuladas
frecuencias_B <- table(datos_ordenadosB$B)
frecuencias_acumuladas_B <- cumsum(frecuencias_B)

# Crear un DataFrame con los valores únicos de B, las frecuencias y las frecuencias acumuladas
datos_frecuencia_acumulada_B <- data.frame(
  B = unique(datos_ordenadosB$B),
  Frecuencia = frecuencias_B,
  Frecuencia_Acumulada = frecuencias_acumuladas_B
)

# Crear la ojiva y el diagrama de barras acumuladas
ggplot() +
  geom_bar(data = datos_frecuencia_acumulada_B, aes(x = B, y = Frecuencia_Acumulada), stat = "identity", fill = "skyblue", color = "black") +
  geom_line(data = datos_frecuencia_acumulada_B, aes(x = B, y = Frecuencia_Acumulada), color = "red") +
  labs(title = "Diagrama de barras acumuladas y ojiva de la columna B", x = "Valores de la columna B", y = "Frecuencia acumulada") +
  scale_y_continuous(sec.axis = sec_axis(~., breaks = NULL, name = "Frecuencia acumulada"))


#####PUNTO 4#####
# Datos A
summary(datos$A)
mean(datos$A)
median(datos$A)
var(datos$A)
skewness(datos$A)
quantile(datos$A)
kurtosis(datos$A)
cuartiles_A <- function(x) {
  Q1_A <- quantile(x, 0.25)
  median <- median(x)
  Q3_A <- quantile(x, 0.75)
  return(data.frame(y = c(Q1_A, median, Q3_A)))
}
IQR_datos_A <- Q3_A - Q1_A
print(IQR_datos_A)
# Datos B
summary(datos$B)
mean(datos$B)
median(datos$B)
var(datos$B)
skewness(datos$B)
quantile(datos$B)
kurtosis(datos$B)
Q1_B <- quantile(datos$B, 0.25)
Q3_B <- quantile(datos$B, 0.75)
IQR_datos_B <- Q3_B - Q1_B
print(IQR_datos_B)

#####PUNTO 5#####
#Datos A
ggplot(datos, aes(x = "", y = A)) +
  geom_boxplot(outlier.shape = NA, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_point(position = position_jitter(width = 0.1), color = "red", size = 2) +
  geom_jitter(position = position_jitter(width = 0.1), color = "blue", size = 1, alpha = 0.5) +
  geom_text(data = data.frame(y = quantile(datos$A, c(0.25, 0.5, 0.75))), aes(x = 0.8, y = y, label = round(y, 2)), color = "black", size = 3) +
  geom_text(data = data.frame(y = max(datos$A) + 0.5), aes(x = 1.2, y = y, label = paste("Q3 + 1.5 * IQR =", round(quantile(datos$A, 0.75) + 1.5 * IQR(datos$A), 2))), color = "black", size = 3) +
  geom_text(data = data.frame(y = min(datos$A) - 0.5), aes(x = 1.2, y = y, label = paste("Q1 - 1.5 * IQR =", round(quantile(datos$A, 0.25) - 1.5 * IQR(datos$A), 2))), color = "black", size = 3) +
  labs(title = "Diagrama de Caja y Bigotes de la columna A", y = "Valores de la columna A", x = "")

#Datos atípicos A
limite_superior_A <- Q3_A + 1.5 * IQR_datos_A
limite_inferior_A <- Q1_A - 1.5 * IQR_datos_A
valores_atipicos_A <- datos$A[datos$A > limite_superior_A | datos$A < limite_inferior_A]
print(valores_atipicos_A) #En el conjunto A no hay valores atípicos

#Datos B
ggplot(datos, aes(x = "", y = B)) +
  geom_boxplot(outlier.shape = NA, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_point(position = position_jitter(width = 0.1), color = "red", size = 2) +
  geom_jitter(position = position_jitter(width = 0.1), color = "blue", size = 1, alpha = 0.5) +
  geom_text(data = data.frame(y = quantile(datos$A, c(0.25, 0.5, 0.75))), aes(x = 0.8, y = y, label = round(y, 2)), color = "black", size = 3) +
  geom_text(data = data.frame(y = max(datos$A) + 0.5), aes(x = 1.2, y = y, label = paste("Q3 + 1.5 * IQR =", round(quantile(datos$A, 0.75) + 1.5 * IQR(datos$A), 2))), color = "black", size = 3) +
  geom_text(data = data.frame(y = min(datos$A) - 0.5), aes(x = 1.2, y = y, label = paste("Q1 - 1.5 * IQR =", round(quantile(datos$A, 0.25) - 1.5 * IQR(datos$A), 2))), color = "black", size = 3) +
  labs(title = "Diagrama de Caja y Bigotes de la columna B", y = "Valores de la columna B", x = "")

#Datos atípicos B
limite_superior_B <- Q3_B + 1.5 * IQR_datos_B
limite_inferior_B <- Q1_B - 1.5 * IQR_datos_B
valores_atipicos_B <- datos$B[datos$B > limite_superior_B | datos$B < limite_inferior_B]
print(valores_atipicos_B)
print(paste("Los valores atípicos son", valores_atipicos_B))


#####PUNTO 6#####
#DATOS A
concentracion_critica_A <- sum(datos$A > 5) / nrow(datos)  # mg/kg
area_total_A <- 8000  # m^2
area_limpiar_A <- concentracion_critica_A*area_total_A
print(paste("El área a limpiar es", area_limpiar_A, "m2"))

#####PUNTO 7#####
#DATOS B
concentracion_critica_B <- sum(datos$B > 10) / nrow(datos)  # mg/kg
area_total_B <- 8000  # m^2
area_limpiar_B <- concentracion_critica_B*area_total_B
print(paste("El área a limpiar es", area_limpiar_B, "m2"))

#####PUNTO 8#####
coeficiente_correlacion <- cor(datos$A, datos$B)
print(paste("El coeficiente de correlación entre A y B es", coeficiente_correlacion))

#####PUNTO 9#####
datos_filtrados_1 <- datos[datos$A < 5 & datos$B < 10, ]
fraccion_1 <- nrow(datos_filtrados_1) / nrow(datos)
print(paste("La fracción de los datos que tiene un valor A menor que 5 y un valor B menor que 10 es ", fraccion_1))

#####PUNTO 10#####
datos_filtrados_A <- datos[datos$A < 5, ]
datos_filtrados_B <- datos[datos$B < 10, ]
datos_filtrados_2 <- rbind(datos_filtrados_A, datos_filtrados_B)
datos_filtrados_2 <- unique(datos_filtrados)
fraccion_2 <- nrow(datos_filtrados_2) / nrow(datos)
print(paste("La fracción de los datos que tiene un valor A menor que 5 o un valor B menor que 10 es ", fraccion_2))