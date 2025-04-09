# ===========================================
# Reporte Final - Análisis de Datos de Salarios de Mujeres
# Curso: Visualización y Análisis de Datos
# Herramientas: R, dplyr, ggplot2, tidyverse
# ===========================================

# -------------------------------------------
# MÓDULO 2: Carga, Exploración y Limpieza de Datos
# -------------------------------------------

# Cargar librerías necesarias
library(readr)
library(dplyr)

# Cargar el dataset original
salarios <- read_csv("salarios_mujeres.csv")

# Explorar el dataset
head(salarios)               # Primeras filas
str(salarios)                # Estructura del dataset
summary(salarios)           # Resumen estadístico

# Verificar valores nulos
colSums(is.na(salarios))

# Identificar registros duplicados
duplicados <- salarios[duplicated(salarios), ]
cat("Registros duplicados encontrados:", nrow(duplicados), "\n")

# Limpieza: eliminar duplicados y filas con NA
salarios_limpios <- salarios %>%
  filter(!duplicated(.)) %>%
  na.omit()

# Guardar dataset limpio
write_csv(salarios_limpios, "salarios_mujeres_limpio.csv")
cat("Datos limpios guardados en 'salarios_mujeres_limpio.csv'\n")

# -------------------------------------------
# MÓDULO 3: Análisis Estadístico y Visualización
# -------------------------------------------

# Cargar librerías adicionales
library(ggplot2)
library(modeest)
library(tidyverse)

# Usar el dataset limpio
salarios <- salarios_limpios

# Medidas de tendencia central
media_salario <- mean(salarios$Salario, na.rm = TRUE)
mediana_salario <- median(salarios$Salario, na.rm = TRUE)
moda_salario <- as.numeric(names(sort(table(salarios$Salario), decreasing = TRUE)[1]))

# Mostrar resultados
cat("Media del salario:", media_salario, "\n")
cat("Mediana del salario:", mediana_salario, "\n")
cat("Moda del salario:", moda_salario, "\n")

# Visualización: Histograma + Curva de densidad
ggplot(salarios, aes(x = Salario)) +
  geom_histogram(aes(y = ..density..), bins = 25, fill = "#69b3a2", color = "black", alpha = 0.8) +
  geom_density(color = "darkred", size = 1.2) +
  labs(
    title = "Distribución de Salarios",
    x = "Salario",
    y = "Densidad"
  ) +
  theme_bw()

