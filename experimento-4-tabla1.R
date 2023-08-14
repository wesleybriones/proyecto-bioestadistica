# Crear la tabla en R
data <- data.frame(
  Tratamiento = rep(c("A1", "A2", "A3"), each = 50),
  Repeticiones = rep(rep(c("I", "II", "III", "IV", "V"), each = 2 * 5), times = 3),
  dias = c(
    14.52, 1.81, 4.08, 2.19, 2.80, 2.36, 1.91, 3.99, 1.81, 1.95,
    89.70, 1.36, 80.63, 2.99, 80.27, 1.32, 79.63, 2.59, 83.03, 1.45,
    14.515, 1.36, 3.18, 3.08, 2.45, 1.50, 1.77, 3.45, 2.45, 1.72,
    13.15, 0.91, 2.27, 3.82, 2.09, 1.00, 3.22, 3.22, 2.63, 1.63,
    14.52, 3.63, 78.36, 5.63, 2.18, 78.72, 3.08, 2.18, 78.91, 1.54,
    93.33, 2.27, 3.63, 4.26, 79.36, 2.81, 3.49, 3.77, 79.04, 2.40,
    18.144, 2.27, 1.81, 6.17, 1.91, 2.09, 3.40, 3.31, 1.95, 1.68,
    91.97, 0.91, 4.08, 3.99, 80.36, 2.99, 2.63, 3.54, 78.72, 2.13,
    15.88, 0.45, 4.08, 4.54, 2.63, 2.64, 3.54, 2.42, 1.72, 1.86,
    95.14, 5.90, 1.82, 4.62, 79.18, 2.99, 3.72, 2.95, 1.54, 1.68,
    90.61, 3.18, 1.36, 8.80, 78.81, 4.63, 3.04, 2.86, 80.09, 2.99,
    14.97, 2.72, 3.18, 4.17, 3.09, 3.90, 3.45, 3.18, 2.81, 2.99,
    95.14, 2.72, 4.54, 3.36, 80.36, 2.54, 3.81, 3.54, 79.59, 2.81,
    17.69, 1.81, 1.36, 2.00, 7.26, 3.08, 3.11, 2.42, 2.04, 2.00,
    90.61, 2.27, 4.08, 4.17, 79.36, 3.18, 2.50, 2.95, 2.81, 2.63
  )
)

# Imprimir la tabla
print(data)

# Realizar la prueba de Shapiro-Wilk para cada grupo de tratamiento
unique_treatments <- unique(data$Tratamiento)

for (treatment in unique_treatments) {
  subset_data <- data[data$Tratamiento == treatment, "dias"]
  shapiro_result <- shapiro.test(subset_data)
  
  cat("Tratamiento:", treatment, "\n")
  cat("p-value:", shapiro_result$p.value, "\n")
  
  if (shapiro_result$p.value < 0.05) {
    cat("La distribución no es normal (rechazar H0)\n\n")
  } else {
    cat("La distribución es normal (no se puede rechazar H0)\n\n")
  }
}

# Librería necesaria para cálculos de percentiles
library(Hmisc)

# Tratamientos únicos
unique_treatments <- unique(data$Tratamiento)

# Calcular medidas para cada tratamiento
for (treatment in unique_treatments) {
  subset_data <- data[data$Tratamiento == treatment, "dias"]
  
  cat("Tratamiento:", treatment, "\n")
  
  # Medidas de tendencia central
  mean_val <- mean(subset_data)
  cat("Media:", mean_val, "\n")
  
  moda_val <- as.numeric(names(table(subset_data))[table(subset_data) == max(table(subset_data))])
  cat("Moda:", moda_val, "\n")
  
  median_val <- median(subset_data)
  cat("Mediana:", median_val, "\n")
  
  # Medidas de dispersión
  var_val <- var(subset_data)
  cat("Varianza:", var_val, "\n")
  
  sd_val <- sd(subset_data)
  cat("Desviación Estándar:", sd_val, "\n")
  
  se_val <- sd_val / sqrt(length(subset_data))
  cat("Error Estándar:", se_val, "\n")
  
  cv_val <- (sd_val / mean_val) * 100
  cat("Coeficiente de Variación:", cv_val, "%\n")
  
  range_val <- range(subset_data)[2] - range(subset_data)[1]
  cat("Rango:", range_val, "\n")
  
  # Medidas de posición
  quantiles <- quantile(subset_data, probs = c(0.25, 0.5, 0.75))
  cat("Cuartiles (25%, 50%, 75%):", quantiles, "\n")
  
  deciles <- Hmisc::wtd.quantile(subset_data, probs = seq(0.1, 0.9, by = 0.1))
  cat("Deciles:", deciles, "\n")
  
  percentiles <- Hmisc::wtd.quantile(subset_data, probs = seq(0.01, 0.99, by = 0.01))
  cat("Percentiles (1-99):", percentiles, "\n")
  
  cat("\n")
}
# Cargar la librería necesaria
library(ggplot2)

# Tratamientos únicos
unique_treatments <- unique(data$Tratamiento)

# Crear gráficos para cada tratamiento
for (treatment in unique_treatments) {
  subset_data <- data[data$Tratamiento == treatment, ]
  
  # Crear gráfico de boxplot
  gg_boxplot <- ggplot(subset_data, aes(x = Tratamiento, y = dias)) +
    geom_boxplot(fill = "lightgreen", color = "darkgreen", alpha = 0.6) +
    labs(title = paste("Boxplot de Tratamiento", treatment), x = "Tratamiento", y = "Días") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Imprimir los gráficos
  print(gg_boxplot)
}

# Librería necesaria para la prueba de Shapiro-Wilk
library(stats)

# Tratamientos únicos
unique_treatments <- unique(data$Tratamiento)

# Realizar análisis para cada tratamiento
for (treatment in unique_treatments) {
  subset_data <- data[data$Tratamiento == treatment, "dias"]
  
  cat("Tratamiento:", treatment, "\n")
  
  # Medidas de tendencia central
  mean_val <- mean(subset_data)
  cat("Media:", mean_val, "\n")
  
  median_val <- median(subset_data)
  cat("Mediana:", median_val, "\n")
  
  # Desviación estándar
  sd_val <- sd(subset_data)
  cat("Desviación Estándar:", sd_val, "\n")
  
  # Prueba de Shapiro-Wilk
  shapiro_result <- shapiro.test(subset_data)
  cat("Prueba de Shapiro-Wilk - p-value:", shapiro_result$p.value, "\n")
  
  if (shapiro_result$p.value < 0.05) {
    cat("La distribución no es normal (rechazar H0)\n\n")
  } else {
    cat("La distribución es normal (no se puede rechazar H0)\n\n")
  }
}

# ANOVA (Análisis de varianza)
# Realizar análisis de varianza (ANOVA)
anova_result <- aov(dias ~ Tratamiento, data = data)

# Imprimir el resultado del ANOVA
print(summary(anova_result))

