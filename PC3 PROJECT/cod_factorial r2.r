# =========================================================
# LIBRERÍAS
# =========================================================
# install.packages("readxl")
# install.packages("dplyr")
library(readxl)
library(dplyr)

# =========================================================
# 1. LECTURA DEL EXCEL Y ESTRUCTURA DEL DISEÑO
# =========================================================
ARCHIVO <- "PLANTILLA FACTORIAL.xlsx"

if (!file.exists(ARCHIVO)) {
  stop(sprintf("Error: No se encontró el archivo '%s'.", ARCHIVO))
}

df <- read_excel(ARCHIVO)

columnas_req <- c("Fila", "Columna", "Respuesta")
if (!all(columnas_req %in% names(df))) {
  stop("El Excel debe tener las columnas exactas: Fila, Columna, Respuesta")
}

# Convertir a factores para el análisis categórico
df$Fila <- as.factor(df$Fila)
df$Columna <- as.factor(df$Columna)

cat("============================================================\n")
cat("  DISEÑO FACTORIAL (2 FACTORES) - ANÁLISIS ESTADÍSTICO EN R\n")
cat("============================================================\n")

a <- length(unique(df$Columna))
b <- length(unique(df$Fila))
n <- sum(df$Fila == df$Fila[1] & df$Columna == df$Columna[1])
total_datos <- a * b * n

cat(sprintf("\n[1] ESTRUCTURA DEL DISEÑO\n"))
cat(sprintf("  Factor A (Columnas) : a = %d niveles\n", a))
cat(sprintf("  Factor B (Filas)    : b = %d niveles\n", b))
cat(sprintf("  Réplicas            : n = %d réplicas por celda\n", n))
cat(sprintf("  Total de datos (N)  : %d observaciones\n", total_datos))

# =========================================================
# 2. TABLA ANOVA FACTORIAL
# =========================================================
cat("\n[2] TABLA ANOVA\n")
cat("------------------------------------------------------------\n")

modelo <- aov(Respuesta ~ Fila * Columna, data = df)
tabla_anova <- anova(modelo)

print(round(tabla_anova, 4))

df$Interaccion_AB <- as.factor(paste(df$Fila, "-", df$Columna))

alpha <- 0.05
cat(sprintf("\n  Resumen de significancia (alpha = %s):\n", alpha))

factores_sig <- list()
nombres_factores <- c("Fila", "Columna", "Fila:Columna")
columnas_df <- c("Fila", "Columna", "Interaccion_AB")

for (i in 1:length(nombres_factores)) {
  factor_nom <- nombres_factores[i]
  col_df <- columnas_df[i]
  
  if (factor_nom %in% rownames(tabla_anova)) {
    p_val <- tabla_anova[factor_nom, "Pr(>F)"]
    if (!is.na(p_val)) {
      sig <- ifelse(p_val < alpha, "SI **", "NO")
      cat(sprintf("    %-16s p = %.4f  ->  Diferencia significativa: %s\n", factor_nom, p_val, sig))
      if (p_val < alpha) {
        factores_sig[[factor_nom]] <- col_df
      }
    }
  }
}

# =========================================================
# 3. PRUEBA DE TUKEY HSD (PROTEGIDA Y EXACTA)
# =========================================================
imprimir_tukey_factorial <- function(df_datos, col_factor, tabla_anova_res, alpha=0.05) {
  cat(sprintf("\n============================================================\n"))
  cat(sprintf("  PRUEBA DE TUKEY HSD - %s (alpha = %s)\n", toupper(col_factor), alpha))
  cat(sprintf("============================================================\n"))
  
  mse <- tabla_anova_res["Residuals", "Mean Sq"]
  gl_error <- tabla_anova_res["Residuals", "Df"]
  
  medias <- tapply(df_datos$Respuesta, df_datos[[col_factor]], mean)
  grupos <- names(medias)
  k <- length(grupos)
  
  n_obs_por_grupo <- as.integer(table(df_datos[[col_factor]])[1])
  
  se <- sqrt(mse / n_obs_por_grupo)
  q_crit <- qtukey(1 - alpha, k, gl_error)
  margen <- q_crit * se
  
  cat(sprintf("  MSE (Error): %.4f | GL Error: %d\n", mse, gl_error))
  cat(sprintf("  Observaciones por nivel evaluado: %d\n", n_obs_por_grupo))
  cat(sprintf("  Margen Crítico de Tukey (T): %.4f\n\n", margen))
  
  pares <- combn(grupos, 2)
  resultados <- data.frame()
  
  for (i in 1:ncol(pares)) {
    g1 <- pares[1, i]
    g2 <- pares[2, i]
    diff <- medias[g1] - medias[g2]
    q_stat <- abs(diff) / se
    
    p_adj <- ptukey(q_stat, k, gl_error, lower.tail = FALSE)
    reject <- ifelse(p_adj < alpha, "Si", "No")
    
    fila_res <- data.frame(
      Grupo1 = g1,
      Grupo2 = g2,
      Meandiff = round(diff, 4),
      p_adj = round(p_adj, 4),
      Lower = round(diff - margen, 4),
      Upper = round(diff + margen, 4),
      Rechazar = reject
    )
    resultados <- rbind(resultados, fila_res)
  }
  
  print(resultados, row.names = FALSE)
  
  cat(sprintf("\n  Medias por nivel - %s (mayor a menor):\n", col_factor))
  medias_ord <- sort(medias, decreasing = TRUE)
  print(round(medias_ord, 4))
}

# =========================================================
# 4. EJECUCIÓN DE LAS PRUEBAS DE TUKEY
# =========================================================
if (length(factores_sig) > 0) {
  cat("\n[3] PRUEBAS DE TUKEY HSD PARA FACTORES SIGNIFICATIVOS\n")
  for (fac in names(factores_sig)) {
    col_df <- factores_sig[[fac]]
    imprimir_tukey_factorial(df, col_df, tabla_anova, alpha)
  }
} else {
  cat("\n[3] PRUEBA DE TUKEY HSD\n")
  cat("  >> Ningún efecto resultó significativo. No se aplica Tukey.\n")
}

# =========================================================
# 5. GRÁFICOS DE DIAGNÓSTICO Y CONTROL (1, 2 y 4)
# =========================================================
cat("\n[4] GENERANDO GRÁFICOS DE DIAGNÓSTICO OLS...\n")

# Configurar una ventana de gráficos con una matriz de 2x2
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

# --- GRÁFICO 1: Efecto de Interacción AB ---
interaction.plot(
  x.factor = df$Columna, 
  trace.factor = df$Fila, 
  response = df$Respuesta,
  fun = mean,
  type = "b", 
  pch = 19,
  fixed = TRUE,
  xlab = "Niveles del Factor Columna (A)",
  ylab = "Media de la Respuesta",
  trace.label = "Factor Fila (B)",
  main = "1. Gráfico de Interacción (A x B)"
)

# --- GRÁFICO 2: Intervalos de Confianza de Tukey (Factor Principal Columna) ---
# Usamos el comando nativo de R sobre el modelo para graficar las comparaciones directamente
plot(
  TukeyHSD(modelo, "Columna"), 
  las = 1, 
  col = "darkgreen"
)
title(main = "2. Intervalos de Tukey (Columna)")

# --- GRÁFICO 4A: Residuos vs Ajustados (Homocedasticidad) ---
plot(
  modelo, 
  which = 1, 
  caption = "",
  main = "4A. Residuos vs Valores Ajustados"
)

# --- GRÁFICO 4B: normal Q-Q Plot (Normalidad de Residuos) ---
plot(
  modelo, 
  which = 2, 
  caption = "",
  main = "4B. Gráfico Q-Q Normal"
)

# Restaurar la configuración original de la pantalla gráfica
par(mfrow = c(1, 1))

cat("\n============================================================\n")
cat("  ANÁLISIS COMPLETADO EXITOSAMENTE\n")
cat("============================================================\n")