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

# Verificar si el archivo existe
if (!file.exists(ARCHIVO)) {
  stop(sprintf("Error: No se encontró el archivo '%s'.", ARCHIVO))
}

df <- read_excel(ARCHIVO)

# Verificar columnas requeridas
columnas_req <- c("Fila", "Columna", "Respuesta")
if (!all(columnas_req %in% names(df))) {
  stop("El Excel debe tener las columnas exactas: Fila, Columna, Respuesta")
}

# Convertir a factores (¡CRUCIAL en R para que no los trate como números continuos!)
df$Fila <- as.factor(df$Fila)
df$Columna <- as.factor(df$Columna)

cat("============================================================\n")
cat("  DISEÑO FACTORIAL (2 FACTORES) - ANÁLISIS ESTADÍSTICO EN R\n")
cat("============================================================\n")

# Calcular a, b y n
a <- length(unique(df$Columna))
b <- length(unique(df$Fila))
# Calculamos n (réplicas) contando los datos del primer cruce
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

# El símbolo '*' incluye los efectos principales y la interacción (Fila:Columna)
modelo <- aov(Respuesta ~ Fila * Columna, data = df)
tabla_anova <- anova(modelo)

print(round(tabla_anova, 4))

# CREACIÓN DE LA COLUMNA DE INTERACCIÓN PARA TUKEY
# Une los textos (Ej. "B1 - A1") y lo convierte a factor
df$Interaccion_AB <- as.factor(paste(df$Fila, "-", df$Columna))

alpha <- 0.05
cat(sprintf("\n  Resumen de significancia (alpha = %s):\n", alpha))

factores_sig <- list()

# R llama a la interacción "Fila:Columna" por defecto en la tabla ANOVA
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
      
      # Guardamos el nombre de la columna si es significativo
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
  
  # Extraemos el Error Real del modelo
  mse <- tabla_anova_res["Residuals", "Mean Sq"]
  gl_error <- tabla_anova_res["Residuals", "Df"]
  
  # Calcular medias
  medias <- tapply(df_datos$Respuesta, df_datos[[col_factor]], mean)
  grupos <- names(medias)
  k <- length(grupos)
  
  # Determina dinámicamente las observaciones por nivel (n o b*n o a*n)
  n_obs_por_grupo <- as.integer(table(df_datos[[col_factor]])[1])
  
  # Error estándar y Rango Crítico
  se <- sqrt(mse / n_obs_por_grupo)
  q_crit <- qtukey(1 - alpha, k, gl_error)
  margen <- q_crit * se
  
  cat(sprintf("  MSE (Error): %.4f | GL Error: %d\n", mse, gl_error))
  cat(sprintf("  Observaciones por nivel evaluado: %d\n", n_obs_por_grupo))
  cat(sprintf("  Margen Crítico de Tukey (T): %.4f\n\n", margen))
  
  # Combinaciones y cálculo matemático de p-values ajustados
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
# 4. EJECUCIÓN DE LAS PRUEBAS
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

cat("\n============================================================\n")
cat("  ANÁLISIS COMPLETADO\n")
cat("============================================================\n")