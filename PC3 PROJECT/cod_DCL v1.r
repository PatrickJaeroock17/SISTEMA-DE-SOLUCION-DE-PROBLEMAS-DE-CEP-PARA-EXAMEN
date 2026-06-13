# =========================================================
# LIBRERÍAS
# =========================================================
# install.packages("readxl")
# install.packages("dplyr")
library(readxl)
library(dplyr)

# =========================================================
# 1. LECTURA DEL EXCEL
# =========================================================
ARCHIVO <- "PLANTILLA DCL.xlsx"
df <- read_excel(ARCHIVO)

# Verificar columnas requeridas
columnas_req <- c("Fila", "Columna", "Tratamiento", "Respuesta")
if (!all(columnas_req %in% names(df))) {
  stop("El Excel debe tener las columnas: Fila, Columna, Tratamiento, Respuesta")
}

# Convertir a factores (crucial para ANOVA en R)
df$Fila <- as.factor(df$Fila)
df$Columna <- as.factor(df$Columna)
df$Tratamiento <- as.factor(df$Tratamiento)

# =========================================================
# 2. DCL COMPLETO O INCOMPLETO (FÓRMULA DE YATES)
# =========================================================
hallar_dato <- function(data) {
  idx <- which(is.na(data$Respuesta))
  
  if (length(idx) == 0) stop("No se encontró ningún dato perdido (NA) en 'Respuesta'.")
  if (length(idx) > 1) stop("Se encontró más de un dato perdido. Solo aplica para UN dato.")
  
  fila_val <- data$Fila[idx]
  col_val <- data$Columna[idx]
  trat_val <- data$Tratamiento[idx]
  
  data_obs <- data[!is.na(data$Respuesta), ]
  
  t <- length(unique(data$Fila))
  Ri <- sum(data_obs$Respuesta[data_obs$Fila == fila_val])
  Cj <- sum(data_obs$Respuesta[data_obs$Columna == col_val])
  Tk <- sum(data_obs$Respuesta[data_obs$Tratamiento == trat_val])
  G <- sum(data_obs$Respuesta)
  
  x_est <- (t * Ri + t * Cj + t * Tk - 2 * G) / ((t - 1) * (t - 2))
  
  cat(sprintf("\n  Dato perdido detectado:\n"))
  cat(sprintf("    Fila=%s, Columna=%s, Tratamiento=%s\n", fila_val, col_val, trat_val))
  cat(sprintf("    t=%d, Ri=%.4f, Cj=%.4f, Tk=%.4f, G=%.4f\n", t, Ri, Cj, Tk, G))
  cat(sprintf("    Dato estimado x' = %.4f\n", x_est))
  
  return(x_est)
}

cat("============================================================\n")
cat("  DISEÑO DE CUADRADO LATINO - ANÁLISIS ESTADÍSTICO EN R\n")
cat("============================================================\n")

# Interacción por consola
a <- toupper(trimws(readline(prompt="Escribe COMPLETO o INCOMPLETO: ")))

if (a == "INCOMPLETO") {
  cat("\n>> Modo: DCL INCOMPLETO (un dato faltante)\n")
  cat(">> Estimando el dato perdido con la fórmula de Yates...\n")
  dato <- hallar_dato(df)
  df$Respuesta[is.na(df$Respuesta)] <- dato
  cat(sprintf("\n>> Dato estimado insertado en el DataFrame: %.4f\n", dato))
} else {
  cat("\n>> Modo: DCL COMPLETO\n")
}

# =========================================================
# 3. RESUMEN DESCRIPTIVO
# =========================================================
cat("\n[1] ESTADÍSTICOS DESCRIPTIVOS POR TRATAMIENTO\n")
desc <- df %>%
  group_by(Tratamiento) %>%
  summarise(
    N = n(),
    Media = round(mean(Respuesta), 4),
    DE = round(sd(Respuesta), 4),
    Min = min(Respuesta),
    Max = max(Respuesta)
  )
print(as.data.frame(desc), row.names = FALSE)

# =========================================================
# 4. ANOVA CUADRADO LATINO Y CORRECCIÓN
# =========================================================
cat("\n[2] TABLA ANOVA - CUADRADO LATINO\n")
cat("------------------------------------------------------------\n")

# Ajustar el modelo OLS
modelo <- aov(Respuesta ~ Fila + Columna + Tratamiento, data = df)
tabla_anova <- anova(modelo)

# --- INICIO DE LA CORRECCIÓN PARA DCL INCOMPLETO ---
if (a == "INCOMPLETO") {
  # 1. Restar 1 al Grado de Libertad del Error (Residuals)
  tabla_anova["Residuals", "Df"] <- tabla_anova["Residuals", "Df"] - 1
  
  # 2. Recalcular el Cuadrado Medio del Error
  nuevo_gl_error <- tabla_anova["Residuals", "Df"]
  nuevo_mse <- tabla_anova["Residuals", "Sum Sq"] / nuevo_gl_error
  tabla_anova["Residuals", "Mean Sq"] <- nuevo_mse
  
  # 3. Recalcular F y P-values
  factores <- c("Fila", "Columna", "Tratamiento")
  for (factor in factores) {
    nuevo_f <- tabla_anova[factor, "Mean Sq"] / nuevo_mse
    tabla_anova[factor, "F value"] <- nuevo_f
    
    gl_factor <- tabla_anova[factor, "Df"]
    # pf() es la función de R equivalente a f.sf() de Python
    nuevo_p <- pf(nuevo_f, gl_factor, nuevo_gl_error, lower.tail = FALSE)
    tabla_anova[factor, "Pr(>F)"] <- nuevo_p
  }
}
# --- FIN DE LA CORRECCIÓN ---

print(round(tabla_anova, 4))

alpha <- 0.05
cat(sprintf("\n  Resumen de significancia (alpha = %s):\n", alpha))

factores_sig <- c()
for (factor in c("Fila", "Columna", "Tratamiento")) {
  p_val <- tabla_anova[factor, "Pr(>F)"]
  if (!is.na(p_val)) {
    sig <- ifelse(p_val < alpha, "SI **", "NO")
    cat(sprintf("    %-14s p = %.4f  ->  Diferencia significativa: %s\n", factor, p_val, sig))
    if (p_val < alpha) {
      factores_sig <- c(factores_sig, factor)
    }
  }
}

# =========================================================
# 5. PRUEBA DE TUKEY CORREGIDA
# =========================================================
imprimir_tukey_corregido <- function(df_datos, col_factor, tabla_anova_res, alpha=0.05) {
  cat(sprintf("\n============================================================\n"))
  cat(sprintf("  PRUEBA DE TUKEY HSD (DCL) - %s (alpha = %s)\n", toupper(col_factor), alpha))
  cat(sprintf("============================================================\n"))
  
  # Rescatar MSE y GL
  mse <- tabla_anova_res["Residuals", "Mean Sq"]
  gl_error <- tabla_anova_res["Residuals", "Df"]
  
  # Calcular medias
  medias <- tapply(df_datos$Respuesta, df_datos[[col_factor]], mean)
  grupos <- names(medias)
  k <- length(grupos)
  n <- nrow(df_datos) / k
  
  # Error estándar y valor Q (qtukey es la versión nativa de R para qsturng)
  se <- sqrt(mse / n)
  q_crit <- qtukey(1 - alpha, k, gl_error)
  margen <- q_crit * se
  
  cat(sprintf("  MSE (Error DCL): %.4f | GL Error: %d\n", mse, gl_error))
  cat(sprintf("  Margen Crítico de Tukey (T): %.4f\n\n", margen))
  
  # Iterar combinaciones de pares
  pares <- combn(grupos, 2)
  resultados <- data.frame()
  
  for (i in 1:ncol(pares)) {
    g1 <- pares[1, i]
    g2 <- pares[2, i]
    diff <- medias[g1] - medias[g2]
    q_stat <- abs(diff) / se
    
    # ptukey es la versión nativa de R para psturng
    p_adj <- ptukey(q_stat, k, gl_error, lower.tail = FALSE)
    reject <- ifelse(p_adj < alpha, "Si (diferencia sig.)", "No")
    
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
  
  cat(sprintf("\n  Medias por grupo - %s (mayor a menor):\n", col_factor))
  medias_ord <- sort(medias, decreasing = TRUE)
  print(round(medias_ord, 4))
}

# =========================================================
# 6. EJECUCIÓN DE TUKEY
# =========================================================
if (length(factores_sig) > 0) {
  cat("\n[3] PRUEBAS DE TUKEY HSD PARA FACTORES SIGNIFICATIVOS\n")
  for (fac in factores_sig) {
    imprimir_tukey_corregido(df, fac, tabla_anova, alpha)
  }
} else {
  cat("\n[3] PRUEBA DE TUKEY HSD\n")
  cat("  >> Ningún factor resultó significativo. No se aplica Tukey.\n")
}

cat("\n============================================================\n")
cat("  ANÁLISIS COMPLETADO\n")
cat("============================================================\n")