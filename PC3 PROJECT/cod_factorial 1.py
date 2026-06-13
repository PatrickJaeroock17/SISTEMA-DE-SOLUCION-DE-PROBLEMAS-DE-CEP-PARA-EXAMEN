import sys
import pandas as pd
import numpy as np
import itertools
from statsmodels.formula.api import ols
from statsmodels.stats.anova import anova_lm
from statsmodels.stats.libqsturng import psturng, qsturng
import warnings
warnings.filterwarnings("ignore")

# Forzar UTF-8 en consola Windows
if sys.stdout.encoding != "utf-8":
    import io
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8", errors="replace")

# ---------------------------------------------------------
# 1. LECTURA DEL EXCEL
# ---------------------------------------------------------
ARCHIVO = "PLANTILLA FACTORIAL.xlsx"
try:
    df = pd.read_excel(ARCHIVO)
except FileNotFoundError:
    print(f"Error: No se encontró el archivo '{ARCHIVO}'. Asegúrate de que esté en la misma carpeta.")
    sys.exit()

# Asumimos que las columnas se llaman Fila, Columna y Respuesta. 
# (Se pueden cambiar si tu plantilla tiene otros nombres)
columnas_req = {"Fila", "Columna", "Respuesta"}
if not columnas_req.issubset(df.columns):
    raise ValueError(f"El Excel debe tener las columnas: {columnas_req}")

print("=" * 60)
print("  DISEÑO FACTORIAL (2 FACTORES) - ANÁLISIS ESTADÍSTICO")
print("=" * 60)

# Calcular a, b y n
a = df["Columna"].nunique()
b = df["Fila"].nunique()
# Asumiendo un diseño balanceado, calculamos n (réplicas) contando cuántos datos hay en una combinación
n = len(df[(df["Fila"] == df["Fila"].iloc[0]) & (df["Columna"] == df["Columna"].iloc[0])])
total_datos = a * b * n

print(f"\n[1] ESTRUCTURA DEL DISEÑO")
print(f"  Factor A (Columnas) : a = {a} niveles")
print(f"  Factor B (Filas)    : b = {b} niveles")
print(f"  Réplicas            : n = {n} réplicas por celda")
print(f"  Total de datos (N)  : {total_datos} observaciones")

# ---------------------------------------------------------
# 2. TABLA ANOVA FACTORIAL
#    Modelo: Respuesta ~ Fila + Columna + Fila*Columna
# ---------------------------------------------------------
print("\n[2] TABLA ANOVA")
print("-" * 60)

# El símbolo '*' en OLS incluye los efectos principales y la interacción automáticamente
modelo = ols("Respuesta ~ C(Fila) * C(Columna)", data=df).fit()
tabla_anova = anova_lm(modelo, typ=1)

# Renombrar índices para mayor claridad en la consola
tabla_anova.index = [
    idx.replace("C(Fila)", "Filas")
       .replace("C(Columna)", "Columnas")
       .replace("C(Fila):C(Columna)", "Interaccion_AB")
    for idx in tabla_anova.index
]

print(tabla_anova.round(4).to_string())

alpha = 0.05
print(f"\n  Resumen de significancia (alpha = {alpha}):")

factores_sig = {}
# Evaluamos principales e interacción
for factor_label, col_df in [("Filas", "Fila"), ("Columnas", "Columna"), ("Interaccion_AB", None)]:
    if factor_label in tabla_anova.index:
        p = tabla_anova.loc[factor_label, "PR(>F)"]
        if pd.notna(p):
            sig = "SI **" if p < alpha else "NO"
            print(f"    {factor_label:<14} p = {p:.4f}  ->  Diferencia significativa: {sig}")
            # Solo guardamos los factores principales para hacer Tukey (no la interacción)
            if p < alpha and col_df is not None:
                factores_sig[factor_label] = col_df

# ---------------------------------------------------------
# 3. PRUEBA DE TUKEY HSD (PROTEGIDA)
# ---------------------------------------------------------
def imprimir_tukey_factorial(df_datos, col_factor, tabla_anova_res, alpha=0.05):
    print(f"\n{'=' * 60}")
    print(f"  PRUEBA DE TUKEY HSD - EFECTO PRINCIPAL: {col_factor.upper()} (alpha = {alpha})")
    print(f"{'=' * 60}")

    # Extraemos el Error Real del modelo factorial completo
    mse = float(tabla_anova_res.loc["Residual", "sum_sq"] / tabla_anova_res.loc["Residual", "df"])
    gl_error = int(tabla_anova_res.loc["Residual", "df"])

    # Calcular medias por grupo del factor solicitado
    medias = df_datos.groupby(col_factor)["Respuesta"].mean()
    grupos = medias.index.tolist()
    k = len(grupos)
    
    # En un diseño factorial, el 'n' para el error estándar de un factor principal 
    # es la cantidad total de datos evaluados en ese nivel específico.
    n_obs_por_grupo = int(df_datos.groupby(col_factor)["Respuesta"].count().iloc[0])

    se = np.sqrt(mse / n_obs_por_grupo)
    q_crit = qsturng(1 - alpha, k, gl_error)
    margen = float(q_crit * se)

    print(f"  MSE (Error): {mse:.4f} | GL Error: {gl_error}")
    print(f"  Observaciones por nivel evaluado: {n_obs_por_grupo}")
    print(f"  Margen Crítico de Tukey (T): {margen:.4f}\n")

    resultados = []
    for g1, g2 in itertools.combinations(grupos, 2):
        diff = float(medias[g1] - medias[g2]) 
        q_stat = float(abs(diff) / se)
        
        p_adj = float(np.squeeze(psturng(q_stat, k, gl_error)))
        reject = True if p_adj < alpha else False
        
        resultados.append({
            "Grupo 1": g1,
            "Grupo 2": g2,
            "Meandiff": round(diff, 4),
            "p-adj": round(p_adj, 4),
            "Lower": round(diff - margen, 4),
            "Upper": round(diff + margen, 4),
            "Rechazar?": "Si" if reject else "No"
        })

    df_res = pd.DataFrame(resultados)
    print(df_res.to_string(index=False))

    print(f"\n  Medias por nivel - {col_factor} (mayor a menor):")
    print(medias.sort_values(ascending=False).round(4).to_string())

# ---------------------------------------------------------
# 4. EJECUCIÓN DE LAS PRUEBAS
# ---------------------------------------------------------
if factores_sig:
    print(f"\n[3] PRUEBAS DE TUKEY HSD PARA FACTORES SIGNIFICATIVOS")
    for factor_label, col_df in factores_sig.items():
        imprimir_tukey_factorial(df, col_df, tabla_anova, alpha)
else:
    print("\n[3] PRUEBA DE TUKEY HSD")
    print("  >> Ningún efecto principal resultó significativo. No se aplica Tukey.")

print("\n" + "=" * 60)
print("  ANÁLISIS COMPLETADO")
print("=" * 60)
