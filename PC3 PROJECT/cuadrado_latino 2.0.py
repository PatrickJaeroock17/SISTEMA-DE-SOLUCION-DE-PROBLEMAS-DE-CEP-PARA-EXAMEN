import sys
import pandas as pd
import numpy as np
from statsmodels.formula.api import ols
from statsmodels.stats.anova import anova_lm
from statsmodels.stats.multicomp import pairwise_tukeyhsd
from statsmodels.stats.libqsturng import psturng, qsturng
import itertools
import warnings
warnings.filterwarnings("ignore")

# Forzar UTF-8 en consola Windows
if sys.stdout.encoding != "utf-8":
    import io
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8", errors="replace")

# ---------------------------------------------------------
# 1. LECTURA DEL EXCEL
# ---------------------------------------------------------
ARCHIVO = "PLANTILLA DCL.xlsx"
df = pd.read_excel(ARCHIVO)

columnas_req = {"Fila", "Columna", "Tratamiento", "Respuesta"}
if not columnas_req.issubset(df.columns):
    raise ValueError(f"El Excel debe tener las columnas: {columnas_req}")

# ---------------------------------------------------------
# 2. DCL COMPLETO O INCOMPLETO
# ---------------------------------------------------------

def hallar_dato(df):
    """
    Estima el dato perdido en un DCL incompleto usando la fórmula de Yates:

        x' = (t*Ri + t*Cj + t*Tk - 2*G) / ((t-1)*(t-2))

    Donde:
        t  = tamaño del cuadrado (número de tratamientos / filas / columnas)
        Ri = suma de la fila donde falta el dato
        Cj = suma de la columna donde falta el dato
        Tk = suma del tratamiento donde falta el dato
        G  = gran total de todas las observaciones disponibles

    El Excel debe tener NaN en la celda de Respuesta del dato perdido,
    y los valores de Fila, Columna y Tratamiento de esa celda deben estar completos.
    """
    fila_perdida = df[df["Respuesta"].isnull()]

    if len(fila_perdida) == 0:
        raise ValueError("No se encontro ningun dato perdido (NaN) en la columna 'Respuesta'.")
    if len(fila_perdida) > 1:
        raise ValueError("Se encontro mas de un dato perdido. Esta formula solo aplica para UN dato faltante.")

    idx_perdido = fila_perdida.index[0]
    fila_val    = df.loc[idx_perdido, "Fila"]
    col_val     = df.loc[idx_perdido, "Columna"]
    trat_val    = df.loc[idx_perdido, "Tratamiento"]

    df_obs = df.dropna(subset=["Respuesta"])  # Solo observaciones completas

    t  = df["Fila"].nunique()                                                      # Tamaño del cuadrado
    Ri = df_obs[df_obs["Fila"]        == fila_val]["Respuesta"].sum()              # Total de la fila
    Cj = df_obs[df_obs["Columna"]     == col_val]["Respuesta"].sum()               # Total de la columna
    Tk = df_obs[df_obs["Tratamiento"] == trat_val]["Respuesta"].sum()              # Total del tratamiento
    G  = df_obs["Respuesta"].sum()                                                 # Gran total

    x_est = (t * Ri + t * Cj + t * Tk - 2 * G) / ((t - 1) * (t - 2))

    print(f"\n  Dato perdido detectado:")
    print(f"    Fila={fila_val}, Columna={col_val}, Tratamiento={trat_val}")
    print(f"    t={t}, Ri={Ri:.4f}, Cj={Cj:.4f}, Tk={Tk:.4f}, G={G:.4f}")
    print(f"    Dato estimado x' = ({t}*{Ri:.4f} + {t}*{Cj:.4f} + {t}*{Tk:.4f} - 2*{G:.4f})")
    print(f"                     / (({t}-1)*({t}-2))")
    print(f"                   = {x_est:.4f}")

    return x_est


a = input("COMPLETO O INCOMPLETO: ").strip().upper()

print("=" * 60)
print("  DISENO DE CUADRADO LATINO - ANALISIS ESTADISTICO")
print("=" * 60)

if a == "INCOMPLETO":
    print("\n>> Modo: DCL INCOMPLETO (un dato faltante)")
    print(">> Estimando el dato perdido con la formula de Yates...")
    dato = hallar_dato(df)
    df.loc[df["Respuesta"].isnull(), "Respuesta"] = dato
    print(f"\n>> Dato estimado insertado en el DataFrame: {dato:.4f}")
else:
    print("\n>> Modo: DCL COMPLETO")

# ---------------------------------------------------------
# 3. RESUMEN DESCRIPTIVO
# ---------------------------------------------------------
print("\n[1] DATOS UTILIZADOS")
print(df.to_string(index=False))

print("\n[2] ESTADISTICOS DESCRIPTIVOS POR TRATAMIENTO")
desc = df.groupby("Tratamiento")["Respuesta"].agg(
    N="count", Media="mean", DE="std", Min="min", Max="max"
).round(4)
print(desc.to_string())

# ---------------------------------------------------------
# 4. ANOVA CUADRADO LATINO
#    Modelo: Respuesta ~ Fila + Columna + Tratamiento
# ---------------------------------------------------------
print("\n[3] TABLA ANOVA - CUADRADO LATINO")
print("-" * 60)

modelo = ols("Respuesta ~ C(Fila) + C(Columna) + C(Tratamiento)", data=df).fit()
tabla_anova = anova_lm(modelo, typ=1)

tabla_anova.index = [
    idx.replace("C(Fila)", "Filas")
       .replace("C(Columna)", "Columnas")
       .replace("C(Tratamiento)", "Tratamientos")
    for idx in tabla_anova.index
]

print(tabla_anova.round(4).to_string())

alpha = 0.05
print(f"\n  Resumen de significancia (alpha = {alpha}):")

# Guardar que factores son significativos para despues hacer Tukey
factores_sig = {}  # nombre_label -> nombre_columna_df

for factor_label, col_df in [("Filas", "Fila"), ("Columnas", "Columna"), ("Tratamientos", "Tratamiento")]:
    if factor_label in tabla_anova.index:
        p = tabla_anova.loc[factor_label, "PR(>F)"]
        if pd.notna(p):
            sig = "SI **" if p < alpha else "NO"
            print(f"    {factor_label:<14} p = {p:.4f}  ->  Diferencia significativa: {sig}")
            if p < alpha:
                factores_sig[factor_label] = col_df

# ---------------------------------------------------------
# 5. PRUEBA DE TUKEY CORREGIDA PARA DCL
# ---------------------------------------------------------

def imprimir_tukey_corregido(df_datos, col_factor, tabla_anova_res, alpha=0.05):
    print(f"\n{'=' * 60}")
    print(f"  PRUEBA DE TUKEY HSD (DCL) - {col_factor.upper()} (alpha = {alpha})")
    print(f"{'=' * 60}")

    # 1. Rescatar el MSE y los Grados de Libertad del Error de la tabla ANOVA real
    mse = float(tabla_anova_res.loc["Residual", "sum_sq"] / tabla_anova_res.loc["Residual", "df"])
    gl_error = int(tabla_anova_res.loc["Residual", "df"])

    # 2. Calcular medias y el número de observaciones (n) por grupo
    medias = df_datos.groupby(col_factor)["Respuesta"].mean()
    grupos = medias.index.tolist()
    k = len(grupos)
    n = len(df_datos) / k  # Funciona para el diseño balanceado del DCL

    # 3. Calcular Error Estándar (SE) y el Rango Crítico de Tukey
    se = np.sqrt(mse / n)
    q_crit = qsturng(1 - alpha, k, gl_error)
    margen = float(q_crit * se)

    print(f"  MSE (Error DCL): {mse:.4f} | GL Error: {gl_error}")
    print(f"  Margen Crítico de Tukey (T): {margen:.4f}\n")

    # 4. Calcular p-values ajustados y diferencias para cada par
    resultados = []
    for g1, g2 in itertools.combinations(grupos, 2):
        # FORZAMOS a que diff sea un float de Python
        diff = float(medias[g1] - medias[g2]) 
        q_stat = float(abs(diff) / se)
        
        # FORZAMOS a que p_adj se extraiga como escalar usando np.squeeze y float
        p_adj = float(np.squeeze(psturng(q_stat, k, gl_error)))
        
        reject = True if p_adj < alpha else False
        
        resultados.append({
            "Grupo 1": g1,
            "Grupo 2": g2,
            "Meandiff": round(diff, 4),
            "p-adj": round(p_adj, 4),
            "Lower": round(diff - margen, 4),
            "Upper": round(diff + margen, 4),
            "Rechazar?": "Si (diferencia sig.)" if reject else "No"
        })

    # Mostrar resultados en formato tabla
    df_res = pd.DataFrame(resultados)
    print(df_res.to_string(index=False))

    print(f"\n  Medias por grupo - {col_factor} (mayor a menor):")
    print(medias.sort_values(ascending=False).round(4).to_string())
# ---------------------------------------------------------
# 6. EJECUCIÓN DE TUKEY (Asegúrate de pegarlo al final, sin sangría)
# ---------------------------------------------------------
if factores_sig:
    print(f"\n[4] PRUEBAS DE TUKEY HSD PARA FACTORES SIGNIFICATIVOS")
    for factor_label, col_df in factores_sig.items():
        # Aquí es donde realmente le damos la orden a Python de calcular
        imprimir_tukey_corregido(df, col_df, tabla_anova, alpha)
else:
    print("\n[4] PRUEBA DE TUKEY HSD")
    print("  >> Ningun factor resulto significativo. No se aplica Tukey.")

print("\n" + "=" * 60)
print("  ANALISIS COMPLETADO")
print("=" * 60)

