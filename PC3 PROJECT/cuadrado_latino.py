import sys
import pandas as pd
import numpy as np
from statsmodels.formula.api import ols
from statsmodels.stats.anova import anova_lm
from statsmodels.stats.multicomp import pairwise_tukeyhsd
import warnings
warnings.filterwarnings("ignore")
import time

a = input("COMPLETO O INCOMPLETO")

def hallar_dato():
    #
    #
    #
    return dato

if a == "INCOMPLETO":
    print("FALTA HALLAR EL DATO")

    #Poniendo el dato en el df
    dato = hallar_dato()
    df.loc[df['Tratamiento'].isnull(), 'Tratamiento'] = dato

else:
    pass


    


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

print("=" * 60)
print("  DISENO DE CUADRADO LATINO - ANALISIS ESTADISTICO")
print("=" * 60)

# ---------------------------------------------------------
# 2. RESUMEN DESCRIPTIVO
# ---------------------------------------------------------
print("\n[1] DATOS LEIDOS DEL EXCEL")
print(df.to_string(index=False))

print("\n[2] ESTADISTICOS DESCRIPTIVOS POR TRATAMIENTO")
desc = df.groupby("Tratamiento")["Respuesta"].agg(
    N="count", Media="mean", DE="std", Min="min", Max="max"
).round(4)
print(desc.to_string())

# ---------------------------------------------------------
# 3. ANOVA CUADRADO LATINO
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

p_tratamiento = tabla_anova.loc["Tratamientos", "PR(>F)"]
alpha = 0.05

print(f"\n  >> p-valor Tratamientos: {p_tratamiento:.4f}")
if p_tratamiento < alpha:
    print(f"  >> Conclusion: Existen diferencias significativas entre tratamientos (p < {alpha})")
else:
    print(f"  >> Conclusion: No hay diferencias significativas entre tratamientos (p >= {alpha})")

# ---------------------------------------------------------
# 4. PRUEBA DE TUKEY
# ---------------------------------------------------------
print("\n[4] PRUEBA DE TUKEY HSD (alpha = 0.05)")
print("-" * 60)

tukey = pairwise_tukeyhsd(
    endog=df["Respuesta"],
    groups=df["Tratamiento"],
    alpha=0.05
)
print(tukey.summary())

print("\n  Resumen de comparaciones:")
resultado = pd.DataFrame(
    data=tukey._results_table.data[1:],
    columns=tukey._results_table.data[0]
)
resultado.columns = ["Grupo 1", "Grupo 2", "Meandiff", "p-adj", "Lower", "Upper", "Rechazar?"]
resultado["Rechazar?"] = resultado["Rechazar?"].map({True: "Si (diferencia sig.)", False: "No"})
print(resultado.to_string(index=False))

# ---------------------------------------------------------
# 5. RANKING DE MEDIAS
# ---------------------------------------------------------
print("\n[5] MEDIAS POR TRATAMIENTO (mayor a menor)")
print("-" * 60)
medias = df.groupby("Tratamiento")["Respuesta"].mean().sort_values(ascending=False)
for trat, media in medias.items():
    print(f"  {trat}: {media:.4f}")

print("\n" + "=" * 60)
print("  ANALISIS COMPLETADO")
print("=" * 60)
