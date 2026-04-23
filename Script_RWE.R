################################################################################
##                                                                            ##
##        PRÁCTICA — REAL-WORLD EVIDENCE (RWE)                               ##
##        Propensity Score Matching, IPTW y Regresión Logística              ##
##                                                                            ##
##  CONTEXTO CLÍNICO                                                          ##
##  ─────────────────────────────────────────────────────────────────────    ##
##  963 mujeres perimenopáusicas:                                             ##
##    · Grupo 0 (Sanas):             758 mujeres sin patología crónica        ##
##    · Grupo 1 (Esclerosis Múltiple): 205 mujeres con EM diagnosticada      ##
##                                                                            ##
##  PREGUNTA DE INVESTIGACIÓN                                                 ##
##  ¿Tiene la esclerosis múltiple un impacto en la calidad de vida           ##
##  relacionada con la menopausia, más allá de los factores demográficos     ##
##  y de composición corporal que ya difieren entre grupos?                  ##
##                                                                            ##
##  OUTCOMES — Cuestionario Cervantes SF v2 (0–100)                          ##
##  ⚠ ATENCIÓN: puntuación MÁS ALTA = PEOR calidad de vida                  ##
##                                                                            ##
##  ESTRUCTURA DE LA PRÁCTICA                                                 ##
##    BLOQUE 0 · Preparación                                                  ##
##    BLOQUE 1 · Exploración y análisis sin ajuste                            ##
##    BLOQUE 2 · Propensity Score Matching (PSM)                              ##
##    BLOQUE 3 · Inverse Probability of Treatment Weighting (IPTW)            ##
##    BLOQUE 4 · Regresión Logística                                          ##
##    BLOQUE 5 · Comparación de métodos                                       ##
##    TAREA    · DAG causal (para entregar)                                   ##
##                                                                            ##
################################################################################


# ══════════════════════════════════════════════════════════════════════════════
#  BLOQUE 0 · INSTALACIÓN Y CARGA DE PAQUETES
# ══════════════════════════════════════════════════════════════════════════════
#
# Descomenta con Ctrl+Shift+C las líneas install.packages() si algún paquete
# no está instalado todavía.

# install.packages("haven")      # leer archivos SPSS (.sav)
# install.packages("dplyr")      # manipulación de datos: filter, mutate, group_by…
# install.packages("tableone")   # Tabla 1 con SMD automático
# install.packages("MatchIt")    # propensity score matching
# install.packages("cobalt")     # love plots para evaluar balance
# install.packages("WeightIt")   # IPTW y otros métodos de ponderación
# install.packages("survey")     # modelos ponderados (usado con IPTW)

library(haven)       # read_sav()
library(dplyr)       # %>%, mutate(), group_by(), summarise()
library(tableone)    # CreateTableOne()
library(MatchIt)     # matchit(), match.data()
library(cobalt)      # love.plot(), bal.tab()
library(WeightIt)    # weightit()
library(survey)      # svydesign(), svyglm()


# ══════════════════════════════════════════════════════════════════════════════
#  BLOQUE 1 · CARGA, EXPLORACIÓN Y ANÁLISIS SIN AJUSTE
# ══════════════════════════════════════════════════════════════════════════════


# ── 1.1 Carga de datos ────────────────────────────────────────────────────────

# read_sav() lee archivos de SPSS directamente desde R.
# Ajusta la ruta si el archivo está en otra carpeta.
base_PS <- read_sav("base_rwe.sav")


# ── 1.2 Exploración inicial ───────────────────────────────────────────────────

dim(base_PS)      # filas × columnas
names(base_PS)    # nombre de todas las variables
head(base_PS)     # primeras 6 filas
summary(base_PS)  # estadísticos descriptivos de cada variable


# ── 1.3 Preparación de variables ─────────────────────────────────────────────

# Convertimos 'grupo' (numérica) en factor con etiquetas legibles.
# Un factor es como una variable categórica nominal.
# levels: valores originales en los datos
# labels: etiquetas que queremos ver en los resultados

base_PS <- base_PS %>%
  mutate(
    GRUPO.f = factor(grupo, levels = c(0, 1), labels = c("Sanas", "Esclerosis"))
  )

# Verificamos cuántos sujetos hay en cada grupo
table(base_PS$GRUPO.f)
# Resultado: Sanas = 758, Esclerosis = 205


# ── 1.4 Tabla 1: estadísticas basales con SMD ────────────────────────────────
#
# La Tabla 1 es el estándar en artículos clínicos. Muestra cómo son los grupos
# ANTES de cualquier ajuste.
#
# El SMD (Standardized Mean Difference, Diferencia Estandarizada de Medias)
# mide si los grupos están equilibrados en cada variable:
#   SMD < 0.10  →  balance aceptable (los grupos son comparables)
#   SMD ≥ 0.10  →  desequilibrio: esa variable puede estar confundiendo

variables_tabla <- c("edad", "imc", "edad_ultimamenst",
                     "Domino_Menopausia_Salud", "Dominio_psiquico",
                     "Dominio_sexualidad", "Dominio_pareja", "SFv2global")

tab1_cruda <- CreateTableOne(
  vars   = variables_tabla,
  strata = "GRUPO.f",   # separa la tabla por grupo
  data   = base_PS,
  test   = TRUE         # añade p-valores de los contrastes
)
print(tab1_cruda, smd = TRUE)

# PREGUNTA 1: ¿Qué variables tienen SMD > 0.10?
#             ¿Qué problema supone eso para comparar los grupos directamente?


# ── 1.5 Contrastes estadísticos de las variables basales ─────────────────────
#
# t.test() compara las medias de dos grupos.
# Si p < 0.05, la diferencia es estadísticamente significativa.
# PERO con N grande casi todo es significativo aunque sea clínicamente irrelevante.
# Por eso preferimos el SMD para evaluar balance.

t.test(edad             ~ GRUPO.f, data = base_PS)
t.test(imc              ~ GRUPO.f, data = base_PS)
t.test(edad_ultimamenst ~ GRUPO.f, data = base_PS)


# ── 1.6 Efectos CRUDOS sobre cada outcome ────────────────────────────────────
#
# Regresión lineal simple: solo el grupo como predictor, SIN ajustar por nada.
# El coeficiente 'GRUPO.fEsclerosis' es la diferencia media en el outcome
# entre mujeres con EM y mujeres sanas.
#
# ⚠ Recuerda: mayor puntuación = PEOR calidad de vida.
# Si el coeficiente es positivo → el grupo EM tiene peor calidad de vida.

outcomes <- c("Domino_Menopausia_Salud", "Dominio_psiquico",
              "Dominio_sexualidad", "Dominio_pareja", "SFv2global")

cat("\n══ EFECTOS CRUDOS (sin ajuste) ══\n")
for (y in outcomes) {
  cat("\n▸", y, "\n")
  m <- lm(as.formula(paste(y, "~ GRUPO.f")), data = base_PS)
  coef_tabla <- cbind(
    Coeficiente = round(coef(m), 3),
    round(confint(m, level = 0.95), 3),
    p_valor = round(summary(m)$coef[, 4], 4)
  )
  print(coef_tabla["GRUPO.fEsclerosis", , drop = FALSE])
}

# PREGUNTA 2: ¿En qué outcomes hay diferencia significativa?
#             ¿Puedes concluir que la EM CAUSA peor calidad de vida? ¿Por qué?


# ══════════════════════════════════════════════════════════════════════════════
#  BLOQUE 2 · PROPENSITY SCORE MATCHING (PSM)
# ══════════════════════════════════════════════════════════════════════════════
#
# ¿Qué es el Propensity Score (PS)?
#   PS = P(pertenecer al grupo EM | edad, IMC, edad_ultimamenst)
#   Es la probabilidad estimada de tener EM dado el perfil clínico de cada mujer.
#   Resume todas las covariables en un único número.
#
# ¿Qué hace el PS Matching?
#   Empareja cada mujer con EM con la mujer sana que tenga el PS más parecido.
#   Resultado: dos grupos comparables en sus características basales,
#   como si hubieran sido aleatorizados.
#
# ESTRUCTURA EN DOS ETAPAS (muy importante):
#   ETAPA 1 – DISEÑO:      crear los pares y verificar el balance (SMD < 0.10)
#   ETAPA 2 – ESTIMACIÓN:  analizar los outcomes SOLO si el balance es aceptable
#
#  NUNCA mires los outcomes hasta haber verificado el balance.


# ── ETAPA 1: DISEÑO ───────────────────────────────────────────────────────────

match_out <- matchit(
  formula  = GRUPO.f ~ edad + imc + edad_ultimamenst,
  # fórmula: variable de grupo ~ confusores que queremos equilibrar
  data     = base_PS,
  method   = "nearest",   # vecino más próximo en PS
  distance = "glm",       # PS estimado con regresión logística
  ratio    = 1,           # 1 control por cada caso (matching 1:1)
  replace  = FALSE        # sin reemplazamiento: cada control se usa solo una vez
)

# Resumen del matching: muestra el balance antes y después
summary(match_out)
# Fíjate en la columna "Std. Mean Diff." antes y después del matching.
# Queremos que TODAS las variables queden con |SMD| < 0.10 post-matching.


# ── Gráficos de evaluación del balance ────────────────────────────────────────

# Gráfico 1: Jitter plot — distribución del PS en cada grupo
# Los puntos deberían solaparse bien después del matching
plot(match_out, type = "jitter", interactive = FALSE)

# Gráfico 2: Love plot — SMD de cada variable antes y después del matching
# ✓ Puntos VERDES (post-matching) deben estar a la IZQUIERDA de la línea roja (0.10)
# ✗ Puntos ROJOS (pre-matching) pueden estar a la derecha — eso es el problema inicial
love.plot(match_out,
          threshold = 0.10,   # línea roja en SMD = 0.10
          abs       = TRUE,   # mostrar el valor absoluto del SMD
          title     = "Balance: Propensity Score Matching 1:1")

# Gráfico 3: Densidades de las covariables antes y después
plot(match_out, type = "density", interactive = FALSE,
     which.xs = ~ edad + imc + edad_ultimamenst)

# PREGUNTA 3: ¿Quedan todas las covariables con SMD < 0.10 después del matching?
#               ¿Qué aspecto tienen las distribuciones antes vs. después?


# ── Extraer la muestra emparejada ─────────────────────────────────────────────

# match.data() crea un dataframe solo con los pares formados
# La columna 'distance' contiene el PS de cada sujeto
# La columna 'subclass' indica a qué par pertenece cada sujeto
base_matched <- match.data(match_out)

cat("\n══ Tamaño de la muestra emparejada ══\n")
table(base_matched$GRUPO.f)
# Resultado esperado: 205 Esclerosis + 205 Sanas = 410 observaciones

# Tabla 1 POST-matching para verificar el balance numéricamente
tab1_matched <- CreateTableOne(
  vars   = c("edad", "imc", "edad_ultimamenst"),
  strata = "GRUPO.f",
  data   = base_matched,
  test   = TRUE
)
print(tab1_matched, smd = TRUE)

# PREGUNTA 4: ¿Cuántos sujetos del grupo control fueron descartados?
#             ¿Es eso un problema? ¿Qué se gana y qué se pierde?


# ── ETAPA 2: ESTIMACIÓN DEL EFECTO ───────────────────────────────────────────
#
# Solo llegamos aquí si el balance es aceptable (SMD < 0.10 en todas las variables).
# Ajustamos la regresión sobre la muestra emparejada.
# Incluimos las covariables de matching en el modelo como doble ajuste
# (reduce algo de varianza residual y mejora la precisión de los IC).

cat("\n══ EFECTOS TRAS EL MATCHING (PSM) ══\n")
for (y in outcomes) {
  cat("\n▸", y, "\n")
  m <- lm(as.formula(paste(y, "~ GRUPO.f + edad + imc + edad_ultimamenst")),
          data = base_matched)
  coef_tabla <- cbind(
    Coeficiente = round(coef(m), 3),
    round(confint(m, level = 0.95), 3),
    p_valor = round(summary(m)$coef[, 4], 4)
  )
  print(coef_tabla["GRUPO.fEsclerosis", , drop = FALSE])
}

# PREGUNTA 5: Compara los efectos crudos (Bloque 1) con los de post-matching.
#             ¿Cambia el tamaño o la significación del efecto? ¿Por qué?


# ══════════════════════════════════════════════════════════════════════════════
#  BLOQUE 3 · IPTW — Inverse Probability of Treatment Weighting
# ══════════════════════════════════════════════════════════════════════════════
#
# ¿Qué es el IPTW?
#   Es una alternativa al matching que NO descarta observaciones.
#   En lugar de crear pares, PONDERA cada observación por la inversa
#   de su probabilidad de pertenecer al grupo que realmente pertenece.
#
#   Peso para un caso (EM):       w = 1 / PS
#   Peso para un control (Sana):  w = 1 / (1 – PS)
#
# ¿Para qué sirve esto?
#   - Un caso con PS bajo (poca probabilidad de ser EM) recibe peso ALTO:
#     es "inesperado" en ese grupo, y se le da más importancia.
#   - Un control con PS alto (mucha probabilidad de ser EM) también recibe
#     peso alto: es el tipo de control más útil para la comparación.
#   - El resultado es una pseudopoblación donde los grupos están equilibrados.
#
# VENTAJA sobre PSM: usa todos los datos (N=963 vs N=410 en el matching).
# RIESGO: pesos extremos si algunos sujetos tienen PS muy cercano a 0 o a 1.
#         Por eso se recomienda revisar la distribución de pesos y truncar si es necesario.


# ── 3.1 Estimación del IPTW con WeightIt ─────────────────────────────────────

# weightit() estima el PS y calcula los pesos automáticamente
iptw_out <- weightit(
  formula = GRUPO.f ~ edad + imc + edad_ultimamenst,
  # fórmula: grupo ~ confusores (igual que en el matching)
  data    = base_PS,
  method  = "ps",         # método: propensity score (regresión logística)
  estimand = "ATE"        # ATE: Average Treatment Effect
  # ATE = efecto medio si tratáramos a TODOS vs. a NADIE (población completa)
  # ATT = efecto medio solo en los que ya recibieron el tratamiento
  # Usamos ATE porque queremos generalizar a toda la población perimenopáusica
)

# Resumen de los pesos: media, rango, y balance obtenido
summary(iptw_out)


# ── 3.2 Diagnóstico: distribución de los pesos ───────────────────────────────
#
# Un peso muy elevado (>10-20) indica que esa persona es muy "inesperada"
# en su grupo. Pesos extremos pueden hacer inestable la estimación.
# Comprobamos el rango y la distribución.

pesos <- iptw_out$weights

cat("\n══ Distribución de los pesos IPTW ══\n")
cat("Mínimo:   ", round(min(pesos), 3), "\n")
cat("Máximo:   ", round(max(pesos), 3), "\n")
cat("Media:    ", round(mean(pesos), 3), "\n")
cat("Mediana:  ", round(median(pesos), 3), "\n")

# Histograma de los pesos
hist(pesos,
     breaks = 50,
     col    = "#1A9770",
     main   = "Distribución de pesos IPTW",
     xlab   = "Peso IPTW",
     ylab   = "Frecuencia")

# Si hay pesos muy extremos (>20), es recomendable truncar:
# base_PS$peso_trunc <- pmin(base_PS$peso, cuantil_99)

# PREGUNTA 6: ¿Hay pesos extremos? ¿Qué implica un peso muy alto?


# ── 3.3 Evaluación del balance con IPTW ──────────────────────────────────────

# Love plot ponderado: ¿quedaron equilibradas las covariables tras aplicar IPTW?
love.plot(iptw_out,
          threshold = 0.10,
          abs       = TRUE,
          title     = "Balance: IPTW (ATE)")

# Tabla de balance numérico
bal.tab(iptw_out, thresholds = c(m = 0.10))

# PREGUNTA 7: Compara el balance del IPTW con el del PSM.
#               ¿Cuál logra mejor balance? ¿Cuál retiene más sujetos?


# ── 3.4 Estimación del efecto con IPTW ───────────────────────────────────────
#
# Para analizar los datos ponderados usamos el paquete 'survey'.
# svydesign() crea un "diseño de encuesta" que incorpora los pesos.
# svyglm() ajusta modelos lineales con esos pesos.

# Añadimos los pesos al dataframe original
base_PS$peso_iptw <- iptw_out$weights

# Creamos el diseño ponderado
diseno_iptw <- svydesign(
  ids     = ~1,              # ~1 = no hay clusters (diseño simple)
  weights = ~peso_iptw,      # columna con los pesos IPTW
  data    = base_PS
)

cat("\n══ EFECTOS ESTIMADOS CON IPTW ══\n")
for (y in outcomes) {
  cat("\n▸", y, "\n")
  m <- svyglm(
    formula = as.formula(paste(y, "~ GRUPO.f")),
    design  = diseno_iptw,   # modelo lineal ponderado por IPTW
    family  = gaussian()     # distribución gaussian = regresión lineal
  )
  coef_tabla <- cbind(
    Coeficiente = round(coef(m), 3),
    round(confint(m, level = 0.95), 3),
    p_valor = round(summary(m)$coef[, 4], 4)
  )
  print(coef_tabla["GRUPO.fEsclerosis", , drop = FALSE])
}

# PREGUNTA 8: ¿Son coherentes los resultados del IPTW con los del PSM?
#               ¿Qué método preferirías y por qué?


# ══════════════════════════════════════════════════════════════════════════════
#  BLOQUE 4 · REGRESIÓN LOGÍSTICA
# ══════════════════════════════════════════════════════════════════════════════
#
# ¿Cuándo usar regresión logística?
#   Cuando el outcome es BINARIO (sí/no, evento/no evento).
#   En nuestro caso los outcomes son continuos (0–100), así que los
#   dicotomizaremos en "calidad de vida ALTA" vs "BAJA" usando la mediana
#   como punto de corte.
#
# ¿Qué mide la regresión logística?
#   El Odds Ratio (OR):
#     OR = odds de evento en expuestos / odds de evento en no expuestos
#     OR = 1  → sin efecto
#     OR > 1  → el grupo EM tiene mayor odds de mala calidad de vida
#     OR < 1  → el grupo EM tiene menor odds de mala calidad de vida
#
# Estructura del modelo:
#   logit(P(evento)) = β₀ + β₁·grupo + β₂·edad + β₃·imc + β₄·edad_menst
#   OR = exp(β₁)


# ── 4.1 Dicotomizar los outcomes ─────────────────────────────────────────────
#
# Usamos la mediana de cada outcome en la muestra completa como punto de corte.
# sfv2_alto = 1 si la puntuación está POR ENCIMA de la mediana (peor calidad de vida)
# sfv2_alto = 0 si está por debajo o igual (mejor calidad de vida)

base_PS <- base_PS %>%
  mutate(
    men_salud_alto = as.integer(Domino_Menopausia_Salud > median(Domino_Menopausia_Salud)),
    psiquico_alto  = as.integer(Dominio_psiquico        > median(Dominio_psiquico)),
    sexual_alto    = as.integer(Dominio_sexualidad       > median(Dominio_sexualidad)),
    pareja_alto    = as.integer(Dominio_pareja           > median(Dominio_pareja)),
    sfv2_alto      = as.integer(SFv2global               > median(SFv2global))
  )

# Comprobamos la distribución de los nuevos outcomes binarios
cat("\n══ Prevalencia de 'alta carga' por grupo ══\n")
outcomes_bin <- c("men_salud_alto", "psiquico_alto", "sexual_alto",
                  "pareja_alto", "sfv2_alto")
for (y in outcomes_bin) {
  cat("\n▸", y, "\n")
  print(prop.table(table(base_PS$GRUPO.f, base_PS[[y]]), margin = 1))
}

# PREGUNTA 9: ¿En qué outcomes hay mayor proporción de 'alta carga' en el grupo EM?


# ── 4.2 Modelo logístico CRUDO ────────────────────────────────────────────────
#
# glm() con family = binomial(link = "logit") ajusta una regresión logística.
# Primero sin ajustar por confusores (efecto crudo).

cat("\n══ ODDS RATIOS CRUDOS ══\n")
for (y in outcomes_bin) {
  cat("\n▸", y, "\n")
  m <- glm(as.formula(paste(y, "~ GRUPO.f")),
           data   = base_PS,
           family = binomial(link = "logit"))
  # exp(coef) transforma el coeficiente logístico en OR
  OR_tabla <- cbind(
    OR      = round(exp(coef(m)), 3),
    round(exp(confint(m, level = 0.95)), 3),
    p_valor = round(summary(m)$coef[, 4], 4)
  )
  print(OR_tabla["GRUPO.fEsclerosis", , drop = FALSE])
}


# ── 4.3 Modelo logístico AJUSTADO ─────────────────────────────────────────────
#
# Añadimos los confusores (edad, IMC, edad_ultimamenst) al modelo.
# El OR de GRUPO.fEsclerosis ahora representa el efecto del grupo
# "manteniendo constantes" los confusores: es el OR AJUSTADO.

cat("\n══ ODDS RATIOS AJUSTADOS ══\n")
for (y in outcomes_bin) {
  cat("\n▸", y, "\n")
  m <- glm(as.formula(paste(y, "~ GRUPO.f + edad + imc + edad_ultimamenst")),
           data   = base_PS,
           family = binomial(link = "logit"))
  OR_tabla <- cbind(
    OR      = round(exp(coef(m)), 3),
    round(exp(confint(m, level = 0.95)), 3),
    p_valor = round(summary(m)$coef[, 4], 4)
  )
  print(OR_tabla["GRUPO.fEsclerosis", , drop = FALSE])
}

# PREGUNTA 10: ¿Cambia el OR al ajustar por los confusores?
#                ¿En qué dirección y por qué?


# ── 4.4 Análisis detallado del modelo para SFv2global ────────────────────────
#
# Hacemos el análisis completo (diagnóstico, supuestos, forest plot)
# sobre el outcome global para ilustrar el flujo de trabajo completo.

# Modelo completo para SFv2global
modelo_sfv2 <- glm(sfv2_alto ~ GRUPO.f + edad + imc + edad_ultimamenst,
                   data   = base_PS,
                   family = binomial(link = "logit"))

# Resumen completo del modelo
summary(modelo_sfv2)

# OR con IC 95% por perfil de verosimilitud (más preciso que el IC de Wald)
exp(cbind(OR = coef(modelo_sfv2), confint(modelo_sfv2)))


# ── 4.5 Diagnóstico del modelo logístico ─────────────────────────────────────
#
# Un modelo logístico válido debe cumplir varios supuestos.
# Comprobamos los más importantes:

# install.packages("ResourceSelection")   # para el test de Hosmer-Lemeshow
# install.packages("pROC")                # para la curva ROC y AUC
library(ResourceSelection)
library(pROC)

# DISCRIMINACIÓN — Curva ROC y AUC
# ¿Distingue bien el modelo entre las que tienen mala/buena calidad de vida?
# AUC = 0.5  → sin capacidad discriminatoria (como lanzar una moneda)
# AUC = 0.7–0.8 → aceptable
# AUC > 0.8  → buena

probs_pred <- predict(modelo_sfv2, type = "response")
# type = "response" devuelve probabilidades predichas (P), no logits

curva_roc <- roc(base_PS$sfv2_alto, probs_pred)
plot(curva_roc,
     main = paste("Curva ROC — AUC =", round(auc(curva_roc), 3)),
     col  = "#1A9770",
     lwd  = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")   # línea de azar (AUC = 0.5)

cat("\nAUC:", round(auc(curva_roc), 3), "\n")

# CALIBRACIÓN — Test de Hosmer-Lemeshow
# ¿Las probabilidades predichas coinciden con las observadas?
# H₀: el modelo está bien calibrado
# Si p > 0.05 → no se rechaza H₀ → buena calibración
hl_test <- hoslem.test(base_PS$sfv2_alto, probs_pred, g = 10)
print(hl_test)

# PREGUNTA 11: ¿Es el AUC aceptable? ¿Está el modelo bien calibrado?


# ── 4.6 Regresión logística sobre la muestra emparejada (post-PSM) ───────────
#
# Para integrar PSM y logística: ajustamos el modelo logístico
# sobre la muestra emparejada (base_matched) en vez de la muestra completa.

# Creamos el outcome binario también en la muestra emparejada
base_matched <- base_matched %>%
  mutate(
    sfv2_alto = as.integer(SFv2global > median(base_PS$SFv2global))
    # usamos la mediana de la muestra COMPLETA como referencia, no la del subconjunto
  )

cat("\n══ ODDS RATIO LOGÍSTICA POST-PSM (sfv2_alto) ══\n")
m_log_psm <- glm(sfv2_alto ~ GRUPO.f + edad + imc + edad_ultimamenst,
                 data   = base_matched,
                 family = binomial(link = "logit"))
exp(cbind(OR = coef(m_log_psm), confint(m_log_psm)))

# PREGUNTA 12: ¿Es distinto el OR obtenido en la muestra emparejada
#                respecto al obtenido en la muestra completa ajustada?


# ══════════════════════════════════════════════════════════════════════════════
#  BLOQUE 5 · TABLA COMPARATIVA DE MÉTODOS
# ══════════════════════════════════════════════════════════════════════════════
#
# Reunimos los resultados de los tres métodos para el outcome SFv2global.
# La coherencia entre métodos refuerza la solidez de las conclusiones.

cat("\n")
cat("══════════════════════════════════════════════════════\n")
cat("  RESUMEN COMPARATIVO — Outcome: SFv2global          \n")
cat("══════════════════════════════════════════════════════\n")

# ── Diferencia de medias ──────────────────────────────────────────────────────

# Crudo
m_cru <- lm(SFv2global ~ GRUPO.f, data = base_PS)
c_cru <- coef(m_cru)["GRUPO.fEsclerosis"]
i_cru <- confint(m_cru)["GRUPO.fEsclerosis", ]

# Post-PSM
m_psm <- lm(SFv2global ~ GRUPO.f + edad + imc + edad_ultimamenst, data = base_matched)
c_psm <- coef(m_psm)["GRUPO.fEsclerosis"]
i_psm <- confint(m_psm)["GRUPO.fEsclerosis", ]

# Post-IPTW
m_iptw <- svyglm(SFv2global ~ GRUPO.f, design = diseno_iptw, family = gaussian())
c_iptw <- coef(m_iptw)["GRUPO.fEsclerosis"]
i_iptw <- confint(m_iptw)["GRUPO.fEsclerosis", ]

cat("\n── Diferencia de medias en SFv2global (Esclerosis – Sanas) ──\n")
cat(sprintf("Crudo:     %+.2f  (IC95%%: %.2f  a %.2f)\n", c_cru, i_cru[1], i_cru[2]))
cat(sprintf("PSM:       %+.2f  (IC95%%: %.2f  a %.2f)\n", c_psm, i_psm[1], i_psm[2]))
cat(sprintf("IPTW:      %+.2f  (IC95%%: %.2f  a %.2f)\n", c_iptw, i_iptw[1], i_iptw[2]))

# ── Odds Ratios ───────────────────────────────────────────────────────────────

# OR crudo
m_or_cru <- glm(sfv2_alto ~ GRUPO.f, data = base_PS, family = binomial())
or_cru   <- exp(coef(m_or_cru)["GRUPO.fEsclerosis"])
ic_or_cru <- exp(confint(m_or_cru)["GRUPO.fEsclerosis", ])

# OR ajustado (muestra completa)
m_or_adj <- glm(sfv2_alto ~ GRUPO.f + edad + imc + edad_ultimamenst,
                data = base_PS, family = binomial())
or_adj   <- exp(coef(m_or_adj)["GRUPO.fEsclerosis"])
ic_or_adj <- exp(confint(m_or_adj)["GRUPO.fEsclerosis", ])

# OR logística post-PSM
or_psm   <- exp(coef(m_log_psm)["GRUPO.fEsclerosis"])
ic_or_psm <- exp(confint(m_log_psm)["GRUPO.fEsclerosis", ])

cat("\n── Odds Ratios para sfv2_alto (puntuación > mediana = peor calidad) ──\n")
cat(sprintf("OR crudo:          %.3f (IC95%%: %.3f – %.3f)\n", or_cru, ic_or_cru[1], ic_or_cru[2]))
cat(sprintf("OR ajustado (N=963): %.3f (IC95%%: %.3f – %.3f)\n", or_adj, ic_or_adj[1], ic_or_adj[2]))
cat(sprintf("OR logística PSM:    %.3f (IC95%%: %.3f – %.3f)\n", or_psm, ic_or_psm[1], ic_or_psm[2]))

# ❓ PREGUNTA 13: ¿Qué observas al comparar los tres métodos?
#                ¿La consistencia de resultados aumenta o disminuye tu confianza
#                en que hay un efecto real de la EM sobre la calidad de vida?


# ══════════════════════════════════════════════════════════════════════════════
#  TAREA — DAG CAUSAL (para entregar)
# ══════════════════════════════════════════════════════════════════════════════
#
# El paquete dagitty permite construir y analizar DAGs en R.
# Un DAG (Directed Acyclic Graph) es un grafo causal donde:
#   - Cada nodo representa una variable
#   - Cada flecha (→) representa una relación causal
#   - No hay ciclos (una variable no puede ser causa de sí misma)
#
# Los DAGs nos permiten identificar:
#   CONFUSOR:   variable que causa tanto la exposición como el outcome (ajustar)
#   MEDIADOR:   variable en el camino causal X → M → Y (NO ajustar si queremos efecto total)
#   COLISIONADOR: variable causada por la exposición Y el outcome (NUNCA ajustar)

# install.packages("dagitty")
library(dagitty)

# ── Tarea: construye y analiza el DAG de este estudio ─────────────────────────
#
# A continuación tienes el DAG base con los elementos que conocemos.
# Tu tarea es:
#
#   1. Añadir al DAG al menos DOS confusores NO medidos en nuestro dataset
#      que podrían sesgar los resultados (piensa en qué variables clínicas
#      podrían influir tanto en tener EM como en la calidad de vida).
#
#   2. Discutir si alguna variable podría ser un MEDIADOR en lugar de un confusor.
#
#   3. Identificar si hay algún posible COLISIONADOR.
#
#   4. Usar adjustmentSets() para verificar qué variables hay que ajustar.

dag_estudio <- dagitty('dag {
  EM              [exposure, pos="0,1"]
  CalidadVida     [outcome,  pos="2,1"]
  edad            [pos="1,0"]
  imc             [pos="1,2"]
  edad_menst      [pos="0.5,2"]

  edad       -> EM
  edad       -> CalidadVida
  imc        -> EM
  imc        -> CalidadVida
  edad_menst -> EM
  edad_menst -> CalidadVida
  EM         -> CalidadVida
}')

# Visualizamos el DAG
plot(dag_estudio)

# ¿Qué variables hay que ajustar para estimar el efecto causal de EM → CalidadVida?
adjustmentSets(dag_estudio, exposure = "EM", outcome = "CalidadVida")

# Comprobamos los caminos causales abiertos y bloqueados
paths(dag_estudio, from = "EM", to = "CalidadVida")

# ──────────────────────────────────────────────────────────────────────────────
# PREGUNTAS A RESPONDER EN LA ENTREGA DEL DAG:
#
#   A. ¿Cuál es el adjustment set mínimo que devuelve adjustmentSets()?
#      ¿Coincide con las variables que usamos en el PSM y en la logística?
#
#   B. Añade al DAG al menos 2 confusores no medidos. ¿Cómo cambia
#      el adjustment set? ¿Qué implica para la validez de nuestros resultados?
#
#   C. ¿Podría la 'discapacidad funcional' (producida por la EM) ser un
#      mediador entre EM y CalidadVida? ¿Qué ocurriría si la incluyéramos
#      como covariable en el modelo? ¿Ajustaríamos por ella?
#
#   D. El 'acceso a tratamiento farmacológico' podría estar causado tanto
#      por la EM como por el nivel socioeconómico, que a su vez afecta a
#      CalidadVida. Dibuja ese fragmento del DAG. ¿Es un confusor o un colisionador?
# ──────────────────────────────────────────────────────────────────────────────


# ══════════════════════════════════════════════════════════════════════════════
#  FIN DEL SCRIPT
# ══════════════════════════════════════════════════════════════════════════════
