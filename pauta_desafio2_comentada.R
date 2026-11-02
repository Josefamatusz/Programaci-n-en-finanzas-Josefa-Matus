# =============================================================================
# Programación en Finanzas (FIN443) - UAI 2026
# PAUTA COMENTADA DEL DESAFÍO 2 (publicada en la Clase 3)
# -----------------------------------------------------------------------------
# Este es el programa con el que se corrigió el desafío 2: con él se generaron las
# cifras "correctas" para el archivo público y para los tres archivos ocultos.
#
# Cómo usarla:
#   1. Ejecútela con precios_desafio2.csv y compare, cifra por cifra, con su salida.csv.
#   2. Si difieren, busque en qué PASO se separan (lea los comentarios "TRAMPA").
#   3. Corrija SU programa: no lo reemplace por este. Su desafío 3 se construye sobre
#      su pieza 1, y en el proyecto final deberá poder explicar cada línea.
#
# Se usa igual que su desafío:   Rscript pauta_desafio2_comentada.R datos.csv salida.csv [rf]
# En RStudio: abra el archivo y haga Source (usa precios_desafio2.csv de esta carpeta).
# Solo usa R base (sin paquetes): así corre en cualquier computador.
# =============================================================================

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}
args    <- commandArgs(trailingOnly = TRUE)
entrada <- if (length(args) >= 1) args[1] else "precios_desafio2.csv"
salida  <- if (length(args) >= 2) args[2] else "salida.csv"
rf      <- if (length(args) >= 3) as.numeric(args[3]) else 0.05
# TRAMPA (capa C4): rf llega como TEXTO desde la terminal ("0.03"); sin as.numeric() falla.
# Y si se escribe rf <- 0.05 fijo, el programa ignora el rf que se le pasa (oculto 2 usaba 3%).

# ---- 1. Leer el archivo, sea cual sea su formato ----------------------------
# No sabemos de antemano si es chileno (; y ,) o internacional (, y .). Se decide mirando
# la PRIMERA línea: si tiene más ";" que ",", es chileno.
# TRAMPA: no use la extensión ni el nombre del archivo; los ocultos se llamaban oculto_1.csv, etc.
leer_precios <- function(archivo) {
  l1 <- readLines(archivo, n = 1, warn = FALSE, encoding = "UTF-8")   # UTF-8: nombres con tildes (SALMÓN)
  n_pyc <- lengths(regmatches(l1, gregexpr(";", l1)))
  n_com <- lengths(regmatches(l1, gregexpr(",", l1)))
  sep <- if (n_pyc > n_com) ";" else ","
  dec <- if (sep == ";") "," else "."
  d <- read.table(archivo, sep = sep, dec = dec, header = TRUE, na.strings = c("", "NA"),
                  stringsAsFactors = FALSE,
                  check.names = FALSE,     # TRAMPA: sin esto, "BANCO SUR" pasa a "BANCO.SUR" (oculto 3)
                  strip.white = TRUE, quote = "\"")
  names(d)[1] <- "Fecha"                   # la primera columna es la fecha, se llame como se llame

  # Fechas: se prueban los formatos posibles hasta que TODAS se lean sin NA.
  f <- NULL
  for (fmt in c("%d-%m-%Y", "%Y-%m-%d", "%d/%m/%Y")) {
    f <- as.Date(d$Fecha, format = fmt)
    if (!anyNA(f)) break
  }
  if (anyNA(f)) stop("Formato de fecha no reconocido")   # mejor detenerse que calcular con fechas malas
  d$Fecha <- f
  for (j in 2:ncol(d)) d[[j]] <- as.numeric(d[[j]])

  d <- d[order(d$Fecha), ]               # TRAMPA: oculto 1 venía descendente y oculto 2 desordenado
  d <- d[!duplicated(d$Fecha), ]         # fechas repetidas: se conserva la primera
  rownames(d) <- NULL
  d
}

# ---- 2. Ajustar splits -------------------------------------------------------
# Regla del enunciado: si P_t / P_anterior ≈ 1/k (k = 2..5, margen ±5%), es un split k:1
# y se dividen por k TODOS los precios anteriores.
# Se recorre de la fecha más antigua a la más reciente, comparando con el último precio
# DISPONIBLE (no con el de ayer, que puede ser NA).
# TRAMPA: un activo puede tener dos splits (SALMÓN en oculto 3): el bucle sigue después del primero.
# TRAMPA: ajustar ANTES de rellenar los NA; si no, el NA rellenado "esconde" el día del salto.
ajustar_splits <- function(p, tol = 0.05, ks = 2:5) {
  ult <- NA
  for (i in seq_along(p)) {
    if (is.na(p[i])) next
    if (!is.na(ult)) {
      for (k in ks) if (abs(p[i] / ult * k - 1) <= tol) {
        p[1:(i - 1)] <- p[1:(i - 1)] / k     # los precios anteriores quedan en la "escala nueva"
        break
      }
    }
    ult <- p[i]
  }
  p
}
# Verificación sugerida: calcule el retorno del día del split antes y después de ajustar
# (en el reto de la Clase 1: −48% sin ajustar y +3% ajustado).

# ---- 3. Rellenar faltantes con el último precio disponible (LOCF) -----------
# Los NA al inicio (activo que todavía no transaba) quedan NA: no hay precio anterior.
locf <- function(x) { for (i in seq_along(x)[-1]) if (is.na(x[i])) x[i] <- x[i - 1]; x }

# ---- 4. Métricas -------------------------------------------------------------
metricas_desafio2 <- function(archivo, rf = 0.05) {
  d <- leer_precios(archivo)
  act <- names(d)[-1]                                   # TRAMPA: no escriba los nombres a mano
  P <- sapply(act, function(a) locf(ajustar_splits(d[[a]])))
  P <- matrix(P, ncol = length(act), dimnames = list(NULL, act))
  R <- P[-1, , drop = FALSE] / P[-nrow(P), , drop = FALSE] - 1   # retorno simple, no logarítmico

  una <- function(r, p, fechas) {
    i0 <- which(!is.na(p))[1]                 # TRAMPA: SOLAR empezaba en 2023. P0 es su PRIMER precio,
    iT <- tail(which(!is.na(p)), 1)           # y los años se cuentan desde ESA fecha, no desde el inicio del archivo
    anos <- as.numeric(fechas[iT] - fechas[i0]) / 365.25   # días calendario / 365,25 (no n.º de filas / 252)
    cagr <- (p[iT] / p[i0])^(1 / anos) - 1
    vol <- sd(r, na.rm = TRUE) * sqrt(252)                 # desviación MUESTRAL (n−1), anualizada con √252
    c(cagr = cagr, vol = vol, sharpe = (mean(r, na.rm = TRUE) * 252 - rf) / vol)
  }
  out <- t(sapply(act, function(a) una(R[, a], P[, a], d$Fecha)))

  # Cartera equiponderada con rebalanceo diario: cada día, el promedio de los retornos de los
  # activos que tienen dato ese día (na.rm = TRUE). No es el promedio de los CAGR de los activos.
  rp <- rowMeans(R, na.rm = TRUE)
  anos <- as.numeric(diff(range(d$Fecha))) / 365.25
  vol_p <- sd(rp) * sqrt(252)
  cart <- c(cagr = prod(1 + rp)^(1 / anos) - 1, vol = vol_p, sharpe = (mean(rp) * 252 - rf) / vol_p)

  data.frame(activo = c(act, "CARTERA_EW"), rbind(out, cart), row.names = NULL)
}

# ---- 5. Escribir la salida en el formato del contrato ------------------------
# Coma como separador y punto decimal (write.csv), cifras en FRACCIÓN (0.0462, no 4.62).
# TRAMPA: write.csv2 o multiplicar por 100 rompe el contrato aunque las cifras "estén bien".
resultado <- metricas_desafio2(entrada, rf)
write.csv(resultado, salida, row.names = FALSE)
print(resultado)
