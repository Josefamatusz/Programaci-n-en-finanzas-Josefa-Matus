# =============================================================================
# Programación en Finanzas (FIN443) - UAI 2026 - Desafío 2
# Nombre y apellido: _____Josefa Matus______
# -----------------------------------------------------------------------------
# CÓMO SE USA
#   - Usted lo prueba en RStudio: abra este archivo y haga Source (botón arriba a la
#     derecha). Lee precios_desafio2.csv de esta misma carpeta y crea salida.csv.
#   - El profesor lo ejecutará automáticamente, sin abrirlo, con OTROS archivos:
#         Rscript desafio2.R datos.csv salida.csv [rf]
#     Por eso no cambie las líneas de "args", no pida nada por teclado y no use
#     rutas fijas de su computador (C:/Users/...).
#   - salida.csv debe tener las columnas activo,cagr,vol,sharpe (vea el enunciado).
# =============================================================================

# Si se ejecuta en RStudio, se ubica en la carpeta de este archivo
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

args    <- commandArgs(trailingOnly = TRUE)
entrada <- if (length(args) >= 1) args[1] else "precios_desafio2.csv"   # para probar en RStudio
salida  <- if (length(args) >= 2) args[2] else "salida.csv"
rf      <- if (length(args) >= 3) as.numeric(args[3]) else 0.05
# rf entregado con coma decimal ("0,05") o inválido: se corrige / se usa 0.05
if (length(args) >= 3) rf <- suppressWarnings(as.numeric(gsub(",", ".", args[3], fixed = TRUE)))
if (is.na(rf)) rf <- 0.05

# ---- 1. Leer el archivo -----------------------------------------------------
# Solo R base. Se detecta automáticamente: codificación, separador (";" o ","),
# coma/punto decimal y formato de fecha (aaaa-mm-dd o dd-mm-aaaa).

l0 <- readLines(entrada, warn = FALSE)
l0 <- l0[nzchar(trimws(l0))]
enc_utf8 <- all(validUTF8(l0))
cab <- sub("^\xef\xbb\xbf", "", l0[1], useBytes = TRUE)          # sin BOM
n_pc <- nchar(gsub("[^;]", "", cab))
n_co <- nchar(gsub("[^,]", "", cab))
sep  <- if (n_pc > n_co) ";" else ","

tab <- read.table(entrada, sep = sep, header = TRUE, colClasses = "character",
                  check.names = FALSE, quote = "\"", comment.char = "",
                  strip.white = TRUE, na.strings = c("", "NA", "NaN"),
                  fill = TRUE, blank.lines.skip = TRUE,
                  fileEncoding = if (enc_utf8) "UTF-8-BOM" else "latin1")
nombres <- names(tab)[-1]

# Fechas
f <- gsub("/", "-", sub("[ T].*$", "", trimws(tab[[1]])), fixed = TRUE)
muestra <- f[!is.na(f) & nzchar(f)][1]
fmt <- if (grepl("^[0-9]{4}-", muestra)) "%Y-%m-%d" else "%d-%m-%Y"
fechas <- as.Date(f, format = fmt)

# Precios: texto -> número (coma decimal chilena); no positivos o no numéricos -> faltante
a_numero <- function(x) {
  x <- gsub("[[:space:]]", "", x)
  if (sep == ";") x <- gsub(",", ".", x, fixed = TRUE)
  v <- suppressWarnings(as.numeric(x))
  v[!is.na(v) & v <= 0] <- NA_real_
  v
}
P <- do.call(cbind, lapply(tab[-1], a_numero))
P <- matrix(P, nrow = length(fechas), ncol = length(nombres))

ok_f <- !is.na(fechas)                       # filas sin fecha válida se descartan
fechas <- fechas[ok_f]
P <- P[ok_f, , drop = FALSE]

# ---- 2. Limpiar: orden, duplicados, splits, precios faltantes ---------------
# (1) ordenar por fecha (estable) y (2) eliminar fechas repetidas (se conserva una)
o <- order(fechas)
fechas <- fechas[o]
P <- P[o, , drop = FALSE]
dup <- duplicated(fechas)
fechas <- fechas[!dup]
P <- P[!dup, , drop = FALSE]
n <- length(fechas)

# (3) splits: sobre el precio anterior DISPONIBLE (se saltan los vacíos)
ajustar_splits <- function(p) {
  idx <- which(!is.na(p))
  if (length(idx) < 2) return(p)
  s <- p[idx]
  razon <- s[-1] / s[-length(s)]
  adj <- rep(1, length(s))
  for (i in seq_along(razon)) {
    for (k in 2:5) {
      if (abs(razon[i] * k - 1) <= 0.05 + 1e-12) {
        adj[1:i] <- adj[1:i] * k          # se dividen por k todos los precios anteriores
        break
      }
    }
  }
  p[idx] <- s / adj
  p
}

# (4) rellenar faltantes con el último precio disponible (hacia adelante)
rellenar <- function(p) {
  ult <- cummax(ifelse(is.na(p), 0L, seq_along(p)))
  out <- p
  hay <- ult > 0
  out[hay] <- p[ult[hay]]
  out
}

Pl <- matrix(NA_real_, nrow = n, ncol = length(nombres))
for (j in seq_along(nombres)) Pl[, j] <- rellenar(ajustar_splits(P[, j]))

# ---- 3. Calcular métricas por activo y de la cartera equiponderada ---------
if (n >= 2) {
  Ret <- rbind(NA_real_, Pl[-1, , drop = FALSE] / Pl[-n, , drop = FALSE] - 1)
} else {
  Ret <- matrix(NA_real_, nrow = n, ncol = length(nombres))
}

# vol y Sharpe a partir de una serie de retornos diarios
vol_sharpe <- function(r, rf, etiqueta) {
  r <- r[!is.na(r)]
  v <- if (length(r) >= 2) sd(r) * sqrt(252) else NA_real_
  if (is.na(v) || v == 0) {
    message("Aviso: ", etiqueta, " no tiene variabilidad suficiente; vol y sharpe = 0")
    return(c(vol = 0, sharpe = 0))
  }
  c(vol = v, sharpe = (mean(r) * 252 - rf) / v)
}

res_act <- matrix(NA_real_, nrow = length(nombres), ncol = 3,
                  dimnames = list(nombres, c("cagr", "vol", "sharpe")))
for (j in seq_along(nombres)) {
  validos <- which(!is.na(Pl[, j]))
  if (length(validos) == 0) {
    message("Aviso: ", nombres[j], " no tiene precios; métricas = 0")
    res_act[j, ] <- c(0, 0, 0)
    next
  }
  i0 <- validos[1]; i1 <- validos[length(validos)]
  anios <- as.numeric(fechas[i1] - fechas[i0]) / 365.25
  cagr <- if (anios > 0) (Pl[i1, j] / Pl[i0, j])^(1 / anios) - 1 else 0
  vs <- vol_sharpe(Ret[, j], rf, nombres[j])
  res_act[j, ] <- c(cagr, vs["vol"], vs["sharpe"])
}

# Cartera equiponderada: promedio de los retornos de los activos con dato ese día
rp <- rowMeans(Ret, na.rm = TRUE)
rp <- rp[!is.nan(rp)]
anios_p <- as.numeric(fechas[n] - fechas[1]) / 365.25
cagr_p <- if (length(rp) > 0 && anios_p > 0) prod(1 + rp)^(1 / anios_p) - 1 else 0
vs_p <- vol_sharpe(rp, rf, "CARTERA_EW")

# ---- 4. Escribir la salida (no cambie el formato) ---------------------------
resultado <- data.frame(
  activo = c(nombres, "CARTERA_EW"),
  cagr   = unname(c(res_act[, "cagr"],   cagr_p)),
  vol    = unname(c(res_act[, "vol"],    vs_p["vol"])),
  sharpe = unname(c(res_act[, "sharpe"], vs_p["sharpe"])),
  stringsAsFactors = FALSE
)
write.csv(resultado, salida, row.names = FALSE)
