# =============================================================================
# Programación en Finanzas (FIN443) - UAI 2026 - Desafío 2
# PRUEBA PÚBLICA DE FORMATO: ejecútela antes de entregar.
# -----------------------------------------------------------------------------
# Corre su desafio2.R (o desafio2.py) igual que lo hará el profesor y revisa que
# la salida cumpla el contrato. NO revisa si sus números están bien: eso lo
# decide usted (y luego los datos ocultos del profesor).
# Uso: deje este archivo, su desafio2.R o .py y precios_desafio2.csv en la misma
# carpeta, abra este archivo en RStudio y haga Source.
# =============================================================================
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}
ok <- function(cond, msg) { cat(if (cond) "  [OK]   " else "  [FALLA]", msg, "\n"); invisible(cond) }
source("C:/Users/Usuario/Downloads/desafio2_1.R")
salida
source("C:/Users/Usuario/Downloads/desafio2_1.R")
source("C:/Users/Usuario/Downloads/Proyecto Clase 2 Progra en Finanzas/probar_mi_entrega.R")
correr <- function(f) {
out <- tempfile(fileext = ".csv")
system2(file.path(R.home("bin"), "Rscript"), c("desafio2.R", f, out))
read.csv(out, check.names = FALSE)
}
d <- read.csv("precios_desafio2.csv", check.names = FALSE, stringsAsFactors = FALSE)
base <- correr("precios_desafio2.csv")
set.seed(1)
v1 <- d[sample(c(seq_len(nrow(d)), sample(nrow(d), 20))), ]
write.csv(v1, "v1.csv", row.names = FALSE)
v2 <- v1
v2$Fecha <- format(as.Date(v2$Fecha), "%d-%m-%Y")
v2[-1] <- lapply(v2[-1], function(x) sub(".", ",", as.character(x), fixed = TRUE))
write.table(v2, "v2.csv", sep = ";", row.names = FALSE, quote = FALSE, na = "")
all.equal(base, correr("v1.csv"))
all.equal(base, correr("v2.csv"))
read.csv("salida.csv")
source("C:/Users/Usuario/Downloads/desafio2.R")
system2(file.path(R.home("bin"), "Rscript"), c("pauta_desafio2_comentada.R", "precios_desafio2.csv", "s.csv", "0.03"))
read.csv("s.csv")
system2(file.path(R.home("bin"), "Rscript"), c("desafio2.R", "precios_desafio2.csv", "mi_s.csv", "0.03"))
read.csv("mi_s.csv")

script <- c("desafio2.R", "desafio2.py")[file.exists(c("desafio2.R", "desafio2.py"))][1]
if (is.na(script)) stop("No encuentro desafio2.R ni desafio2.py en esta carpeta.")
datos <- "precios_desafio2.csv"
if (!file.exists(datos)) stop("Copie precios_desafio2.csv en esta carpeta.")
out <- tempfile(fileext = ".csv")
cmd <- if (grepl("\\.R$", script)) file.path(R.home("bin"), "Rscript") else (if (nzchar(Sys.which("python3"))) "python3" else "python")
cat("Ejecutando:", cmd, script, datos, "salida.csv\n")
t0 <- Sys.time()
st <- system2(cmd, c(shQuote(script), shQuote(datos), shQuote(out)), stdout = TRUE, stderr = TRUE, timeout = 120)
cat("Tiempo:", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n\n")
if (!is.null(attr(st, "status")) && attr(st, "status") != 0) cat(st, sep = "\n")

cat("Revisión del contrato:\n")
if (!ok(file.exists(out), "el script creó el archivo de salida")) stop("Revise los mensajes de error de arriba.")
s <- tryCatch(read.csv(out, stringsAsFactors = FALSE), error = function(e) NULL)
ok(!is.null(s), "la salida se lee como CSV separado por comas")
names(s) <- tolower(trimws(names(s)))
ok(all(c("activo", "cagr", "vol", "sharpe") %in% names(s)), "tiene las columnas activo, cagr, vol, sharpe")
p <- read.csv(datos, nrows = 1, check.names = FALSE); act <- names(p)[-1]
ok(all(c(act, "CARTERA_EW") %in% trimws(s$activo)), paste("tiene una fila por activo y CARTERA_EW:", paste(c(act, "CARTERA_EW"), collapse = ", ")))
ok(all(sapply(s[c("cagr", "vol", "sharpe")], is.numeric)), "las métricas son números (punto decimal, sin %)")
ok(all(s$vol > 0 & s$vol < 3, na.rm = TRUE), "cagr y vol están en fracción (0.0462), no en porcentaje (4.62)")
ok(!anyNA(s[c("cagr", "vol", "sharpe")]), "no hay valores faltantes")
cat("\nSu salida:\n"); print(s)
