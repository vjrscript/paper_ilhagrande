# =============================================================================
# 01_recorte.R — Recorte dos rasters nacionais da Coleção 11 para Ilha Grande
#
# Entrada : rasters nacionais em DIR_MB (brazil_coverage-col11_AAAA.tif)
#           limite municipal em ARQ_LIMITE (Ilha_Grande.gpkg)
# Saída   : DIR_RECORTE/ig_col11_AAAA.tif (um por ano, só o município)
#
# Substitui o arquivo .bat enviado antes: faz o recorte E a máscara pelo
# limite em um único passo, no R.
# Tempo esperado: poucos segundos por ano. O terra lê só a janela do
# município, não o raster nacional inteiro.
# =============================================================================

# Garante que o R está na pasta dos scripts (necessário para achar o
# 00_config.R). Se você mudar a pasta de lugar, ajuste o caminho abaixo.
DIR_SCRIPTS <- "C:/Users/vj_ac/Downloads/Art_ruan/scripts_R"
if (!file.exists("00_config.R")) setwd(DIR_SCRIPTS)
source("00_config.R")


# -----------------------------------------------------------------------------
# 1. Lista os rasters nacionais disponíveis
# -----------------------------------------------------------------------------
arqs <- list.files(DIR_MB, pattern = "^brazil_coverage-col11_\\d{4}\\.tif$",
                   full.names = TRUE)
if (length(arqs) == 0) stop("Nenhum raster da Coleção 11 encontrado em ", DIR_MB)

anos <- as.integer(sub(".*_(\\d{4})\\.tif$", "\\1", basename(arqs)))
cat("Anos encontrados:", paste(anos, collapse = ", "), "\n")

# Aviso se algum ano escolhido para os mapas não estiver na pasta.
faltam <- setdiff(ANOS_MAPAS, anos)
if (length(faltam) > 0) {
  warning("Anos de ANOS_MAPAS sem raster nacional: ", paste(faltam, collapse = ", "),
          ". Baixe-os da plataforma MapBiomas antes de seguir.")
}


# -----------------------------------------------------------------------------
# 2. Lê o limite municipal no SRC dos rasters
# -----------------------------------------------------------------------------
# Todos os rasters do MapBiomas usam o mesmo SRC (WGS84, EPSG:4326), então
# basta usar o primeiro como referência. O limite do IBGE vem em SIRGAS 2000
# (EPSG:4674) e é reprojetado para o SRC do raster.
r_ref  <- rast(arqs[1])
limite <- le_limite(r_ref)
cat("SRC do raster:", crs(r_ref, describe = TRUE)$name, "\n")


# -----------------------------------------------------------------------------
# 3. Recorta e mascara cada ano
# -----------------------------------------------------------------------------
# crop(..., snap = "out"): corta pela extensão do município sem reamostrar,
#   mantendo os pixels originais de 30 m alinhados com a grade do MapBiomas.
# mask(...): pixels com centro fora do limite viram NA. Assim a soma das
#   áreas corresponde à área do município (± pixels de borda).
for (i in seq_along(arqs)) {
  saida <- arq_recorte(anos[i])
  cat(sprintf("[%d/%d] %d -> %s\n", i, length(arqs), anos[i], basename(saida)))

  r <- rast(arqs[i])
  r <- crop(r, limite, snap = "out")
  r <- mask(r, limite)
  names(r) <- paste0("c", anos[i])

  # INT1U (0–255) é suficiente para os códigos da legenda e deixa o arquivo
  # pequeno. NAflag = 0 mantém o "sem dado" do MapBiomas.
  writeRaster(r, saida, datatype = "INT1U", NAflag = 0, overwrite = TRUE,
              gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
}


# -----------------------------------------------------------------------------
# 4. Conferência rápida
# -----------------------------------------------------------------------------
# Área total mapeada em cada ano (deve ser praticamente igual entre os anos e
# próxima à área do limite, cerca de 13.264 ha. Uma diferença de 1–2% é
# esperada: pixels de borda e ilhotas/canais menores que 30 m se perdem).
conf <- lapply(anos, function(a) {
  r <- rast(arq_recorte(a))
  data.frame(ano = a,
             area_ha = global(cellSize(r, unit = "ha", mask = TRUE), "sum",
                              na.rm = TRUE)[1, 1])
})
print(do.call(rbind, conf))
cat("Recortes salvos em:", DIR_RECORTE, "\n")
