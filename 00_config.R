# =============================================================================
# 00_config.R — Configuração geral do projeto
# Artigo: Uso e cobertura da terra em Ilha Grande (PI) — ressubmissão
# Dados: MapBiomas Brasil, Coleção 11 (1985–2025)
#
# Este arquivo NÃO precisa ser rodado sozinho: os scripts 01 a 04 chamam
# source("00_config.R") no início. Aqui ficam caminhos, anos, pacotes e a
# legenda das classes, para que qualquer ajuste seja feito em um só lugar.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Correção do PROJ (conflito com o PostGIS no Windows)
# -----------------------------------------------------------------------------
# O instalador do PostGIS cria a variável de ambiente PROJ_LIB apontando para
# um proj.db antigo (C:/Program Files (x86)/PostgreSQL/.../proj). Com isso, o
# terra e o sf não conseguem reprojetar ("proj.db contains
# DATABASE.LAYOUT.VERSION.MINOR = 2", "Cannot set raster SRS: empty srs").
# Aqui apontamos o PROJ para a base que vem junto com o próprio pacote terra.
# IMPORTANTE: isso precisa acontecer ANTES de carregar terra/sf. Se eles já
# estiverem carregados, reinicie o R (Ctrl+Shift+F10) e rode de novo.
proj_dir <- system.file("proj", package = "terra")
if (!file.exists(file.path(proj_dir, "proj.db"))) proj_dir <- system.file("proj", package = "sf")
if (file.exists(file.path(proj_dir, "proj.db"))) {
  Sys.setenv(PROJ_LIB = proj_dir, PROJ_DATA = proj_dir)
} else {
  Sys.unsetenv(c("PROJ_LIB", "PROJ_DATA"))   # usa a base padrão do GDAL
}
if (any(c("terra", "sf") %in% loadedNamespaces())) {
  message("Atenção: terra/sf já estavam carregados. Se aparecer erro de PROJ, ",
          "reinicie o R (Ctrl+Shift+F10) e rode o script de novo.")
}


# -----------------------------------------------------------------------------
# 1. Pacotes
# -----------------------------------------------------------------------------
# terra   : leitura, recorte e álgebra de rasters
# sf      : leitura do limite municipal e operações vetoriais
# dplyr/tidyr/readr : manipulação e exportação das tabelas
# ggplot2/scales    : mapas e gráficos
# ggspatial (opcional): escala gráfica e seta de norte nos mapas
pacotes <- c("terra", "sf", "dplyr", "tidyr", "readr", "ggplot2", "scales")
faltando <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltando) > 0) install.packages(faltando)
invisible(lapply(pacotes, library, character.only = TRUE))

# Reforça, já com o sf carregado, onde está o proj.db correto.
if (file.exists(file.path(proj_dir, "proj.db"))) {
  try(sf::sf_proj_search_paths(proj_dir), silent = TRUE)
}

# Silencia mensagens do sf sobre geometria esférica (s2): o limite é pequeno e
# as áreas são calculadas no raster, não no vetor.
sf::sf_use_s2(FALSE)


# -----------------------------------------------------------------------------
# 2. Caminhos
# -----------------------------------------------------------------------------
# Use barras normais (/) mesmo no Windows.
DIR_BASE <- "C:/Users/vj_ac/Downloads/Art_ruan"            # pasta do artigo
DIR_MB   <- "C:/PosDoc/MagPie_Crops/raster/mb_col_11"      # rasters nacionais Col. 11

ARQ_LIMITE <- file.path(DIR_BASE, "Ilha_Grande.gpkg")      # limite municipal (IBGE)

# Opcionais: se estes arquivos existirem, o script 02 calcula quanto do
# município está dentro da APA e da RESEX (pedido do parecerista B).
ARQ_APA   <- file.path(DIR_BASE, "APA_Delta_Parnaiba.gpkg")
ARQ_RESEX <- file.path(DIR_BASE, "RESEX_Delta_Parnaiba.gpkg")

# Tabelas: salvas com write_excel_csv2() — separador ";", decimal "," e
# codificação UTF-8 com BOM, para abrir direto no Excel em português com os
# acentos corretos.

# Pastas de saída (criadas automaticamente)
DIR_RECORTE <- file.path(DIR_BASE, "mb_col11_recorte")     # rasters recortados
DIR_SAIDA   <- file.path(DIR_BASE, "resultados")
DIR_TAB     <- file.path(DIR_SAIDA, "tabelas")
DIR_RAS     <- file.path(DIR_SAIDA, "rasters")
DIR_FIG     <- file.path(DIR_SAIDA, "figuras")
for (d in c(DIR_RECORTE, DIR_SAIDA, DIR_TAB, DIR_RAS, DIR_FIG)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}


# -----------------------------------------------------------------------------
# 3. Anos
# -----------------------------------------------------------------------------
# ANOS_MAPAS: anos dos mapas e das transições (intervalos decenais).
#   Alternativa discutida: c(1992, 2002, 2012, 2025) — exige baixar 1992,
#   2002 e 2012, que não estão na pasta da Coleção 11.
ANOS_MAPAS <- c(1985, 1995, 2005, 2015, 2025)

# ANOS_SERIE: anos usados no gráfico de linhas. Por padrão, todos os anos
# que existirem na pasta de recortes (detectados no script 02).
ANOS_SERIE <- NULL


# -----------------------------------------------------------------------------
# 4. Legenda da Coleção 11 (códigos, nomes, cores e grupos)
# -----------------------------------------------------------------------------
# Códigos, nomes, cores e hierarquia (nível 1) conferidos com a tabela
# oficial "Códigos das classes da legenda da Coleção 11 do MapBiomas Brasil"
# (publicada em 13/08/2026) e com o estilo ESTILO_QGIS_COL11_PT.qml.
# Só as classes finais aparecem no raster; as de
# nível 1 (Floresta, Agropecuária etc.) não são valores de pixel.
# Classes que aparecerem no raster e não estiverem aqui são avisadas no 02.
#
# Novidades da Coleção 11 que importam para Ilha Grande:
#   91 Parque Eólico (beta) -> permite quantificar diretamente a área da
#      usina eólica citada no texto (antes inferida só pelas fotos).
#   50 agora se chama "Restinga Herbácea ou Arbustiva".
#   77 Formação Herbáceo Arbustiva, 84 Marisma (beta), 7 Savana Alagada (beta).
#   Cores de 29 (Afloramento Rochoso), 50 e 75 mudaram em relação à Col. 10.
#
# Coluna "grupo": agrupamento usado para as classes de mudança (Quadro 1).
#   VEG  = vegetação natural (florestal e não florestal)
#   AGRO = agropecuária
#   NVEG = área não vegetada antrópica (urbano, mineração, usinas), SEM praia/duna
#   DUNA = praia, duna e areal (tratada à parte, foco do artigo)
#   AGUA = corpos d'água naturais
#   AQUI = aquicultura (uso antrópico sobre água)
#   NOBS = não observado
# Coluna "nivel1": classe de nível 1 do MapBiomas, para tabelas resumidas.
# "Não Observado" (27) não consta da legenda da Col. 11; fica aqui só por
# segurança, caso algum pixel ainda use esse código.
legenda <- tibble::tribble(
  ~codigo, ~classe,                            ~nivel1,                         ~grupo, ~cor,
   3,      "Formação Florestal",               "Floresta",                      "VEG",  "#1f8d49",
   4,      "Formação Savânica",                "Floresta",                      "VEG",  "#7dc975",
   5,      "Mangue",                           "Floresta",                      "VEG",  "#04381d",
   6,      "Floresta Alagável",                "Floresta",                      "VEG",  "#007785",
   7,      "Savana Alagada",                   "Floresta",                      "VEG",  "#228c70",
  49,      "Restinga Arbórea",                 "Floresta",                      "VEG",  "#02d659",
  11,      "Campo Alagado e Área Pantanosa",   "Vegetação Herbácea e Arbustiva","VEG",  "#519799",
  12,      "Formação Campestre",               "Vegetação Herbácea e Arbustiva","VEG",  "#d6bc74",
  32,      "Apicum",                           "Vegetação Herbácea e Arbustiva","VEG",  "#fc8114",
  29,      "Afloramento Rochoso",              "Vegetação Herbácea e Arbustiva","VEG",  "#ad5100",
  50,      "Restinga Herbácea ou Arbustiva",   "Vegetação Herbácea e Arbustiva","VEG",  "#ffaa5f",
  77,      "Formação Herbáceo Arbustiva",      "Vegetação Herbácea e Arbustiva","VEG",  "#86b074",
  84,      "Marisma",                          "Vegetação Herbácea e Arbustiva","VEG",  "#81dbbf",
  15,      "Pastagem",                         "Agropecuária",                  "AGRO", "#edde8e",
  20,      "Cana",                             "Agropecuária",                  "AGRO", "#db7093",
  39,      "Soja",                             "Agropecuária",                  "AGRO", "#f5b3c8",
  40,      "Arroz",                            "Agropecuária",                  "AGRO", "#c71585",
  62,      "Algodão",                          "Agropecuária",                  "AGRO", "#ff69b4",
  41,      "Outras Lavouras Temporárias",      "Agropecuária",                  "AGRO", "#f54ca9",
  46,      "Café",                             "Agropecuária",                  "AGRO", "#d68fe2",
  47,      "Citrus",                           "Agropecuária",                  "AGRO", "#9932cc",
  35,      "Dendê",                            "Agropecuária",                  "AGRO", "#9065d0",
  48,      "Outras Lavouras Perenes",          "Agropecuária",                  "AGRO", "#e6ccff",
   9,      "Silvicultura",                     "Agropecuária",                  "AGRO", "#7a5900",
  21,      "Mosaico de Usos",                  "Agropecuária",                  "AGRO", "#ffefc3",
  23,      "Praia, Duna e Areal",              "Área não Vegetada",             "DUNA", "#ffa07a",
  24,      "Área Urbanizada",                  "Área não Vegetada",             "NVEG", "#d4271e",
  30,      "Mineração",                        "Área não Vegetada",             "NVEG", "#9c0027",
  75,      "Usina Fotovoltaica",               "Área não Vegetada",             "NVEG", "#757272",
  91,      "Parque Eólico",                    "Área não Vegetada",             "NVEG", "#403d3e",
  25,      "Outras Áreas não Vegetadas",       "Área não Vegetada",             "NVEG", "#db4d4f",
  33,      "Rio, Lago e Oceano",               "Corpo D'água",                  "AGUA", "#2532e4",
  31,      "Aquicultura",                      "Corpo D'água",                  "AQUI", "#091077",
  27,      "Não Observado",                    "Não Observado",                 "NOBS", "#ffffff"
)

# Classes que ganham tabela própria de "de onde veio" no script 03
# (o que cada uma substituiu em cada período).
CLASSES_ORIGEM <- c(24, 91)   # Área Urbanizada e Parque Eólico


# -----------------------------------------------------------------------------
# 5. Classes de mudança (substituem o Quadro 1 do manuscrito)
# -----------------------------------------------------------------------------
# Os pareceristas criticaram "Degradação" e "Regeneração" (são conceitos que
# não se deduzem só da troca de classe) e a ambiguidade de "Dunas fixas"
# (praia/duna que continua praia/duna não é duna FIXA, que é vegetada).
# Os nomes abaixo descrevem apenas a transição observada. A coluna
# "nome_antigo" ajuda a mapear o texto original para a nova versão.
# "Movimento de dunas" foi separado em ganho e perda, para dizer em que
# direção a areia avançou; a soma dos dois equivale à classe antiga.
classes_mudanca <- tibble::tribble(
  ~id, ~mudanca,                         ~nome_antigo,            ~cor_mud,
   1,  "Vegetação natural persistente",  "Formação Vegetal",      "#1f8d49",
   2,  "Conversão para uso antrópico",   "Degradação Ambiental",  "#d4271e",
   3,  "Retorno à vegetação natural",    "Regeneração Ambiental", "#7dc975",
   4,  "Uso antrópico persistente",      "Antrópico",             "#edde8e",
   5,  "Praia/duna persistente",         "Dunas Fixas",           "#ffa07a",
   6,  "Ganho de praia/duna",            "Movimento de Dunas",    "#e65100",
   7,  "Perda de praia/duna",            "Movimento de Dunas",    "#8d6e63",
   8,  "Conversão para água",            "Área Inundável",        "#2532e4",
   9,  "Água persistente",               "(não existia)",         "#9ecae1",
  10,  "Recuo da água",                  "(não existia)",         "#6a51a3",
  11,  "Não observado",                  "(não existia)",         "#bdbdbd"
)

# Regra de classificação de uma transição (grupo no ano inicial -> grupo no
# ano final). A ORDEM importa: praia/duna é avaliada antes de tudo, porque
# no Quadro 1 original ela aparecia ao mesmo tempo em "Área não vegetada" e
# em "Movimento de dunas", o que gerava sobreposição de regras.
regra_mudanca <- function(de, para) {
  antropico <- c("AGRO", "NVEG", "AQUI")
  dplyr::case_when(
    de == "NOBS" | para == "NOBS"                 ~ 11L,
    de == "DUNA" & para == "DUNA"                 ~ 5L,
    de != "DUNA" & para == "DUNA"                 ~ 6L,
    de == "DUNA" & para != "DUNA"                 ~ 7L,
    de == "AGUA" & para == "AGUA"                 ~ 9L,
    de != "AGUA" & para == "AGUA"                 ~ 8L,
    de == "AGUA" & para != "AGUA"                 ~ 10L,
    de == "VEG"  & para == "VEG"                  ~ 1L,
    de == "VEG"  & para %in% antropico            ~ 2L,
    de %in% antropico & para == "VEG"             ~ 3L,
    de %in% antropico & para %in% antropico       ~ 4L,
    TRUE                                          ~ NA_integer_
  )
}


# -----------------------------------------------------------------------------
# 6. Funções auxiliares
# -----------------------------------------------------------------------------
# Lê o limite municipal e devolve um SpatVector no mesmo SRC do raster.
le_limite <- function(r) {
  lim <- sf::st_read(ARQ_LIMITE, quiet = TRUE)
  lim <- sf::st_union(lim) |> sf::st_as_sf()
  terra::project(terra::vect(lim), terra::crs(r))
}

# Caminho do raster recortado de um ano.
arq_recorte <- function(ano) file.path(DIR_RECORTE, sprintf("ig_col11_%d.tif", ano))

# Anos com recorte disponível na pasta.
anos_disponiveis <- function() {
  f <- list.files(DIR_RECORTE, pattern = "^ig_col11_\\d{4}\\.tif$")
  sort(as.integer(sub("^ig_col11_(\\d{4})\\.tif$", "\\1", f)))
}
