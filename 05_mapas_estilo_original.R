# =============================================================================
# 05_mapas_estilo_original.R — Figuras no estilo do manuscrito original
#
# Objetivo: refazer as Figuras 2, 4, 5 e 6 com os dados da Coleção 11, o
# mais perto possível do visual da versão submetida (painéis lado a lado,
# fundo OpenStreetMap, contorno preto com sombra, rosa dos ventos, escala em
# km, legenda em duas colunas, bloco de texto com projeção e datum), para
# comparar com as figuras do script 04.
#
# Entrada : recortes (01), rasters de mudança (03), tabela de mudanças (03)
# Saída   : DIR_FIG/estilo_original/
#   fig2_uso_cobertura.png   -> uso e cobertura por ano
#   fig4_mudancas.png        -> classes de mudança por período
#   fig5_mudancas_rosca.png  -> gráficos de rosca (% de cada classe de mudança)
#   fig6_praia_duna.png      -> praia/duna persistente sobreposta por período
#
# Pacotes extras: patchwork (montagem) e maptiles (fundo OpenStreetMap).
#   install.packages(c("patchwork", "maptiles"))
# O fundo OSM precisa de internet. Sem internet, as figuras saem com fundo
# cinza claro (o resto não muda).
# =============================================================================

DIR_SCRIPTS <- "C:/Users/vj_ac/Downloads/Art_ruan/scripts_R"
if (!file.exists("00_config.R")) setwd(DIR_SCRIPTS)
source("00_config.R")

for (p in c("patchwork", "maptiles")) {
  if (!requireNamespace(p, quietly = TRUE)) try(install.packages(p))
}
library(patchwork)


# =============================================================================
# 1. OPÇÕES — ajuste aqui
# =============================================================================

# Paleta: "original" = cores das figuras do manuscrito (amostradas das
# imagens); "mapbiomas" = cores oficiais da Coleção 11 (como no script 04).
PALETA <- "original"

# TRUE  = agrupa as classes de mudança como no Quadro 1 original (ganho e
#         perda de praia/duna viram uma classe só; as três classes de água
#         viram uma só). Mantém a ideia do artigo com nomes revisados.
# FALSE = mostra todas as classes separadas (como no script 04).
AGRUPAR_COMO_ORIGINAL <- TRUE

# Incluir o período completo (ex.: 1985–2025) nas Figuras 4, 5 e 6?
# O original mostrava só os períodos consecutivos.
INCLUIR_PERIODO_COMPLETO <- FALSE

# Fundo OpenStreetMap (requer internet). FALSE = fundo cinza claro.
USAR_FUNDO_OSM <- TRUE

# Sistema de projeção dos mapas (como no original: SIRGAS 2000 / UTM 23S).
# Escrito como texto PROJ (e não "EPSG:31983") de propósito: assim o R não
# precisa consultar o proj.db, o que evita o conflito com o PROJ do PostGIS.
SRC_MAPA <- "+proj=utm +zone=23 +south +ellps=GRS80 +towgs84=0,0,0 +units=m +no_defs"
SRC_LL   <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs"   # SIRGAS 2000 geográfico

# Texto do bloco de informações (canto inferior esquerdo).
TEXTO_INFO <- paste("SISTEMA DE PROJEÇÃO UTM – FUSO 23S",
                    "COORDENADAS GEOGRÁFICAS",
                    "DATUM: SIRGAS 2000",
                    "FONTE: MAPBIOMAS, COLEÇÃO 11 (2026)",
                    sep = "\n")

# Fonte tipográfica: "serif" vira Times New Roman no Windows (como no
# original). Troque por "sans" se preferir Arial.
FONTE <- "serif"

# Largura das figuras (cm). A altura é calculada pelo número de painéis.
LARGURA_CM <- 17

DIR_FIG_ORIG <- file.path(DIR_FIG, "estilo_original")
dir.create(DIR_FIG_ORIG, showWarnings = FALSE, recursive = TRUE)


# =============================================================================
# 2. Paletas
# =============================================================================

# 2.1 Uso e cobertura. Cores amostradas da Figura 2 do manuscrito. Classes
# que não existiam na Coleção 7.1 (restingas, parque eólico etc.) recebem
# cores escolhidas para combinar com o conjunto; as demais usam a cor
# oficial do MapBiomas.
cores_uso_original <- c(
  "3"  = "#026302",  # Formação Florestal
  "4"  = "#06fd00",  # Formação Savânica
  "5"  = "#67723a",  # Mangue
  "11" = "#49bfa7",  # Campo Alagado e Área Pantanosa
  "12" = "#b5ae54",  # Formação Campestre
  "15" = "#fdd96b",  # Pastagem
  "21" = "#fef5bc",  # Mosaico de Usos
  "23" = "#dd7c6b",  # Praia, Duna e Areal
  "24" = "#b02b2c",  # Área Urbanizada
  "25" = "#fc98fa",  # Outras Áreas não Vegetadas
  "32" = "#948947",  # Apicum
  "33" = "#0604ee",  # Rio, Lago e Oceano
  "39" = "#c49ef3",  # Soja
  "41" = "#ea86fa",  # Outras Lavouras Temporárias
  # --- classes novas na Coleção 11 (não estavam no original) ---
  "49" = "#3cb371",  # Restinga Arbórea
  "50" = "#c4a484",  # Restinga Herbácea ou Arbustiva
  "29" = "#8b4513",  # Afloramento Rochoso
  "91" = "#403d3e"   # Parque Eólico
)
leg_uso <- legenda |>
  mutate(cor_fig = if (PALETA == "original")
    coalesce(cores_uso_original[as.character(codigo)], cor) else cor)

# 2.2 Classes de mudança. Cores amostradas da Figura 4 do manuscrito, ligadas
# aos nomes revisados (o nome antigo está no comentário).
mud_fig <- tibble::tribble(
  ~id, ~cat_agrupada,                        ~cor_original, ~cor_original_sep,
   1,  "Vegetação natural persistente",      "#036500",     "#036500",  # Formação Vegetal
   2,  "Conversão para uso antrópico",       "#f50504",     "#f50504",  # Degradação Ambiental
   3,  "Retorno à vegetação natural",        "#5bd949",     "#5bd949",  # Regeneração Ambiental
   4,  "Uso antrópico persistente",          "#f3fc05",     "#f3fc05",  # Antrópico
   5,  "Praia/duna persistente",             "#d8806c",     "#d8806c",  # Dunas Fixas
   6,  "Dinâmica de praia/duna",             "#fc7a02",     "#fc7a02",  # Movimento de Dunas (ganho)
   7,  "Dinâmica de praia/duna",             "#fc7a02",     "#8b4500",  # Movimento de Dunas (perda)
   8,  "Corpos d'água e áreas inundáveis",   "#0002f9",     "#0002f9",  # Área Inundável
   9,  "Corpos d'água e áreas inundáveis",   "#0002f9",     "#6baed6",
  10,  "Corpos d'água e áreas inundáveis",   "#0002f9",     "#9e9ac8",
  11,  "Não observado",                      "#bdbdbd",     "#bdbdbd"
) |>
  left_join(classes_mudanca |> select(id, mudanca, cor_mud), by = "id")

# Monta a tabela final (id -> categoria da figura -> cor) conforme as opções.
mud_fig <- mud_fig |>
  mutate(categoria = if (AGRUPAR_COMO_ORIGINAL) cat_agrupada else mudanca,
         cor_fig = dplyr::case_when(
           PALETA == "original" &  AGRUPAR_COMO_ORIGINAL ~ cor_original,
           PALETA == "original" & !AGRUPAR_COMO_ORIGINAL ~ cor_original_sep,
           TRUE                                          ~ cor_mud))
ordem_cat <- unique(mud_fig$categoria)
pal_mud   <- mud_fig |> distinct(categoria, .keep_all = TRUE) |>
  with(setNames(cor_fig, categoria))

# 2.3 Praia/duna por período (Figura 6): uma cor por período, como no
# original (salmão, amarelo, vermelho), mais roxo e verde se houver mais.
cores_periodo_duna <- c("#de7d6a", "#f3fb07", "#fb0102", "#8e44ad", "#1b9e77")


# =============================================================================
# 3. Insumos comuns: limite, sombra, extensão, fundo
# =============================================================================

# Limite municipal no SRC do mapa.
limite <- st_read(ARQ_LIMITE, quiet = TRUE) |> st_union() |> st_as_sf() |>
  st_transform(SRC_MAPA)

# "Sombra" do contorno: o mesmo polígono deslocado 250 m para sudeste,
# desenhado em cinza translúcido por baixo (efeito do layout original).
sombra <- limite
st_geometry(sombra) <- st_geometry(limite) + c(250, -250)
st_crs(sombra) <- st_crs(limite)

# Extensão de todos os painéis: limite + 1,2 km de margem.
bb   <- st_bbox(limite)
marg <- 1200
lim_x <- c(bb["xmin"] - marg, bb["xmax"] + marg)
lim_y <- c(bb["ymin"] - marg, bb["ymax"] + marg)

# Fundo OpenStreetMap: baixa os blocos uma vez (ficam em cache em
# resultados/cache_osm), reprojeta para UTM e guarda como imagem.
fundo <- NULL
if (USAR_FUNDO_OSM) {
  fundo <- tryCatch({
    ext_ll <- st_as_sfc(st_bbox(c(xmin = lim_x[[1]], xmax = lim_x[[2]],
                                  ymin = lim_y[[1]], ymax = lim_y[[2]]),
                                crs = st_crs(SRC_MAPA))) |> st_transform(SRC_LL)
    tiles <- maptiles::get_tiles(ext_ll, provider = "OpenStreetMap", zoom = 13,
                                 crop = TRUE,
                                 cachedir = file.path(DIR_SAIDA, "cache_osm"))
    tiles <- project(tiles, SRC_MAPA, method = "bilinear")
    tiles <- crop(tiles, ext(lim_x[[1]], lim_x[[2]], lim_y[[1]], lim_y[[2]]))
    # Converte as 3 bandas (R, G, B) numa imagem para annotation_raster().
    v <- as.array(tiles)[, , 1:3]
    v[is.na(v)] <- 255
    list(img = grDevices::as.raster(v / 255), e = as.vector(ext(tiles)))
  }, error = function(e) {
    message("Fundo OSM indisponível (", conditionMessage(e), "). Usando fundo cinza.")
    NULL
  })
}

# Camada de fundo (OSM clareado, como no original, ou cinza liso).
camada_fundo <- function() {
  if (is.null(fundo)) return(NULL)
  list(annotation_raster(fundo$img, xmin = fundo$e[["xmin"]], xmax = fundo$e[["xmax"]],
                         ymin = fundo$e[["ymin"]], ymax = fundo$e[["ymax"]]),
       # véu branco translúcido: deixa o OSM mais suave, como no original
       annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
                fill = "white", alpha = 0.25))
}

# Raster (já recortado) -> UTM -> data.frame para o ggplot.
# method = "near": reprojeção por vizinho mais próximo, que não cria
# códigos de classe inexistentes.
raster_utm_df <- function(arq) {
  r <- project(rast(arq), SRC_MAPA, method = "near", res = 30)
  d <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(d)[3] <- "valor"
  d
}


# =============================================================================
# 4. Elementos do layout (coordenadas, rosa dos ventos, escala, texto)
# =============================================================================

# Rótulos em graus-minutos-segundos, como no original (41°48'0"W).
gms <- function(v, eixo) {
  h <- if (eixo == "x") ifelse(v < 0, "W", "E") else ifelse(v < 0, "S", "N")
  seg <- round(abs(v) * 3600)            # arredonda em segundos primeiro
  g <- seg %/% 3600; m <- (seg %% 3600) %/% 60; s <- seg %% 60
  sprintf("%d°%d'%d\"%s", g, m, s, h)
}
# Marcas a cada 0,1° (6'), dentro da extensão do mapa.
ext_ll <- st_bbox(st_transform(st_as_sfc(st_bbox(c(xmin = lim_x[[1]], xmax = lim_x[[2]],
  ymin = lim_y[[1]], ymax = lim_y[[2]]), crs = st_crs(SRC_MAPA))), SRC_LL))
brk_x <- seq(ceiling(ext_ll["xmin"] * 10) / 10, floor(ext_ll["xmax"] * 10) / 10, by = 0.1)
brk_y <- seq(ceiling(ext_ll["ymin"] * 10) / 10, floor(ext_ll["ymax"] * 10) / 10, by = 0.1)

# Barra de escala desenhada à mão (em metros, pois o mapa está em UTM):
# "2  0  2  4 km", no canto inferior direito do painel, fora do município.
barra_escala <- function(km_total = 4, km_esq = 2) {
  x0 <- lim_x[[2]] - 1800 - (km_total + km_esq) * 1000   # folga p/ o "km"
  y0 <- lim_y[[1]] + 700
  xs <- x0 + c(0, km_esq, km_esq + km_total / 2, km_esq + km_total) * 1000
  rot <- c(km_esq, 0, km_total / 2, km_total)
  list(
    annotate("segment", x = xs[1], xend = xs[4], y = y0, yend = y0, linewidth = 0.4),
    annotate("segment", x = xs, xend = xs, y = y0, yend = y0 + 250, linewidth = 0.4),
    annotate("text", x = xs, y = y0 + 700, label = rot, size = 1.9, family = FONTE),
    annotate("text", x = xs[4] + 350, y = y0 + 700, label = "km", size = 1.9,
             family = FONTE, hjust = 0)
  )
}

# Rosa dos ventos simples (4 pontas + círculo + N, S, E, W), como no
# original. Desenhada em coordenadas de 0 a 1, num painel à parte.
rosa_dos_ventos <- function() {
  ponta <- function(ang) {
    a <- ang * pi / 180
    dir  <- c(sin(a), cos(a))          # direção da ponta
    perp <- c(cos(a), -sin(a))         # perpendicular (largura da base)
    pts <- rbind(0.5 + 0.40 * dir,                     # ponta
                 0.5 + 0.15 * dir + 0.08 * perp,       # base, lado 1
                 0.5 + 0.15 * dir - 0.08 * perp)       # base, lado 2
    data.frame(x = pts[, 1], y = pts[, 2], g = ang)
  }
  pts  <- do.call(rbind, lapply(c(0, 90, 180, 270), ponta))
  circ <- data.frame(x = 0.5 + 0.16 * cos(seq(0, 2 * pi, length.out = 60)),
                     y = 0.5 + 0.16 * sin(seq(0, 2 * pi, length.out = 60)))
  ggplot() +
    geom_polygon(data = pts, aes(x, y, group = g), fill = "grey40", colour = "grey15",
                 linewidth = 0.2) +
    geom_polygon(data = circ, aes(x, y), fill = "white", colour = "black", linewidth = 0.6) +
    annotate("text", x = c(0.5, 0.5, 0.97, 0.03), y = c(0.99, 0.01, 0.5, 0.5),
             label = c("N", "S", "E", "W"), size = 2.2, fontface = "bold", family = FONTE) +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off") +
    theme_void()
}

# Tema dos painéis de mapa: sem fundo, coordenadas nas bordas superior e
# esquerda, como no original.
tema_painel <- theme_void(base_family = FONTE) +
  theme(panel.border = element_rect(fill = NA, colour = "grey40", linewidth = 0.3),
        axis.text.x.top = element_text(size = 6, family = FONTE),
        axis.text.y.left = element_text(size = 6, angle = 90, hjust = 0.5, family = FONTE),
        axis.ticks.length = unit(0, "pt"),
        plot.margin = margin(1, 1, 1, 1),
        # a legenda é recolhida pelo patchwork e desenhada uma vez só,
        # no bloco inferior direito (ver monta_figura)
        legend.title = element_text(face = "bold", size = 7, family = FONTE),
        legend.text = element_text(size = 6, family = FONTE),
        legend.key.size = unit(0.28, "cm"),
        legend.key = element_rect(colour = "black", linewidth = 0.2))

# Escala de cores + formato da legenda (título "LEGENDA", n colunas).
escala_fill <- function(pal, ncol = 2) {
  list(scale_fill_manual(values = pal, drop = FALSE, name = "LEGENDA"),
       guides(fill = guide_legend(ncol = ncol, title.position = "top",
                                  title.hjust = 0.5, byrow = FALSE)))
}

# Moldura comum de um painel: fundo, sombra, (conteúdo), contorno, rótulo
# (a), (b)..., coordenadas e recorte na extensão.
painel <- function(conteudo, letra, escala = FALSE, contorno_por_cima = TRUE,
                   fundo_municipio = NA) {
  ggplot() +
    camada_fundo() +
    geom_sf(data = sombra, fill = "grey30", colour = NA, alpha = 0.45) +
    geom_sf(data = limite, fill = fundo_municipio, colour = NA) +
    conteudo +
    geom_sf(data = limite, fill = NA, colour = "black", linewidth = 1.1) +
    annotate("text", x = lim_x[[1]] + 250, y = lim_y[[2]] - 250, label = letra,
             hjust = 0, vjust = 1, size = 3, family = FONTE) +
    (if (escala) barra_escala() else NULL) +
    scale_x_continuous(breaks = brk_x, labels = gms(brk_x, "x"), position = "top") +
    scale_y_continuous(breaks = brk_y, labels = gms(brk_y, "y")) +
    coord_sf(crs = SRC_MAPA, datum = st_crs(SRC_LL), xlim = lim_x, ylim = lim_y, expand = FALSE,
             label_axes = list(top = "E", left = "N")) +
    tema_painel
}

# Monta a figura final: painéis lado a lado em cima; embaixo, rosa dos
# ventos, texto de projeção e a legenda (recolhida dos painéis pelo
# patchwork, para ser desenhada uma única vez e na mesma fonte da figura).
# A altura é calculada pela proporção do mapa e pelo número de painéis,
# para não sobrar espaço em branco.
monta_figura <- function(paineis, arquivo, altura_baixo_cm = 3.6) {
  n <- length(paineis)
  prop <- diff(lim_y) / diff(lim_x)                  # altura/largura do painel
  alt_mapa <- (LARGURA_CM - 0.3 * n) / n * prop + 0.5
  altura_cm <- alt_mapa + altura_baixo_cm

  # Grade de 2n colunas: cada mapa ocupa 2 colunas na linha de cima; na de
  # baixo, a rosa, o texto e a legenda dividem a largura.
  C <- 2 * n
  c_rosa <- max(1, floor(0.12 * C)); c_txt <- max(c_rosa + 2, round(0.48 * C))
  desenho <- c(
    lapply(seq_len(n), function(i) patchwork::area(1, 2 * i - 1, 1, 2 * i)),
    list(patchwork::area(2, 1, 2, c_rosa),
         patchwork::area(2, c_rosa + 1, 2, c_txt),
         patchwork::area(2, c_txt + 1, 2, C)))
  desenho <- do.call(c, desenho)

  texto <- ggplot() +
    annotate("text", x = 0, y = 0.5, label = TEXTO_INFO, hjust = 0, size = 2.1,
             lineheight = 1.1, family = FONTE) +
    xlim(0, 1) + ylim(0, 1) + theme_void()

  fig <- wrap_plots(c(paineis, list(rosa_dos_ventos(), texto, guide_area()))) +
    plot_layout(design = desenho, guides = "collect",
                heights = c(alt_mapa, altura_baixo_cm))
  ggsave(arquivo, fig, width = LARGURA_CM, height = altura_cm, units = "cm", dpi = 300,
         bg = "white")
  message("Salvo: ", arquivo)
}


# =============================================================================
# 5. Figura 2 — Uso e cobertura por ano
# =============================================================================
anos_m <- intersect(ANOS_MAPAS, anos_disponiveis())
dfs_uso <- lapply(anos_m, function(a) raster_utm_df(arq_recorte(a)))

# Classes presentes em algum ano, na ordem da legenda oficial.
cod_presentes <- sort(unique(unlist(lapply(dfs_uso, `[[`, "valor"))))
lu <- leg_uso |> filter(codigo %in% cod_presentes)
pal_uso <- setNames(lu$cor_fig, lu$classe)

paineis_uso <- lapply(seq_along(anos_m), function(i) {
  d <- dfs_uso[[i]]
  d$classe <- factor(d$valor, levels = lu$codigo, labels = lu$classe)
  painel(list(geom_raster(data = d, aes(x, y, fill = classe)), escala_fill(pal_uso)),
         letra = sprintf("(%s) %d", letters[i], anos_m[i]), escala = (i == 1))
})
monta_figura(paineis_uso, file.path(DIR_FIG_ORIG, "fig2_uso_cobertura.png"))


# =============================================================================
# 6. Figura 4 — Classes de mudança por período
# =============================================================================
arqs_mud <- list.files(DIR_RAS, pattern = "^mudanca_\\d{4}-\\d{4}\\.tif$", full.names = TRUE)
per <- sub("^mudanca_(.*)\\.tif$", "\\1", basename(arqs_mud))
completo <- paste0(min(anos_m), "-", max(anos_m))
per_sel <- sort(setdiff(per, completo))
if (INCLUIR_PERIODO_COMPLETO && completo %in% per) per_sel <- c(per_sel, completo)

dfs_mud <- lapply(per_sel, function(p) {
  d <- raster_utm_df(file.path(DIR_RAS, paste0("mudanca_", p, ".tif")))
  d |> left_join(mud_fig |> select(valor = id, categoria), by = "valor") |>
    mutate(categoria = factor(categoria, levels = ordem_cat))
})
cats_pres <- ordem_cat[ordem_cat %in% unique(unlist(lapply(dfs_mud, function(d) as.character(d$categoria))))]

paineis_mud <- lapply(seq_along(per_sel), function(i) {
  d <- dfs_mud[[i]]
  # mesmas categorias (níveis) em todos os painéis, para a legenda ser única
  d$categoria <- factor(as.character(d$categoria), levels = cats_pres)
  painel(list(geom_raster(data = d, aes(x, y, fill = categoria)),
              escala_fill(pal_mud[cats_pres])),
         letra = sprintf("(%s) %s", letters[i], per_sel[i]), escala = (i == 1))
})
monta_figura(paineis_mud, file.path(DIR_FIG_ORIG, "fig4_mudancas.png"))


# =============================================================================
# 7. Figura 5 — Gráficos de rosca (mesmas cores e classes do mapa)
# =============================================================================
# O parecerista B apontou que as cores da Figura 5 não batiam com as do
# mapa. Aqui a rosca usa exatamente a paleta e as categorias da Figura 4.
mud_tab <- read_csv2(file.path(DIR_TAB, "classes_mudanca_area.csv"), show_col_types = FALSE) |>
  filter(periodo %in% per_sel) |>
  left_join(mud_fig |> select(id, categoria), by = "id") |>
  group_by(periodo, categoria) |>
  summarise(area_ha = sum(area_ha), .groups = "drop") |>
  group_by(periodo) |>
  mutate(pct = 100 * area_ha / sum(area_ha)) |>
  ungroup() |>
  mutate(categoria = factor(categoria, levels = ordem_cat),
         periodo = factor(periodo, levels = per_sel),
         rotulo = ifelse(pct >= 1, paste0(format(round(pct, 2), decimal.mark = ","), "%"), ""))

g_rosca <- ggplot(mud_tab, aes(x = 2, y = pct, fill = categoria)) +
  geom_col(colour = "white", linewidth = 0.4, width = 1) +
  # rótulos de % do lado de fora do anel
  geom_text(aes(x = 2.85, label = rotulo), position = position_stack(vjust = 0.5),
            size = 2.1, family = FONTE) +
  # nome do período no centro
  geom_text(data = distinct(mud_tab, periodo), aes(x = 0.4, y = 0, label = periodo),
            inherit.aes = FALSE, size = 2.4, fontface = "bold", family = FONTE) +
  coord_polar(theta = "y", clip = "off") +
  xlim(0.4, 3) +
  scale_fill_manual(values = pal_mud, drop = TRUE, name = NULL) +
  facet_wrap(~ periodo, nrow = 1) +
  guides(fill = guide_legend(ncol = 3)) +
  theme_void(base_family = FONTE) +
  theme(strip.text = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.33, "cm"))
ggsave(file.path(DIR_FIG_ORIG, "fig5_mudancas_rosca.png"), g_rosca,
       width = LARGURA_CM, height = 6.5, units = "cm", dpi = 300, bg = "white")
message("Salvo: fig5_mudancas_rosca.png")


# =============================================================================
# 8. Figura 6 — Praia/duna persistente sobreposta por período
# =============================================================================
# Como no original: município em branco e, em cada painel, a praia/duna
# persistente do período anterior (por baixo) e a do período atual (por
# cima), para mostrar o deslocamento da areia. O painel (a) mostra só o
# primeiro período.
id_duna <- 5  # "Praia/duna persistente" (antes "Dunas fixas")
dfs_duna <- lapply(per_sel, function(p) {
  d <- raster_utm_df(file.path(DIR_RAS, paste0("mudanca_", p, ".tif")))
  d <- d[d$valor == id_duna, c("x", "y")]
  d$periodo <- p
  d
})
rot_duna <- paste0("Praia/duna persistente (", per_sel, ")")
pal_duna <- setNames(cores_periodo_duna[seq_along(per_sel)], rot_duna)

paineis_duna <- lapply(seq_along(per_sel), function(i) {
  camadas <- if (i == 1) dfs_duna[1] else dfs_duna[c(i - 1, i)]
  d <- do.call(rbind, camadas)
  d$rotulo <- factor(paste0("Praia/duna persistente (", d$periodo, ")"), levels = rot_duna)
  painel(list(geom_raster(data = d, aes(x, y, fill = rotulo)), escala_fill(pal_duna, ncol = 1)),
         letra = sprintf("(%s) %s", letters[i], per_sel[i]), escala = (i == 1),
         fundo_municipio = "#f7f8f3")
})
monta_figura(paineis_duna, file.path(DIR_FIG_ORIG, "fig6_praia_duna.png"))

cat("\nFiguras no estilo original salvas em:", DIR_FIG_ORIG, "\n")
