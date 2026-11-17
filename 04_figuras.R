# =============================================================================
# 04_figuras.R — Mapas e gráficos do artigo
#
# Entrada : recortes (01), tabelas (02) e rasters de mudança (03)
# Saída   : DIR_FIG/ (PNG, 300 dpi)
#   fig_uso_cobertura.png     -> mapas de uso e cobertura, um painel por ano
#                                (a), (b), (c)... — substitui a Figura 2
#   fig_mudancas.png          -> mapas das classes de mudança por período
#                                — substitui a Figura 4
#   fig_serie_classes.png     -> gráfico de linhas da área de cada classe ao
#                                longo da série (pedido do parecerista H)
#   fig_mudancas_barras.png   -> área das classes de mudança por período,
#                                com as MESMAS cores do mapa — substitui a
#                                Figura 5 (pedido do parecerista B)
#   fig_praia_duna.png        -> ganho, perda e persistência de praia/duna
#                                — substitui a Figura 6
#
# Escala gráfica e seta de norte usam o pacote ggspatial, se instalado:
#   install.packages("ggspatial")
# =============================================================================

# Garante que o R está na pasta dos scripts (necessário para achar o
# 00_config.R). Se você mudar a pasta de lugar, ajuste o caminho abaixo.
DIR_SCRIPTS <- "C:/Users/vj_ac/Downloads/Art_ruan/scripts_R"
if (!file.exists("00_config.R")) setwd(DIR_SCRIPTS)
source("00_config.R")
tem_ggspatial <- requireNamespace("ggspatial", quietly = TRUE)
if (!tem_ggspatial) message("ggspatial não instalado: mapas sairão sem escala e norte.")


# -----------------------------------------------------------------------------
# 1. Insumos comuns
# -----------------------------------------------------------------------------
anos_m <- intersect(ANOS_MAPAS, anos_disponiveis())
limite <- st_read(ARQ_LIMITE, quiet = TRUE) |> st_union() |> st_as_sf() |>
  st_transform(4326)

# Rótulos (a), (b), (c)... para os painéis (parecerista B: "completar o
# título com as letras").
rotula <- function(x) paste0("(", letters[seq_along(x)], ") ", x)

# Tema de mapa: fundo limpo, coordenadas pequenas, legenda embaixo.
tema_mapa <- theme_bw(base_size = 9) +
  theme(panel.grid = element_line(linewidth = 0.2, colour = "grey85"),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text = element_text(size = 6),
        axis.title = element_blank())

# Poucas marcas de longitude (a cada ~0,05°), calculadas a partir da
# extensão do município, para os rótulos não se sobreporem nos painéis.
eixos_mapa <- scale_x_continuous(breaks = pretty(st_bbox(limite)[c("xmin", "xmax")], n = 2))

# Acrescenta escala gráfica e seta de norte (só no primeiro painel não dá
# no facet; o ggspatial desenha em todos — aceitável para o artigo, ou
# remova e acrescente uma vez na edição final).
extras_mapa <- function() {
  if (!tem_ggspatial) return(NULL)
  list(ggspatial::annotation_scale(location = "bl", width_hint = 0.3,
                                   text_cex = 0.5, height = unit(0.1, "cm")),
       ggspatial::annotation_north_arrow(location = "tr",
                                         height = unit(0.6, "cm"), width = unit(0.5, "cm"),
                                         style = ggspatial::north_arrow_minimal(text_size = 6)))
}

# Converte um raster em data.frame (x, y, valor) para o ggplot. A área é
# pequena (~150 mil pixels por ano), então geom_tile dá conta.
raster_df <- function(arq, rotulo) {
  r <- rast(arq)
  d <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(d)[3] <- "valor"
  d$painel <- rotulo
  d
}


# -----------------------------------------------------------------------------
# 2. Mapas de uso e cobertura (nova Figura 2)
# -----------------------------------------------------------------------------
df_uso <- bind_rows(lapply(seq_along(anos_m), function(i)
  raster_df(arq_recorte(anos_m[i]), rotula(anos_m)[i])))

# Só as classes que aparecem de fato no município entram na legenda, na
# ordem dos códigos da legenda oficial.
leg_uso <- legenda |> filter(codigo %in% unique(df_uso$valor))
df_uso$classe <- factor(df_uso$valor, levels = leg_uso$codigo, labels = leg_uso$classe)

g_uso <- ggplot() +
  geom_tile(data = df_uso, aes(x, y, fill = classe)) +
  geom_sf(data = limite, fill = NA, colour = "black", linewidth = 0.3) +
  scale_fill_manual(values = setNames(leg_uso$cor, leg_uso$classe), drop = TRUE) +
  facet_wrap(~ painel, ncol = 3) +
  coord_sf(expand = FALSE) +
  eixos_mapa +
  extras_mapa() +
  tema_mapa +
  guides(fill = guide_legend(ncol = 4))
ggsave(file.path(DIR_FIG, "fig_uso_cobertura.png"), g_uso,
       width = 17, height = 13, units = "cm", dpi = 300)


# -----------------------------------------------------------------------------
# 3. Mapas das classes de mudança (nova Figura 4)
# -----------------------------------------------------------------------------
arqs_mud <- list.files(DIR_RAS, pattern = "^mudanca_\\d{4}-\\d{4}\\.tif$", full.names = TRUE)
per_mud  <- sub("^mudanca_(.*)\\.tif$", "\\1", basename(arqs_mud))

# Os períodos consecutivos vêm primeiro; o período completo, por último.
completo <- paste0(min(anos_m), "-", max(anos_m))
ordem    <- c(setdiff(sort(per_mud), completo), intersect(completo, per_mud))
arqs_mud <- arqs_mud[match(ordem, per_mud)]

df_mud <- bind_rows(lapply(seq_along(ordem), function(i)
  raster_df(arqs_mud[i], rotula(ordem)[i])))
leg_mud <- classes_mudanca |> filter(id %in% unique(df_mud$valor))
df_mud$mudanca <- factor(df_mud$valor, levels = leg_mud$id, labels = leg_mud$mudanca)
pal_mud <- setNames(leg_mud$cor_mud, leg_mud$mudanca)

g_mud <- ggplot() +
  geom_tile(data = df_mud, aes(x, y, fill = mudanca)) +
  geom_sf(data = limite, fill = NA, colour = "black", linewidth = 0.3) +
  scale_fill_manual(values = pal_mud) +
  facet_wrap(~ painel, ncol = 3) +
  coord_sf(expand = FALSE) +
  eixos_mapa +
  extras_mapa() +
  tema_mapa +
  guides(fill = guide_legend(ncol = 3))
ggsave(file.path(DIR_FIG, "fig_mudancas.png"), g_mud,
       width = 17, height = 13, units = "cm", dpi = 300)


# -----------------------------------------------------------------------------
# 4. Série temporal da área por classe (gráfico de linhas)
# -----------------------------------------------------------------------------
# Usa todos os anos disponíveis (ANOS_SERIE). Cada classe em seu próprio
# painel com eixo y livre: classes pequenas (ex.: Área Urbanizada) ficam
# legíveis ao lado das grandes (ex.: Formação Savânica).
areas <- read_csv2(file.path(DIR_TAB, "areas_classes_longa.csv"), show_col_types = FALSE)
leg_s <- legenda |> filter(codigo %in% unique(areas$codigo))
# O fator é montado pelo CÓDIGO (não pelo nome), para não depender de acentos.
areas$classe <- factor(areas$codigo, levels = leg_s$codigo, labels = leg_s$classe)

g_serie <- ggplot(areas, aes(ano, area_ha, colour = classe)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1) +
  # Linhas verticais marcam os anos dos mapas.
  geom_vline(xintercept = anos_m, linetype = "dotted", colour = "grey60", linewidth = 0.3) +
  scale_colour_manual(values = setNames(leg_s$cor, leg_s$classe), guide = "none") +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  facet_wrap(~ classe, scales = "free_y", ncol = 3) +
  labs(x = NULL, y = "Área (ha)") +
  theme_bw(base_size = 9) +
  theme(strip.background = element_blank(), strip.text = element_text(face = "bold"))
ggsave(file.path(DIR_FIG, "fig_serie_classes.png"), g_serie,
       width = 17, height = 14, units = "cm", dpi = 300)


# -----------------------------------------------------------------------------
# 5. Barras das classes de mudança (nova Figura 5, mesmas cores do mapa)
# -----------------------------------------------------------------------------
# Exclui as classes "persistentes" de vegetação e água, que dominam a área e
# achatam as demais; elas continuam na tabela classes_mudanca_larga.csv.
mud <- read_csv2(file.path(DIR_TAB, "classes_mudanca_area.csv"), show_col_types = FALSE) |>
  filter(!mudanca %in% c("Vegetação natural persistente", "Água persistente")) |>
  mutate(periodo = factor(periodo, levels = ordem),
         mudanca = factor(mudanca, levels = classes_mudanca$mudanca))

g_bar <- ggplot(mud, aes(periodo, area_ha, fill = mudanca)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  scale_fill_manual(values = setNames(classes_mudanca$cor_mud, classes_mudanca$mudanca)) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(x = NULL, y = "Área (ha)", fill = NULL) +
  theme_bw(base_size = 9) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 3))
ggsave(file.path(DIR_FIG, "fig_mudancas_barras.png"), g_bar,
       width = 17, height = 10, units = "cm", dpi = 300)


# -----------------------------------------------------------------------------
# 6. Dinâmica de praia/duna (nova Figura 6)
# -----------------------------------------------------------------------------
# Recorta os mapas de mudança só nas três classes ligadas a praia/duna, para
# mostrar onde a areia avançou, onde recuou e onde permaneceu.
df_duna <- df_mud |>
  filter(mudanca %in% c("Praia/duna persistente", "Ganho de praia/duna", "Perda de praia/duna")) |>
  droplevels()

g_duna <- ggplot() +
  geom_sf(data = limite, fill = "grey95", colour = "black", linewidth = 0.3) +
  geom_tile(data = df_duna, aes(x, y, fill = mudanca)) +
  scale_fill_manual(values = pal_mud) +
  facet_wrap(~ painel, ncol = 3) +
  coord_sf(expand = FALSE) +
  eixos_mapa +
  extras_mapa() +
  tema_mapa
ggsave(file.path(DIR_FIG, "fig_praia_duna.png"), g_duna,
       width = 17, height = 13, units = "cm", dpi = 300)

cat("Figuras salvas em:", DIR_FIG, "\n")
