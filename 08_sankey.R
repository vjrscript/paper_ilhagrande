# =============================================================================
# 08_sankey.R — Diagrama aluvial (Sankey) das trajetórias 1985–2025
#
# Mostra "quem virou quem" ao longo de 1985 → 1995 → 2005 → 2015 → 2025,
# na versão recomendada:
#   1. Classes agrupadas em 7 grupos (com 17 classes o gráfico fica ilegível).
#   2. Trajetórias calculadas PIXEL A PIXEL: cada pixel tem sua sequência
#      de grupos nos 5 anos. (Encadear matrizes de períodos consecutivos
#      sugeriria trajetórias que não existem.)
#   3. Fluxos que NÃO mudam de grupo entre dois anos ficam em cinza claro;
#      só as mudanças aparecem coloridas, com a cor do grupo de origem.
#
# Entrada : recortes do script 01 (mb_col11_recorte/ig_col11_AAAA.tif)
# Saída   : resultados/figuras/artigo/Figura_sankey_trajetorias.png
#           resultados/tabelas/analise/sankey_trajetorias.csv
#           resultados/tabelas/analise/sankey_trajetorias_mais_comuns.csv
#
# Pacotes extras: ggalluvial, ragg (fonte Georgia)
#   install.packages(c("ggalluvial", "ragg"))
# =============================================================================

DIR_SCRIPTS <- "C:/Users/vj_ac/Downloads/Art_ruan/scripts_R"
if (!file.exists("00_config.R")) setwd(DIR_SCRIPTS)
source("00_config.R")
for (p in c("ggalluvial", "ragg")) if (!requireNamespace(p, quietly = TRUE)) try(install.packages(p))
library(ggalluvial)


# =============================================================================
# 1. OPÇÕES
# =============================================================================
FONTE      <- "Georgia"   # fonte do manuscrito
TAM_BASE   <- 8
LARGURA_CM <- 16
ALTURA_CM  <- 11
ANOS_SANKEY <- c(1985, 1995, 2005, 2015, 2025)

# Rótulo dentro das caixas só se o grupo tiver pelo menos esta área (ha);
# caixas menores ficam sem texto (a legenda identifica a cor).
MIN_ROTULO_HA <- 400

# Agrupamento das classes (código -> grupo). Ajuste se quiser outro corte.
# A ordem dos níveis define a ordem de cima para baixo no gráfico.
GRUPOS <- c(
  "Savana"                     = "4",
  "Floresta e mangue"          = "3,5,6,49",
  "Restinga herbácea"          = "50",
  "Campos e áreas úmidas"      = "11,12,13,29,32,77,84,7",
  "Praia e duna"               = "23",
  "Água"                       = "33,26",
  "Uso antrópico"              = "9,15,18,19,20,21,24,25,30,31,35,36,39,40,41,46,47,48,62,75,91"
)
CORES_GRUPOS <- c(
  "Savana"                = "#7dc975",
  "Floresta e mangue"     = "#1f8d49",
  "Restinga herbácea"     = "#ffaa5f",
  "Campos e áreas úmidas" = "#519799",
  "Praia e duna"          = "#ffa07a",
  "Água"                  = "#2532e4",
  "Uso antrópico"         = "#d4271e"
)
COR_PERMANENCIA <- "grey85"   # fluxos sem mudança de grupo

DIR_TAB_A <- file.path(DIR_TAB, "analise")
DIR_ART   <- file.path(DIR_FIG, "artigo")
for (d in c(DIR_TAB_A, DIR_ART)) dir.create(d, showWarnings = FALSE, recursive = TRUE)


# =============================================================================
# 2. Tabela código -> grupo
# =============================================================================
tab_grupo <- do.call(rbind, lapply(names(GRUPOS), function(g)
  data.frame(codigo = as.integer(strsplit(GRUPOS[[g]], ",")[[1]]), grupo = g)))
niveis <- names(GRUPOS)


# =============================================================================
# 3. Trajetórias pixel a pixel
# =============================================================================
# Empilha os 5 rasters (mesma grade, pois vêm do mesmo recorte), converte
# para tabela com uma linha por pixel e uma coluna por ano, e soma a área
# real de cada pixel (cellSize, em ha) por trajetória.
faltam <- ANOS_SANKEY[!file.exists(vapply(ANOS_SANKEY, arq_recorte, ""))]
if (length(faltam) > 0) stop("Recortes ausentes para: ", paste(faltam, collapse = ", "))

pilha <- rast(lapply(ANOS_SANKEY, function(a) rast(arq_recorte(a))))
names(pilha) <- paste0("a", ANOS_SANKEY)
cel <- cellSize(pilha[[1]], unit = "ha", mask = TRUE)
names(cel) <- "area_ha"
px <- as.data.frame(c(pilha, cel), na.rm = TRUE)

# Códigos -> grupos (pixels com código fora dos grupos, ex.: 27 Não
# Observado, saem da análise e são contados no aviso abaixo).
for (a in names(pilha)) px[[a]] <- tab_grupo$grupo[match(px[[a]], tab_grupo$codigo)]
fora <- sum(px$area_ha[!complete.cases(px)])
if (fora > 0) message(sprintf("%.1f ha com código fora dos grupos foram descartados.", fora))
px <- px[complete.cases(px), ]

traj <- px |>
  group_by(across(all_of(names(pilha)))) |>
  summarise(area_ha = sum(area_ha), n_pixels = n(), .groups = "drop") |>
  mutate(id = row_number(),
         n_mudancas = rowSums(across(all_of(names(pilha)[-1])) != across(all_of(names(pilha)[-length(names(pilha))]))))
write_excel_csv2(traj |> mutate(area_ha = round(area_ha, 2)),
                 file.path(DIR_TAB_A, "sankey_trajetorias.csv"))

# As 15 trajetórias com mudança que mais ocupam área: ajuda a escrever o
# texto ("X ha de savana viraram uso antrópico entre 2015 e 2025"...).
top <- traj |> filter(n_mudancas > 0) |> arrange(desc(area_ha)) |> head(15) |>
  mutate(trajetoria = do.call(paste, c(across(all_of(names(pilha))), sep = " → ")),
         area_ha = round(area_ha, 1),
         pct_municipio = round(100 * area_ha / sum(traj$area_ha), 2)) |>
  select(trajetoria, area_ha, pct_municipio, n_mudancas)
write_excel_csv2(top, file.path(DIR_TAB_A, "sankey_trajetorias_mais_comuns.csv"))
print(top, n = 15)

pct_estavel <- 100 * sum(traj$area_ha[traj$n_mudancas == 0]) / sum(traj$area_ha)
message(sprintf("Área sem nenhuma mudança de grupo em 1985–2025: %.1f%%", pct_estavel))


# =============================================================================
# 4. Formato "lodes" (uma linha por trajetória × ano) e cor dos fluxos
# =============================================================================
# Para cada trajetória e cada ano, marca se o pixel MUDA de grupo no
# próximo ano. O fluxo que sai daquele ano é colorido (cor do grupo de
# origem) se muda, ou cinza se permanece. Com aes.flow = "forward", o
# ggalluvial usa a estética do ano de origem de cada segmento.
lodes <- traj |>
  pivot_longer(all_of(names(pilha)), names_to = "ano", values_to = "grupo") |>
  mutate(ano = as.integer(sub("^a", "", ano)),
         grupo = factor(grupo, levels = niveis)) |>
  arrange(id, ano) |>
  group_by(id) |>
  mutate(proximo = lead(grupo),
         cor_fluxo = ifelse(!is.na(proximo) & proximo != grupo,
                            as.character(grupo), "Permanência")) |>
  ungroup() |>
  mutate(cor_fluxo = factor(cor_fluxo, levels = c(niveis, "Permanência")),
         ano = factor(ano, levels = ANOS_SANKEY))

# Rótulos das caixas: nome do grupo (só nas caixas grandes).
caixas <- lodes |> group_by(ano, grupo) |> summarise(area_ha = sum(area_ha), .groups = "drop")


# =============================================================================
# 5. Gráfico
# =============================================================================
num_br <- label_number(big.mark = ".", decimal.mark = ",")
pal_fluxo <- c(CORES_GRUPOS, "Permanência" = COR_PERMANENCIA)

g <- ggplot(lodes, aes(x = ano, stratum = grupo, alluvium = id, y = area_ha)) +
  # fluxos: permanência em cinza, mudanças na cor do grupo de origem
  # (não use colour = NA aqui: em algumas versões do ggalluvial isso
  # faz os fluxos desaparecerem)
  geom_flow(aes(fill = cor_fluxo), aes.flow = "forward", width = 0.22,
            alpha = 0.9, curve_type = "sigmoid") +
  # caixas de cada grupo em cada ano
  geom_stratum(aes(fill = grupo), width = 0.22, colour = "white", linewidth = 0.3) +
  geom_text(stat = "stratum",
            aes(label = ifelse(after_stat(count) >= MIN_ROTULO_HA,
                               as.character(after_stat(stratum)), "")),
            size = (TAM_BASE - 1.5) / .pt, family = FONTE, lineheight = 0.85) +
  scale_fill_manual(values = pal_fluxo, breaks = niveis, name = NULL) +
  scale_y_continuous(labels = num_br, expand = expansion(mult = c(0.01, 0.01))) +
  labs(x = NULL, y = "Área (ha)") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_minimal(base_size = TAM_BASE, base_family = FONTE) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = TAM_BASE, face = "bold", colour = "grey15"),
        axis.text.y = element_text(size = TAM_BASE - 1, colour = "grey30"),
        legend.position = "bottom",
        legend.text = element_text(size = TAM_BASE - 1),
        legend.key.size = unit(0.35, "cm"),
        plot.caption = element_text(size = TAM_BASE - 1.5, colour = "grey30", hjust = 0)) +
  labs(caption = sprintf(paste0("Fluxos coloridos = mudança de grupo (cor do grupo de origem); ",
                               "cinza = permanência.\n%s%% da área não mudou de grupo em 1985–2025."),
                         formatC(pct_estavel, format = "f", digits = 1, decimal.mark = ",")))

arq <- file.path(DIR_ART, "Figura_sankey_trajetorias.png")
if (requireNamespace("ragg", quietly = TRUE)) {
  ggsave(arq, g, device = ragg::agg_png, width = LARGURA_CM, height = ALTURA_CM,
         units = "cm", res = 300, bg = "white")
} else {
  ggsave(arq, g, width = LARGURA_CM, height = ALTURA_CM, units = "cm", dpi = 300, bg = "white")
}
message("Salvo: ", arq)
