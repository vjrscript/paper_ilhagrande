# =============================================================================
# 07_figuras_artigo.R — Figuras finais para o texto do artigo
#
# Monta, no formato de publicação, as figuras do texto que saem do R
# (as Figuras 1, 2 e 4 — mapas — são feitas no QGIS):
#   Figura 3 — Séries das 8 classes principais com tendência (MK, Sen, Pettitt)
#   Figura 5 — Intensidade da mudança: (a) ritmo por período;
#              (b) de quais classes veio a pastagem
#   Figura 6 — Pecuária × pastagem: (a) composição do rebanho;
#              (b) pastagem mapeada × rebanho em unidades animais
#   Figura 7 — Contexto socioeconômico: (a) estrutura do PIB (VAB por setor);
#              (b) área urbanizada × população
#
# Entrada : tabelas dos scripts 02, 03 e 06 (rode o 06 antes)
# Saída   : resultados/figuras/artigo/ (PNG 300 dpi; TIFF opcional)
#
# Fonte: Georgia, a mesma do corpo do manuscrito (o texto usa Georgia 12 pt;
# legendas de figura, 10 pt). O pacote ragg usa as fontes instaladas no
# Windows diretamente, sem configuração extra.
#   install.packages(c("ragg", "patchwork"))
# =============================================================================

DIR_SCRIPTS <- "C:/Users/vj_ac/Downloads/Art_ruan/scripts_R"
if (!file.exists("00_config.R")) setwd(DIR_SCRIPTS)
source("00_config.R")
for (p in c("ragg", "patchwork")) if (!requireNamespace(p, quietly = TRUE)) try(install.packages(p))
library(patchwork)


# =============================================================================
# 1. OPÇÕES
# =============================================================================
FONTE      <- "Georgia"   # fonte do manuscrito
TAM_BASE   <- 8           # pt do texto dos gráficos (eixos ~7 pt, títulos 8 pt)
LARGURA_CM <- 16          # largura útil da página (A4 com margens de ~2,5 cm)
SALVAR_TIFF <- FALSE      # TRUE = também salva .tif (algumas revistas exigem)

# As 8 classes da Figura 3 (códigos da legenda), na ordem dos painéis.
CLASSES_FIG3 <- c(4, 3, 50, 15, 23, 24, 33, 91)
# Formação Savânica, Formação Florestal, Restinga Herbácea ou Arbustiva,
# Pastagem, Praia/Duna/Areal, Área Urbanizada, Rio/Lago/Oceano, Parque Eólico

DIR_TAB_A <- file.path(DIR_TAB, "analise")
DIR_ART   <- file.path(DIR_FIG, "artigo")
dir.create(DIR_ART, showWarnings = FALSE, recursive = TRUE)
le <- function(nome, pasta = DIR_TAB_A) suppressMessages(read_csv2(file.path(pasta, nome), show_col_types = FALSE))

# Confere se as tabelas do script 06 (versão atual) existem antes de começar.
necessarios <- c("C_tendencias_classes.csv", "B1_intensidade_intervalo.csv",
                 "B3_intensidade_transicao.csv", "E1_rebanhos_cabecas_ua.csv",
                 "E1_pastagem_x_rebanho_ua.csv", "E4_urbano_x_populacao.csv",
                 "E5_vab_setores.csv")
faltando <- necessarios[!file.exists(file.path(DIR_TAB_A, necessarios))]
if (length(faltando) > 0) {
  stop("Tabelas do script 06 ausentes: ", paste(faltando, collapse = ", "),
       ".\nRode a versão mais recente do 06_analise.R antes do 07.", call. = FALSE)
}


# =============================================================================
# 2. Estilo comum
# =============================================================================
# Tema limpo, sem título interno (o título vai na legenda da figura no
# Word), letras (a), (b) em negrito no canto superior esquerdo.
tema <- theme_bw(base_size = TAM_BASE, base_family = FONTE) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.25, colour = "grey88"),
        panel.border = element_rect(linewidth = 0.4, colour = "grey30"),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = TAM_BASE),
        axis.text = element_text(size = TAM_BASE - 1, colour = "grey15"),
        axis.title = element_text(size = TAM_BASE),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = TAM_BASE - 1),
        legend.key.size = unit(0.35, "cm"),
        plot.tag = element_text(face = "bold", size = TAM_BASE + 1, family = FONTE),
        plot.margin = margin(4, 6, 4, 4))
theme_set(tema)
update_geom_defaults("text", list(family = FONTE))

num_br <- label_number(big.mark = ".", decimal.mark = ",")
virgula <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = ",", big.mark = ".")
tags <- plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")

# Salva com o ragg (usa a fonte Georgia do sistema). Sem ragg, cai no
# dispositivo padrão — nesse caso a fonte pode não ser a Georgia.
salva <- function(g, nome, altura_cm) {
  arq <- file.path(DIR_ART, paste0(nome, ".png"))
  if (requireNamespace("ragg", quietly = TRUE)) {
    ggsave(arq, g, device = ragg::agg_png, width = LARGURA_CM, height = altura_cm,
           units = "cm", res = 300, bg = "white")
    if (SALVAR_TIFF) ggsave(sub("png$", "tif", arq), g, device = ragg::agg_tiff,
                            width = LARGURA_CM, height = altura_cm, units = "cm",
                            res = 300, bg = "white", compression = "lzw")
  } else {
    warning("Pacote ragg ausente: a fonte Georgia pode não ser aplicada.")
    ggsave(arq, g, width = LARGURA_CM, height = altura_cm, units = "cm", dpi = 300, bg = "white")
  }
  message("Salvo: ", arq)
}


# =============================================================================
# 3. Figura 3 — Séries das classes principais com tendência
# =============================================================================
areas <- le("areas_classes_longa.csv", DIR_TAB)
tend  <- le("C_tendencias_classes.csv")
leg3  <- legenda |> filter(codigo %in% CLASSES_FIG3) |>
  slice(match(CLASSES_FIG3, codigo)) |> filter(!is.na(codigo))

s3 <- areas |>
  filter(codigo %in% leg3$codigo) |>
  select(ano, codigo, area_ha) |>
  complete(ano, codigo = leg3$codigo, fill = list(area_ha = 0)) |>
  left_join(leg3 |> select(codigo, classe), by = "codigo") |>
  mutate(classe = factor(classe, levels = leg3$classe))

# Texto de cada painel: declive de Sen, p do Mann-Kendall e, se houver,
# o ano de ruptura (Pettitt, p < 0,05).
r3 <- tend |> filter(codigo %in% leg3$codigo) |>
  left_join(leg3 |> select(codigo), by = "codigo") |>
  mutate(classe = factor(classe, levels = leg3$classe),
         p_txt = ifelse(p_mk < 0.001, "< 0,001", virgula(p_mk, 3)),
         rotulo = paste0("Sen = ", virgula(sen_ha_ano, 1), " ha/ano\n",
                         "p (MK) ", ifelse(p_mk < 0.001, "", "= "), p_txt,
                         ifelse(p_pettitt < 0.05, paste0("\nRuptura: ", ano_ruptura), "")))

g3 <- ggplot(s3, aes(ano, area_ha, colour = classe)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1) +
  geom_text(data = r3, aes(x = -Inf, y = Inf, label = rotulo), inherit.aes = FALSE,
            hjust = -0.06, vjust = 1.15, size = (TAM_BASE - 1.5) / .pt, lineheight = 0.95,
            family = FONTE) +
  scale_colour_manual(values = setNames(leg3$cor, leg3$classe), guide = "none") +
  scale_y_continuous(labels = num_br, expand = expansion(mult = c(0.08, 0.55))) +
  scale_x_continuous(breaks = c(1985, 2005, 2025)) +
  # quebra nomes longos em duas linhas (ex.: "Restinga Herbácea ou Arbustiva")
  facet_wrap(~ classe, scales = "free_y", ncol = 4, labeller = label_wrap_gen(width = 14)) +
  labs(x = NULL, y = "Área (ha)")
salva(g3, "Figura3_series_tendencia", altura_cm = 10.5)


# =============================================================================
# 4. Figura 5 — Intensidade da mudança
# =============================================================================
# (a) Intensidade anual de mudança por período (S_t) × taxa uniforme (U).
b1 <- le("B1_intensidade_intervalo.csv")
U  <- b1$U[1]
g5a <- ggplot(b1, aes(periodo, S_t, fill = ritmo)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = U, linetype = "dashed", linewidth = 0.4) +
  annotate("text", x = 0.45, y = U, label = paste0("Taxa uniforme (", virgula(U), "%/ano)"),
           hjust = 0, vjust = -0.6, size = (TAM_BASE - 1) / .pt, family = FONTE) +
  scale_fill_manual(values = c("rápido" = "#b2182b", "lento" = "#92c5de"),
                    labels = c("rápido" = "Acima da taxa uniforme (rápido)",
                               "lento" = "Abaixo da taxa uniforme (lento)")) +
  scale_y_continuous(labels = function(x) virgula(x, 1), expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "Área alterada (%/ano)")

# (b) De quais classes veio a pastagem: intensidade observada × uniforme.
# Períodos em que a pastagem ganhou menos de 10 ha ficam de fora: a
# intensidade relativa fica enorme com áreas ínfimas (ex.: 0,45 ha em
# 1985–1995) e passa uma impressão errada.
b3 <- le("B3_intensidade_transicao.csv") |>
  filter(alvo == "Pastagem", virou_alvo_ha > 0) |>
  group_by(periodo) |> filter(sum(virou_alvo_ha) >= 10) |> ungroup()
ordem_b3 <- b3 |> group_by(de_classe) |> summarise(t = sum(virou_alvo_ha)) |>
  arrange(t) |> pull(de_classe)
b3 <- b3 |> mutate(de_classe = factor(de_classe, levels = ordem_b3),
                   comportamento = ifelse(grepl("^alvo", comportamento), "Alvo", "Evitada"))
g5b <- ggplot(b3, aes(R, de_classe, fill = comportamento)) +
  geom_col(width = 0.7) +
  geom_vline(aes(xintercept = W), linetype = "dashed", linewidth = 0.4) +
  scale_fill_manual(values = c("Alvo" = "#b2182b", "Evitada" = "#92c5de"),
                    labels = c("Alvo" = "Transição alvo (acima do uniforme)",
                               "Evitada" = "Transição evitada (abaixo do uniforme)")) +
  # 2 algarismos significativos: no 1º período as intensidades são minúsculas
  scale_x_continuous(labels = function(x) format(signif(x, 2), decimal.mark = ",",
                                                 scientific = FALSE, drop0trailing = TRUE),
                     n.breaks = 3) +
  facet_wrap(~ periodo, nrow = 1, scales = "free_x") +
  labs(x = "Intensidade da conversão para Pastagem (% da classe de origem ao ano)", y = NULL)

# free(): o painel (a) não é alinhado aos rótulos longos do (b)
g5 <- (free(g5a) / g5b) + plot_layout(heights = c(1, 1.15)) + tags
salva(g5, "Figura5_intensidade", altura_cm = 15)


# =============================================================================
# 5. Figura 6 — Pecuária × pastagem
# =============================================================================
# (a) Composição do rebanho que pasteja (cabeças), com os Censos Agro.
reb <- le("E1_rebanhos_cabecas_ua.csv")
tipos <- intersect(c("Bovino", "Bubalino", "Equino", "Ovino", "Caprino"), names(reb))
reb_l <- reb |> select(ano, all_of(tipos)) |>
  pivot_longer(-ano, names_to = "tipo", values_to = "cabecas") |>
  filter(!is.na(cabecas)) |>
  group_by(tipo) |> filter(sum(cabecas) > 0) |> ungroup() |>
  mutate(tipo = factor(tipo, levels = tipos))
cores_reb <- c(Bovino = "#8c510a", Bubalino = "#543005", Equino = "#bf812d",
               Ovino = "#80cdc1", Caprino = "#35978f")
anos_censo <- c(2006, 2017)
g6a <- ggplot(reb_l, aes(ano, cabecas, fill = tipo)) +
  geom_area(colour = "white", linewidth = 0.2) +
  geom_vline(xintercept = anos_censo + 0.5, linetype = "dotted", linewidth = 0.4) +
  annotate("text", x = anos_censo + 0.8, y = Inf, vjust = 1.4, hjust = 0,
           label = paste("Censo Agropecuário", anos_censo), size = (TAM_BASE - 1.5) / .pt,
           family = FONTE) +
  scale_fill_manual(values = cores_reb) +
  scale_y_continuous(labels = num_br, expand = expansion(mult = c(0, 0.08))) +
  labs(x = NULL, y = "Efetivo (cabeças)")

# (b) Pastagem mapeada (barras, eixo esquerdo) × rebanho em UA (linha, eixo
# direito). O eixo secundário é só uma reescala; o subtítulo traz o rho.
e1 <- le("E1_pastagem_x_rebanho_ua.csv")
fat <- max(e1$ibge, na.rm = TRUE) / max(e1$mapbiomas_ha, na.rm = TRUE)
comum <- e1 |> filter(!is.na(mapbiomas_ha), !is.na(ibge))
rho <- if (nrow(comum) >= 4) suppressWarnings(cor.test(comum$mapbiomas_ha, comum$ibge,
                                                       method = "spearman")) else NULL
sub6 <- if (!is.null(rho)) sprintf("Spearman \u03c1 = %s (p = %s; n = %d)",
                                   virgula(rho$estimate, 2), virgula(rho$p.value, 2),
                                   nrow(comum)) else NULL
g6b <- ggplot(e1, aes(ano)) +
  geom_col(data = filter(e1, !is.na(mapbiomas_ha)), aes(y = mapbiomas_ha, fill = "Pastagem mapeada (ha)"),
           width = 0.8) +
  geom_line(data = filter(e1, !is.na(ibge)), aes(y = ibge / fat, colour = "Rebanho (unidades animais)"),
            linewidth = 0.7) +
  scale_fill_manual(values = c("Pastagem mapeada (ha)" = "#edde8e")) +
  scale_colour_manual(values = c("Rebanho (unidades animais)" = "#8c510a")) +
  scale_y_continuous(name = "Pastagem (ha)", labels = num_br,
                     sec.axis = sec_axis(~ . * fat, name = "Rebanho (UA)", labels = num_br),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(x = NULL, subtitle = sub6) +
  theme(plot.subtitle = element_text(size = TAM_BASE - 1, family = FONTE))

g6 <- (g6a / g6b) + tags
salva(g6, "Figura6_pecuaria_pastagem", altura_cm = 14)


# =============================================================================
# 6. Figura 7 — Contexto socioeconômico
# =============================================================================
# (a) Estrutura do valor adicionado bruto por setor (%).
vab <- le("E5_vab_setores.csv") |>
  mutate(setor = factor(setor, levels = c("Agropecuária", "Indústria", "Serviços",
                                          "Administração pública")))
g7a <- ggplot(vab, aes(ano, pct, fill = setor)) +
  geom_area(colour = "white", linewidth = 0.2) +
  scale_fill_manual(values = c("Agropecuária" = "#a6d96a", "Indústria" = "#2c7bb6",
                               "Serviços" = "#fdae61", "Administração pública" = "#d7d7d7")) +
  scale_y_continuous(expand = c(0, 0), labels = function(x) paste0(x, "%")) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = NULL, y = "Valor adicionado bruto (%)")

# (b) Área urbanizada (MapBiomas) e população (IBGE), em dois painéis com o
# mesmo eixo de anos.
e4 <- le("E4_urbano_x_populacao.csv") |>
  pivot_longer(c(mapbiomas_ha, ibge), names_to = "serie", values_to = "valor") |>
  filter(!is.na(valor), ano >= 1985) |>
  mutate(serie = factor(ifelse(serie == "ibge", "População (habitantes)", "Área urbanizada (ha)"),
                        levels = c("Área urbanizada (ha)", "População (habitantes)")))
g7b <- ggplot(e4, aes(ano, valor, colour = serie)) +
  geom_line(linewidth = 0.6) + geom_point(size = 0.9) +
  scale_colour_manual(values = c("Área urbanizada (ha)" = "#d4271e",
                                 "População (habitantes)" = "grey25"), guide = "none") +
  scale_y_continuous(labels = num_br) +
  facet_wrap(~ serie, ncol = 2, scales = "free_y") +
  labs(x = NULL, y = NULL)

g7 <- (g7a / g7b) + plot_layout(heights = c(1.1, 1)) + tags
salva(g7, "Figura7_socioeconomico", altura_cm = 13)

cat("\nFiguras do artigo salvas em:", DIR_ART, "\n")
