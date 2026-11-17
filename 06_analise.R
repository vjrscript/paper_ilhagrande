# =============================================================================
# 06_analise.R — Análise dos resultados (responde aos pareceres B e H)
#
# Usa as tabelas já geradas pelos scripts 02 e 03 (MapBiomas Col. 11) e
# baixa do SIDRA/IBGE os dados socioeconômicos que os pareceristas pediram.
# Não faz mapas (eles serão feitos no QGIS); gera tabelas, gráficos e um
# resumo em texto com os números principais para a discussão.
#
# Blocos:
#   A. Ganhos, perdas, mudança líquida e troca (swap) por classe/período
#   B. Análise de intensidade (Aldwaik & Pontius, 2012): intervalo,
#      categoria e transição — "a mudança acelerou?", "quais classes
#      ganharam/perderam mais que o esperado?", "a pastagem avançou
#      preferencialmente sobre qual vegetação?"
#   C. Estatística de tendência das séries por classe: Mann-Kendall,
#      declive de Sen e teste de Pettitt (ano de ruptura)
#   D. Dados do IBGE via API do SIDRA: PAM, PPM, PIB municipal, população
#   E. Cruzamentos: pastagem × rebanhos (bovinos, caprinos, ovinos... em UA); agricultura × área plantada;
#      área urbanizada × população; parque eólico × VAB da indústria
#   F. Resumo em texto com os números-chave
#
# Saídas: resultados/tabelas/analise/, resultados/figuras/analise/,
#         resultados/resumo_analise.txt, dados_ibge/ (cache dos downloads)
#
# Pacotes extras: jsonlite (leitura da API do SIDRA).
# Requer internet só na primeira vez (os downloads ficam em cache).
# =============================================================================

DIR_SCRIPTS <- "C:/Users/vj_ac/Downloads/Art_ruan/scripts_R"
if (!file.exists("00_config.R")) setwd(DIR_SCRIPTS)
source("00_config.R")
if (!requireNamespace("jsonlite", quietly = TRUE)) try(install.packages("jsonlite"))

DIR_TAB_A <- file.path(DIR_TAB, "analise")
DIR_FIG_A <- file.path(DIR_FIG, "analise")
DIR_IBGE  <- file.path(DIR_BASE, "dados_ibge")
for (d in c(DIR_TAB_A, DIR_FIG_A, DIR_IBGE)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

COD_MUN <- "2204659"   # Ilha Grande (PI), código IBGE

# O último ano divulgado pelo IBGE é preliminar e às vezes vem incompleto
# (ex.: PAM 2025 com só parte das lavouras). TRUE = descarta o último ano de
# cada série do IBGE nos cruzamentos.
EXCLUIR_ANO_PRELIMINAR <- TRUE

# Tema dos gráficos e formatos numéricos em português.
tema <- theme_bw(base_size = 9) +
  theme(strip.background = element_blank(), strip.text = element_text(face = "bold"),
        legend.position = "bottom", panel.grid.minor = element_blank())
num_br <- label_number(big.mark = ".", decimal.mark = ",")
# Rótulos curtos para eixos com poucas marcas (evita números sobrepostos).
num_curto <- function(x) format(x, big.mark = ".", decimal.mark = ",", drop0trailing = TRUE,
                                scientific = FALSE, trim = TRUE)
salva_fig <- function(g, nome, w = 17, h = 10) {
  ggsave(file.path(DIR_FIG_A, nome), g, width = w, height = h, units = "cm", dpi = 300,
         bg = "white")
}
salva_tab <- function(df, nome) write_excel_csv2(df, file.path(DIR_TAB_A, nome))

# Linhas do resumo em texto (preenchidas ao longo do script).
resumo <- character()
anota <- function(...) { resumo <<- c(resumo, sprintf(...)); invisible() }


# =============================================================================
# 0. Leitura das tabelas do MapBiomas (scripts 02 e 03)
# =============================================================================
areas <- read_csv2(file.path(DIR_TAB, "areas_classes_longa.csv"), show_col_types = FALSE)
trans <- read_csv2(file.path(DIR_TAB, "transicoes_longa.csv"),   show_col_types = FALSE)

# Períodos consecutivos (ex.: 1985-1995 ... 2015-2025); o período completo
# fica de fora dos blocos A e B para não contar a mesma mudança duas vezes.
per_all  <- unique(trans$periodo)
ini_fim  <- do.call(rbind, strsplit(per_all, "-"))
per_tab  <- data.frame(periodo = per_all, ini = as.integer(ini_fim[, 1]),
                       fim = as.integer(ini_fim[, 2]))
completo <- per_tab$periodo[which.max(per_tab$fim - per_tab$ini)]
per_tab  <- per_tab[per_tab$periodo != completo, ]
per_tab  <- per_tab[order(per_tab$ini), ]
per_tab$dur <- per_tab$fim - per_tab$ini
trans_c  <- trans |> filter(periodo %in% per_tab$periodo) |>
  left_join(per_tab, by = "periodo") |>
  mutate(periodo = factor(periodo, levels = per_tab$periodo))
AREA_TOTAL <- trans |> filter(periodo == per_tab$periodo[1]) |> summarise(sum(area_ha)) |> pull()

anota("Área total analisada: %s ha", format(round(AREA_TOTAL), big.mark = "."))
anota("Períodos: %s", paste(per_tab$periodo, collapse = ", "))


# =============================================================================
# A. Ganhos, perdas, mudança líquida e troca (Pontius)
# =============================================================================
# Para cada classe e período:
#   ganho bruto  = área que virou a classe (vinda de outras)
#   perda bruta  = área que deixou de ser a classe
#   líquida      = ganho − perda (o que aparece na comparação de mapas)
#   troca (swap) = 2 × min(ganho, perda): mudança que "não aparece" no saldo
#                  porque a classe ganhou num lugar e perdeu em outro.
# Explica oscilações que o parecerista H apontou (ex.: Formação Vegetal que
# "diminui e volta a crescer"): saldo pequeno pode esconder muita troca.
gp <- bind_rows(
  trans_c |> filter(de != para) |> group_by(periodo, codigo = para, classe = classe_para) |>
    summarise(ganho = sum(area_ha), .groups = "drop") |> mutate(perda = 0),
  trans_c |> filter(de != para) |> group_by(periodo, codigo = de, classe = classe_de) |>
    summarise(perda = sum(area_ha), .groups = "drop") |> mutate(ganho = 0)
) |>
  group_by(periodo, codigo, classe) |>
  summarise(ganho = sum(ganho), perda = sum(perda), .groups = "drop") |>
  mutate(liquida = ganho - perda, troca = 2 * pmin(ganho, perda),
         across(c(ganho, perda, liquida, troca), ~ round(.x, 2))) |>
  arrange(periodo, codigo)
salva_tab(gp, "A_ganhos_perdas_troca.csv")

# Gráfico: ganho (positivo) e perda (negativa) por classe, um painel por
# período; o ponto preto marca a mudança líquida. Só classes com alguma
# mudança acima de 10 ha, para o gráfico não ficar poluído.
cls_rel <- gp |> group_by(classe) |> filter(max(ganho, perda) >= 10) |> pull(classe) |> unique()
leg_gp  <- legenda |> filter(classe %in% cls_rel)
gp_plot <- gp |> filter(classe %in% cls_rel) |>
  mutate(classe = factor(classe, levels = rev(leg_gp$classe)))
g_gp <- ggplot(gp_plot, aes(y = classe)) +
  geom_col(aes(x = ganho, fill = classe), width = 0.7) +
  geom_col(aes(x = -perda, fill = classe), width = 0.7, alpha = 0.55) +
  geom_point(aes(x = liquida), size = 1.2) +
  geom_vline(xintercept = 0, linewidth = 0.3) +
  scale_fill_manual(values = setNames(leg_gp$cor, leg_gp$classe), guide = "none") +
  scale_x_continuous(labels = num_curto, n.breaks = 3) +
  facet_wrap(~ periodo, nrow = 1) +
  labs(x = "Área (ha) — perda (à esquerda, clara) e ganho (à direita); ponto = mudança líquida",
       y = NULL) +
  tema
salva_fig(g_gp, "A_ganhos_perdas.png", h = 11)

# Resumo: troca × mudança líquida no município, por período.
gp_tot <- gp |> group_by(periodo) |>
  summarise(mudanca_total = sum(ganho), liquida_abs = sum(abs(liquida)) / 2,
            troca = sum(troca) / 2, .groups = "drop")
salva_tab(gp_tot, "A_troca_vs_liquida_municipio.csv")


# =============================================================================
# B. Análise de intensidade (Aldwaik & Pontius, 2012)
# =============================================================================
# B1. Nível de intervalo: intensidade anual de mudança S_t (% da área do
#     município que mudou por ano em cada período) comparada à taxa uniforme
#     U (como seria se a mudança fosse igualmente distribuída no tempo).
#     S_t > U = período "rápido"; S_t < U = período "lento".
intervalo <- trans_c |>
  group_by(periodo, dur) |>
  summarise(mudou_ha = sum(area_ha[de != para]), .groups = "drop") |>
  mutate(S_t = 100 * mudou_ha / AREA_TOTAL / dur)
U <- 100 * sum(intervalo$mudou_ha) / AREA_TOTAL / sum(intervalo$dur)
intervalo <- intervalo |> mutate(U = U, ritmo = ifelse(S_t > U, "rápido", "lento"),
                                 across(c(mudou_ha, S_t, U), ~ round(.x, 3)))
salva_tab(intervalo, "B1_intensidade_intervalo.csv")
anota("Intensidade anual de mudança (S_t, %%/ano): %s; uniforme U = %.2f%%/ano",
      paste(sprintf("%s = %.2f", intervalo$periodo, intervalo$S_t), collapse = "; "), U)

g_int <- ggplot(intervalo, aes(periodo, S_t, fill = ritmo)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = U, linetype = "dashed") +
  annotate("text", x = 0.5, y = U, label = "U (uniforme)", hjust = 0, vjust = -0.5, size = 2.6) +
  scale_fill_manual(values = c("rápido" = "#d4271e", "lento" = "#9ecae1"), name = NULL) +
  scale_y_continuous(labels = num_br) +
  labs(x = NULL, y = "Intensidade anual de mudança (% da área/ano)") + tema
salva_fig(g_int, "B1_intensidade_intervalo.png", h = 8)

# B2. Nível de categoria: para cada classe, intensidade anual de ganho
#     (ganho / área da classe no fim / duração) e de perda (perda / área no
#     início / duração), comparadas com S_t do período.
#     Acima de S_t = classe "ativa"; abaixo = "dormente".
area_ini <- trans_c |> group_by(periodo, codigo = de) |> summarise(a_ini = sum(area_ha), .groups = "drop")
area_fim <- trans_c |> group_by(periodo, codigo = para) |> summarise(a_fim = sum(area_ha), .groups = "drop")
categoria <- gp |>
  left_join(area_ini, by = c("periodo", "codigo")) |>
  left_join(area_fim, by = c("periodo", "codigo")) |>
  left_join(intervalo |> select(periodo, S_t, dur), by = "periodo") |>
  mutate(int_ganho = ifelse(coalesce(a_fim, 0) > 0, 100 * ganho / a_fim / dur, NA),
         int_perda = ifelse(coalesce(a_ini, 0) > 0, 100 * perda / a_ini / dur, NA),
         ganho_ativo = int_ganho > S_t, perda_ativa = int_perda > S_t,
         across(c(int_ganho, int_perda), ~ round(.x, 3))) |>
  select(periodo, codigo, classe, a_ini, a_fim, ganho, perda, int_ganho, int_perda, S_t,
         ganho_ativo, perda_ativa)
salva_tab(categoria, "B2_intensidade_categoria.csv")

cat_plot <- categoria |> filter(classe %in% cls_rel) |>
  select(periodo, classe, S_t, Ganho = int_ganho, Perda = int_perda) |>
  pivot_longer(c(Ganho, Perda), names_to = "tipo", values_to = "intensidade") |>
  mutate(classe = factor(classe, levels = rev(leg_gp$classe)))
g_cat <- ggplot(cat_plot, aes(intensidade, classe, fill = tipo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_vline(aes(xintercept = S_t), linetype = "dashed") +
  scale_fill_manual(values = c(Ganho = "#1b9e77", Perda = "#d95f02"), name = NULL) +
  scale_x_continuous(labels = num_curto, n.breaks = 3) +
  facet_wrap(~ periodo, nrow = 1) +
  labs(x = "Intensidade anual (% da área da classe/ano) — linha tracejada = S_t", y = NULL) +
  tema
salva_fig(g_cat, "B2_intensidade_categoria.png", h = 11)

# B3. Nível de transição: para classes-alvo (pastagem, urbano, eólico), de
#     quais classes elas ganharam MAIS do que o esperado se o ganho fosse
#     uniforme sobre tudo o que não era a classe ("alvo") ou menos
#     ("evitada"). Responde: "a pastagem avançou preferencialmente sobre a
#     savana ou sobre a restinga?"
ALVOS <- c(15, 24, 91)   # Pastagem, Área Urbanizada, Parque Eólico
transicao <- bind_rows(lapply(ALVOS, function(n) {
  trans_c |>
    filter(de != n) |>
    group_by(periodo, dur, de, classe_de) |>
    summarise(a_ini_i = sum(area_ha), para_n = sum(area_ha[para == n]), .groups = "drop") |>
    group_by(periodo) |>
    mutate(W = 100 * sum(para_n) / sum(a_ini_i) / dur,     # intensidade uniforme
           R = 100 * para_n / a_ini_i / dur) |>            # intensidade observada
    ungroup() |>
    filter(sum(para_n) > 0) |>
    mutate(alvo = legenda$classe[legenda$codigo == n],
           comportamento = case_when(para_n == 0 ~ "sem transição",
                                     R > W ~ "alvo (acima do uniforme)",
                                     TRUE ~ "evitada (abaixo do uniforme)"))
})) |>
  filter(para_n > 0 | a_ini_i > 50) |>
  mutate(across(c(a_ini_i, para_n), ~ round(.x, 2)), across(c(R, W), ~ round(.x, 4))) |>
  select(alvo, periodo, de_classe = classe_de, area_ini_ha = a_ini_i, virou_alvo_ha = para_n,
         R, W, comportamento)
salva_tab(transicao, "B3_intensidade_transicao.csv")

for (a in unique(transicao$alvo)) {
  d <- transicao |> filter(alvo == a, virou_alvo_ha > 0)
  if (nrow(d) == 0) next
  g <- ggplot(d, aes(R, reorder(de_classe, R), fill = comportamento)) +
    geom_col(width = 0.7) +
    geom_vline(aes(xintercept = W), linetype = "dashed") +
    scale_fill_manual(values = c("alvo (acima do uniforme)" = "#d4271e",
                                 "evitada (abaixo do uniforme)" = "#9ecae1"), name = NULL) +
    facet_wrap(~ periodo, nrow = 1, scales = "free_x") +
    scale_x_continuous(labels = num_curto, n.breaks = 3) +
    labs(title = paste("Ganho de", a, "por classe de origem"),
         x = "Intensidade anual da transição (% da área de origem/ano) — tracejado = uniforme",
         y = NULL) + tema
  salva_fig(g, paste0("B3_transicao_", gsub("[^a-z]+", "_", tolower(iconv(a, to = "ASCII//TRANSLIT"))), ".png"),
            h = 9)
}


# =============================================================================
# C. Tendência das séries por classe (Mann-Kendall, Sen, Pettitt)
# =============================================================================
# Implementados em R base para não depender de pacotes extras.
# ATENÇÃO: com os 11 anos disponíveis (espaçamento irregular) os testes têm
# pouco poder; com a série anual completa (41 anos) ficam robustos. Tratar
# como apoio descritivo e citar n no texto.
mann_kendall <- function(t, y) {
  o <- order(t); t <- t[o]; y <- y[o]; n <- length(y)
  s <- 0
  for (i in 1:(n - 1)) s <- s + sum(sign(y[(i + 1):n] - y[i]))
  tie <- table(y); tie <- tie[tie > 1]
  v <- (n * (n - 1) * (2 * n + 5) - sum(tie * (tie - 1) * (2 * tie + 5))) / 18
  z <- if (s > 0) (s - 1) / sqrt(v) else if (s < 0) (s + 1) / sqrt(v) else 0
  # declive de Sen: mediana de todas as inclinações entre pares de anos
  pares <- combn(n, 2)
  sen <- median((y[pares[2, ]] - y[pares[1, ]]) / (t[pares[2, ]] - t[pares[1, ]]))
  list(S = s, Z = z, p = 2 * pnorm(-abs(z)), tau = s / (n * (n - 1) / 2), sen = sen)
}
pettitt <- function(t, y) {
  o <- order(t); t <- t[o]; y <- y[o]; n <- length(y)
  U <- sapply(1:(n - 1), function(k) sum(sign(outer(y[(k + 1):n], y[1:k], "-"))))
  k <- which.max(abs(U)); K <- abs(U[k])
  list(ano_ruptura = t[k], K = K, p = min(1, 2 * exp(-6 * K^2 / (n^3 + n^2))),
       media_antes = mean(y[1:k]), media_depois = mean(y[(k + 1):n]))
}

serie <- areas |>
  select(ano, codigo, classe, area_ha) |>
  complete(ano, nesting(codigo, classe), fill = list(area_ha = 0))
tend <- serie |>
  group_by(codigo, classe) |>
  filter(max(area_ha) >= 10) |>                 # ignora classes ínfimas
  summarise(n = n(),
            area_inicial = area_ha[which.min(ano)], area_final = area_ha[which.max(ano)],
            mk = list(mann_kendall(ano, area_ha)), pt = list(pettitt(ano, area_ha)),
            .groups = "drop") |>
  mutate(tau = sapply(mk, `[[`, "tau"), Z = sapply(mk, `[[`, "Z"), p_mk = sapply(mk, `[[`, "p"),
         sen_ha_ano = sapply(mk, `[[`, "sen"),
         ano_ruptura = sapply(pt, `[[`, "ano_ruptura"), p_pettitt = sapply(pt, `[[`, "p"),
         media_antes = sapply(pt, `[[`, "media_antes"), media_depois = sapply(pt, `[[`, "media_depois"),
         tendencia = case_when(p_mk < 0.05 & tau > 0 ~ "aumento significativo",
                               p_mk < 0.05 & tau < 0 ~ "redução significativa",
                               TRUE ~ "sem tendência significativa"),
         ruptura = ifelse(p_pettitt < 0.05, paste("sim, após", ano_ruptura), "não significativa")) |>
  select(-mk, -pt) |>
  mutate(across(where(is.double) & !c(p_mk, p_pettitt), ~ round(.x, 2)),
         across(c(p_mk, p_pettitt), ~ signif(.x, 3)))
salva_tab(tend, "C_tendencias_classes.csv")
print(tend |> select(classe, tendencia, sen_ha_ano, p_mk, ruptura))

# Gráfico de linhas com a estatística anotada em cada painel.
leg_t <- legenda |> filter(codigo %in% tend$codigo)
serie_p <- serie |> filter(codigo %in% tend$codigo) |>
  mutate(classe = factor(classe, levels = leg_t$classe))
rot_t <- tend |>
  mutate(classe = factor(classe, levels = leg_t$classe),
         rotulo = sprintf("Sen = %s ha/ano\nMK p = %s%s", format(sen_ha_ano, decimal.mark = ","),
                          format(signif(p_mk, 2), decimal.mark = ","),
                          ifelse(p_pettitt < 0.05, paste0("\nruptura: ", ano_ruptura), "")))
g_tend <- ggplot(serie_p, aes(ano, area_ha, colour = classe)) +
  geom_line(linewidth = 0.6) + geom_point(size = 0.9) +
  geom_text(data = rot_t, aes(x = -Inf, y = Inf, label = rotulo), inherit.aes = FALSE,
            hjust = -0.05, vjust = 1.1, size = 2, lineheight = 0.9) +
  scale_colour_manual(values = setNames(leg_t$cor, leg_t$classe), guide = "none") +
  scale_y_continuous(labels = num_br, expand = expansion(mult = c(0.05, 0.45))) +
  facet_wrap(~ classe, scales = "free_y", ncol = 3) +
  labs(x = NULL, y = "Área (ha)") + tema
salva_fig(g_tend, "C_series_com_tendencia.png", h = 16)

for (i in seq_len(nrow(tend))) {
  r <- tend[i, ]
  anota("%s: %.0f -> %.0f ha; %s (Sen = %.1f ha/ano, p = %.3g); ruptura: %s",
        r$classe, r$area_inicial, r$area_final, r$tendencia, r$sen_ha_ano, r$p_mk, r$ruptura)
}


# =============================================================================
# D. Dados do IBGE (API do SIDRA), com cache em dados_ibge/
# =============================================================================
# Cada consulta é salva em CSV na primeira vez; nas próximas execuções o
# script lê o CSV (não precisa de internet). Para atualizar, apague o CSV.
# Símbolos do SIDRA: "-" = zero absoluto (vira 0); "..", "...", "X" = sem
# dado/sigilo (viram NA).
sidra <- function(nome, caminho) {
  arq <- file.path(DIR_IBGE, paste0(nome, ".csv"))
  if (file.exists(arq)) return(suppressMessages(read_csv2(arq, show_col_types = FALSE)))
  url <- paste0("https://apisidra.ibge.gov.br/values", caminho)
  d <- tryCatch(jsonlite::fromJSON(url), error = function(e) {
    message("Falha ao baixar ", nome, ": ", conditionMessage(e)); NULL })
  if (is.null(d) || nrow(d) < 2) return(NULL)
  names(d) <- make.unique(as.character(unlist(d[1, ])))     # 1ª linha = nomes
  d <- d[-1, , drop = FALSE]
  d$valor <- suppressWarnings(as.numeric(ifelse(d$Valor == "-", "0", d$Valor)))
  d$ano   <- as.integer(d$Ano)
  write_excel_csv2(d, arq)
  d
}

# PAM (tabela 5457): área plantada (8331), área colhida (216), quantidade
# (214) e valor (215), todos os produtos (c782/all).
pam <- sidra("pam_5457", paste0("/t/5457/n6/", COD_MUN, "/v/8331,216,214,215/p/all/c782/all"))
# PPM (tabela 3939): efetivo dos rebanhos, por tipo (c79/all).
ppm <- sidra("ppm_3939", paste0("/t/3939/n6/", COD_MUN, "/v/all/p/all/c79/all"))
# PPM aquicultura (tabela 3940): produção por tipo (desde 2013).
aqui <- sidra("ppm_aquicultura_3940", paste0("/t/3940/n6/", COD_MUN, "/v/all/p/all/c654/all"))
# PIB dos municípios (tabela 5938): PIB e valor adicionado por setor.
pib <- sidra("pib_5938", paste0("/t/5938/n6/", COD_MUN, "/v/all/p/all"))
# População: estimativas (6579) e Censos 2022 (4709) e 2000/2010 (202).
pop_est <- sidra("populacao_estimativas_6579", paste0("/t/6579/n6/", COD_MUN, "/v/all/p/all"))
pop_c22 <- sidra("populacao_censo2022_4709", paste0("/t/4709/n6/", COD_MUN, "/v/93/p/all"))
pop_c   <- sidra("populacao_censos_202", paste0("/t/202/n6/", COD_MUN, "/v/allxp/p/all/c1/0/c2/0"))

# Ilha Grande foi desmembrada de Parnaíba nos anos 1990: antes disso as
# séries municipais não existem. O script usa só os anos com dado.
ano_min <- function(d) if (is.null(d)) NA else suppressWarnings(min(d$ano[!is.na(d$valor)]))
anota("IBGE — primeiro ano com dado: PAM %s, PPM %s, PIB %s",
      ano_min(pam), ano_min(ppm), ano_min(pib))


# =============================================================================
# E. Cruzamentos MapBiomas × IBGE
# =============================================================================
# Com o MapBiomas em 11 anos, o número de anos em comum com o IBGE é pequeno
# (5 a 7). As correlações (Spearman) são indicativas; o principal é o
# gráfico das duas séries lado a lado. Com a série anual do MapBiomas,
# rode de novo e as correlações ganham força.
area_classe <- function(cods) serie |> filter(codigo %in% cods) |>
  group_by(ano) |> summarise(mapbiomas_ha = sum(area_ha), .groups = "drop")
cruza <- function(mb, ibge, nome_ibge, unidade, titulo, arquivo) {
  if (is.null(ibge) || nrow(ibge) == 0) { message("Sem dados para: ", titulo); return(invisible(NULL)) }
  d <- full_join(mb, ibge, by = "ano") |> arrange(ano)
  comum <- d |> filter(!is.na(mapbiomas_ha), !is.na(ibge))
  rho <- p <- NA
  if (nrow(comum) >= 4) {
    ct <- suppressWarnings(cor.test(comum$mapbiomas_ha, comum$ibge, method = "spearman"))
    rho <- unname(ct$estimate); p <- ct$p.value
  }
  long <- d |> pivot_longer(c(mapbiomas_ha, ibge), names_to = "fonte", values_to = "valor") |>
    filter(!is.na(valor)) |>
    mutate(fonte = ifelse(fonte == "ibge", paste0(nome_ibge, " (", unidade, ")"),
                          "MapBiomas (ha)"))
  g <- ggplot(long, aes(ano, valor)) +
    geom_line(colour = "grey30") + geom_point(size = 1) +
    facet_wrap(~ fonte, ncol = 1, scales = "free_y") +
    scale_y_continuous(labels = num_br) +
    labs(title = titulo, x = NULL, y = NULL,
         subtitle = if (!is.na(rho)) sprintf("Spearman rho = %s (p = %s; n = %d anos em comum)",
           format(round(rho, 2), decimal.mark = ","), format(signif(p, 2), decimal.mark = ","),
           nrow(comum)) else "Menos de 4 anos em comum: correlação não calculada") + tema
  salva_fig(g, arquivo, h = 10)
  salva_tab(d, sub("\\.png$", ".csv", arquivo))
  anota("%s: rho = %s, p = %s, n = %d", titulo,
        ifelse(is.na(rho), "NA", sprintf("%.2f", rho)),
        ifelse(is.na(p), "NA", sprintf("%.3g", p)), nrow(comum))
  invisible(d)
}

# Seleciona uma série do IBGE pelo texto da variável e da categoria.
# excluir_prelim: descarta o último ano (preliminar). Use FALSE para Censo,
# estimativas de população e PIB, que não têm ano preliminar.
pega <- function(d, var_regex, cat_regex = NULL, excluir_prelim = EXCLUIR_ANO_PRELIMINAR) {
  if (is.null(d)) return(NULL)
  x <- d |> filter(grepl(var_regex, Variável, ignore.case = TRUE))
  if (!is.null(cat_regex)) {
    col_cat <- setdiff(names(x)[!grepl("Código|^valor$|^ano$|^Valor$", names(x))],
                       c("Nível Territorial", "Unidade de Medida", "Município", "Ano", "Variável"))
    col_cat <- col_cat[length(col_cat)]
    x <- x |> filter(grepl(cat_regex, .data[[col_cat]], ignore.case = TRUE))
  }
  # Anos só com "sem dado" ficam NA (e não 0): antes da criação do
  # município as séries do IBGE vêm vazias e não podem virar zero.
  x <- x |> group_by(ano) |>
    summarise(ibge = if (all(is.na(valor))) NA_real_ else sum(valor, na.rm = TRUE),
              .groups = "drop") |>
    filter(!is.na(ano), !is.na(ibge))
  if (excluir_prelim && nrow(x) > 0) x <- x |> filter(ano < max(ano))
  x
}

# E1. Pastagem (15) × rebanhos (PPM).
# Ilha Grande não é polo de bovinocultura; caprinos e ovinos são tradicionais
# no Piauí. Por isso, além do bovino, o rebanho é somado em UNIDADES ANIMAIS
# (UA), que põem espécies de portes diferentes na mesma escala de pressão de
# pastejo. Fatores aproximados (ajuste se preferir outra referência):
FATOR_UA <- c("Bovino" = 1.00, "Bubalino" = 1.25, "Equino" = 1.00,
              "Caprino" = 0.15, "Ovino" = 0.15)
# Suínos e aves ficam de fora: não pastejam e não pressionam pastagem.

# Anos do Censo Agropecuário: a PPM municipal é recalibrada por eles e, entre
# um censo e outro, costuma ser estimada por tendência. Saltos logo depois
# desses anos (ex.: 2007, 2018) são ajuste estatístico, não mudança real.
ANOS_CENSO_AGRO <- c(2006, 2017)

if (!is.null(ppm)) {
  col_reb <- grep("rebanho", names(ppm), value = TRUE)
  col_reb <- col_reb[!grepl("Código", col_reb)][1]
  reb <- ppm |>
    filter(.data[[col_reb]] %in% names(FATOR_UA), !is.na(valor)) |>
    transmute(ano, tipo = .data[[col_reb]], cabecas = valor,
              ua = valor * FATOR_UA[.data[[col_reb]]])
  if (EXCLUIR_ANO_PRELIMINAR) reb <- reb |> filter(ano < max(ano))
  reb_tot <- reb |> group_by(ano) |>
    summarise(cabecas = sum(cabecas), ua = sum(ua), .groups = "drop")
  salva_tab(reb |> pivot_wider(id_cols = ano, names_from = tipo, values_from = cabecas) |>
              left_join(reb_tot, by = "ano"), "E1_rebanhos_cabecas_ua.csv")

  # Composição do rebanho (cabeças por tipo), com os anos de censo marcados.
  g <- ggplot(reb, aes(ano, cabecas, fill = tipo)) +
    geom_area(colour = "white", linewidth = 0.2) +
    geom_vline(xintercept = ANOS_CENSO_AGRO + 0.5, linetype = "dotted") +
    annotate("text", x = ANOS_CENSO_AGRO + 0.7, y = Inf, vjust = 1.3, hjust = 0, size = 2.3,
             label = paste("Censo Agro", ANOS_CENSO_AGRO)) +
    scale_y_continuous(labels = num_br) +
    scale_fill_brewer(palette = "Set2", name = NULL) +
    labs(x = NULL, y = "Cabeças", title = "Rebanhos que pastejam — PPM/IBGE") + tema
  salva_fig(g, "E1_rebanhos_composicao.png", h = 9)

  # Pressão de pastejo (UA) × pastagem mapeada
  cruza(area_classe(15), reb_tot |> select(ano, ibge = ua), "Rebanho total", "UA",
        "Pastagem (MapBiomas) × rebanho total em unidades animais (PPM)",
        "E1_pastagem_x_rebanho_ua.png")

  # Lotação aparente: UA por hectare de pastagem mapeada (anos em comum)
  lot <- inner_join(area_classe(15), reb_tot, by = "ano") |>
    mutate(ua_por_ha = round(ua / mapbiomas_ha, 2))
  salva_tab(lot, "E1_lotacao_aparente.csv")
  anota("Rebanho em UA: %s", paste(sprintf("%d = %.0f", lot$ano, lot$ua), collapse = "; "))
  anota("Lotação aparente (UA/ha de pastagem mapeada): %s",
        paste(sprintf("%d = %.2f", lot$ano, lot$ua_por_ha), collapse = "; "))
}
# Só bovinos, para comparação.
cruza(area_classe(15), pega(ppm, "efetivo", "^bovino$"), "Rebanho bovino", "cabeças",
      "Pastagem (MapBiomas) × rebanho bovino (PPM)", "E1b_pastagem_x_bovinos.png")

# E2. Agricultura (lavouras temporárias e perenes) × área plantada total (PAM).
COD_AGRI <- c(18, 19, 20, 39, 40, 41, 62, 36, 46, 47, 35, 48)
cruza(area_classe(COD_AGRI), pega(pam, "plantada", "^Total$"), "Área plantada", "ha",
      "Agricultura (MapBiomas) × área plantada (PAM)", "E2_agricultura_x_pam.png")
# Inclui Mosaico de Usos (21), que no litoral concentra a agricultura familiar.
cruza(area_classe(c(COD_AGRI, 21)), pega(pam, "plantada", "^Total$"), "Área plantada", "ha",
      "Agricultura + Mosaico de Usos (MapBiomas) × área plantada (PAM)",
      "E2b_agricultura_mosaico_x_pam.png")

# E3. Principais lavouras (PAM): média da área plantada no último decênio.
if (!is.null(pam)) {
  # coluna com o NOME do produto (não a do código)
  col_prod <- grep("Produto", names(pam), value = TRUE)
  col_prod <- col_prod[!grepl("Código", col_prod)][1]
  lav <- pam |>
    filter(grepl("plantada", Variável), .data[[col_prod]] != "Total", !is.na(valor),
           !(EXCLUIR_ANO_PRELIMINAR & ano == max(ano))) |>
    group_by(produto = .data[[col_prod]]) |>
    summarise(media_ultimos_10_anos_ha = mean(valor[ano >= max(ano) - 9], na.rm = TRUE),
              primeiro_ano = min(ano[valor > 0]), ultimo_ano = max(ano[valor > 0]),
              .groups = "drop") |>
    filter(media_ultimos_10_anos_ha > 0) |>
    arrange(desc(media_ultimos_10_anos_ha))
  salva_tab(lav, "E3_principais_lavouras_pam.csv")
  top <- head(lav$produto, 6)
  g <- pam |> filter(grepl("plantada", Variável), .data[[col_prod]] %in% top, !is.na(valor),
                     !(EXCLUIR_ANO_PRELIMINAR & ano == max(ano))) |>
    ggplot(aes(ano, valor, colour = .data[[col_prod]])) + geom_line() + geom_point(size = 0.8) +
    scale_y_continuous(labels = num_br) +
    labs(x = NULL, y = "Área plantada (ha)", colour = NULL,
         title = "Principais lavouras — PAM/IBGE") + tema
  salva_fig(g, "E3_principais_lavouras.png", h = 9)
}

# E4. Área urbanizada × população (parecerista B: "fundamentar o
#     crescimento urbano com dados de população").
pop <- bind_rows(
  if (!is.null(pop_c))   pega(pop_c, "População", excluir_prelim = FALSE) |> mutate(fonte = "Censo"),
  if (!is.null(pop_c22)) pega(pop_c22, "População", excluir_prelim = FALSE) |> mutate(fonte = "Censo"),
  if (!is.null(pop_est)) pega(pop_est, "População", excluir_prelim = FALSE) |> mutate(fonte = "Estimativa")
)
if (nrow(pop) > 0) {
  pop <- pop |> filter(ibge > 0) |>                          # anos antes da criação do município
    group_by(ano) |> slice(1) |> ungroup() |>                # se houver os dois, fica o Censo
    rename(populacao = ibge) |> arrange(ano)
  salva_tab(pop, "E4_populacao.csv")
  urb <- area_classe(24)
  # Taxa anual de crescimento geométrico entre censos e da área urbana
  censos <- pop |> filter(fonte == "Censo") |> arrange(ano)
  if (nrow(censos) >= 2) {
    tx <- censos |> mutate(ano_ant = lag(ano), pop_ant = lag(populacao)) |> filter(!is.na(ano_ant)) |>
      rowwise() |>
      mutate(tx_pop = 100 * ((populacao / pop_ant)^(1 / (ano - ano_ant)) - 1),
             urb_ini = approx(urb$ano, urb$mapbiomas_ha, ano_ant, rule = 2)$y,
             urb_fim = approx(urb$ano, urb$mapbiomas_ha, ano, rule = 2)$y,
             tx_urb = 100 * ((urb_fim / urb_ini)^(1 / (ano - ano_ant)) - 1),
             m2_urb_por_novo_hab = ifelse(populacao > pop_ant,
                                          1e4 * (urb_fim - urb_ini) / (populacao - pop_ant), NA)) |>
      ungroup()
    salva_tab(tx, "E4_taxas_populacao_vs_urbano.csv")
    for (i in seq_len(nrow(tx))) anota("%d-%d: população %+.2f%%/ano; área urbana %+.2f%%/ano",
                                       tx$ano_ant[i], tx$ano[i], tx$tx_pop[i], tx$tx_urb[i])
  }
  cruza(urb, pop |> select(ano, ibge = populacao), "População", "hab.",
        "Área urbanizada (MapBiomas) × população (IBGE)", "E4_urbano_x_populacao.png")
}

# E5. Parque eólico (91) × valor adicionado da indústria (PIB municipal).
#     A entrada em operação de parques eólicos costuma aparecer como salto
#     no VAB da indústria (eletricidade).
if (!is.null(pib)) {
  # Só os valores em mil R$ (exclui as variáveis "percentual do total").
  # "Serviços" é testado antes de "administração" porque o nome da variável
  # de serviços contém "exclusive administração...".
  vab <- pib |> filter(grepl("adicionado", Variável), !grepl("percentual|participa", Variável, TRUE),
                       !is.na(valor)) |>
    mutate(setor = case_when(grepl("agropecu", Variável, TRUE) ~ "Agropecuária",
                             grepl("indústria", Variável, TRUE) ~ "Indústria",
                             grepl("serviços", Variável, TRUE) ~ "Serviços",
                             grepl("administração", Variável, TRUE) ~ "Administração pública",
                             TRUE ~ NA_character_)) |>
    filter(!is.na(setor), !grepl("total", Variável, TRUE)) |>
    group_by(ano, setor) |> summarise(valor = sum(valor), .groups = "drop") |>
    group_by(ano) |> mutate(pct = 100 * valor / sum(valor)) |> ungroup()
  salva_tab(vab, "E5_vab_setores.csv")
  g <- ggplot(vab, aes(ano, pct, fill = setor)) + geom_area(alpha = 0.85, colour = "white") +
    labs(x = NULL, y = "% do valor adicionado bruto", fill = NULL,
         title = "Estrutura econômica — PIB dos Municípios/IBGE") + tema
  salva_fig(g, "E5_vab_setores.png", h = 9)
  cruza(area_classe(91), vab |> filter(setor == "Indústria") |> select(ano, ibge = valor),
        "VAB da indústria", "mil R$", "Parque eólico (MapBiomas) × VAB da indústria",
        "E5_eolico_x_vab_industria.png")
}


# =============================================================================
# F. Resumo em texto
# =============================================================================
arq_res <- file.path(DIR_SAIDA, "resumo_analise.txt")
writeLines(c("RESUMO DA ANÁLISE — Ilha Grande (PI), MapBiomas Col. 11 + IBGE",
             paste("Gerado em", format(Sys.time(), "%d/%m/%Y %H:%M")), "", resumo), arq_res,
           useBytes = TRUE)
cat("\n", paste(resumo, collapse = "\n"), "\n")
cat("\nTabelas:", DIR_TAB_A, "\nFiguras:", DIR_FIG_A, "\nResumo:", arq_res, "\n")
