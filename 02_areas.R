# =============================================================================
# 02_areas.R — Área por classe, variações e taxas de mudança
#
# Entrada : recortes em DIR_RECORTE (script 01)
# Saída   : DIR_TAB/
#   areas_classes_longa.csv     -> ano, classe, área (ha) e % do município
#   areas_classes_larga.csv     -> classes nas linhas, anos nas colunas
#   areas_nivel1_larga.csv      -> idem, agregado no nível 1 do MapBiomas
#   variacao_entre_anos.csv     -> Δ ha, Δ %, taxa anual entre ANOS_MAPAS
#   series_urbano_eolico.csv    -> série da Área Urbanizada (24) e do
#                                  Parque Eólico (91), em ha
#   unidades_conservacao.csv    -> % do município na APA e na RESEX (opcional)
#
# Responde aos pareceres: tabela com deltas e taxas (H), série atualizada
# (H), área urbana isolada e em hectares (B e H), APA/RESEX (B).
# =============================================================================

# Garante que o R está na pasta dos scripts (necessário para achar o
# 00_config.R). Se você mudar a pasta de lugar, ajuste o caminho abaixo.
DIR_SCRIPTS <- "C:/Users/vj_ac/Downloads/Art_ruan/scripts_R"
if (!file.exists("00_config.R")) setwd(DIR_SCRIPTS)
source("00_config.R")


# -----------------------------------------------------------------------------
# 1. Anos disponíveis
# -----------------------------------------------------------------------------
anos <- anos_disponiveis()
if (length(anos) == 0) stop("Rode o 01_recorte.R primeiro.")
if (is.null(ANOS_SERIE)) ANOS_SERIE <- anos
cat("Anos da série:", paste(ANOS_SERIE, collapse = ", "), "\n")


# -----------------------------------------------------------------------------
# 2. Área de cada classe em cada ano
# -----------------------------------------------------------------------------
# A área de cada pixel é calculada com cellSize(), que considera a latitude.
# Em raster geográfico (graus), um pixel de "30 m" não tem exatamente 0,09 ha,
# então somar pixels × 0,09 introduziria um erro pequeno, mas evitável.
# zonal() soma a área dos pixels agrupando pelo código da classe.
area_por_ano <- function(ano) {
  r   <- rast(arq_recorte(ano))
  cel <- cellSize(r, unit = "ha", mask = TRUE)
  z   <- zonal(cel, r, fun = "sum", na.rm = TRUE)
  names(z) <- c("codigo", "area_ha")
  z$ano <- ano
  z
}
areas <- bind_rows(lapply(ANOS_SERIE, area_por_ano))

# Avisa se aparecer algum código que não está na legenda do 00_config.R.
sem_legenda <- setdiff(unique(areas$codigo), legenda$codigo)
if (length(sem_legenda) > 0) {
  warning("Códigos sem legenda (acrescente no 00_config.R): ",
          paste(sem_legenda, collapse = ", "))
}

# Junta nomes/grupos da legenda e calcula o % da área do município.
areas <- areas |>
  left_join(legenda, by = "codigo") |>
  group_by(ano) |>
  mutate(pct = 100 * area_ha / sum(area_ha)) |>
  ungroup() |>
  arrange(ano, codigo)

write_excel_csv2(areas |> select(ano, codigo, classe, nivel1, grupo, area_ha, pct),
          file.path(DIR_TAB, "areas_classes_longa.csv"))


# -----------------------------------------------------------------------------
# 3. Tabelas largas (formato de tabela de artigo)
# -----------------------------------------------------------------------------
# Classes nas linhas, anos nas colunas, área em ha com 2 casas decimais.
larga <- areas |>
  select(codigo, classe, ano, area_ha) |>
  mutate(area_ha = round(area_ha, 2)) |>
  pivot_wider(names_from = ano, values_from = area_ha, values_fill = 0) |>
  arrange(codigo)
write_excel_csv2(larga, file.path(DIR_TAB, "areas_classes_larga.csv"))

# Mesmo formato no nível 1 (Floresta, Vegetação Herbácea e Arbustiva, ...).
larga_n1 <- areas |>
  group_by(nivel1, ano) |>
  summarise(area_ha = round(sum(area_ha), 2), .groups = "drop") |>
  pivot_wider(names_from = ano, values_from = area_ha, values_fill = 0)
write_excel_csv2(larga_n1, file.path(DIR_TAB, "areas_nivel1_larga.csv"))


# -----------------------------------------------------------------------------
# 4. Variação entre os anos dos mapas (pedido central do parecerista H)
# -----------------------------------------------------------------------------
# Para cada par consecutivo de ANOS_MAPAS (ex.: 1985–1995) e para o período
# completo (1985–2025), calcula:
#   delta_ha     : área final − área inicial
#   delta_pct    : variação relativa à área inicial (%)
#   taxa_anual   : taxa anual de mudança (% ao ano), fórmula de Puyravaud
#                  (2003): r = (1 / t) × ln(A2 / A1) × 100. É a taxa mais
#                  usada em estudos de mudança de cobertura e permite
#                  comparar períodos de durações diferentes.
#   delta_ha_ano : variação absoluta média por ano (ha/ano)
# Quando a classe não existe no ano inicial (A1 = 0), as taxas relativas
# ficam NA, porque não são definidas.
anos_m <- intersect(ANOS_MAPAS, ANOS_SERIE)
periodos <- data.frame(ini = head(anos_m, -1), fim = tail(anos_m, -1))
periodos <- rbind(periodos, data.frame(ini = min(anos_m), fim = max(anos_m)))

base <- areas |> select(codigo, classe, ano, area_ha)
variacao <- bind_rows(lapply(seq_len(nrow(periodos)), function(i) {
  p <- periodos[i, ]
  full_join(base |> filter(ano == p$ini) |> select(codigo, classe, a1 = area_ha),
            base |> filter(ano == p$fim) |> select(codigo, classe, a2 = area_ha),
            by = c("codigo", "classe")) |>
    mutate(a1 = coalesce(a1, 0), a2 = coalesce(a2, 0),
           periodo      = paste0(p$ini, "-", p$fim),
           t            = p$fim - p$ini,
           delta_ha     = a2 - a1,
           delta_pct    = ifelse(a1 > 0, 100 * delta_ha / a1, NA),
           taxa_anual   = ifelse(a1 > 0 & a2 > 0, 100 * log(a2 / a1) / t, NA),
           delta_ha_ano = delta_ha / t)
})) |>
  mutate(across(c(a1, a2, delta_ha, delta_pct, taxa_anual, delta_ha_ano),
                ~ round(.x, 2))) |>
  select(periodo, codigo, classe, area_inicial_ha = a1, area_final_ha = a2,
         delta_ha, delta_pct, taxa_anual_pct = taxa_anual, delta_ha_ano)
write_excel_csv2(variacao, file.path(DIR_TAB, "variacao_entre_anos.csv"))


# -----------------------------------------------------------------------------
# 5. Área Urbanizada e Parque Eólico isolados (parecerista B: "em hectares")
# -----------------------------------------------------------------------------
# Série anual das classes de CLASSES_ORIGEM (00_config.R). Anos em que a
# classe não aparece recebem 0 ha. O Parque Eólico (91) é classe nova da
# Coleção 11 e mede diretamente a área ocupada pela usina citada no texto.
urb <- tidyr::expand_grid(ano = ANOS_SERIE, codigo = CLASSES_ORIGEM) |>
  left_join(areas |> select(ano, codigo, area_ha, pct), by = c("ano", "codigo")) |>
  left_join(legenda |> select(codigo, classe), by = "codigo") |>
  mutate(area_ha = round(coalesce(area_ha, 0), 2), pct = round(coalesce(pct, 0), 3)) |>
  select(ano, classe, area_ha, pct) |>
  arrange(classe, ano)
write_excel_csv2(urb, file.path(DIR_TAB, "series_urbano_eolico.csv"))
cat("\nÁrea Urbanizada e Parque Eólico (ha):\n"); print(urb, n = 50)


# -----------------------------------------------------------------------------
# 6. Quanto do município está na APA e na RESEX (opcional)
# -----------------------------------------------------------------------------
# Só roda se os arquivos indicados no 00_config.R existirem. As áreas são
# calculadas em projeção equivalente (Albers SIRGAS 2000, EPSG:5880) para
# não distorcer hectares.
uc_area <- function(arq, nome) {
  if (!file.exists(arq)) return(NULL)
  mun <- st_read(ARQ_LIMITE, quiet = TRUE) |> st_union() |> st_transform(5880)
  uc  <- st_read(arq, quiet = TRUE) |> st_union() |> st_transform(5880)
  inter <- st_intersection(mun, uc)
  a_mun <- as.numeric(st_area(mun)) / 1e4
  a_int <- if (length(inter) == 0) 0 else as.numeric(st_area(inter)) / 1e4
  data.frame(unidade = nome, area_mun_ha = round(a_mun, 2),
             area_dentro_ha = round(a_int, 2),
             pct_mun = round(100 * a_int / a_mun, 2))
}
uc <- bind_rows(uc_area(ARQ_APA, "APA Delta do Parnaíba"),
                uc_area(ARQ_RESEX, "RESEX Marinha do Delta do Parnaíba"))
if (nrow(uc) > 0) {
  write_excel_csv2(uc, file.path(DIR_TAB, "unidades_conservacao.csv"))
  print(uc)
} else {
  cat("\nArquivos da APA/RESEX não encontrados; etapa 6 pulada.\n")
}

cat("\nTabelas salvas em:", DIR_TAB, "\n")
