# =============================================================================
# 03_transicoes.R — Matrizes de transição e classes de mudança
#
# Entrada : recortes em DIR_RECORTE (script 01)
# Saída   : DIR_TAB/
#   transicoes_longa.csv            -> período, classe de origem, classe de
#                                      destino, área (ha) — todas as trocas
#   matriz_transicao_AAAA-AAAA.csv  -> matriz "de (linhas) × para (colunas)"
#   matriz_nivel1_AAAA-AAAA.csv     -> idem, no nível 1 do MapBiomas
#   classes_mudanca_area.csv        -> área de cada classe de mudança
#                                      (novo Quadro 1) por período
#   origem_area_urbanizada.csv      -> de quais classes veio a área urbana
#   origem_parque_eolico.csv        -> idem para o Parque Eólico (Col. 11)
#   origem_ganho_praia_duna.csv     -> sobre o que a areia avançou
#           DIR_RAS/
#   mudanca_AAAA-AAAA.tif           -> raster das classes de mudança (mapa)
#
# Responde aos pareceres: matriz "quem se converteu em quem" (H); conceitos
# de degradação/regeneração (H); o que a expansão urbana substituiu (B);
# dinâmica de dunas por período (B e H).
# =============================================================================

# Garante que o R está na pasta dos scripts (necessário para achar o
# 00_config.R). Se você mudar a pasta de lugar, ajuste o caminho abaixo.
DIR_SCRIPTS <- "C:/Users/vj_ac/Downloads/Art_ruan/scripts_R"
if (!file.exists("00_config.R")) setwd(DIR_SCRIPTS)
source("00_config.R")


# -----------------------------------------------------------------------------
# 1. Períodos analisados
# -----------------------------------------------------------------------------
# Pares consecutivos de ANOS_MAPAS + período completo (primeiro -> último).
anos_m <- intersect(ANOS_MAPAS, anos_disponiveis())
if (length(anos_m) < 2) stop("São necessários pelo menos 2 anos de ANOS_MAPAS recortados.")
periodos <- data.frame(ini = head(anos_m, -1), fim = tail(anos_m, -1))
periodos <- rbind(periodos, data.frame(ini = min(anos_m), fim = max(anos_m)))
periodos$rotulo <- paste0(periodos$ini, "-", periodos$fim)
print(periodos)


# -----------------------------------------------------------------------------
# 2. Tabulação cruzada de um período
# -----------------------------------------------------------------------------
# Truque de álgebra de mapas: codigo_combinado = origem × 1000 + destino.
# Ex.: Restinga Herbácea (50) -> Área Urbanizada (24) vira 50024.
# Somando a área dos pixels por código combinado obtemos, em uma só
# operação, a área de cada transição. Depois o código é "desmontado".
# É o mesmo resultado da ferramenta Cross classification do SCP 8, mas
# reprodutível e documentado (pedido do parecerista H).
tabula_periodo <- function(ini, fim) {
  r1  <- rast(arq_recorte(ini))
  r2  <- rast(arq_recorte(fim))
  cmb <- r1 * 1000 + r2
  cel <- cellSize(r1, unit = "ha", mask = TRUE)
  z   <- zonal(cel, cmb, fun = "sum", na.rm = TRUE)
  names(z) <- c("cmb", "area_ha")
  z |>
    mutate(de = cmb %/% 1000, para = cmb %% 1000,
           periodo = paste0(ini, "-", fim)) |>
    select(periodo, de, para, area_ha)
}
trans <- bind_rows(lapply(seq_len(nrow(periodos)),
                          function(i) tabula_periodo(periodos$ini[i], periodos$fim[i])))


# -----------------------------------------------------------------------------
# 3. Nomes, grupos e classe de mudança de cada transição
# -----------------------------------------------------------------------------
leg_de   <- legenda |> select(de = codigo, classe_de = classe, nivel1_de = nivel1, grupo_de = grupo)
leg_para <- legenda |> select(para = codigo, classe_para = classe, nivel1_para = nivel1, grupo_para = grupo)

trans <- trans |>
  left_join(leg_de, by = "de") |>
  left_join(leg_para, by = "para") |>
  mutate(id = regra_mudanca(grupo_de, grupo_para)) |>
  left_join(classes_mudanca |> select(id, mudanca), by = "id") |>
  mutate(area_ha = round(area_ha, 4))

if (any(is.na(trans$id))) {
  warning("Há transições sem classe de mudança (grupo ausente na legenda?). ",
          "Veja as linhas com 'mudanca' vazia em transicoes_longa.csv.")
}
write_excel_csv2(trans, file.path(DIR_TAB, "transicoes_longa.csv"))


# -----------------------------------------------------------------------------
# 4. Matrizes de transição (de × para), uma por período
# -----------------------------------------------------------------------------
# Linhas = classe no ano inicial; colunas = classe no ano final; células em
# ha. A diagonal é a área que permaneceu na mesma classe. Totais de linha e
# coluna conferem com as áreas do script 02.
salva_matriz <- function(df, col_de, col_para, arquivo) {
  m <- df |>
    group_by(de = .data[[col_de]], para = .data[[col_para]]) |>
    summarise(area_ha = round(sum(area_ha), 2), .groups = "drop") |>
    pivot_wider(names_from = para, values_from = area_ha, values_fill = 0)
  m$Total <- rowSums(m[, -1])
  tot <- c(list(de = "Total"), as.list(colSums(m[, -1])))
  m <- bind_rows(m, as_tibble(tot))
  write_excel_csv2(m, arquivo)
  invisible(m)
}
for (p in periodos$rotulo) {
  d <- trans |> filter(periodo == p)
  salva_matriz(d, "classe_de", "classe_para",
               file.path(DIR_TAB, paste0("matriz_transicao_", p, ".csv")))
  salva_matriz(d, "nivel1_de", "nivel1_para",
               file.path(DIR_TAB, paste0("matriz_nivel1_", p, ".csv")))
}


# -----------------------------------------------------------------------------
# 5. Área das classes de mudança por período (novo Quadro 1 / Tabela 2)
# -----------------------------------------------------------------------------
mud_area <- trans |>
  group_by(periodo, id, mudanca) |>
  summarise(area_ha = sum(area_ha), .groups = "drop") |>
  group_by(periodo) |>
  mutate(pct = 100 * area_ha / sum(area_ha)) |>
  ungroup() |>
  mutate(area_ha = round(area_ha, 2), pct = round(pct, 2)) |>
  arrange(periodo, id)

# Versão larga (classes nas linhas, períodos nas colunas), no formato da
# Tabela 2 do manuscrito, para comparar diretamente com a versão anterior.
mud_larga <- mud_area |>
  select(id, mudanca, periodo, area_ha) |>
  mutate(periodo = factor(periodo, levels = periodos$rotulo)) |>  # ordem cronológica
  pivot_wider(names_from = periodo, values_from = area_ha, values_fill = 0,
              names_sort = TRUE) |>
  left_join(classes_mudanca |> select(id, nome_antigo), by = "id") |>
  arrange(id)
write_excel_csv2(mud_area,  file.path(DIR_TAB, "classes_mudanca_area.csv"))
write_excel_csv2(mud_larga, file.path(DIR_TAB, "classes_mudanca_larga.csv"))
print(mud_larga)


# -----------------------------------------------------------------------------
# 6. De onde vieram a Área Urbanizada e o Parque Eólico? (parecerista B)
# -----------------------------------------------------------------------------
# Para cada classe de CLASSES_ORIGEM (00_config.R), considera só os pixels
# que NÃO eram daquela classe no ano inicial e passaram a ser no ano final,
# e mostra quais classes eles substituíram. Gera um CSV por classe:
#   origem_area_urbanizada.csv, origem_parque_eolico.csv
# Se a classe não ocorrer no município, o CSV sai vazio (sem erro).
for (cod in CLASSES_ORIGEM) {
  nome <- legenda$classe[legenda$codigo == cod]
  orig <- trans |>
    filter(para == cod, de != cod) |>
    group_by(periodo, classe_de) |>
    summarise(area_ha = round(sum(area_ha), 2), .groups = "drop") |>
    group_by(periodo) |>
    mutate(pct_do_ganho = round(100 * area_ha / sum(area_ha), 1)) |>
    ungroup() |>
    arrange(periodo, desc(area_ha))
  arq <- paste0("origem_", gsub("[^a-z]+", "_", tolower(iconv(nome, to = "ASCII//TRANSLIT"))), ".csv")
  write_excel_csv2(orig, file.path(DIR_TAB, arq))
  cat("\nOrigem de", nome, "(ha):\n"); print(orig, n = 30)
}


# -----------------------------------------------------------------------------
# 7. Sobre o que a praia/duna avançou, e o que ocupou a areia
# -----------------------------------------------------------------------------
# Ganho: pixels que viraram Praia, Duna e Areal (23) — em geral avanço de
# areia sobre vegetação, água ou uso antrópico.
# Perda: pixels que eram 23 e viraram outra classe — por exemplo, fixação
# por vegetação ou ocupação.
orig_duna <- trans |>
  filter(xor(de == 23, para == 23)) |>
  mutate(sentido = ifelse(para == 23, "ganho (virou praia/duna)",
                                     "perda (deixou de ser praia/duna)"),
         outra_classe = ifelse(para == 23, classe_de, classe_para)) |>
  group_by(periodo, sentido, outra_classe) |>
  summarise(area_ha = round(sum(area_ha), 2), .groups = "drop") |>
  arrange(periodo, sentido, desc(area_ha))
write_excel_csv2(orig_duna, file.path(DIR_TAB, "origem_ganho_praia_duna.csv"))


# -----------------------------------------------------------------------------
# 8. Rasters das classes de mudança (base para os mapas do script 04)
# -----------------------------------------------------------------------------
# Usa a mesma regra da etapa 3, aplicada pixel a pixel por reclassificação
# do código combinado (origem × 1000 + destino) -> id da classe de mudança.
for (i in seq_len(nrow(periodos))) {
  r1  <- rast(arq_recorte(periodos$ini[i]))
  r2  <- rast(arq_recorte(periodos$fim[i]))
  cmb <- r1 * 1000 + r2
  tab <- trans |>
    filter(periodo == periodos$rotulo[i], !is.na(id)) |>
    transmute(cmb = de * 1000 + para, id)
  mud <- classify(cmb, as.matrix(tab), others = NA)
  names(mud) <- "mudanca"
  writeRaster(mud, file.path(DIR_RAS, paste0("mudanca_", periodos$rotulo[i], ".tif")),
              datatype = "INT1U", NAflag = 0, overwrite = TRUE,
              gdal = c("COMPRESS=DEFLATE"))
}

cat("\nTabelas salvas em:", DIR_TAB, "\nRasters salvos em:", DIR_RAS, "\n")
