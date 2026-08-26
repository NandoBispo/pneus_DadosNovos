# ==============================================================================
# SCRIPT 01: EXTRAÇÃO, TRANSFORMAÇÃO E CARREGAMENTO (ETL)
# Objetivo: Transformar a base bruta (wide) em bases analíticas (long e surv),
# realizar a classificação de eventos extremos e estratificar em Treino/Teste.
# Autor: Fernando Oliveira Bispo
# ==============================================================================

# 0. PACOTES ===================================================================
if(!require(pacman)){install.packages("pacman")}
pacman::p_load(tidyverse, janitor, readxl, lubridate, JMbayes2)


# 1. LEITURA DO ARQUIVO BRUTO ==================================================
# Certifique-se de que o arquivo Excel está na pasta correta
caminho_bruto <- "data/raw/Datos para modelo de pronóstico (mina A).xlsx"

dados_brutos <- readxl::read_excel(caminho_bruto, sheet = "Minera A") |> 
  janitor::clean_names()


# 2. CONSTRUÇÃO DA BASE LONGITUDINAL (Formato Long) ============================
dados_long <- dados_brutos |>
  select(-matches("life[0-9]+_hr")) |>   
  mutate(across(
    matches("truck[0-9]+|pos[0-9]+|on[0-9]+|off[0-9]+|life[0-9]+_km|a[0-9]+|b[0-9]+|irrw_[0-9]+"),
    as.character
  )) |>
  rename_with(~ str_replace(., "life([0-9]+)_km", "life_km_\\1"), matches("life[0-9]+_km")) |>
  rename_with(~ str_replace(., "^([A-Za-z]+)([0-9]+)$", "\\1_\\2"), matches("^[A-Za-z]+[0-9]+$")) |>
  pivot_longer(
    cols = matches("_[0-9]+$"),        
    names_to = c(".value", "cycle"),   
    names_pattern = "(.*)_([0-9]+)$"   
  ) |>
  filter(!is.na(truck)) |>
  mutate(
    cycle = as.numeric(cycle),
    rec = lubridate::ymd(rec),
    on = lubridate::ymd(on),
    off = lubridate::ymd(off),
    life_km = as.numeric(life_km),
    a_cm = as.numeric(a) / 10, # Convertendo mm para cm conforme texto da dissertação
    pos = as.numeric(pos)
  )


# 3. CONSTRUÇÃO DA BASE DE SOBREVIVÊNCIA (Transversal) =========================
dados_surv <- dados_long |>
  arrange(serial, cycle) |>
  group_by(serial) |>
  mutate(
    lado_ciclo = case_when(
      pos %in% c(1, 3, 4) ~ "E",
      pos %in% c(2, 5, 6) ~ "D",
      TRUE ~ NA_character_
    ),
    pos_traseira_ciclo = case_when(
      pos %in% c(3, 6) ~ "Externo",
      pos %in% c(4, 5) ~ "Interno",
      TRUE ~ "Dianteira"
    ),
    troca_lateral = if_else(row_number() == 1, 0, if_else(lado_ciclo != lag(lado_ciclo), 1, 0))
  ) |>
  summarise(
    fleet = first(fleet),
    size = first(size),
    otd = first(otd),
    
    tempo_total_km = sum(life_km, na.rm = TRUE),
    qtd_ciclos = n(),
    
    km_esquerda = sum(life_km[lado_ciclo == "E"], na.rm = TRUE),
    prop_esquerda = km_esquerda / tempo_total_km,
    
    km_externo = sum(life_km[pos_traseira_ciclo == "Externo"], na.rm = TRUE),
    km_interno = sum(life_km[pos_traseira_ciclo == "Interno"], na.rm = TRUE),
    prop_externo = if_else((km_externo + km_interno) > 0, km_externo / (km_externo + km_interno), NA_real_),
    
    n_trocas_laterais = sum(troca_lateral, na.rm = TRUE),
    n_caminhoes = n_distinct(truck),
    
    status_final = last(status),
    motivo_descarte = last(main_bs_abe)
  ) |>
  ungroup() |>
  
  # Filtro Crítico de Operação
  mutate(prop_externo = if_else(is.infinite(prop_externo), NA_real_, prop_externo)) |>
  filter(!status_final %in% c("Stock", NA), tempo_total_km > 0) |>
  
  # Engenharia de Covariáveis do Modelo
  mutate(
    tempo_total_10k = tempo_total_km / 10000,
    tempo_cat = factor(case_when(
      tempo_total_10k <= 3 ~ "Inicial",
      tempo_total_10k <= 6 ~ "Medio",
      TRUE ~ "Final"
    ), levels = c("Inicial", "Medio", "Final")),
    
    lado_pred = case_when(
      prop_esquerda >= 0.7 ~ "Esquerda",
      prop_esquerda <= 0.3 ~ "Direita",
      TRUE ~ "Equilibrado"
    ),
    
    pos_traseira = case_when(
      is.na(prop_externo) ~ "Sem_exposicao_traseira",
      prop_externo >= 0.6 ~ "Externo",
      prop_externo <= 0.4 ~ "Interno",
      TRUE ~ "Equilibrado"
    ),
    
    trocas_cat = case_when(
      n_trocas_laterais == 0 ~ "Sem_troca",
      n_trocas_laterais == 1 ~ "Uma_troca",
      TRUE ~ "Multiplas_trocas"
    ),
    
    caminhoes_cat = factor(case_when(
      n_caminhoes == 1 ~ "1_Caminhao",
      n_caminhoes == 2 ~ "2_Caminhoes",
      n_caminhoes == 3 ~ "3_Caminhoes",
      TRUE ~ "4_ou_mais_Caminhoes"
    ), levels = c("1_Caminhao", "2_Caminhoes", "3_Caminhoes", "4_ou_mais_Caminhoes")),
    
    falha = if_else(status_final == "Scrapped", 1, 0),
    
    mecanismo_falha = case_when(
      falha == 0 ~ "Censurado",
      motivo_descarte %in% c("WO", "IRRW") ~ "Desgaste",
      motivo_descarte %in% c("SWC", "CSEPL", "CSEPS", "CTH", "ACC") ~ "Impacto/Corte",
      motivo_descarte %in% c("ILSEP", "SWSEP", "BB", "SPS", "BES", "MSEP", "MSEPSHO", "MSEPTREAD") ~ "Estrutural",
      motivo_descarte %in% c("CHUNK", "RIM FRIC", "RF") ~ "Operacional",
      TRUE ~ "Outros"
    ),
    
    status_cr = case_when(
      mecanismo_falha == "Censurado" ~ 0,
      mecanismo_falha == "Desgaste" ~ 1,
      mecanismo_falha == "Impacto/Corte" ~ 2,
      mecanismo_falha == "Operacional" ~ 3,
      mecanismo_falha == "Estrutural" ~ 4,
      TRUE ~ 5 
    )
  )


# 4. CLASSIFICAÇÃO DE EVENTOS EXTREMOS E CONTAMINAÇÕES =========================
dados_classificados <- dados_surv |>
  mutate(
    tipo_comportamento = case_when(
      mecanismo_falha == "Estrutural" ~ "Defeito_Fabrica",
      tempo_total_10k <= 3.0 & falha == 1 ~ "Falha_Prematura",
      TRUE ~ "Normal"
    )
  )


# 5. ESTRATIFICAÇÃO (BASE DE TREINO E TESTE) ===================================
set.seed(20261) # Semente fixada conforme Metodologia da Dissertação

dados_normais <- dados_classificados |> filter(tipo_comportamento == "Normal")

dados_treino_normais <- dados_normais |>
  group_by(mecanismo_falha) |>
  slice_sample(prop = 0.8) |>
  ungroup()

seriais_treino <- dados_treino_normais |> pull(serial)
seriais_teste_normais <- setdiff(dados_normais$serial, seriais_treino)

seriais_prematuros <- dados_classificados |> filter(tipo_comportamento == "Falha_Prematura") |> pull(serial)

# A Base de Teste recebe: Os 20% normais (estratificados) + TODOS os Prematuros
seriais_teste <- c(seriais_teste_normais, seriais_prematuros)

# Criação das bases espelhadas
dados_long_treino <- dados_long |> filter(serial %in% seriais_treino)
dados_long_teste  <- dados_long |> filter(serial %in% seriais_teste)

dados_surv_treino <- dados_classificados |> filter(serial %in% seriais_treino)
dados_surv_teste  <- dados_classificados |> filter(serial %in% seriais_teste)

# 6. EXPORTAÇÃO DAS BASES PROCESSADAS ==========================================
# Verifica se a pasta existe, senão, cria
if(!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)

saveRDS(dados_long_treino, "data/processed/dados_long_treino.rds")
saveRDS(dados_long_teste,  "data/processed/dados_long_teste.rds")

saveRDS(dados_surv_treino, "data/processed/dados_surv_treino.rds")
saveRDS(dados_surv_teste,  "data/processed/dados_surv_teste.rds")

message("ETL concluído com sucesso. Bases analíticas salvas em 'data/processed/'.")