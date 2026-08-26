# ==============================================================================
# SCRIPT 02: ANÁLISE EXPLORATÓRIA DE DADOS (AED)
# Objetivo: Gerar tabelas descritivas (gtsummary), gráficos de distribuição de
# falhas e avaliar os zeros estruturais.
# Autor: Fernando Oliveira Bispo
# ==============================================================================

# 0. PACOTES ===================================================================
if(!require(pacman)){install.packages("pacman")}
pacman::p_load(tidyverse, gtsummary, kableExtra, summarytools)

# Cria o diretório de outputs caso não exista
if(!dir.exists("outputs")) dir.create("outputs")


# 1. CARREGAMENTO DOS DADOS PROCESSADOS ========================================
dados_surv_treino <- readRDS("data/processed/dados_surv_treino.rds")
dados_surv_teste  <- readRDS("data/processed/dados_surv_teste.rds")


# 2. TABELA DESCRITIVA GERAL (Amostra de Treinamento N = 1999) =================
# Esta é a tabela que agrupa as categorias esparsas e trata a Censura como Status Final
tabela_descritiva <- dados_surv_treino |>
  mutate(
    mecanismo_falha = factor(
      mecanismo_falha,
      levels = c("Desgaste", "Impacto/Corte", "Operacional", "Estrutural", "Outros", "Censurado")
    )
  ) |>
  select(
    mecanismo_falha, lado_pred, trocas_cat, pos_traseira, caminhoes_cat
  ) |>
  gtsummary::tbl_summary(
    statistic = gtsummary::all_categorical() ~ "{n} ({p}%)",
    digits = gtsummary::all_categorical() ~ c(0, 1),
    label = list(
      mecanismo_falha ~ "Status Final / Mecanismo",
      lado_pred ~ "Lado Predominante de Exposição",
      trocas_cat ~ "Política de Rodízio (Trocas)",
      pos_traseira ~ "Posição no Eixo Traseiro",
      caminhoes_cat ~ "Histórico de Caminhões"
    )
  ) |>
  gtsummary::modify_header(label = "**Variável / Categoria**") |>
  gtsummary::bold_labels()

# Para visualizar a tabela no Viewer do Positron
tabela_descritiva


# 3. GRÁFICOS EXPLORATÓRIOS ====================================================

## 3.1 Boxplot de Vida Útil por Mecanismo de Falha ----
figura_boxplot <- dados_surv_treino |> 
  filter(falha == 1) |> 
  mutate(mecanismo_falha = reorder(mecanismo_falha, tempo_total_10k, FUN = median)) |> 
  ggplot(aes(x = mecanismo_falha, y = tempo_total_10k)) +
  geom_boxplot(fill = "steelblue", alpha = 0.8, outlier.color = "red", outlier.shape = 4) +
  labs(
    title = "Distribuição da Vida Útil por Mecanismo de Falha",
    subtitle = "Foco nos eventos observados (excluindo censuras)",
    x = "Mecanismo de Falha",
    y = "Quilometragem Acumulada (x 10.000 km)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(figura_boxplot)

## 3.2 Histograma com a "Linha de Corte" de Falha Prematura ----
figura_histograma <- dados_surv_treino |> 
  filter(falha == 1) |> 
  ggplot(aes(x = tempo_total_10k)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white", alpha = 0.9) +
  geom_vline(xintercept = 3.0, color = "red", linetype = "dashed", linewidth = 1) +
  annotate(
    "text", x = 3.2, y = 150, # Ajuste o 'y' conforme o pico do seu histograma
    label = "Falha Prematura\n(< 30.000 km)", 
    color = "red", hjust = 0, fontface = "bold"
  ) +
  labs(
    title = "Distribuição de Quilometragem no Momento da Falha",
    x = "Quilometragem Acumulada (x 10.000 km)",
    y = "Frequência Absoluta (Nº de Pneus)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

print(figura_histograma)


# 4. AUDITORIA DAS PROPORÇÕES DE TREINO E TESTE ================================
cat("\n--- Proporção de Mecanismos de Falha no Treino (Normais) ---\n")
dados_surv_treino |> 
  select(mecanismo_falha) |> 
  summarytools::freq(round.digits = 3, report.nas = FALSE, totals = TRUE)

cat("\n--- Proporção de Mecanismos de Falha no Teste (Normais) ---\n")
dados_surv_teste |> 
  filter(tipo_comportamento == "Normal") |> 
  select(mecanismo_falha) |> 
  summarytools::freq(round.digits = 3, report.nas = FALSE, totals = TRUE)


# 5. EXPORTAÇÃO DOS GRÁFICOS (Automático) ======================================
# Salva os gráficos em alta resolução (300 dpi) na pasta 'outputs/'
ggsave(filename = "outputs/boxplot_vida_util_mecanismo.png", plot = figura_boxplot, 
       width = 8, height = 6, dpi = 300, bg = "white")

ggsave(filename = "outputs/histograma_falha_prematura.png", plot = figura_histograma, 
       width = 8, height = 6, dpi = 300, bg = "white")

message("\nAnálise Exploratória concluída! Gráficos exportados para a pasta 'outputs/'.")

# ==============================================================================
# COMPLEMENTO SCRIPT 02: ANÁLISE EXPLORATÓRIA LONGITUDINAL
# Cole este bloco no final do seu script 02_analise_exploratoria.R
# ==============================================================================

# Carregando a base longitudinal para a EDA
dados_long_treino <- readRDS("data/processed/dados_long_treino.rds") |> 
  filter(!is.na(a_cm), life_km > 0)

## 1. Gráfico de Espaguete (Trajetórias Individuais por Eixo) ----
figura_espaguete <- dados_long_treino |>
  arrange(serial, cycle) |>
  ggplot(aes(x = km_acumulada_10k, y = a_cm, group = serial)) +
  geom_line(alpha = 0.2, color = "steelblue") + 
  geom_smooth(aes(group = 1), method = "loess", color = "darkred", linewidth = 1.2, se = FALSE) +
  facet_wrap(~ eixo) +
  labs(
    x = "Quilometragem Acumulada (por 10 mil km)",
    y = "Espessura do Sulco Remanescente (cm)"
  ) +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_rect(fill = "gray95"))

ggsave("outputs/espaguete_desgaste_eixo.png", plot = figura_espaguete, width = 10, height = 6, dpi = 300)

## 2. Evolução da Variância Empírica ----
figura_variancia <- dados_long_treino |>
  mutate(
    km_bin = cut(km_acumulada_10k,
                 breaks = seq(0, max(km_acumulada_10k, na.rm = TRUE), by = 1),
                 include.lowest = TRUE)
  ) |>
  group_by(km_bin) |>
  summarise(variancia_a = var(a_cm, na.rm = TRUE), n = n()) |>
  filter(n > 30, !is.na(km_bin)) |> 
  ggplot(aes(x = km_bin, y = variancia_a)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  labs(
    x = "Faixa de Quilometragem Acumulada (por 10 mil km)",
    y = "Variância Empírica da Medida A"
  ) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/variancia_empirica_desgaste.png", plot = figura_variancia, width = 8, height = 6, dpi = 300)

## 3. Correlograma de Variáveis Operacionais ----
figura_correlograma <- dados_long_treino |>
  arrange(serial, cycle) |>
  select(serial, cycle, a_cm) |>
  pivot_wider(names_from = cycle, values_from = a_cm, names_prefix = "Ciclo_") |>
  select(-serial) |>
  cor(use = "pairwise.complete.obs") |> 
  ggcorrplot::ggcorrplot(
    method = "square", type = "lower", lab = TRUE, lab_size = 4,
    colors = c("#e74c3c", "white", "#3498db"),
    ggtheme = ggplot2::theme_bw()
  )

ggsave("outputs/correlograma_ciclos.png", plot = figura_correlograma, width = 8, height = 8, dpi = 300)

