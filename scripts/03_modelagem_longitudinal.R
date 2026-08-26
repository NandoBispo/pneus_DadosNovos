# ==============================================================================
# SCRIPT 03: MODELAGEM LONGITUDINAL E ANÁLISE DE RESÍDUOS
# Objetivo: Ajustar o Modelo Linear de Efeitos Mistos (LMM), calcular splines,
# aplicar estrutura varPower e gerar o diagnóstico de Worm Plot.
# Autor: Fernando Oliveira Bispo
# ==============================================================================

# 0. PACOTES ===================================================================
if(!require(pacman)){install.packages("pacman")}
pacman::p_load(tidyverse, nlme, splines, ggplot2)

if(!dir.exists("outputs")) dir.create("outputs")

# 1. CARREGAMENTO DOS DADOS ====================================================
dados_long_treino <- readRDS("data/processed/dados_long_treino.rds") |> 
  filter(!is.na(a_cm), life_km > 0)

dados_surv_treino <- readRDS("data/processed/dados_surv_treino.rds")


# 2. COMPATIBILIZAÇÃO E CONSTRUÇÃO DAS SPLINES =================================
# Criação do ID numérico (Elo para o futuro Modelo Conjunto)
dados_long_treino <- dados_long_treino |>
  mutate(id_numerico = match(serial, unique(dados_surv_treino$serial)))

# Função auxiliar para automatizar o cálculo das splines
calcular_splines <- function(df, n_nos_internos) {
  
  probs <- if(n_nos_internos == 2) c(0.33, 0.66) else c(0.25, 0.50, 0.75)
  knots <- quantile(df$km_acumulada_10k, probs = probs, na.rm = TRUE)
  boundary <- range(df$km_acumulada_10k, na.rm = TRUE)
  
  base_spline <- ns(df$km_acumulada_10k, knots = knots, Boundary.knots = boundary)
  
  df_splines <- as.data.frame(base_spline)
  colnames(df_splines) <- paste0("spline_", 1:ncol(df_splines))
  
  bind_cols(df, df_splines)
}

# Criando a base com 2 nós internos (Referência da Dissertação)
dados_long_v1 <- calcular_splines(dados_long_treino, n_nos_internos = 2)

# Criando a base com 3 nós internos (Para teste de sensibilidade)
dados_long_v2 <- calcular_splines(dados_long_treino, n_nos_internos = 3)


# 3. AJUSTE DOS MODELOS MISTOS (LMM) ===========================================
cat("\nAjustando Modelo LME 1 (2 Nós Internos)...\n")
mod_long_v1 <- lme(
  fixed = a_cm ~ (spline_1 + spline_2 + spline_3) * lado * rodado,
  random = ~ 1 | id_numerico, 
  weights = varPower(form = ~ km_acumulada_10k), 
  data = dados_long_v1,
  na.action = na.exclude,
  control = lmeControl(opt = "optim", maxIter = 200)
)

cat("\nAjustando Modelo LME 2 (3 Nós Internos)...\n")
mod_long_v2 <- lme(
  fixed = a_cm ~ (spline_1 + spline_2 + spline_3 + spline_4) * lado * rodado,
  random = ~ 1 | id_numerico, 
  weights = varPower(form = ~ km_acumulada_10k), 
  data = dados_long_v2,
  na.action = na.exclude,
  control = lmeControl(opt = "optim", maxIter = 200)
)


# 4. DIAGNÓSTICO DE RESÍDUOS (WORM PLOTS) ======================================
# Função para calcular dados do Worm Plot
gerar_dados_worm <- function(modelo, df) {
  df$residuos <- residuals(modelo, type = "normalized")
  
  df |>
    filter(!is.na(residuos)) |>
    mutate(km_cat = ggplot2::cut_number(km_acumulada_10k, n = 6)) |>
    group_by(km_cat) |>
    arrange(residuos) |>
    mutate(
      n_group = n(),
      p_val = (row_number() - 0.5) / n_group,
      z_teo = qnorm(p_val),
      desvio = residuos - z_teo,
      se = sqrt(p_val * (1 - p_val) / n_group) / dnorm(z_teo),
      lim_sup = 1.96 * se,
      lim_inf = -1.96 * se
    ) |>
    ungroup()
}

# Plot V1 (2 Nós)
figura_worm_v1 <- ggplot(gerar_dados_worm(mod_long_v1, dados_long_v1), aes(x = z_teo, y = desvio)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "loess", se = FALSE, color = "blue", span = 0.7) +
  geom_line(aes(y = lim_sup), linetype = "dashed", color = "black") +
  geom_line(aes(y = lim_inf), linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted") +
  facet_wrap(~ km_cat, scales = "free_x", ncol = 3) +
  labs(x = "Quantil Teórico Normal", y = "Desvio (Observado - Teórico)") +
  theme_bw(base_size = 14) +
  theme(strip.background = element_rect(fill = "gray95"), panel.grid.minor = element_blank())

# Plot V2 (3 Nós)
figura_worm_v2 <- ggplot(gerar_dados_worm(mod_long_v2, dados_long_v2), aes(x = z_teo, y = desvio)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "loess", se = FALSE, color = "blue", span = 0.7) +
  geom_line(aes(y = lim_sup), linetype = "dashed", color = "black") +
  geom_line(aes(y = lim_inf), linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted") +
  facet_wrap(~ km_cat, scales = "free_x", ncol = 3) +
  labs(x = "Quantil Teórico Normal", y = "Desvio (Observado - Teórico)") +
  theme_bw(base_size = 14) +
  theme(strip.background = element_rect(fill = "gray95"), panel.grid.minor = element_blank())


# 5. EXPORTAÇÃO DOS ARQUIVOS ===================================================
ggsave("outputs/worm_plot_2_nos.png", plot = figura_worm_v1, width = 10, height = 6, dpi = 300, bg = "white")
ggsave("outputs/worm_plot_3_nos.png", plot = figura_worm_v2, width = 10, height = 6, dpi = 300, bg = "white")

# Salvamos a base processada com os 2 nós internos, pois será a estrutura 
# final utilizada no INLAjoint (Conforme Capítulo 4 da Dissertação)
saveRDS(dados_long_v1, "data/processed/dados_long_inla_ready.rds")

message("\nModelagem Longitudinal Concluída! Gráficos salvos em 'outputs/' e base pronta para o INLA.")