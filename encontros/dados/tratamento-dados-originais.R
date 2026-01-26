
# Tratamento

library(dplyr)
library(tidyr)

# Leitura dos dados V-Dem a partir do arquivo .rds
dados <- readRDS("V-Dem-CY-Full+Others-v14.rds")

# Seleção, renomeação, filtro e tratamento do conjunto de dados
dados_aula <- dados %>%
  select(
    nome_pais = country_name,
    ano = year,
    tipo_regime_cod = v2x_regime,
    regiao_cod = e_regionpol_6C,
    indice_polity_num = e_p_polity,
    dem_liberal_num = v2x_libdem,
    dem_eleitoral_num = v2x_polyarchy,
    populacao_num = e_pop
  ) %>%
  filter(ano >= 2000 & ano <= 2020) %>%
  mutate(
    # Recodifica as variáveis qualitativas
    tipo_regime = case_when(
      tipo_regime_cod == 0 ~ "Autocracia Fechada",
      tipo_regime_cod == 1 ~ "Autocracia Eleitoral",
      tipo_regime_cod == 2 ~ "Democracia Eleitoral",
      tipo_regime_cod == 3 ~ "Democracia Liberal"
    ),
    regiao = case_when(
      regiao_cod == 1 ~ "Europa Oriental e Ásia Central",
      regiao_cod == 2 ~ "América Latina e Caribe",
      regiao_cod == 3 ~ "Oriente Médio e Norte da África",
      regiao_cod == 4 ~ "África Subsaariana",
      regiao_cod == 5 ~ "Europa Ocidental e América do Norte",
      regiao_cod == 6 ~ "Ásia e Pacífico"
    ),
    # Trata os códigos especiais do Índice Polity como NA
    indice_polity = case_when(
      indice_polity_num %in% c(-66, -77, -88) ~ NA_real_,
      TRUE ~ indice_polity_num
    )
  )

write.csv(dados_aula, "dados_vdem_aula.csv", row.names = FALSE)

