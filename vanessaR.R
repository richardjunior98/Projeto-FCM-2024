library(tidyverse)

dados <- read_csv("C:/Botucatu/UNESP/2024/2024_2/FCM/Projeto Final/Analises/female_players.csv")

str(dados)
head(dados)

dados_limpos <- dados %>%
  filter(!is.na(Position), !is.na(League)) %>%
  mutate(
    posicao = as.factor(Position),
    liga = as.factor(League)
  )


# Gráfico 1: Distribuição de jogadores por posição
grafico1 <- ggplot(dados_limpos, aes(x = posicao)) +
  geom_bar(fill = "skyblue") +
  labs(
    title = "Distribuição de Jogadores por Posição",
    x = "Posição",
    y = "Quantidade"
  ) +
  theme_minimal()

# Gráfico 2: Comparação de ratings gerais por liga e gênero usando facet_wrap
grafico2 <- ggplot(dados_limpos, aes(x = liga, y = OVR, fill = posicao)) +
  geom_boxplot() +
  facet_wrap(~ liga) +
  labs(
    title = "Comparação de Ratings por Liga",
    x = "Liga",
    y = "Rating Geral"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico 3: Relação entre atributos físicos e posição usando facet_grid
grafico3 <- ggplot(dados_limpos, aes(x = Stamina, y = Acceleration, color = posicao)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ liga) +  # Facetar apenas por liga
  labs(
    title = "Relação entre Resistência e Aceleração por Liga",
    x = "Resistência",
    y = "Aceleração"
  ) +
  theme_minimal()


# Exibição dos gráficos
print(grafico1)
print(grafico2)
print(grafico3)

# Salvar gráficos em arquivos PNG
ggsave("distribuicao_posicoes.png", plot = grafico1, width = 8, height = 6)
ggsave("ratings_liga_genero.png", plot = grafico2, width = 10, height = 8)
ggsave("relacao_atributos.png", plot = grafico3, width = 12, height = 8)


colnames(dados)

# Identificar as últimas 10 ligas em ordem alfabética
ligas_unicas <- sort(unique(dados$League), decreasing = TRUE)[1:10]

# Filtrar os dados para essas ligas
dados_filtrados <- dados %>% filter(League %in% ligas_unicas)

head(dados_filtrados)
dados_filtrados <- dados_filtrados %>%
  rename(
    posicao = Position,
    liga = League,
    resistencia = Stamina,
    aceleracao = Acceleration
  )

ggplot(dados_filtrados, aes(x = resistencia)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  facet_wrap(~ posicao, ncol = 3) +
  labs(
    title = "Distribuição de Resistência por Posição",
    x = "Resistência",
    y = "Quantidade"
  ) +
  theme_minimal()

ggplot(dados_filtrados, aes(x = resistencia, y = aceleracao, color = posicao)) +
  geom_point(alpha = 0.6) +
  facet_grid(posicao ~ liga) +
  labs(
    title = "Resistência vs Aceleração por Posição e Liga",
    x = "Resistência",
    y = "Aceleração"
  ) +
  theme_minimal()

ggsave("distribuicao_resistencia.png", width = 8, height = 6)
ggsave("resistencia_vs_aceleracao.png", width = 12, height = 8)

write_csv(dados_filtrados, "C:/Botucatu/UNESP/2024/2024_2/FCM/Projeto Final/Analises/dados_filtrados.csv")

head(dados_filtrados)

getwd()

dados_filtrados <- dados_filtrados %>%
  rename(
    Aceleração = PAC,      # Ajuste se as colunas tiverem nomes diferentes
    Força = PHY,
    Cruzamento = Crossing,
    Passes_Longos = Long Passing
  )


ggplot(dados_filtrados, aes(x = Age)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  facet_wrap(~ posicao, ncol = 3) +
  labs(
    title = "Distribuição de Idade por Posição",
    x = "Idade",
    y = "Quantidade"
  ) +
  theme_minimal()

ggplot(dados_filtrados, aes(x = OVR, fill = posicao)) +
  geom_boxplot() +
  facet_grid(posicao ~ liga) +
  labs(
    title = "Comparação de Rating Geral (OVR) por Posição e Liga",
    x = "Rating Geral (OVR)",
    y = "Distribuição"
  ) +
  theme_minimal()

ggplot(dados_filtrados, aes(x = aceleracao, y = Força, color = posicao)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ posicao, ncol = 4) +
  labs(
    title = "Aceleração vs Força por Posição",
    x = "Aceleração",
    y = "Força"
  ) +
  theme_minimal()

ggplot(dados_filtrados, aes(x = Crossing, y = Long_Passing, color = liga)) +
  geom_point(alpha = 0.6) +
  facet_grid(~ liga) +
  labs(
    title = "Cruzamento vs Passes Longos por Liga",
    x = "Cruzamento",
    y = "Passes Longos"
  ) +
  theme_minimal()

ggplot(dados_filtrados, aes(x = Aceleração, y = Força, color = posicao)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ posicao, ncol = 3) +
  labs(
    title = "Aceleração vs Força por Posição",
    x = "Aceleração",
    y = "Força"
  ) +
  theme_minimal()

ggsave("grafico1.png", width = 8, height = 6)
ggsave("grafico2.png", width = 10, height = 8)

