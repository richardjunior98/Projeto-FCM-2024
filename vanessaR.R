# Instalação e carregamento de pacotes necessários
install.packages(c("ggplot2", "dplyr", "readr"))
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)

# Importação dos dados
dados <- read_csv("C:/Botucatu/UNESP/2024/2024_2/FCM/Projeto Final/Analises/all_players.csv")

# Limpeza e transformação dos dados
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

