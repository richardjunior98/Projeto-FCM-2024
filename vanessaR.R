library(tidyverse)

dados <- read_csv("C:/Botucatu/UNESP/2024/2024_2/FCM/Projeto Final/Analises/female_players.csv")

# Inspecionar as primeiras linhas dos dados
head(dados)

# Verificar as colunas
colnames(dados)

# Limpeza dos dados: Remover linhas com dados faltantes importantes
dados_limpos <- dados %>%
  filter(!is.na(League), !is.na(Name), !is.na(OVR))

# Verificar as colunas disponíveis
colnames(dados_limpos)

# Selecionar as 5 ligas mais importantes (com base no número de jogadoras por liga)
top_5_ligas <- dados_limpos %>%
  group_by(League) %>%
  tally() %>%
  arrange(desc(n)) %>%
  slice(1:5) %>%
  pull(League)

# Filtrar os dados para as 5 ligas mais importantes
dados_filtrados <- dados_limpos %>%
  filter(League %in% top_5_ligas)


# Filtrando os dados para as 10 ligas mais importantes
dados_filtrados <- dados_limpos %>%
  filter(League %in% top_10_ligas)

# Verificar as colunas disponíveis no conjunto de dados
colnames(dados_filtrados)

grafico_histograma <- ggplot(dados_filtrados, aes(x = OVR, fill = League)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "Distribuição de OVR nas 10 Ligas Mais Importantes",
       x = "OVR da Jogadora", y = "Frequência") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  Exibição do gráfico
print(grafico_histograma)

# Salvar o gráfico de histograma
ggsave("grafico_histograma_10_ligas_importantes.png", plot = grafico_histograma)

grafico_rating_por_liga <- ggplot(dados_filtrados, aes(x = League, y = OVR, fill = League)) +
  geom_boxplot() +
  labs(title = "Distribuição de OVR por Liga", x = "Liga", y = "Rating Geral (OVR)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  Exibição do gráfico
print(grafico_rating_por_liga)

# Salvar o gráfico de distribuição de OVR por liga
ggsave("grafico_rating_por_liga.png", plot = grafico_rating_por_liga)

grafico_idade_vs_OVR <- ggplot(dados_filtrados, aes(x = Age, y = OVR, color = League)) +
  geom_point() +
  labs(title = "Relação entre Idade e Rating Geral nas 10 Ligas Mais Importantes",
       x = "Idade", y = "Rating Geral (OVR)") +
  theme_minimal()

#  Exibição do gráfico
print(grafico_idade_vs_OVR)

# Salvar o gráfico de dispersão de idade vs OVR
ggsave("grafico_idade_vs_OVR.png", plot = grafico_idade_vs_OVR)

# Salvar os dados filtrados como tabela CSV
write_csv(dados_filtrados, "dados_filtrados_10_ligas_importantes.csv")

# Mostrar as primeiras linhas dos dados filtrados para confirmar
head(dados_filtrados)