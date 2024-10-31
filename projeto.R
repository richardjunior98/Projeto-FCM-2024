library(tidyverse)
library(rcartocolor)
library(gt)

# Definir cores
cores <- rcartocolor::carto_pal(12, "Bold")

dados <- read_csv("C:/Users/richa/Desktop/Doutorado/Disciplinas/2º semestre/Ferramentas Computacionais/Projeto_FCM_2024/all_players.csv")

# Usando a função base para renomear de novo
colnames(dados)[colnames(dados) == "Unnamed: 0"] <- "Ranking_by_sex"
colnames(dados)[colnames(dados) == "Unnamed: 0"] <- "Overall ranking"

dados$...1[1:16161] <- "MALE"
dados$...1[16162:nrow(dados)] <- "FEMALE"
dados$Ranking_by_sex <- dados$Ranking_by_sex + 1

View(dados)

df <- filter(dados, Name %in% c("Rodri", "Vini Jr.","Jude Bellingham",
"Carvajal","Erling Haaland","Kylian Mbappé","Lautaro Martínez","Lamine Yamal","Harry Kane",
"Phil Foden"))
df <- df[-11, ]
df <- df[,-3]
nova_coluna=c(6,1,5,3,2,10,7,11,4,8);
df <- df %>%
  add_column(Rank_Ballon_dor = nova_coluna, .after = "Ranking_by_sex")

View(df)
#write.csv(df, "C:/Users/richa/Desktop/Doutorado/Disciplinas/2º semestre/Ferramentas Computacionais/Projeto_FCM_2024/tabela1.csv", row.names = FALSE)

# Criando a tabela
tabela <- gt(df)
gtsave(tabela, "tabela.png")

league_counts <- df %>%
  group_by(League) %>%
  summarise(Count = n())

# Gráfico de Pizza
p1 <- ggplot(league_counts, aes(x = "", y = Count, fill = League)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição dos Jogadores por Liga", fill = "Ligas") +  # Título do gráfico e da legenda
  theme_void() +  # Remove background and grid
  theme(
    #plot.background = element_rect(fill = "white"),  # Fundo branco para todo o gráfico
    #panel.background = element_rect(fill = "white"),  # Fundo branco para o painel
    legend.position = "right",  # Coloca a legenda à direita
    plot.title = element_text(size = 16, face = "bold"),  # Ajusta título
    legend.title = element_text(size = 15, face = "bold"),  # Título da legenda
    legend.text = element_text(size = 13)  # Aumenta tamanho da fonte da legenda
  ) +
  scale_fill_manual(values = cores) +  # Adiciona cores personalizadas
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5), color = "white",size=6)  # Adiciona números ao gráfico
p1

ggsave("C:/Users/richa/Desktop/Doutorado/Disciplinas/2º semestre/Ferramentas Computacionais/Projeto_FCM_2024/ligas_ballon.png", plot = p1, device = "png", height = 6, width = 10)
