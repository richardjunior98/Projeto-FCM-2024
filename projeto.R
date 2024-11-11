library(tidyverse)
library(rcartocolor)
library(gt)

# Definir cores
#cores <- rcartocolor::carto_pal(12, "Bold")

dados <- read_csv("C:/Users/richa/Desktop/Doutorado/Disciplinas/2º semestre/Ferramentas Computacionais/Projeto_FCM_2024/all_players.csv")

# Usando a função base para renomear de novo
colnames(dados)[colnames(dados) == "Unnamed: 0"] <- "Ranking_by_sex"
colnames(dados)[colnames(dados) == "Unnamed: 0"] <- "Overall ranking"

dados$...1[1:16161] <- "MALE"
dados$...1[16162:nrow(dados)] <- "FEMALE"
dados$Ranking_by_sex <- dados$Ranking_by_sex + 1
colnames(dados)[colnames(dados) == "...1"] <- "sex"

View(dados)

df <- filter(dados, Name %in% c("Rodri", "Vini Jr.","Jude Bellingham",
"Carvajal","Erling Haaland","Kylian Mbappé","Lautaro Martínez","Lamine Yamal","Harry Kane",
"Phil Foden"))
df <- df[-11, ]
df <- df[,-3]
nova_coluna=c(6,1,5,3,2,10,7,11,4,8);
df <- df %>%
  add_column(Rank_Ballon_dor = nova_coluna, .after = "Ranking_by_sex")

#View(df)
#write.csv(df, "C:/Users/richa/Desktop/Doutorado/Disciplinas/2º semestre/Ferramentas Computacionais/Projeto_FCM_2024/tabela1.csv", row.names = FALSE)

# Criando a tabela
tabela <- gt(df)
gtsave(tabela, "tabela.png")

league_counts <- df %>%
  group_by(League) %>%
  summarise(Count = n())

# Definir cores específicas para cada liga
cores_ligas <- c(
  "Bundesliga" = carto_pal(12, "Vivid")[1],  # Roxo
  "LALIGA EA SPORTS" = carto_pal(12, "Vivid")[2],  # Verde
  "Premier League" = carto_pal(12, "Vivid")[3],  # Azul
  "Serie A Enilive" = carto_pal(12, "Vivid")[4],  # Amarelo
  "Ligue 1 McDonald's" = carto_pal(12, "Vivid")[5],  # Vermelho
  "Liga Portugal" = carto_pal(12, "Vivid")[11],  # Cor adicional
  "1A Pro League" = carto_pal(12, "Earth")[4],  # Cor adicional
  "MLS" = carto_pal(12, "Peach")[5],  # Cor adicional
  "EFL Championship" = carto_pal(12, "Vivid")[12],  # Cor adicional
  "Primera División" = carto_pal(12, "Vivid")[10]  # Cor adicional
)

# Gráfico de Pizza
p1 <- ggplot(league_counts, aes(x = "", y = Count, fill = League)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição dos Melhores do Mundo de 2024 por Liga", fill = "Ligas") +  # Título do gráfico e da legenda
  theme_void() +  # Remove background and grid
  theme(
    #plot.background = element_rect(fill = "white"),  # Fundo branco para todo o gráfico
    #panel.background = element_rect(fill = "white"),  # Fundo branco para o painel
    legend.position = "right",  # Coloca a legenda à direita
    plot.title = element_text(size = 16, face = "bold"),  # Ajusta título
    legend.title = element_text(size = 15, face = "bold"),  # Título da legenda
    legend.text = element_text(size = 13)  # Aumenta tamanho da fonte da legenda
  ) +
  scale_fill_manual(values = cores_ligas) +  # Adiciona cores personalizadas
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5), color = "white",size=6)  # Adiciona números ao gráfico
p1

ggsave("C:/Users/richa/Desktop/Doutorado/Disciplinas/2º semestre/Ferramentas Computacionais/Projeto_FCM_2024/ligas_ballon.png", plot = p1, device = "png", height = 6, width = 10)

league_counts_2 <- dados %>%
  slice_head(n = 10) %>%  # Seleciona as 10 primeiras linhas
  group_by(League) %>%
  summarise(Count = n())

# Gráfico de Pizza
p2 <- ggplot(league_counts_2, aes(x = "", y = Count, fill = League)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição dos Jogadores com Melhores Overall's por Liga", fill = "Ligas") +  # Título do gráfico e da legenda
  theme_void() +  # Remove background and grid
  theme(
    #plot.background = element_rect(fill = "white"),  # Fundo branco para todo o gráfico
    #panel.background = element_rect(fill = "white"),  # Fundo branco para o painel
    legend.position = "right",  # Coloca a legenda à direita
    plot.title = element_text(size = 16, face = "bold"),  # Ajusta título
    legend.title = element_text(size = 15, face = "bold"),  # Título da legenda
    legend.text = element_text(size = 13)  # Aumenta tamanho da fonte da legenda
  ) +
  scale_fill_manual(values = cores_ligas) +  # Adiciona cores personalizadas
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5), color = "white",size=6)  # Adiciona números ao gráfico
p2

ggsave("C:/Users/richa/Desktop/Doutorado/Disciplinas/2º semestre/Ferramentas Computacionais/Projeto_FCM_2024/ligas_ea.png", plot = p2, device = "png", height = 6, width = 10)

df2 <- dados %>%
  select(sex,Position,League,PAC,SHO,PAS,DRI,DEF,PHY)

df2 <- filter(df2, sex == "MALE", Position != "GK", League %in% c("Premier League","Serie A Enilive","Bundesliga",
"LALIGA EA SPORTS","Ligue 1 McDonald's","Liga Portugal","1A Pro League",
"MLS","EFL Championship","Primera División"))
#df2 <- filter(df2, sex == "MALE", League %in% c("Premier League","Serie A Enilive","Bundesliga",
#"LALIGA EA SPORTS","Ligue 1 McDonald's"))
#View(df2)

# Gráfico de dispersão
p3 <- ggplot(df2, aes(x = "", y = PAC, fill = League)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 16, outlier.size = 1.5, width = 1.1) +  # Aumenta a largura dos boxplots
  labs(title = "Distribuição da Velocidade dos Jogadores por Liga",
       x = "",  # Rótulo do eixo x
       y = "Velocidade dos jogadores (PAC)",  # Rótulo do eixo y
       fill = "Ligas") +
  theme_minimal() +  # Tema minimalista
  theme(
    legend.position = "right",  # Coloca a legenda à direita
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Ajusta título
    legend.title = element_text(size = 11.3, face = "bold"),  # Título da legenda
    legend.text = element_text(size = 10),  # Tamanho da fonte da legenda
    axis.title.x = element_text(size = 11, face = "bold"),  # Título do eixo x em negrito
    axis.title.y = element_text(size = 11.3, face = "bold")   # Título do eixo y em negrito
  ) +
  scale_fill_manual(values = cores_ligas)+
    ylim(0, 100)  # Define os limites do eixo y de 0 a 100
p3

ggsave("C:/Users/richa/Desktop/Doutorado/Disciplinas/2º semestre/Ferramentas Computacionais/Projeto_FCM_2024/Imagens/ligas_pac.png", plot = p3, device = "png", height = 6, width = 10)

# Gráfico de dispersão
p4 <- ggplot(df2, aes(x = "", y = SHO, fill = League)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 16, outlier.size = 1.5, width = 1.1) +  # Aumenta a largura dos boxplots
  labs(title = "Distribuição do Chute dos Jogadores por Liga",
       x = "",  # Rótulo do eixo x
       y = "Chute dos jogadores (SHO)",  # Rótulo do eixo y
       fill = "Ligas") +
  theme_minimal() +  # Tema minimalista
  theme(
    legend.position = "right",  # Coloca a legenda à direita
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Ajusta título
    legend.title = element_text(size = 11.3, face = "bold"),  # Título da legenda
    legend.text = element_text(size = 10),  # Tamanho da fonte da legenda
    axis.title.x = element_text(size = 11, face = "bold"),  # Título do eixo x em negrito
    axis.title.y = element_text(size = 11.3, face = "bold")   # Título do eixo y em negrito
  ) +
  scale_fill_manual(values = cores_ligas)+
    ylim(0, 100)  # Define os limites do eixo y de 0 a 100
p4

ggsave("C:/Users/richa/Desktop/Doutorado/Disciplinas/2º semestre/Ferramentas Computacionais/Projeto_FCM_2024/Imagens/ligas_sho.png", plot = p4, device = "png", height = 6, width = 10)

# Gráfico de dispersão
p5 <- ggplot(df2, aes(x = "", y = PAS, fill = League)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 16, outlier.size = 1.5, width = 1.1) +  # Aumenta a largura dos boxplots
  labs(title = "Distribuição do Passe dos Jogadores por Liga",
       x = "",  # Rótulo do eixo x
       y = "Passe dos jogadores (PAS)",  # Rótulo do eixo y
       fill = "Ligas") +
  theme_minimal() +  # Tema minimalista
  theme(
    legend.position = "right",  # Coloca a legenda à direita
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Ajusta título
    legend.title = element_text(size = 11.3, face = "bold"),  # Título da legenda
    legend.text = element_text(size = 10),  # Tamanho da fonte da legenda
    axis.title.x = element_text(size = 11, face = "bold"),  # Título do eixo x em negrito
    axis.title.y = element_text(size = 11.3, face = "bold")   # Título do eixo y em negrito
  ) +
  scale_fill_manual(values = cores_ligas)+
    ylim(0, 100)  # Define os limites do eixo y de 0 a 100
p5

ggsave("C:/Users/richa/Desktop/Doutorado/Disciplinas/2º semestre/Ferramentas Computacionais/Projeto_FCM_2024/Imagens/ligas_pas.png", plot = p5, device = "png", height = 6, width = 10)

# Gráfico de dispersão
p6 <- ggplot(df2, aes(x = "", y = DRI, fill = League)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 16, outlier.size = 1.5, width = 1.1) +  # Aumenta a largura dos boxplots
  labs(title = "Distribuição do Drible dos Jogadores por Liga",
       x = "",  # Rótulo do eixo x
       y = "Drible dos jogadores (DRI)",  # Rótulo do eixo y
       fill = "Ligas") +
  theme_minimal() +  # Tema minimalista
  theme(
    legend.position = "right",  # Coloca a legenda à direita
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Ajusta título
    legend.title = element_text(size = 11.3, face = "bold"),  # Título da legenda
    legend.text = element_text(size = 10),  # Tamanho da fonte da legenda
    axis.title.x = element_text(size = 11, face = "bold"),  # Título do eixo x em negrito
    axis.title.y = element_text(size = 11.3, face = "bold")   # Título do eixo y em negrito
  ) +
  scale_fill_manual(values = cores_ligas)+
    ylim(0, 100)  # Define os limites do eixo y de 0 a 100
p6

ggsave("C:/Users/richa/Desktop/Doutorado/Disciplinas/2º semestre/Ferramentas Computacionais/Projeto_FCM_2024/Imagens/ligas_dri.png", plot = p6, device = "png", height = 6, width = 10)

# Gráfico de dispersão
p7 <- ggplot(df2, aes(x = "", y = DEF, fill = League)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 16, outlier.size = 1.5, width = 1.1) +  # Aumenta a largura dos boxplots
  labs(title = "Distribuição da Defesa dos Jogadores por Liga",
       x = "",  # Rótulo do eixo x
       y = "Defesa dos jogadores (DEF)",  # Rótulo do eixo y
       fill = "Ligas") +
  theme_minimal() +  # Tema minimalista
  theme(
    legend.position = "right",  # Coloca a legenda à direita
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Ajusta título
    legend.title = element_text(size = 11.3, face = "bold"),  # Título da legenda
    legend.text = element_text(size = 10),  # Tamanho da fonte da legenda
    axis.title.x = element_text(size = 11, face = "bold"),  # Título do eixo x em negrito
    axis.title.y = element_text(size = 11.3, face = "bold")   # Título do eixo y em negrito
  ) +
  scale_fill_manual(values = cores_ligas)+
    ylim(0, 100)  # Define os limites do eixo y de 0 a 100
p7

ggsave("C:/Users/richa/Desktop/Doutorado/Disciplinas/2º semestre/Ferramentas Computacionais/Projeto_FCM_2024/Imagens/ligas_def.png", plot = p7, device = "png", height = 6, width = 10)

# Gráfico de dispersão
p8 <- ggplot(df2, aes(x = "", y = DEF, fill = League)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 16, outlier.size = 1.5, width = 1.1) +  # Aumenta a largura dos boxplots
  labs(title = "Distribuição do Físico dos Jogadores por Liga",
       x = "",  # Rótulo do eixo x
       y = "Físico dos jogadores (PHY)",  # Rótulo do eixo y
       fill = "Ligas") +
  theme_minimal() +  # Tema minimalista
  theme(
    legend.position = "right",  # Coloca a legenda à direita
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Ajusta título
    legend.title = element_text(size = 11.3, face = "bold"),  # Título da legenda
    legend.text = element_text(size = 10),  # Tamanho da fonte da legenda
    axis.title.x = element_text(size = 11, face = "bold"),  # Título do eixo x em negrito
    axis.title.y = element_text(size = 11.3, face = "bold")   # Título do eixo y em negrito
  ) +
  scale_fill_manual(values = cores_ligas)+
    ylim(0, 100)  # Define os limites do eixo y de 0 a 100
p8

ggsave("C:/Users/richa/Desktop/Doutorado/Disciplinas/2º semestre/Ferramentas Computacionais/Projeto_FCM_2024/Imagens/ligas_phy.png", plot = p8, device = "png", height = 6, width = 10)
