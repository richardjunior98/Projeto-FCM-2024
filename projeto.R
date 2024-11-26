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

##############################################################################################################################################################################################

# Criando a tabela
tabela <- gt(df)
gtsave(tabela, "tabela.png")

##############################################################################################################################################################################################

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

##############################################################################################################################################################################################

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

##############################################################################################################################################################################################

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

##############################################################################################################################################################################################

df3 <- dados %>%
  select(sex,Name,Position,League,`GK Diving`, `GK Handling`,`GK Kicking`,`GK Positioning`,`GK Reflexes`)

df3 <- filter(df3, sex == "MALE", Position == "GK", League %in% c("Premier League","Serie A Enilive","Bundesliga",
"LALIGA EA SPORTS","Ligue 1 McDonald's","Liga Portugal","1A Pro League",
"MLS","EFL Championship","Primera División"))

# Gráfico de dispersão
p9 <- ggplot(df3, aes(x = "", y = `GK Diving`, fill = League)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 16, outlier.size = 1.5, width = 1.1) +  # Aumenta a largura dos boxplots
  labs(title = "Distribuição do Mergulho dos Goleiros por Liga",
       x = "",  # Rótulo do eixo x
       y = "Mergulho dos Goleiros (GK Diving)",  # Rótulo do eixo y
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
    ylim(40, 100)  # Define os limites do eixo y de 0 a 100
p9

ggsave("C:/Users/richa/Desktop/Doutorado/Disciplinas/2º semestre/Ferramentas Computacionais/Projeto_FCM_2024/Imagens/ligas_gk_div.png", plot = p9, device = "png", height = 6, width = 10)

# Gráfico de dispersão
p10 <- ggplot(df3, aes(x = "", y = `GK Handling`, fill = League)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 16, outlier.size = 1.5, width = 1.1) +  # Aumenta a largura dos boxplots
  labs(title = "Distribuição do Manuseio dos Goleiros por Liga",
       x = "",  # Rótulo do eixo x
       y = "Manuseio dos Goleiros (GK Handling)",  # Rótulo do eixo y
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
    ylim(40, 100)  # Define os limites do eixo y de 0 a 100
p10

ggsave("C:/Users/richa/Desktop/Doutorado/Disciplinas/2º semestre/Ferramentas Computacionais/Projeto_FCM_2024/Imagens/ligas_gk_hand.png", plot = p10, device = "png", height = 6, width = 10)

# Gráfico de dispersão
p11 <- ggplot(df3, aes(x = "", y = `GK Kicking`, fill = League)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 16, outlier.size = 1.5, width = 1.1) +  # Aumenta a largura dos boxplots
  labs(title = "Distribuição do Chute dos Goleiros por Liga",
       x = "",  # Rótulo do eixo x
       y = "Chute dos Goleiros (GK Kicking)",  # Rótulo do eixo y
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
    ylim(40, 100)  # Define os limites do eixo y de 0 a 100
p11

ggsave("C:/Users/richa/Desktop/Doutorado/Disciplinas/2º semestre/Ferramentas Computacionais/Projeto_FCM_2024/Imagens/ligas_gk_kick.png", plot = p11, device = "png", height = 6, width = 10)

# Gráfico de dispersão
p12 <- ggplot(df3, aes(x = "", y = `GK Positioning`, fill = League)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 16, outlier.size = 1.5, width = 1.1) +  # Aumenta a largura dos boxplots
  labs(title = "Distribuição do Posicionamento dos Goleiros por Liga",
       x = "",  # Rótulo do eixo x
       y = "Posicionamento dos Goleiros (GK Positioning)",  # Rótulo do eixo y
       fill = "Ligas") +
  theme_minimal() +  # Tema minimalista
  theme(
    legend.position = "right",  # Coloca a legenda à direita
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Ajusta título
    legend.title = element_text(size = 11.3, face = "bold"),  # Título da legenda
    legend.text = element_text(size = 10),  # Tamanho da fonte da legenda
    axis.title.x = element_text(size = 11, face = "bold"),  # Título do eixo x em negrito
    axis.title.y = element_text(size = 10, face = "bold")   # Título do eixo y em negrito
  ) +
  scale_fill_manual(values = cores_ligas)+
    ylim(40, 100)  # Define os limites do eixo y de 0 a 100
p12

ggsave("C:/Users/richa/Desktop/Doutorado/Disciplinas/2º semestre/Ferramentas Computacionais/Projeto_FCM_2024/Imagens/ligas_gk_pos.png", plot = p12, device = "png", height = 6, width = 10)

# Gráfico de dispersão
p13 <- ggplot(df3, aes(x = "", y = `GK Reflexes`, fill = League)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 16, outlier.size = 1.5, width = 1.1) +  # Aumenta a largura dos boxplots
  labs(title = "Distribuição dos Reflexos dos Goleiros por Liga",
       x = "",  # Rótulo do eixo x
       y = "Reflexos dos Goleiros (GK Reflexes)",  # Rótulo do eixo y
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
    ylim(40, 100)  # Define os limites do eixo y de 0 a 100
p13

ggsave("C:/Users/richa/Desktop/Doutorado/Disciplinas/2º semestre/Ferramentas Computacionais/Projeto_FCM_2024/Imagens/ligas_gk_refl.png", plot = p13, device = "png", height = 6, width = 10)

##############################################################################################################################################################################################
df4 <- dados %>%
  select(sex,League,Age,PAC,SHO,PAS,DRI,DEF,PHY)

df4 <- filter(df4, sex == "MALE", League %in% c("Premier League","Serie A Enilive","Bundesliga",
"LALIGA EA SPORTS","Ligue 1 McDonald's","Liga Portugal","1A Pro League",
"MLS","EFL Championship","Primera División"))

# Calcular a média de idade por liga
df4 <- df4 %>%
  select(League, Age) %>%       # Selecionar apenas as colunas necessárias
  group_by(League) %>%          # Agrupar os dados por "League"
  summarise(mean_age = mean(Age, na.rm = TRUE)) # Calcular a média de idade

# Gráfico de barra
p14 <- ggplot(df4, aes(x = League, y = mean_age, fill = League)) +
  geom_col(width = 0.7) +  # Cria o gráfico de barras com as médias já calculadas
  labs(
    title = "Média da idade dos jogadores por Liga",
    x = "",  # Sem rótulo no eixo x
    y = "Média da idade dos jogadores",  # Rótulo do eixo y
    fill = "Ligas"
  ) +
  theme_minimal() +  # Tema minimalista
  theme(
    legend.position = "right",  # Coloca a legenda à direita
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Ajusta título
    legend.title = element_text(size = 11, face = "bold"),  # Título da legenda
    legend.text = element_text(size = 10),  # Tamanho da fonte da legenda
    axis.title.x = element_text(size = 11, face = "bold"),  # Título do eixo x em negrito
    axis.title.y = element_text(size = 11.3, face = "bold"),  # Título do eixo y em negrito
    axis.text.x = element_blank(),  # Remove os rótulos do eixo x
    #axis.ticks.x = element_blank()  # Remove as marcas do eixo x
  ) +
  scale_fill_manual(values = cores_ligas)+
    ylim(0, 30)  # Define os limites do eixo y de 0 a 100
p14

ggsave("C:/Users/richa/Desktop/Doutorado/Disciplinas/2º semestre/Ferramentas Computacionais/Projeto_FCM_2024/Imagens/media_idade.png", plot = p14, device = "png", height = 6, width = 10)

##############################################################################################################################################################################################
df5 <- dados %>%
  select(sex,Position,League,Age,Acceleration,`Sprint Speed`,`Finishing`,`Free Kick Accuracy`,PAC,SHO,PAS,DRI,DEF,PHY)

df5 <- filter(df5, sex == "MALE", Position != "GK", League %in% c("Premier League","Serie A Enilive","Bundesliga",
"LALIGA EA SPORTS","Ligue 1 McDonald's"))

# Precisão em faltas
df6 <- df5 %>%
  select(League, Age,`Free Kick Accuracy`) %>%       # Selecionar apenas as colunas necessárias
  group_by(League,Age) %>%          # Agrupar os dados por "League"
  summarise(mean_age = mean(`Free Kick Accuracy`, na.rm = TRUE)) # Calcular a média de idade

p15 <- ggplot(df6, aes(x = Age, y = mean_age, color = League, group = League)) +
  geom_line(size = 1) +  # Cria as linhas para cada liga
  geom_point(size = 2) +  # Adiciona pontos nos dados para melhor visualização
  labs(
    title = "Média de precisão em cobranças de faltas por idade em cada liga",
    x = "Idade",  # Rótulo do eixo x
    y = "Média de precisão em cobranças de faltas",  # Rótulo do eixo y
    color = "Ligas"  # Legenda para as cores
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = seq(15, 40, by = 1),  # Exibe todas as idades de 15 a 40
    expand = c(0.000001, 1)  # Aumenta o espaçamento entre os valores do eixo
  ) +
  theme(
    legend.position = "right",  # Coloca a legenda à direita
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Ajusta título
    legend.title = element_text(size = 11, face = "bold"),  # Título da legenda
    legend.text = element_text(size = 10),  # Tamanho da fonte da legenda
    axis.title.x = element_text(size = 12, face = "bold"),  # Título do eixo x em negrito
    axis.title.y = element_text(size = 12, face = "bold"),   # Título do eixo y em negrito
    axis.text.x = element_text(size = 10,  angle = 45, hjust = 1,margin = margin(t = 10))  # Adiciona espaçamento nos rótulos do eixo X
  ) +
  scale_color_manual(values = cores_ligas)
p15

ggsave("C:/Users/richa/Desktop/Doutorado/Disciplinas/2º semestre/Ferramentas Computacionais/Projeto_FCM_2024/Imagens/media_accur_falta.png", plot = p15, device = "png", height = 6, width = 10)

# Aceleração
df7 <- df5 %>%
  select(League, Age,`Acceleration`) %>%       # Selecionar apenas as colunas necessárias
  group_by(League,Age) %>%          # Agrupar os dados por "League"
  summarise(mean_age = mean(`Acceleration`, na.rm = TRUE)) # Calcular a média de idade

p16 <- ggplot(df7, aes(x = Age, y = mean_age, color = League, group = League)) +
  geom_line(size = 1) +  # Cria as linhas para cada liga
  geom_point(size = 2) +  # Adiciona pontos nos dados para melhor visualização
  labs(
    title = "Média de aceleração por idade em cada liga",
    x = "Idade",  # Rótulo do eixo x
    y = "Média de aceleração",  # Rótulo do eixo y
    color = "Ligas"  # Legenda para as cores
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = seq(15, 40, by = 1),  # Exibe todas as idades de 15 a 40
    expand = c(0.000001, 1)  # Aumenta o espaçamento entre os valores do eixo
  ) +
  theme(
    legend.position = "right",  # Coloca a legenda à direita
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Ajusta título
    legend.title = element_text(size = 11, face = "bold"),  # Título da legenda
    legend.text = element_text(size = 10),  # Tamanho da fonte da legenda
    axis.title.x = element_text(size = 12, face = "bold"),  # Título do eixo x em negrito
    axis.title.y = element_text(size = 12, face = "bold"),   # Título do eixo y em negrito
    axis.text.x = element_text(size = 10,  angle = 45, hjust = 1,margin = margin(t = 10))  # Adiciona espaçamento nos rótulos do eixo X
  ) +
  scale_color_manual(values = cores_ligas)
p16

ggsave("C:/Users/richa/Desktop/Doutorado/Disciplinas/2º semestre/Ferramentas Computacionais/Projeto_FCM_2024/Imagens/media_accele.png", plot = p16, device = "png", height = 6, width = 10)

# Velocidade
df8 <- df5 %>%
  select(League, Age,`Sprint Speed`) %>%       # Selecionar apenas as colunas necessárias
  group_by(League,Age) %>%          # Agrupar os dados por "League"
  summarise(mean_age = mean(`Sprint Speed`, na.rm = TRUE)) # Calcular a média de idade

p17 <- ggplot(df8, aes(x = Age, y = mean_age, color = League, group = League)) +
  geom_line(size = 1) +  # Cria as linhas para cada liga
  geom_point(size = 2) +  # Adiciona pontos nos dados para melhor visualização
  labs(
    title = "Média de velocidade em corrida por idade em cada liga",
    x = "Idade",  # Rótulo do eixo x
    y = "Média de velocidade em corrida",  # Rótulo do eixo y
    color = "Ligas"  # Legenda para as cores
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = seq(15, 40, by = 1),  # Exibe todas as idades de 15 a 40
    expand = c(0.000001, 1)  # Aumenta o espaçamento entre os valores do eixo
  ) +
  theme(
    legend.position = "right",  # Coloca a legenda à direita
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Ajusta título
    legend.title = element_text(size = 11, face = "bold"),  # Título da legenda
    legend.text = element_text(size = 10),  # Tamanho da fonte da legenda
    axis.title.x = element_text(size = 12, face = "bold"),  # Título do eixo x em negrito
    axis.title.y = element_text(size = 12, face = "bold"),   # Título do eixo y em negrito
    axis.text.x = element_text(size = 10,  angle = 45, hjust = 1,margin = margin(t = 10))  # Adiciona espaçamento nos rótulos do eixo X
  ) +
  scale_color_manual(values = cores_ligas)
p17

ggsave("C:/Users/richa/Desktop/Doutorado/Disciplinas/2º semestre/Ferramentas Computacionais/Projeto_FCM_2024/Imagens/media_veloci.png", plot = p17, device = "png", height = 6, width = 10)

# Finalização
df9 <- df5 %>%
  select(League, Age,Finishing) %>%       # Selecionar apenas as colunas necessárias
  group_by(League,Age) %>%          # Agrupar os dados por "League"
  summarise(mean_age = mean(Finishing, na.rm = TRUE)) # Calcular a média de idade

p18 <- ggplot(df9, aes(x = Age, y = mean_age, color = League, group = League)) +
  geom_line(size = 1) +  # Cria as linhas para cada liga
  geom_point(size = 2) +  # Adiciona pontos nos dados para melhor visualização
  labs(
    title = "Média de finalização por idade em cada liga",
    x = "Idade",  # Rótulo do eixo x
    y = "Média de finalização",  # Rótulo do eixo y
    color = "Ligas"  # Legenda para as cores
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = seq(15, 40, by = 1),  # Exibe todas as idades de 15 a 40
    expand = c(0.000001, 1)  # Aumenta o espaçamento entre os valores do eixo
  ) +
  theme(
    legend.position = "right",  # Coloca a legenda à direita
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Ajusta título
    legend.title = element_text(size = 11, face = "bold"),  # Título da legenda
    legend.text = element_text(size = 10),  # Tamanho da fonte da legenda
    axis.title.x = element_text(size = 12, face = "bold"),  # Título do eixo x em negrito
    axis.title.y = element_text(size = 12, face = "bold"),   # Título do eixo y em negrito
    axis.text.x = element_text(size = 10,  angle = 45, hjust = 1,margin = margin(t = 10))  # Adiciona espaçamento nos rótulos do eixo X
  ) +
  scale_color_manual(values = cores_ligas)
p18

ggsave("C:/Users/richa/Desktop/Doutorado/Disciplinas/2º semestre/Ferramentas Computacionais/Projeto_FCM_2024/Imagens/media_finish.png", plot = p18, device = "png", height = 6, width = 10)
