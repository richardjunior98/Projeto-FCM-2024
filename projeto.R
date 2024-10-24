library(tidyverse)

dados <- read_csv("C:/Users/richa/Desktop/Doutorado/Disciplinas/2º semestre/Ferramentas Computacionais/PROJETO/all_players.csv")

# Usando a função base para renomear de novo
colnames(dados)[colnames(dados) == "Unnamed: 0"] <- "Ranking_by_sex"
colnames(dados)[colnames(dados) == "Unnamed: 0"] <- "Overall ranking"

dados$...1[1:16161] <- "MALE"
dados$...1[16162:nrow(dados)] <- "FEMALE"
dados$Ranking_by_sex <- dados$Ranking_by_sex + 1

View(dados)
