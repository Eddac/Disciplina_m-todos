## Lista de exercícios

#1 - Construa um dataframe com as seguintes características:
# Uma coluna com 8 participantes, com nomes a sua escola
# Uma coluna com 8 cidades diferentes
# Uma coluna com o Sexo dos participantes
# Uma coluna chamada Felicidade, com as seguintes respostas: 3,4,5,2,1,5,5,4
# Uma coluna chamada Esportes, com as seguintes respostas: 2,3,5,2,2,3,3,4


#2 Reordene esse dataframe colocando as duas últimas colunas na frente do banco de dados.

#3 Filtre os participantes masculinos e coloque em outro banco de dados

#4 Filtre os participantes femininos e coloque em outro banco de dados

library(tidyverse)

df_exe <- data.frame(Nome = c("Oscar", "Luana", "Amanda", "Hugo", "Lucas",
                              "Sara", "Roger", "Laura"),
                     Cidade = c("Olinda", "Recife", "Garanhus", "Paulista",
                                "Igarassu", "Abreu", "Pombos", "Camaragibe"),
                     Sexo = c("Masculino", "Feminino", "Feminino", "Masculino",
                              "Masculino", "Feminino", "Masculino", "Feminino"),
                     Felicidade = c(3,4,5,2,1,5,5,4),
                     Esportes = c(2,3,5,2,2,3,3,4))

df_exe <- df_exe %>% select(4,5, everything())

df_exe_mas <- df_exe %>% filter(Sexo == "Masculino")
df_exe_fem <- df_exe %>% filter(Sexo == "Feminino")

# Exercício 2 -------------------------------------------------------------

# Com base no dataframe construido no 1 exercício, crie uma nova variável somando felicidade e esportes com o nome Fator Geral
# Após carregar o banco de dados HSE_AP e HSE_DOC_SERSO_2, junte os dois bancos para ampliar a amostra

# A partir do exemplo abaixo
df_1 <- data.frame(Nome = c("Carlos", "Eduarda", "Maria"), Idade = c(20,21,20), 
                   Felicidade = c(9,8,7), Inteligencia = c(9,9,7))

df_2 <- data.frame(Nome = c("Carlos", "Eduarda", "Pedro", "Maria", "João"),
                   Bairro = c("A", "A", "B", "C", "A"), H_social = c(9,9,6,5,7))

# Utilize a função inner_join para unir os dois bancos a partir do nome.
# perceba que a quantidade de sujeitos no banco é diferentes, explique a importância dessa função nesses casos.
