#### Introdução a métodos quantitativos ####
## Prof. Emanuel Cordeiro ##

## Carregando pacotes
library(tidyverse)
library(ds4psy)
library(readxl)

# Conceitos básicos

# Multiplicação
2*2
5*14
# Divisão
4/2
# soma
8+9
# subt
(89-34)/2+2

## Tipos de dados
# Vetor
# Lista
# Dataframe

x <- 5
Oscar <- 28
Mariana <- 22
Bia <- 22

Oscar == Pele
Bia == Mariana
Pele <- 50
glimpse(Oscar)
x+y

x-y

x/y

Oscar ^ Bia

Oscar %/% Bia

# Lista
Estudante <- list(Estudante = "Pedro", idade = 22, estado_civil = "solteiro")

Estudante_Oscar <- list(Estudante = "Oscar", idade = 28, estado_civil = "Casado")

Estudante_Oscar

### Construindo um vetor
# Guardando uma lista de nomes
Nomes <- c("Oscar", "Bia", "Tulio", "Mariana", "Inacio", "Vitoria", "Gilberto", "Isis", "Emanuel")
Nomes
# Guardando uma lista de idades
Idade <- c(28, 22, 26, 22, 23, 21, 22, 22, 31)

# Unindo os vetores em um dataframe
Sala_20 <- data.frame(Nomes, Idade)

Sala_21 <- data.frame(Cidade = c("Carlos", "Ana"), Altura = c(22, 35))

Pesquisa <- data.frame(Nomes, Idade, Esportes)

# Visualizar o dataframe
View(Sala_20)

# Selecionar um vetor específico dentro do dataframe
Sala_20$Nomes

## Conceitos de Estatística descritiva 

glimpse(Sala_20)

# Visualizar a média de Idade no dataframe
mean(Sala_20$Idade)

# Visualizar o desvio padrão
sd(Sala_20$Idade)

# Média das respostas
mean(Pesquisa$Esportes)

# Mediana da Idade
median(Sala_20$Idade)

# Apagar as informações
rm 

## Valores aleatórios para teste
mediana <- c(3,6,9,10,15,24,32)
x <- c(62, 38, 29, 19, 10, 48, 90, 98, 27, 47, 20, 1, 4, 67, 56)

# Analisando média, mediana e desvio padrão
mean(x)
median(x)
sd(x)

# Outro modelo de construir o dataframe de modo direto
df <- data.frame(Média = mean(Sala_20$Idade), "Desvio Padrão" = sd(Sala_20$Idade))

df <- data.frame(Média = mean(Sala_20$Idade), 'Desvio Padrão' = sd(Sala_20$Idade))

summary(Sala_20$Idade)
glimpse(Sala_20)
Sala_20 %>% select(Idade) %>% sd()

Sala_20 %>% slice(1,4,5)


# Aula 3 -------------------------------------------------------------------

# Select
#
HSE_AP_2 <- HSE_AP

names(HSE_AP_2)

HSE_AP_3 <- HSE_AP_2 %>% select(1:5, 121,122)

HSE_AP_3 <- HSE_AP_2 %>% select(Idade, "Atividades universitárias")

HSE_AP_3 <- HSE_AP_2 %>% select(!3:7, -121)

HSE_AP_3 <- HSE_AP_2 %>% select(1:34,36,35, everything())

view(HSE_AP_3)

# filter
HSE_AP_4 <- HSE_AP_2 %>% filter(Gênero == "Masculino")

HSE_AP_4 <- HSE_AP_2 %>% select(1:10) %>% filter(Gênero == "Feminino")

# slice
HSE_AP_5 <- HSE_AP_2 %>% slice(22:36)

HSE_AP_5 <- HSE_AP_2 %>% select(1:10) %>% filter(Gênero == "Feminino") %>% slice(1)

HSE_AP_2 %>% slice(-33)

view(HSE_AP_4)
# Quais as principais funções no pacote dplyr
# Select
# filter
# slice
# arrange
# mutate
# group_by

names(HSE_AP)

## select
HSE_AP %>% select(1,2)
HSE_AP %>% select(1:3)
HSE_AP %>% select(Idade)
HSE_AP_2 <- HSE_AP %>% select(1,2,4,6, 45:60)

# reordenando o banco a partir do select
HSE_AP_2 <- HSE_AP %>% select(4:6, everything())

# retirando uma coluna
HSE_AP_2 %>% select(-1)

# Quando desejamos retirar mais de uma coluna em sequencia, utilizaremos o sinal de exclamação
HSE_AP_3 <- HSE_AP_2 %>% select(!3:7, -121)

# selecionar variáveis que começam com determinadas letras
HSE_AP %>% select(starts_with("D")) %>% view()

## filter
# essa função permite a seleção de linhas
HSE_AP %>% filter(Gênero == "Masculino")

HSE_AP %>% select(8) %>% filter(1)
glimpse(HSE_AP)
unique(HSE_AP$`Estado de moradia`)

## Slice
# selecionar linhas
HSE_AP %>% slice(1:5)

## Arrange
# Ordenar os dados por um critério
HSE_AP %>% arrange(Idade)
HSE_AP %>% arrange(desc(Idade))

## Rename
HSE_AP %>% rename(M1 = "Sucesso na universidade, para mim, é fazer as coisas melhor que a maioria da classe.")

## Mutate
# criar variáveis dentro de um dataframe
HSE_AP %>% mutate(MF = "Sucesso na universidade, para mim, é fazer as coisas melhor que a maioria da classe." + "É muito importante, para mim, fazer as tarefas melhor que os colegas.")
HSE_AP %>% mutate(Fator1 = 58+59+60+61+64/5)
HSE_AP_2 <- HSE_AP %>% mutate(Fator1 = 58+59+60+61+64)

df_geral$MF1 <- df_geral %>% select(58:61, 64) %>% rowMeans() # Meta performance 
df_geral$MF2 <- df_geral %>% select(62,63,65:70) %>% rowMeans() # Meta aprender
df_geral$MF3 <- df_geral %>% select(71:74) %>% rowMeans() # Meta evitação

## Group_by
# Agrupa informações
HSE_AP %>% group_by(Gênero)


# Aula 4 ------------------------------------------------------------------

df_ap <- read_xlsx("HSE_AP.xlsx")
df_ss <- read_xlsx("HSE_DOC_SERSO_2.xlsx")

names(df_ap)
names(df_ss)

df_ap <- df_ap %>% rename("1ª Amizade" = "1ª amizade", 
                          "2ª Amizade" = "2ª amizade",
                          "3ª Amizade" = "3ª amizade",
                          "4ª Amizade" = "4ª amizade",
                          "5ª Amizade" = "5ª amizade",
                          "1º Distanciamento" = "1ª distanciamento",
                          "2º Distanciamento" = "2ª distanciamento",
                          "3º Distanciamento" = "3ª distanciamento",
                          "4º Distanciamento" = "4ª distanciamento",
                          "5º Distanciamento" = "5ª distanciamento")

df_geral <- rbind(df_ap, df_ss)
