#### Introdução a métodos quantitativos ####
## Prof. Emanuel Cordeiro ##

## Carregando pacotes
library(tidyverse)
library(ds4psy)
library(readxl)
library(pander)
library(descr)
library(janitor)
library(ggplot2)
library(psych)
library(ggcorrplot)
library(ggpubr)
# Aula 1 ------------------------------------------------------------------

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


# Aula 2 ------------------------------------------------------------------

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
# A partir de agora, utilizaremos um banco de dados pertencente a pesquisa de doutorado de Emanuel
# O banco de dados corresponde a uma turma específica da amostra da tese
# Para carregar essas informações, será necessário utilizar uma função específica read_slsx
# É importante lembrar que o arquivo do banco deve estar no mesmo diretório do projeto

# Carregando o banco de dados
HSE_AP <- read_xlsx("HSE_AP.xlsx")

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
HSE_AP_2 <- HSE_AP %>% select(!3:7, -121)
HSE_AP_2 <- HSE_AP %>% select(1:34,36,35, everything())

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
HSE_AP %>% slice(-33)

## Arrange
# Ordenar os dados por um critério
HSE_AP %>% arrange(Idade)
HSE_AP %>% arrange(desc(Idade))

## Mutate e #rename
# Rename
HSE_AP <- HSE_AP %>% rename(H1 = "Tenho jeito para lidar com pessoas problemáticas.",
                            H2 = "Detecto a influência, positiva ou negativa, que outras pessoas exercem sobre as minhas emoções.")
# criar variáveis dentro de um dataframe

# Mutate
HSE_AP <- HSE_AP %>% mutate(HG = H1 + H2)
HSE_AP <- HSE_AP %>% mutate(HGDIVI = H1/H2)
HSE_AP <- HSE_AP %>% select(1:13,123,124, everything())

# Calculando a média dos itens para chegar ao fator
df_geral$MF1 <- df_geral %>% select(58:61, 64) %>% rowMeans() # Meta performance 
df_geral$MF2 <- df_geral %>% select(62,63,65:70) %>% rowMeans() # Meta aprender
df_geral$MF3 <- df_geral %>% select(71:74) %>% rowMeans() # Meta evitação

## Group_by
# Agrupa informações
HSE_AP %>% group_by(Gênero)

# O uso do group_by é constantemente aplicado para análises estatísticas
HSE_AP %>% group_by(Gênero) %>% summarise("Coluna H1" = mean(H1), 
                                          "Desvio Padrão" = sd(H1),
                                          Quantidade = n())

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

# Outras combinações de tabela
# Inner join
df_1 <- data.frame(Nome = c("Carlos", "Eduarda", "Maria"), Idade = c(20,21,20), 
                   Felicidade = c(9,8,7), Inteligencia = c(9,9,7))

df_2 <- data.frame(Nome = c("Carlos", "Eduarda", "Pedro", "Maria", "João"),
                   Bairro = c("A", "A", "B", "C", "A"), H_social = c(9,9,6,5,7))


# Aula 31/01/2023 ---------------------------------------------------------

# Introdução as análises estatísticas
# Análise de qui-quadrado
# O teste apresenta três versões
# Entendendo o conceito do teste de hipóteses
# Hipótese Nula/Hipótese alternativa
# Conceito do valor de p
# Nesse teste, analisamos a associação entre variáveis a partir de uma escolha ou tipo de resposta
# variáveis dicotômicas ou nominais

# Retomando conceitos de estatística descritiva
df_exe %>% select(1) %>% colSums()
df_exe %>% select(1) %>% colMeans()
df_exe %>% select(1,2) %>% colMeans()

count(df_exe, Sexo)

df_exe %>% count(Sexo) %>% mutate(porc = n/sum(n)*100)

median(df_exe$Felicidade)
sd(df_exe$Felicidade)


# O foco dessa análise está na frequência de resposta sobre categorias, e diferenças nos padrões de grupos
# Há algumas possibilidades de executar essa análise

# 1º Possibilidade
# Qui-quadrado de independência
# Tabela com as proporção de respostas e respostas esperadas
descr::CrossTable(Base_ADHD.2020$sex_male, Base_ADHD.2020$adhd_parent, 
                  expected = T, prop.c = F, prop.chisq = F, prop.t = F,
                  dnn = c("Sexo", "TDAH"))

# resultado do teste qui-quadrado
descr::CrossTable(Base_ADHD.2020$sex_male, Base_ADHD.2020$adhd_parent, 
                  chisq = T)$CST

# Instalar o pacote ggplot
# Esse gráfico permite ver a proporção de resposta por sexo
ggplot(Base_ADHD.2020, aes(x = sex_male, fill = adhd_parent)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(x = "Sexo", y = "Proporção", fill = "TDAH")

# Hipótese nula foi rejeitada
# tabela com resultados olhando de forma vertical
Base_ADHD.2020 %>% tabyl(sex_male, adhd_parent) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("col") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns() %>% 
  slice(2,1,3) %>% 
  select(sex_male, yes, no, Total) %>% 
  pander()

# Criando um novo dataframe para testar outra forma de fazer o qui-quadrado
Habilidade <- data.frame(Corrida=c(3,5,5,2,6), Escalada=c(7,1,8,4,4),
                         row.names = c('Tiago', 'Pedro', 'Maria', 'Karla','Brenda'))

# salvando a análise do qui-quadrado em um dataframe com nome de x1
x1 <- chisq.test(Habilidade)
# resultado do qui-quadrado
x1
# respostas observadas por categoria
x1$observed
# respostas esperadas por categoria
x1$expected
# diferença entre respostas esperadas e obtidas gera os resíduos
x1$residuals
# desvio padrão dos resíduos
x1$stdres



# Aula 28/02/2023 --------------------------------------------------------------
# Qui quadrado de aderencia
festa <- data.frame(doces = c(90,80,50,95,40), 
                                 row.names = c("Pedro","Maria", 
                                               "Samuel", "Mari", 
                                               "Joao"))

festa <- data.frame(doces = c(90,80,50,95,40),
                    row.names = c("Pedro", "Maria", "Samuel",
                                  "Mari", "Joao"))

options(scipen = 999)

x_festa <- chisq.test(festa, p=c(0.20, 0.20, 0.20, 0.20, 0.20))

x_festa

view(x_festa)

x_festa$observed
x_festa$expected
x_festa$residuals
x_festa$stdres

novoalfa <- 0.05/5
# Valor do novo alfa 0.01
qnorm(novoalfa/2)
# Ponto de corte dos resíduos = 2.57

# Carregando banco de dados DADOS1_EMAN
df_pf <- read_xlsx("DADOS1_EMAN.xlsx")

df_pf$Percentil <- df_pf$Percentil*100


# transformando o percentil
df_pf$Percentil <- df_pf$Percentil*100

df_pf %>% select(Acertos) %>% summary

# Preparando análises para o test t

# t test para amostras independentes
df_pf_ttest <- t.test(Total ~ Gênero, var.equal = T, data = df_pf)

# visualizando a tabela
df_pf_ttest %>% pander(., split.table = Inf)

# criando dados aleatórios como nova coluna
df_pf$Acertos_2 <- rnorm(256, mean = 125, sd = 4)

# Olhando o sumário da nova coluna
df_pf %>% select(Acertos_2) %>% summary()
# Transformando em banco vertical

df_pf_long_2 <- df_pf %>% select(1,5,10)

df_pf_long <- df_pf %>% select(1,5,10) %>% gather("Acertos", "Total", 2:3)

# t test pareado
t.test(Total ~ Acertos, paired = TRUE, data = df_pf_long)


# Aula 07/03/2023 ---------------------------------------------------------

qqnorm(df_pf$Acertos)
qqline(df_pf$Acertos)
shapiro.test(df_pf$Acertos)

ggplot(data.frame(x = df_pf$Acertos), aes(x)) +
  geom_density(fill = "blue")

ggplot(data.frame(x = df_pf$Acertos), aes(x)) +
  geom_density(fill = "blue", alpha = 0.2) +
  ggtitle("Distribuição dos Dados")+
  xlab("Valores")+
  ylab("Participantes")

ggplot(data.frame(x = df_pf$Acertos), aes(x))+
  geom_histogram(fill = "yellow")

ggplot(data.frame(x = df_pf$Acertos), aes(x)) +
  geom_density(fill = "blue", alpha = 0.2) +
  ggtitle("Distribuição dos Dados") +
  xlab("Valores")

ggplot(data.frame(x = df_pf$Total), aes(x)) +
  geom_density(fill = "blue", alpha = 0.2) +
  ggtitle("Distribuição dos Dados") +
  xlab("Valores")

# Teste ANOVA

modelo_anova <- aov(Total ~ Escolaridade, data = df_pf)
summary(modelo_anova)
modelo_anova$coefficients
modelo_anova$fitted.values

modelo_anova <- aov(Total ~ Escolaridade, data = df_pf)
summary(modelo_anova)

modelo_anova_hocbonf <- glht(modelo_anova) 
summary(modelo_anova_hocbonf)

# Solução da atividade

df_pf <- df_pf %>% mutate(Idade_Grupo = ifelse(Idade <= 27, "Idade_1", 
                                               ifelse(Idade %in% 28:35, "Idade_2",
                                                      ifelse(Idade >= 36, "Idade_3", 0))))

df_pf <- df_pf %>% mutate(Idade_Grupo = ifelse(Idade <= 27, "Idade_1", 
                                               ifelse(Idade  %in% 28:35, "Idade_2", 
                                                      ifelse(Idade >= 36, "Idade_3", 0))))

df_pf <- df_pf %>% mutate(Escolaridade_Grupo = ifelse(Escolaridade %in% c("FC", "FI", "EMI"), "Ensino Básico", 
                                                      ifelse(Escolaridade %in% c("EMC", "SI"), "Ensino Médio", 
                                                             ifelse(Escolaridade %in% c("SC"), "Ensino Superior", 0))))

aov(Total ~ Escolaridade_Grupo, data = df_pf)

modelo_anova_2 <- aov(Total ~ Escolaridade_Grupo, data = df_pf)
summary(modelo_anova_2)
modelo_anova_2$coefficients

df_pf %>%
  group_by(Escolaridade_Grupo) %>%
  summarise(
    media_total = mean(Total),
    desvio_padrao_total = sd(Total),
    minimo_total = min(Total),
    maximo_total = max(Total)
  )

df_pf %>%
  group_by(Idade_Grupo) %>%
  summarise(
    media_total = mean(Total),
    desvio_padrao_total = sd(Total),
    minimo_total = min(Total),
    maximo_total = max(Total)
  )

df_pf %>%
  group_by(Gênero) %>%
  summarise(
    media_total = mean(Total),
    desvio_padrao_total = sd(Total),
    minimo_total = min(Total),
    maximo_total = max(Total)
  )

# Teste Posthoc
TukeyHSD(modelo_anova_2)

df_pf %>% select(8,11) %>% filter(Escolaridade_Grupo == "Ensino Médio") %>% summary()
df_pf %>% select(8,11) %>% filter(Escolaridade_Grupo == "Ensino Básico") %>% summary()
df_pf %>% select(8,11) %>% filter(Escolaridade_Grupo == "Ensino Superior") %>% summary()

modelo_anova_hocbonf_2 <- glht(modelo_anova_2)
summary(modelo_anova_hocbonf_2)

modelo_anova_3 <- aov(Total ~ Idade_Grupo, data = df_pf)
summary(modelo_anova_3)
modelo_anova_3$coefficients
TukeyHSD(modelo_anova_3) %>% plot()



modelo_anova_hocbonf_3 <- glht(modelo_anova_3)
summary(modelo_anova_hocbonf_3)

modelo_anova_4 <- aov(Total ~ Escolaridade_Grupo, data = df_pf)
summary(modelo_anova_3)
TukeyHSD(modelo_anova_4)

modelo_anova_hocbonf_4 <- glht(modelo_anova_4)
summary(modelo_anova_hocbonf_4)


# Aula 21/03/2023 ---------------------------------------------------------

# Análises de correlação
# As variáveis precisam ser contínuas ou discretas longas

glimpse(df_pf)

# Hipótese nula é que os dados tem distribuição normal >0.05
shapiro.test(df_pf$Idade)
shapiro.test(df_pf$Total)

boxplot(df_pf$Idade)
boxplot(df_pf$Total)

plot(df_pf$Idade, df_pf$Total)

cor.test(df_pf$Idade, df_pf$Total, method = "pearson")

cor.test(df_pf$Idade, df_pf$Total, method = "pearson")

ggplot(df_pf, aes(x = Idade, y = Total))+
  geom_point(size = 1.2)+
  labs(y = "Resultado do Teste")+
  theme_linedraw()

ggplot(df_pf, aes(x = Idade, y = Total))+
  geom_point(size=1.5)+
  labs(y = "Resultado do Teste")+
  theme_classic()

matriz <- df_pf %>% select(2,8) %>% cor(method = "pearson") %>% round(2)
view(matriz)


corrplot::corrplot(matriz, method = "number")

corrplot::corrplot(matriz, method = "circle",
                   type = "upper", order = "hclust",)

# Banco construção

ggcorrplot(matriz,
           method = "circle")

banco_ufpe %>% view()

names(banco_ufpe)

# pressupostos
# testes parametricos - /person
# testes não parametricos - spearman/kendal
# correlacao tetracorica/ policorica

matriz_cor <- banco_ufpe %>% select(87:95) %>% cor(method = "pearson") %>% round(2)
matriz_cor %>% view()

corrplot::corrplot(matriz_cor, method = "number", p.mat = Sig$p, sig.level = 0.05, type = "upper",
                   number.cex = 1.2, cl.cex = 1.5, tl.col = "blue")

Sig <- corrplot::cor.mtest(matriz_cor, conf.level = 0.95)




matriz_cor <- df_geral %>% select(86:94) %>% cor(method = "pearson") %>% round(2)

testRes <-  corrplot::cor.mtest(matriz_cor, conf.level = 0.95) 
corrplot::corrplot(matriz_cor, method = "number", p.mat = testRes$p, sig.level = 0.05, type = "upper",
                   number.cex = 0.8)


# Regressão linear --------------------------------------------------------

# Verificação dos pressupostos
plot(banco_ufpe$Idade, banco_ufpe$F_IM1)

# Observação sobre variáveis dependentes e independentes 

# Regressão linear simples
mod_1 <- lm(Idade ~ F_IM1, data = banco_ufpe)
summary(mod_1)

# Análise gráfica
par(mfrow=c(2,2))
plot(mod_1)


# 1 Gráfico permite visualizar a linearidade e homocedasticidade
# A linha vermelha deve ficar próxima da linha cinza

# 2 gráfico para visualizar a normalidade
# os pontos devem ficar em cima da linha

# 3 gráfico para visualizar a homocedasticidade
# a linha vermelha deve ficar mais horizontal

# 4 Gráfico para visualizar outliers
# dados que estejam acima ou abaixo de -3 +3 

# Normalidade dos resíduos
# p menor que 0.05 a distribuição não é normal
shapiro.test(mod_1$residuals)

# Análise do modelo
summary(mod_1)

# Intercepto é o valor quando a VI é igual a zero. No modelo, ao subir um ponto de impulsividade,
# a idade cai em aproximadamente 3 pontos, ou 3 anos.

# R quadrado ajustado
# Impulsividade está explicando a variação em idade em aproximadamente 8%
# Estatística F, compara o modelo com previsor e um modelo hipotetico sem previsor.

# Gráfico de dispersão
ggplot(data = banco_ufpe, mapping = aes(x = Idade, y = F_IM1))+
  geom_point()+
  geom_smooth(method = "lm", col = "red")+
  theme_classic()


# Regressão linear múltipla
mod_2 <- lm(Idade ~ F_IM1 + F_IM2, data = banco_ufpe)
summary(mod_2)

# Ausência de multicolinearidade 
banco_ufpe %>% select(93:95) %>% pairs.panels()

vif(mod_2)

# quando a correlação entre as variáveis estão acima de 0.8


