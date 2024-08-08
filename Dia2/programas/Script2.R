library(tidyverse)
library(lubridate)
library(agridat)
library(readxl)

# Dia 1 (revisão) ----

# Tipos: numeric, character, factor, logical, date, ...
# Estruturas:
#    Vetor: seleciona elementos com colchetes simples []
vetor <- c(1,2,3,4)
vetor
vetor[3]
#    Cada um dos elementos de um vetor pode ou não ser nomeado
c(um=1, dois=2, 3, quatro=4)
#    Matriz: seleciona elementos com colchetes com vírgula [,]
matriz <- matrix(c(1,2,3,4), nrow=2)
matriz
matriz[1,]
matriz[,2]
matriz[1,2]
#    As linhas e colunas de uma matriz podem ou não ser nomeadas
dimnames(matriz) <- list(linhas=c("um", "dois"),
                         colunas=c("col_um", "col_dois"))
matriz
#    Data-frame: seleciona elementos com colchetes com vírgula [,] ou cifrão $
df <- data.frame(bloco=rep(seq(1,4),each=2),
                 trat=rep(seq(1,2), 4))
df
df[,2]
df[,"trat"]
df$trat
#    As colunas e linhas de um data-frame precisam ser nomeadas
data.frame(rep(seq(1,4),each=2),
           rep(seq(1,2), 4))
#    Lista: seleciona elementos com colchetes duplos [[]] ou cifrão $
#    Os elementos podem ou não ser nomeados
lista <- list(vetor=vetor, M=matriz, df)
lista
lista[[2]]
lista$M
# Obs: uma lista de objetos do mesmo tipo pode ser transformada em vetor
lista2 <- list(vetor1=c(1,2,3), vetor2=c(4,5,6), vetor3=c(7,8,9))
lista2
unlist(lista2)

# Dia 2 ----

# Tidyverse ----

# tibble - rownames_to_column, column_to_rownames
load("dados/Dados.RData")

# enframe e deframe
sapply(c("p", "g", "m", "sap"), paste0, "ato")

# dplyr - select, filter, mutate, group_by, summarise, case_when
nomes <- names(read_xls("dados/diario2023.xls",
                        skip=7, n_max = 0))
diario2023 <- read_xls("dados/diario2023.xls", skip=10,
                       col_names=nomes) %>%
  filter_all(any_vars(!is.na(.)))

tamanhos <- tibble(tam = c(1.5, 1.9, 1.6, 1.8, 2.0, 1.7))

medidas <- read_xlsx("dados/Planilha.xlsx") %>%
  mutate(Tamanho = factor(Tamanho,
                          levels=c("Pequeno", "Médio",
                                   "Grande")),
         Cor = factor(Cor, levels=c("Claro", "Escuro")),
         Repetição = factor(Repetição))

usuarios <- read_xlsx("dados/Transacoes.xlsx", "usuarios")
transacoes <- read_xlsx("dados/Transacoes.xlsx", "transacoes")

# Exercícios - dplyr ----

agridat::walsh.cottonprice

dplyr::starwars

# tidyr - pivot_wider, pivot_longer,
#         unnest_longer, unnest_wider, separate, unite
#         replace_na

avaliacoes <-
    tibble("Avaliação 1" = c(1.50, 1.55, 1.54),
           "Avaliação 2" = c(1.51, 1.56, 1.54))

estados <-
  tibble(Região = c(rep("Sul", 3), rep("Centro-Oeste", 3)),
         ID = c(1,2,3, 1,2,3),
         Estado = c("RS", "PR", "SC", "MS", "MT", "GO"))

lapply(c("Antigua", "StVincent"),
       read_xlsx, path="dados/CaribbeanMaize.xlsx") %>%
  lapply(pivot_longer, c(-block, -plot), names_to="site", values_to="yield") %>% 
         tibble(isle=c("Antigua", "StVincent"),
                area=c(280, 345),
                dados=.) %>%
           unnest(dados)
         
# Exemplos ----

linhas <- read_lines("http://www.leb.esalq.usp.br/leb/exceldados/DCE2023.TXT")

inicio <- which(str_starts(linhas, "="))[c(1, seq(3, 37, 3))]
final <- which(str_starts(linhas, "="))[c(2, seq(5,37,3), 37)]
pular <- sapply(seq(1,13), function(i){
  seq(inicio[i], final[i])
})
pular2 <- which(linhas=="")

linhas2 <- linhas[-c(unlist(pular),pular2)]

as_tibble(linhas2) %>%
  separate(value,
           c("No", "ANO", "DIA", "MES", "R.GLOBA",
             "INSOLACAO", "PRECIPITACAO", "UMIDADE RELATIV",
             "VENTO MAXIMO", "VENTO MEDIO", "TEMPER MAXIMA",
             "TEMPER MINIMA", "TEMPER MEDIA", "EVAPORACAO"),
           sep=" +") %>%
  unite(Data, DIA, MES, ANO) %>%
  mutate(Data=lubridate::dmy(Data)) %>%
  mutate_at(vars(-Data), str_replace, ",", ".") %>%
  mutate_at(vars(-Data), parse_number)


# Archbold, Brown, Cornelius (1987) - Table 4
# https://journals.ashs.org/jashs/view/journals/jashs/112/2/article-p219.xml
archbold.apple


# Exercicios

# Exercicio 1

pValores <- tibble(
  contraste = c("Trat 1 - Trat 2", "Trat 1 - Trat 3",
                "Trat 2 - Trat 3"),
  `p-valor` = c(0.9999, 0.050, 0.0001)
)

# vetor.ex1 <- 

# Exercicio 5

tempo1 <- c(15, 14, 17, 14, 17, 13)
tempo2 <- c(14, 13, 16, 14, 12, 16)
tempo3 <- c(16, 14, 15, 12, 17, 15)


