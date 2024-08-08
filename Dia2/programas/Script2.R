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
vetor2 <- c(um=1, dois=2, 3, quatro=4)
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

rownames_to_column(dados, "rownames")

# enframe e deframe
vetor3 <- sapply(c("p", "g", "m", "sap"), paste0, "ato")
enframe(vetor3)

enframe(vetor2)
deframe(dados)

# dplyr - select, filter, mutate, group_by, summarise, case_when

# Select pode ser usado pra reordenar as colunas
iris %>% 
  select(Species, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
iris %>%
  relocate(Species, .before=Sepal.Width) # mas existe também a função relocate,
                                         # que é feita especificamente pra isso

# Exemplo de um conjunto de dados com mais de uma linha de cabeçalho
read_xls("dados/diario2023.xls")

# Lendo nomes e dados com chamadas separadas
nomes <- names(read_xls("dados/diario2023.xls",
                        skip=7, n_max = 0))
diario2023 <- read_xls("dados/diario2023.xls", skip=10,
                       col_names=nomes) %>%
  filter_all(any_vars(!is.na(.))) # Exemplo do filter_all

# Exemplo da função case_when
tamanhos <- tibble(tam = c(1.5, 1.9, 1.6, 1.8, 2.0, 1.7))

tamanhos %>%
  mutate(classe = case_when(.<quantile(tam,.25) ~ "Muito pequeno",
                            .<median(tam) ~ "Peq",
                            T ~ "Grande")) # código mais enxuto que o ifelse

# Conjunto de dados para demonstrar as funções group_by e summarise
medidas <- read_xlsx("dados/Planilha.xlsx") %>%
  mutate(Tamanho = factor(Tamanho,
                          levels=c("Pequeno", "Médio",
                                   "Grande")),
         Cor = factor(Cor, levels=c("Claro", "Escuro")),
         Repetição = factor(Repetição))

mtcars %>%
  group_by(cyl) %>%
  mutate(n = row_number()) %>%
  select(cyl, n)

mtcars %>%
  group_by(cyl) %>%
  summarise(mean(hp))

# Dados para exemplificar as funções left_join, right_join, inner_join e full_join
usuarios <- read_xlsx("dados/Transacoes.xlsx", "usuarios")
transacoes <- read_xlsx("dados/Transacoes.xlsx", "transacoes")

# Exercícios - dplyr ----

agridat::walsh.cottonprice %>%
  select(year, cotton, cottonseed, combined) %>%
  mutate(cottonseed = cottonseed/2000*100,
         combined2 = cotton+1.857*cottonseed) %>%
  # Tem vários jeitos de fazer essa variável "década"
  mutate(década = 10*(year %/% 10)) %>%
  #mutate(década = str_sub(year, 1, 3)) %>%
  #mutate(década = str_replace(year, "\\d$", "0")) %>%
  #mutate(década = str_extract(year, "\\d{3}")) %>%
  #mutate(década = cut(year, seq(1909, 1949, 10))) %>%
  group_by(década) %>%
  summarise(média = mean(combined2),
            mediana=median(combined2))

(aparece <- dplyr::starwars %>%
  select(name, films) %>%
  group_by(name) %>% 
  mutate(n = length(films[[1]])) %>%
  select(name, n) %>%
  deframe())

tibble(name=rep(names(aparece), aparece))

tibble(name=rep(names(aparece), aparece),
       id=unlist(lapply(aparece, \(x) seq(1, x))),
       films=unlist(starwars$films))

# a mesma coisa pode ser feita com a função unnest do tidyr
starwars %>%
  select(name, films) %>%
  unnest_longer(films)
starwars %>%
  select(name, films) %>%
  unnest_longer(films) %>% # acrescentando a coluna `id`
  group_by(name) %>% 
  mutate(id=row_number()) %>%
  ungroup() %>%
  relocate(id, .after=name)

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


tibble(id=paste0("Trat", rep(seq(1,4),each=2),
                 "Bloco", rep(seq(1,2), 4))) %>%
  mutate(id = str_replace(id, "([Bb])", "_\\1")) %>%
  separate(id, c("Trat", "Bloco"), sep="_")

lapply(c("Antigua", "StVincent"),
       read_xlsx, path="dados/CaribbeanMaize.xlsx") %>%
  lapply(pivot_longer, c(-block, -plot), names_to="site", values_to="yield") %>% 
  tibble(isle=c("Antigua", "StVincent"),
         area=c(280, 345),
         dados=.) %>%
  unnest(dados)

c("Antigua", "StVincent") %>%
  lapply(read_xlsx, path="dados/CaribbeanMaize.xlsx") %>%
  lapply(pivot_longer, cols=c(-block, -plot)) %>%
  bind_rows()

medidas %>%
  mutate(Medida = replace_na(Medida, 0)) %>% 
  group_by(Tamanho) %>%
  summarise_at(vars(Medida), mean)

# Exemplos ----

linhas <- read_lines("http://www.leb.esalq.usp.br/leb/exceldados/DCE2023.TXT")

inicio <- which(str_starts(linhas, "="))[c(1, seq(3, 37, 3))]
final <- which(str_starts(linhas, "="))[c(2, seq(5,37,3), 37)]
pular <- sapply(seq(1,13), function(i){
  seq(inicio[i], final[i])
})
pular2 <- which(linhas=="")

linhas2 <- linhas[-c(unlist(pular),pular2)]


sapply(linhas, nchar)
tibble(linhas=linhas) %>%
  mutate(n = nchar(linhas)) %>% 
  filter(n > 98)

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

# Exercicios

# Exercicio 1

pValores <- tibble(
  contraste = c("Trat 1 - Trat 2", "Trat 1 - Trat 3",
                "Trat 2 - Trat 3"),
  `p-valor` = c(0.9999, 0.050, 0.0001)
)

pValores %>%
  mutate(contraste = str_replace(contraste, " - ", "-")) %>%
  deframe()

# Exercício 2

dados %>%
  rownames_to_column("id") %>%
  separate(id, c("Trat", "Rep")) %>%
  group_by(Trat) %>%
  summarise_at(vars(val), list(média=mean,
                               desvio=sd)) %>%
  mutate_at(vars(Trat), str_sub, 5, 5)

# Exercício 3

dados_meteorologicos <- as_tibble(linhas2) %>%
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

dados_meteorologicos %>%
  arrange(desc(`TEMPER MEDIA`)) %>%
  slice(1:5) %>%
  select(Data, `TEMPER MEDIA`) %>%
  deframe()

# Exercício 4

dados_meteorologicos %>%
  filter(PRECIPITACAO>30 & PRECIPITACAO<40) %>% 
  select(Data, PRECIPITACAO) %>%
  deframe()

# Exercicio 5

tempo1 <- c(15, 14, 17, 14, 17, 13)
tempo2 <- c(14, 13, 16, 14, 12, 16)
tempo3 <- c(16, 14, 15, 12, 17, 15)

tibble(t1=tempo1, t2=tempo2, t3=tempo3) %>%
  pivot_longer(everything()) %>%
  mutate(Tempo=name %>% 
           str_remove("t") %>%
           paste("semana") %>%
           factor()) %>%
  group_by(Tempo) %>%
  summarise(Média=mean(value))

# Exercício 6

mtcars %>%
  rownames_to_column("Carro") %>%
  filter(str_starts(Carro, "M"))

# Exercício 7

mtcars %>%
  rownames_to_column("Carro") %>%
  filter(str_ends(Carro, " \\d+"))

# Exercício 8

# Archbold, Brown, Cornelius (1987) - Table 4
# https://journals.ashs.org/jashs/view/journals/jashs/112/2/article-p219.xml
list(
  agridat::archbold.apple %>%
    mutate(spacing = spacing*.3) %>% 
    group_by(spacing, gen),
  agridat::archbold.apple %>%
    group_by(stock, gen)) %>%
  lapply(. %>% 
           summarise(value=mean(yield, na.rm=T)) %>%
           ungroup() %>% 
           pivot_wider(names_from=gen))

# Exercício 9

iris %>%
  filter(Species != "virginica") %>%
  select(Species, starts_with("Petal")) %>%
  mutate(Species = paste("Iris", Species)) %>% 
  group_by(Species) %>% 
  summarise_all(list(média=mean, desvio=sd)) %>%
  pivot_longer(-Species)
# ou
iris %>%
  filter(Species != "virginica") %>%
  select(Species, starts_with("Petal")) %>%
  mutate(Species = paste("Iris", Species)) %>%
  pivot_longer(-Species) %>% 
  group_by(Species, name) %>% 
  summarise(média = mean(value),
            desvio=sd(value)) %>%
  pivot_longer(c(-Species, -name), names_to="estat") %>%
  unite(name, name, estat)

# Exercício 10

iris %>%
  filter(Species != "virginica") %>%
  pivot_longer(-Species) %>%
  group_by(Species, name) %>%
  summarise(Mediana = median(value)) %>%
  ungroup() %>% 
  mutate(name = paste0("Mediana (", name, ")")) %>%
  mutate(Species = fct_drop(Species)) %>% 
  split(.$Species) %>%
  lapply(. %>%
           select(-Species) %>%
           deframe())
