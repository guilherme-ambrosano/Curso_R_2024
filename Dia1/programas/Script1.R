library(tidyverse)

library(stringr)
library(forcats)

library(readxl)

# R básico ----

# "objetos" e classes no R
nFolhas1 <- 15
class(nFolhas1)
cidade <- "Piracicaba"
class(cidade)

# tipos: numeric, character, factor, logical ----
nFolhas <- c(15, 20, 25, 17, 26, 19)
class(nFolhas)
nomes <- c("Alice", "Bob", "Charlie", "David")
class(nomes)
# é possível pegar o elemento em uma 
# posição específica de um vetor usando os colchetes
nomes[3]
nFolhas[3]

posicao <- 2
nomes[posicao]

# Comprimento de um vetor vs. comprimento de uma palavra
length(nomes)    # 4 nomes
length(nomes[1]) # 1 nome
nchar(nomes[1])  # 5 letras
nchar(cidade)    # 10 letras

# Manipulando vetores de palavras
# Funções para detectar um padrão em uma palavra
grep("e", nomes)       # R base
str_detect(nomes, "e") # tidyverse

grep("li", nomes, value = T) # R base
str_detect(nomes, "li")      # tidyverse

grep("ci", cidade)       # R base
str_detect(cidade, "ci") # tidyverse

# Funções para selecionar um trecho de uma palavra
substr(cidade, 1, 4)  # R base
str_sub(cidade, 1, 4) # tidyverse

# Funções para substituir um trecho de uma palavra
gsub("cicaba", "ssununga", cidade)        # R base
str_replace(cidade, "cicaba", "ssununga") # tidyverse

# Funções para extrair um trecho de uma palavra
str_extract(tratamentos, "\\d")       # tidyverse
str_extract_all(cidade, "[Prcb][ia]") # tidyverse

# Função paste - concatenar palavras
paste("Oi, sou", nomes)

textoComprido <- "Um texto muito comprido, que pode ser quebrado em várias linhas"

cat(paste(
  strwrap(textoComprido, width = 10),
  collapse="\n"))                       # R base
cat(str_wrap(textoComprido, width=10))  # tidyverse

# Fatores 

tratamentos <- factor(rep(paste("Trat", seq(1,3)), each=2))
blocos <- factor(rep(c(1,2), times=3))

levels(tratamentos)
levels(blocos)

fct_relevel(tratamentos, "Trat 2")
# Funções do stringr sempre retornam vetores do tipo character
str_replace(tratamentos, "Trat", "Tratamento")
# Funções do forcats sempre retornam vetores do tipo factor
fct_relabel(tratamentos, str_replace, "Trat", "Tratamento")


# Obs: é possível passar uma função como argumento para outra função
# Note que no exemplo acima a função `str_replace` foi passada
# como segundo argumento da função `fct_relabel`.
# Nesses casos, é passado o nome da função sem parênteses
# O terceiro ("Trat") e quarto ("Tratamento") argumentos
# foram passados como segundo e terceiro argumentos para a
# função `str_replace`. O primeiro argumento é o objeto `tratamentos`


# Reordenando os níveis de um fator com base em outra variável
fct_reorder(fct_relevel(tratamentos, "Trat 3"), nFolhas)
aggregate(nFolhas, list(trat=tratamentos), mean)

# Atenção para a ordem dos níveis ao manipular os níveis de um fator
tratamentos <- relevel(tratamentos, 2)
tratamentos # 2, 1, 3
##levels(tratamentos) <- paste("Tratamento", c(1, 2, 3))
##tratamentos
levels(tratamentos) <- paste("Tratamento", c(2, 1, 3))
tratamentos


# Estruturas: vetor, matriz, data.frame, lista, tabela ----

# Criando vetores
# Funções `rep` e `seq`
paste("Tratamento", seq(1, 10))
seq(0,1,length.out=5)
rep(seq(1,2), times=5, each=3)

# Transformando um vetor numérico em fator
# Função `cut`
altura <- c(1.65, 1.67, 1.70, 1.58, 1.69, 1.73, 1.60, 1.70)
altura
cut(altura, c(-Inf, quantile(altura)))

# Operações com vetores
compRamos <- c(6.0, 7.5, 9.2, 7.2, 8.9, 7.8)

nFolhas/compRamos

for(i in seq(1,6)) {
  print(nFolhas[i]/compRamos[i])
}

# Matrizes
M <- matrix(c(1,4,7, 2,5,8, 3,6,9), nrow=3)

# Diferença ao selecionar um elemento de um vetor e de uma matriz
posicao <- 1
posicao_linha <- 2
posicao_coluna <- 3

altura[posicao]                 # vetor
M[posicao_linha,]               # matriz
M[,posicao_coluna]              # matriz
M[posicao_linha,posicao_coluna] # matriz

# data.frames
dados <- data.frame(blocos, tratamentos, nFolhas, compRamos)
names(dados) <- c("Blocos", "Trat", "Folhas", "comprimento")

# Diferença entre matriz e data-frame
M[1,2]
dados[2,"Trat"] # nome das colunas
dados$Trat[2]   # nome das colunas
# selecionando linhas com base nos valores de uma coluna
dados$Trat=="Tratamento 2"
dados[dados$Trat=="Tratamento 2",]

mean(dados[dados$Trat=="Tratamento 1","Folhas"])
mean(dados[dados$Trat=="Tratamento 2","Folhas"])
mean(dados[dados$Trat=="Tratamento 3","Folhas"])

aggregate(dados$Folhas, list(trat=dados$Trat), mean)

# Criando um data.frame com o expand.grid
expand.grid(c(1,2,3),
            c(1,2))
data.frame(Var1=rep(seq(1,3), 2),
           Var2=rep(seq(1,2), each=3))

# listas
lista <- list(c(1,2,3), M, dados)

# Exercício -----

ratings <- attitude$rating
mediana <- median(ratings)
factor(ratings < mediana, 
       levels=c(F, T),
       labels=c("Menor", "Maior"))

fator <- cut(ratings, c(-Inf, mediana, Inf))
levels(fator) <- c("Menor", "Maior")

fator

cut(attitude$rating, c(-Inf, median(attitude$rating), Inf))

factor(ifelse(ratings < mediana, "Menor", "Maior"),
       levels=c("Menor", "Maior"))

factor(ifelse(ratings < mediana, 1,2))

critical_dicotomizado <- cut(attitude$critical,
                             c(-Inf, median(attitude$critical), Inf))

levels(critical_dicotomizado)
mean(attitude$learning[critical_dicotomizado=="(-Inf,77.5]"])
mean(attitude$learning[critical_dicotomizado=="(77.5, Inf]"])

aggregate(attitude$learning,
          list(critical=critical_dicotomizado),
          mean)

critical <- attitude$critical
mediana_crit <- median(critical)
factor(critical < mediana_crit, 
       levels=c(F, T),
       labels=c("Menor", "Maior"))

attitude$learning[
  factor(critical < mediana_crit, 
         levels=c(F, T),
         labels=c("Menor", "Maior"))=="Maior"]

dicotomizar <- function(x) cut(x, c(-Inf, median(x), Inf))
dicotomizar(attitude$rating)
dicotomizar(attitude$critical)

# família apply -----
# apply, lapply, sapply

folhasERamos <- cbind(nFolhas, compRamos)

apply(folhasERamos, 1, function(x) x[1]/x[2])
apply(folhasERamos, 2, mean)
apply(as.matrix(attitude), 2, dicotomizar)
sapply(attitude, dicotomizar)

sapply(dados, length)
lapply(dados, length)

# leitura de arquivos externos ----

read_xls("dados/diario2023.xls", skip = 10)
read_lines("dados/DCE2023.TXT")
read_lines("http://www.leb.esalq.usp.br/leb/exceldados/DCE2023.TXT")

# Exercícios ----

# Exercicio 1
tamanho <- c("Pequeno", "Médio", "Grande")
id <- c(1, 2, 3, 1, 2, 1, 2, 3)

tamanho <- str_sub(tamanho, 1, 3)
rep(tamanho, each=3)[-6]
c(rep(tamanho[1], 3), 
  rep(tamanho[2], 2),
  rep(tamanho[3], 3))

dados.ex1 <- data.frame(id=id,
                        tamanho=rep(tamanho, each=3)[-6])
str(dados.ex1)

# Exercicio 2

frase <- "eu vou comer laranjas e bananas"

vetor.ex2 <- sapply(c("a", "e", "i", "o", "u"),
                    str_replace_all, string=frase, pattern="[aeiou]")

# Exercicio 3

nome <- c("Cecília", "Pedro", "Helena", "Júlia",
          "Lorenzo", "Benício", "Lívia", "Theo")

vetor.ex3 <- sapply(nome, function(x) ifelse(substr(x, nchar(x), nchar(x))=="o", "Menino", "Menina"))
#paste0("Menin", str_extract(nome, "[ao]$"))
#str_replace(nome, ".+(?=[ao]$)", "Menin")

# Exercicio 4

estadoCivil <- c("Casado", "Solteiro", "Viúvo",
                 "Solteiro", "Casado")

vetor.ex4 <- factor(estadoCivil, levels=c("Solteiro", "Casado", "Viúvo"))

# Exercicio 5

Cor <- c("Claro", "Escuro")
Tamanho <- c("Pequeno", "Médio", "Grande")
Rep <- seq(1,5)
Medida <- c(1.8, 15.4, 5.2, 2.2, 12.4, 20.0, 5.4, NA, 5.8, 5.4,
            NA, 7.0, 16.33, 27.33, 26.67, 12, 2, NA, 16.33, 11.17,
            12.5, 16, 17.75, 14.25, NA, 37, 49.5, NA, 47.5, 26.5)

dados.ex5 <- cbind(expand.grid(Rep=Rep, Cor=factor(Cor),
                               Tamanho=factor(Tamanho[seq(3,1)])),
                   Medida=Medida) 

# Exercicio 6

col1 <- c(12,13,11,10,12,13,11)
col2 <- c(10,11,12,13,11,13,11)
vetor.num <- seq(12,15)
vetor.chr <- c("g.102", "g.104", "d.202", "d.104")

matriz <- matrix(c(col1, col2),nrow=7)
lista.ex6 <- list(Matriz=matriz,
                  Vetor=vetor.num,
                  DataFrame=data.frame(vetor.num,vetor.chr))

# Exercicio 7

posicoes <- c(1,5,9,16,21)
consoantes <- letters[-posicoes]
vogais <- letters[posicoes]

lista.ex7 <- list(vogais=sapply(vogais, paste, "é uma vogal"),
                  consoantes=sapply(consoantes, paste, "é uma consoante"))
lista.ex7

# Exercicio 8

posicao <- c(seq(1,5),seq(1,4),seq(1,5),seq(1,3))
grupo <- rep(seq(1,4),c(5,4,5,3))

dados.ex8 <- data.frame(posicao=posicao, grupo=grupo)

dados.ex8$primeiros <- dados.ex8$posicao==1
dados.ex8$ultimos <- dados.ex8$posicao > c(dados.ex8$posicao[2:length(posicao)], 1)

dados.ex8$posicao.fct <- ifelse(dados.ex8$primeiros, "Primeiro",
                                ifelse(dados.ex8$ultimos, "Último",
                                       "Meio"))

dados.ex8$posicao.fct <- factor(dados.ex8$posicao.fct, levels=c("Primeiro", "Meio", "Último"))
dados.ex8

# Exercicio 9

fator <- factor(c(rep(seq(1,3),each=3)), levels=c(2, 1, 3))
levels(fator) <- c("Nível 2", "Nível 1", "Nível 3")
fator <- relevel(fator, "Nível 1")
fator

# Exercicio 10

nomes <- c("Larissa Cunha Azevedo", "Marisa Alves Cardoso",
           "Enzo Martins Costa", "Luis Cunha Rodrigues",
           "Antônio Castro Carvalho")
gsub("(\\w+)( (\\w)\\w+)? (\\w+)", "\\4, \\1 \\3.", nomes)
