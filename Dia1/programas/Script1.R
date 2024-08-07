library(tidyverse)
library(readxl)

# R básico ----

# tipos: numeric, character, factor, logical ----
nFolhas <- c(15, 20, 25, 17, 26, 19)
cidade <- "Piracicaba"
nomes <- c("Alice", "Bob", "Charlie", "David")

textoComprido <- "Um texto muito comprido, que pode ser quebrado em várias linhas"

tratamentos <- factor(rep(paste("Trat", seq(1,3)), each=2))
blocos <- factor(rep(c(1,2), times=3))

# estruturas: vetor, matriz, data.frame, lista, tabela ----

altura <- c(1.65, 1.67, 1.70, 1.58, 1.69, 1.73, 1.60, 1.70)

compRamos <- c(6.0, 7.5, 9.2, 7.2, 8.9, 7.8)

M <- matrix(c(1,4,7, 2,5,8, 3,6,9))

dados <- data.frame(blocos, tratamentos, nFolhas, compRamos)

lista <- list(c(1,2,3), M, dados)

# Exercício -----

# família apply -----
# apply, lapply, sapply

folhasERamos <- cbind(nFolhas, compRamos)

# leitura de arquivos externos ----

read_xls("dados/diario2023.xls")
read_lines("dados/DCE2023.TXT")
read_lines("http://www.leb.esalq.usp.br/leb/exceldados/DCE2023.TXT")

# Exercícios ----

tamanho <- c("Pequeno", "Médio", "Grande")
id <- c(1, 2, 3, 1, 2, 1, 2, 3)

# dados.ex1 <-

frase <- "eu vou comer laranjas e bananas"

# vetor.ex2 <- 

nome <- c("Cecília", "Pedro", "Helena", "Júlia",
          "Lorenzo", "Benício", "Lívia", "Theo")

# vetor.ex3 <-

estadoCivil <- c("Casado", "Solteiro", "Viúvo",
                 "Solteiro", "Casado")

# vetor.ex4 <- 

Cor <- c("Claro", "Escuro")
Tamanho <- c("Pequeno", "Médio", "Grande")
Rep <- seq(1,5)
Medida <- c(1.8, 15.4, 5.2, 2.2, 12.4, 20.0, 5.4, NA, 5.8, 5.4,
            NA, 7.0, 16.33, 27.33, 26.67, 12, 2, NA, 16.33, 11.17,
            12.5, 16, 17.75, 14.25, NA, 37, 49.5, NA, 47.5, 26.5)

# dados.ex5 <- 

col1 <- c(12,13,11,10,12,13,11)
col2 <- c(10,11,12,13,11,13,11)
vetor.num <- seq(12,15)
vetor.chr <- c("g.102", "g.104", "d.202", "d.104")

# lista.ex6 <-

posicoes <- c(1,5,9,16,21)
consoantes <- letters[-posicoes]
vogais <- letters[posicoes]

# lista.ex7 <-

posicao <- c(seq(1,5),seq(1,4),seq(1,5),seq(1,3))
grupo <- rep(seq(1,4),c(5,4,5,3))

# dados.ex8 <-

fator <- factor(c(rep(seq(1,3),each=3)), levels=c(2, 1, 3))

# fator <- 

nomes <- c("Larissa Cunha Azevedo", "Marisa Alves Cardoso",
           "Enzo Martins Costa", "Luis Cunha Rodrigues",
           "Antônio Castro Carvalho")

# nomes <-
