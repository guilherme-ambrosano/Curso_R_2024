library(tidyverse)
library(broom)

# Tidyverse ----

# broom

modelo.aov <- aov(Sepal.Length ~ Species, data=iris)
modelo.lm  <- lm(Sepal.Width ~ Petal.Width, data=iris)
modelo.wt  <- wilcox.test(iris$Petal.Length, g=iris$Species)

# purrr

excel_sheets("../dados/Relação nominal dos alunos - Bolsa Extensão - 2023.xlsx")
read_xlsx("../dados/Relação nominal dos alunos - Bolsa Extensão - 2023.xlsx")

# ggplot2

linhas <- read_lines(
  "http://www.leb.esalq.usp.br/leb/exceldados/DCE2023.TXT")

inicio <- which(str_starts(linhas, "="))[c(1, seq(3, 37, 3))]
final <- which(str_starts(linhas, "="))[c(2, seq(5,37,3), 37)]
pular <- sapply(seq(1,13), function(i){
  seq(inicio[i], final[i])
})
pular2 <- which(linhas=="")

linhas2 <- linhas[-c(unlist(pular),pular2)]

dados_metereologicos <- as_tibble(linhas2) %>%
  separate(value,
           c("No", "ANO", "DIA", "MES", "R.GLOBA",
             "INSOLACAO", "PRECIPITACAO", "UMIDADE RELATIV",
             "VENTO MAXIMO", "VENTO MEDIO", "TEMPER MAXIMA",
             "TEMPER MINIMA", "TEMPER MEDIA", "EVAPORACAO"),
           sep=" +") %>%
  unite(Data, DIA, MES, ANO) %>%
  mutate(Data=lubridate::dmy(Data)) %>%
  mutate_at(vars(-Data), str_replace, ",", ".") %>%
  mutate_at(vars(-Data), parse_number) %>%
  filter(!is.na(Data))

# Exemplos ----

invasoras <- read_xlsx("../dados/listagem-de-plantas-alternativas-as-plantas-exoticas-invasoras-listadas-para-o-estado-do-rio-de.xlsx",
                       col_names=c("Codigo", "Familia_EEI", "NomeCient_EEI",
                                   "NomePop_EEI",
                                   "NomeCient_Ombro", "NouE_Ombro",
                                   "NomeCient_Semidec", "NouE_Semidec",
                                   "NomeCient_InfMar", "NouE_InfMar",
                                   "Ecossist_EEI", "Categoria"),
                       range="A8:L268")

ecossist <- invasoras %>%
  mutate_at(vars(Ecossist_EEI), replace_na, "?") %>% 
  group_by(NomeCient_EEI, Ecossist_EEI) %>%
  count() %>%
  group_by(NomeCient_EEI) %>%
  arrange(Ecossist_EEI) %>% 
  summarise(Ecossist_EEI = (function(x) {
    if(length(x)==1) return(x)
    else {
      if(is.na(x[1])) return(x[2])
      if(is.na(x[2])) return(x[1])
      if(grepl(x[1], x[2])) return(x[2])
      if(grepl(x[2], x[1])) return(x[1])
      return(paste(x, collapse=", "))
    }
  })(Ecossist_EEI),
  n = sum(n)) %>%
  mutate_at(vars(Ecossist_EEI), ~ifelse(
    .=="Formações Pioneiras de Influência Marinha, Floresta Ombrófila Densa",
    "Floresta Ombrófila Densa, Formações Pioneiras de Influência Marinha", .))

fomentoRural <- read_csv(
  paste0("https://aplicacoes.mds.gov.br/sagi/",
         "servicos/misocial/?",
         "fq=anomes_s:2023*&",
         "fq=formento_qtd_total_familias_benef_i:*&",
         "q=*:*&",
         "rows=1000000&",
         "wt=csv"))

fr <- fomentoRural %>% 
  select_if(~mean(is.na(.)|.==0)<.5) %>%
  select(mes_ano, matches("qtd\\_pes\\_(cad\\_nao\\_)?pbf\\_idade\\_.+\\_sexo.+\\_i")) %>%
  group_by(mes_ano) %>%
  summarise_all(sum, na.rm=T) %>% 
  pivot_longer(-mes_ano) %>%
  mutate(pbf = str_extract(name, "(nao\\_)?pbf"),
         idade=str_extract(name, "idade\\_(maior\\_que\\_)?\\d+(\\_[ea]\\_\\d+)?"),
         sexo =str_extract(name, "sexo\\_[^\\_]+ino")) %>%
  mutate(pbf = factor(pbf, levels=c("pbf", "nao_pbf"), labels=c("PBF", "Não-PBF")),
         idade=str_replace_all(str_remove(idade, "idade\\_"), "\\_", " "),
         sexo =str_to_title(str_remove(sexo, "sexo\\_"))) %>%
  mutate(idade1 = parse_number(str_extract(idade, "\\d+"))) %>% 
  mutate_at(vars(idade, sexo), factor) %>% 
  mutate(idade = fct_reorder(idade, idade1)) %>% 
  select(mes_ano, pbf, idade, sexo, name, value) %>%
  arrange(mes_ano, pbf, idade, sexo) %>%
  pivot_wider(id_cols=c(mes_ano, idade, sexo), names_from=pbf)

fr %>%
  ggplot(aes(x=mes_ano, col=idade, linetype=sexo, y=PBF)) +
  geom_line()

fr %>%
  ggplot(aes(x=mes_ano, col=idade, linetype=sexo, y=`Não-PBF`)) +
  geom_line()

# Mais conjuntos de dados -----

?datasets
?`datasetsICR-package`
?agridat
# https://dados.gov.br/dados/conjuntos-dados/

