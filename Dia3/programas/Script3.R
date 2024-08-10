library(tidyverse)
library(broom)

# Tidyverse ----

# broom

modelo.aov <- aov(Sepal.Length ~ Species, data=iris)
modelo.lm  <- lm(Sepal.Width ~ Petal.Width, data=iris)
modelo.wt  <- wilcox.test(iris$Petal.Length, g=iris$Species)

tidy(modelo.aov)
glance(modelo.aov)

# purrr

# Função split e group_split - transformam tibble em list
caribbean.maize %>%
  split(.$isle)

caribbean.maize %>%
  group_split(isle) %>%
  map(function(x) x %>%
        pivot_wider(id_cols=c(block, plot),
                    names_from=site,
                    values_from=yield))

# Exemplo das bolsas

excel_sheets("dados/Relação nominal dos alunos - Bolsa Extensão - 2023.xlsx")
arquivo_xlsx <- "dados/Relação nominal dos alunos - Bolsa Extensão - 2023.xlsx"

bolsas <- map_dfr(excel_sheets(arquivo_xlsx),
    function(x) read_xlsx(arquivo_xlsx, sheet=x) %>%
      mutate(`VALOR DA BOLSA` = as.character(`VALOR DA BOLSA`),
             Mes = x)) %>%
  mutate(`VALOR DA BOLSA` = str_replace(`VALOR DA BOLSA`, ",", ".")) %>% 
  mutate(`VALOR DA BOLSA` = parse_number(`VALOR DA BOLSA`)) %>%
  mutate(Mes = str_replace(Mes, "APR", "ABR")) %>% 
  mutate(Mes = parse_date_time2(Mes, orders = "%B-%Y"))

map_dfr(excel_sheets(arquivo_xlsx),
        \(planilha) {
          read_xlsx(arquivo_xlsx, planilha) %>%
            mutate_at(vars(`VALOR DA BOLSA`), as.character) %>% 
            mutate(Mes=planilha)
        }) %>%
  mutate_at(vars(`VALOR DA BOLSA`), parse_number, locale=locale(decimal=",")) %>% 
  mutate_at(vars(Mes), ~ifelse(.=="ABR-2023", "APR-2023", .)) %>%
  mutate_at(vars(Mes), parse_date_time2, "%B-%Y")

# Fazendo a mesma operação em várias variáveis de uma vez
iris %>%
  select(-Species) %>%
  as.list() %>% # as.list() transforma data.frame em list
  map_dfr(. %>%
            enframe() %>% 
            mutate(Species = iris$Species) %>%
            aov(value ~ Species, data=.) %>%
            residuals() %>%
            shapiro.test() %>% 
            tidy(), .id = "Variável") %>%
  select(Variável, p.value) %>%
  mutate(signif = case_when(p.value<.001 ~ "***",
                            p.value<.01 ~ "**",
                            p.value<.05 ~ "*",
                            T ~ "ns"))

iris %>%
  select(-Species) %>%
  cor()

cor.test(iris$Sepal.Length, iris$Sepal.Width)

expand_grid(Var1=names(iris[-5]),
            Var2=names(iris[-5])) %>%
  pmap_dfr(function(Var1, Var2) {
    tibble(Var1=Var1,
           Var2=Var2,
           p.value=cor.test(iris[,Var1],
                            iris[,Var2])$p.value)
  }) %>%
  pivot_wider(id_cols=Var1, names_from=Var2,
              values_from=p.value) %>%
  column_to_rownames("Var1") %>%
  as.matrix()

# ggplot2

iris %>%
  ggplot(aes(col=Species,x=Sepal.Width, y=Sepal.Length)) +
  geom_point() +
  guides(color = guide_legend(direction = "vertical")) +
  theme(legend.position = "bottom")

dados_meteorologicos <- read_lines(
  "http://www.leb.esalq.usp.br/leb/exceldados/DCE2023.TXT") %>%
  as_tibble() %>% 
  mutate(n = nchar(value)) %>%
  filter(n > 98) %>%
  select(-n) %>%
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

dados_meteorologicos %>%
  ggplot(aes(x=Data, y=`TEMPER MEDIA`)) +
  geom_line() +
  scale_x_date(date_breaks="6 months",
               date_labels = "%m-%Y")


dados_meteorologicos %>%
  select(Data, PRECIPITACAO, starts_with("TEMPER")) %>%
  pivot_longer(starts_with("TEMPER")) %>%
  ggplot(aes(x=Data, y=PRECIPITACAO)) +
  geom_bar(stat="unique") +
  geom_line(aes(col=name, y=value))


iris %>%
  pivot_longer(-Species) %>%
  ggplot(aes(x=Species, y=value)) +
  geom_boxplot() +
  facet_wrap(~name)

NBA.game %>%
  group_by(TEAM) %>%
  summarise_at(vars(W), list(média=mean,
                             erro=\(x)sd(x/sqrt(length(x))))) %>%
  mutate(TEAM = fct_reorder(TEAM, desc(média))) %>% 
  #arrange(média) %>%
  #slice(1:5) %>% 
  ggplot(aes(x=TEAM, y=média)) +
  geom_col() +
  geom_errorbar(aes(ymin=média-erro, ymax=média+erro))

# Exercício 1

attitude %>% 
  cor() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>% 
  mutate_at(vars(rowname, name), str_to_title) %>%
  mutate(rowname.num = as.numeric(factor(rowname)), 
         name.num = as.numeric(factor(name))) %>%
  filter(rowname.num > name.num) %>%
  mutate_at(vars(value), round, 4) %>% 
  mutate(cor = ifelse(abs(value)<.45, "white", "black")) %>% 
  ggplot(aes(x=rowname, y=name, fill=value)) +
  geom_tile(height=.9, width=.9) +
  geom_text(aes(label=value, color=cor), show.legend = F) +
  scale_color_manual(values=c("grey10", "grey90")) +
  theme_test() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5))

# Exemplos ----

invasoras <- read_xlsx("dados/listagem-de-plantas-alternativas-as-plantas-exoticas-invasoras-listadas-para-o-estado-do-rio-de.xlsx",
                       col_names=c("Codigo", "Familia_EEI", "NomeCient_EEI",
                                   "NomePop_EEI",
                                   "NomeCient_Ombro", "NouE_Ombro",
                                   "NomeCient_Semidec", "NouE_Semidec",
                                   "NomeCient_InfMar", "NouE_InfMar",
                                   "Ecossist_EEI", "Categoria"),
                       range="A8:L268")


invasoras %>%
  pivot_longer(c(ends_with("Ombro"),
                 ends_with("Semidec"),
                 ends_with("InfMar"))) %>% 
  separate(name, c("Variável", "Ecossistema")) %>% 
  pivot_wider(names_from="Variável",
              id_cols=c(Codigo, ends_with("EEI"), 
                        Categoria, Ecossistema)) %>%
  # Values from `value` are not uniquely identified; 
  #output will contain list-cols.
  unnest_longer(c(NomeCient, NouE)) %>% # o `unnest` desfaz isso
  filter(!is.na(NomeCient), !is.na(NouE))

# Algumas plantas da mesma espécie aparecem com ecossistemas diferentes
invasoras %>%
  group_by(NomeCient_EEI, Ecossist_EEI) %>%
  count() %>%
  group_by(NomeCient_EEI) %>%
  count()

ecossist <- invasoras %>%
  mutate_at(vars(Ecossist_EEI), replace_na, "?") %>% 
  group_by(NomeCient_EEI, Ecossist_EEI) %>%
  count() %>%
  group_by(NomeCient_EEI) %>%
  arrange(Ecossist_EEI) %>% # arrumando os ecossistemas
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

indicadores <- read_xlsx("dados/ranking_2015_2020.xlsx")
estados <- read_state(
  year = 2020, 
  showProgress = FALSE)

indicadores %>%
  filter(Pilar!="Ranking Geral",
         Indicador=="Total", UF=="SP",
         `Ano Publicação` %in% c(2019, 2020)) %>% 
  group_by(`Ano Publicação`, Pilar) %>%
  summarise(soma = sum(`Nota Normalizada Indicador`)) %>%
  mutate(`Ano Publicação` = factor(`Ano Publicação`)) %>% 
  mutate(Pilar = fct_reorder(Pilar, -soma)) %>% 
  mutate(Pilar = fct_relabel(Pilar, str_wrap, width=10)) %>% 
  ggplot(aes(x=Pilar, y=soma, color=`Ano Publicação`)) +
  geom_line(aes(group=`Ano Publicação`)) +
  coord_radial(expand=F, r_axis_inside = T) + # transforma as coordenadas
  theme_minimal() +
  theme(axis.title = element_blank())

# Dois jeitos de mudar os nomes na legenda:
dados_meteorologicos %>%
  select(Data, PRECIPITACAO, starts_with("TEMPER")) %>% 
  pivot_longer(c(-Data, -PRECIPITACAO)) %>% 
  #mutate(name = factor(name, levels=c("TEMPER MINIMA",
  #                                    "TEMPER MEDIA",
  #                                    "TEMPER MAXIMA"),
  #                     labels=c("Mínima", "Média", "Máxima"))) %>%
  #mutate(name = fct_relabel(name, \(x) paste("Temperatura", x))) %>% 
  ggplot(aes(x=Data, y=PRECIPITACAO)) +
  geom_col() +
  geom_line(aes(y=value, col=name)) +
  #scale_color_manual(values=c("blue", "black", "red"),
  #                   breaks=c("TEMPER MINIMA",
  #                            "TEMPER MEDIA",
  #                            "TEMPER MAXIMA"),
  #                   labels=c("Temperatura mínima",
  #                            "Média", "Max"),
  #                   name="") +
  theme(legend.position = "bottom")

# Mudando os nomes antes do ggplot:
dados_meteorologicos %>%
  select(Data, PRECIPITACAO, starts_with("TEMPER")) %>% 
  pivot_longer(c(-Data, -PRECIPITACAO)) %>% 
  mutate(name = factor(name, levels=c("TEMPER MINIMA",
                                      "TEMPER MEDIA",
                                      "TEMPER MAXIMA"),
                       labels=c("Mínima", "Média", "Máxima"))) %>%
  mutate(name = fct_relabel(name, \(x) paste("Temperatura", x))) %>% 
  ggplot(aes(x=Data, y=PRECIPITACAO)) +
  geom_col() +
  geom_line(aes(y=value, col=name)) +
  theme(legend.position = "bottom")

# Mudando os nomes pelo ggplot:
dados_meteorologicos %>%
  select(Data, PRECIPITACAO, starts_with("TEMPER")) %>% 
  pivot_longer(c(-Data, -PRECIPITACAO)) %>% 
  ggplot(aes(x=Data, y=PRECIPITACAO)) +
  geom_col() +
  geom_line(aes(y=value, col=name)) +
  scale_color_manual(values=c("blue", "black", "red"),
                     breaks=c("TEMPER MINIMA",
                              "TEMPER MEDIA",
                              "TEMPER MAXIMA"),
                     labels=c("Temperatura mínima",
                              "Média", "Max"),
                     name="") +
  theme(legend.position = "bottom")

# Mais conjuntos de dados -----

?datasets
?`datasetsICR-package`
?agridat
# https://dados.gov.br/dados/conjuntos-dados/

