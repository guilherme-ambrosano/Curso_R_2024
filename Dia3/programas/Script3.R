library(tidyverse)
library(broom)

# Tidyverse ----

# broom
# purrr
# ggplot2


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

# Exemplos ----

adugna.sorghum
alwan.lamb
archbold.apple
australia.soybean
devries.pine
usgs.herbicides
kreusler.maize
eden.potato
belamkar.augmented

