##nube de palabras de los temas

library(purrr)
library(dplyr)
library(tidyr)
library(wordcloud2)
library(tidytext)
library(cld2)
library(cld3)
library(lubridate)

#Nube de palabras para ayudar a determinar los topicos
# preparo un listado de stopwords en varios idiomas para sacarlos de la nube de palabras
# No tengo idea si esto se puede hacer de otra manera más sencilla, pero funciona

idiomas <- list('spanish', 'portuguese', 'french', 'danish', 'dutch', 'finnish', 'german', 'hungarian', 'italian', 'norwegian', 'russian', 'swedish')

variosIdiomas_stop_words <- idiomas %>% 
  map(tm::stopwords) %>%
  flatten_dfc() %>% 
  gather(var, word) %>%
  bind_rows(stop_words) %>%
  select(word, lexicon)

palabras <- data_test %>%
  unnest_tokens(word, name) %>%
  anti_join(variosIdiomas_stop_words) %>%
  count(word)
