library(tidyverse)
library(rio)
library(here)

# Clean data --------------------------------------------------------------
# data_files <- import("filesRLadiesrepos.csv")
# data_files <- filesRLadiesrepos
# data_files <- fulldata_final

data_files <- read_csv(here("websites_repos_topics_tweets", "github_repo_files.csv"))

data_files <- data_files %>% 
  mutate(repo = str_extract(repo, "_(.*)"),
         repo = str_remove(repo, "_"),
         repo = str_to_title(repo))

### Count of materials updated by repo 

cantidad_material <- data_files %>% 
  count(repo, sort = TRUE)

data_files %>% 
  count(repo, sort = TRUE) %>% 
  filter(n > 5) %>% 
  mutate(repo = fct_reorder(repo, n)) %>% 
  ggplot(aes(repo, n)) +
  geom_col() +
  coord_flip() +
  theme_minimal() %>% 
  labs(title = "R-Ladies chapters with uploaded content to Github repository",
       y = "",
       x = "Number of materials")

data_files <- data_files %>% 
  mutate(path = str_replace_all(path, "[:digit:]", ""),
         path = str_replace_all(path, "^-", ""),
         path = str_replace_all(path, "-", " "),
         path = str_replace_all(path, "_", " "),
         path = str_replace_all(path, "_", " "),
         path = str_remove(path, "[.](md|Rmd|pdf|pptx|R|Rproj|jpg|csv|txt|xls|PNG|docx)"))

data_files <- data_files %>% 
  mutate(name = str_replace_all(name, "[:digit:]", ""),
         name = str_replace_all(name, "^-", ""),
         name = str_replace_all(name, "-", " "),
         name = str_replace_all(name, "_", " "),
         name = str_replace_all(name, "_", " "),
         name = str_remove(name, "[.](md|Rmd|pdf|pptx|R|Rproj|jpg|csv|txt|xls|PNG|docx)"))

## Una idea podria ser qué repos tienen contenido sombre x tema?
# perfecto, usé la nube de palabras para determinar los temas 

data_with_topic <- data_files %>%
  mutate(
    topic = ifelse(str_detect(name, "intro|Básico|beggin|Novice|scratch|Scratch"), "Intro", NA),
    topic = ifelse(str_detect(name, "ggplot|Ggplot"), "ggplot", topic),
    topic = ifelse(str_detect(name, "shiny|Shiny"), "shiny", topic),
    topic = ifelse(str_detect(name, "tidy|Tidy"), "tidyverse", topic),
    topic = ifelse(str_detect(name, "rmarkdown|Rmarkdown"), "rmarkdown", topic),
    topic = ifelse(str_detect(name, "git|Git"), "git", topic),
    topic = ifelse(str_detect(name, "dplyr"), "dplyr", topic),
    topic = ifelse(str_detect(name, "blog"), "blogdown", topic),
    topic = ifelse(str_detect(name, "scrap"), "Web scrapping", topic),
    topic = ifelse(str_detect(name, "map|spatial|espacial"), "spatial", topic),
    topic = ifelse(str_detect(name, "docker"), "docker", topic),
    topic = ifelse(str_detect(name, "mining"), "data mining", topic),
    topic = ifelse(str_detect(name, "Machine learning"), "Machine learning", topic),
    topic = ifelse(str_detect(name, "pack"), "Package", topic),
    topic = ifelse(str_detect(name, "pur"), "purrr", topic),
    topic = ifelse(str_detect(name, "datavi|visualizacion"), "Data Vizualization", topic),
    topic = ifelse(str_detect(name, "model"), "Modelling", topic),
    topic = ifelse(str_detect(name, "RStudio|rstudio"), "RStudio", topic),
  )


#Filtramos el repo de Global porque tiene charlas sobre R-Ladies.
data_with_topic <- data_with_topic %>%
  filter(!is.na(topic)) %>% 
  rename(City = repo) %>% 
  mutate(City = ifelse(City == "La", "LA", City),
         City = ifelse(City == "Rtp", "RTP", City),
         City = ifelse(City == "Dc", "DC", City))

readr::write_csv(data_with_topic, path = here::here("websites_repos_topics_tweets", "data_with_topic.csv"))

#Podríamos filtrar por tema para armar las frases??? 

data_with_topic %>% 
  filter(topic == "Intro") %>% 
  select(name)

data_with_topic %>% 
  filter(topic == "ggplot") %>% 
  select(html_url)

data_with_topic %>% 
  filter(topic == "shiny") %>% 
  select(name)

data_with_topic %>% 
  filter(topic == "tidyverse") %>% 
  select(name)

