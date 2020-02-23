library(tidyverse)


# Clean data --------------------------------------------------------------
data <- import("filesRLadiesrepos.csv")

data <- data %>% 
  mutate(repo = str_extract(repo, "_(.*)"),
         repo = str_remove(repo, "_"),
         repo = str_to_title(repo))

### Count of materials updated by repo 

data %>% 
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

data <- data %>% 
  mutate(path = str_replace_all(path, "[:digit:]", ""),
         path = str_replace_all(path, "^-", ""),
         path = str_replace_all(path, "-", " "),
         path = str_replace_all(path, "_", " "),
         path = str_replace_all(path, "_", " "),
         path = str_remove(path, "[.](md|Rmd|pdf|pptx|R|Rproj|jpg|csv|txt|xls|PNG|docx)"))

data <- data %>% 
  mutate(name = str_replace_all(name, "[:digit:]", ""),
         name = str_replace_all(name, "^-", ""),
         name = str_replace_all(name, "-", " "),
         name = str_replace_all(name, "_", " "),
         name = str_replace_all(name, "_", " "),
         name = str_remove(name, "[.](md|Rmd|pdf|pptx|R|Rproj|jpg|csv|txt|xls|PNG|docx)"))

## Una idea podria ser qué repos tienen contenido sombre x tema?
data_test <- data %>%
  mutate(
    topic = ifelse(str_detect(name, "intro|Básico|beggin"), "Intro", NA),
    topic = ifelse(str_detect(name, "ggplot|Ggplot"), "ggplot", topic),
    topic = ifelse(str_detect(name, "shiny|Shiny"), "shiny", topic),
    topic = ifelse(str_detect(name, "tidy|Tidy"), "tidyverse", topic),
    topic = ifelse(str_detect(name, "rmarkdown|Rmarkdown"), "rmarkdown", topic),
    topic = ifelse(str_detect(name, "git|Git"), "git", topic),
    topic = ifelse(str_detect(name, "dplyr"), "dplyr", topic),
    topic = ifelse(str_detect(name, "blog"), "blogdown", topic),
    topic = ifelse(str_detect(name, "scrap"), "Web scrapping", topic),
    topic = ifelse(str_detect(name, "map"), "maps", topic),
    topic = ifelse(str_detect(name, "docker"), "docker", topic),
    topic = ifelse(str_detect(name, "mining"), "data mining", topic),
    topic = ifelse(str_detect(name, "Machine learning"), "Machine learning", topic),
  )

data_test %>% 
  filter(topic == "Intro") %>% 
  select(name)
