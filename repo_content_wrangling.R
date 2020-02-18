library(rio)
library(tidyverse)
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
data %>% 
  filter(str_detect(name, "ggplot|Ggplot")) %>% 
  select(name, repo)

data %>% 
  filter(str_detect(name, "shiny|Shiny")) %>% 
  select(name, repo)

data %>% 
  filter(str_detect(name, "tidy|Tidy")) %>% 
  select(name, repo)

data %>% 
  filter(str_detect(name, "rmarkdown|Rmarkdown")) %>% 
  select(name, repo)
