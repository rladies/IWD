#Some adds: use este código: str_locate_all(data$Twitter, "/") para encontrar las / y poder generar el tag
# de twitter en el texto

# Tweets function ---------------------------------------------------------
library(tidyverse)

create_tweets <- function(data,
                          templates,
                          adjectives,
                          variable,
                          extra_text)
{
# Get as many sentences as there are Chapters
combinations <- expand.grid(templates, adjectives)
combinations <- dplyr::mutate_all(combinations, as.character)

create_sentence <- function(template, adjective) {
  stringr::str_replace(template, "adjective", adjective)
}

sentences <- purrr::map2_chr(combinations$Var1, combinations$Var2,
                             create_sentence)
set.seed(123)
# the first one is chosen, this way it's not "another" or "one more"
sentences <- c(sentences[1],
               sample(sentences, nrow(data) - 1, replace = TRUE))

# create tweet

dataTw <- mutate(
    data,
    tweet = stringr::str_replace(sentences, "chapter name", data$City),
    tweet = paste0(tweet, extra_text, {{variable}}),
    tweet = paste(tweet, "#rladies #iwd2020"))

attach(dataTw)
if(exists("Twitter")){
  dataTw <- dataTw %>%
    mutate(tweet = paste(tweet, " @", str_sub(data$Twitter, 21, str_length(data$Twitter)),sep=""))}
detach(dataTw)

# save tweets
tweets <- dplyr::select(dataTw, City, tweet) %>%
  arrange(City)
Encoding(tweets$City) <- "UTF-8"
Encoding(tweets$tweet) <- "UTF-8"

return(tweets)
}


# Repos -------------------------------------------------------------------

data <- read_csv("https://raw.githubusercontent.com/rladies/starter-kit/master/Current-Chapters.csv") %>%
  filter(!is.na(GitHub))

adjectives <- c("awesome", "fantastic", "wonderful", "amazing",  "phenomenal",
                "remarkable", "incredible",  "magnificent", "marvelous",
                "insightful", "admirable", "outstanding", "splendid", "exceptional",
                "intuitive", "brilliant", "excellent", "cool", "super", "epic")

repo_templates <-
  c(
    "Did you know that chapter name has a adjective Github repo full of awesome content? You can visit here!",
    "Hop over to watch this adjective R-Ladies Github repo from chapter name!",
    "Do you like #rstats? Looking to learn more? Visit the chapter name's adjective Github repo!",
    "Looking for something to inspire you? browse the adjective chapter name's Github repo",
    "Learn all about what #rstat with this adjective Github repo from chapter name!",
    "Nourish your mind today! Watch this adjective chapter name Github repo!",
    "Want some adjective ideas to implement at your R-Ladies meetup? chapter name Github repo has some in store for you!"

  )

github_repo_tweets <- create_tweets(data,
              repo_templates,
              adjectives,
              variable = GitHub,
              extra_text = " Repo here: ")

readr::write_csv(github_repo_tweets, path = here::here("websites_repos_topics_tweets", "github_repo_tweets.csv"))

#Used csv2 because we have some encode issues (windows and/or non english characters)
readr::write_excel_csv2(github_repo_tweets, path = here::here("github_repo_tweets_csv2.csv"))


# Websites ----------------------------------------------------------------
website_data <- read_csv("https://raw.githubusercontent.com/rladies/starter-kit/master/Current-Chapters.csv") %>%
  filter(!is.na(Website))

web_templates <-
  c(
    "Did you know that chapter name has a adjective web page full of awesome content? You can visit here!",
    "Hop over to watch this adjective R-Ladies web page from chapter name!",
    "Do you like #rstats? Looking to learn more? Visit the chapter name's adjective web page!",
    "Looking for something to inspire you? browse the adjective chapter name's web page",
    "Learn all about what #rstat with this adjective web page from chapter name!",
    "Nourish your mind today! Watch this adjective chapter name material!",
    "Want some adjective ideas to implement at your R-Ladies meetup? chapter name website has some in store for you!"
  )

website_tweets <- create_tweets(website_data,
                                    repo_templates,
                                    adjectives,
                                    variable = Website,
                                    extra_text = " Website here: ")

readr::write_csv(website_tweets, path = here::here("websites_repos_topics_tweets", "website_tweets.csv"))


# Topics GGPLOT------------------------------------------------------------------
data_with_topic <- read_csv(here::here("websites_repos_topics_tweets", "data_with_topic.csv"))

ggplot_data <- data_with_topic %>%
  filter(topic == "ggplot") %>%
  filter(!is.na(City)) %>%
  filter(!str_detect(html_url, ".Rmd")) %>% ## Istambul had 2 rows with same content, 1 .R and one .Rmd
  select(City, html_url)

adjectives <- c("awesome", "fantastic", "wonderful", "amazing",  "phenomenal",
                "remarkable", "incredible",  "magnificent", "marvelous",
                "insightful", "admirable", "outstanding", "splendid", "exceptional",
                "intuitive", "brilliant", "excellent", "cool", "super", "epic")

ggplot_templates <-
  c(
    "Learn about the grammar of graphics with #ggplot! Check chapter name adjective materials!",
    "Feeling like learning about #datavis with #ggplot? chapter name chapter has some adjective materials on it!",
    "Because plots with #ggplot2 are adjective plots! Visit the chapter name's materials on the topic!",
    "Need to prepare a workshop on #ggplot? Browse the adjective chapter name's materials for inspiration!",
    "#Datavis is fun with #ggplot! Check chapter name adjective materials for extra fun!",
    "Looking for materials in #ggplot? Check chapter name's adjective materials!"
    )


ggplot_tweets <- create_tweets(ggplot_data,
                                ggplot_templates,
                                adjectives,
                                variable = html_url,
                                extra_text = " ")


readr::write_csv(ggplot_tweets, path = here::here("websites_repos_topics_tweets", "ggplot_tweets.csv"))


# Topics Shiny ------------------------------------------------------------

# data_with_topic <- read_csv(here::here("websites_repos_topics_tweets", "data_with_topic.csv"))

shiny_data <- data_with_topic %>%
  filter(topic == "shiny") %>%
  filter(!is.na(City)) %>%
  filter(!str_detect(html_url, "_images.zip")) %>% ## Istambul had 2 rows with same content
  select(City, html_url)

adjectives <- c("awesome", "fantastic", "wonderful", "amazing",  "phenomenal",
                "remarkable", "incredible",  "magnificent", "marvelous",
                "insightful", "admirable", "outstanding", "splendid", "exceptional",
                "intuitive", "brilliant", "excellent", "cool", "super", "epic")

shiny_templates <-
  c(
    "Take your skills to the next level with #shiny! Check chapter name adjective materials!",
    "Want to build your first #Shiny app? chapter name chapter has some adjective materials on it!",
    "Get your hands on #Shiny dashboards! Visit the chapter name's adjective materials on the topic!",
    "Need to prepare a workshop on #Shiny Apps? Browse the adjective chapter name's materials for inspiration!",
    "Learn how to create a #Shiny web app in R! Check chapter name adjective materials on it!",
    "Check the basics of building #Shiny apps in R at chapter name's adjective materials!"
  )


shiny_tweets <- create_tweets(shiny_data,
                               shiny_templates,
                               adjectives,
                               variable = html_url,
                               extra_text = " ")


readr::write_csv(shiny_tweets, path = here::here("websites_repos_topics_tweets", "shiny_tweets.csv"))

# Topics Rmarkdown ------------------------------------------------------------

# data_with_topic <- read_csv(here::here("websites_repos_topics_tweets", "data_with_topic.csv"))

rmarkdown_data <- data_with_topic %>%
  filter(topic == "rmarkdown") %>%
  filter(!is.na(City)) %>%
  select(City, html_url)

rmarkdown_templates <-
  c(
    "Take your skills to the next level with #rmarkdown! Check chapter name adjective materials!",
    "Want to make reproducible reports with #rmarkdown? chapter name chapter has some adjective materials on it!",
    "Forget about copy-pasting outputs, learn #rmarkdown and make your research reproducible! Check chapter name adjective materials",
    "Need to prepare a workshop on #Rmarkdown? Browse the adjective chapter name's materials for inspiration!",
    "Help your future self by making reproducible reports with #Rmardown and pick chapter name's adjective materials!"
  )


rmarkdown_tweets <- create_tweets(rmarkdown_data,
                                  rmarkdown_templates,
                              adjectives,
                              variable = html_url,
                              extra_text = " ")


readr::write_csv(rmarkdown_tweets, path = here::here("websites_repos_topics_tweets", "rmarkdown_tweets.csv"))

# Topics Tidyverse ------------------------------------------------------------

# data_with_topic <- read_csv(here::here("websites_repos_topics_tweets", "data_with_topic.csv"))

tidyverse_data <- data_with_topic %>%
  filter(topic == "tidyverse") %>%
  filter(!is.na(City)) %>%
  select(City, html_url)

tidyverse_templates <-
  c(
    "Are you ready to master the %>% pipe with chapter name adjective materials on #tidyverse?",
    "Master your data science skills with the #tidyverse ! Check chapter name adjective materials",
    "Are you taking your first R steps? The #tidyverse will make this journey more fun! Check chapter name's adjective materials!",
    "Learn all you need to know about the #tidyverse! Check chapter name adjective materials!",
    "Need to prepare a workshop on #Tidyverse? Browse the adjective chapter name's materials for inspiration!"
  )


tidyverse_tweets <- create_tweets(tidyverse_data,
                                  tidyverse_templates,
                                  adjectives,
                                  variable = html_url,
                                  extra_text = " ")


readr::write_csv(tidyverse_tweets, path = here::here("websites_repos_topics_tweets", "tidyverse_tweets.csv"))

# Topics Dplyr ------------------------------------------------------------

data_with_topic <- read_csv(here::here("websites_repos_topics_tweets", "data_with_topic.csv"))

dplyr_data <- data_with_topic %>%
  filter(topic == "dplyr") %>%
  filter(!is.na(City)) %>%
  select(City, html_url)

dplyr_templates <-
  c(
    "Are you ready to master the %>% pipe with chapter name adjective materials on #dplyr?",
    "Master your data science skills with #dplyr ! Check chapter name adjective materials",
    "Are you taking your first R steps? The #dplyr package will make this journey more fun! Check chapter name's adjective materials!",
    "Select %>% mutate %>% arrange %>% filter, master the #dplyr verbs with chapter name adjective materials!",
    "Need to prepare a workshop on #dplyr? Browse the adjective chapter name's materials for inspiration!"
  )


dplyr_tweets <- create_tweets(
  dplyr_data,
  dplyr_templates,
  adjectives,
  variable = html_url,
  extra_text = " "
)


readr::write_csv(dplyr_tweets, path = here::here("websites_repos_topics_tweets", "dplyr_tweets.csv"))


# Tweets git ------------------------------------------------------------


data_with_topic <- read_csv(here::here("websites_repos_topics_tweets", "data_with_topic.csv"))

git_data <- data_with_topic %>% 
  filter(topic == "git") %>% 
  filter(!is.na(City)) %>% 
  select(City, html_url) 

git_templates <-
  c(
    "Tired of naming your file_verylast_v10? Check chapter name adjective materials on #git!",
    "Dive in the world of git with chapter name adjective materials!",
    "Embrace version control with #git! Check the adjective chapter name's materials for encouragement!",
    "Need to prepare a workshop on #git? Browse the adjective chapter name's materials for inspiration!")


git_tweets <- create_tweets(
  git_data,
  git_templates,
  adjectives,
  variable = html_url,
  extra_text = " "
)


readr::write_csv(git_tweets,
                 path = here::here("websites_repos_topics_tweets", "git_tweets.csv"))


# Tweets webscrap  -----------------------------------------------------------------


data_with_topic <- read_csv(here::here("websites_repos_topics_tweets", "data_with_topic.csv"))

web_data <- data_with_topic %>% 
  filter(topic == "Web scrapping") %>% 
  filter(!is.na(City)) %>% 
  select(City, html_url) 

web_templates <-
  c(
    "Dive in the world of web scrapping with chapter name adjective materials!",
    "Want to learn web scrapping? Check chapter name adjective materials on it!",
    "Master your skills for webscrapping! Browse the adjective chapter name's materials for inspiration!")


web_tweets <- create_tweets(
  web_data,
  web_templates,
  adjectives,
  variable = html_url,
  extra_text = " "
)


readr::write_csv(web_tweets,
                 path = here::here("websites_repos_topics_tweets", "web_tweets.csv"))



# Tweets blogdown -----------------------------------------------------------
data_with_topic <- read_csv(here::here("websites_repos_topics_tweets", "data_with_topic.csv"))

topics <- data_with_topic %>% count(topic, sort = TRUE) %>% slice(6,8,10:17) %>% pull(topic)

blogdown_data <- data_with_topic %>% 
  filter(topic == "blogdown") %>% 
  filter(!is.na(City)) %>% 
  select(City, html_url) 

blogdown_templates <-
  c(
    "Want to start a blog? Check chapter name adjective materials on #blogdown!")

blogdown_tweets <- create_tweets(
  blogdown_data,
  blogdown_templates,
  adjectives,
  variable = html_url,
  extra_text = " "
)


readr::write_csv(blogdown_tweets,
                 path = here::here("websites_repos_topics_tweets", "blogdown_tweets.csv"))

# Tweets purrr -----------------------------------------------------------
data_with_topic <- read_csv(here::here("websites_repos_topics_tweets", "data_with_topic.csv"))

topics <- data_with_topic %>% count(topic, sort = TRUE) %>% slice(6,8,10:17) %>% pull(topic)

purrr_data <- data_with_topic %>% 
  filter(topic == "purrr") %>% 
  filter(!is.na(City)) %>% 
  select(City, html_url) 

purrr_templates <-
  c(
    "Loops without a for or a while? Check chapter name adjective materials on #purrr!",
    "Listen to Jenny Brian and repeat yourself with  #purrr. Check chapter name adjective materials here",
    "Iterating with #purrr -> Check chapter name adjective materials on it!")

purrr_tweets <- create_tweets(
  purrr_data,
  purrr_templates,
  adjectives,
  variable = html_url,
  extra_text = " "
)


readr::write_csv(purrr_tweets,
                 path = here::here("websites_repos_topics_tweets", "purrr_tweets.csv"))


# Tweets spatial -----------------------------------------------------------

#data_with_topic <- read_csv(here::here("websites_repos_topics_tweets", "data_with_topic.csv"))

topics <- data_with_topic %>% count(topic, sort = TRUE) %>% slice(6,8,10:17) %>% pull(topic)

spatial_data <- data_with_topic %>% 
  filter(topic == "spatial") %>% 
  filter(!is.na(City)) %>% 
  select(City, html_url) 

spatial_templates <-
  c("Do you like maps? Check chapter name adjective materials on #rspatial!",
    "Maps are great !, Do you want to learn how to do them with #rstats?. Check chapter name adjective materials here",
    "Geotechnologies and geosciences with #rspatial! Check chapter name adjective materials on it!")

spatial_tweets <- create_tweets(
  spatial_data,
  spatial_templates,
  adjectives,
  variable = html_url,
  extra_text = " "
)


readr::write_csv(spatial_tweets,
                 path = here::here("websites_repos_topics_tweets", "spatial_tweets.csv"))


# Tweets Packages -----------------------------------------------------------

#data_with_topic <- read_csv(here::here("websites_repos_topics_tweets", "data_with_topic.csv"))

topics <- data_with_topic %>% count(topic, sort = TRUE) %>% slice(6,8,10:17) %>% pull(topic)

pack_data <- data_with_topic %>% 
  filter(topic == "Package") %>% 
  filter(!is.na(City)) %>% 
  select(City, html_url) 

pack_templates <-
  c("CRAN is waiting for you package!, learn how to do it Checking chapter name adjective materials on #rpackage!",
    "Do you want to learn how to do a #rpackage?. Check chapter name adjective materials here",
    "Check chapter name adjective materials on #rpackage development!")

pack_tweets <- create_tweets(
  pack_data,
  pack_templates,
  adjectives,
  variable = html_url,
  extra_text = " "
)


readr::write_csv(pack_tweets,
                 path = here::here("websites_repos_topics_tweets", "pack_tweets.csv"))

# Tweets Data Vizualization -----------------------------------------------------------

#data_with_topic <- read_csv(here::here("websites_repos_topics_tweets", "data_with_topic.csv"))

topics <- data_with_topic %>% count(topic, sort = TRUE) %>% slice(6,8,10:17) %>% pull(topic)

dataviz_data <- data_with_topic %>% 
  filter(topic == "Data Vizualization") %>% 
  filter(!is.na(City)) %>% 
  select(City, html_url) 

dataviz_templates <-
  c(
    "Feeling like learning about #datavis? chapter name chapter has some adjective materials on it!",
    "Need to prepare a workshop on #datavis? Browse the adjective chapter name's materials for inspiration!",
    "#Datavis is fun! Check chapter name adjective materials for extra fun!",
    "Looking for materials in #datavis? Check chapter name's adjective materials!")

dataviz_tweets <- create_tweets(
  dataviz_data,
  dataviz_templates,
  adjectives,
  variable = html_url,
  extra_text = " "
)


readr::write_csv(dataviz_tweets,
                 path = here::here("websites_repos_topics_tweets", "datavis_tweets.csv"))

# Tweets Modeling -----------------------------------------------------------

#data_with_topic <- read_csv(here::here("websites_repos_topics_tweets", "data_with_topic.csv"))

topics <- data_with_topic %>% count(topic, sort = TRUE) %>% slice(6,8,10:17) %>% pull(topic)

modeling_data <- data_with_topic %>% 
  filter(topic == "Modelling") %>% 
  filter(!is.na(City)) %>%
  filter(City == "Bari") %>% #Bari has a New Year's Eve greeting and a word in Italian has the tag search pattern and is misclassified
  select(City, html_url) 

modeling_templates <-
  c(
    "Feeling like learning about #modeling? chapter name chapter has some adjective materials on it!",
    "Need to prepare a workshop on #modeling? Browse the adjective chapter name's materials for inspiration!")

modeling_tweets <- create_tweets(
  modeling_data,
  modeling_templates,
  adjectives,
  variable = html_url,
  extra_text = " "
)


readr::write_csv(modeling_tweets,
                 path = here::here("websites_repos_topics_tweets", "modeling_tweets.csv"))
