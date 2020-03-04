# Some adds: use este código: str_locate_all(data$Twitter, "/") para encontrar las / y poder generar el tag
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
set.seed(42)
# the first one is chosen, this way it's not "another" or "one more"
sentences <- c(sentences[1],
               sample(sentences, nrow(data) - 1, replace = TRUE))

# create tweet 

dataTw <- mutate(
    data,
    tweet = stringr::str_replace(sentences, "chapter name", data$City),
    tweet = paste0(tweet, extra_text, {{variable}}),
    tweet = paste(tweet, "#rladies #iwd2020"),
    tweet = paste(tweet, " @", str_sub(data$Twitter, 21, str_length(data$Twitter)),sep=""),
    picture = paste("Chapter", str_replace_all(data$City, fixed(" "), ""), ".png",sep="")
  )

# save tweets
tweets <- dplyr::select(dataTw, City, tweet, picture) %>%
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
