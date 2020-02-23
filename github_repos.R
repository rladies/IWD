# Tweets Github repos ---------------------------------------------------------
library(tidyverse)

data <- read_csv("https://raw.githubusercontent.com/rladies/starter-kit/master/Current-Chapters.csv") %>% 
  filter(!is.na(GitHub))

# Sentences template ------------------------------------------------------

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

# Get as many sentences as there are Chapters
combinations <- expand.grid(repo_templates, adjectives)
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


# Create tweet ------------------------------------------------------------

dataTw <- dplyr::mutate(
  data,
  tweet = stringr::str_replace(sentences, "chapter name", data$City),
  tweet = paste0(tweet, " Repo here: ", data$GitHub),
  tweet = paste(tweet, "#rladies #iwd2020"),
  picture = paste("Chapter", str_replace_all(data$City, fixed(" "), ""), ".png")
)

# save tweets
tweets <- dplyr::select(dataTw, City, tweet, picture) %>%
  arrange(City)
Encoding(tweets$City) <- "UTF-8"
Encoding(tweets$tweet) <- "UTF-8"

readr::write_csv(tweets, path = here::here("websites_repos_topics_tweets", "github_repo_tweets.csv"))
