#Armado de las frases para los capitulos con datos
#Base code from Maëlle and the post in R-Ladies blog

# templates

templates <- c()


Chapter_adjectives <- c()


# get as many sentences as there are Chapters
combinations <- expand.grid(templates, Chapter_adjectives)
combinations <- dplyr::mutate_all(combinations, as.character)

create_sentence <- function(template, adjective){
  stringr::str_replace(template, "adjective", adjective)
}

sentences <- purrr::map2_chr(combinations$Var1, combinations$Var2,
                             create_sentence)
set.seed(42)
# the first one is chosen, this way it's not "another" or "one more"
sentences <- c(sentences[1],
               sample(sentences, nrow(CCRL) - 1, replace = TRUE))

# add actual tweet text
CCRLTw <- dplyr::mutate(CCRL,
                        tweet = stringr::str_replace(sentences,"X",CCRL$City),
                        tweet = ifelse(!is.na(CCRL$Meetup),
                                       paste0(tweet, " Meetups here: ", CCRL$Meetup), tweet),
                        tweet = paste(tweet, "#rladies #iwd2018"),
                        picture=paste("Chapter",str_replace_all(CCRL$City, fixed(" "), ""),".png"))

# save tweets
tweets <- dplyr::select(CCRLTw, City, tweet, picture) %>%
  arrange(City)
Encoding(tweets$City) <- "UTF-8"
Encoding(tweets$tweet) <- "UTF-8"
readr::write_csv(tweets, path = "ready_tweets.csv")
