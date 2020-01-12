# IWD
## International Women Day Ideas
Every March 8 International Women's Day is celebrated, from R-Ladies a campaign is carried out using the twitter account [@rladies_iwd](https://twitter.com/rladies_iwd)

Here some details of previous campaigns:

2019: [IWD 2019 Twitter Action](https://blog.rladies.org/post/blog_iwdtwitter_2019/)
2018: [Behind the scenes of R-Ladies IWD2018 Twitter action!](https://blog.rladies.org/post/ideation_and_creation/)

## Other ideas for future campaings:

- Catalog the meetups material in github from the chapters and tweet this materials during the campaing.
- Made a list of blogs of R-Ladies and a post choose by the autor and tweet this materials during 8 March.
- Made a list of packages made for R-Ladies and post this during the campaing.

## Catalog the meetups material in github from the chapters and tweet this materials during the campaing.

We have this code made by Marianna Foos as a starting point:

```
library(httr)
library(jsonlite)
library(dplyr)repolist <- fromJSON("https://api.github.com/users/rladies/repos?page=1&per_page=100")
repolist2 <- fromJSON("https://api.github.com/users/rladies/repos?page=2&per_page=100")pres_repos <- grep("presentation", c(repolist$name, repolist2$name),
     value = T, ignore.case = T)combin <- list()
for (r in 1:length(pres_repos)){
  try(url <- GET(paste0("https://api.github.com/repos/rladies", "/", pres_repos[r], "/contents"),
                 authenticate("mfoos", Sys.getenv("GITHUBTOKEN")),
                 query = list(state = "all")))
  if (!(is.null(url))){
    contents <- fromJSON(content(url, type = "text"))
    df <- contents %>%
      filter(name != "README.md") %>%
      mutate(repo = pres_repos[r]) %>%
      select(-`_links`) #bind_rows won't work unless you remove this column
    combin[[r]] <- df
  }
}
fulldata <- bind_rows(combin) %>%
  filter(!(startsWith(name, "."))) %>%
  filter(!(startsWith(name, "_")))
 ``` 
This could be an starting point for order and made more searchable and findable our materials.
