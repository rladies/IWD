# IWD
## International Women's Day Ideas
Every March 8, R-Ladies launches a campaign for International Women's Day using the twitter account [@rladies_iwd](https://twitter.com/rladies_iwd)

Here some details of previous campaigns:

* 2019: [IWD 2019 Twitter Action](https://blog.rladies.org/post/blog_iwdtwitter_2019/)
* 2018: [Behind the scenes of R-Ladies IWD2018 Twitter action!](https://blog.rladies.org/post/ideation_and_creation/)

## Other ideas for future campaigns

- Catalog the meetup material in GitHub from the chapters and tweet this material during the campaign.
- Make a list of blogs (and posts) by R-Ladies, and tweet this material on March 8.
- Make a list of packages built for/by R-Ladies and post this during the campaign.

## Catalog the meetup material in GitHub from the chapters and tweet this material during the campaign

We have this code from Marianna Foos as a starting point:

```
library(httr)
library(jsonlite)
library(dplyr)repolist <- fromJSON("https://api.github.com/users/rladies/repos?page=1&per_page=100")
repolist2 <- fromJSON("https://api.github.com/users/rladies/repos?page=2&per_page=100")pres_repos <- grep("presentation", c(repolist$name, repolist2$name),
     value = T, ignore.case = T)combin <- list()
for (r in 1:length(pres_repos)){
  try(url <- GET(paste0("https://api.github.com/repos/rladies", "/", pres_repos[r], "/contents"),
                 authenticate("user name", Sys.getenv("GITHUBTOKEN")),
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
This could be a starting point for order and made more searchable and findable our materials.

## Campaign

Step 1: a week before 8M we start tweeting about the campaign, we can use something like this as a start point: https://www.canva.com/design/DADQogEJSyE/DKhQvMc602cnmCtt4w1jhg/edit

Step 2: the day before 8M, we post some stats about R-Ladies' events: here is the [code](https://github.com/rladies/IWD/blob/master/events.R) with some stats idea as a starting point. Here is the [data](https://github.com/rladies/IWD/blob/master/eventsRLadiesUntilJanuary2020.csv)

Step 3: on 8M (48 hours), we tweet the material from Chapters repo (we have to figure out how to compile the list of materials).

Step 4: Celebrate and enjoy another IWD!!
