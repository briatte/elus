Code to estimate Bayesian ideal points for French politicians from Twitter data. Forked from [pablobarbera](https://github.com/pablobarbera)'s code at [`twitter_ideology`](https://github.com/pablobarbera/twitter_ideology/). Edits by [briatte](https://github.com/briatte) and [3wen](https://github.com/3wen).

# HOWTO

Create a `credentials` folder and use it to save your Twitter credentials as
`my_oauth` objects in one or more `rda` files:

```{r}
library(ROAuth)
dir.create("credentials")

requestURL = "https://api.twitter.com/oauth/request_token"
accessURL = "https://api.twitter.com/oauth/access_token"
authURL = "https://api.twitter.com/oauth/authorize"

# fill in appropriately
consumerKey = "AAABBBCCCXXXYYYZZZ"
consumerSecret = "AAABBBCCCXXXYYYZZZ"

my_oauth = OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret,
                            requestURL = requestURL, accessURL = accessURL, authURL = authURL)

# run handshake separately
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

save(my_oauth, file = "credentials/oauth_youraccount.rda")
```

Then run the scripts in R, in the following order:

- `00_functions.r` initialises the Twitter scraper functions
- `01_politicians.r` scrapes Twitter accounts for ~ 800 French politicians
- `02_followers.r` retrieves their followers and creates the adjacency matrix
- `03_users.r` retrieves Twitter user information on the followers
- `04_select.r` selects followers based on their activity and geographical location
- `05_map.r` draws maps of the followers located in metropolitan France
- `06_model_stage1.r` runs the first stage of the model from selected users

<!-- - `07_model_functions.r` prepares the second stage of the model -->
<!-- - `08_model_stage2.r` runs the second stage of the model on all users -->

`data/politicians.csv` contains edited results of script 01 as of March 2015, with additional accounts retrieved either by parsing the followers and appending all verified accounts that corresponded to individual politicians, or by adding them manually after checking the friends of selected Twitter accounts. The code to find politicians in the verified accounts was:

```{r}
library(dplyr)
library(readr)

u = read_csv("data/users.csv")
filter(u, verified &
         grepl("énat(eur|rice)|éputé|maire|conseil|élu|communaut|président|ministre|secrétaire|porte-parole|région|départem", description, ignore.case = TRUE) &
         !tolower(screen_name) %in% d$twitter) %>%
  mutate(twitter = tolower(screen_name), description = gsub("\\n", ". ", description), party = NA) %>%
  select(name, party, twitter, description) %>%
  write_csv(., "additions.csv")
```

# SOURCES

- Initial politicians' Twitter accounts from [Elus 2.0](http://www.elus20.fr/elus-web-facebook-twitter/).
- Follower maps based on French administrative units from Insee via [Wikipedia Francophone](https://fr.wikipedia.org/wiki/), geocodes from [Google Maps](https://developers.google.com/maps/), and GEOFLA shapefiles from [IGN](http://professionnels.ign.fr/geofla).
- List of French first names by [Mike Campbell and Boris New](http://www.lexique.org/public/prenoms.php).
- Population age group figures by [Irdes](https://www.data.gouv.fr/fr/datasets/population-par-tranche-d-age-et-sexe-estimations-localisees-de-population/).
- Voter registration figures by [Conseil général de Gironde](https://www.data.gouv.fr/fr/datasets/taux-d-inscription-sur-les-listes-electorales-rdl/).

# DEPENDENCIES

- `dplyr` and `readr` to manipulate results
- `rvest` and `stringr` to scrape and extract details
- `ROAuth` and `rjson` to access and parse Twitter data
- `rstan` for the first stage of the model
- `ggplot2` for graphs and `ggmap` for maps

<!-- - `arm`, `parallel` and `R2WinBUGS` for the second stage -->

# SEEALSO

- [This paper](https://files.nyu.edu/pba220/public/barbera_twitter_ideal_points.pdf) by [pablobarbera](https://github.com/pablobarbera) explains the nuts and bolts of the model.
- [This blog post](http://blogs.lse.ac.uk/europpblog/2014/12/09/political-discussions-on-twitter-during-elections-are-dominated-by-those-with-extreme-views/) by [pablobarbera](https://github.com/pablobarbera) and [griverorz](https://github.com/griverorz) explains how to measure partisan polarisation on Twitter.
- [This blog post](http://politbistro.hypotheses.org/2589) (in French) explains how to plot the data as a network.
- [This blog post](http://stats2u.blogspot.fr/2015/03/ideologia-de-politicos-usando-o-twitter.html) does something similar with Brazilian politicians.
- [This paper](http://dx.doi.org/10.1017/S0003055414000525) does something similar with Facebook likes.
