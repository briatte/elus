# 1. Creating Twitter credentials

Create a `credentials` folder and use it to save your Twitter credentials as
`my_oauth` objects stored in `rda` files:

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

# 2. Running the code

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

Please let use know if you produce a better sample of politicians!
