# 1. Storing Twitter credentials

Create a `credentials` folder and use it to save your credentials to the [Twitter REST API](https://dev.twitter.com/) as `my_oauth` objects stored in `rda` files:

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

Run all scripts in R in the indicated order.

`data/politicians.csv` contains edited results of script 01 as of May 2015, with additional accounts retrieved either by parsing the followers and appending all verified accounts that corresponded to individual politicians, or by adding them manually after checking the friends of selected Twitter accounts.

The code to find politicians in the verified accounts was:

```{r}
library(dplyr)
library(readr)

u = read_csv("data/users.csv")
filter(u, verified &
         grepl("énat(eur|rice)|éputé|maire|conseil|élu|communaut|président|ministre|secrétaire|porte-parole|région|départem", description, ignore.case = TRUE) &
         !tolower(screen_name) %in% d$twitter) %>%
  mutate(twitter = tolower(screen_name), description = gsub("\\n", ". ", description), party = NA) %>%
  select(name, party, twitter, description)
```

Please let use know if you improve the contents of that file!
