# 1. Storing Twitter credentials

Most scripts require scraping Twitter data with the functions written into `code/00_functions.r`. In order for the functions to run properly, c
reate a `credentials` folder and use it to save your credentials to the [Twitter REST API](https://dev.twitter.com/) as `my_oauth` objects stored in `rda` files:

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

# 2. Getting seed politicians

The `data/politicians.csv` file contains an edited version of the results produced by the scripts `code/01_politicians.r` and `code/A0_additions.r` as of May 2015, with additional accounts retrieved either by parsing their "4+" followers (produced by `code/
03_users.r`) and appending all verified accounts that corresponded to individual politicians, or by adding them manually after checking the friends of selected Twitter accounts.

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

Please let use know if you improve the contents of that file, which is released under [ODBL](http://www.vvlibri.org/node/61) to match the [requirements of Regards Citoyens](https://github.com/regardscitoyens/twitter-parlementaires/blob/master/README.md).


# 3. Running the code

To replicate, move all scripts from the `code` folder to the root repository, and run them in the indicated order. Package dependencies are indicated at the beginning of each script.

Most scripts will require a few days to run in full, either because of time limits imposed by the Twitter REST API, or because of the very slow Bayesian estimation procedure.

We recommend going through the [replication material](http://dx.doi.org/10.7910/DVN/26589) for Pablo Barberá's paper to anyone who would be seriously interested in replicating the analysis.
