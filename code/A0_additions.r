# ==============================================================================
#
# A0_additions.r -- add politicians' accounts from Regards Citoyens
#
# The script gets Twitter accounts from MPs and Senators, listed by the official
# Twitter accounts of both chambers and scraped by Regards Citoyens, and exports
# the list of those that do not exist in the main dataset.
#
# ==============================================================================

library(dplyr)
library(readr)

# ==============================================================================
# GET THE DATA
# ==============================================================================

# Source: https://github.com/regardscitoyens/twitter-parlementaires
d = read_csv("draft/twitter-parlementaires-master/data/deputes.csv",
             col_types = list(twitter_created_at = col_character())) %>%
  mutate(source = "Regards Citoyens", keep = "yes", mandates = "Député", last_tweeted = NA) %>%
  mutate(twitter = tolower(twitter), gender = ifelse(sexe == "H", "m", "f"),
         created = as.Date(substr(twitter_created_at, 1, 10)),
         name = paste(prenom, toupper(nom_de_famille))) %>%
  select(source, keep, name, gender, twitter, mandates,
         description = twitter_description, id = twitter_id, statuses = twitter_tweets,
         followers = twitter_followers, created, last_tweeted, slug)

# Source: https://github.com/regardscitoyens/twitter-parlementaires
s = read_csv("draft/twitter-parlementaires-master/data/senateurs.csv",
             col_types = list(twitter_created_at = col_character())) %>%
  mutate(source = "Regards Citoyens", keep = "yes", mandates = "Sénateur", last_tweeted = NA) %>%
  mutate(twitter = tolower(twitter), gender = ifelse(sexe == "H", "m", "f"),
         created = as.Date(substr(twitter_created_at, 1, 10)),
         name = paste(prenom, toupper(nom_de_famille))) %>%
  select(source, keep, name, gender, twitter, mandates,
         description = twitter_description, id = twitter_id, statuses = twitter_tweets,
         followers = twitter_followers, created, last_tweeted, slug)

# remove François Baroin from MPs (he became a Senator in October 2014)
d = filter(d, name != "François BAROIN")

# remove Julien Aubert from MPs (closed his account in 2013)
d = filter(d, name != "Julien AUBERT")

# bind both sources
rc = rbind(d, s)

# sanity check
stopifnot(!duplicated(rc$slug))

# ==============================================================================
# PARTY AFFILIATIONS
# ==============================================================================

# Source: https://github.com/regardscitoyens/rattachement-financier-parlementaires
p = rbind(read_csv("draft/rattachement-financier-parlementaires-master/data/1412-AN-rattachement-2015.csv") %>%
            select(slug = id_nosdeputes, party = rattachement_parti, groupe),
          read_csv("draft/rattachement-financier-parlementaires-master/data/1412-Sénat-rattachement-2015.csv") %>%
            select(slug = id_nossenateurs, party = rattachement_parti, groupe))

stopifnot(!duplicated(p$slug))

p$party[ p$party == "Europe Écologie Les Verts" ] = "EELV"
p$party[ p$party %in% c("Forces de Gauche", "Parti communiste Français") ] = "FDG"
p$party[ p$party == "Front National" ] = "FN"
p$party[ p$party %in% c("Non rattaché", "Non rattachée") ] = "IND"
p$party[ p$party == "Le Centre pour la France" ] = "MODEM"
p$party[ p$party == "Parti radical de gauche" ] = "PRG"
p$party[ p$party == "Parti socialiste" ] = "PS"
p$party[ p$party %in% c("Association PSLE - Nouveau Centre",
                        "Union des Radicaux, Centristes, Indépendants et Démocrates") ] = "UDI"
p$party[ p$party == "Union pour un Mouvement Populaire" ] = "UMP"

# mixed-left
p$party[ p$slug == "bruno-nestor-azerot" ] = "DVG" # Mouvement Initiative Populaire
p$party[ p$slug == "jacques-gillot" ] = "PS" # Guadeloupe Unie Socialisme et Réalité, sitting with PS
p$party[ p$slug == "serge-letchimy" ] = "PS" # GParti progressiste Martiniquais, sitting with PS
# p$party[ p$slug == "ary-chalus" ] = "PRG" # Guadeloupe Unie Socialisme et Réalité, sitting with PRG

print(table(p$party, exclude = NULL))

# ==============================================================================
# KEEP ONLY NEW ACCOUNTS
# ==============================================================================

k = read_csv("data/politicians.csv", col_types = list(id = col_character()))

rc = left_join(rc, p, by = "slug") %>%
  filter(!twitter %in% k$twitter)

# manual party additions
rc$party[ rc$name == "Frédéric BARBIER" ] = "PS"
rc$party[ rc$name == "Christian ECKERT" ] = "PS"
rc$party[ rc$name == "Jean-Philippe MALLÉ" ] = "PS"
rc$party[ rc$name == "Sylvie PICHOT" ] = "PS"
rc$party[ rc$name == "Patrick ABATE" ] = "FDG" # PCF
rc$party[ rc$name == "Jean BESSON" ] = "PS" # the Senator
rc$party[ rc$name == "Alain VASSELLE" ] = "UMP"

table(rc$party, exclude = NULL)

# ==============================================================================
# EXPORT ADDITIONS
# ==============================================================================

select(rc, source, keep, name, gender, party, twitter, mandates, description,
       id, statuses, followers, created, last_tweeted) %>%
  mutate(id = as.character(id)) %>%
  write_csv(path = "additions_rc.csv")

rm(list = ls())
gc()
