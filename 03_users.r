#==============================================================================
#
# 03_users.r -- download information on Twitter users
#
# The sampled users are those who follow 4+ politicians. A few checks are in
# place to make sure that the resulting dataset (model/users.rda) matches the
# initial "4+ followers" list (model/userlist.rda). The final sample size of
# above 700,000 users took two weeks to scrape in full.
#
#==============================================================================

source("00_functions.r")

library(readr)

#==============================================================================
# DOWNLOAD DETAILS ON SAMPLED USERS
#==============================================================================

if(!file.exists("model/users.rda")) {

  load("model/userlist.rda")
  users = vector(mode = "list", length = length(userlist))
  names(users) = userlist
  save(users, file = "model/users.rda")

}

#
# download user information
#

load("model/users.rda")
load("model/userlist.rda")

# sanity check 1: all followers are from the user list
ex = !names(users) %in% userlist
if(any(ex)) {

  cat("Removing", sum(ex), "excess users...\n")
  users = users[ !ex ]

}

# sanity check 2: remove duplicates
du = which(duplicated(names(users)))
if(length(du)) {

  cat("Removing", length(du), "duplicates...\n")
  users = users[ -du ]

}

# append any new follower
if(length(userlist) != length(users)) {

  cat("Appending", length(userlist) - length(users), "additional users...\n")
  stopifnot(length(userlist) > length(users))

  more = userlist[ !userlist %in% names(users) ]
  adds = vector(mode = "list", length(more))
  names(adds) = more

  users = c(users, adds)

  cat("Saving new users dataset...\n")
  save(users, file = "model/users.rda")

}

empty = sapply(users, is.null)
left = names(users)[ empty ]

# # uncomment to try fixing up to 1,000 network errors on first iteration
# # n.b. some errors are unsolvable (e.g. closed or protected accounts)
# empty = sapply(users, class) == "character"
# left = names(users)[ empty ]

# loop over each account
while(length(left) > 0) {

  cat("Retrieving users:", length(users), "in total,",
      length(users) - sum(empty), "identified", length(left), "left\n")

  left_sample = sample(left, ifelse(length(left) >= 1000, 1000, length(left)))

  for(i in rev(left_sample)) {

    j = which(left_sample == i)

    # download users (with some exception handling...)
    error = tryCatch(user <- getUsers(user_id = i, oauth_folder = "credentials"),
                     error = function(e) e)

    if (inherits(error, 'error')) {

      cat(sprintf("%4.0f", j), "id", sprintf("%10s", i), ":", error$message, "(waiting 10 sec.)\n")

      users[[ i ]] = "failed" # less than 1% of all users failed
      Sys.sleep(10)

    } else {

      cat(sprintf("%4.0f", j), "id", sprintf("%10s", i), ":", user$screen_name, "\n")

      users[[ i ]] = user

    }

  }

  cat(date(), ": saving users...\n")
  save(users, file = "model/users.rda")

  # loop over remaining users

  cat(date(), ": looping...\n")

  empty = sapply(users, is.null)
  left = names(users)[ empty ]

  cat(date(), ": done.\n\n")

}

#==============================================================================
# PARSE IDENTIFIED USERS
#==============================================================================

cat(date(), ": building users dataset...\n")

cat("- Followers:", length(users),
    "\n- Missing:", sum(sapply(users, is.null)),
    "\n- Failed:", sum(sapply(users, class) == "character"), "\n")

users = users[ sapply(users, class) == "list" ]
cat("- Parsing", length(users), "users...\n")

u = data.frame(
  id = get_nfo(users, "id_str"),
  name = get_nfo(users, "name"),
  screen_name = get_nfo(users, "screen_name"), # not used later on
  # lang = get_nfo(users, "lang"),
  location = get_nfo(users, "location"),
  # tz = get_nfo(users, "time_zone"),
  # offset = get_nfo(users, "utc_offset"),
  # geo = get_nfo(users, "geo_enabled"),
  statuses = get_nfo(users, "statuses_count"),
  followers = get_nfo(users, "followers_count"),
  created = get_nfo(users, "created_at"),
  description = get_nfo(users, "description"),
  last_tweeted = get_status(users, "created_at"),
  # last_tweet = get_status(users, "text"),
  # protected = get_nfo(users, "protected"),
  verified = get_nfo(users, "verified"),
  row.names = NULL,
  stringsAsFactors = FALSE
)

# date of last tweet (when available)
u$last_tweeted = as.POSIXct(u$last_tweeted, format = "%a %b %d %H:%M:%S %z %Y")
u$last_tweeted = as.Date(u$last_tweeted)

# account creation time, in days
u$age = as.integer(Sys.time() - as.POSIXct(u$created, format = "%a %b %d %H:%M:%S %z %Y"))

# buggy creation date (happens to be Xavière Tibéri)
u$age[ u$age > as.integer(Sys.Date() - as.Date("2006-01-01")) ] = NA

# account creation year
u$created = as.integer(substring(u$created, first = nchar(u$created) - 3))
u$created[ u$created < 2006 ] = NA

# get rid of a few strange duplicates
u = unique(u)
stopifnot(!duplicated(u$id))

write_csv(u, "data/users.csv")
cat(date(), ":", nrow(u), "users in dataset.\n")
