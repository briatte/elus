#==============================================================================
#
# 02_followers.r -- download lists of Twitter users who follow politicians
#
# The script selects all users who follow 4+ of these accounts to create the
# [ 729846 followers x 1167 politicians ] adjacency matrix, and sets the
# starting values for the model based on the politicians' party affiliation
# (-1 for leftwing, +1 for rightwing, 0 for independents).
#
# The complete follower lists of all politicians are stored in the "followers"
# folder as .rda files, selected followers are saved to model/userlist.rda,
# the adjacency matrix is saved to model/matrix.rda, and the starting values of
# politicians are saved to model/startingvalues.rda.
#
# Author: Pablo Barberá, with minor edits
# Source: https://github.com/pablobarbera/twitter_ideology/tree/master/code
#
#==============================================================================

source("00_functions.r")

dir.create("followers", showWarnings = FALSE)
dir.create("model", showWarnings = FALSE)

library(dplyr)
library(readr)

# list of Twitter accounts
d = read_csv("data/politicians.csv", col_types = list(id = col_character()))
accounts = na.omit(d$twitter)

#==============================================================================
# DOWNLOAD FOLLOWER LISTS
#==============================================================================

# removing those that we already did (downloaded to "followers/")
accounts.done = list.files("followers")
accounts.left = accounts[ !accounts %in% gsub("\\.rda$", "", accounts.done) ]
accounts.left = accounts.left[ !is.na(accounts.left) ]

# loop over each account
while(length(accounts.left) > 0) {

  # sample randomly one account to get followers
  new.user = sample(accounts.left, 1)
  cat(new.user, " -- ", length(accounts.left), " accounts left!\n")

  # download followers (with some exception handling...)
  error = tryCatch(followers <- getFollowers(screen_name = new.user,
                                             oauth_folder = "credentials"), error = function(e) e)

  if(inherits(error, 'error')) {

    cat("\nERROR! On to the next one...\n")
    next

  }

  # save to file and remove from lists of "accounts.left"
  file.name = paste0("followers/", new.user, ".rda")
  save(followers, file = file.name)
  accounts.left = accounts.left[ -which(accounts.left %in% new.user) ]

}

#==============================================================================
# SELECT FOLLOWERS WHO FOLLOW 4+ POLITICIANS
#==============================================================================

filesList = list.files("followers", full.names = TRUE)
followers_m = list(NULL)

# sanity check: all followers files match sampled politicians
# filesList[ !gsub("followers/|\\.rda", "", filesList) %in% d$twitter ]
stopifnot(gsub("followers/|\\.rda", "", filesList) %in% d$twitter)

for(i in length(filesList):1) {

  load(filesList[i])
  followers_m[[i]] = followers

}

mean(sapply(followers_m, length)) # ~ 11,000
median(sapply(followers_m, length)) # ~ 1,350

# re-save politicians dataset with followers counts
data.frame(twitter = gsub("(.*)/(.*).rda", "\\2", filesList),
           followers_sample = sapply(followers_m, length), stringsAsFactors = FALSE) %>%
  full_join(d, ., by = "twitter") %>%
  arrange(-followers) %>%
  write_csv(., "data/politicians.csv")

all_users_list = unlist(followers_m)

cat("- Users following at least 1 of", length(followers_m), "accounts:",
    n_distinct(all_users_list), "\n") # 3.17 million

userlistfollowers = table(all_users_list)

quantile(as.vector(userlistfollowers), .99) # top 1% follows 45+ accounts
summary(as.vector(userlistfollowers[ userlistfollowers < 45 ])) # mean ~ 3
summary(as.vector(userlistfollowers)) # mean ~ 4

options(scipen = 30)
names(userlistfollowers) = as.numeric(names(userlistfollowers))

# option 1: users following 4+ politicians ~ 730,000
length(userlistfollowers[ userlistfollowers >= 4 ])

# option 2: users following 3 to 43 politicians ~ 943,000
length(userlistfollowers[ userlistfollowers >= 3 & userlistfollowers < 45 ])

# option 3: users following 4 to 43 politicians ~ 698,000
length(userlistfollowers[ userlistfollowers >= 4 & userlistfollowers < 45 ])

# using option 1 (equivalent to Barberá's)
userlistfollowers = userlistfollowers[ userlistfollowers >= 4 ]

cat("- Users following at least 4 of", length(followers_m), "accounts:",
    n_distinct(names(userlistfollowers)), "\n")

userlist = names(userlistfollowers)
save(followers_m, filesList, userlist, file = "model/userlist.rda")

#==============================================================================
# CENSUS (POLITICIANS): M = 1,167
#==============================================================================

census = gsub("followers/|\\.rda$", "", filesList)
m = length(census)

#==============================================================================
# USERS (FOLLOWERS): N = 729,670
#==============================================================================

users = as.character(userlist)
n = length(users)

#==============================================================================
# COMPLETE N x M MATRIX
#==============================================================================

cat("- Creating matrix of", n, "followers x", m, "politicians\n")
y = matrix(NA, nrow = n, ncol = m)
rownames(y) = users
colnames(y) = census

# filling in with zeros and ones
pb = txtProgressBar(min = 1, max = m)
for (j in 1:m) {

  load(paste(getwd(), "/followers/", census[ j ], ".rda", sep=""))
  y[, j ] = (users %in% followers) * 1
  setTxtProgressBar(pb, j)

}

users = rownames(y)
n = length(users)

save(y, users, n, file = "model/matrix.rda")

#==============================================================================
# CHOOSE STARTING VALUES
#==============================================================================

d = merge(data.frame(twitter = census, stringsAsFactors = FALSE),
          d, sort = FALSE)

# -1 for leftwing, +1 for rightwing, 0 for independent
start.phi = ifelse(d$party %in% c("FDG", "EELV", "DVG", "PRG", "PS"), -1, 1)
start.phi[ d$party == "IND" ] = 0
names(start.phi) = d$twitter

cat("-", sum(start.phi == -1), "starting values at -1 (left-wing)\n")
cat("-", sum(start.phi == 1), "starting values at +1 (right-wing)\n")
cat("-", sum(start.phi == 0), "starting values at 0 (independent)\n")

save(start.phi, file = "model/startingvalues.rda")

rm(list = ls())
gc()
