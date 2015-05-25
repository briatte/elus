#==============================================================================
#
# 06_tweets.r -- download tweets from selected Twitter users
#
# The script gets the last 200 tweets from every user included in the matrix
# that is later sent to the model (model/matrix_selected.rda). The tweets are
# stored as .rda files in the "tweets" folder.
#
# The tweets are then parsed to CSV files that are saved to the "tweets-csv"
# folder. The CSV files contain one "mention" column for each political party,
# with the names of any politician from that party mentioned in the tweet.
#
# Re-running the script will append more tweets to the users for whom all tweets
# are more recent than February 22, 2015 (one month before the last local French
# election). Remember to edit that date to fit your own needs.
#
#==============================================================================

library(dplyr)
library(readr)
library(stringr)

source("00_functions.r")

p = read_csv("data/politicians.csv", col_types = list(id = col_character()))

u = read_csv("data/users.csv", col_types = list(id = col_character()))
y = na.omit(u$id[ u$sample ])

cat("Model matrix contains", length(y), "users\n")

# # sanity check: all model users were parsed
u = read_csv("data/users.csv", col_types = list(id = col_character()))
u = filter(select(u, id, age, gender), id %in% y)

stopifnot(y %in% u$id) # all found

dir.create("tweets", showWarnings = FALSE)
dir.create("tweets-csv", showWarnings = FALSE)

#==============================================================================
# DOWNLOAD TWEETS
#==============================================================================

# # uncomment to scrape only completely new users
# yy = sample(y[ !file.exists(paste0("tweets/", y, ".rda")) ])
yy = sample(y)

cat(length(yy), "left!\n")

# failed users
if(!file.exists("tweets_failed.rda")) {

  failed = c()
  save(failed, file = "tweets_failed.rda")

} else {

  load("tweets_failed.rda")
  cat("Removing", sum(y %in% failed), "users who permanently failed\n")

  yy = yy[ !yy %in% failed ]

}

# download loop
for(i in rev(yy)) {

  j = which(yy == i)

  if(!file.exists(paste0("tweets/", i, ".rda"))) {

    # download tweets (with some exception handling...)
    error = tryCatch(tweets <- getTweets(user_id = i,
                                         oauth_folder = "credentials"),
                     error = function(e) e)

    if(inherits(error, 'error')) {

      cat(sprintf("%5.0f", j), sprintf("%10s", i), ":", error$message, "\n")

      if(grepl("Authorization|unclosed|unexpected", error$message)) {

        failed = unique(c(failed, i))
        cat(length(failed), "users have permanently failed\n")

        save(failed, file = "tweets_failed.rda")

      }

      Sys.sleep(1)

    } else {

      save(tweets, file = paste0("tweets/", i, ".rda")) # raw JSON

    }

  }

  if(file.exists(paste0("tweets/", i, ".rda"))) {

    load(paste0("tweets/", i, ".rda"))

    cat(sprintf("%5.0f", j), sprintf("%10s", i), ":", length(tweets), "tweets")

    if(!length(tweets)) {
      cat(", skipping\n")
      next
    }

    dates = get_nfo(tweets, "created_at")
    dates = as.POSIXct(dates, format = "%a %b %d %H:%M:%S %z %Y")
    dates = as.Date(dates)

    older = dates[ dates < as.Date("2015-02-22") ]
    cat(ifelse(length(older), ", covered",
               paste0(", oldest: ", dates[ which.min(dates) ])), "\n")

    # # note: appending might not work properly if the tweets are very old
    # next

    # append if no tweet is older than one month before first round
    if(!length(older)) {

      older = get_nfo(tweets, "id_str")[ which.min(dates) ]

      error = tryCatch(add <- getTweets(user_id = i,
                                        oauth_folder = "credentials",
                                        max_id = older),
                       error = function(e) e)

      if(inherits(error, 'error')) {

        cat(sprintf("%5.0f", j), sprintf("%10s", i), ":", error$message, "\n")

        if(grepl("Authorization|unclosed|unexpected", error$message)) {

          failed = unique(c(failed, i))
          cat(length(failed), "users have permanently failed\n")

          save(failed, file = "tweets_failed.rda")

        }

        Sys.sleep(1)

      } else {

        ids = get_nfo(add, "id_str")
        ids = which(!ids %in% get_nfo(tweets, "id_str"))
        tweets = c(tweets, add[ ids ])

        dates = get_nfo(tweets, "created_at")
        dates = as.POSIXct(dates, format = "%a %b %d %H:%M:%S %z %Y")
        dates = as.Date(dates)

        cat(sprintf("%5.0f", j), sprintf("%10s", i), ":", length(tweets),
            "tweets, oldest:", as.character(dates[ which.min(dates) ]),
            "(appended", length(add[ ids ]), "tweets)\n")

        # save only if some new tweets were appended
        if(length(add[ ids ]) > 0)
          save(tweets, file = paste0("tweets/", i, ".rda"))

      }

    }

  }

}

#==============================================================================
# PARSE TWEETS FOLDER
#==============================================================================

files = dir("tweets", pattern = "rda$", full.names = TRUE)
files = files[ gsub("tweets/|\\.rda", "", files) %in% y ]

for(i in rev(files)) {

  file = gsub("rda$", "csv", gsub("tweets", "/Volumes/OHM/tweets-csv", i))

  # skip CSV files that are more recent than their tweets file
  if(!is.na(file.info(file)$mtime)) {

    if(file.info(file)$ctime > file.info(i)$mtime)
      next

  }

  # skip empty files
  if(!file.info(i)$size)
    next

  cat(sprintf("%5.0f", which(files == i)), "Parsing user",
      sprintf("%10.0f", as.integer(gsub("\\D", "", i))))

  load(i)

  cat(":", length(tweets), "tweets\n")
  if(length(tweets) > 0) {

    tweets = data.frame(id = gsub("tweets/|\\.rda", "", i),
                        date = get_nfo(tweets, "created_at"),
                        text = get_nfo(tweets, "text"),
                        reply = sapply(tweets, function(x) {
                          y = x$in_reply_to_screen_name
                          !is.null(y)
                        }),
                        retweet = sapply(tweets, function(x) {
                          y = x$retweeted_status$id_str
                          !is.null(y)
                        }),
                        retweets = get_nfo(tweets, "retweet_count"),
                        stringsAsFactors = FALSE)

    # find mentions of sampled politicians
    for(j in unique(p$party)) {

      pol = regex(paste0("@", p$twitter[ p$party == j ], collapse = "|"), ignore_case = TRUE)
      pol = sapply(str_extract_all(tweets$text, pol), function(x) paste0(unique(x), collapse = ","))
      pol = tolower(gsub("@", "", pol))
      tweets[, paste0("mentions_", tolower(j)) ] = pol

    }

    write_csv(tweets, file)

  }

}

#==============================================================================
# SAVE TWEETS OBJECT
#==============================================================================

files = gsub("tweets", "tweets-csv", gsub("rda$", "csv", files))
files = paste0("tweets-csv/", y, ".csv")
files = files[ file.exists(files) ]

t = lapply(files, read_csv, col_types = "ccclliccccccccccc") %>%
  bind_rows

t$date = as.POSIXct(t$date, format = "%a %b %d %H:%M:%S %z %Y")
t$date = as.Date(t$date)

save(t, file = "tweets.rda")

rm(list = ls())
gc()
