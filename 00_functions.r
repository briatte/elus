#==============================================================================
#
# 00_functions.r -- functions to download information on Twitter users
#
# Author: Pablo Barbera, with minor edits
# Source: https://github.com/pablobarbera/twitter_ideology/tree/master/code
#
#==============================================================================

require(rjson)
require(ROAuth)

#' getFollowers
#'
#' Get list of Twitter followers.
#' @author Pablo Barberá, with minor edits
#' @source https://github.com/pablobarbera/twitter_ideology/
getFollowers <- function(screen_name, oauth_folder, cursor = -1) {

  # open a random credential
  creds <- list.files(oauth_folder, full.names = TRUE)
  cr <- sample(creds, 1)

  cat(cr, "\n")
  load(cr)

  # while rate limit is 0, open a new one
  limit <- getFollowersLimit(my_oauth)
  cat(limit, " API calls left\n")

  while(limit == 0) {

    cr <- sample(creds, 1)
    cat(cr, "\n")
    load(cr)
    Sys.sleep(1)

    # sleep for 5 minutes if limit rate is less than 100
    rate.limit <- getFollowersLimitRate(my_oauth)
    if (rate.limit < 100){
      for(i in 5:1) {
        cat("Waiting", i, "min. (API rate limit)...\n")
        Sys.sleep(60)
      }
    }
    limit <- getFollowersLimit(my_oauth)
    cat(limit, " API calls left\n")

  }

  # url to call
  url <- "https://api.twitter.com/1.1/followers/ids.json"

  # empty list for followers
  followers <- c()

  # while there's more data to download...
  while(cursor != 0) {

    # making API call
    params <- list(screen_name = screen_name, cursor = cursor)
    url.data <- my_oauth$OAuthRequest(URL = url, params = params, method = "GET",
                                      cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    Sys.sleep(1)

    # one API call less
    limit <- limit - 1

    # trying to parse JSON data
    json.data <- fromJSON(url.data, unexpected.escape = "skip")
    if(length(json.data$error) != 0) {
      cat(url.data)
      stop("error! Last cursor: ", cursor)
    }

    # adding new IDS
    followers <- c(followers, as.character(json.data$ids))

    # previous cursor
    prev_cursor <- json.data$previous_cursor_str
    # next cursor
    cursor <- json.data$next_cursor_str
    # giving info
    cat(length(followers), "followers. Next cursor: ", cursor, "\n")

    # changing oauth token if we hit the limit
    cat(limit, " API calls left\n")
    while(limit == 0) {

      cr <- sample(creds, 1)
      cat(cr, "\n")
      load(cr)
      Sys.sleep(1)

      # sleep for 5 minutes if limit rate is less than 100
      rate.limit <- getFollowersLimitRate(my_oauth)
      if(rate.limit < 100) {
        Sys.sleep(300)
      }
      limit <- getFollowersLimit(my_oauth)
      cat(limit, " API calls left\n")

    }
  }

  return(followers)

}

#' getFollowersLimitRate
#'
#' Monitor API limit rate while getting followers.
#' @author Pablo Barberá, with minor edits
#' @source https://github.com/pablobarbera/twitter_ideology/
getFollowersLimitRate <- function(my_oauth) {

  url = "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params = list(resources = "followers,application")
  response = my_oauth$OAuthRequest(URL = url, params = params, method = "GET",
                                   cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  return(unlist(fromJSON(response)$resources$application$`/application/rate_limit_status`[['remaining']]))

}

#' getFollowersLimit
#'
#' Monitor API limit while getting followers.
#' @author Pablo Barberá, with minor edits
#' @source https://github.com/pablobarbera/twitter_ideology/
getFollowersLimit <- function(my_oauth) {

  url = "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params = list(resources = "followers,application")
  response = my_oauth$OAuthRequest(URL = url, params = params, method = "GET",
                                   cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  return(unlist(fromJSON(response)$resources$followers$`/followers/ids`[['remaining']]))

}

#' getUsers
#'
#' Get Twitter user information.
#' @author Pablo Barberá, with minor edits
#' @source https://github.com/pablobarbera/twitter_ideology/
getUsers <- function(user_id = NULL, screen_name = NULL, oauth_folder) {

  # open a random credential
  creds = list.files(oauth_folder, full.names = TRUE)
  cr = sample(creds, 1)

  load(cr)
  Sys.sleep(1) # be patient

  # while rate limit is close to 0, open a new one
  limit = getUsersLimit(my_oauth)

  if(!limit %% 10)
    cat(cr,":", limit, "API calls left\n")

  while(limit < 5) {

    cr = sample(creds, 1)
    cat(cr)
    load(cr)

    rate.limit = getUserLimitRate(my_oauth)
    cat(": API rate limit at", rate.limit, "\n")
    Sys.sleep(1)

    # sleep for 5 minutes if limit rate is less than 100
    if(rate.limit < 100) {

      for(i in 5:1) {
        cat("Waiting", i, "min. (API rate limit)...\n")
        Sys.sleep(60)
      }

    }

    limit <- getUsersLimit(my_oauth)
    cat(limit, "API calls left\n")

  }

  # url to call
  url = "https://api.twitter.com/1.1/users/show.json"

  if(!is.null(user_id)) # when parsing users
    params = list(user_id = user_id)

  if(!is.null(screen_name)) # when parsing politicians
    params = list(screen_name = screen_name)

  stopifnot(exists("params")) # sanity check

  url.data = my_oauth$OAuthRequest(URL = url, params = params, method = "GET",
                                   cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  json.data = fromJSON(url.data, unexpected.escape = "skip")

  return(json.data)

}

#' getUserLimitRate
#'
#' Monitor API limit rate while getting users.
#' @author Pablo Barberá, with minor edits
#' @source https://github.com/pablobarbera/twitter_ideology/
getUserLimitRate <- function(my_oauth) {

  url = "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params = list(resources = "users,application")
  response = my_oauth$OAuthRequest(URL = url, params = params, method = "GET",
                                   cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  return(unlist(fromJSON(response)$resources$application$`/application/rate_limit_status`[['remaining']]))

}

#' getUsersLimit
#'
#' Monitor API limit while getting users.
#' @author Pablo Barberá, with minor edits
#' @source https://github.com/pablobarbera/twitter_ideology/
getUsersLimit <- function(my_oauth) {

  url = "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params = list(resources = "users,application")
  response = my_oauth$OAuthRequest(URL = url, params = params, method = "GET",
                                   cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  return(unlist(fromJSON(response)$resources$users$`/users/show/:id`[['remaining']]))

}

#' getTweets
#'
#' Get tweets.
getTweets <- function(user_id, oauth_folder, max_id = NULL, since_id = NULL) {

  # open a random credential
  creds = list.files(oauth_folder, full.names = TRUE)
  cr = sample(creds, 1)

  load(cr)
  Sys.sleep(1) # be patient

  # while rate limit is close to 0, open a new one
  limit = getTweetsLimit(my_oauth)

  if(!limit %% 10)
    cat(cr,":", limit, "API calls left\n")

  while(limit < 5) {

    cr = sample(creds, 1)
    cat(cr)
    load(cr)

    rate.limit = getTweetsLimitRate(my_oauth)
    cat(": API rate limit at", rate.limit, "\n")
    Sys.sleep(1)

    # sleep for 5 minutes if limit rate is less than 100
    if(rate.limit < 100) {

      for(i in 5:1) {
        cat("Waiting", i, "min. (API rate limit)...\n")
        Sys.sleep(60)
      }

    }

    limit <- getTweetsLimit(my_oauth)
    cat(limit, "API calls left\n")

  }

  # url to call
  url = "https://api.twitter.com/1.1/statuses/user_timeline.json"
  params = list(user_id = user_id, count = 200, trim_user = "true", contributor_details = "true")

  # start further away if requested
  if(!is.null(max_id))
    params = c(params, max_id = max_id)

  # start closer if requested
  if(!is.null(since_id))
    params = c(params, since_id = since_id)

  url.data = my_oauth$OAuthRequest(URL = url, params = params, method = "GET",
                                   cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  json.data = fromJSON(url.data, unexpected.escape = "skip")

  return(json.data)

}

#' getTweetsLimit
#'
#' Monitor API limit while getting tweets.
getTweetsLimit <- function(my_oauth) {

  url = "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params = list(resources = "statuses,application")
  response = my_oauth$OAuthRequest(URL = url, params = params, method = "GET",
                                   cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  return(unlist(fromJSON(response)$resources$statuses$`/statuses/user_timeline`[['remaining']]))

}

#' getTweetsLimitRate
#'
#' Monitor API limit rate while getting tweets.
getTweetsLimitRate <- function(my_oauth) {

  url = "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params = list(resources = "statuses,application")
  response = my_oauth$OAuthRequest(URL = url, params = params, method = "GET",
                                   cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  return(unlist(fromJSON(response)$resources$application$`/application/rate_limit_status`[['remaining']]))

}

#' get_nfo
#'
#' Retrieve information on Twitter account.
get_nfo <- function(z, y) {

  y = sapply(z, function(x) x[y])
  return(get_vector(y))

}

#' get_status
#'
#' Retrieve information on last Twitter status.
get_status <- function(z, y) {

  y = sapply(z, function(x) x$status[y])
  return(get_vector(y))

}

#' get_vector
#'
#' Get a vector out of Twitter account fields.
get_vector <- function(x) {

  x[ sapply(x, is.null) ] = NA
  x = unlist(x)
  x[ x == "" ] = NA
  return(x)

}
