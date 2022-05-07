
#' Connection Function
#'
#' This function connects the user to the Twitter API, using user-provided keys
#' @keywords connect
#' @export
#' @examples
#' connect()
connect <- function() {
  app_name <- readline(prompt = "Enter the name of your Twitter application: ")
  consumer_key <- readline(prompt = "Enter your application API key: ")
  consumer_secret <- readline(prompt = "Enter your application secret API key: ")
  access_token <- readline(prompt = "Enter your Twitter-provided access token: ")
  access_secret <- readline(prompt = "Enter your Twitter-provided access secret key: ")
  message("Please wait...")
  rtweet::create_token(app = app_name, consumer_key, consumer_secret, access_token, access_secret, set_renv = T)
  message("Connected!")
}

#' Single Hydration Function
#'
#' Takes a single argument, a Twitter status ID, and provides all available data on that Tweet.
#' @param x A character string representing a single Twitter status ID
#' @export
#' @examples
#' hydrate()

hydrate <- function(x) {
  status_id <- x
  tweet <- rtweet::lookup_statuses(x, parse = TRUE)
  tweet <<- tweet
}

#' List Prepration Function
#'
#' Takes a vector as argument and transforms it into a character vector of Twitter status IDs
#' @param x A data frame, tibble, or list vector of one variable, consisting of Twitter status IDs
#' @export
#' @examples
#' list_prep()

list_prep <- function(x) {
  x <- as.character(x)
  x <- gsub('[[:alpha:]]+', '', x)
  tweet_list <<- x
}

#' Hydrate List Function
#'
#' Takes a character vector of Twitter status IDs, such as the output of list_prep(), and hydrates them. Outputs to a data frame.
#' @param x A character vector of Twitter status IDs, such as the output of list_prep()
#' @export
#' @examples
#' hydrate_list()

hydrate_list <- function(x) {
    total <- vector()
    for(i in 1:length(x)) {
      ongoing <- rtweet::lookup_statuses(statuses = x[i], parse = TRUE)
      total <- rbind(total, ongoing)
    }
    tweets <<- total
}

