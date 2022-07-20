# At the beginning of any R session, record your AWS database password:
# source("setAWSPassword.R")

source("usePackages.R")
loadPkgs(c("shiny","DBI","stringr"))

getAWSConnection <- function(){
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "project008",
    host = "esddbinstance-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
    username = "project008",
    password = getOption("AWSPassword"))
  conn
}

# Using mine (Robin's) database to test,
getAWSConnection_robin <- function(){
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "student049",
    host = "esddbinstance-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
    username = "student049",
    password = getOption("AWSPassword"))
  conn
}

# WORKING
# Input: (playername, cash-on-hand) 
# Update cash-on-hand as scores to our database
update_records <- function(playername, cash_on_hand) {
  conn <- getAWSConnection_robin()
  querytemplate <- "INSERT INTO LeaderScore_test (playername, cash_on_hand, asoftime) VALUES (?id1, ?id2, NOW())"
  query <- sqlInterpolate(conn, querytemplate,id1 = playername,id2 = cash_on_hand)
  print(query) #for debug
  success <- FALSE
  tryCatch(
    {  # This is not a SELECT query so we use dbExecute
      result <- dbExecute(conn,query)
      print("Score published")
      success <- TRUE
    }, error = function(cond){print("publishScore: ERROR")
      print(cond)
    }, 
    warning = function(cond){print("publishScore: WARNING")
      print(cond)},
    finally = {}
  )
  dbDisconnect(conn)
}

### Testing update_records() ###
test_playername <- "test1"
test_cash_on_hand <- 10800
update_records(test_playername, test_cash_on_hand)
### Test End ###

# WORKING
# publish_leaderboard() 
# Input: none
# Output:leaderboard
publish_leaderboard <- function() {
  conn <- getAWSConnection_robin()
  # Extract columns playername and cash_on_hand
  # ORDER BY cash_on_hand_DESC
  # Show just the top 5 scores
  result <- dbGetQuery(conn, "SELECT playername, cash_on_hand FROM LeaderScore_test ORDER BY cash_on_hand DESC LIMIT 5")
  dbDisconnect(conn)
  
  # return the leader board
  result
}

### Testing publish_leaderboard() ###
publish_leaderboard()
### Test End ###


# clear_record(if restart) 
# input: (playername) 
# clear the records of the player in our database
clear_record <- function(playername) {
  conn <- getAWSConnection_robin()
  query <- paste0("DELETE FROM LeaderScore_test WHERE playername = '", playername, "'")
  dbGetQuery(conn, query)
  dbDisconnect(conn)
}

### Testing publish_leaderboard() ###
test6 <- "test6"
clear_record(test6)
### Test End ###