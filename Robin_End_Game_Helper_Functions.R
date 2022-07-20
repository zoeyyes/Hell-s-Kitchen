# At the beginning of any R session, record your AWS database password:
source("setAWSPassword.R")

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

# update_records()
# Input: (playername, cash-on-hand) 
# Update cash-on-hand as scores to our database
update_records <- function(playerid, cash_on_hand) {
  conn <- getAWSConnection_robin()
  querytemplate <- "INSERT INTO LeaderScore_test (playerid, cash_on_hand, asoftime) VALUES (?id1, ?id2, NOW())"
  query <- sqlInterpolate(conn, querytemplate,id1 = playerid,id2 = cash_on_hand)
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

# publish_leaderboard() 
# Input: (playerid) 
# Output:leaderboard 



# clear_record(if restart) 
# input:(playername) 
# clear the records of the player in our database 