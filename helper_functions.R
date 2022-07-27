#tmB4qqNs
source("setAWSPassword.R")
source("usePackages.R")
loadPkgs(c("tidyverse","shiny","DBI"))
library(Matrix)
library(broom.mixed)

#----------------------------login-------------------------------------------
passwordModal <- function(failed = FALSE) {
  modalDialog(
    title = 'Sign up',
    textInput('playername','Enter your unique username:'),
    passwordInput("password1", "Enter a new password:"),
    passwordInput("password2", "Confirm by re-entering the new password:"),
    "If successful, you will log in with this username and password.",
    if (failed)
      div(tags$b("Registration failed. Try again.", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("passwordok", "OK")
    )
  )
}

warningModel <- function(){
  modalDialog(
    title='WARNING',
    "Your order cost is more than your cash_on_hand,please renter your inventory",
    footer = tagList(
      modalButton('OK')
    )
  )
}
reEnterPasswordModal<-function(failed=FALSE,playername){
  modalDialog(
    title = paste('Re-enter your current password,',playername),
    passwordInput('currentPassword',"Enter your current password:"),
    if (failed)
      div(tags$b("The password is wrong. Try again.", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("oldpasswordok", "OK")
    )
  )
}

UpdatePasswordModal <- function(failed = FALSE) {
  modalDialog(
    title = "Change your password",
    passwordInput("newpassword1", "Enter a new password:"),
    passwordInput("newpassword2", "Confirm by re-entering the new password:"),
    "If successful, you will be able to re-login using your new password.",
    if (failed)
      div(tags$b("The passwords do not match. Try again.", style = "color: red;")),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("newpasswordok", "OK")
    )
  )
}

acknowledgeModel<-function(){
  modalDialog(
    title = 'Acknowledgement',
    'You have successfully changed your password.',
    footer = tagList(
      modalButton('OK')
    )
  )
}

loginModal <- function(failed = FALSE) {
  modalDialog(
    title = "Login",
    textInput("playername", "Enter your Playername", "ABC"),
    passwordInput("password3", "Enter your password:"),
    if (failed)
      div(tags$b("There is no registered player with that name and password. Try again or re-register.", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("loginok", "OK")
    )
  )
}

updatePassword<-function(playername,newpassword){
  conn<-getAWSConnection()
  querytemplate<-'Update Player SET password=?id1 WHERE Username=?id2;'
  query<- sqlInterpolate(conn, querytemplate,id2=playername,id1=newpassword)
  print(paste0('update',query))
  result <- dbGetQuery(conn,query)
  dbDisconnect(conn)
}

getAWSConnection <- function(){
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "project008",
    host = "esddbinstance-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
    username = "project008",
    password = getOption("AWSPassword"))
  conn
}

getPlayerID <- function(playername,password){
  
  #open the connection
  conn <- getAWSConnection()
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholders for playername and password
  querytemplate <- "SELECT * FROM Player WHERE Username=?id1 AND password=?id2;"
  query<- sqlInterpolate(conn, querytemplate,id1=playername,id2=password)
  print(query) #for debug
  result <- dbGetQuery(conn,query)
  print(result)
  # If the query is successful, result should be a dataframe with one row
  if (nrow(result)==1){
    playerid <- result$id[1]
  } else {
    print(result) #for debugging
    playerid <- 0
  }
  #print(result)
  #print(playerid)
  #Close the connection
  dbDisconnect(conn)
  # return the playerid
  playerid
}

createNewPlayerQuery <- function(conn,playername,password){
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholder for  password
  querytemplate <- "INSERT INTO Player (Username,password) VALUES (?id1,?id2);"
  query<- sqlInterpolate(conn, querytemplate,id1=playername,id2=password)
}

registerPlayer <- function(playername,password){
  #open the connection
  conn <- getAWSConnection()
  
  query <- createNewPlayerQuery(conn,playername,password)
  # This query could fail to run properly so we wrap it in a loop with tryCatch()
  success <- FALSE
  iter <- 0
  MAXITER <- 5
  while(!success & iter < MAXITER){
    iter <- iter+1
    tryCatch(
      {  # This is not a SELECT query so we use dbExecute
        result <- dbExecute(conn,query)
        success <- TRUE
      }, error=function(cond){print("registerPlayer: ERROR")
        print(cond)
        # The query failed, likely because of a duplicate playername
        query <- createNewPlayerQuery(conn,playername,password) }, 
      
      warning=function(cond){print("registerPlayer: WARNING")
        print(cond)},
      finally = {print(paste0("Iteration ",iter," done."))
      }
    )
  } # end while loop
  # This may not have been successful
  if (!success) playername = NULL
  #Close the connection
  dbDisconnect(conn)
  playername
}

publishScore <- function(playerid,score){
  conn <- getAWSConnection()
  querytemplate <- "INSERT INTO Score (id,asoftime,score) VALUES (?id1,NOW(),?id2)"
  query <- sqlInterpolate(conn, querytemplate,id1=playerid,id2=score)
  #print(query) #for debug
  success <- FALSE
  tryCatch(
    {  # This is not a SELECT query so we use dbExecute
      result <- dbExecute(conn,query)
      print("Score published")
      success <- TRUE
    }, error=function(cond){print("publishScore: ERROR")
      print(cond)
    }, 
    warning=function(cond){print("publishScore: WARNING")
      print(cond)},
    finally = {}
  )
  dbDisconnect(conn)
}

getLeaderBoard <- function(){
  conn <- getAWSConnection()

  #Assemble the query for this gamevariantid
  query <- "SELECT Player.Username,Score.score,Score.asoftime  FROM Score INNER JOIN Player"
  query <- paste0(query," ON (ls.playerid=lp.playerid) ORDER BY ls.score DESC,ls.asoftime ASC")
  
  print(query) # for debugging
  result <- dbGetQuery(conn,query)
  dbDisconnect(conn)
  result
}

#----------------------------game----------------------------------------------

generate_random_demand <- function(round_number, demand_df) {
  
  # For Monday to Thursday of every round,
  for (day in (7 * round_number - 6):(7 * round_number - 3)) {
    
    # Generate demand for Dish 1: Mixed Vegetable Rice Set A
    mixed_veg_rice_set_A <- round(rnorm(1, mean = 40, sd = 5))
    while (mixed_veg_rice_set_A <= 0) {
      mixed_veg_rice_set_A <- round(rnorm(1, mean = 40, sd = 5))
    }
    # print(mixed_veg_rice_set_A) # For debugging
    demand_df[demand_df$Day == day, 'Mixed_Vegetable_Rice_Set_A'] = mixed_veg_rice_set_A
    # print(demand_df) # For debugging
    
    # Generate demand for Dish 2: Mixed Vegetable Rice Set B
    
    mixed_veg_rice_set_B <- round(rnorm(1, mean = 40, sd = 5))
    while (mixed_veg_rice_set_B <= 0) {
      mixed_veg_rice_set_B <- round(rnorm(1, mean = 40, sd = 5))
    }
      demand_df[demand_df$Day == day, 'Mixed_Vegetable_Rice_Set_B'] = mixed_veg_rice_set_B
    
  }
  
  # For Friday and Saturday of every round,
  for (day in (7 * round_number - 2):(7 * round_number)) {
    
    # Generate demand for Dish 1: Mixed Vegetable Rice Set A
    mixed_veg_rice_set_A <- round(rnorm(1, mean = 80, sd = 10))
    while (mixed_veg_rice_set_A <= 0) {
      mixed_veg_rice_set_A <- round(rnorm(1, mean = 80, sd = 10))
    }
    # print(mixed_veg_rice_set_A) # For debugging
    demand_df[demand_df$Day == day, 'Mixed_Vegetable_Rice_Set_A'] = mixed_veg_rice_set_A
    # print(demand_df) # For debugging
    
    # Generate demand for Dish 2: Mixed Vegetable Rice Set B
    
    mixed_veg_rice_set_B <- round(rnorm(1, mean = 80, sd = 10))
    
    while (mixed_veg_rice_set_B <= 0) {
      mixed_veg_rice_set_B <- round(rnorm(1, mean = 80, sd = 10))
      }
    demand_df[demand_df$Day == day, 'Mixed_Vegetable_Rice_Set_B'] = mixed_veg_rice_set_B
    }
  
  
  # Return the update demand data frame
  demand_df
  
}

#oderplan df:
#    chicken pork noodle rice vegetable
# 1 | num    num   num   num   num
# 2 | num    num   num   num   num
form_orderplan_df<-function(df,chicken,pork,noodle,rice,vegetable){
  df[nrow(df)+1,]<-c(chicken,pork,noodle,rice,vegetable)
  df
}



calculate_orderingcost<-function(round,orderplan,stats){
#seq: chicken pork noodle rice vegetable
    unit_price<-c(5,5,2,2,4)
    ordercost<-sum(unit_price*orderplan[round,])
    stats[stats$Day==round*7-6,"Ordering_cost"]<-ordercost
  stats
}

update_storage_used<-function(round,orderplan,stats){
  if(round==1){
    stats[stats$Day==0,'Chicken']<-orderplan[round,'chicken']
    stats[stats$Day==0,'Pork']<-orderplan[round,'pork']
    stats[stats$Day==0,'Noodles']<-orderplan[round,'noodles']
    stats[stats$Day==0,'Vegetables']<-orderplan[round,'vegetables']
    stats[stats$Day==0,'Rice']<-orderplan[round,'rice']
    print(stats[stats$Day==0,'Chicken'])
    print(stats[stats$Day==0,'Rice'])
    stats
    }else{
    purchase_day<-round*7-6
    stats[stats$Day==(purchase_day-1),'Chicken']<-stats[stats$Day==(purchase_day-1),'Chicken']+orderplan[round,'chicken']
    stats[stats$Day==(purchase_day-1),'Pork']<-stats[stats$Day==(purchase_day-1),'Pork']+orderplan[round,'pork']
    stats[stats$Day==(purchase_day-1),'Noodles']<-stats[stats$Day==(purchase_day-1),'Noodles']+orderplan[round,'noodles']
    stats[stats$Day==(purchase_day-1),'Vegetables']<-stats[stats$Day==(purchase_day-1),'Vegetables']+orderplan[round,'vegetables']
    stats[stats$Day==(purchase_day-1),'Rice']<-stats[stats$Day==(purchase_day-1),'Rice']+orderplan[round,'rice']
    stats
  }
  
  
}

# WORKING
# This function updates the amount of remaining ingredients as the week plays on.
# HOW TO USE: stats_test_df <- calculate_consumptions(1, stats_test_df, demand_test_df)
# This will update your stats data frame every day in Round 1 (for the example above).
# Would recommend storing the round number as a reactive value.
calculate_consumption <- function(round_number, stats_df, demand_df) {
  
  for (day in (7 * round_number - 6):(7 * round_number)) {
    
   
    # Find the max number of Cai Fan Set B that can be sold.
    # Priority 2: Sell Cai Fan Set B
    max_no_mixed_veg_rice_set_B <- floor(min(stats_df[stats_df$Day == (day-1), "Noodles"],
                                             (stats_df[stats_df$Day == (day-1), "Chicken"])/ 2,
                                             stats_df[stats_df$Day == (day-1), "Vegetables"], # Based on amount of vegetables left the same day after selling Cai Fan Set A
                                             demand_df[demand_df$Day == day, 'Mixed_Vegetable_Rice_Set_B'] # If there is an excess of ingredients, max_no == demand for the day
    )
    )
    # Store the number of dishes sold in stats (to be retrieved later for calculate_revenues())
    stats_df[stats_df$Day == day, "Mixed_Vegetable_Rice_Set_B_Sold"] <- max_no_mixed_veg_rice_set_B
    
    # Calculate ingredients consumed based on Cai Fan Set A demand
    noodles_consumed <- 1 * max_no_mixed_veg_rice_set_B
    # Update each day's stats based on ingredients consumed
    stats_df[stats_df$Day == day, "Noodles"] <- stats_df[stats_df$Day == (day-1), "Noodles"]- noodles_consumed # max(0, __) is to stop ingredient level from going below 0.
    
    chicken_consumed <- 2 * max_no_mixed_veg_rice_set_B
    stats_df[stats_df$Day == day, "Chicken"] <- stats_df[stats_df$Day == (day-1) , "Chicken"] - chicken_consumed
    
    vegetables_consumed <- 1 * max_no_mixed_veg_rice_set_B
    stats_df[stats_df$Day == day, "Vegetables"] <- stats_df[stats_df$Day == (day-1), "Vegetables"] - vegetables_consumed
    
    # At the end of each day, update Total_storage_used column
    stats_df[stats_df$Day == day, "Total_storage_used"] <- stats_df[stats_df$Day == day, "Rice"] + stats_df[stats_df$Day == day, "Pork"] + stats_df[stats_df$Day == day, "Vegetables"] + stats_df[stats_df$Day == day, "Noodles"] + stats_df[stats_df$Day == day, "Chicken"]
    # Priority 1: Sell Cai Fan Set A
    # Find the max number of Cai Fan Set A that can be sold.
    print("1")
    print(day)
    if (stats_df[stats_df$Day == day, "Vegetables"] == 0){
      max_no_mixed_veg_rice_set_A <- 0
      }else{
      max_no_mixed_veg_rice_set_A <- floor(min(stats_df[stats_df$Day ==(day-1), "Rice"],
                                               stats_df[stats_df$Day ==(day-1), "Pork"],
                                               (stats_df[stats_df$Day ==(day-1), "Vegetables"]) / 2,
                                               demand_df[demand_df$Day == day, 'Mixed_Vegetable_Rice_Set_A'] # If there is an excess of ingredients, max_no == demand for the day
      )
      )
      print("2")
      if (max_no_mixed_veg_rice_set_A<0){max_no_mixed_veg_rice_set_A<-0}
      print('set A max sold')
      print(max_no_mixed_veg_rice_set_A)
      # Store the number of dishes sold in stats (to be retrieved later for calculate_revenues())
      stats_df[stats_df$Day == day, "Mixed_Vegetable_Rice_Set_A_Sold"] <- max_no_mixed_veg_rice_set_A
      
      # Calculate ingredients consumed based on Cai Fan Set A demand
      rice_consumed <- 1 * max_no_mixed_veg_rice_set_A
      # Update each day's stats based on ingredients consumed
      stats_df[stats_df$Day == day, "Rice"] <- stats_df[stats_df$Day == (day-1), "Rice"] - rice_consumed # max(0, __) is to stop ingredient level from going below 0.
      
      pork_consumed <- 1 * max_no_mixed_veg_rice_set_A
      stats_df[stats_df$Day == day, "Pork"] <- stats_df[stats_df$Day == (day-1), "Pork"] - pork_consumed
      
      vegetables_consumed <- 2 * max_no_mixed_veg_rice_set_A
      print(vegetables_consumed)
      stats_df[stats_df$Day == day, "Vegetables"] <- stats_df[stats_df$Day ==(day-1), "Vegetables"] - vegetables_consumed
      
    }
    
  }
  # Return updated stats_df
  stats_df
  # data.frame(c(stats_df, demand_df))
  
}

# Xin Yi's calculate_revenue()
# Robin: Can consider merging this function with calculate_consumption().
calculate_revenue <- function (round_number, stats_df) {
  
  for (day in (7*round_number-6):(7*round_number)){
    # Set the price of each dish
    caifan_A_price <- 5
    caifan_B_price <- 6.5
    stats_df[stats_df$Day == day, "Revenue"] <- stats_df[stats_df$Day == day, "Mixed_Vegetable_Rice_Set_A_Sold"] * caifan_A_price + stats_df[stats_df$Day == day, "Mixed_Vegetable_Rice_Set_B_Sold"] * caifan_B_price
    stats_df[stats_df$Day == day, "Accumulative_Revenue"] <- stats_df[stats_df$Day == (day - 1), "Accumulative_Revenue"] + stats_df[stats_df$Day == day, "Revenue"]
  }
  # Return stats_df
  stats_df
}

calculate_cash_on_hand<-function(round,stats_df){
  
    for (day in (7*round-6):(7*round)){
      stats_df[stats_df$Day == day, "Cash_on_hand"] <-stats_df[stats_df$Day == (day-1),'Cash_on_hand']+stats_df[stats_df$Day == day,'Revenue']-stats_df[stats_df$Day == day,'Ordering_cost']
    }
   stats_df
}

MaxroundModal<-function(failed=FALSE){
  modalDialog(
    if (failed)
      div(tags$b("You have tried 5 rounds.", style = "color: red;")),
    footer = tagList(
      modalButton("OK")
    )
  )
}
#----------------------------testing initialization-------------------------------------------
MAXROUND<-2
Initial_cash_on_hand<-20000

orderplan<-data.frame(matrix(ncol = 5, nrow = 0))
colnames(orderplan)<-c('chicken', 'pork', 'noodles', 'rice', 'vegetables')


stats_test_df <- data.frame(Day = c(seq(0, MAXROUND*7)),
                            Rice = c(rep(0, MAXROUND*7+1)),
                            Pork = c(rep(0, MAXROUND*7+1)),
                            Vegetables = c(rep(0, MAXROUND*7+1)),
                            Noodles = c(rep(0, MAXROUND*7+1)),
                            Chicken = c(rep(0, MAXROUND*7+1)),
                            Total_storage_used = c(rep(0, MAXROUND*7+1)),
                            Mixed_Vegetable_Rice_Set_A_Sold = c(rep(0, MAXROUND*7+1)),
                            Mixed_Vegetable_Rice_Set_B_Sold = c(rep(0, MAXROUND*7+1)),
                            Revenue = c(rep(0, MAXROUND*7+1)),
                            Accumulative_Revenue = c(rep(0, MAXROUND*7+1)),
                            Ordering_cost = c(rep(0, MAXROUND*7+1)),
                            Cash_on_hand = c(rep(0, MAXROUND*7+1)))

demand_test_df <- data.frame(Day = c(seq(1, MAXROUND*7)),
                             Mixed_Vegetable_Rice_Set_A = c(rep(0, MAXROUND*7)),
                             Mixed_Vegetable_Rice_Set_B = c(rep(0, MAXROUND*7)))

#----------------------------testing start (round1)-------------------------------------------
round<-1

#order 1st round
orderplan<-form_orderplan_df(orderplan,500,500,500,200,200)

stats_test_df[stats_test_df$Day==0,"Total_storage_used"]<-sum(orderplan[1,])
stats_test_df[stats_test_df$Day==0,"Cash_on_hand"]<-Initial_cash_on_hand

#transfer inventory to stats_test_df
stats_test_df<-update_storage_used(round,orderplan,stats_test_df)

# Generate demand for Round 1
demand_test_df <- generate_random_demand(round, demand_test_df)

stats_test_df <- calculate_orderingcost(round,orderplan,stats_test_df)


# Deduct inventory stats based on dishes' demand in Round 1 + Update number of each dish sold
stats_test_df <- calculate_consumption(round, stats_test_df, demand_test_df)

# Update revenue columns
stats_test_df <- calculate_revenue(round, stats_test_df)
# Update cash_on_hand
stats_test_df <- calculate_cash_on_hand(round,stats_test_df)


#----------------------------testing start (round2)-------------------------------------------
round<-round+1
orderplan<-form_orderplan_df(orderplan,500,500,500,200,500)

stats_test_df<-update_storage_used(round,orderplan,stats_test_df)

demand_test_df <- generate_random_demand(round, demand_test_df)

stats_test_df <- calculate_orderingcost(round,orderplan,stats_test_df)

stats_test_df <- calculate_consumption(round, stats_test_df, demand_test_df)

stats_test_df <- calculate_revenue(round, stats_test_df)

stats_test_df <- calculate_cash_on_hand(round,stats_test_df)

