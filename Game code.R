### Helper Function
# At the beginning of any R session, record your AWS
source("setAWSPassword.R")
source("usePackages.R")
loadPkgs(c("tidyverse","shiny","DBI"))


#Login/Register/Change_password

#Purchase_ingredients(ensure sufficient cash-on-hand&inventory space)


#Calculate the total ingredient cost 
#Cost --> fixed cost we set
#inventory_quantity --> list of ingredient quality
ingredient_cost <- function(Redmeat,Whitemeat,Rice,Noodle,Vegtetable){
  inventory_quality<- c(Redmeat,Whitemeat,Rice,Noodle,Vegtetable)
  inventory_cost = rowsum(as.data.frame(inventory_quality)*as.data.frame(list(cost=c(1,2,3,4,5))))
}

#Calculate the total ingredient quantity
#current inventory --> inventory on hand
current_inventory <- function(inventory_quantity){
  current_inventory = sum(as.data.frame(inventory_quantity))
}
#Calculate the total revenue 
#Demand need to store as a list
#Dish_cost store as a list
Revenue <- function (dish_cost,demand) {
  for (day in (7*round_number-6):(7*round_number -1)){
    Revenue = rowsum(as.data.frame(list(demand=c(Mixed_Vegetable_Rice_Set_A_Sold,Mixed_Vegetable_Rice_Set_B_Sold)))*as.data.frame(list(dish_cost=c(6.5,5))))
    Revenue
  }
}
  

#Check if qualified to purchase 
#Cash --> Cash-on-hand
#Storage_limit --> Maximum capacity
Check_Purchase <- function(ingredient_cost,current_inventory){
  if((ingredient_cost < cash) && (current_inventory < storage_limit)){}
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

#Update_inventory (amount of inventory left)
##Update_inventory = initial storage - total inventory 
Update_inventory<- function(storage_limit,inventory_quantity){
  conn<-getAWSConnection()
  querytemplate <- ''
}
  


#Update_cash (amount of cash-on-hand)
##Update_cash = initial cash - total cost
Update_cash <- function(cash,inventory_cost) {
  conn<-getAWSConnection()
  
  cash <- cash - (inventory_cost)
}
  

#Create_record_in_database (end of the game)
#Update revenue 
save_data <- function (data){
  #Connect to the database
  db <- dbConnect(M)
}

