# WORKING
# This function generates random demand for each of the dishes
# (argument1) round_number: depending on the round_number, we know which dishes will need non-zero demand
# (e.g. in Round 1, Dish 5 will have no demand but will have non-zero demand in Round 4)
# (argument2) demand_df: this is the demand data frame that needs to be updated
# HOW TO USE: demand <- function(1, demand)
# This will update your demand for dishes in every round and store it in the demand data frame
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
    if (round_number >= 2) {
      mixed_veg_rice_set_B <- round(rnorm(1, mean = 40, sd = 5))
      while (mixed_veg_rice_set_B <= 0) {
        mixed_veg_rice_set_B <- round(rnorm(1, mean = 40, sd = 5))
      }
      demand_df[demand_df$Day == day, 'Mixed_Vegetable_Rice_Set_B'] = mixed_veg_rice_set_B
    }
  }
  
  # For Friday and Saturday of every round,
  for (day in (7 * round_number - 2):(7 * round_number - 1)) {
    
    # Generate demand for Dish 1: Mixed Vegetable Rice Set A
    mixed_veg_rice_set_A <- round(rnorm(1, mean = 80, sd = 10))
    while (mixed_veg_rice_set_A <= 0) {
      mixed_veg_rice_set_A <- round(rnorm(1, mean = 80, sd = 10))
    }
    # print(mixed_veg_rice_set_A) # For debugging
    demand_df[demand_df$Day == day, 'Mixed_Vegetable_Rice_Set_A'] = mixed_veg_rice_set_A
    # print(demand_df) # For debugging
    
    # Generate demand for Dish 2: Mixed Vegetable Rice Set B
    if (round_number >= 2) {
      mixed_veg_rice_set_B <- round(rnorm(1, mean = 80, sd = 10))
      while (mixed_veg_rice_set_B <= 0) {
        mixed_veg_rice_set_B <- round(rnorm(1, mean = 80, sd = 10))
      }
      demand_df[demand_df$Day == day, 'Mixed_Vegetable_Rice_Set_B'] = mixed_veg_rice_set_B
    }
  }
  
  # Return the update demand data frame
  demand_df
  
}

# WORKING
# This function updates the amount of remaining ingredients as the week plays on.
# HOW TO USE: stats_test_df <- calculate_consumptions(1, stats_test_df, demand_test_df)
# This will update your stats data frame every day in Round 1 (for the example above).
# Would recommend storing the round number as a reactive value.
calculate_consumption <- function(round_number, stats_df, demand_df) {
  
  for (day in (7 * round_number - 6):(7 * round_number - 1)) {
    
    # Priority 1: Sell Cai Fan Set A
    # Find the max number of Cai Fan Set A that can be sold.
    max_no_mixed_veg_rice_set_A <- floor(min(stats_df[stats_df$Day == (day - 1), "Rice"],
                                             stats_df[stats_df$Day == (day - 1), "Pork"],
                                             stats_df[stats_df$Day == (day - 1), "Vegetables"] / 2,
                                             demand_df[demand_df$Day == day, 'Mixed_Vegetable_Rice_Set_A'] # If there is an excess of ingredients, max_no == demand for the day
                                             )
                                         )
    
    # Store the number of dishes sold in stats (to be retrieved later for calculate_revenues())
    stats_df[stats_df$Day == day, "Mixed_Vegetable_Rice_Set_A_Sold"] <- max_no_mixed_veg_rice_set_A
    
    # Calculate ingredients consumed based on Cai Fan Set A demand
    rice_consumed <- 1 * max_no_mixed_veg_rice_set_A
    # Update each day's stats based on ingredients consumed
    stats_df[stats_df$Day == day, "Rice"] <- stats_df[stats_df$Day == (day - 1), "Rice"] - rice_consumed
    
    pork_consumed <- 1 * max_no_mixed_veg_rice_set_A
    stats_df[stats_df$Day == day, "Pork"] <- stats_df[stats_df$Day == (day - 1), "Pork"] - pork_consumed
    
    vegetables_consumed <- 2 * max_no_mixed_veg_rice_set_A
    print(vegetables_consumed)
    stats_df[stats_df$Day == day, "Vegetables"] <- stats_df[stats_df$Day == (day - 1), "Vegetables"] - vegetables_consumed
    
    # Priority 2: Sell Cai Fan Set B
    # Find the max number of Cai Fan Set B that can be sold.
    max_no_mixed_veg_rice_set_B <- floor(min(stats_df[stats_df$Day == (day - 1), "Noodles"],
                                             stats_df[stats_df$Day == (day - 1), "Chicken"] / 2,
                                             stats_df[stats_df$Day == day, "Vegetables"], # Based on amount of vegetables left the same day after selling Cai Fan Set A
                                             demand_df[demand_df$Day == day, 'Mixed_Vegetable_Rice_Set_B'] # If there is an excess of ingredients, max_no == demand for the day
                                             )
                                         )
    
    # Store the number of dishes sold in stats (to be retrieved later for calculate_revenues())
    stats_df[stats_df$Day == day, "Mixed_Vegetable_Rice_Set_B_Sold"] <- max_no_mixed_veg_rice_set_B
    
    # Calculate ingredients consumed based on Cai Fan Set A demand
    noodles_consumed <- 1 * max_no_mixed_veg_rice_set_B
    # Update each day's stats based on ingredients consumed
    stats_df[stats_df$Day == day, "Noodles"] <- stats_df[stats_df$Day == (day - 1), "Noodles"] - noodles_consumed # max(0, __) is to stop ingredient level from going below 0.
    
    chicken_consumed <- 1 * max_no_mixed_veg_rice_set_B
    stats_df[stats_df$Day == day, "Chicken"] <- stats_df[stats_df$Day == (day - 1), "Chicken"] - chicken_consumed
    
    vegetables_consumed <- 2 * max_no_mixed_veg_rice_set_B
    stats_df[stats_df$Day == day, "Vegetables"] <- stats_df[stats_df$Day == day, "Vegetables"] - vegetables_consumed
    
    # At the end of each day, update Total_storage_used column
    stats_df[stats_df$Day == day, "Total_storage_used"] <- stats_df[stats_df$Day == day, "Rice"] + stats_df[stats_df$Day == day, "Pork"] + stats_df[stats_df$Day == day, "Vegetables"] + stats_df[stats_df$Day == day, "Noodles"] + stats_df[stats_df$Day == day, "Chicken"]
    
  }
  
  # Return updated stats_df
  stats_df
  # data.frame(c(stats_df, demand_df))
  
}

# Xin Yi's calculate_revenue()
# Robin: Can consider merging this function with calculate_consumption().
calculate_revenue <- function (round_number, stats_df) {
  for (day in (7*round_number-6):(7*round_number -1)){
    # Set the price of each dish
    caifan_A_price <- 5
    caifan_B_price <- 6.5
    stats_df[stats_df$Day == day, "Revenue"] <- stats_df[stats_df$Day == day, "Mixed_Vegetable_Rice_Set_A_Sold"] * caifan_A_price + stats_df[stats_df$Day == day, "Mixed_Vegetable_Rice_Set_B_Sold"] * caifan_B_price
    stats_df[stats_df$Day == day, "Accumulative_Revenue"] <- stats_df[stats_df$Day == (day - 1), "Accumulative_Revenue"] + stats_df[stats_df$Day == day, "Revenue"]
    stats_df[stats_df$Day == day, "Cash_on_hand"] <- stats_df[stats_df$Day == (day - 1), "Cash_on_hand"] + stats_df[stats_df$Day == day, "Revenue"]
  }
  # Return stats_df
  stats_df
}

###################### DEBUGGING #############################

### Use Case: Player orders more than sufficient number of ingredients. ###

# Create a test stats_df (this is based on what we discussed in player_stats_dataframe.xlsx)
stats_test_df <- data.frame(Day = c(seq(0, 13)),
                            Rice = c(1000, rep(0, 13)),
                            Pork = c(1000, rep(0, 13)),
                            Vegetables = c(1000, rep(0, 13)),
                            Noodles = c(1000, rep(0, 13)),
                            Chicken = c(1000, rep(0, 13)),
                            Total_storage_used = c(5000, rep(0, 13)),
                            Mixed_Vegetable_Rice_Set_A_Sold = c(rep(0, 14)),
                            Mixed_Vegetable_Rice_Set_B_Sold = c(rep(0, 14)),
                            Revenue = c(rep(0, 14)),
                            Accumulative_Revenue = c(rep(0, 14)),
                            Ordering_cost = c(rep(0, 14)),
                            Cash_on_hand = c(rep(0, 14)))

# Create a test demand_df
demand_test_df <- data.frame(Day = c(seq(0, 13)),
                             Mixed_Vegetable_Rice_Set_A = c(rep(0, 14)),
                             Mixed_Vegetable_Rice_Set_B = c(rep(0, 14)))

# Generate demand for Round 1
demand_test_df <- generate_random_demand(1, demand_test_df)

# Deduct inventory stats based on dishes' demand in Round 1 + Update number of each dish sold
stats_test_df <- calculate_consumption(1, stats_test_df, demand_test_df)

# Update revenue columns
stats_test_df <- calculate_revenue(1, stats_test_df)

# Top up/Restock before Round 2 begins
stats_test_df$Rice[stats_test_df$Day == 7] <- stats_test_df$Rice[stats_test_df$Day == 6] + 500
stats_test_df$Pork[stats_test_df$Day == 7] <- stats_test_df$Pork[stats_test_df$Day == 6] + 500
stats_test_df$Vegetables[stats_test_df$Day == 7] <- stats_test_df$Vegetables[stats_test_df$Day == 6] + 1500
stats_test_df$Noodles[stats_test_df$Day == 7] <- stats_test_df$Noodles[stats_test_df$Day == 6] + 500
stats_test_df$Chicken[stats_test_df$Day == 7] <- stats_test_df$Chicken[stats_test_df$Day == 6] + 1000
stats_test_df$Total_storage_used[stats_test_df$Day == 7] <- stats_test_df$Rice[stats_test_df$Day == 7] + stats_test_df$Pork[stats_test_df$Day == 7] + stats_test_df$Vegetables[stats_test_df$Day == 7] + stats_test_df$Noodles[stats_test_df$Day == 7] + stats_test_df$Chicken[stats_test_df$Day == 7]
stats_test_df$Accumulative_Revenue[stats_test_df$Day == 7] <- stats_test_df$Accumulative_Revenue[stats_test_df$Day == 6]
stats_test_df$Ordering_cost[stats_test_df$Day == 7] <- 1000
stats_test_df$Cash_on_hand[stats_test_df$Day == 7] <- stats_test_df$Cash_on_hand[stats_test_df$Day == 6] - stats_test_df$Ordering_cost[stats_test_df$Day == 7]

# Generate demand for Round 2
demand_test_df <- generate_random_demand(2, demand_test_df)

# Deduct inventory stats based on dishes' demand in Round 2 + Update number of each dish sold
stats_test_df <- calculate_consumption(2, stats_test_df, demand_test_df)

# Update revenue columns
stats_test_df <- calculate_revenue(2, stats_test_df)

### Use Case: Player orders an insufficient amount of ingredients. ###

# Create a test stats_df (this is based on what we discussed in player_stats_dataframe.xlsx)
stats_test_df <- data.frame(Day = c(seq(0, 13)),
                            Rice = c(30, rep(0, 13)),
                            Pork = c(35, rep(0, 13)),
                            Vegetables = c(70, rep(0, 13)),
                            Noodles = c(28, rep(0, 13)),
                            Chicken = c(56, rep(0, 13)),
                            Total_storage_used = c(30 + 35 + 70 + 28 + 56, rep(0, 13)),
                            Mixed_Vegetable_Rice_Set_A_Sold = c(rep(0, 14)),
                            Mixed_Vegetable_Rice_Set_B_Sold = c(rep(0, 14)),
                            Revenue = c(rep(0, 14)),
                            Accumulative_Revenue = c(rep(0, 14)),
                            Ordering_cost = c(rep(0, 14)),
                            Cash_on_hand = c(rep(0, 14)))

# Create a test demand_df
demand_test_df <- data.frame(Day = c(seq(0, 13)),
                             Mixed_Vegetable_Rice_Set_A = c(rep(0, 14)),
                             Mixed_Vegetable_Rice_Set_B = c(rep(0, 14)))

# Generate demand for Round 1
demand_test_df <- generate_random_demand(1, demand_test_df)

# Deduct inventory stats based on dishes' demand in Round 1 + Update number of each dish sold
stats_test_df <- calculate_consumption(1, stats_test_df, demand_test_df)

# Update revenue columns
stats_test_df <- calculate_revenue(1, stats_test_df)

# Top up/Restock before Round 2 begins
stats_test_df$Rice[stats_test_df$Day == 7] <- stats_test_df$Rice[stats_test_df$Day == 6] + 500
stats_test_df$Pork[stats_test_df$Day == 7] <- stats_test_df$Pork[stats_test_df$Day == 6] + 500
stats_test_df$Vegetables[stats_test_df$Day == 7] <- stats_test_df$Vegetables[stats_test_df$Day == 6] + 1500
stats_test_df$Noodles[stats_test_df$Day == 7] <- stats_test_df$Noodles[stats_test_df$Day == 6] + 500
stats_test_df$Chicken[stats_test_df$Day == 7] <- stats_test_df$Chicken[stats_test_df$Day == 6] + 1000
stats_test_df$Total_storage_used[stats_test_df$Day == 7] <- stats_test_df$Rice[stats_test_df$Day == 7] + stats_test_df$Pork[stats_test_df$Day == 7] + stats_test_df$Vegetables[stats_test_df$Day == 7] + stats_test_df$Noodles[stats_test_df$Day == 7] + stats_test_df$Chicken[stats_test_df$Day == 7]
stats_test_df$Accumulative_Revenue[stats_test_df$Day == 7] <- stats_test_df$Accumulative_Revenue[stats_test_df$Day == 6]
stats_test_df$Ordering_cost[stats_test_df$Day == 7] <- 1000
stats_test_df$Cash_on_hand[stats_test_df$Day == 7] <- stats_test_df$Cash_on_hand[stats_test_df$Day == 6] - stats_test_df$Ordering_cost[stats_test_df$Day == 7]

# Generate demand for Round 2
demand_test_df <- generate_random_demand(2, demand_test_df)

# Deduct inventory stats based on dishes' demand in Round 2 + Update number of each dish sold
stats_test_df <- calculate_consumption(2, stats_test_df, demand_test_df)

# Update revenue columns
stats_test_df <- calculate_revenue(2, stats_test_df)

###################### DEBUGGING ENDS #############################

##### END-OF-GAME HELPER FUNCTIONS #####