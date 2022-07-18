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
# HOW TO USE: inventory_test_df <- calculate_consumptions(1, inventory_test_df, demand_test_df)
# This will update your inventory data frame every day in Round 1.
# I DON'T KNOW A BETTER WAY OF DOING THIS :(, but this function will store the number of dishes sold in inventory_df (to be retrieved later for calculate_revenues())
calculate_consumptions <- function(round_number, inventory_df, demand_df) {
  
  for (day in (7 * round_number - 6):(7 * round_number - 1)) {
    
    # Priority 1: Sell Cai Fan Set A
    # Find the max number of Cai Fan Set A that can be sold.
    max_no_mixed_veg_rice_set_A <- floor(min(inventory_df[inventory_df$Day == (day - 1), "Rice"],
                                             inventory_df[inventory_df$Day == (day - 1), "Pork"],
                                             inventory_df[inventory_df$Day == (day - 1), "Vegetables"] / 2,
                                             demand_df[demand_df$Day == day, 'Mixed_Vegetable_Rice_Set_A'] # If there is an excess of ingredients, max_no == demand for the day
                                             )
                                         )
    
    # I DON'T KNOW A BETTER WAY OF DOING THIS :(, but I will store the number of dishes sold in inventory (to be retrieved later for calculate_revenues())
    inventory_df[inventory_df$Day == day, "Mixed_Vegetable_Rice_Set_A_Sold"] <- max_no_mixed_veg_rice_set_A
    
    # Calculate ingredients consumed based on Cai Fan Set A demand
    rice_consumed <- 1 * max_no_mixed_veg_rice_set_A
    # Update each day's inventory based on ingredients consumed
    inventory_df[inventory_df$Day == day, "Rice"] <- inventory_df[inventory_df$Day == (day - 1), "Rice"] - rice_consumed # max(0, __) is to stop ingredient level from going below 0.
    
    pork_consumed <- 1 * max_no_mixed_veg_rice_set_A
    inventory_df[inventory_df$Day == day, "Pork"] <- inventory_df[inventory_df$Day == (day - 1), "Pork"] - pork_consumed
    
    vegetables_consumed <- 2 * max_no_mixed_veg_rice_set_A
    inventory_df[inventory_df$Day == day, "Vegetables"] <- inventory_df[inventory_df$Day == (day - 1), "Vegetables"] - vegetables_consumed
    
    # Priority 2: Sell Cai Fan Set B
    # Find the max number of Cai Fan Set B that can be sold.
    max_no_mixed_veg_rice_set_B <- floor(min(inventory_df[inventory_df$Day == (day - 1), "Noodles"],
                                             inventory_df[inventory_df$Day == (day - 1), "Chicken"] / 2,
                                             inventory_df[inventory_df$Day == day, "Vegetables"], # Based on amount of vegetables left the same day after selling Cai Fan Set A
                                             demand_df[demand_df$Day == day, 'Mixed_Vegetable_Rice_Set_B'] # If there is an excess of ingredients, max_no == demand for the day
                                             )
                                         )
    
    # I DON'T KNOW A BETTER WAY OF DOING THIS :(, but I will store the number of dishes sold in inventory (to be retrieved later for calculate_revenues())
    inventory_df[inventory_df$Day == day, "Mixed_Vegetable_Rice_Set_B_Sold"] <- max_no_mixed_veg_rice_set_B
    
    # Calculate ingredients consumed based on Cai Fan Set A demand
    noodles_consumed <- 1 * max_no_mixed_veg_rice_set_B
    # Update each day's inventory based on ingredients consumed
    inventory_df[inventory_df$Day == day, "Noodles"] <- inventory_df[inventory_df$Day == (day - 1), "Noodles"] - noodles_consumed # max(0, __) is to stop ingredient level from going below 0.
    
    chicken_consumed <- 1 * max_no_mixed_veg_rice_set_B
    inventory_df[inventory_df$Day == day, "Chicken"] <- inventory_df[inventory_df$Day == (day - 1), "Chicken"] - chicken_consumed
    
    vegetables_consumed <- 2 * max_no_mixed_veg_rice_set_B
    inventory_df[inventory_df$Day == day, "Vegetables"] <- inventory_df[inventory_df$Day == (day - 1), "Vegetables"] - vegetables_consumed
    
  }
  
  # Return updated inventory_df
  inventory_df
  # data.frame(c(inventory_df, demand_df))
  
}

# WORK IN PROGRESS
# This function calculates the revenue generated each day depending on the number of dishes sold.
# Takes in round_number, inventory_df, demand_df
# Check maximum number of dishes that can be sold given the inventory levels
# Calculate revenue
calculate_revenues <- function(round_number, inventory_df, demand_df) {
  ###
}

###################### DEBUGGING #############################

# Create a test inventory_df
# Storing the number of dishes sold here for now because I can't find a better place to store it :(
inventory_test_df <- data.frame(Day = c(seq(0, 13)),
                                Rice = c(1000, rep(0, 13)),
                                Pork = c(1000, rep(0, 13)),
                                Vegetables = c(1000, rep(0, 13)),
                                Noodles = c(1000, rep(0, 13)),
                                Chicken = c(1000, rep(0, 13)),
                                Mixed_Vegetable_Rice_Set_A_Sold = c(rep(0, 14)),
                                Mixed_Vegetable_Rice_Set_B_Sold = c(rep(0, 14)))

# Create a test demand_df
demand_test_df <- data.frame(Day = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
                             Mixed_Vegetable_Rice_Set_A = c(rep(0, 14)),
                             Mixed_Vegetable_Rice_Set_B = c(rep(0, 14)))

# Generate demand for Round 1
demand_test_df <- generate_random_demand(1, demand_test_df)

# Deduct inventory based on dishes' demand in Round 1 + Update number of each dish sold
inventory_test_df <- calculate_consumptions(1, inventory_test_df, demand_test_df)

# Generate demand for Round 2
demand_test_df <- generate_random_demand(2, demand_test_df)

# Top up/Restock before Round 2 begins
inventory_test_df$Rice[inventory_test_df$Day == 7] <- inventory_test_df$Rice[inventory_test_df$Day == 6] + 500
inventory_test_df$Pork[inventory_test_df$Day == 7] <- inventory_test_df$Pork[inventory_test_df$Day == 6] + 500
inventory_test_df$Vegetables[inventory_test_df$Day == 7] <- inventory_test_df$Vegetables[inventory_test_df$Day == 6] + 1500
inventory_test_df$Noodles[inventory_test_df$Day == 7] <- inventory_test_df$Noodles[inventory_test_df$Day == 6] + 500
inventory_test_df$Chicken[inventory_test_df$Day == 7] <- inventory_test_df$Chicken[inventory_test_df$Day == 6] + 1000

# Debugging: Deduct inventory based on dishes' demand in Round 2
inventory_test_df <- calculate_consumptions(2, inventory_test_df, demand_test_df)

