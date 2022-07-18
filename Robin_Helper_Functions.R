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
calculate_consumptions <- function(round_number, inventory_df, demand_df) {

  for (day in (7 * round_number - 6):(7 * round_number - 1)) {
    # Calculate ingredients consumed based on dishes' demand
    rice_consumed <- 1 * demand_df[demand_df$Day == day, 'Mixed_Vegetable_Rice_Set_A']
    # Update each day's inventory based on ingredients consumed
    inventory_df[inventory_df$Day == day, "Rice"] <- max(0, inventory_df[inventory_df$Day == (day - 1), "Rice"] - rice_consumed) # max(0, __) is to stop ingredient level from going below 0.
    
    pork_consumed <- 1 * demand_df[demand_df$Day == day, 'Mixed_Vegetable_Rice_Set_A']
    inventory_df[inventory_df$Day == day, "Pork"] <- max(0, inventory_df[inventory_df$Day == (day - 1), "Pork"] - pork_consumed)
    
    vegetables_consumed <- 2 * demand_df[demand_df$Day == day, 'Mixed_Vegetable_Rice_Set_A'] + 1 * demand_df[demand_df$Day == day, 'Mixed_Vegetable_Rice_Set_B']
    inventory_df[inventory_df$Day == day, "Vegetables"] <- max(0, inventory_df[inventory_df$Day == (day - 1), "Vegetables"] - vegetables_consumed)
    
    noodles_consumed <- 1 * demand_df[demand_df$Day == day, 'Mixed_Vegetable_Rice_Set_B']
    inventory_df[inventory_df$Day == day, "Noodles"] <- max(0, inventory_df[inventory_df$Day == (day - 1), "Noodles"] - noodles_consumed)
    
    chicken_consumed <- 2 * demand_df[demand_df$Day == day, 'Mixed_Vegetable_Rice_Set_B']
    inventory_df[inventory_df$Day == day, "Chicken"] <- max(0, inventory_df[inventory_df$Day == (day - 1), "Chicken"] - chicken_consumed)
  }
  
  # Return updated inventory_df
  inventory_df
}

###################### DEBUGGING #############################

# Create a test inventory_df
inventory_test_df <- data.frame(Day = c(seq(0, 13)),
                                Rice = c(1000, rep(0, 13)),
                                Pork = c(1000, rep(0, 13)),
                                Vegetables = c(1000, rep(0, 13)),
                                Noodles = c(1000, rep(0, 13)),
                                Chicken = c(1000, rep(0, 13)))

# Create a test demand_df
demand_test_df <- data.frame(Day = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
                             Mixed_Vegetable_Rice_Set_A = c(rep(0, 14)),
                             Mixed_Vegetable_Rice_Set_B = c(rep(0, 14)))

# Generate demand for Round 1
demand_test_df <- generate_random_demand(1, demand_test_df)

# Deduct inventory based on dishes' demand in Round 1
inventory_test_df <- calculate_consumptions(1, inventory_test_df, demand_test_df)

# Generate demand for Round 2
demand_test_df <- generate_random_demand(2, demand_test_df)

# Top up before Round 2 begins
inventory_test_df$Rice[inventory_test_df$Day == 7] <- inventory_test_df$Rice[inventory_test_df$Day == 6] + 500
inventory_test_df$Pork[inventory_test_df$Day == 7] <- inventory_test_df$Pork[inventory_test_df$Day == 6] + 500
inventory_test_df$Vegetables[inventory_test_df$Day == 7] <- inventory_test_df$Vegetables[inventory_test_df$Day == 6] + 1500
inventory_test_df$Noodles[inventory_test_df$Day == 7] <- inventory_test_df$Noodles[inventory_test_df$Day == 6] + 500
inventory_test_df$Chicken[inventory_test_df$Day == 7] <- inventory_test_df$Chicken[inventory_test_df$Day == 6] + 1000

# Debugging: Deduct inventory based on dishes' demand in Round 2
inventory_test_df <- calculate_consumptions(2, inventory_test_df, demand_test_df)

