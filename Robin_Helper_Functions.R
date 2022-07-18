# WORKING
# This function generates random demand for each of the dishes
# round_number: depending on the round_number, we know which dishes will need non-zero demand
# (e.g. in Round 1, Dish 5 will have no demand but will have non-zero demand in Round 4)
# demand_df: this is the demand data frame that needs to be updated
# HOW TO USE: demand <- function(1, demand)
# This will update your demand for dishes in Round 1
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

# # Debugging: Create a test demand_df
# demand_test_df <- data.frame(Day = c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13),
#                              Mixed_Vegetable_Rice_Set_A = c(rep(0, 12)),
#                              Mixed_Vegetable_Rice_Set_B = c(rep(0, 12)))
# 
# # Debugging: Generate demand for Round 1
# demand_test_df <- generate_random_demand(1, demand_test_df)
# print(demand_test_df)
# 
# # Debugging: Generate demand for Round 2
# demand_test_df <- generate_random_demand(2, demand_test_df)
# print(demand_test_df)
