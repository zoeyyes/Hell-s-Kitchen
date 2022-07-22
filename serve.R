library(ggplot2)
library(shiny)
library(shinyjs)
library(shinydashboard)

passwordModal <- function(failed = FALSE) {
  modalDialog(
    title = "Create a new password",
    passwordInput("password1", "Enter a new password:"),
    passwordInput("password2", "Confirm by re-entering the new password:"),
    "If successful, you will be assigned a Player Name to go with this password.",
    if (failed)
      div(tags$b("The passwords do not match. Try again.", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("passwordok", "OK")
    )
  )
}

loginModal <- function(failed = FALSE) {
  modalDialog(
    title = "Login",
    textInput("playername", "Enter your assigned Player Name", "FrostyFuzzyPickle"),
    passwordInput("password3", "Enter your password:"),
    if (failed)
      div(tags$b("There is no registered player with that name and password. Try again or re-register.", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("loginok", "OK")
    )
  )
}

registerPlayer <- function(password){
  #open the connection
  conn <- getAWSConnection()
  playername <- getRandomPlayerName(conn)
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
        playername <- getRandomPlayerName(conn)
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

publishScore <- function(playerid,gamevariantid,score){
  conn <- getAWSConnection()
  querytemplate <- "INSERT INTO LeaderScore (playerid,gamevariantid,asoftime,score) VALUES (?id1,?id2,NOW(),?id3)"
  query <- sqlInterpolate(conn, querytemplate,id1=playerid,id2=gamevariantid,id3=score)
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

getLeaderBoard <- function(gamevariantid){
  conn <- getAWSConnection()
  #First, we need to know whether highscorewins for this game variant
  query <- paste0("SELECT highscorewins FROM LeaderGameVariant WHERE gamevariantid=",gamevariantid)
  result <- dbGetQuery(conn,query)
  #result should return a single row
  highscorewins <- result$highscorewins[1]
  #Assemble the query for this gamevariantid
  query <- "SELECT lp.playername,ls.score,ls.asoftime  FROM LeaderScore as ls INNER JOIN LeaderPlayer as lp"
  query <- paste0(query," ON (ls.playerid=lp.playerid) WHERE ls.gamevariantid =")
  query <- paste0(query,gamevariantid)
  if (highscorewins)
    query <- paste0(query, " ORDER BY ls.score DESC,ls.asoftime ASC")
  else
    query <- paste0(query, " ORDER BY ls.score ASC,ls.asoftime ASC")
  print(query) # for debugging
  result <- dbGetQuery(conn,query)
  dbDisconnect(conn)
  result
}

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
    stats_df[stats_df$Day == day, "Rice"] <- stats_df[stats_df$Day == (day - 1), "Rice"] - rice_consumed # max(0, __) is to stop ingredient level from going below 0.
    
    pork_consumed <- 1 * max_no_mixed_veg_rice_set_A
    stats_df[stats_df$Day == day, "Pork"] <- stats_df[stats_df$Day == (day - 1), "Pork"] - pork_consumed
    
    vegetables_consumed <- 2 * max_no_mixed_veg_rice_set_A
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

ui <- dashboardPage(
  dashboardHeader(title = "Hell's Kitchen"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome", icon = icon("door-open")),
      menuItem("Game", tabName = "game", icon = icon("chess-board")),
      menuItem("Scores", tabName = "scores", icon = icon("cash-register"))
    )),
  
  dashboardBody(
    tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #CD853F;
                                }
                                
                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #CD853F;
                                }
                                
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #CD853F;
                                }
                                
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #CD853F;
                                }
                                
                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #D2B48C;
                                }
                                
                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #DEB887;
                                color: #000000;
                                }
                                
                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #D2B48C;
                                }
                                
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #DEB887;
                                }
                                
                            
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #FFE4B5;
                                }
                                
                                .box.box-solid.box-primary>.box-header {
                                color:#fff;
                                background:#CD853F
                                }
                                
                                .box.box-solid.box-primary{
                                border-bottom-color:#CD853F;
                                border-left-color:#CD853F;
                                border-right-color:#CD853F;
                                border-top-color:#CD853F;
                                }
                                
                                .box.box-danger>.box-primary {
                                color:#fff; 
                                background:#CD853F
                                }

                                .box.box-primary{
                                border-bottom-color:#CD853F;
                                border-left-color:#CD853F;
                                border-right-color:#CD853F;
                                border-top-color:#CD853F;
                                background: #CD853F;
                                }
                                '))),
    
    tabItems(
      # First tab content
      tabItem(tabName = "welcome",
              h2("Welcome, instructions, and login go here"),
              actionButton("register", "Register"),
              actionButton("login", "Login"),
              tags$h4("Logged in as:"),
              htmlOutput("loggedInAs"),
              tags$br(),
              box(
                tags$h4("Instructions"),
                tags$p("There are no rules for this game yet. We are simply demonstrating the elements of a two-player game. ")
              )
              
      ),
      
      # Second tab content
      tabItem(tabName = "game",
              fluidRow(
                column(3,tags$div(id="inputsection"),
                       numericInput("pork","PORK",1,min=1),
                       numericInput("chicken","CHICKEN",1,min=1),
                       numericInput("rice","RICE",1,min=1),
                       numericInput("noodles","NOODLES",1,min=1),
                       numericInput("vegetables","VEGETABLESset",1,min=1),
                       div(style = "color:brown, font-size=500%",align='center', 
                           'INVENTORY LIMIT: 100'),
                       div(align='center',actionButton('start','START GAME',style="background-color:#CD853F",class="btn-lg"))),
                column(9,
                       box(height='450px',width='1050px',status = 'primary',
                           img(src='Meat.gif',height='420px',width='1030px'))
                )
              ),
              tags$br(),
              fluidRow(
                
                box(
                  title = 'Message Box',solidHeader = TRUE,width =3,height = '320px',
                  htmlOutput('message')),
                box(title = 'Recipes',solidHeader = TRUE,width = 5,status = 'primary',height = '320px',
                    imageOutput("recipe1",height="100px",width="100px",inline=TRUE),
                    imageOutput("recipe2",height="100px",width="100px",inline=TRUE),
                    imageOutput("recipe3",height="100px",width="100px",inline=TRUE),
                    tags$br(),
                    imageOutput("recipe4",height="80px",width="120px",inline=TRUE),
                    imageOutput("recipe5",height="80px",width="120px",inline=TRUE)),
                tabBox(width = 4,height = '320px', 
                       tabPanel('Revenue',plotOutput("Revenueplot")),
                       tabPanel('Demand',plotOutput("Demandplot")),
                       tabPanel('Inventory',plotOutput("Inventoryplot"))))
      ),
      
      tabItem(tabName = 'scores',
              h2('Leaderboard'),
              fluidRow(
                htmlOutput('leaderboard')
              )
      )
    )
  )
)

server <- function(input, output, session) {
  vals <- reactiveValues(password = NULL,playerid=NULL,playername=NULL)
  #Fire some code if the user clicks the Register button
  observeEvent(input$register, {
    showModal(passwordModal(failed=FALSE))
  })
  observeEvent(input$login, {
    showModal(loginModal(failed=FALSE))
  })
  observeEvent(input$start, {
    # Create the state_df
    stats <- data.frame(Day = c(seq(0, 13)),
                           Rice = c(input$rice, rep(0, 13)),
                           Pork = c(input$pork, rep(0, 13)),
                           Vegetables = c(input$vegetables, rep(0, 13)),
                           Noodles = c(input$noodles, rep(0, 13)),
                           Chicken = c(input$chicken, rep(0, 13)),
                           Total_storage_used = c(input$rice+input$pork+input$vegetables+input$noodles+input$chicken, rep(0, 13)),
                           Mixed_Vegetable_Rice_Set_A_Sold = c(rep(0, 14)),
                           Mixed_Vegetable_Rice_Set_B_Sold = c(rep(0, 14)),
                           Revenue = c(rep(0, 14)),
                           Accumulative_Revenue = c(rep(0, 14)),
                           Ordering_cost = c(rep(0, 14)),
                           Cash_on_hand = c(rep(0, 14)))
    
    # Create the demand_df
    demand <- data.frame(Day = c(seq(0, 13)),
                            Mixed_Vegetable_Rice_Set_A = c(rep(0, 14)),
                            Mixed_Vegetable_Rice_Set_B = c(rep(0, 14)))
    
    # Generate demand for Round 1
    demand <- generate_random_demand(1, demand)
    View(demand)
    
    # Deduct inventory stats based on dishes' demand in Round 1 + Update number of each dish sold
    stats <- calculate_consumption(1, stats, demand)
    
    # Update revenue columns
    stats <- calculate_revenue(1, stats)
    View(stats)
    
    output$Revenueplot <- renderPlot({
      ggplot(stats,mapping=aes(x=Day,y=Accumulative_Revenue))+
        geom_line()+
        geom_text(aes(label=Accumulative_Revenue))
    })
    output$Inventoryplot <- renderPlot({
      ggplot(stats)+
        geom_line(aes(x=Day,y=Pork,color="red"))+
        geom_line(aes(x=Day,y=Rice,color="blue"))+
        geom_line(aes(x=Day,y=Vegetables,color="green"))+
        geom_line(aes(x=Day,y=Noodles,color="yello"))+
        geom_line(aes(x=Day,y=Chicken,color="black"))
    })
    output$Demandplot <- renderPlot({
      ggplot(demand,mapping=aes(x=Day,y=Mixed_Vegetable_Rice_Set_A))+
        geom_line()+
        geom_text(aes(label=Mixed_Vegetable_Rice_Set_A))
    })
  })
  
  # to be editted
  output$leaderboard <- renderTable({numclicks <- input$publishscore +input$start #to force a refresh whenever one of these buttons is clicked
  leaderboard <- getLeaderBoard(vals$gamevariantid)
  leaderboard}
  )
  
  observeEvent(input$playgame, {
    vals$score = as.integer(runif(1,min=0,max=100))
    print(vals$score)
  })
  
  # React to completion of game
  output$score <- renderUI({
    if (is.null(vals$score))
      "No score yet."
    else
      as.character(vals$score)
  })
  
  output$moreControls <- renderUI({
    req(vals$score) # if vals$score is NULL, the controls will not be visible
    tagList(
      actionButton("publishscore", "Publish Your Score"),
      tableOutput("leaderboard")
    )
  })
  
  observeEvent(input$publishscore,{
    publishScore(vals$playerid,vals$gamevariantid,vals$score)
  })
  
  observeEvent(input$publishscore,{
    publishScore(vals$playerid,vals$gamevariantid,vals$score)
  })
  output$leaderboard <- renderTable({numclicks <- input$publishscore +input$playgame #to force a refresh whenever one of these buttons is clicked
  leaderboard <- getLeaderBoard(vals$gamevariantid)
  leaderboard}
  )
}


shinyApp(ui, server)

