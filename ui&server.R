source('helper_functions2.R')
library(shiny)
library(shinyjs)
library(shinydashboard)
library(jsonlite)

ui <- dashboardPage(
  dashboardHeader(title = "Hell's Kitchen"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome", icon = icon("door-open")),
      menuItem("Game", tabName = "game", icon = icon("chess-board")),
      menuItem("Analysis", tabName = "analysis", icon = icon("line-chart")),
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
              uiOutput('changePassword'),
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
                       numericInput("pork","PORK",0),
                       numericInput("chicken","CHICKEN",0),
                       numericInput("rice","RICE",0),
                       numericInput("noodles","NOODLES",0),
                       numericInput("vegetables","VEGETABLESset",0),
                       div(style = "color:brown, font-size=500%",align='center', 
                           'INVENTORY LIMIT: 5000'),
                       div(align='center',uiOutput('startbutton'))),
                column(9,
                       box(height='450px',width='1050px',status = 'primary',
                           img(src='Meat.gif',height='420px',width='1030px'))
                )
              ),
              uiOutput("moreControls"),
              tags$br(),
              fluidRow(
                
                box(
                  title = 'Message Box',solidHeader = TRUE,width =3,height = '320px',
                  htmlOutput('message')),
                box(title = 'Recipes',solidHeader = TRUE,width = 5,status = 'primary',height = '320px',
                    img(src='SetA.png',height='100px',width='180px'),
                    uiOutput("recipe3"),
                    uiOutput("recipe4"),
                    tags$br(),
                    img(src='SetB.png',height='100px',width='100px'),
                    uiOutput("recipe5")),
                tabBox(width = 4,height = '320px', 
                       tabPanel('Cash-on-hand',plotOutput("Cash_on_hand_plot")),
                       tabPanel('Demand',plotOutput("Demandplot")),
                       tabPanel('Inventory',plotOutput("Inventoryplot"))))
      ),
      
      # Analysis tab content
      tabItem(tabName = 'analysis',
              h2('Weekly Summary'),
              tableOutput("analysis"),
              tableOutput("dishes")
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
  
  
  #------------------------initialize---------------------------------------------------------
  
  MAXROUND<-5
  Initial_cash_on_hand<-20000
  Inventory_limit<-5000
  
  initial_orderplan<-data.frame(matrix(ncol = 5, nrow = MAXROUND))
  colnames(initial_orderplan)<-c('chicken', 'pork', 'noodles', 'rice', 'vegetables')
  
  initial_stats <- data.frame(Day = c(seq(0, MAXROUND*7)),
                              Rice = c(rep(0, MAXROUND*7+1)),
                              Pork = c(rep(0, MAXROUND*7+1)),
                              Vegetables = c(rep(0, MAXROUND*7+1)),
                              Noodles = c(rep(0, MAXROUND*7+1)),
                              Chicken = c(rep(0, MAXROUND*7+1)),
                              Total_storage_used = c(rep(0, MAXROUND*7+1)),
                              Mixed_Vegetable_Rice_Set_A_Sold = c(rep(0, MAXROUND*7+1)),
                              Mixed_Vegetable_Rice_Set_B_Sold = c(rep(0, MAXROUND*7+1)),
                              Japanese_Bowl_A_Sold=c(rep(0, MAXROUND*7+1)),
                              Japanese_Bowl_B_Sold=c(rep(0, MAXROUND*7+1)),
                              Ultimate_Bowl_Sold=c(rep(0, MAXROUND*7+1)),
                              Revenue = c(rep(0, MAXROUND*7+1)),
                              Accumulative_Revenue = c(rep(0, MAXROUND*7+1)),
                              Ordering_cost = c(rep(0, MAXROUND*7+1)),
                              Cash_on_hand = c(rep(0, MAXROUND*7+1)))
  
  
  initial_demand<- data.frame(Day = c(seq(1, MAXROUND*7)),
                              Mixed_Vegetable_Rice_Set_A = c(rep(0, MAXROUND*7)),
                              Mixed_Vegetable_Rice_Set_B = c(rep(0, MAXROUND*7)),
                              Japanese_Bowl_A=c(rep(0, MAXROUND*7)),
                              Japanese_Bowl_B=c(rep(0, MAXROUND*7)),
                              Ultimate_Bowl=c(rep(0, MAXROUND*7)))
  
  initial_analysis <- data.frame(Week = c (seq(1,5)),
                                 Rice_Sold = c(rep(0, 5)),
                                 Pork_Sold = c(rep(0, 5)),
                                 Vegetables_Sold = c(rep(0, 5)),
                                 Noodles_Sold = c(rep(0, 5)),
                                 Chicken_Sold = c(rep(0, 5)))
  
  initial_dishes <-data.frame(Week = c (seq(1,5)),
                              Set_A_Demand = c(rep(0, 5)),
                              Set_A_Sold = c(rep(0, 5)),
                              Set_A_Lost = c(rep(0, 5)),
                              Set_B_Demand = c(rep(0, 5)),
                              Set_B_Sold = c(rep(0, 5)),
                              Set_B_Lost = c(rep(0, 5)))
  
  
  vals <- reactiveValues(password = NULL,playerid=NULL,playername=NULL,round=1,stats=initial_stats,demand=initial_demand,orderplan=initial_orderplan,weekly_plan=initial_analysis,dishes=initial_dishes)
  
  
  #------------------------log in---------------------------------------------------------  
  
  observeEvent(input$register, {
    showModal(passwordModal(failed=FALSE))
  })
  
  observeEvent(input$passwordok, {
   
    
    if (str_length(input$password1) >0 && (input$password1 == input$password2)) {
      vals$password <- input$password1
      print(vals$password) 
      vals$playername = input$playername
      #####return false if error (duplicated username)
      unique<-registerPlayer(vals$playername,vals$password)
      
      if(unique){
          if (!is.null(vals$playername)){
            vals$playerid <- getPlayerID(vals$playername,vals$password)
          }
          removeModal()
      }else{showModal(passwordModal(failed = TRUE,duplicated=TRUE,password_wrong=FALSE))}
    }else{
        showModal(passwordModal(failed = TRUE,duplicated=FALSE,password_wrong=TRUE))
    }
  })
  
  observeEvent(input$login, {
    showModal(loginModal(failed=FALSE))
  })
  
  observeEvent(input$loginok, {
    playerid <- getPlayerID(input$playername,input$password3)
    if (playerid>0) {
      vals$playerid <- playerid
      vals$playername <- input$playername
      vals$password<-input$password3
      removeModal()
    } else {
      showModal(loginModal(failed = TRUE))
    }
  })
  
  output$loggedInAs <- renderUI({
    if (is.null(vals$playername))
      "Not logged in yet."
    else
      vals$playername
  })
  
  output$changePassword <- renderUI({
    req(vals$playerid)
    tagList(
      actionButton('changePassword','Change your password')
    )
  })
  
  observeEvent(input$changePassword,{
    print('pressed changePassowrd')
    showModal(reEnterPasswordModal(failed=FALSE,vals$playername))
  })
  
  observeEvent(input$oldpasswordok,{
    print('oldpasswordok')
    
    print(input$currentPassword == vals$password)
    if (input$currentPassword == vals$password){
      #removeModal()
      print('yes')
      showModal(UpdatePasswordModal(failed =FALSE))
    } else {
      print('no')
      showModal(reEnterPasswordModal(failed = FALSE,vals$playername))
    }
  })
  
  observeEvent(input$newpasswordok,{
    if (str_length(input$newpassword1) >0 && (input$newpassword1 == input$newpassword2)) {
      #store the password and close the dialog
      vals$password <- input$newpassword1
      print(paste0('newpass=',vals$password)) # for debugging
      updatePassword(vals$playername,vals$password)
      removeModal()
    } else {
      showModal(UpdatePasswordModal(failed = TRUE))
    }
  })
  
  output$changePassword<-renderUI({
    req(vals$playerid)
    tagList(
      actionButton('changePassword','Change your password')
    )
  })
  
  observeEvent(input$changePassword,{
    showModal(reEnterPasswordModal(failed=FALSE,vals$playername))
  })
  
  observeEvent(input$oldpasswordok,{
    
    print(paste0('entered',input$currentPassword))
    print(paste0('correct',vals$password))
    print('checking now')
    checked<-(input$currentPassword == vals$password)
    print(checked)
    if (input$currentPassword==vals$password){
      #removeModal()
      print('True alr')
      showModal(UpdatePasswordModal(failed = FALSE))
    } else {
      showModal(reEnterPasswordModal(failed = TRUE,vals$playername))
    }
  })
  
  observeEvent(input$newpasswordok,{
    if (str_length(input$newpassword1) >0 && (input$newpassword1 == input$newpassword2)) {
      #store the password and close the dialog
      vals$password <- input$newpassword1
      print(paste0('newpass=',vals$password)) # for debugging
      updatePassword(vals$playername,vals$password)
      removeModal()
    } else {
      showModal(UpdatePasswordModal(failed = TRUE))
    }
  })
  
  
  output$startbutton <- renderUI({
    req(vals$playerid) # if not logged in, the controls will not be visible
    tagList(
      actionButton("start",'START GAME',style="background-color:#CD853F",class="btn-lg")
    )
  })
  
  #----------------------------start game--------------------------------------------------------------  
  observeEvent(input$pork,{
    vals$orderplan$pork[vals$round] <- input$pork
    print(paste0('pork:',vals$orderplan$pork[vals$round]))
  })
  
  observeEvent(input$chicken,{
    vals$orderplan$chicken[vals$round] <- input$chicken
    print(paste0('chicken:',vals$orderplan$chicken[vals$round]))
  })
  
  observeEvent(input$rice,{
    vals$orderplan$rice[vals$round] <- input$rice
    print(paste0('rice:',vals$orderplan$rice[vals$round]))
  })
  
  observeEvent(input$noodles,{
    vals$orderplan$noodles[vals$round] <- input$noodles
    print(paste0('noodles:',vals$orderplan$noodles[vals$round]))
  })
  
  observeEvent(input$vegetables,{
    vals$orderplan$vegetables[vals$round] <- input$vegetables
    print(paste0('vegetables:',vals$orderplan$vegetables[vals$round]))
  })
  
  observeEvent(input$start,{
    showModal(ConfirmStartModal(vals$round))
  })
  
  
  observeEvent(input$confirm, {
    
    removeModal()
    
    if(vals$orderplan$chicken[vals$round]+vals$orderplan$pork[vals$round]+vals$orderplan$rice[vals$round]+vals$orderplan$noodles[vals$round]+vals$orderplan$vegetables[vals$round]>Inventory_limit){
      showModal(ExceedCapacityModal())
    }else if(vals$orderplan$chicken[vals$round]<0 | vals$orderplan$pork[vals$round]<0 | vals$orderplan$rice[vals$round]<0 | vals$orderplan$noodles[vals$round]<0 | vals$orderplan$vegetables[vals$round]<0){
      showModal(NegativeInvenModal())
    }else{
    
    
    if(vals$round>MAXROUND){
      #NOT FINISHED
      showModal(MaxroundModal(failed=TRUE))
    }
    
    if(vals$round>1){
      #NOT FINISHED
      print(paste('now is round',vals$round))
      vals$stats <- calculate_orderingcost(vals$round,vals$orderplan,vals$stats)
      print('ordering cost')
      if(vals$stats[vals$stats$Day==vals$round*7-6,"Ordering_cost"] <= vals$stats[vals$stats$Day == vals$round*7-7,'Cash_on_hand']){
        vals$stats<-update_storage_used(vals$round,vals$orderplan,vals$stats)
        print('update inventory')
        vals$demand <- generate_random_demand(vals$round, vals$demand)
        print('demand generated')
        vals$stats <- calculate_consumption(vals$round, vals$stats, vals$demand)
        print('consumed')
        vals$stats <- calculate_revenue(vals$round, vals$stats)
        print('update revenue')
        vals$stats <- calculate_cash_on_hand(vals$round,vals$stats)
        print('cash-on-hand')
        View(vals$stats)
        View(vals$orderplan)
        View(vals$demand)
        row_num <- 7*vals$round
        output$Cash_on_hand_plot <- renderPlot({
          ggplot(vals$stats[0:row_num,],mapping=aes(x=Day,y=Cash_on_hand))+
            geom_line()+
            geom_text(aes(label=Cash_on_hand))+
            labs(x="Day",y="Cash on hand",title = "Cash on hand VS Day")
        })
        print("plot1")
        
        output$Inventoryplot <- renderPlot({
          ggplot(vals$stats[0:row_num,])+
            geom_line(aes(x=Day,y=Pork,color="Pork"))+
            geom_line(aes(x=Day,y=Rice,color="Rice"))+
            geom_line(aes(x=Day,y=Vegetables,color="Vegetables"))+
            geom_line(aes(x=Day,y=Noodles,color="Noodles"))+
            geom_line(aes(x=Day,y=Chicken,color="Chicken"))+
            xlab("Day")+ylab("Inventory")
        })
        print("plot2")
        
        if(vals$round==2){output$Demandplot <- renderPlot({
          ggplot(vals$demand[0:row_num,])+
            geom_line(aes(x=Day,y=Mixed_Vegetable_Rice_Set_A,color='Mixed_Vegetable_Rice_Set_A'))+
            geom_line(aes(x=Day,y=Mixed_Vegetable_Rice_Set_B,color='Mixed_Vegetable_Rice_Set_B'))+
            xlab("Day")+ylab("Demand")
        })}
        
        if(vals$round==3){output$Demandplot <- renderPlot({
          ggplot(vals$demand[0:row_num,])+
            geom_line(aes(x=Day,y=Mixed_Vegetable_Rice_Set_A,color='Mixed_Vegetable_Rice_Set_A'))+
            geom_line(aes(x=Day,y=Mixed_Vegetable_Rice_Set_B,color='Mixed_Vegetable_Rice_Set_B'))+
            geom_line(aes(x=Day,y=Japanese_Bowl_A,color='Japanese_Bowl_A'))+
            xlab("Day")+ylab("Demand")
        })}
        
        if(vals$round==4){output$Demandplot <- renderPlot({
          ggplot(vals$demand[0:row_num,])+
            geom_line(aes(x=Day,y=Mixed_Vegetable_Rice_Set_A,color='Mixed_Vegetable_Rice_Set_A'))+
            geom_line(aes(x=Day,y=Mixed_Vegetable_Rice_Set_B,color='Mixed_Vegetable_Rice_Set_B'))+
            geom_line(aes(x=Day,y=Japanese_Bowl_A,color='Japanese_Bowl_A'))+
            geom_line(aes(x=Day,y=Japanese_Bowl_B,color='Japanese_Bowl_B'))+
            xlab("Day")+ylab("Demand")
        })}
        
        if(vals$round==5){output$Demandplot <- renderPlot({
          ggplot(vals$demand[0:row_num,])+
            geom_line(aes(x=Day,y=Mixed_Vegetable_Rice_Set_A,color='Mixed_Vegetable_Rice_Set_A'))+
            geom_line(aes(x=Day,y=Mixed_Vegetable_Rice_Set_B,color='Mixed_Vegetable_Rice_Set_B'))+
            geom_line(aes(x=Day,y=Japanese_Bowl_A,color='Japanese_Bowl_A'))+
            geom_line(aes(x=Day,y=Japanese_Bowl_B,color='Japanese_Bowl_B'))+
            geom_line(aes(x=Day,y=Ultimate_Bowl,color='Ultimate_Bowl'))+
            xlab("Day")+ylab("Demand")
        })}
        
        print("plot3")
        week<-vals$round
        View(vals$weekly_plan)
        View(vals$weekly_plan)
        vals$dishes[vals$dishes$Week==week,"Set_A_Demand"]<-sum(vals$demand$Mixed_Vegetable_Rice_Set_A)-vals$dishes[vals$dishes$Week==week-1,"Set_A_Demand"]
        vals$dishes[vals$dishes$Week==week,"Set_A_Sold"]<-sum(vals$stats$Mixed_Vegetable_Rice_Set_A)-vals$dishes[vals$dishes$Week==week-1,"Set_A_Sold"]
        vals$dishes[vals$dishes$Week==week,"Set_A_Lost"]<-vals$dishes[vals$dishes$Week==week,"Set_A_Demand"]-vals$dishes[vals$dishes$Week==week,"Set_A_Sold"]
        vals$dishes[vals$dishes$Week==week,"Set_B_Demand"]<-sum(vals$demand$Mixed_Vegetable_Rice_Set_B)-vals$dishes[vals$dishes$Week==week-1,"Set_B_Demand"]
        vals$dishes[vals$dishes$Week==week,"Set_B_Sold"]<-sum(vals$stats$Mixed_Vegetable_Rice_Set_B)-vals$dishes[vals$dishes$Week==week-1,"Set_B_Sold"]
        vals$dishes[vals$dishes$Week==week,"Set_B_Lost"]<-vals$dishes[vals$dishes$Week==week,"Set_B_Demand"]-vals$dishes[vals$dishes$Week==week,"Set_B_Sold"]
        vals$weekly_plan[vals$weekly_plan$Week==week,"Rice_Sold"]<-vals$dishes[vals$dishes$Week==week,"Set_A_Sold"]
        vals$weekly_plan[vals$weekly_plan$Week==week,"Pork_Sold"]<-vals$dishes[vals$dishes$Week==week,"Set_A_Sold"]
        vals$weekly_plan[vals$weekly_plan$Week==week,"Vegetables_Sold"]<-vals$dishes[vals$dishes$Week==week,"Set_A_Sold"]*2+vals$dishes[vals$dishes$Week==week,"Set_B_Sold"]
        vals$weekly_plan[vals$weekly_plan$Week==week,"Noodles_Sold"]<-vals$dishes[vals$dishes$Week==week,"Set_B_Sold"]
        vals$weekly_plan[vals$weekly_plan$Week==week,"Chicken_Sold"]<-vals$dishes[vals$dishes$Week==week,"Set_B_Sold"]*2
        vals$round <- vals$round+1
      }else{
        showModal(warningModel())
        View(vals$stats)
      }
      
    }
    
    if(vals$round==1){
      print(paste('now is round',vals$round))
      
      vals$stats[vals$stats$Day==0,"Total_storage_used"]<-sum(vals$orderplan[1,])
      print('total storage updated')
      
      vals$stats[vals$stats$Day==0,"Cash_on_hand"]<-Initial_cash_on_hand
      print('initialze cash-on-hand')
      
      
      vals$demand<- generate_random_demand(vals$round, vals$demand)
      print('demand generated')
      
      
      vals$stats <- calculate_orderingcost(vals$round,vals$orderplan,vals$stats)
      print('ordering cost')
      
      #error occurs
      vals$stats<-update_storage_used(vals$round,vals$orderplan,vals$stats)
      print('update inventory')
      
      # Deduct inventory stats based on dishes' demand in Round 1 + Update number of each dish sold
      vals$stats <- calculate_consumption(vals$round, vals$stats, vals$demand)
      print('consumed')
      
      # Update revenue columns
      vals$stats <- calculate_revenue(vals$round, vals$stats)
      print('update revenue')
      
      # Update cash_on_hand
      vals$stats <- calculate_cash_on_hand(vals$round,vals$stats)
      print('cash-on-hand')
      
      output$Cash_on_hand_plot <- renderPlot({
        ggplot(vals$stats[0:7,],mapping=aes(x=Day,y=Cash_on_hand))+
          geom_line()+
          geom_text(aes(label=Cash_on_hand))
      })
      
      output$Inventoryplot <- renderPlot({
        ggplot(vals$stats[0:7,])+
          geom_line(aes(x=Day,y=Pork,color="Pork"))+
          geom_line(aes(x=Day,y=Rice,color="Rice"))+
          geom_line(aes(x=Day,y=Vegetables,color="Vegetables"))+
          geom_line(aes(x=Day,y=Noodles,color="Noodles"))+
          geom_line(aes(x=Day,y=Chicken,color="Chicken"))+
          xlab("Day")+ylab("Inventory")
      })
      output$Demandplot <- renderPlot({
        ggplot(vals$demand[0:7,])+
          geom_line(aes(x=Day,y=Mixed_Vegetable_Rice_Set_A,color='Mixed_Vegetable_Rice_Set_A'))+
          geom_line(aes(x=Day,y=Mixed_Vegetable_Rice_Set_B,color='Mixed_Vegetable_Rice_Set_B'))+
          xlab("Day")+ylab("Demand")
        
      })
      week<-vals$round
      vals$dishes[vals$dishes$Week==week,"Set_A_Demand"]<-sum(vals$demand$Mixed_Vegetable_Rice_Set_A)
      vals$dishes[vals$dishes$Week==week,"Set_A_Sold"]<-sum(vals$stats$Mixed_Vegetable_Rice_Set_A)
      vals$dishes[vals$dishes$Week==week,"Set_A_Lost"]<-vals$dishes[vals$dishes$Week==week,"Set_A_Demand"]-vals$dishes[vals$dishes$Week==week,"Set_A_Sold"]
      vals$dishes[vals$dishes$Week==week,"Set_B_Demand"]<-sum(vals$demand$Mixed_Vegetable_Rice_Set_B)
      vals$dishes[vals$dishes$Week==week,"Set_B_Sold"]<-sum(vals$stats$Mixed_Vegetable_Rice_Set_B)
      vals$dishes[vals$dishes$Week==week,"Set_B_Lost"]<-vals$dishes[vals$dishes$Week==week,"Set_B_Demand"]-vals$dishes[vals$dishes$Week==week,"Set_B_Sold"]
      vals$weekly_plan[vals$weekly_plan$Week==week,"Rice_Sold"]<-vals$dishes[vals$dishes$Week==week,"Set_A_Sold"]
      vals$weekly_plan[vals$weekly_plan$Week==week,"Pork_Sold"]<-vals$dishes[vals$dishes$Week==week,"Set_A_Sold"]
      vals$weekly_plan[vals$weekly_plan$Week==week,"Vegetables_Sold"]<-vals$dishes[vals$dishes$Week==week,"Set_A_Sold"]*2+vals$dishes[vals$dishes$Week==week,"Set_B_Sold"]
      vals$weekly_plan[vals$weekly_plan$Week==week,"Noodles_Sold"]<-vals$dishes[vals$dishes$Week==week,"Set_B_Sold"]
      vals$weekly_plan[vals$weekly_plan$Week==week,"Chicken_Sold"]<-vals$dishes[vals$dishes$Week==week,"Set_B_Sold"]*2
      
      vals$round <- vals$round+1
      
    }}
    output$analysis <- renderTable(head(vals$weekly_plan))
    output$dishes <- renderTable(head(vals$dishes))
    updateNumericInput(session,'pork',value = 0)
    updateNumericInput(session,'chicken',value = 0)
    updateNumericInput(session,'noodles',value = 0)
    updateNumericInput(session,'rice',value = 0)
    updateNumericInput(session,'vegetables',value = 0)
    
    
    
  })
  
  #-----------------------------------leaderboard( tbd )---------------------------------------------  
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


