library(ggplot2)
library(shiny)
library(shinyjs)



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
                       numericInput("redmeat","RED MEAT",1,min=1),
                       numericInput("whitemeat","WHITE MEAT",1,min=1),
                       numericInput("rice","RICE",1,min=1),
                       numericInput("noodle","NOODLE",1,min=1),
                       numericInput("vegitable","VEGETABLE",1,min=1),
                       div(style = "color:brown",align='center',
                           'INVENTORY LIMIT: 100'),
                       div(align='center',actionButton('start','START GAME',width=2,style="background-color:#CD853F",class="btn-lg"))),
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
                       box(title = 'Recipes',solidHeader = TRUE,width = 5,status = 'primary',height = '320px',),
                       tabBox(width = 4,height = '320px', 
                              tabPanel('Revenue','Revenues'),
                              tabPanel('Demand','Demands'),
                              tabPanel('Inventory','Inventory')))
              
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

server <- function(input, output, session) {}


shinyApp(ui, server)

