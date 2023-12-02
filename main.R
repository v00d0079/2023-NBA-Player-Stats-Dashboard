# Loading required libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(stringr, include.only = c('str_detect', 'str_wrap'))

# Reading data and selecting needed variables
Nba_player_data <- read.csv("C:/Users/sniss/OneDrive/Desktop/NBAStats2023/2023_nba_player_stats.csv")
selection<- Nba_player_data %>%select('Player','POS','Team','Age','GP','Min','PTS','FGM','FGA','FG.','X3PM','X3PA','X3P.','FT.','REB','AST','TOV', 'STL','DD2','TD3','X...')

#Creating additional variables for graphs
selection$Eff<-(selection$FG.+selection$X3P.)/2
selection$PPG<-round(selection$PTS/selection$GP,2)
selection$MPG<-round(selection$Min/selection$GP,2)
selection<-selection[selection$POS!="N/A",]

# Section getting possible inputs and setting the default selection
playerinput<-selection$Player
player_selected <- head(playerinput, 75)

ageinput<-unique(selection$Age)
age_selected <- head(ageinput, 5)

teaminput<-unique(selection$Team)
team_selected <- head(teaminput, 5)

posinput<-unique(selection$POS)
pos_selected<-head(posinput,3)


#Creation of UI
ui <- dashboardPage(
  skin = 'blue',
  dashboardHeader(title = 'NBA Stats 2023'),
  dashboardSidebar( # Creating sidebar tabs to display multiple pages
    sidebarMenu(
      id = 'sidebarID',
      # Sidebar menu items generally contain an id to be referenced in stylesheets, a tabName used for linking, and an icon
      menuItem(
        'Efficency',
        tabName = 'dot',
        icon = icon('random', lib = 'glyphicon')
      ),
      menuItem(
        'Points Per Game',
        tabName = 's1',
        icon = icon('stats', lib = 'glyphicon')
      ),
      menuItem(
        'Top 50 Players',
        tabName = 's2',
        icon = icon('align-left', lib = 'glyphicon')
      )
    )
  ),
  #Setting Dashboard body up
  dashboardBody(
    uiOutput('dashboard_theme'),
    tabItems(
      #Creating page for efficiency graph
    tabItem(
      'dot',
      navbarPage( # Navbar creation to access multiple graphs
        title = 'Efficency Stats',
        id = 'eff_navbar',
        fluid = TRUE,
        collapsible = TRUE,
        tabPanel('Effiency by Player',
      fluidPage(
        fluidRow(),
        fluidRow(
        column(12,div(
          selectizeInput(
                'Player',
                label = 'Select up to 150 Players:',
                choices = playerinput,
                multiple = TRUE,
                selected = player_selected,
                width = '500%',
                # This plugin creates an 'x'-button that users can click to more easily remove selected items
                options = list(plugins = list("remove_button"),maxItems = 150)
              )
          ))),
        fluidRow(plotlyOutput('Effbyplayer', height = 'auto'))
        )),
      tabPanel('Effiency by Age',
               fluidPage(
                 fluidRow(),
                 fluidRow(
                   column(12,div(
                     selectizeInput(
                       'Age',
                       label = 'Select Ages:',
                       choices = ageinput,
                       multiple = TRUE,
                       selected = age_selected,
                       width = '200%',
                       # This plugin creates an 'x'-button that users can click to more easily remove selected items
                       options = list(plugins = list("remove_button"))
                     )
                   ))),
                 fluidRow(plotlyOutput('Effbyage', height = 'auto'))
               )),
      tabPanel('Effiency by Team',
               fluidPage(
                 fluidRow(),
                 fluidRow(
                   column(12,div(
                     selectizeInput(
                       'Team',
                       label = 'Select Teams:',
                       choices = teaminput,
                       multiple = TRUE,
                       selected = team_selected,
                       width = '200%',
                       # This plugin creates an 'x'-button that users can click to more easily remove selected items
                       options = list(plugins = list("remove_button"))
                     )
                   ))),
                 fluidRow(plotlyOutput('Effbyteam', height = 'auto'))
               )),
      tabPanel('Effiency by Position',
               fluidPage(
                 fluidRow(),
                 fluidRow(
                   column(12,div(
                     selectizeInput(
                       'POS',
                       label = 'Select Positions:',
                       choices = posinput,
                       multiple = TRUE,
                       selected = pos_selected,
                       width = '200%',
                       # This plugin creates an 'x'-button that users can click to more easily remove selected items
                       options = list(plugins = list("remove_button"))
                     )
                   ))),
                 fluidRow(plotlyOutput('Effbypos', height = 'auto'))
               ))
      
      )
      ),
    #Creating the 2nd drop down page for average points per game
    tabItem(
      's1',
      fluidPage(
        fluidRow(plotlyOutput('pbam', height = 'auto')),
        fluidRow(column(4,div(
          selectInput(
            inputId = 'catagory',
            label = 'Select A Catagory:',
            choices = c('Position' = 'POS',
                        'Team' = 'Team'),
            width = '100%'
          )))))
    ),
    #Creation of the top 50 players by given stat 
    tabItem(
      's2',
      navbarPage(
        title = 'Top 50 Players',
        id = 'top50_navbar',
        fluid = TRUE,
        collapsible = TRUE,
        tabPanel('Points',
      fluidPage(
        plotlyOutput('T50Pts',height = 'auto'),
        fluidRow(selectInput(
          inputId = 'color',
          label = 'Select A Catagory for Color:',
          choices = c('Position' = 'POS',
                      'Team' = 'Team',
                      'Age' = 'Age'),
          width = '50%'
        )))
          ),
      tabPanel('Points Per Game',
               fluidPage(
                 plotlyOutput('T50PPG',height = 'auto'),
                 fluidRow(selectInput(
                   inputId = 'color9',
                   label = 'Select A Catagory for Color:',
                   choices = c('Position' = 'POS',
                               'Team' = 'Team',
                               'Age' = 'Age'),
                   width = '50%'
                 )))
      ),
      tabPanel('3 Point %',
               fluidPage(
                 plotlyOutput('T503xpm.',height = 'auto'),
                 fluidRow(selectInput(
                   inputId = 'color1',
                   label = 'Select A Catagory for Color:',
                   choices = c('Position' = 'POS',
                               'Team' = 'Team',
                               'Age' = 'Age'),
                   width = '50%'
                 )))
      ),
      tabPanel('Rebounds',
               fluidPage(
                 plotlyOutput('T50Reb',height = 'auto'),
                 fluidRow(selectInput(
                   inputId = 'color2',
                   label = 'Select A Catagory for Color:',
                   choices = c('Position' = 'POS',
                               'Team' = 'Team',
                               'Age' = 'Age'),
                   width = '50%'
                 )))
      ),
      tabPanel('Assists',
               fluidPage(
                 plotlyOutput('T50Ast',height = 'auto'),
                 fluidRow(selectInput(
                   inputId = 'color3',
                   label = 'Select A Catagory for Color:',
                   choices = c('Position' = 'POS',
                               'Team' = 'Team',
                               'Age' = 'Age'),
                   width = '50%'
                 )))
      ),
      tabPanel('Steals',
               fluidPage(
                 fluidRow(plotlyOutput('T50Stl',height = 'auto')),
                 fluidRow(selectInput(
                   inputId = 'color4',
                   label = 'Select A Catagory for Color:',
                   choices = c('Position' = 'POS',
                               'Team' = 'Team',
                               'Age' = 'Age'),
                   width = '50%'
                 )))
      ),
      tabPanel('Double Doubles',
               fluidPage(
                 plotlyOutput('T50DD',height = 'auto'),
                 fluidRow(selectInput(
                   inputId = 'color5',
                   label = 'Select A Catagory for Color:',
                   choices = c('Position' = 'POS',
                               'Team' = 'Team',
                               'Age' = 'Age'),
                   width = '50%'
                 )))
      ),
      tabPanel('Triple Doubles',
               fluidPage(
                 plotlyOutput('T50TD',height = 'auto'),
                 fluidRow(selectInput(
                   inputId = 'color6',
                   label = 'Select A Catagory for Color:',
                   choices = c('Position' = 'POS',
                               'Team' = 'Team',
                               'Age' = 'Age'),
                   width = '50%'
                 )))
      )
    ))
    )
      # tabItems
    )
    # dashboardBody
  )
  # dashboardPage
server <- function(input, output, session) {
  output$Effbyplayer <- renderPlotly({
    # GGplotly changes the normal ggplot into a plotly plot in order to allow different functions for the users to use such as changing hovermodes and zooming.
    ggplotly(
      # Functions that creates a scatter plot that eneters all the data to be used by geom_bar.
      ggplot(
        # Filters data to include only entries in given input
        filter(selection, Player %in% input$Player),
        aes(text=paste("Player:",Player),fill = POS,x = Eff,y=FGA)) +
        # Creates the bars stacked based on the fill variable specified above in aes
        geom_point() +
        labs(title = str_wrap('Player Efficiency Based on Total Field Goals Attempted', 75),
             y = "Field Goals Attempted", x = 'Efficiency',
             fill = '<b> Position </b>'
        ))
  })
  output$Effbyage <- renderPlotly({
    # GGplotly changes the normal ggplot into a plotly plot in order to allow different functions for the users to use such as changing hovermodes and zooming.
    ggplotly(
      # Functions that creates a scatter plot that eneters all the data to be used by geom_bar.
      ggplot(
        # Filters data to include only entries in given input
        filter(selection, Age %in% input$Age),
        aes(text=paste("Player:",Player,"\nAge:",Age),fill = POS,x = Eff,y=FGA)) +
        # Creates the bars stacked based on the fill variable specified above in aes
        geom_point() +
        labs(title = str_wrap('Player Efficiency Based on Total Field Goals Attempted', 75),
             y = "Field Goals Attempted", x = 'Efficiency',
             fill = '<b> Position </b>'
        ))
  })
  
  output$Effbyteam <- renderPlotly({
    # GGplotly changes the normal ggplot into a plotly plot in order to allow different functions for the users to use such as changing hovermodes and zooming.
    ggplotly(
      # Functions that creates a scatter plot that eneters all the data to be used by geom_bar.
      ggplot(
        # Filters data to include only entries in given input
        filter(selection, Team %in% input$Team),
        aes(text=paste("Player:",Player,"\nTeam:",Team),fill = POS,x = Eff,y=FGA)) +
        # Creates the bars stacked based on the fill variable specified above in aes
        geom_point() +
        labs(title = str_wrap('Player Efficiency Based on Total Field Goals Attempted', 75),
             y = "Field Goals Attempted", x = 'Efficiency',
             fill = '<b> Position </b>'
        ))
  })

  output$Effbypos <- renderPlotly({
    # GGplotly changes the normal ggplot into a plotly plot in order to allow different functions for the users to use such as changing hovermodes and zooming.
    ggplotly(
      # Functions that creates a scatter plot that eneters all the data to be used by geom_bar.
      ggplot(
        # Filters data to include only entries in given input
        filter(selection, POS %in% input$POS),
        aes(text=paste("Player:",Player),fill = POS,x = Eff,y=FGA)) +
        # Creates the bars stacked based on the fill variable specified above in aes
        geom_point() +
        labs(title = str_wrap('Player Efficiency Based on Total Field Goals Attempted', 75),
             y = "Field Goals Attempted", x = 'Efficiency',
             fill = '<b> Position </b>'
        ))
  })
  output$pbam<- renderPlotly({
    ggplotly(
      ggplot(
        selection,aes_string(x=input$catagory,y="PPG"))+
      geom_boxplot(varwidth=T, fill="plum")+
      labs(title = str_wrap('Points per Game For All Players by Input Variable', 50),
             y = "Points per Game", x = input$catagory)+theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
    )
  })
  output$T50Pts<- renderPlotly({
    ggplotly(
      ggplot(head(arrange(selection,desc(PTS)),50), aes(x = reorder(Player,-PTS), y = PTS)) +
      geom_bar(stat = "identity",aes_string(fill=input$color))+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        labs(title = str_wrap('Top 50 Players By Points', 75),
             y = "Number of Points", x = 'Player',
             fill = input$color
        )
    ,tooltip = c('y','fill'))
  })
  output$T50PPG<- renderPlotly({
    ggplotly(
      ggplot(head(arrange(selection,desc(PPG)),50), aes(x = reorder(Player,-PPG), y = PPG)) +
        geom_bar(stat = "identity",aes_string(fill=input$color9))+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        labs(title = str_wrap('Top 50 Players By Points', 75),
             y = "Points Per Game", x = 'Player',
             fill = input$color9
        )
      ,tooltip = c('y','fill'))
  })
  output$T503xpm.<- renderPlotly({
    ggplotly(
      ggplot(head(arrange(selection,desc(X3P.)),50), aes(x = reorder(Player,-X3P.), y = X3P.)) +
        geom_bar(stat = "identity",aes_string(fill=input$color1))+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        labs(title = str_wrap('Top 50 Players By Three Point Percent', 75),
             y = "3 Point %", x = 'Player',
             fill = input$color1
        )
      ,tooltip = c('y','fill'))
  })
  output$T50Reb<- renderPlotly({
    ggplotly(
      ggplot(head(arrange(selection,desc(REB)),50), aes(x = reorder(Player,-REB), y = REB)) +
        geom_bar(stat = "identity",aes_string(fill=input$color2))+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        labs(title = str_wrap('Top 50 Players By Rebounds', 75),
             y = "Number of Rebounds", x = 'Player',
             fill = input$color2
        )
      ,tooltip = c('y','fill'))
  })
  output$T50Ast<- renderPlotly({
    ggplotly(
      ggplot(head(arrange(selection,desc(AST)),50), aes(x = reorder(Player,-AST), y = AST)) +
        geom_bar(stat = "identity",aes_string(fill=input$color3))+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        labs(title = str_wrap('Top 50 Players By Assists', 75),
             y = "Number of Assists", x = 'Player',
             fill = input$color3
        )
      ,tooltip = c('y','fill'))
  })
  output$T50Stl<- renderPlotly({
    ggplotly(
      ggplot(head(arrange(selection,desc(STL)),50), aes(x = reorder(Player,-STL), y = STL)) +
        geom_bar(stat = "identity",aes_string(fill=input$color4))+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        labs(title = str_wrap('Top 50 Players By Steals', 75),
             y = "Number of Steals", x = 'Player',
             fill = input$color4
        )
      ,tooltip = c('y','fill'))
  })
  output$T50DD<- renderPlotly({
    ggplotly(
      ggplot(head(arrange(selection,desc(DD2)),50), aes(x = reorder(Player,-DD2), y = DD2)) +
        geom_bar(stat = "identity",aes_string(fill=input$color5))+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        labs(title = str_wrap('Top 50 Players By Double Doubles', 75),
             y = "Number of Double Doubles", x = 'Player',
             fill = input$color5
        )
      ,tooltip = c('y','fill'))
  })
  output$T50TD<- renderPlotly({
    ggplotly(
      ggplot(head(arrange(selection,desc(TD3)),50), aes(x = reorder(Player,-TD3), y = TD3)) +
        geom_bar(stat = "identity",aes_string(fill=input$color6))+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        labs(title = str_wrap('Top 50 Players By Triple Doubles', 75),
             y = "Number of Triple Doubles", x = 'Player',
             fill = input$color6
        )
      ,tooltip = c('y','fill'))
  })

}
shinyApp(ui, server)
