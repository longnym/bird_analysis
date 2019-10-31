library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
dashboardPage(
    dashboardHeader(title='Observation of Birds'),
    dashboardSidebar(
        sidebarMenu(
            menuItem('Map', tabName='map', icon=icon('map')),
            menuItem('Data', tabName='data', icon=icon('database'))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName="map",
                    fluidRow(
                        column(2, selectizeInput('birdName', 'Bird Name', c('All', as.character(breedSeasonDf$common_name)))),
                        column(2, selectizeInput('breedSeason', 'Breed Season', c('All', 'True', 'False'))),
                        column(2, selectizeInput('state', 'State', c('All', unique(state_map$region))))
                    ),
                    fluidRow(
                        box(title='Observation of Birds in USA', width=7, solidHeader=TRUE, status='primary', plotOutput('map')),
                        box(title='Top 10 of State', width=5, plotOutput('state_summary'))
                    ),
                    fluidRow(
                        box(title='Monthly Observation', width=6, plotOutput('month_summary')),
                        box(title='Observation of Birds', width=6, plotOutput('bird_summary'))
                    )
            ),
            tabItem(tabName='data', fluidRow(box(width=12, dataTableOutput('table'))))
        )
    )
)