#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load general packages 
require(tidyverse)

# Load shiny packages
require(shiny)
require(shinydashboard)
require(dashboardthemes) # experimental, install_github("nik01010/dashboardthemes")
require(plotly, quietly = TRUE)

# Define sidebar contents
ui_sidebar = dashboardSidebar(
    sidebarMenu(
        menuItem(text = "State Data",
                 tabName = "tab_dash_state"),
        menuItem(text = "Community Data",
                 tabName = "tab_dash_community"),
        menuItem(text = "County Data",
                 tabName = "tab_dash_county"),
        menuItem(text = "About", 
                 tabName = "tab_other_about"),
        menuItem(text = "Supporters", 
                 tabName = "tab_other_support")
    )
)

# Define body & tab contents
ui_body = dashboardBody(
    dashboardthemes::shinyDashboardThemes(theme = "grey_dark"), # set theme from package::dashboardthemes
    tabItems(
        tabItem(tabName = "tab_dash_state", 
                h2("Daily Cases"),
                plotly::plotlyOutput(outputId = "plot.state.new_cases.bar", width = "50%"),
                
                h2("Daily Tests"),
                plotly::plotlyOutput(outputId = "plot.state.new_tests.bar", width = "50%"),
                
                h2("Daily Deaths"),
                plotly::plotlyOutput(outputId = "plot.state.new_deaths.bar", width = "50%"),
        ),
        tabItem(tabName = "tab_dash_community", "community"),
        tabItem(tabName = "tab_dash_county", "county"),
        tabItem(tabName = "tab_other_about", "about"),
        tabItem(tabName = "tab_other_support", "support")
    )
)


# Render ui
dashboardPage(
    dashboardHeader(title = "Texas Covid-19 Dashboard"),
    ui_sidebar,
    ui_body
)

