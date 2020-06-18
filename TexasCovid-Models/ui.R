#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

rm(list = ls());

# Load general packages 
require(tidyverse)

# Load shiny packages
require(shiny)
require(shinydashboard)
require(dashboardthemes) # experimental, install_github("nik01010/dashboardthemes")
require(plotly, quietly = TRUE)

# LOAD REQUIRED DATA ---
dat_ts = readRDS(file = "../dat_ts.RDS") # Master data set
pop = readRDS(file = "../texas-demographics_county-populations_segmented.RDS")

# PREPARE DATA-OBJECTS -----
# County-level Data
dat <<- dat_ts %>% 
    select(County, data) %>% 
    unnest(data = ., cols = c(data))

# State-level Data
dat_state <<- dat %>% 
    select(-County) %>% 
    group_by(Date) %>% 
    summarise_at(vars(-group_cols()), sum)

# Community-level Data
dat_pop <<- dat %>% 
    left_join(x = .,
              y = (pop %>% 
                       select(County, pop_group, jan1_2019_pop_est) %>%
                       rename(Population = jan1_2019_pop_est)
                   ), 
              by = "County") %>%
    select(-County) %>% 
    group_by(pop_group, Date) %>% 
    summarise_at(vars(-group_cols()), sum)
        

# CALCULATE DYNAMIC METRICS ----
# ... Data loaded in "server.r" 

# >>STATE TAB<<
state.comps.new_cases = (sum(dat_state$DailyDelta_cases < dat_state$DailyDelta_cases[nrow(dat_state)]) / nrow(dat_state) * 100) %>%
    round(x = ., digits = 0)

state.comps.new_tests = (sum(dat_state$DailyDelta_tests > dat_state$DailyDelta_tests[nrow(dat_state)]) / nrow(dat_state) * 100) %>%
    round(x = ., digits = 0)

state.comps.new_deaths = (sum(dat_state$DailyDelta_deaths < dat_state$DailyDelta_deaths[nrow(dat_state)]) / nrow(dat_state) * 100) %>%
    round(x = ., digits = 0)

# DEFINE UI ELEMENTS ----
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
                fluidRow(
                    tabBox(title = tagList(shiny::icon(name = "stethoscope", class = "fa-1x",lib = "font-awesome"), 
                                           HTML("<b>Tests</b>")), selected = "Daily",
                           tabPanel("Total",
                                    plotly::plotlyOutput(outputId = "plot.state.total_tests.line")),
                           tabPanel("Daily",
                                    plotly::plotlyOutput(outputId = "plot.state.daily_tests.bar"),
                                    HTML(paste0("<br>Appx. <b>", state.comps.new_tests, "%</b> of the days were better.</br>"))),
                           tabPanel("Forecasts", 
                                    "... Coming Soon ...")
                           
                    ),
                    
                    tabBox(title = tagList(shiny::icon(name = "ambulance", class = "fa-1x",lib = "font-awesome"), 
                                           HTML("<b>Cases</b>")), selected = "Daily",
                           tabPanel("Total",
                                    plotly::plotlyOutput(outputId = "plot.state.total_cases.line")),
                           tabPanel("Daily",
                                    plotly::plotlyOutput(outputId = "plot.state.daily_cases.bar"),
                                    HTML(paste0("<br>Appx. <b>", state.comps.new_cases, "%</b> of the days were better.</br>"))
                           ),
                           tabPanel("Forecasts", 
                                    "... Coming Soon ...")
                           
                    )
                    
                ),
                
                fluidRow(
                    tabBox(title = tagList(shiny::icon(name = "skull", class = "fa-1x",lib = "font-awesome"), 
                                           HTML("<b>Deaths</b>")), selected = "Daily",
                           tabPanel("Total",
                                    plotly::plotlyOutput(outputId = "plot.state.total_deaths.line")),
                           tabPanel("Daily",
                                    plotly::plotlyOutput(outputId = "plot.state.daily_deaths.bar"),
                                    HTML(paste0("<br>Appx. <b>", state.comps.new_deaths, "%</b> of the days were better.</br>"))),
                           tabPanel("Forecasts", 
                                    "... Coming Soon ...")
                           
                    ),
                    tabBox(title = tagList(shiny::icon(name = "compass", class = "fa-1x",lib = "font-awesome"), 
                                           HTML("<b>Rates</b>")), selected = "Detection Rate",
                           tabPanel("Detection Rate",
                                    plotly::plotlyOutput(outputId = "plot.state.rate_detection.line")),
                           tabPanel("Mortality Rate",
                                    plotly::plotlyOutput(outputId = "plot.state.rate_mortality.line")),
                           tabPanel("Notes", 
                                    "... Coming Soon ...")
                           
                    )
                    
                )
        ),
        tabItem(tabName = "tab_dash_community", 
                fluidRow(
                    tabBox(title = tagList(shiny::icon(name = "compass", class = "fa-1x",lib = "font-awesome"), 
                                           HTML("<b>Rates</b>")),
                           tabPanel("Infection Rate",
                                    plotly::plotlyOutput(outputId = "comm.rates_infected.line")),
                           tabPanel("Daily Infection Rate",
                                    plotly::plotlyOutput(outputId = "comm.rates_dailyinfected.line")),
                           tabPanel("Testing Rate",
                                    plotly::plotlyOutput(outputId = "comm.rates_tested.line")),
                           tabPanel("Daily Testing Rate",
                                    plotly::plotlyOutput(outputId = "comm.rates_dailytested.line")),
                           tabPanel("Mortality Rate", 
                                    plotly::plotlyOutput(outputId = "comm.rates_mortality.line")),
                           tabPanel("Daily Mortality Rate", 
                                    plotly::plotlyOutput(outputId = "comm.rates_dailymortality.line"))
                    ),
                    tabBox(title = tagList(shiny::icon(name = "info-circle", class = "fa-1x",lib = "font-awesome"), 
                                           HTML("<b>Segmentation Details</b>")), selected = "Map",
                           tabPanel("Map", 
                                    shiny::plotOutput(outputId = "comm.info_segementation.map", width = "700px", height = "600px")),
                           tabPanel("Curve", 
                                    shiny::plotOutput(outputId = "comm.info_segementation.hist", width = "700px", height = "600px"))
                    )
                )),
        tabItem(tabName = "tab_dash_county", 
                shiny::fluidRow(
                    tabBox(title = tagList(shiny::icon(name = "compass", class = "fa-1x",lib = "font-awesome"), 
                                           HTML("<b>Rates</b>")),
                           tabPanel("Infection Rate",
                                    "asdf")
                    )
                )
                ),
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

