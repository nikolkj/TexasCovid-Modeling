#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

rm(list = ls());

# Load Tokens
drop_token = readRDS(file = "../dropbox_token.rds")

# Load general packages 
require(tidyverse)

# Load shiny packages
require(shiny)
require(shinydashboard)
require(dashboardthemes) # experimental, install_github("nik01010/dashboardthemes")
require(plotly, quietly = TRUE)

# LOAD REQUIRED DATA ---
# Fetch population data
pop <<- readRDS(file = "../texas-demographics_county-populations_segmented.RDS")

# Fetch main data
dat <<- readr::read_csv(
        file = "https://raw.githubusercontent.com/nikolkj/Texas-Covid/master/daily-county-data/Texas-County-Main.csv",
        col_names = TRUE,
        col_types = cols(
            County = readr::col_factor(),
            Date = readr::col_date(),
            DailyCount_cases = readr::col_integer(),
            DailyDelta_cases = readr::col_integer(),
            DailyCount_tests = readr::col_integer(),
            DailyDelta_tests = readr::col_integer(),
            DailyCount_deaths = readr::col_integer(),
            DailyDelta_deaths = readr::col_integer(),
            LastUpdateDate = readr::col_date()
        ),
        na = ""
    ) %>% select(-LastUpdateDate) 

# PREPARE DATA-OBJECTS -----
# # County-level Data
# dat <<- dat_ts %>% 
#     select(County, data) %>% 
#     unnest(data = ., cols = c(data))

# State-level Data
dat_state <<- dat %>% 
    select(-County) %>% 
    group_by(Date) %>% 
    summarise_at(vars(-group_cols()), sum, na.rm = TRUE)

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

# County-level Data
dat_county <<- dat %>% 
    group_by(County) %>%
    arrange(Date) %>% 
    nest() %>%
    ungroup() 

# County-level Models
# ... Generated mod_DailyCount-*.R scripts
# ... Daily copies stored in "daily-dashboard-objects/".
# ...
# ... Update as necessary.
# ... This should be updated to a remotely read-binary in production
# ... ... and used to generate "dat_county", above.

# Pull Model Data From Dropbox
rdrop2::drop_download(path = "Texas-Covid/mod_DailyCount-cases.RDS", local_path = "../rdrop_dowloads/", overwrite = TRUE, dtoken = drop_token )
rdrop2::drop_download(path = "Texas-Covid/mod_DailyCount-tests.RDS", local_path = "../rdrop_dowloads/", overwrite = TRUE, dtoken = drop_token )
rdrop2::drop_download(path = "Texas-Covid/mod_DailyCount-deaths.RDS", local_path = "../rdrop_dowloads/", overwrite = TRUE, dtoken = drop_token )

mod_county_cases <<- readRDS("../rdrop_dowloads/mod_DailyCount-cases.RDS") 
mod_county_tests <<- readRDS("../rdrop_dowloads/mod_DailyCount-tests.RDS") 
mod_county_deaths <<- readRDS("../rdrop_dowloads/mod_DailyCount-deaths.RDS") 

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
        menuItem(text = "County Data",
                 tabName = "tab_dash_county", 
                 icon = shiny::icon(name = "square", class = "fa-1x",lib = "font-awesome")),
        menuItem(text = "Community Data",
                 tabName = "tab_dash_community", 
                 icon = shiny::icon(name = "th-large", class = "fa-1x",lib = "font-awesome")),
        menuItem(text = "State Data",
                 tabName = "tab_dash_state", 
                 icon = shiny::icon(name = "th", class = "fa-1x",lib = "font-awesome")),
        menuItem(text = "About", 
                 tabName = "tab_other_about",
                 icon = shiny::icon(name = "info-circle", class = "fa-1x",lib = "font-awesome")),
        menuItem(text = "Supporters", 
                 tabName = "tab_other_support",
                 icon = shiny::icon(name = "thumbs-up", class = "fa-1x",lib = "font-awesome"))
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
                                    HTML(paste0("<br>Appx. <b>", state.comps.new_tests, "%</b> of the days were better.</br>")))
                           
                    ),
                    
                    tabBox(title = tagList(shiny::icon(name = "ambulance", class = "fa-1x",lib = "font-awesome"), 
                                           HTML("<b>Cases</b>")), selected = "Daily",
                           tabPanel("Total",
                                    plotly::plotlyOutput(outputId = "plot.state.total_cases.line")),
                           tabPanel("Daily",
                                    plotly::plotlyOutput(outputId = "plot.state.daily_cases.bar"),
                                    HTML(paste0("<br>Appx. <b>", state.comps.new_cases, "%</b> of the days were better.</br>"))
                           )
                           
                    )
                    
                ),
                
                fluidRow(
                    tabBox(title = tagList(shiny::icon(name = "skull", class = "fa-1x",lib = "font-awesome"), 
                                           HTML("<b>Deaths</b>")), selected = "Daily",
                           tabPanel("Total",
                                    plotly::plotlyOutput(outputId = "plot.state.total_deaths.line")),
                           tabPanel("Daily",
                                    plotly::plotlyOutput(outputId = "plot.state.daily_deaths.bar"),
                                    HTML(paste0("<br>Appx. <b>", state.comps.new_deaths, "%</b> of the days were better.</br>")))
                           
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
                                    plotly::plotlyOutput(outputId = "comm.rates_infected.line", height = "750px")),
                           tabPanel("Testing Rate",
                                    plotly::plotlyOutput(outputId = "comm.rates_tested.line", height = "750px")),
                           tabPanel("Mortality Rate", 
                                    plotly::plotlyOutput(outputId = "comm.rates_mortality.line", height = "750px"))
                    ),
                    tabBox(title = tagList(shiny::icon(name = "info-circle", class = "fa-1x",lib = "font-awesome"), 
                                           HTML("<b>Segmentation Details</b>")), selected = "Map",
                           tabPanel("Map", 
                                    plotlyOutput(outputId = "comm.info_segementation.map")),
                           tabPanel("Curve", 
                                    shiny::plotOutput(outputId = "comm.info_segementation.hist", width = "700px", height = "600px"))
                    )
                )),
        tabItem(tabName = "tab_dash_county", 
                fluidPage(
                fluidRow(
                    shiny::column(width = 4, uiOutput("select.county_name"), offset = 0)
                ),
                fluidRow(shiny::column(width = 6,
                                       fluidRow(
                                           tabBox(title = tagList(shiny::icon(name = "stethoscope", class = "fa-1x",lib = "font-awesome"), 
                                                                  HTML("<b>Tests</b>")), selected = "Daily", width = 12,
                                                  tabPanel("Total",
                                                           plotly::plotlyOutput(outputId = "plot.county.total_tests.line")
                                                           ),
                                                  tabPanel("Daily",
                                                           plotly::plotlyOutput(outputId = "plot.county.daily_tests.bar"),
                                                           htmlOutput("text.county.daily_tests_comp")
                                                           # HTML(paste0("<br>Appx. <b>", state.comps.new_tests, "%</b> of the days were better.</br>"))),
                                                  ),
                                                  tabPanel("Forecasts", 
                                                           plotly::plotlyOutput(outputId = "plot.county.forecasts_tests_total.line")
                                                           )
                                                  )
                                           ),
                                       fluidRow(tabBox(
                                           title = tagList(
                                               shiny::icon(name = "ambulance", class = "fa-1x", lib = "font-awesome"),
                                               HTML("<b>Cases</b>")
                                           ),
                                           selected = "Daily",
                                           width = 12,
                                           tabPanel(
                                               "Total",
                                               plotly::plotlyOutput(outputId = "plot.county.total_cases.line")
                                           ),
                                           tabPanel(
                                               "Daily",
                                               plotly::plotlyOutput(outputId = "plot.county.daily_cases.bar"),
                                               htmlOutput("text.county.daily_cases_comp")
                                               # HTML(paste0("<br>Appx. <b>", textOutput("text.county.daily_cases_comp"), "%</b> of the days were better.</br>"))
                                               # uiOutput(outputId = "text.county.daily_cases_comp_html")   
                                        ),
                                           tabPanel("Forecasts",
                                                    plotly::plotlyOutput(outputId = "plot.county.forecasts_cases_total.line")
                                                    )
                                       )),
                                       fluidRow(tabBox(
                                           title = tagList(
                                               shiny::icon(name = "skull", class = "fa-1x", lib = "font-awesome"),
                                               HTML("<b>Deaths</b>")
                                           ),
                                           selected = "Daily",
                                           width = 12,
                                           tabPanel(
                                               "Total",
                                               plotly::plotlyOutput(outputId = "plot.county.total_deaths.line")
                                           ),
                                           tabPanel(
                                               "Daily",
                                               plotly::plotlyOutput(outputId = "plot.county.daily_deaths.bar"),
                                               htmlOutput("text.county.daily_deaths_comp")
                                               # HTML(paste0("<br>Appx. <b>", state.comps.new_deaths, "%</b> of the days were better.</br>"))
                                           ),
                                           tabPanel("Forecasts",
                                                    plotly::plotlyOutput(outputId = "plot.county.forecasts_deaths_total.line")
                                                    )
                                           
                                       ))
                                       ),
                         shiny::column(width = 6,
                                       fluidRow(
                                           tabBox(
                                               title = tagList(
                                                   shiny::icon(name = "info-circle", class = "fa-1x", lib = "font-awesome"),
                                                   HTML("<b>Segmentation Details</b>")
                                               ), width = 12,
                                               tabPanel(
                                                   "Map",
                                                   plotlyOutput(
                                                       outputId = "county.info_segementation.map"
                                                   )
                                               )
                                           )
                                       ),
                                       fluidRow(tabBox(
                                           title = tagList(
                                               shiny::icon(name = "compass", class = "fa-1x", lib = "font-awesome"),
                                               HTML("<b>Rates</b>")
                                           ),
                                           width = 12,
                                           tabPanel(
                                               "Infection Rate",
                                               plotly::plotlyOutput(outputId = "county.rates_infected.line")
                                           ),
                                           tabPanel(
                                               "Detection Rate",
                                               plotly::plotlyOutput(outputId = "plot.county.rate_detection.line")
                                           ),
                                           tabPanel(
                                               "Mortality Rate",
                                               plotly::plotlyOutput(outputId = "plot.county.rate_mortality.line")
                                           ),
                                           tabPanel("Notes",
                                                    "... Coming Soon ...")
                                       ))
                                       ))
                )), 
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

