#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# LOAD REQUIRED DATA ---
# ... Automatically loaded through special "global.R" file

# LOAD PACKAGES ----
# Load shiny packages
require(shiny, quietly = TRUE)
require(shinydashboard, quietly = TRUE)
require(dashboardthemes, quietly = TRUE) # experimental, install_github("nik01010/dashboardthemes")

# Load general packages ...
require(tidyverse)
# zoo
# imputeTS

# Other ...
require(plotly, quietly = TRUE)

# DEFINE UI ELEMENTS ----
# Define sidebar contents
ui_sidebar = dashboardSidebar(
    sidebarMenu(
        menuItem(text = "About", 
                 tabName = "tab_other_about",
                 icon = shiny::icon(name = "info-circle", class = "fa-1x",lib = "font-awesome")),
        menuItem(text = "County Data",
                 tabName = "tab_dash_county", 
                 icon = shiny::icon(name = "th", class = "fa-1x",lib = "font-awesome"), selected = TRUE),
        menuItem(text = "Community Data",
                 tabName = "tab_dash_community", 
                 icon = shiny::icon(name = "th-large", class = "fa-1x",lib = "font-awesome")),
        menuItem(text = "State Data",
                 tabName = "tab_dash_state", 
                 icon = shiny::icon(name = "square", class = "fa-1x",lib = "font-awesome"))
        # menuItem(text = "Supporters", 
        #          tabName = "tab_other_support",
        #          icon = shiny::icon(name = "thumbs-up", class = "fa-1x",lib = "font-awesome"))
    )
)

# Define body & tab contents
ui_body = dashboardBody(
    dashboardthemes::shinyDashboardThemes(theme = "grey_light"), # set theme from package::dashboardthemes
    tabItems(
        tabItem(tabName = "tab_dash_state", 
                fluidPage(
                fluidRow(shinydashboard::box(title = "About: State Data", status = "success", collapsible = TRUE, collapsed = FALSE,
                                             includeHTML("state_dash-info.html"))),
                shiny::hr(),
                fluidRow(
                    tabBox(title = tagList(shiny::icon(name = "stethoscope", class = "fa-1x",lib = "font-awesome"), 
                                           HTML("<b>Tests</b>")), selected = "Daily", 
                           tabPanel("Total",
                                    plotly::plotlyOutput(outputId = "plot.state.total_tests.line")),
                           tabPanel("Daily",
                                    plotly::plotlyOutput(outputId = "plot.state.daily_tests.bar"),
                                    # HTML(paste0("<br>Appx. <b>", state.comps.new_tests, "%</b> of the days were better.</br>"))
                                    htmlOutput("text.state.daily_tests_comp")
                                    )
                           
                    ),
                    
                    tabBox(title = tagList(shiny::icon(name = "ambulance", class = "fa-1x",lib = "font-awesome"), 
                                           HTML("<b>Cases</b>")), selected = "Daily",
                           tabPanel("Total",
                                    plotly::plotlyOutput(outputId = "plot.state.total_cases.line")),
                           tabPanel("Daily",
                                    plotly::plotlyOutput(outputId = "plot.state.daily_cases.bar"),
                                    # HTML(paste0("<br>Appx. <b>", state.comps.new_cases, "%</b> of the days were better.</br>"))
                                    htmlOutput("text.state.daily_cases_comp")
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
                                    # HTML(paste0("<br>Appx. <b>", state.comps.new_deaths, "%</b> of the days were better.</br>"))
                                    htmlOutput("text.state.daily_deaths_comp")
                                    )
                           
                    ),
                    tabBox(title = tagList(shiny::icon(name = "compass", class = "fa-1x",lib = "font-awesome"), 
                                           HTML("<b>Rates</b>")), selected = "Detection",
                           tabPanel("Detection",
                                    plotly::plotlyOutput(outputId = "plot.state.rate_detection.line")),
                           tabPanel("Mortality",
                                    plotly::plotlyOutput(outputId = "plot.state.case_mortality.line")),
                           tabPanel("Definitions", 
                                    shiny::includeHTML(path = "rate-defintions.html"))
                           
                    )
                    
                )
        )),
        tabItem(tabName = "tab_dash_community", 
                fluidPage(
                    fluidRow(shinydashboard::box(title = "About: Community Data", status = "success", collapsible = TRUE, collapsed = FALSE,
                                                 includeHTML("community_dash-info.html"))),
                    shiny::hr(),
                    fluidRow(
                    tabBox(title = tagList(shiny::icon(name = "compass", class = "fa-1x",lib = "font-awesome"), 
                                           HTML("<b>Rates</b>")),
                           tabPanel("Infection",
                                    plotly::plotlyOutput(outputId = "comm.rates_infected.line", height = "750px")),
                           tabPanel("Testing",
                                    plotly::plotlyOutput(outputId = "comm.rates_tested.line", height = "750px")),
                           tabPanel("Detection",
                                    plotly::plotlyOutput(outputId = "comm.rates_detection.line", height = "750px")),
                           tabPanel("Mortality", 
                                    plotly::plotlyOutput(outputId = "comm.rates_mortality.line", height = "750px")),
                           tabPanel("Definitions",
                                    shiny::includeHTML(path = "rate-defintions.html"))
                    ),
                    tabBox(title = tagList(shiny::icon(name = "info-circle", class = "fa-1x",lib = "font-awesome"), 
                                           HTML("<b>Reference</b>")), selected = "Map",
                           tabPanel("Map", 
                                    plotlyOutput(outputId = "comm.info_segementation.map")),
                           tabPanel("Segmentation", 
                                    shiny::plotOutput(outputId = "comm.info_segementation.hist", width = "700px", height = "600px"))
                    )
                ))),
        tabItem(tabName = "tab_dash_county", 
                fluidPage(
                fluidRow(shinydashboard::box(title = "About: County Data", status = "success", collapsible = TRUE, collapsed = FALSE,
                                             includeHTML("county_dash-info.html"))),
                    shiny::hr(),
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
                                                  tabPanel("Trend Forecast", 
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
                                           tabPanel("Trend Forecast",
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
                                           tabPanel("Trend Forecast", 
                                                    plotly::plotlyOutput(outputId = "plot.county.forecasts_deaths_total.line")
                                                    )
                                           
                                       ))
                                       ),
                         shiny::column(width = 6,
                                       fluidRow(
                                           tabBox(
                                               title = tagList(
                                                   shiny::icon(name = "info-circle", class = "fa-1x", lib = "font-awesome"),
                                                   HTML("<b>Reference</b>")
                                               ), width = 12,
                                               tabPanel(
                                                   "Map",
                                                   plotlyOutput(
                                                       outputId = "county.info_segementation.map"
                                                   )
                                               ),
                                               tabPanel(
                                                    "Forecasting Method",
                                                   shiny::includeHTML("forecast-definitions.html")
                                               )
                                           )
                                       ),
                                       fluidRow(tabBox(
                                           title = tagList(
                                               shiny::icon(name = "compass", class = "fa-1x", lib = "font-awesome"),
                                               HTML("<b>Rates</b>")
                                           ),
                                           width = 12,
                                           tabPanel("Risk",
                                                    plotly::plotlyOutput(outputId = "county.rates_risk.line")
                                                    ),
                                           tabPanel(
                                               "Infection",
                                               plotly::plotlyOutput(outputId = "county.rates_infected.line")
                                           ),
                                           tabPanel(
                                               "Detection",
                                               plotly::plotlyOutput(outputId = "plot.county.rate_detection.line")
                                           ),
                                           tabPanel(
                                               "Mortality",
                                               plotly::plotlyOutput(outputId = "plot.county.rate_mortality.line")
                                           ),
                                           tabPanel("Definitions",
                                                    shiny::includeHTML(path = "rate-defintions.html"))
                                       )),
                                       fluidRow(tabBox(
                                           title = tagList(
                                               shiny::icon(name = "procedures", class = "fa-1x", lib = "font-awesome"),
                                               HTML("<b>Regional Hospital Info</b>")
                                           ),
                                           width = 12,
                                           tabPanel("Patients",
                                                    plotly::plotlyOutput(outputId = "tsa.daily_hospitalizations.bar"),
                                                    shiny::htmlOutput(outputId = "tsa_description_out_1")
                                                    ),
                                           tabPanel("Beds", 
                                                    plotly::plotlyOutput(outputId = "tsa.daily_beds.bar"),
                                                    shiny::htmlOutput(outputId = "tsa_description_out_2"),
                                                    shiny::HTML(text = "<br><b>Note:</b> TSA data does not differentiate between ICU and general hospital beds.")
                                                    ),
                                           tabPanel("Patient Mortality",
                                                    plotly::plotlyOutput(outputId = "tsa.rate_patientmortality"),
                                                    shiny::htmlOutput(outputId = "tsa_description_out_3"),
                                                    shiny::HTML(text = "<br><b>Patient Mortality:</b> 7-day moving average of new daily deaths divided by the prior-day's active patient count.")
                                                    )
                                       ))
                                       ))
                )), 
        tabItem(tabName = "tab_other_about",
                fluidPage(
                    fluidRow(
                           box(status = "success", includeHTML("about-page_summary.html")),
                           box(status = "warning", includeHTML("about-page_details.html"))
                            ),
                    fluidRow(box(status = "primary", includeHTML("about-page_stack.html")))
                    )
                )
        # tabItem(tabName = "tab_other_support", "support")
    )
)


# Render ui
dashboardPage(
    dashboardHeader(title = "TexasCovid"),
    ui_sidebar,
    ui_body
)

