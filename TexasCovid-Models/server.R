#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load Required Data
dat_ts = readRDS(file = "/home/niko/Documents/R-projects/TexasCovid-modeling/dat_ts.RDS")

# Prepare Data-Objects -----
# County-level Data
dat = dat_ts %>% 
    select(County, data) %>% 
    unnest(data = ., cols = c(data))

# State-level Data
dat_state = dat %>% 
    select(-County) %>% 
    group_by(Date) %>% 
    summarise_at(vars(-group_cols()), sum)

# Define Generic Plotting Parameters ----
plotly_titlefont.axis = list(family = "Courier New, monospace",
                             size = 18,
                             color = "#7f7f7f")

# Define Server-side Operations ----
shinyServer(function(input, output) {
    
    
    output$plot.state.new_cases.bar = renderPlotly({
        p = dat_state %>%
            plot_ly(
                data = .,
                x = ~ Date,
                y = ~ DailyDelta_cases,
                type = "bar"
            ) %>%
            layout(
                p = .,
                xaxis = list(
                    title = "Date", titlefont = plotly_titlefont.axis,
                    type = "date",
                    tickformat = "%m/%d"
                ),
                yaxis = list(title = "Daily Cases", titlefont = plotly_titlefont.axis)
            )
        
        p
    })
    
    output$plot.state.new_tests.bar = renderPlotly({
        p = dat_state %>%
            plot_ly(
                data = .,
                x = ~ Date,
                y = ~ DailyDelta_tests,
                type = "bar"
            ) %>%
            layout(
                p = .,
                xaxis = list(
                    title = "Date", titlefont = plotly_titlefont.axis,
                    type = "date",
                    tickformat = "%m/%d"
                ),
                yaxis = list(title = "Daily Tests", titlefont = plotly_titlefont.axis)
            )
        
        p
    })
    
    output$plot.state.new_deaths.bar = renderPlotly({
        p = dat_state %>%
            plot_ly(
                data = .,
                x = ~ Date,
                y = ~ DailyDelta_cases,
                type = "bar"
            ) %>%
            layout(
                p = .,
                xaxis = list(
                    title = "Date", titlefont = plotly_titlefont.axis,
                    type = "date",
                    tickformat = "%m/%d"
                ),
                yaxis = list(title = "Daily Deaths", titlefont = plotly_titlefont.axis)
            )
        
        p
    })
    
    
    
    
})
