#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# LOAD EXTERNAL PLOTS ----
plot_TexasCommunities.hist = readRDS(file = "../plot_Texas-Communities_Population-Segmentation.RDS")
plot_TexasCommunities.map = readRDS(file = "../plot_Texas-Communities_Population-Segmentation-Map.RDS")

# DEF. GENERIC PLOTTING PARAMETERS ----
# >>FONTS<<
plotly_titlefont.axis = list(family = "Courier New, monospace",
                             size = 18,
                             color = "#7f7f7f")
# >>AXIS FORMATTING<<
plotly_axisformat.date = list(
    titlefont = plotly_titlefont.axis,
    type = "date",
    tickformat = "%m/%d",
    tickangle = -15
)

# >>PLOTTING COLORS<<
# @90% opacity, unless noted otherwise
plotly_color.cases = "#ffe77c" # @95% opacity
plotly_color.tests = "#5beb90" 
plotly_color.deaths = "#ff4837"

# Define Server-side Operations ----
shinyServer(function(input, output) {
    
    # PLOTS: "state.daily_*" ----
    output$plot.state.daily_cases.bar = renderPlotly({
        p = dat_state %>%
            plot_ly(
                data = .,
                x = ~ Date,
                y = ~ DailyDelta_cases,
                marker = list(color = plotly_color.cases),
                opacity = .95,
                type = "bar"
            ) %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date,
                yaxis = list(title = "Daily Cases", titlefont = plotly_titlefont.axis)
            )
        
        p
    })
    
    output$plot.state.daily_tests.bar = renderPlotly({
        p = dat_state %>%
            plot_ly(
                data = .,
                x = ~ Date,
                y = ~ DailyDelta_tests,
                marker = list(color = plotly_color.tests),
                opacity = .90,
                type = "bar"
            ) %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date,
                yaxis = list(title = "Daily Tests", titlefont = plotly_titlefont.axis)
            )
        
        p
    })
    
    output$plot.state.daily_deaths.bar = renderPlotly({
        p = dat_state %>%
            plot_ly(
                data = .,
                x = ~ Date,
                y = ~ DailyDelta_deaths,
                marker = list(color = plotly_color.deaths),
                opacity = .90,
                type = "bar"
            ) %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date,
                yaxis = list(title = "Daily Deaths", titlefont = plotly_titlefont.axis)
            )
        
        p
    })
    
    output$plot.state.daily_cases.curveicon = renderPlot({
        temp
    })
    
    # PLOTS: "state.total_*" ----
    output$plot.state.total_cases.line = renderPlotly({
        p = dat_state %>%
            plot_ly(
                data = .,
                x = ~ Date,
                y = ~ DailyCount_cases,
                line = list(color = plotly_color.cases, width = 8),
                opacity = .95,
                type = 'scatter', mode = 'lines'
            ) %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date,
                yaxis = list(title = "Total Cases", titlefont = plotly_titlefont.axis, type = "log")
            )
        
        p
    })
    
    output$plot.state.total_tests.line = renderPlotly({
        p = dat_state %>%
            plot_ly(
                data = .,
                x = ~ Date,
                y = ~ DailyCount_tests,
                line = list(color = plotly_color.tests, width = 8),
                opacity = .90,
                type = 'scatter', mode = 'lines'
            ) %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date,
                yaxis = list(title = "Total Tests", titlefont = plotly_titlefont.axis, type = "log")
            )
        
        p
    })
    
    output$plot.state.total_deaths.line = renderPlotly({
        p = dat_state %>%
            plot_ly(
                data = .,
                x = ~ Date,
                y = ~ DailyCount_deaths,
                line = list(color = plotly_color.deaths, width = 8),
                opacity = .90,
                type = 'scatter', mode = 'lines'
            ) %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date,
                yaxis = list(title = "Total Deaths", titlefont = plotly_titlefont.axis, type = "log")
            )
        
        p
    })
    
    # PLOTS: "state.rate_*" ----
    # Rates should use numerator data as COLOR and OPACITY reference
    output$plot.state.rate_detection.line = renderPlotly({
        p = dat_state %>%
            mutate(daily_detection = round((DailyDelta_cases/DailyDelta_tests), 2)) %>%
            filter(!is.na(daily_detection)) %>%
            plot_ly(
                data = .,
                x = ~ Date,
                y = ~ daily_detection,
                line = list(color = plotly_color.cases, width = 5),
                opacity = .95,
                type = 'scatter', mode = 'lines'
            ) %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date,
                yaxis = list(title = "Detection Rate", titlefont = plotly_titlefont.axis, tickformat = ".2%")
            )
        
        p
        
        
    })
    
    output$plot.state.rate_mortality.line = renderPlotly({
        p = dat_state %>%
            mutate(daily_mortality = (DailyDelta_deaths/DailyDelta_cases)) %>%
            filter(!is.na(daily_mortality)) %>%
            plot_ly(
                data = .,
                x = ~ Date,
                y = ~ daily_mortality,
                line = list(color = plotly_color.deaths, width = 5),
                opacity = .90,
                type = 'scatter', mode = 'lines'
            ) %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date,
                yaxis = list(title = "Mortality Rate", titlefont = plotly_titlefont.axis, tickformat = ".2%")
            )
        
        p
        
        
    })
    
    # PLOTS: "comm.rates_*"
    # Rates should use numerator data as COLOR and OPACITY reference
    output$comm.rates_infected.line = renderPlotly({
        f1 = dat_pop %>%
            group_by(Date, pop_group) %>%
            mutate(pcnt_infected = DailyCount_cases/Population) %>% 
            filter(!is.na(pcnt_infected)) %>%
            select(Date, pop_group, pcnt_infected) %>%
            ungroup() %>%
            arrange(pop_group) %>%
            plot_ly(data = ., 
                    x = ~Date,
                    y = ~pcnt_infected,
                    color = ~pop_group,
                    legendgroup = ~pop_group,
                    mode = 'lines', 
                    line = list(width = 3),
                    type = 'scatter'
            ) %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date,
                yaxis = list(title = "Pcnt Population Infected", titlefont = plotly_titlefont.axis, tickformat = ".2%")
            ) 
        
        f2 = dat_pop %>%
            group_by(Date, pop_group) %>%
            mutate(pcnt_infected = DailyDelta_cases/Population) %>% 
            filter(!is.na(pcnt_infected)) %>%
            select(Date, pop_group, pcnt_infected) %>%
            ungroup() %>%
            arrange(pop_group) %>%
            plot_ly(data = ., 
                    x = ~Date,
                    y = ~pcnt_infected,
                    color = ~pop_group,
                    legendgroup = ~pop_group,
                    mode = 'lines', 
                    line = list(width = 3),
                    showlegend = FALSE,
                    type = 'scatter', 
                    showlegend = FALSE
            ) %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date,
                yaxis = list(title = "Daily Infection Rate", titlefont = plotly_titlefont.axis, tickformat = ".2%")
            )
        
        fig = subplot(f1, f2, nrows = 2, shareX = TRUE, shareY = FALSE, titleX = TRUE, titleY = TRUE)
        fig
    })
    
    output$comm.rates_tested.line = renderPlotly({
        f1 = dat_pop %>%
            group_by(Date, pop_group) %>%
            mutate(pcnt_tested = DailyCount_tests/Population) %>% 
            filter(!is.na(pcnt_tested)) %>%
            select(Date, pop_group, pcnt_tested) %>%
            ungroup() %>%
            plot_ly(data = ., 
                    x = ~Date,
                    y = ~pcnt_tested,
                    color = ~pop_group,
                    legendgroup = ~pop_group,
                    mode = 'lines', 
                    line = list(width = 3),
                    type = 'scatter'
            ) %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date,
                yaxis = list(title = "Pcnt Population Tested", titlefont = plotly_titlefont.axis, tickformat = ".2%")
            )
        
        f2 = dat_pop %>%
            group_by(Date, pop_group) %>%
            mutate(pcnt_tested = DailyDelta_tests/Population) %>% 
            filter(!is.na(pcnt_tested)) %>%
            select(Date, pop_group, pcnt_tested) %>%
            ungroup() %>%
            plot_ly(data = ., 
                    x = ~Date,
                    y = ~pcnt_tested,
                    color = ~pop_group,
                    legendgroup = ~pop_group,
                    mode = 'lines', 
                    line = list(width = 3),
                    type = 'scatter',
                    showlegend = FALSE
            ) %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date,
                yaxis = list(title = "Daily Testing Rate", titlefont = plotly_titlefont.axis, tickformat = ".2%")
            )
        
        fig = subplot(f1, f2, nrows = 2, shareX = TRUE, shareY = FALSE, titleX = TRUE, titleY = TRUE)
        fig
        
    })
    
    output$comm.rates_mortality.line = renderPlotly({
        f1 = 
        dat_pop %>%
            group_by(Date, pop_group) %>%
            mutate(pcnt_mortality = DailyCount_deaths/DailyCount_cases) %>% 
            filter(!is.na(pcnt_mortality)) %>%
            select(Date, pop_group, pcnt_mortality) %>%
            ungroup() %>%
            plot_ly(data = ., 
                    x = ~Date,
                    y = ~pcnt_mortality,
                    color = ~pop_group,
                    legendgroup = ~pop_group,
                    mode = 'lines', 
                    line = list(width = 3),
                    type = 'scatter'
            ) %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date,
                yaxis = list(title = "Case Mortality Rate", titlefont = plotly_titlefont.axis, tickformat = ".2%")
            )
        
        f2 = dat_pop %>%
            group_by(Date, pop_group) %>%
            mutate(pcnt_mortality = DailyDelta_deaths/DailyDelta_cases) %>% 
            filter(!is.na(pcnt_mortality)) %>%
            select(Date, pop_group, pcnt_mortality) %>%
            ungroup() %>%
            plot_ly(data = ., 
                    x = ~Date,
                    y = ~pcnt_mortality,
                    color = ~pop_group,
                    legendgroup = ~pop_group,
                    mode = 'lines', 
                    line = list(width = 3),
                    type = 'scatter', 
                    showlegend = FALSE
            ) %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date,
                yaxis = list(title = "Daily Case Mortality Rate", titlefont = plotly_titlefont.axis, tickformat = ".2%")
            )
        
        fig = subplot(f1, f2, nrows = 2, shareX = TRUE, shareY = FALSE, titleX = TRUE, titleY = TRUE)
        fig
        
    })
    
    output$comm.rates_dailymortality.line = renderPlotly({
        dat_pop %>%
            group_by(Date, pop_group) %>%
            mutate(pcnt_mortality = DailyDelta_deaths/DailyDelta_cases) %>% 
            filter(!is.na(pcnt_mortality)) %>%
            select(Date, pop_group, pcnt_mortality) %>%
            ungroup() %>%
            plot_ly(data = ., 
                    x = ~Date,
                    y = ~pcnt_mortality,
                    color = ~pop_group,
                    mode = 'lines', 
                    line = list(width = 3),
                    # opacity = .90,
                    type = 'scatter'
            ) %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date,
                yaxis = list(title = "Daily Case Mortality Rate", titlefont = plotly_titlefont.axis, tickformat = ".2%")
            )
    })
    
    # PLOTS: "comm.info_*" ----
    output$comm.info_segementation.hist = renderPlot({plot_TexasCommunities.hist})
    output$comm.info_segementation.map = renderPlot({plot_TexasCommunities.map})
    
})
