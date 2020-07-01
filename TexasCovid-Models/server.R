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

plotly_titlefont.plot = list(family = "Arial, bold",
                             size = 20,
                             color = "#584f73")

# >>TITLES<<
plotly_titleformat.plot = function(plot_title){
    
    return(list(
        text = plot_title,
        xanchor = "left", yanchor = "top",
        x = 0.08, y = 1, yref = "paper",
        font = plotly_titlefont.plot
    ))
}

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
plotly_color.forecast_point = "#778bff"
plotly_color.forecast_interval = "rgba(243, 205, 255, 0.40)" # #F3CDFF HEXA @40% OPACITY

# Define Server-side Operations ----
shinyServer(function(input, output) {
    # UI Dynamic Objects
    output$select.county_name = renderUI(
        selectInput(inputId = "input_county", label = "Select County:", 
                    choices = unique(as.character(dat$County)), 
                    multiple = FALSE, selected = "Harris")
    )
    
    
    # PLOTS: "state.daily_*" ----
    output$plot.state.daily_cases.bar = renderPlotly({
        p = plot_ly() %>%
            add_trace(
                data = dat_state,
                x = ~ Date,
                y = ~ DailyDelta_cases,
                marker = list(color = plotly_color.cases),
                opacity = .95,
                type = "bar",
                name = "Daily Cases"
            ) 
            
        p = add_trace(p = p, 
                      data = dat_state, 
                      x = ~Date,
                      y = ~ma_cases,
                      name = "7-day Avg.",
                      type = 'scatter', 
                      mode = 'lines',
                      connectgaps = TRUE,
                      line = list(width = 4, color = plotly_color.forecast_point),
                      opacity = 1
                      )
        p = p %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date,
                yaxis = list(title = ""),
                title = plotly_titleformat.plot(plot_title = "Daily Cases"),
                showlegend = FALSE
            )
    })
    
    output$plot.state.daily_tests.bar = renderPlotly({
        p = dat_state %>%
            filter(!is.na(DailyDelta_tests)) %>%
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
                connectgaps = TRUE,
                line = list(color = plotly_color.cases, width = 8),
                opacity = .95,
                type = 'scatter', mode = 'lines'
            ) %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date,
                yaxis = list(title = "Total Cases", titlefont = plotly_titlefont.axis)
            )
        
        p
    })
    
    output$plot.state.total_tests.line = renderPlotly({
        p = dat_state %>%
            filter(!is.na(DailyCount_tests)) %>%
            filter(DailyCount_tests != 0) %>%
            plot_ly(
                data = .,
                x = ~ Date,
                y = ~ DailyCount_tests,
                connectgaps = TRUE,
                line = list(color = plotly_color.tests, width = 8),
                opacity = .90,
                type = 'scatter', mode = 'lines'
            ) %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date,
                yaxis = list(title = "Total Tests", titlefont = plotly_titlefont.axis)
            )
        
        p
    })
    
    output$plot.state.total_deaths.line = renderPlotly({
        p = dat_state %>%
            plot_ly(
                data = .,
                x = ~ Date,
                y = ~ DailyCount_deaths,
                connectgaps = TRUE,
                line = list(color = plotly_color.deaths, width = 8),
                opacity = .90,
                type = 'scatter', mode = 'lines'
            ) %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date,
                yaxis = list(title = "Total Deaths", titlefont = plotly_titlefont.axis)
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
                connectgaps = TRUE,
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
    
    output$plot.state.case_mortality.line = renderPlotly({
        p = dat_state %>%
            mutate(case_mortality = (DailyCount_deaths/DailyCount_cases)) %>%
            filter(!is.na(case_mortality)) %>%
            plot_ly(
                data = .,
                x = ~ Date,
                y = ~ case_mortality,
                connectgaps = TRUE,
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
    
    # PLOTS: "comm.rates_*" ----
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
                    connectgaps = TRUE,
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
                    connectgaps = TRUE,
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
                    connectgaps = TRUE,
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
                    connectgaps = TRUE,
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
    output$comm.info_segementation.map = renderPlotly({
        ggplotly(plot_TexasCommunities.map)
    })
    
    # PLOTS: "county.info_*" ----
    output$county.info_segementation.map = renderPlotly({
        ggplotly(plot_TexasCommunities.map)
    })
    
    # PLOTS: "county.daily_*" ----
    output$plot.county.daily_cases.bar = renderPlotly({
        p = dat_county %>% 
            filter(County == input$input_county) %>%
            unnest(cols = c(data)) %>%
            filter(!is.na(DailyDelta_cases)) %>%
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
    
    output$plot.county.daily_tests.bar = renderPlotly({
        p = dat_county %>% 
            filter(County == input$input_county) %>%
            unnest(cols = c(data)) %>%
            filter(!is.na(DailyDelta_tests)) %>%
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
    
    output$plot.county.daily_deaths.bar = renderPlotly({
        p = dat_county %>% 
            filter(County == input$input_county) %>%
            unnest(cols = c(data)) %>%
            filter(!is.na(DailyDelta_deaths)) %>%
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
    
    # TEXT: "county.daily_*" ----
    output$text.county.daily_cases_comp = renderText({
            temp = dat_county %>%
                filter(County == input$input_county) %>%
                unnest(cols = c(data))


            comp = (sum(temp$DailyDelta_cases < temp$DailyDelta_cases[nrow(temp)], na.rm = TRUE) / nrow(temp) * 100) %>%
            round(x = ., digits = 0)

            paste0("<br>Appx. <b>", as.character(comp), "%</b> of the days were better.</br>")
    })
    
    output$text.county.daily_tests_comp = renderText({
        temp = dat_county %>%
            filter(County == input$input_county) %>%
            unnest(cols = c(data)) %>%
            filter(!is.na(DailyDelta_tests))
        
        comp = (sum(temp$DailyDelta_tests > temp$DailyDelta_tests[nrow(temp)], na.rm = TRUE) / nrow(temp) * 100) %>%
            round(x = ., digits = 0)
        
        paste0("<br>Appx. <b>", as.character(comp), "%</b> of the days were better.</br>")
    })
    
    output$text.county.daily_deaths_comp = renderText({
        temp = dat_county %>%
            filter(County == input$input_county) %>%
            unnest(cols = c(data))
        
        
        comp = (sum(temp$DailyDelta_deaths < temp$DailyDelta_deaths[nrow(temp)], na.rm = TRUE) / nrow(temp) * 100) %>%
            round(x = ., digits = 0)
        
        paste0("<br>Appx. <b>", as.character(comp), "%</b> of the days were better.</br>")
    })
    
    # PLOTS: "county.total_*" ----
    output$plot.county.total_cases.line = renderPlotly({
        p = dat_county %>% 
            filter(County == input$input_county) %>%
            unnest(cols = c(data)) %>%
            filter(!is.na(DailyCount_cases)) %>%
            plot_ly(
                data = .,
                x = ~ Date,
                y = ~ DailyCount_cases,
                connectgaps = TRUE,
                line = list(color = plotly_color.cases, width = 8),
                opacity = .95,
                type = 'scatter', mode = 'lines'
            ) %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date,
                yaxis = list(title = "Total Cases", titlefont = plotly_titlefont.axis)
            )
        
        p
    })
    
    output$plot.county.total_tests.line = renderPlotly({
        p = dat_county %>% 
            filter(County == input$input_county) %>%
            unnest(cols = c(data)) %>%
            filter(!is.na(DailyCount_tests)) %>%
            filter(DailyCount_tests != 0) %>%
            plot_ly(
                data = .,
                x = ~ Date,
                y = ~ DailyCount_tests,
                connectgaps = TRUE,
                line = list(color = plotly_color.tests, width = 8),
                opacity = .90,
                type = 'scatter', mode = 'lines'
            ) %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date,
                yaxis = list(title = "Total Tests", titlefont = plotly_titlefont.axis)
            )
        
        p
    })
    
    output$plot.county.total_deaths.line = renderPlotly({
        p = dat_county %>% 
            filter(County == input$input_county) %>%
            unnest(cols = c(data)) %>%
            filter(!is.na(DailyCount_deaths)) %>%
            plot_ly(
                data = .,
                x = ~ Date,
                y = ~ DailyCount_deaths,
                connectgaps = TRUE,
                line = list(color = plotly_color.deaths, width = 8),
                opacity = .90,
                type = 'scatter', mode = 'lines'
            ) %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date,
                yaxis = list(title = "Total Deaths", titlefont = plotly_titlefont.axis)
            )
        
        p
    })
    
    
    # PLOTS: "county.forecasts_*" ----
    output$plot.county.forecasts_cases_total.line = renderPlotly({
        temp = mod_county_cases %>%
            filter(County == input$input_county) %>% 
            select(forecast) %>% 
            .$forecast %>% 
            bind_cols() # Is this really necessary, there should be a better way to access the tibble
            
        
        shiny::validate(shiny::need(expr = !is.na(temp), message = "Insufficient data to generate a forecast."))
        
        plot_ly(
            data = (temp %>%
                        filter(forecast == "dts")),
            x = ~ Date,
            y = ~ value,
            name = "History",
            connectgaps = TRUE,
            line = list(width = 6, color = plotly_color.cases),
            opacity = .95,
            type = 'scatter',
            mode = 'lines'
        ) %>%
            add_trace(
                data = (temp %>%
                            filter(
                                forecast == "dts.forecasts.Hi_95"
                            )),
                x = ~ Date,
                y = ~ value,
                legendgroup = "Conf_90",
                name = "95% Confidence",
                line = list(width = 3, color = plotly_color.forecast_interval, opacity = .80),
                connectgaps = TRUE,
                type = 'scatter',
                mode = 'lines'
            ) %>% 
            add_trace(
                data = (temp %>%
                            filter(
                                forecast == "dts.forecasts.Lo_95"
                            )),
                x = ~ Date,
                y = ~ value,
                fill = "tonexty",
                fillcolor = plotly_color.forecast_interval,
                showlegend=FALSE,
                legendgroup = "Conf_90",
                name = "95% Confidence",
                line = list(width = 3, color = plotly_color.forecast_interval, opacity = .80),
                connectgaps = TRUE,
                type = 'scatter',
                mode = 'lines'
            ) %>% 
            add_trace(
                data = (temp %>%
                            filter(
                                forecast == "dts.forecasts.Point_Forecast"
                            )),
                x = ~ Date,
                y = ~ value,
                name = "Best Bet",
                connectgaps = TRUE,
                line = list(width = 4, color = plotly_color.forecast_point),
                opacity = 1,
                type = 'scatter',
                mode = 'lines'
            )  %>% 
            layout(
                p = .,
                xaxis = plotly_axisformat.date,
                yaxis = list(title = "Total Cases Forecast", titlefont = plotly_titlefont.axis)
            )
            
            
        
    })
    
    output$plot.county.forecasts_tests_total.line = renderPlotly({
        temp = mod_county_tests %>%
            filter(County == input$input_county) %>% 
            select(forecast) %>% 
            .$forecast %>% 
            bind_cols() # Is this really necessary, there should be a better way to access the tibble
        
        shiny::validate(shiny::need(expr = !is.na(temp), message = "Insufficient data to generate a forecast."))
        
        plot_ly(
            data = (temp %>%
                        filter(forecast == "dts")),
            x = ~ Date,
            y = ~ value,
            name = "History",
            connectgaps = TRUE,
            line = list(width = 6, color = plotly_color.tests),
            opacity = .95,
            type = 'scatter',
            mode = 'lines'
        ) %>%
            add_trace(
                data = (temp %>%
                            filter(
                                forecast == "dts.forecasts.Hi_95"
                            )),
                x = ~ Date,
                y = ~ value,
                legendgroup = "Conf_90",
                name = "95% Confidence",
                line = list(width = 3, color = plotly_color.forecast_interval, opacity = .80),
                connectgaps = TRUE,
                type = 'scatter',
                mode = 'lines'
            ) %>% 
            add_trace(
                data = (temp %>%
                            filter(
                                forecast == "dts.forecasts.Lo_95"
                            )),
                x = ~ Date,
                y = ~ value,
                fill = "tonexty",
                fillcolor = plotly_color.forecast_interval,
                showlegend=FALSE,
                legendgroup = "Conf_90",
                name = "95% Confidence",
                line = list(width = 3, color = plotly_color.forecast_interval, opacity = .80),
                connectgaps = TRUE,
                type = 'scatter',
                mode = 'lines'
            ) %>% 
            add_trace(
                data = (temp %>%
                            filter(
                                forecast == "dts.forecasts.Point_Forecast"
                            )),
                x = ~ Date,
                y = ~ value,
                name = "Best Bet",
                connectgaps = TRUE,
                line = list(width = 4, color = plotly_color.forecast_point),
                opacity = 1,
                type = 'scatter',
                mode = 'lines'
            )  %>% 
            layout(
                p = .,
                xaxis = plotly_axisformat.date,
                yaxis = list(title = "Total Deaths Forecast", titlefont = plotly_titlefont.axis)
            )
        
        
        
    })
    
    output$plot.county.forecasts_deaths_total.line = renderPlotly({
        temp = mod_county_deaths %>%
            filter(County == input$input_county) %>% 
            select(forecast) %>% 
            .$forecast %>% 
            bind_cols() # Is this really necessary, there should be a better way to access the tibble
        
        shiny::validate(shiny::need(expr = !is.na(temp), message = "Insufficient data to generate a forecast."))
        
        plot_ly(
            data = (temp %>%
                        filter(forecast == "dts")),
            x = ~ Date,
            y = ~ value,
            name = "History",
            connectgaps = TRUE,
            line = list(width = 6, color = plotly_color.deaths),
            opacity = .95,
            type = 'scatter',
            mode = 'lines'
        ) %>%
            add_trace(
                data = (temp %>%
                            filter(
                                forecast == "dts.forecasts.Hi_95"
                            )),
                x = ~ Date,
                y = ~ value,
                legendgroup = "Conf_90",
                name = "95% Confidence",
                line = list(width = 3, color = plotly_color.forecast_interval, opacity = .80),
                connectgaps = TRUE,
                type = 'scatter',
                mode = 'lines'
            ) %>% 
            add_trace(
                data = (temp %>%
                            filter(
                                forecast == "dts.forecasts.Lo_95"
                            )),
                x = ~ Date,
                y = ~ value,
                fill = "tonexty",
                fillcolor = plotly_color.forecast_interval,
                showlegend=FALSE,
                legendgroup = "Conf_90",
                name = "95% Confidence",
                line = list(width = 3, color = plotly_color.forecast_interval, opacity = .80),
                connectgaps = TRUE,
                type = 'scatter',
                mode = 'lines'
            ) %>% 
            add_trace(
                data = (temp %>%
                            filter(
                                forecast == "dts.forecasts.Point_Forecast"
                            )),
                x = ~ Date,
                y = ~ value,
                name = "Best Bet",
                connectgaps = TRUE,
                line = list(width = 4, color = plotly_color.forecast_point),
                opacity = 1,
                type = 'scatter',
                mode = 'lines'
            )  %>% 
            layout(
                p = .,
                xaxis = plotly_axisformat.date,
                yaxis = list(title = "Total Tests Forecast", titlefont = plotly_titlefont.axis)
            )
        
        
        
    })
    
    
    # PLOTS: "county.rate_*" ----
    # Rates should use numerator data as COLOR and OPACITY reference
    output$plot.county.rate_detection.line = renderPlotly({
        p = dat_county %>% 
            filter(County == input$input_county) %>%
            unnest(cols = c(data)) %>%
            mutate(daily_detection = round((DailyDelta_cases/DailyDelta_tests), 2)) %>%
            filter(!is.na(daily_detection)) %>%
            plot_ly(
                data = .,
                x = ~ Date,
                y = ~ daily_detection,
                connectgaps = TRUE,
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
    
    output$plot.county.rate_mortality.line = renderPlotly({
        p = dat_county %>% 
            filter(County == input$input_county) %>%
            unnest(cols = c(data)) %>%
            mutate(daily_mortality = (DailyDelta_deaths/DailyDelta_cases)) %>%
            filter(!is.na(daily_mortality)) %>%
            plot_ly(
                data = .,
                x = ~ Date,
                y = ~ daily_mortality,
                connectgaps = TRUE,
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
    
    output$county.rates_infected.line = renderPlotly({
        dat_county %>% 
            filter(County == input$input_county) %>%
            unnest(cols = c(data)) %>%
            left_join(x = .,
                      y = (pop %>% 
                               select(County, jan1_2019_pop_est) %>%
                               rename(Population = jan1_2019_pop_est)
                      ), 
                      by = "County") %>% 
            ungroup() %>%
            group_by(Date) %>%
            mutate(pcnt_infected = DailyCount_cases/Population) %>%
            filter(!is.na(pcnt_infected)) %>%
            select(Date, pcnt_infected) %>%
            ungroup() %>%
            plot_ly(data = ., 
                    x = ~Date,
                    y = ~pcnt_infected,
                    connectgaps = TRUE,
                    line = list(width = 6, color = plotly_color.cases),
                    opacity = .95,
                    mode = 'lines', 
                    type = 'scatter'
            ) %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date,
                yaxis = list(title = "Pcnt Population Infected", titlefont = plotly_titlefont.axis, tickformat = ".2%")
            )
    })
    
})
