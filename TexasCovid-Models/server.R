#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# DEF. GENERIC PLOTTING PARAMETERS ----
# >>FONTS<<
plotly_titlefont.axis = list(family = "Arial",
                             size = 16,
                             color = "#7f7f7f")

plotly_titlefont.plot = list(family = "Arial, bold",
                             size = 20,
                             color = "#584f73")

# >>TITLES<<
plotly_titleformat.plot = function(plot_title){
    
    return(list(
        text = plot_title,
        xanchor = "left", yanchor = "top",
        x = 0.08, y = 1.1, yref = "paper",
        font = plotly_titlefont.plot
    ))
}

# >>AXIS FORMATTING<<
# Dynamic Range
plotly_range.date = c(max(dat_state$Date)-60, max(dat_state$Date)+1)


# Formats
# ... auto-dynamic
plotly_axisformat.date = list(
    title = "",
    titlefont = plotly_titlefont.axis,
    type = "date",
    tickformat = "%m/%d",
    tickangle = -15,
    showgrid = FALSE
)
# ... fixed
plotly_axisformat.date_fixed = list(
    title = "",
    titlefont = plotly_titlefont.axis,
    type = "date",
    tickformat = "%m/%d",
    tickangle = -15,
    showgrid = FALSE,
    range = plotly_range.date
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
    
    # TEXT: "state.daily_*" ----
    output$text.state.daily_cases_comp = renderText({

        comp = (sum(dat_state$DailyDelta_cases < dat_state$DailyDelta_cases[nrow(dat_state)]) / nrow(dat_state) * 100) %>%
            round(x = ., digits = 0)
        
        paste0("<br>Appx. <b>", as.character(comp), "%</b> of the days were better.</br>")
    })
    
    output$text.state.daily_tests_comp = renderText({
        
        comp = (sum(dat_state$DailyDelta_tests < dat_state$DailyDelta_tests[nrow(dat_state)]) / nrow(dat_state) * 100) %>%
            round(x = ., digits = 0)
        
        paste0("<br>Appx. <b>", as.character(comp), "%</b> of the days were better.</br>")
    })
    
    output$text.state.daily_deaths_comp = renderText({
        
        comp = (sum(dat_state$DailyDelta_deaths < dat_state$DailyDelta_deaths[nrow(dat_state)]) / nrow(dat_state) * 100) %>%
            round(x = ., digits = 0)
        
        paste0("<br>Appx. <b>", as.character(comp), "%</b> of the days were better.</br>")
    })
    
    
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
                xaxis = plotly_axisformat.date_fixed,
                yaxis = list(title = ""),
                title = plotly_titleformat.plot(plot_title = "Daily Cases"),
                showlegend = FALSE
            )
    })
    
    output$plot.state.daily_tests.bar = renderPlotly({
        p = plot_ly() %>%
            add_trace(
                data = dat_state,
                x = ~ Date,
                y = ~ DailyDelta_tests,
                marker = list(color = plotly_color.tests),
                opacity = .95,
                type = "bar",
                name = "Daily Tests"
            ) 
        
        p = add_trace(p = p, 
                      data = dat_state, 
                      x = ~Date,
                      y = ~ma_tests,
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
                xaxis = plotly_axisformat.date_fixed,
                yaxis = list(title = ""),
                title = plotly_titleformat.plot(plot_title = "Daily Tests"),
                showlegend = FALSE
            )
    })
    
    output$plot.state.daily_deaths.bar = renderPlotly({
        p = plot_ly() %>%
            add_trace(
                data = dat_state,
                x = ~ Date,
                y = ~ DailyDelta_deaths,
                marker = list(color = plotly_color.deaths),
                opacity = .95,
                type = "bar",
                name = "Daily Deaths"
            ) 
        
        p = add_trace(p = p, 
                      data = dat_state, 
                      x = ~Date,
                      y = ~ma_deaths,
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
                xaxis = plotly_axisformat.date_fixed,
                yaxis = list(title = ""),
                title = plotly_titleformat.plot(plot_title = "Daily Deaths"),
                showlegend = FALSE
            )
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
                xaxis = plotly_axisformat.date_fixed,
                yaxis = list(title = ""),
                title = plotly_titleformat.plot(plot_title = "Total Cases")
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
                xaxis = plotly_axisformat.date_fixed,
                yaxis = list(title = ""),
                title = plotly_titleformat.plot(plot_title = "Total Tests")
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
                xaxis = plotly_axisformat.date_fixed,
                yaxis = list(title = ""),
                title = plotly_titleformat.plot(plot_title = "Total Deaths")
            )
        
        p
    })
    
    # PLOTS: "state.rate_*" ----
    # Rates should use numerator data as COLOR and OPACITY reference
    output$plot.state.rate_detection.line = renderPlotly({
        p = dat_state %>%
            mutate(daily_detection = round((DailyCount_cases/DailyCount_tests), 4)) %>%
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
                xaxis = plotly_axisformat.date_fixed,
                yaxis = list(title = "", tickformat = ".2%"),
                title = plotly_titleformat.plot(plot_title = "Case Detection Rate")
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
                xaxis = plotly_axisformat.date_fixed,
                yaxis = list(title = "", tickformat = ".2%"),
                title = plotly_titleformat.plot(plot_title = "Case Mortality Rate")
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
                xaxis = plotly_axisformat.date_fixed,
                yaxis = list(title = "Pcnt. Population Infected", titlefont = plotly_titlefont.axis, tickformat = ".2%")
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
                xaxis = plotly_axisformat.date_fixed,
                yaxis = list(title = "Daily Infection Rate", titlefont = plotly_titlefont.axis, tickformat = ".2%"),
                title = plotly_titleformat.plot(plot_title = "Infection Rates for Community-types")
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
                xaxis = plotly_axisformat.date_fixed,
                yaxis = list(title = "Pcnt. Population Tested", titlefont = plotly_titlefont.axis, tickformat = ".2%")
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
                xaxis = plotly_axisformat.date_fixed,
                yaxis = list(title = "Daily Testing Rate", titlefont = plotly_titlefont.axis, tickformat = ".2%"),
                title = plotly_titleformat.plot(plot_title = "Testing Rates for Community-types")
            )
        
        fig = subplot(f1, f2, nrows = 2, shareX = TRUE, shareY = FALSE, titleX = TRUE, titleY = TRUE)
        fig
        
    })
    
    output$comm.rates_mortality.line = renderPlotly({
        p = 
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
                xaxis = plotly_axisformat.date_fixed,
                yaxis = list(title = "", tickformat = ".2%", range = c(0,.06)),
                title = plotly_titleformat.plot(plot_title = "Case Mortality Rate for Community-types")
            )
        
        p
    })
    
    output$comm.rates_detection.line = renderPlotly({
        p = 
            dat_pop %>%
            group_by(Date, pop_group) %>%
            mutate(pcnt_detection = DailyCount_cases/DailyCount_tests) %>% 
            filter(!is.na(pcnt_detection)) %>%
            select(Date, pop_group, pcnt_detection) %>%
            ungroup() %>%
            plot_ly(data = ., 
                    x = ~Date,
                    y = ~pcnt_detection,
                    connectgaps = TRUE,
                    color = ~pop_group,
                    legendgroup = ~pop_group,
                    mode = 'lines', 
                    line = list(width = 3),
                    type = 'scatter'
            ) %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date_fixed,
                yaxis = list(title = "", tickformat = ".2%"),
                title = plotly_titleformat.plot(plot_title = "Case Detection Rate for Community-types")
            )
        
        p
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
        temp = dat_county %>% 
            filter(County == input$input_county) %>%
            unnest(cols = c(data)) %>%
            filter(!is.na(DailyDelta_cases)) %>%
            
            # Dynamic moving-average calculation
            arrange(Date) %>%
            mutate(ma_cases = zoo::rollmean(x = DailyDelta_cases, k = 7, fill = 0, align = "right"),
                   ma_tests = zoo::rollmean(x = DailyDelta_tests, k = 7, fill = 0, align = "right"),
                   ma_deaths = zoo::rollmean(x = DailyDelta_deaths, k = 7, fill = 0, align = "right")
            ) 
        
        p = plot_ly() %>%
            add_trace(
                p = .,
                data = temp,
                x = ~ Date,
                y = ~ DailyDelta_cases,
                marker = list(color = plotly_color.cases),
                opacity = .95,
                type = "bar",
                name = "Daily Cases"
            ) %>%
            add_trace(
                p = .,
                data = temp,
                x = ~ Date,
                y = ~ ma_cases,
                name = "7-day Avg.",
                type = 'scatter', 
                mode = 'lines',
                connectgaps = TRUE,
                line = list(width = 4, color = plotly_color.forecast_point),
                opacity = 1
            ) %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date_fixed,
                yaxis = list(title = ""),
                title = plotly_titleformat.plot(plot_title = paste("Daily Cases:", input$input_county, "County")),
                showlegend = FALSE
            )
        
        p
    })
    
    output$plot.county.daily_tests.bar = renderPlotly({
        temp = dat_county %>% 
            filter(County == input$input_county) %>%
            unnest(cols = c(data)) %>%
            filter(!is.na(DailyDelta_tests)) %>%
            
            # Dynamic moving-average calculation
            arrange(Date) %>%
            mutate(ma_cases = zoo::rollmean(x = DailyDelta_cases, k = 7, fill = 0, align = "right"),
                   ma_tests = zoo::rollmean(x = DailyDelta_tests, k = 7, fill = 0, align = "right"),
                   ma_deaths = zoo::rollmean(x = DailyDelta_deaths, k = 7, fill = 0, align = "right")
            ) 
        
        p = plot_ly() %>%
            add_trace(
                p = .,
                data = temp,
                x = ~ Date,
                y = ~ DailyDelta_tests,
                marker = list(color = plotly_color.tests),
                opacity = .95,
                type = "bar",
                name = "Daily Tests"
            ) %>%
            add_trace(
                p = .,
                data = temp,
                x = ~ Date,
                y = ~ ma_tests,
                name = "7-day Avg.",
                type = 'scatter', 
                mode = 'lines',
                connectgaps = TRUE,
                line = list(width = 4, color = plotly_color.forecast_point),
                opacity = 1
            ) %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date_fixed,
                yaxis = list(title = ""),
                title = plotly_titleformat.plot(plot_title = paste("Daily Tests:", input$input_county, "County")),
                showlegend = FALSE
            )
        
        p
    })
    
    output$plot.county.daily_deaths.bar = renderPlotly({
        temp = dat_county %>% 
            filter(County == input$input_county) %>%
            unnest(cols = c(data)) %>%
            filter(!is.na(DailyDelta_deaths)) %>%
            
            # Dynamic moving-average calculation
            arrange(Date) %>%
            mutate(ma_cases = zoo::rollmean(x = DailyDelta_cases, k = 7, fill = 0, align = "right"),
                   ma_tests = zoo::rollmean(x = DailyDelta_tests, k = 7, fill = 0, align = "right"),
                   ma_deaths = zoo::rollmean(x = DailyDelta_deaths, k = 7, fill = 0, align = "right")
            ) 
        
        p = plot_ly() %>%
            add_trace(
                p = .,
                data = temp,
                x = ~ Date,
                y = ~ DailyDelta_deaths,
                marker = list(color = plotly_color.deaths),
                opacity = .95,
                type = "bar",
                name = "Daily Deaths"
            ) %>%
            add_trace(
                p = .,
                data = temp,
                x = ~ Date,
                y = ~ ma_deaths,
                name = "7-day Avg.",
                type = 'scatter', 
                mode = 'lines',
                connectgaps = TRUE,
                line = list(width = 4, color = plotly_color.forecast_point),
                opacity = 1
            ) %>%
            layout(
                p = .,
                xaxis = plotly_axisformat.date_fixed,
                yaxis = list(title = ""),
                title = plotly_titleformat.plot(plot_title = paste("Daily Deaths:", input$input_county, "County")),
                showlegend = FALSE
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
                yaxis = list(title = ""),
                title = plotly_titleformat.plot(plot_title = paste("Total Cases:", input$input_county, "County"))
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
                yaxis = list(title = ""),
                title = plotly_titleformat.plot(plot_title = paste("Total Tests:", input$input_county, "County"))
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
                yaxis = list(title = ""),
                title = plotly_titleformat.plot(plot_title = paste("Total Deaths:", input$input_county, "County"))
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
                yaxis = list(title = ""),
                title = plotly_titleformat.plot(plot_title = paste("Total Cases Forecast:", input$input_county, "County"))
                
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
                yaxis = list(title = ""),
                title = plotly_titleformat.plot(plot_title = paste("Total Tests Forecast:", input$input_county, "County"))
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
                yaxis = list(title = ""),
                title = plotly_titleformat.plot(plot_title = paste("Total Deaths Forecast:", input$input_county, "County"))
            )
        
        
        
    })
    
    
    # PLOTS: "county.rate_*" ----
    # Rates should use numerator data as COLOR and OPACITY reference
    output$plot.county.rate_detection.line = renderPlotly({
        p = dat_county %>% 
            filter(County == input$input_county) %>%
            unnest(cols = c(data)) %>%
            mutate(daily_detection = round((DailyCount_cases/DailyCount_tests), 4)) %>%
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
                xaxis = plotly_axisformat.date_fixed,
                yaxis = list(title = "", tickformat = ".2%"),
                title = plotly_titleformat.plot(plot_title = paste("Case Detection Rate:", input$input_county, "County"))
            )
        
        p
        
        
    })
    
    output$plot.county.rate_mortality.line = renderPlotly({
        p = dat_county %>% 
            filter(County == input$input_county) %>%
            unnest(cols = c(data)) %>%
            mutate(daily_mortality = round((DailyCount_deaths/DailyCount_cases), 4)) %>%
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
                xaxis = plotly_axisformat.date_fixed,
                yaxis = list(title = "", tickformat = ".2%"),
                title = plotly_titleformat.plot(plot_title = paste("Case Mortality Rate:", input$input_county, "County"))
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
                xaxis = plotly_axisformat.date_fixed,
                yaxis = list(title = "", tickformat = ".2%"),
                title = plotly_titleformat.plot(plot_title = paste("Pcnt. Population Infected:", input$input_county, "County"))
            )
    })
    
    output$county.rates_risk.line = renderPlotly({
        
        plot_dat = dat_county %>% 
            filter(County == input$input_county) %>%
            unnest(cols = c(data)) %>%
            left_join(x = .,
                      y = (pop %>% 
                               select(County, jan1_2019_pop_est) %>%
                               rename(Population = jan1_2019_pop_est)
                      ), 
                      by = "County") %>% 
            ungroup() 
        
        plot_period = c(min(dat_county$data[[1]]$Date), 
                        max(dat_county$data[[1]]$Date)
        )
        
        plot_dat = plot_dat %>% 
            group_by(Date) %>%
            mutate(dt_infection = DailyDelta_cases/Population,
                   Date = as.Date(Date)) %>%
            ungroup()  %>% 
            arrange(Date) %>%
            complete(Date = seq.Date(plot_period[1], plot_period[2], by="days")) %>% 
            mutate(ma_dt = zoo::rollmean(x = dt_infection, k = 7, fill = 0, align = "right"),
                   ma_dt = imputeTS::na_locf(x = ma_dt, option = "locf", na_remaining = "rev"),
                   ma_dt = ma_dt * 1e5) %>%
            mutate(Risk = ifelse(ma_dt < 1, "Low",
                                 ifelse(ma_dt < 10, "Moderate",
                                        ifelse(ma_dt < 25, "Moderate High", "High"))),
                   Risk = factor(Risk, levels = c("Low", "Moderate", "Moderate High", "High"), ordered = TRUE)) 
        
        plot_dat = plot_dat %>% 
            group_by(Risk) %>% 
            nest() 
        
        for(i in seq_along(plot_dat$Risk)){
            plot_dat$data[[i]] = plot_dat$data[[i]] %>% 
                complete(Date = seq.Date(plot_period[1], plot_period[2], by="day"))
        }
        
        p = plot_ly(
                    # line = list(width = 4),
                    opacity = .95,
                    type = 'scatter',
                    mode = 'markers'
                    # connectgaps = FALSE
                    )
        p = p %>% 
            add_trace(p = ., 
                      data = (plot_dat$data %>% bind_rows() %>% filter(!is.na(ma_dt)) %>% arrange(Date) ),
                      x = ~Date,
                      y = ~ma_dt, 
                      mode = "lines",
                      line = list(width = 1, color = plotly_color.forecast_point),
                      showlegend = FALSE) %>%
            add_trace(p = ., data = tryCatch(expr = plot_dat$data[[1]], error = function(e){return(tibble(Date = NA, ma_dt = NA))}), 
                      x = ~Date, y = ~ma_dt, mode = 'markers',
                      legendgroup = "Risk", name = levels(plot_dat$Risk)[1], 
                      marker = list(color = "#10cc26", size = 10)
                      ) %>%
            add_trace(p = ., data = tryCatch(expr = plot_dat$data[[2]], error = function(e){return(tibble(Date = NA, ma_dt = NA))}), 
                      x = ~Date, y = ~ma_dt, mode = 'markers',
                      legendgroup = "Risk", name = levels(plot_dat$Risk)[2], 
                      marker = list(color = "#ffea00", size = 10)
                      ) %>%
            add_trace(p = ., data = tryCatch(expr = plot_dat$data[[3]], error = function(e){return(tibble(Date = NA, ma_dt = NA))}), 
                      x = ~Date, y = ~ma_dt, mode = 'markers',
                      legendgroup = "Risk", name = levels(plot_dat$Risk)[3], 
                      marker = list(color = "#ff8400", size = 10)
                      ) %>%
            add_trace(p = ., data = tryCatch(expr = plot_dat$data[[4]], error = function(e){return(tibble(Date = NA, ma_dt = NA))}), 
                      x = ~Date, y = ~ma_dt, mode = 'markers',
                      legendgroup = "Risk", name = levels(plot_dat$Risk)[4], 
                      marker = list(color = "#ff0011", size = 10)
                      )
            
        
        p = p %>% 
            layout(p = ., 
                   xaxis = plotly_axisformat.date_fixed,
                   yaxis = list(title = "New Cases per 100K People", titlefont = plotly_titlefont.axis),
                   title = plotly_titleformat.plot(plot_title = paste("Risk:", input$input_county, "County"))
                   )
        
        p
        
    })
    
})
