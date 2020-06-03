# Shiny app creation script
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)
library(DT)
library(caTools)
library(plotly)
library(shinycssloaders)

data <- readRDS("Data/data_for_shiny.Rds")

sa_data <- readRDS("Data/sa_data_for_shiny.Rds")

max_date <- max(data$date)

sa_gov_pred_dates <- as.Date(c("2020-06-01", "2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01"))

# ui <- navbarPage(title = "Title",
# tabPanel(),
# navbarMenu(title = "More",
#            tabPanel())))

ui <- dashboardPage(
  dashboardHeader(
    title = "Covid-19 Projections Comparison"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("International", tabName = "International", icon = icon("globe-africa")),
      menuItem("South Africa", tabName = "SouthAfrica", icon = icon("map-marker-alt"))
    )
  ),
  dashboardBody(
    tabItems(
    tabItem(
      tabName = "International",
      
      column(12, id = "test1",
             tabBox(width = 12, id = "test1b",
                    
      selectInput(inputId = "Country",
                  label = "Choose a country:",
                  choices = unique(data$location[data$location != "SouthAfrica"]),
                  selected = "Belgium"),
      
      #fluidRow(column(8, offset = 4))
      
      #wellPanel()
      #tabsetPanel(tabPanel("tab1", "Hi"))
      
      # sidebarLayout(sidebarPanel(),
      #               mainPanel())
      fluidRow(
        column(6, plotlyOutput(outputId = "cumulative_dpm_plot") %>% withSpinner(color="#0dc5c1")),
        
        column(6, plotlyOutput(outputId = "new_dpm_plot") %>% withSpinner(color="#0dc5c1"))
      ),
      
      fluidRow(
        column(12, plotlyOutput(outputId = "cumulative_dpm_plot_with_predictions") %>% withSpinner(color="#0dc5c1"))
      ),
      
      p("The horizontal lines show the expected final death counts based on different models.\n
    95% confidence bands are given for the IHME and Diamond Princess predictions.\n
    The IHME prediction is plotted over time, while for the others only the final predictions are shown."),
      
      p("Diamond Princess - predictions calculated using nCFR point estimates for age bands obtained",
        a(href = "https://www.medrxiv.org/content/10.1101/2020.03.05.20031773v2.full.pdf", "here"),
        "using each country's",
        a(href = "https://www.populationpyramid.net/", "population pyramid")),
      
      p("IHME predictions are obtained from",
        a(href = "https://covid19.healthdata.org/", "here"))
    )
    )
    ),
    
    tabItem(
      tabName = "SouthAfrica",
      
      column(12, id = "test2",
             tabBox(width = 12, id = "test2b",
      
      selectInput(inputId = "Country2",
                  label = "Choose a province:",
                  choices = unique(sa_data$province),
                  selected = "SouthAfrica"),
      
      #fluidRow(column(8, offset = 4))
      
      #wellPanel()
      #tabsetPanel(tabPanel("tab1", "Hi"))
      
      # sidebarLayout(sidebarPanel(),
      #               mainPanel())
      fluidRow(
        column(6, plotlyOutput(outputId = "cumulative_dpm_plot2") %>% withSpinner(color="#0dc5c1")),
        
        column(6, plotlyOutput(outputId = "new_dpm_plot2") %>% withSpinner(color="#0dc5c1"))
      ),
      
      fluidRow(
        column(12, plotlyOutput(outputId = "cumulative_dpm_plot_with_predictions2") %>% withSpinner(color="#0dc5c1"))
      ),
      
      # selectInput(inputId = "Date",
      #             label = "Choose a reference date:",
      #             choices = unique(sa_data$date[sa_data$date < Sys.Date() - 1]),
      #             selected = Sys.Date() - 2),
      # 
      # fluidRow(
      #   column(12, plotOutput(outputId = "cumulative_dpm_plot_with_predictions3") %>% withSpinner(color="#0dc5c1"))
      # ),
      
      
      p("The horizontal lines show the expected final death counts based on different models.\n
    95% confidence bands are given for the IHME and Diamond Princess predictions.\n
    The IHME prediction is plotted over time, while for the others only the final predictions are shown."),
      
      p("Diamond Princess - predictions calculated using nCFR point estimates for age bands obtained",
        a(href = "https://www.medrxiv.org/content/10.1101/2020.03.05.20031773v2.full.pdf", "here"),
        "using each country's",
        a(href = "https://www.populationpyramid.net/", "population pyramid")),
      
      p("SA Gov optimistic projections are obtained from",
        a(href = "https://storage.googleapis.com/stateless-bhekisisa-website/wordpress-uploads/2020/05/d0ba424b-sacovidmodellingreport_nationallongtermprojections_final.pdf", "here")),
      
      p("ASSA optimistic prediction obtained from",
        a(href = "https://www.actuarialsociety.org.za/download/press-release-29-april-2020/?wpdmdl=13026%22%3E%3Cb%3E%20Press%20Release:%2029%20April%202020%3C/b%3E%3C/a%3E%20%3Cdiv%20class=%22author-journals%22%3E%3Cb%3EAuthor:%3C/b%3E%3Cbr%3E%3Cdiv%20class=%22expects%22%3E%3C/div%3E%20%3C/div%3E%20%20%20%20%20%20%20%20%20%3C/div%3E%3Cbr%3E%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%3Cdiv%20class=%22%20pull-template%22%20align=%22left%22%3E%3Ca%20target=%22_blank%22%20href=%22Press%20Release:%2029%20April%202020%22%3E%3C/a%3E%20%20%20%20%20%20%20%20%20%20%20%20%20%20%3C/div%3E%20%20%20%20%3C/div%3E%3C/div%3E", "here"))
    )
    )
    )
    )
  )
)

# ui <- fluidPage(
#   selectInput(inputId = "Country",
#               label = "Choose a country:",
#               choices = unique(data$location[data$location != "SouthAfrica"]),
#               selected = "SouthAfrica"),
#   
#   #fluidRow(column(8, offset = 4))
#   
#   #wellPanel()
#   #tabsetPanel(tabPanel("tab1", "Hi"))
#   
#   # sidebarLayout(sidebarPanel(),
#   #               mainPanel())
#   fluidRow(
#     column(6, plotlyOutput(outputId = "cumulative_dpm_plot")),
#   
#     column(6, plotlyOutput(outputId = "new_dpm_plot"))
#   ),
#   
#   fluidRow(
#     column(12, plotlyOutput(outputId = "cumulative_dpm_plot_with_predictions"))
#   ),
#   
#   p("The horizontal lines show the expected final death counts based on different models.\n
#     95% confidence bands are given for the IHME and Diamond Princess predictions.\n
#     The IHME prediction is plotted over time, while for the others only the final predictions are shown."),
#   
#   p("Diamond Princess - predictions calculated using nCFR point estimates for age bands obtained",
#     a(href = "https://www.medrxiv.org/content/10.1101/2020.03.05.20031773v2.full.pdf", "here"),
#     "using each country's",
#     a(href = "https://www.populationpyramid.net/", "population pyramid")),
#   
#   p("IHME predictions are obtained from",
#     a(href = "https://covid19.healthdata.org/", "here")),
#   
#   p("SA Gov optimistic projections are obtained from",
#     a(href = "https://storage.googleapis.com/stateless-bhekisisa-website/wordpress-uploads/2020/05/d0ba424b-sacovidmodellingreport_nationallongtermprojections_final.pdf", "here")),
#   
#   p("ASSA optimistic prediction obtained from",
#     a(href = "https://www.actuarialsociety.org.za/download/press-release-29-april-2020/?wpdmdl=13026%22%3E%3Cb%3E%20Press%20Release:%2029%20April%202020%3C/b%3E%3C/a%3E%20%3Cdiv%20class=%22author-journals%22%3E%3Cb%3EAuthor:%3C/b%3E%3Cbr%3E%3Cdiv%20class=%22expects%22%3E%3C/div%3E%20%3C/div%3E%20%20%20%20%20%20%20%20%20%3C/div%3E%3Cbr%3E%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%3Cdiv%20class=%22%20pull-template%22%20align=%22left%22%3E%3Ca%20target=%22_blank%22%20href=%22Press%20Release:%2029%20April%202020%22%3E%3C/a%3E%20%20%20%20%20%20%20%20%20%20%20%20%20%20%3C/div%3E%20%20%20%20%3C/div%3E%3C/div%3E", "here"))
# )

server <- function(input, output) {
  
  filtered_data <- reactive( {
    data %>%
      filter(location == input$Country) 
  } )
  
  output$cumulative_dpm_plot <- renderPlotly( {
    ggplotly(filtered_data() %>%
              filter(!is.na(total_deaths_per_million)) %>%
              ggplot(aes(x = date, y = total_deaths_per_million)) +
              geom_point() +
              geom_line(aes(x = date, y = smooth.spline(total_deaths_per_million)$y, colour = "Smoothing Spline")) +
              geom_line(aes(x = first_death,
                            y = seq(0, max(filtered_data()$total_deaths_per_million, na.rm = T),
                                    length.out = length(date))),
                        linetype = "dotted") +
              geom_line(aes(x = first_case,
                            y = seq(0, max(filtered_data()$total_deaths_per_million, na.rm = T),
                                    length.out = length(date))),
                        linetype = "dotted") +
              geom_text(aes(x = filtered_data()$first_death[1],
                            y = max(filtered_data()$total_deaths_per_million, na.rm = T),
                            label = "First Death"),
                        nudge_x = 8) +
              geom_text(aes(x = filtered_data()$first_case[1],
                            y = max(filtered_data()$total_deaths_per_million, na.rm = T),
                            label = "First Case"),
                        nudge_x = 8) +
               # geom_text(aes(x = filtered_data()$first_death[1], y = 600, label = "First Death")) +
               # geom_text(aes(x = filtered_data()$first_case[1], y = 600, label = "First Case")) +
              labs(title = "Cumulative Deaths Per Million Over Time (with fitted smoothing spline)",
                   x = "Date",
                   y = "Cumulative deaths per million") +
              theme(legend.position = "none") +
              coord_cartesian(xlim = as.Date(c("2020-02-01", Sys.Date())),
                              ylim = c(0, max(filtered_data()$total_deaths_per_million, na.rm = T))) +
              scale_x_date(date_breaks = "2 weeks")
            )
  } )
  
  output$new_dpm_plot <- renderPlotly( {
    ggplotly(filtered_data() %>%
            filter(!is.na(new_deaths_per_million)) %>%
            ggplot(aes(x = date, y = new_deaths_per_million)) +
            geom_point() +
            # geom_line(aes(x = date, y = runmean(new_deaths_per_million, k = 7, endrule = "NA"), colour = "blue")) +
            geom_line(aes(x = date,
                          y = c(smooth.spline(runmean(new_deaths_per_million, k = 7))$y[-c((length(date) - 2):length(date))], NA, NA, NA),
                          colour = "Smoothing Spline")) +
            # geom_line(aes(x = date, y = smooth.spline(new_deaths_per_million), colour = "red")) +
            labs(title = "Daily Deaths Per Million Over Time (with fitted smoothing spline)",
                 x = "Date",
                 y = "Daily deaths per million") +
            theme(legend.position = "none") +
            coord_cartesian(xlim = as.Date(c("2020-02-01", Sys.Date()))) +
            scale_x_date(date_breaks = "2 weeks")
          )
  } )
  
  output$cumulative_dpm_plot_with_predictions <- renderPlotly( {
      ggplotly(filtered_data() %>%
                 ggplot(aes(x = date, y = total_deaths_per_million, color = "Actual")) +
                 geom_point() +
                 
                 # Diamond Princess predictions
                 geom_line(aes(y = dp_expected_final_deaths_per_million,
                               x = date,
                               color = "Diamond Princess"),
                           linetype = "dashed") +
                 geom_ribbon(aes(x = date,
                                 ymin = dp_expected_final_deaths_per_million_lower,
                                 ymax = dp_expected_final_deaths_per_million_upper,
                                 color = NA,
                                 fill = "Diamond Princess CI"),
                             alpha = 0.2,
                             show.legend = F) +
                 
                 # IHME predictions
                 geom_ribbon(aes(x = date,
                                 ymin = ihme_total_deaths_lower_per_million,
                                 ymax = ihme_total_deaths_upper_per_million,
                                 color = NA,
                                 fill = "IHME CI"),
                             alpha = 0.2,
                             show.legend = F) +
                 geom_line(aes(x = date, y = ihme_total_deaths_per_million, color = "IHME"),
                           linetype = "dashed") +
                 # geom_line(aes(y = ihme_final_dpm,
                 #               x = date,
                 #               color = "IHME"),
                 #           linetype = "dashed") +
                 # geom_ribbon(aes(x = date,
                 #                 ymin = ihme_final_dpm_lower,
                 #                 ymax = ihme_final_dpm_upper,
                 #                 colour = NULL),
                 #             fill = "green",
                 #             alpha = 0.2) +
                 
                 
                 labs(title = "Cumulative Deaths Per Million Over Time Compared to Various Predictions",
                      x = "Date",
                      y = "Deaths per million",
                      color = "Legend") +
                 coord_cartesian(xlim = as.Date(c("2020-02-01", max_date))) +
                 scale_x_date(date_breaks = "14 days", date_minor_breaks = "1 days") +
                 # guides(color = guide_legend(title="Legend"), fill = F) +
                 scale_color_manual(name = "Legend",
                                    labels = c("Actual", "Diamond Princess", "IHME"),
                                    values = c("red", "green", "blue"),
                                    guide = 'legend') +
                 scale_fill_manual(values = c("green", "blue"),
                                   # labels = c(NULL, NULL),
                                   name = NULL)
                 # theme(legend.position = "none")
              )
    
  } )
  
  
  
  
  #########################################
  
  
  
  filtered_data2 <- reactive( {
    sa_data %>%
      filter(province == input$Country2) 
  } )
  
  snapshot_data <- reactive( {
    sa_data %>%
      filter(date == input$Date) %>%
      mutate(province = as.factor(province))
  } )
  
  output$cumulative_dpm_plot2 <- renderPlotly( {
    ggplotly(filtered_data2() %>%
               filter(!is.na(total_deaths_per_million)) %>%
               ggplot(aes(x = date, y = total_deaths_per_million)) +
               geom_point() +
               geom_line(aes(x = date, y = smooth.spline(total_deaths_per_million)$y, colour = "Smoothing Spline")) +
               geom_line(aes(x = first_death,
                             y = seq(0, max(filtered_data2()$total_deaths_per_million, na.rm = T),
                                     length.out = length(date))),
                         linetype = "dotted") +
               geom_line(aes(x = first_case,
                             y = seq(0, max(filtered_data2()$total_deaths_per_million, na.rm = T),
                                     length.out = length(date))),
                         linetype = "dotted") +
               geom_text(aes(x = filtered_data2()$first_death[1],
                             y = max(filtered_data2()$total_deaths_per_million, na.rm = T),
                             label = "First Death"),
                         nudge_x = 0) +
               geom_text(aes(x = filtered_data2()$first_case[1],
                             y = max(filtered_data2()$total_deaths_per_million, na.rm = T),
                             label = "First Case"),
                         nudge_x = 0) +
               # geom_text(aes(x = filtered_data()$first_death[1], y = 600, label = "First Death")) +
               # geom_text(aes(x = filtered_data()$first_case[1], y = 600, label = "First Case")) +
               labs(title = "Cumulative Deaths Per Million Over Time (with fitted smoothing spline)",
                    x = "Date",
                    y = "Cumulative deaths per million") +
               theme(legend.position = "none") +
               coord_cartesian(xlim = as.Date(c("2020-02-01", Sys.Date())),
                               ylim = c(0, max(filtered_data2()$total_deaths_per_million, na.rm = T))) +
               scale_x_date(date_breaks = "2 weeks")
    )
  } )
  
  output$new_dpm_plot2 <- renderPlotly( {
    ggplotly(filtered_data2() %>%
               filter(!is.na(new_deaths_per_million)) %>%
               ggplot(aes(x = date, y = new_deaths_per_million)) +
               geom_point() +
               # geom_line(aes(x = date, y = runmean(new_deaths_per_million, k = 7, endrule = "NA"), colour = "blue")) +
               geom_line(aes(x = date,
                             y = c(smooth.spline(runmean(new_deaths_per_million, k = 7))$y[-c((length(date) - 2):length(date))], NA, NA, NA),
                             colour = "Smoothing Spline")) +
               # geom_line(aes(x = date, y = smooth.spline(new_deaths_per_million), colour = "red")) +
               labs(title = "Daily Deaths Per Million Over Time (with fitted smoothing spline)",
                    x = "Date",
                    y = "Daily deaths per million") +
               theme(legend.position = "none") +
               coord_cartesian(xlim = as.Date(c("2020-02-01", Sys.Date()))) +
               scale_x_date(date_breaks = "2 weeks")
    )
  } )
  
  output$cumulative_dpm_plot_with_predictions2 <- renderPlotly( {
      # ggplotly(filtered_data2() %>%
      #            ggplot(aes(x = date, y = total_deaths_per_million, color = "Actual")) +
      #            geom_point() +
      #            
      #            # Diamond Princess predictions
      #            geom_line(aes(y = dp_expected_final_deaths_per_million,
      #                          x = date,
      #                          color = "Diamond Princess"),
      #                      linetype = "dashed") +
      #            geom_ribbon(aes(x = date,
      #                            ymin = dp_expected_final_deaths_per_million_lower,
      #                            ymax = dp_expected_final_deaths_per_million_upper,
      #                            color = NA,
      #                            fill = "Diamond Princess CI"),
      #                        linetype = "dotdash",
      #                        alpha = 0.2) +
      #            
      #            # IHME predictions
      #            # geom_ribbon(aes(x = date,
      #            #                 ymin = ihme_total_deaths_lower_per_million,
      #            #                 ymax = ihme_total_deaths_upper_per_million,
      #            #                 color = NA,
      #            #                 fill = "blue"),
      #            #             linetype = "dashed",
      #            #             alpha = 0.2) +
      #            # geom_line(aes(x = date, y = ihme_total_deaths_per_million, color = "IHME"),
      #            #           linetype = "dashed") +
      #          # geom_line(aes(y = ihme_final_dpm,
      #          #               x = date,
      #          #               color = "IHME"),
      #          #           linetype = "dashed") +
      #          # geom_ribbon(aes(x = date,
      #          #                 ymin = ihme_final_dpm_lower,
      #          #                 ymax = ihme_final_dpm_upper,
      #          #                 colour = NULL),
      #          #             fill = "green",
      #          #             alpha = 0.2) +
      #          
      #          # SA gov optimistic projections
      #          geom_line(aes(x = date,
      #                        y = gov_sa_predictions,
      #                        color = "Gov Optimistic 1/11 Projection"),
      #                    linetype = "dashed") +
      #            # geom_ribbon(aes(x = date,
      #            #                 ymin = rep(34015 * 1000000 / 59308690, length(date)),
      #            #                 ymax = rep(46657 * 1000000 / 59308690, length(date)),
      #            #                 color = NA,
      #            #                 fill = "Gov Projection CI"),
      #            #             alpha = 0.2) +
      #            
      #            # ASSA projection - optimistic
      #            geom_line(aes(x = date,
      #                          y = rep(48300 * 1000000 / 59308690, length(date)),
      #                          color = "ASSA Optimistic Projection"),
      #                      linetype = "dashed") +
      #            # geom_ribbon(aes(x = date,
      #            #                 ymin = rep(48300 * 1000000 / 59308690, length(date)),
      #            #                 ymax = rep(48300 * 1000000 / 59308690, length(date)),
      #            #                 color = "ASSA Optimistic Projection"),
      #            #             linteype = "dashed",
      #            #             alpha = 0.2) +
      #            
      #            labs(title = "Cumulative Deaths Per Million Over Time Compared to Various Predictions",
      #                 x = "Date",
      #                 y = "Deaths per million") +
      #            coord_cartesian(xlim = as.Date(c("2020-02-01", Sys.Date()))) +
      #            scale_x_date(date_breaks = "10 days", date_minor_breaks = "1 days") +
      #            # guides(color = guide_legend(title="Legend"), fill = F) +
      #            scale_color_manual(name = "Legend",
      #                               labels = c("Actual", "Diamond Princess", "Gov Optimistic 1/11 Projection", "ASSA Optimistic Projection"),
      #                               values = c("red", "blue", "green", "purple"),
      #                               guide = 'legend') +
      #            scale_fill_manual(values = c("green", "purple"),
      #                              # labels = c(NULL, NULL),
      #                              name = NULL)
      #          # theme(legend.position = "none")
      # )
    
    ggplotly(
      ggplot(data = filtered_data2(),
             aes(x = date, y = optimistic_gov_projection, color = "Optimistic")) +
        geom_point() +
        geom_line() +
        geom_point(aes(x = date, y = pessimistic_gov_projection, color = "Pessimistic")) +
        geom_line(aes(x = date, y = pessimistic_gov_projection, color = "Pessimistic")) +
        geom_point(aes(y = total_deaths_per_million, color = "Actual")) +
        geom_line(aes(y = total_deaths_per_million, color = "Actual")) +
        geom_ribbon(aes(x = date, ymin = total_deaths_per_million, ymax = total_deaths_per_million, fill = "Actual", color = "Actual"), alpha = 0.2, show.legend = F) +
        geom_ribbon(aes(x = sa_gov_pred_dates, ymin = optimistic_gov_projection_lower, ymax = optimistic_gov_projection_upper, fill = "Optimistic", color = "Optimistic"), alpha = 0.2, show.legend = F) +
        geom_ribbon(aes(x = sa_gov_pred_dates, ymin = pessimistic_gov_projection_lower, ymax = pessimistic_gov_projection_upper, fill = "Pessimistic", color = "Pessimistic"), alpha = 0.2, show.legend = F) +
        labs(title = "South Africa Government Projections",
             y = "DPM",
             color = "Legend"),
      dynamicTicks = T
    )
  } )
  
#   output$cumulative_dpm_plot_with_predictions3 <- renderPlot( {
#   # ggplotly(
#     ggplot(data = snapshot_data()) +
#     geom_point(aes(x = as.numeric(province), y = total_deaths_per_million, color = "Current")) +
#     geom_rect(aes(xmin = as.numeric(province) - 0.4, xmax = as.numeric(province) + 0.4, ymin = dp_expected_final_deaths_per_million_lower, ymax = dp_expected_final_deaths_per_million_upper), alpha = 0.2) +
#     geom_point(aes(x = as.numeric(province), y = dp_expected_final_deaths_per_million, color = "Prediction")) +
#     scale_x_continuous(breaks = c(1:10), labels = province) +
#     labs(title = "Progress towards Diamond Princess Prediction of Final DPM",
#          y = "DPM",
#          x = "Location")
#   # ,dynamicTicks = T
#   # )
# } )
}

shiny::shinyApp(ui = ui,server = server)