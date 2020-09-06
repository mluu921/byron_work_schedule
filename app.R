library(tidyverse)
library(shiny)
library(shinydashboard)
library(lubridate)

options(shiny.autoreload = T)

source('utils.R')

data <- load_data()

dashboard_header <- dashboardHeader(title = 'Is Byron Ng Working?')

dashboard_sidebar <- dashboardSidebar(disable = T)

dashboard_body <- dashboardBody(
  fluidRow(
    infoBoxOutput('infobox_todays_date', width = 12),
    infoBoxOutput('infobox_isheworkingtoday', width = 12),
    infoBoxOutput('infobox_isheworkingweekend', width = 12),
    box(
      title = 'Calendar',
      width = 12,
      status = 'primary',
      collapsible = T,
      plotOutput('calendar')
    )
  )
)

ui <- dashboardPage(
  skin = 'purple',
  dashboard_header,
  dashboard_sidebar,
  dashboard_body
)

server <- function(input, output, session) {
  
  todays_date <- Sys.time() - hours(7)
  
  output$infobox_todays_date <- renderInfoBox({
    
    date <- format(date(todays_date), '%A - %B %d, %Y')
    
    infoBox(title = "Today's Date", value = date, width = 12, icon = icon('calendar'))
    
  })
  
  output$infobox_isheworkingtoday <- renderInfoBox({
    
    working <- function(todays_date) {
      
      results <- data %>%
        filter(date == date(todays_date)) %>%
        pull(work) %>% .[[1]]
      
      return(results)
    }
    
    isheworking <- working(todays_date)
    
    today_off <- ifelse(isheworking == 'OFF', 'YES', 'NO')
    
    color <- ifelse(today_off == 'YES', 'green', 'red')
    
    selected_icon <- ifelse(today_off == 'YES', 'smile-o', 'frown-o')
    
    message <- ifelse(
      today_off == 'YES',
      'YES - Byron is NOT working today',
      'NO - Byron IS working today'
    )
    
    infoBox(
      title = 'IS BYRON OFF TODAY?',
      value = message,
      color = color,
      icon = icon(selected_icon)
    )
    
  })
  
  output$infobox_isheworkingweekend <- renderInfoBox({
    
    weekend_work <- data %>%
      filter(
        week == week(todays_date) & dayofweek %in% c('Sat', 'Sun')
      ) %>%
      pull(work)
    
    weekend_off <- ifelse(any(weekend_work == 'WORK'), 'NO', 'YES')
    
    color <- ifelse(weekend_off == 'NO', 'red', 'green')
    
    selected_icon <- ifelse(weekend_off == 'NO', 'frown-o', 'smile-o')
    
    message <- ifelse(
      weekend_off == 'YES',
      'YES - Byron is NOT working this weekend',
      'NO - Byron IS working this weekend'
    )
    
    infoBox(
      title = 'IS BYRON OFF THIS WEEKEND?',
      value = message,
      color = color,
      icon = icon(selected_icon)
    )
  })
  
  

  output$calendar <- renderPlot({
    
    make_calendar <- function(data) {
      usc_colors <- c('#990000', '#FFCC00')
      
      data <- data %>%
        filter(
          month %in% month(todays_date, label = T, abbr = F) & 
            year == year(todays_date)
        ) %>%
        mutate(
          month = glue::glue('{month} {year}')
        )
      
      ggplot(data, aes(x = dayofweek, y = week)) +
        facet_wrap( ~ month, scales = 'free', ncol = 3) +
        geom_tile(color = 'black', aes(fill = work)) +
        geom_text(aes(label = day, color = today,
                      fontface = ifelse(today == 1, 'bold', 'plain')),
                  show.legend = F,
                  size = 10) +
        scale_fill_manual(values = usc_colors) +
        scale_color_manual(values = c('#000000', '#FFFFFF')) +
        scale_y_reverse() +
        theme_light(base_size = 20) +
        theme(
          axis.text.y = element_blank(),
          panel.grid = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title = element_blank(),
          legend.title = element_blank(),
          legend.position = 'bottom'
        )
      
    }
    
    plot <- make_calendar(data)
    
    return(plot)
    
  })
  
}

shinyApp(ui, server)