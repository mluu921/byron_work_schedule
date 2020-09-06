library(tidyverse)
library(lubridate)
library(ggsci)

load_data <- function() {
  data <- tibble(
    date = seq.Date(mdy('1/1/2018'), mdy('12/31/2030'), '1 day')
  )
  
  data <- data %>%
    mutate(
      'week' = isoweek(date),
      'dayofweek' = wday(date, label = T),
      'month' = month(date, label = T, abbr = F),
      'day' = day(date),
      'year' = year(date)
    ) 
  
  sequence <-
    c(
      "OFF",
      "WORK",
      "WORK",
      "OFF",
      "OFF",
      "WORK",
      "WORK",
      "WORK",
      "OFF",
      "OFF",
      "WORK",
      "WORK",
      "OFF",
      "OFF"
    )
  
  data <- data %>%
    mutate(
      work = rep(sequence, length = nrow(data))
    ) 
  
  data <- data %>%
    mutate(dayofweek = factor(
      dayofweek,
      levels = c("Mon", 'Tue', 'Wed', 'Thu', 'Fri', 'Sat',
                 'Sun')
    ))
  
  temp <- data %>%
    filter(
      date %in% seq(mdy('1/1/2020'), mdy('12/31/2020'), '1 day')
    )
  
  temp <- temp %>%
    mutate(
      today = ifelse(date == Sys.Date(), 1, 0) %>% factor()
    )
  
  return(temp)
}


data <- load_data()


make_calendar <- function(data) {
  usc_colors <- c('#990000', '#FFCC00')
  
  ggplot(data, aes(x = dayofweek, y = week)) +
    facet_wrap( ~ month, scales = 'free', ncol = 3) +
    geom_tile(color = 'black', aes(fill = work)) +
    geom_text(aes(label = day, color = today,
                  fontface = ifelse(today == 1, 'bold', 'plain')),
              show.legend = F) +
    scale_fill_manual(values = usc_colors) +
    scale_color_manual(values = c('#000000', '#FFFFFF')) +
    scale_y_reverse() +
    theme_light() +
    theme(
      axis.text.y = element_blank(),
      panel.grid = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title = element_blank(),
      legend.title = element_blank(),
      legend.position = 'bottom'
    )
  
}

make_calendar(data)
