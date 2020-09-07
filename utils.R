
load_data <- function() {
  
  todays_date <- Sys.time() - hours(7)
  
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

  
  data <- data %>%
    mutate(
      today = ifelse(date == date(todays_date), 1, 0) %>% factor(),
      work = factor(work, levels = c('OFF', 'WORK'), labels = c('WORK', 'OFF'))
    )
  
  return(data)
}