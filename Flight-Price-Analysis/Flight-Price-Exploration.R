
# setup
{
library(tidyverse)
library(lubridate)
getwd()
dir()
setwd("C:/Users/hellm/Desktop/Projects/r_projects/Data-Analytics-in-R/Flight-Price-Analysis")
df = read.csv("Clean_Dataset.csv")
business = read.csv("business.csv")
economy = read.csv("economy.csv")

glimpse(df)
glimpse(business)
glimpse(economy)
}

# clean up
{
# clearing the nonsensical times
index = str_detect(economy$time_taken, "1\\...h")
economy_edit = filter(economy, !index)
}

#Converting business to more correct forms for analysis
{
business_clean = business %>%
    mutate(
        from = as_factor(from),
        to = as_factor(to),
        date = parse_date(date, format = "%d-%m-%Y"),
        dep_time = make_datetime(
            year = year(date), 
            month = month(date), 
            day = day(date),
            hour = hour(parse_time(dep_time, format = "%H:%M")), 
            min = minute(parse_time(dep_time, format = "%H:%M")),
            sec = 0),
        arr_time = make_datetime(
            year = year(date), 
            month = month(date),
            day = day(date),
            hour = hour(parse_time(arr_time, format = "%H:%M")), 
            min = minute(parse_time(arr_time, format = "%H:%M")), 
            sec = 0),
        airline = as_factor(parse_character(airline)),
        price = as.integer(
            parse_number(
                price, 
                locale = locale(grouping_mark = ","))),
        stop = str_replace_all(stop, "\\t", "") %>%
            str_replace_all("\\n", "") %>%
            str_replace("Via\\s.*", "") %>%
            str_replace("2\\+-stop", "2-or-more-stops") %>%
            trimws() %>%
            as_factor(),
        time_taken_sec = (
            day(parse_time(time_taken, format = "%hh %Mm")) * 60 * 60 * 24 +
            hour(parse_time(time_taken, format = "%hh %Mm")) * 60 * 60 + 
            minute(parse_time(time_taken, format = "%hh %Mm")) * 60),
        time_taken = period(second = time_taken_sec),
        flight_code = str_c(ch_code, "-", as.character(num_code))) %>%
    select(date, airline, flight_code, everything()) %>%
    select(-time_taken_sec, -num_code, -ch_code) %>%
    rename(duration = time_taken) %>%
    mutate(class = "business")
}

#Converting economy to more correct forms for analysis
{
economy_clean = economy_edit %>%
    mutate(
        from = as_factor(from),
        to = as_factor(to),
        date = parse_date(date, format = "%d-%m-%Y"),
        dep_time = make_datetime(
            year = year(date), 
            month = month(date), 
            day = day(date),
            hour = hour(parse_time(dep_time, format = "%H:%M")), 
            min = minute(parse_time(dep_time, format = "%H:%M")),
            sec = 0),
        arr_time = make_datetime(
            year = year(date), 
            month = month(date),
            day = day(date),
            hour = hour(parse_time(arr_time, format = "%H:%M")), 
            min = minute(parse_time(arr_time, format = "%H:%M")), 
            sec = 0),
        airline = as_factor(parse_character(airline)),
        price = as.integer(
            parse_number(
                price, 
                locale = locale(grouping_mark = ","))),
        stop = str_replace_all(stop, "\\t", "") %>%
            str_replace_all("\\n", "") %>%
            str_replace("Via\\s.*", "") %>%
            str_replace("2\\+-stop", "2-or-more-stops") %>%
            trimws() %>%
            as_factor(),
        time_taken_sec = (
            day(parse_time(time_taken, format = "%hh %Mm")) * 60 * 60 * 24 +
            hour(parse_time(time_taken, format = "%hh %Mm")) * 60 * 60 + 
            minute(parse_time(time_taken, format = "%hh %Mm")) * 60),
        time_taken = period(second = time_taken_sec),
        flight_code = str_c(ch_code, "-", as.character(num_code))) %>%
    select(date, airline, flight_code, everything()) %>%
    select(-time_taken_sec, -num_code, -ch_code) %>%
    rename(duration = time_taken) %>%
    mutate(class = "economy")
}

#Joining the sets
{
flights = full_join(economy_clean, business_clean)  %>%
    mutate(class = as_factor(class))
}

#checking 
{
summary(flights)
nrow(flights)
nrow(df)
}

flights %>% 
    group_by(date, flight_code, dep_time) %>%
    count() %>%
    arrange(desc(n))
vignette(package = 'dplyr')

#plot to see predictions to be made
{
df %>%
    ggplot(aes(duration, price, color = class)) +
        geom_point(alpha = 0.01) +
        geom_smooth()
}


# Work to illuminate how duration == 0 was occurring
{
    alltimes = unique(business$time_taken)
    alltimes
    index2 = str_detect(business$time_taken, "24h 00m")
    nrow(filter(tibble(business$time_taken), index2))
    nrow(business)
    nrow(filter(business_clean, duration <= 0))
}

#extras
{
#Filter to test how the data structures line-up
business_AI = business %>%
    filter(airline == "Air India")

df_AI = df %>%
    filter(airline == "Air_India" & class == "Business")

glimpse(business_clean)
glimpse(df_AI)
}
