
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
        dep_time = hms(parse_time(dep_time, format = "%H:%M")),
        arr_time = hms(parse_time(arr_time, format = "%H:%M")),
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
        time_taken = 
            as.duration(parse_time(time_taken, format = "%hh %Mm")),
        flight_code = str_c(ch_code, "-", as.character(num_code)),
        price_usd = round(price * 0.01317, 2)) %>%
    select(date, airline, flight_code, everything()) %>%
    select(-num_code, -ch_code) %>%
    rename(duration = time_taken) %>%
    mutate(class = "business")
}

# I tested with Date-times, periods, durations, and hms. I found that 
# durations were good for length of the flight and hms was good for 
# holding the time of day values. 
{
view(business_clean)
filter(flights, arr_time <= hms(parse_time("00h 00m", format = "%hh %Mm")))
as.duration(parse_time("24h 30m", format = "%hh %Mm"))
}

#Converting economy to more correct forms for analysis
{
    economy_clean = economy_edit %>%
        mutate(
            from = as_factor(from),
            to = as_factor(to),
            date = parse_date(date, format = "%d-%m-%Y"),
            dep_time = hms(parse_time(dep_time, format = "%H:%M")),
            arr_time = hms(parse_time(arr_time, format = "%H:%M")),
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
            time_taken = 
                as.duration(parse_time(time_taken, format = "%hh %Mm")),
            flight_code = str_c(ch_code, "-", as.character(num_code)),
            price_usd = round(price * 0.01317, 2)) %>%
        select(date, airline, flight_code, everything()) %>%
        select(-num_code, -ch_code) %>%
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
view(flights)
nrow(flights)
nrow(df)
}

#explorations
{
flights %>%
    filter(date == date("2022-02-20") & flight_code == "UK-838")
df %>%
    filter(flight == "UK-838")
df
flights %>% 
    group_by(date, flight_code, price) %>%
    count() %>%
    arrange(desc(n))
vignette(package = 'dplyr')
}


#plot to see predictions to be made
{
df %>%
    ggplot(aes(days_left, price, color = class)) +
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

#ideas for analysis
# highest price to from pair city
# how duration affects price
# dates with the most price changes
# 
