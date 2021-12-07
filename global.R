library(shiny)
library(shinyWidgets)
library(ggplot2)
library(tidyverse)
library(rworldmap)
library(rnaturalearth)
library(rgeos)

variable_choices = c("cumulative_cases",
                     "new_cases_past_week",
                     "cumulative_deaths",
                     "new_deaths_past_week",
                     "cumulative_cases_per_million",
                     "new_cases_per_million_past_week",
                     "cumulative_deaths_per_million",
                     "new_deaths_per_million_past_week")

covid <- read_csv("www/COVID_data_2021-11-08.csv")

world <- map_data("world")

# find coordinates for each country
earth <- ne_countries(scale = "medium", returnclass = "sp")
earth_coord <- as.data.frame(gCentroid(earth, byid=TRUE))
earth_coord$country <- earth$name
earth_coord$continent <- earth$continent
names(earth_coord) <- c("long", "lat", "country","continent")

#find coordinates for each continent
continent_coord <- earth_coord %>% 
  group_by(continent) %>% 
  summarize(lat = median(lat), long = median(long))


# add lat and long data to covid data to see which won't work
covid_loc_test <- left_join(covid,earth_coord, by = c("country"))

# find which country names didn't match
missing <- covid_loc_test %>% filter(is.na(lat)) %>% 
  group_by(country) %>% summarize(lat = mean(lat))
missing_country <- missing$country


# define some variables to fix the mis match
earth_countries <- unique(earth_coord$country)
count = 0
matching_countries1 <- data.frame()
matching_countries <- data.frame()

# attempt to match country names 
for (i in 1:length(missing_country)){
  for (j in 1:length(earth_countries)){
    if(grepl(missing_country[i], earth_countries[j], fixed = FALSE) | grepl(earth_countries[j], missing_country[i], fixed = FALSE)){
      count = count +1
      matching_countries1<- c(missing_country[i], earth_countries[j])
      
      # add vector to a dataframe
      df <- data.frame(matrix(nrow = 1, data = matching_countries1))
      matching_countries <- rbind(matching_countries,df)
    }
  }
}

names(matching_countries) <- c("covidNames", "earthNames")
matching_countries

# find countries that still don't match
missing_country <- as.data.frame(missing_country)

still_missing <- merge(missing_country, matching_countries, 
                       by.x = "missing_country", by.y = "covidNames",all.x = TRUE) %>% 
  filter(is.na(earthNames))

# manually determine these are missing values
final_missing <- data.frame(matrix(nrow = 9, data = c("Burma", "Myanmar",
                                                      "Cote d'Ivoire" ,"Côte d'Ivoire",
                                                      "Eswatini", "Swaziland",
                                                      "Lao People's Democratic Republic", "Lao PDR",
                                                      "Saint Kitts and Nevis", "St. Kitts and Nevis" ,
                                                      "Saint Vincent and the Grenadines", "St. Vin. and Gren." ,
                                                      "Sao Tome and Principe", "São Tomé and Principe" ,
                                                      "UK", "United Kingdom",
                                                      "USA", "United States" ),byrow = TRUE))  
names(final_missing) <- c("covidNames", "earthNames")

# complete the list of matching countries
matching_countries <- rbind(matching_countries,final_missing)

#replace the names in the covid data with names from earth
for (i in 1:nrow(matching_countries)){
  name <- matching_countries[i,"covidNames"]
  covid["country"][covid["country"] == name] <- matching_countries[i,"earthNames"]
  
}

covid_loc <- merge(covid, earth_coord, by.x = "country", by.y = "country", all.x)

cases_plot <- function(covidVar, countryChoice, dateRange){
  covidVarExpr <- enquo(covidVar)
  covid_loc %>% group_by(country, date) %>% 
    summarize(cases = sum(!!covidVarExpr)) %>% 
    filter(country == countryChoice) %>% 
    ggplot()+
    geom_line(aes(x = date, y = cases))+
    scale_x_continuous(limits = c(as.Date(dateRange[1]), as.Date(dateRange[2])))
  
}


world_map <- ne_countries(scale = "medium", returnclass = "sf")
ggplot() +
  geom_sf(data = world_map)+
  labs( x = "Longitude", y = "Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$admin)), " countries)"))+
  geom_point(data = covid_loc, aes(x=long, y = lat, size =  new_cases_past_week, color = continent))





