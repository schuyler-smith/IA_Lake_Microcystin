# remotes::install_github("rstudio/chromote")
# remotes::install_github("rstudio/webshot2")
unique(metadata$Location)


stations <- fread('../data/weather/wu_stations.csv')

create_wu_urls <- function(station, date){
  urls <- vector()
  for(i in seq_along(station)){
    for(j in seq_along(date)){
      urls <- c(urls, 
                paste0('https://www.wunderground.com/dashboard/pws/', 
                       station[i], 
                       '/graph/',
                       date[j],
                       '/',
                       date[j],
                       '/daily'))
    }
  }
  urls <- unique(urls)
  return(urls)
}

scrape_wu_tables <- function(url, cores = 1){
  cluster <- parallel::makeCluster(cores, type = "PSOCK")
  doParallel::registerDoParallel(cl = cluster)
  weather_data <- foreach::`%dopar%`(foreach::foreach(i = seq_along(url)), {
    data.table::rbindlist(
      rvest::html_table(
        rvest::read_html(url[i]), fill = TRUE))
  })
  parallel::stopCluster(cl = cluster)
  names(weather_data) <- paste(
    sapply(strsplit(url, '/'), `[[`, 6),
    sapply(strsplit(url, '/'), `[[`, 8),
    sep = "_")
  return(weather_data)
}
  
parse_wu_tables <- function(weather_data){
  weather <- data.table::data.table(
    Station = sapply(strsplit(names(weather_data), '_'), `[[`, 1),
    Date = as.Date(sapply(strsplit(names(weather_data), '_'), `[[`, 2)))
  weather[,avg_temp := numeric()]
  weather[,high_temp := numeric()]
  weather[,low_temp := numeric()]
  weather[,avg_humid := numeric()]
  weather[,high_humid := numeric()]
  weather[,low_humid := numeric()]
  weather[,avg_dew := numeric()]
  weather[,high_dew := numeric()]
  weather[,low_dew := numeric()]
  weather[,avg_wind := numeric()]
  weather[,high_wind := numeric()]
  weather[,low_wind := numeric()]
  weather[,avg_gust := numeric()]
  weather[,high_gust := numeric()]
  weather[,precip := numeric()]
  weather[,high_pressure := numeric()]
  weather[,low_pressure := numeric()]
  
  for(i in seq_along(weather_data)){
    weather_table <- weather_data[[i]]
    if(nrow(weather_table)>1){
      set(weather, as.integer(i), names(weather)[-c(1:2)], as.list(c(
        avg_temp = temp_F_to_C(as.numeric(gsub(".?F", "", weather_table[V1 %in% "Temperature"]$Average))),
        high_temp = temp_F_to_C(as.numeric(gsub(".?F", "", weather_table[V1 %in% "Temperature"]$High))),
        low_temp = temp_F_to_C(as.numeric(gsub(".?F", "", weather_table[V1 %in% "Temperature"]$Low))),
        avg_humid = as.numeric(gsub(".?%", "", weather_table[V1 %in% "Humidity"]$Average)),
        high_humid = as.numeric(gsub(".?%", "", weather_table[V1 %in% "Humidity"]$High)),
        low_humid = as.numeric(gsub(".?%", "", weather_table[V1 %in% "Humidity"]$Low)),
        avg_dew = temp_F_to_C(as.numeric(gsub(".?F", "", weather_table[V1 %in% "Dew Point"]$Average))),
        high_dew = temp_F_to_C(as.numeric(gsub(".?F", "", weather_table[V1 %in% "Dew Point"]$High))),
        low_dew = temp_F_to_C(as.numeric(gsub(".?F", "", weather_table[V1 %in% "Dew Point"]$Low))),
        avg_wind = as.numeric(gsub(".?mph", "", weather_table[V1 %in% "Wind Speed"]$Average)),
        high_wind = as.numeric(gsub(".?mph", "", weather_table[V1 %in% "Wind Speed"]$High)),
        low_wind = as.numeric(gsub(".?mph", "", weather_table[V1 %in% "Wind Speed"]$Low)),
        avg_gust = as.numeric(gsub(".?mph", "", weather_table[V1 %in% "Wind Gust"]$Average)),
        high_gust = as.numeric(gsub(".?mph", "", weather_table[V1 %in% "Wind Gust"]$High)),
        precip = as.numeric(gsub(".?in", "", weather_table[V1 %in% "Precipitation"]$High)),
        high_pressure = as.numeric(gsub(".?in", "", weather_table[V1 %in% "Pressure"]$High)),
        low_pressure = as.numeric(gsub(".?in", "", weather_table[V1 %in% "Pressure"]$Low))
      ))
      )
    }
  }
  return(data.table(weather))
}

retry_scrape_wu_tables <- function(weather_table, second_station, cores = 1){
    original_date <- as.Date(weather_table$Date)
    dates <- c(original_date, 
               as.Date(original_date)-1, 
               as.Date(original_date)+1,
               as.Date(original_date)-2,
               as.Date(original_date)+2)
    urls <- c(rbind(create_wu_urls(weather_table$Station, dates),
                    create_wu_urls(second_station, dates)))
    if(is.na(second_station))urls <- create_wu_urls(weather_table$Station, dates)
    new_tables <- scrape_wu_tables(urls, 3)
    new_tables <- parse_wu_tables(new_tables)
    new_tables <- new_tables[!(is.na(high_temp))]
    if(nrow(new_tables) > 0 ){set(weather_table, 1L, names(weather_table)[-2], new_tables[1,-2])
      return(data.table(weather_table))
    } else {return(NULL)}
}

KIAMANSO3 2019-08-06