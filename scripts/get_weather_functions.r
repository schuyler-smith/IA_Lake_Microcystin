screen_grab <- function(urls, pdf.files, by=5){
  for(j in seq(1,length(pdf.files), by=by)){
    webshot2::webshot(urls[c(j:(j+(by-1)))[j:(j+(by-1)) <= length(pdf.files)]], 
                      pdf.files[c(j:(j+(by-1)))[j:(j+(by-1)) <= length(pdf.files)]], 
                      delay = 5)
  }
}

create_wu_urls_pdfs <- function(location, dates, station){
	pdf.files <- vector()
	urls <- vector()
	for(i in seq_along(dates)){
		date <- dates[i]
		pdf.files <- c(pdf.files, 
			paste0('../data/weather/raw_data/wu_screens/', 
			location,
			'_',
			date,
			'_',
			station,'.pdf'))
		urls <- c(urls, 
			paste0('https://www.wunderground.com/dashboard/pws/', 
			station, 
			'/graph/',
			date,
			'/',
			date,
			'/daily'))
	}
	return(data.frame(urls = urls, pdf.files = pdf.files))
}

parse_wu_screengrab <- function(pdf.files){
	weather <- data.table::data.table(
				Location = sapply(sapply(tools::file_path_sans_ext(basename(pdf.files)), strsplit, '_'), `[[`, 1),
				Date = as.Date(sapply(sapply(tools::file_path_sans_ext(basename(pdf.files)), strsplit, '_'), `[[`, 2)),
				Station = sapply(sapply(tools::file_path_sans_ext(basename(pdf.files)), strsplit, '_'), `[[`, 3)
			)
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

	for(i in seq_along(pdf.files)){
		pdf.text <- pdftools::pdf_text(pdf.files[i])
		pdf.text <- strsplit(pdf.text, "\n")
		if(length(pdf.text) < 4){
			next
		} else {
			pdf.text <- pdf.text[[4]]
			temperature <- unlist(strsplit(pdf.text[grep("Temperature", pdf.text)], ' '))
			temp_high <- as.numeric(temperature[temperature != ""][2])
			temp_low <- as.numeric(temperature[temperature != ""][4])
			temp_avg <- as.numeric(temperature[temperature != ""][6])

			dew <- unlist(strsplit(pdf.text[grep("Dew Point", pdf.text)], ' '))
			dew_high <- as.numeric(dew[dew != ""][3])
			dew_low <- as.numeric(dew[dew != ""][5])
			dew_avg <- as.numeric(dew[dew != ""][7])

			humid <- unlist(strsplit(pdf.text[grep("Humidity", pdf.text)], ' '))
			humid_high <- as.numeric(humid[humid != ""][2])
			humid_low <- as.numeric(humid[humid != ""][4])
			humid_avg <- as.numeric(humid[humid != ""][6])

			wind <- unlist(strsplit(pdf.text[grep("Wind Speed", pdf.text)], ' '))
			wind_high <- as.numeric(wind[wind != ""][3])
			wind_low <- as.numeric(wind[wind != ""][5])
			wind_avg <- as.numeric(wind[wind != ""][7])

			gust <- unlist(strsplit(pdf.text[grep("Wind Gust", pdf.text)], ' '))
			gust_high <- as.numeric(gust[gust != ""][3])
			gust_avg <- as.numeric(gust[gust != ""][6])

			precip <- unlist(strsplit(pdf.text[grep("Precipitation", pdf.text)], ' '))
			precip <- as.numeric(precip[precip != ""][2])

			set(weather, as.integer(i), names(weather)[-c(1:3)], as.list(c(
					avg_temp = temp_avg,
					high_temp = temp_high,
					low_temp = temp_low,
					avg_humid = humid_avg,
					high_humid = humid_high,
					low_humid = humid_low,
					avg_dew = dew_avg,
					high_dew = dew_high,
					low_dew = dew_low,
					avg_wind = wind_avg,
					high_wind = wind_high,
					low_wind = wind_low,
					avg_gust = gust_avg,
					high_gust = gust_high,
					precip = precip
				))
			)
		}
	}
	return(data.table(weather))
}


retry_screen_grab <- function(weather_table, stations_file){
  for(i in weather_table[, .I[is.na(avg_temp)]]){
    original_date <- weather_table[i]$Date
    dates <- c(original_date, 
    	as.Date(original_date)-1, 
    	as.Date(original_date)+1,
    	as.Date(original_date)-2,
    	as.Date(original_date)+2)
    for(date_i in seq_along(dates)){
      for(station in stations[Lake %in% weather_table[i]$Location][,c("Station", "Station2")]){
        pdf.file <- paste0('../data//weather/raw_data/wu_screens/', 
                           weather_table[i]$Location,
                           '_',
                           original_date,
                           '_',
                           station,'.pdf')
        if(!(file.exists(pdf.file))){
          webshot2::webshot(paste0('https://www.wunderground.com/dashboard/pws/', 
                                   station,
                                   '/graph/',
                                   dates[date_i],
                                   '/',
                                   dates[date_i],
                                   '/daily'),
                            pdf.file, 
                            delay = 5)          
        }
        new <- parse_wu_screengrab(pdf.file)
        if(!(is.na(new$avg_temp))){
          set(weather_table, as.integer(i), names(weather_table), new)
          break
        }
      }
      if(!(is.na(weather_table[i]$avg_temp))){break}
    }
  }
  return(data.table(weather_table))
}



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
	return(urls)
}

scrape_wu_tables <- function(url){
  weather_data<-list()
  for(i in seq_along(url)){
  weather_data[[i]] <- 
    data.table::rbindlist(
      rvest::html_table(
        rvest::read_html(url[i]), fill = TRUE))
  names(weather_data)[i] <- paste(
    sapply(strsplit(url[i], '/'), `[[`, 6),
    sapply(strsplit(url[i], '/'), `[[`, 8),
    sep = "_")
  }
  return(weather_data)
}

parse_wu_tables <- function(weather_data){
  weather <- data.table::data.table(
    Station = sapply(strsplit(names(weather_data), '_'), `[[`, 1),
    Date = sapply(strsplit(names(weather_data), '_'), `[[`, 2))
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
      set(weather, as.integer(i), names(weather)[-c(1:2)], as.list(c(
        avg_temp = as.numeric(gsub(".?F", "", weather_table[V1 %in% "Temperature"]$Average)),
        high_temp = as.numeric(gsub(".?F", "", weather_table[V1 %in% "Temperature"]$High)),
        low_temp = as.numeric(gsub(".?F", "", weather_table[V1 %in% "Temperature"]$Low)),
        avg_humid = as.numeric(gsub(".?%", "", weather_table[V1 %in% "Humidity"]$Average)),
        high_humid = as.numeric(gsub(".?%", "", weather_table[V1 %in% "Humidity"]$High)),
        low_humid = as.numeric(gsub(".?%", "", weather_table[V1 %in% "Humidity"]$Low)),
        avg_dew = as.numeric(gsub(".?F", "", weather_table[V1 %in% "Dew Point"]$Average)),
        high_dew = as.numeric(gsub(".?F", "", weather_table[V1 %in% "Dew Point"]$High)),
        low_dew = as.numeric(gsub(".?F", "", weather_table[V1 %in% "Dew Point"]$Low)),
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
  return(data.table(weather))
}


wu_tables <- function(station, date){
	wu_urls <- create_wu_urls(station, date)

	}
}

