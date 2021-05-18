library(sf)
library(mapview)
library(tidyverse)
library(ggmap)

stations <- fread('../data/weather/wu_stations.csv')
lakes <- fread('../data/metadata/lakes')
set(lakes, j = "Lake", value = gsub(" Beach", "", lakes$Lake))

locations_sf <- sf::st_as_sf(stations[,-c("Station", "Station2")], coords = c("Longitude", "Latitude"), crs = 4326)
mapview::mapview(locations_sf)

geocode("iowa")

iowa <- readRDS('../data/iowa_terrain_map.RDS')
ggmap(iowa) + 
  ggplot2::labs(x="Longitude", y = "Latitude") +
  geom_point(data = stations, aes(x = Longitude, y = Latitude), size = 0.5) + 
  geom_point(data = lakes, aes(x = Longitude, y = Latitude), color = 'blue') +
  ggrepel::geom_text_repel(data = lakes, aes(x = Longitude, y = Latitude, label =Lake), 
                            point.padding = unit(0.5,"lines"), 
                            box.padding = unit(0.35, "lines"), 
                            label.r = 0,
                            label.padding = unit(0.2,"lines"),
                            max.overlaps = 10,
                            size = 2.8,
                            max.time = 10)
  

# iowa <- get_googlemap(center = c(-93.6, 41.9),
#                       zoom = 7,
#                       size = c(640, 540),
#                       scale = 1,
#                       style = paste("feature:road|visibility:off",
#                                     "style=element:labels|visibility:off",
#                                     sep = "&")
#                       )

# iowa_by <- ggmap::get_googlemap(center = c(-93.6, 41.9), 
#                          zoom = 7, 
#                          scale = 2, 
#                          color = "bw", 
#                          style = paste("feature:road|visibility:off", 
#                                        "style=element:labels|visibility:off",
#                                        sep = "&")
#                          )

get_googlemap2 <- function(
  api_key = "Your API Key",
  center = c(lon = -95.3632715, lat = 29.7632836),
  zoom = 10, size = c(640, 640), scale = 2,
  maptype = c("terrain", "satellite", "roadmap", "hybrid"),
  grayscale = FALSE, style
) {
  maptype <- match.arg(maptype)

  params <- c(
    center = paste0(center[c(2L, 1L)], collapse = ","),
    zoom = zoom,
    size = paste0(size, collapse = "x"),
    scale = scale,
    maptype = maptype,
    style = style,
    key = api_key
  )
  url <- "https://maps.googleapis.com/maps/api/staticmap"
  urltools::parameters(url) <- paste(names(params), params, sep = "=", collapse = "&")
  url <- URLencode(url)

  message("Souce: ", url)
  img <- magick::image_read(httr::content(httr::GET(url)))
  if (grayscale) img <- magick::image_quantize(img, colorspace = "gray")
  ll <- RgoogleMaps::XY2LatLon(
    list(lat = center[2], lon = center[1], zoom = zoom),
    -size[1]/2 + 0.5, -size[2]/2 - 0.5
  )
  ur <- RgoogleMaps::XY2LatLon(
    list(lat = center[2], lon = center[1], zoom = zoom),
    size[1]/2 + 0.5, size[2]/2 - 0.5
  )
  structure(
    as.raster(img), class = c("ggmap", "raster"),
    source = "google", maptype = maptype, zoom = zoom,
    bb = tibble::tibble(ll.lat = ll[1], ll.lon = ll[2], ur.lat = ur[1], ur.lon = ur[2])
  )
}

iowa_bw <- get_googlemap2(api_key = "***REMOVED***",
                      center = c(-93.6, 42.05),
                      zoom = 7,
                      size = c(640, 420),
                      scale = 2,
                      grayscale = TRUE,
                      style = paste("feature:road|visibility:off",
                                    "style=element:labels|visibility:off",
                                    sep = "&")
)
