library("leaflet")
library("rgdal")
library("RColorBrewer")
library("maptools")
library("eurostat")
library("plotly")
library("countrycode")


colourCount = length(unique(team2004_geodata$CNTR_CODE))
getPalette = colorRampPalette(brewer.pal(8, "Greens"))

pal1 = getPalette(colourCount)
pal <- colorFactor(
  topo.colors(24),
  domain = team2004_geodata$CNTR_CODE)

bins <- c(-3, -2.5, -2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5,Inf)
pal2 <- colorBin(topo.colors(24), domain = team2004_geodata$REF, bins = bins)


labels <- sprintf(
  "<strong>%s</strong><br/>Difference from average increase: %g ",
  team2004_geodata$CNTR_CODE, team2004_geodata$REF
) %>% lapply(htmltools::HTML)


geodata <- get_eurostat_geospatial(output_class = "spdf", resolution = "60", nuts_level = "0", year = 2016)
team2004_geodata <- subset(geodata,geodata$CNTR_CODE %in% validcountries)
ref_matrix <- spread_data2map[,c("geo","REF")]
team2004_geodata <- merge(team2004_geodata,ref_matrix, by.x = "id", by.y = "geo")


m2 <- leaflet(team2004_geodata) %>%
  addTiles() %>% 
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor =  ~pal2(team2004_geodata$REF),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = pal2, values = ~REF, opacity = 0.7, title = NULL,
            position = "bottomright") 


m2

spread_data2map <- spread_data2map[,3:10]

spread_data2map$hover <- with(spread_data2map, paste(spread_data2map$`geo`, '<br>', "2005", spread_data2map$`2005`,"<br>","2008", spread_data2map$`2008`, "<br>",
                                                     "2011", spread_data2map$`2011`, "<br>","2014", spread_data2map$`2014`, "<br>",
                                                     "2017", spread_data2map$`2017`, "<br>","Difference", spread_data2map$`DIFF`))

spread_data2map$iso_code <- countrycode(spread_data2map[,1],"eurostat","iso3c")


# common plot options
g <- list(
  scope = 'europe',
  showframe = F,
  showland = T,
  landcolor = toRGB("grey90")
)


p_map <- spread_data2map %>%
  plot_geo(
    locationmode = 'eu countries'
  ) %>%
  add_trace(
    z = spread_data2map$DIFF, text = spread_data2map$hover, locations = spread_data2map$iso_code,
    color = spread_data2map$DIFF, colors = 'Greens'
  ) %>%
  colorbar(title = "Increase of GDP") %>%
  layout(
    title = 'Increase of GDP in EU between 2005 and 2017<br> 
          Source: <a href="https://ec.europa.eu/eurostat/tgm/table.do?tab=table&init=1&language=en&pcode=sdg_08_10&plugin=1">Eurostat</a>',
    geo = g
  )

p_map