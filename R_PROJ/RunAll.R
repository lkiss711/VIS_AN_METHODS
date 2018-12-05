# 
# author: Laszlo Kiss
# email: laci.kiss at gmail.com
# 
# ReadData.R
# 


library("eurostat")
library("tidyr")
library("dplyr")

# Sys.setenv("plotly_username"="lkiss711")
# Sys.setenv("plotly_api_key"="*******")


id <-  "sdg_08_10"

data <- get_eurostat(id, time_format = "date")

data$time = substring(data$time,0,4)

validcountries <- c("BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE","UK")
#validcountries <- c("CY","CZ","EE","HU","LV","LT","MT","PL","SI","SK")
validyears <- c("2005","2008","2011","2014","2017")

data2plot <- dplyr::filter(data, (grepl(paste(validyears,collapse = '|'),time) & grepl(paste(validcountries,collapse = '|'),geo) & unit == "CLV10_EUR_HAB"))
data2plot <- data2plot[,3:5]

data2map <- dplyr::filter(data, (grepl(paste(validyears,collapse = '|'),time) & grepl(paste(validcountries,collapse = '|'),geo) & unit == "CLV10_EUR_HAB"))
data2map$time <- substring(data2map$time,1,4)


data2plot <- label_eurostat(data2plot, fix_duplicated = TRUE)
data2plot$time <- substring(data2plot$time,1,4)


spread_data2plot <- spread(data2plot,time,values)
spread_data2plot <- cbind(spread_data2plot,DIFF = (spread_data2plot$`2017`- spread_data2plot$`2005`))
spread_data2plot <- cbind(spread_data2plot,REF = round((spread_data2plot$DIFF - mean(spread_data2plot$DIFF))/sd(spread_data2plot$DIFF), 2))
spread_data2plot <- cbind(spread_data2plot,IND = ifelse(spread_data2plot$REF < 0, "below", "above"))
spread_data2plot <- spread_data2plot[order(spread_data2plot$REF),]
spread_data2plot$geo <- factor(spread_data2plot$`geo`, levels = spread_data2plot$`geo`) 

spread_data2map <- spread(data2map,time,values)
spread_data2map <- cbind(spread_data2map,DIFF = (spread_data2map$`2017`- spread_data2map$`2005`))
spread_data2map <- cbind(spread_data2map,REF = round((spread_data2map$DIFF - mean(spread_data2map$DIFF))/sd(spread_data2map$DIFF), 2))


geodata <- get_eurostat_geospatial(output_class = "spdf", resolution = "60", nuts_level = "0", year = 2016)
team2004_geodata <- subset(geodata,geodata$CNTR_CODE %in% validcountries)
ref_matrix <- spread_data2map[,c("geo","REF")]
team2004_geodata <- merge(team2004_geodata,ref_matrix, by.x = "id", by.y = "geo")


# 
# DrawPlots.R
# 

library("dplyr")
library("ggplot2")
library("ggthemes")
library("CGPfunctions")
library("plotly")


theme_set(theme_bw())

tufte_sort <- function(df, x="year", y="value", group="group", method="tufte", min.space=0.05) {
  ## First rename the columns for consistency
  ids <- match(c(x, y, group), names(df))
  df <- df[,ids]
  names(df) <- c("x", "y", "group")
  
  ## Expand grid to ensure every combination has a defined value
  tmp <- expand.grid(x=unique(df$x), group=unique(df$group))
  tmp <- merge(df, tmp, all.y=TRUE)
  df <- mutate(tmp, y=ifelse(is.na(y), 0, y))
  
  ## Cast into a matrix shape and arrange by first column
  require(reshape2)
  tmp <- dcast(df, group ~ x, value.var="y")
  ord <- order(tmp[,2])
  tmp <- tmp[ord,]
  
  min.space <- min.space*diff(range(tmp[,-1]))
  yshift <- numeric(nrow(tmp))
  ## Start at "bottom" row
  ## Repeat for rest of the rows until you hit the top
  for (i in 2:nrow(tmp)) {
    ## Shift subsequent row up by equal space so gap between
    ## two entries is >= minimum
    mat <- as.matrix(tmp[(i-1):i, -1])
    d.min <- min(diff(mat))
    yshift[i] <- ifelse(d.min < min.space, min.space - d.min, 0)
  }
  
  
  tmp <- cbind(tmp, yshift=cumsum(yshift))
  
  scale <- 1
  tmp <- melt(tmp, id=c("group", "yshift"), variable.name="x", value.name="y")
  ## Store these gaps in a separate variable so that they can be scaled ypos = a*yshift + y
  
  tmp <- transform(tmp, ypos=y + scale*yshift)
  return(tmp)
  
}



plot_slopegraph <- function(df) {
  
  ylabs <- subset(df, x==head(x,1))$group
  yvals <- subset(df, x==head(x,1))$ypos
  fontSize <- 3
  gg <- ggplot(df,aes(x=x,y=ypos)) +
    geom_line(aes(group=group,colour=df$group),show.legend = FALSE) + # geom_line(aes(group=group),colour="grey80") +
    geom_point(aes(colour = df$group),size=8,show.legend = FALSE) + # geom_point(colour="white",size=8) +
    geom_text(aes(label=y), size=fontSize, family="American Typewriter") +
    scale_y_continuous(name="", breaks=yvals, labels=ylabs)
  
  gg <- gg + theme_economist_white() #+ scale_colour_economist()
  gg <- gg + theme(panel.background = element_rect(fill = "lightgrey"))
  return(gg)
}    


df2 <- tufte_sort(data2plot, 
                  x="time", 
                  y="values", 
                  group="geo", 
                  method="tufte", 
                  min.space=0.05)
df2 <- transform(df2, 
                 x=factor(x, levels=c(2005,2008,2011,2014,2017), 
                          labels=c("2005","2008","2011","2014","2017")), 
                 y=round(y))

gg1 <- plot_slopegraph(df2) + labs(title="Changing the GDP") +
  labs(subtitle = "source: EUROSTAT (sdg_08_10)") +
  theme(axis.title=element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5,
                                  family = "American Typewriter",
                                  face="bold"),
        axis.text = element_text(family = "American Typewriter",
                                 face="bold"))
gg1


gg2 <- ggplot(spread_data2plot, aes(x=`geo`, y=REF, label=REF)) + 
  geom_bar(stat='identity', aes(fill=IND), width=.5)  +
  scale_fill_manual(name="", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="source: EUROSTAT (sdg_08_10)\nChange of GDP between 2005 and 2017 in EU", 
       title= "Difference from average increase of GDP") + 
  # geom_text() +
  coord_flip()


gg2 <- gg2 + theme_economist_white() + scale_colour_economist() +labs(x=NULL, y=NULL)

gg2

spread_data2plot <- spread_data2plot[order(spread_data2plot$REF),]

p <- plot_ly(spread_data2plot, x = spread_data2plot$`2005`, y = spread_data2plot$geo, 
             name = "GDP in 2005", type = "scatter", mode = "markers",
             marker = list(color = "green")) %>% 
  add_trace(x = spread_data2plot$`2017`, y = ~spread_data2plot$geo, name = "GDP in 2017",type = 'scatter',
            mode = "markers", marker = list(color = "blue")) %>% 
  add_trace(x = spread_data2plot$`2008`, y = ~spread_data2plot$geo, name = "GDP in 2008",type = 'scatter',
            mode = "markers", marker = list(color = "orange")) %>% 
  add_trace(x = spread_data2plot$`2011`, y = ~spread_data2plot$geo, name = "GDP in 2011",type = 'scatter',
            mode = "markers", marker = list(color = "black")) %>% 
  add_trace(x = spread_data2plot$`2014`, y = ~spread_data2plot$geo, name = "GDP in 2014",type = 'scatter',
            mode = "markers", marker = list(color = "yellow")) %>% 
  
  layout(
    title = "Change GDP in EU",
    xaxis = list(title = "GDP in EUR"),
    yaxis = list(title = ""),
    margin = list(l = 100)
  ) 

p

# options(browser = 'false')
# api_create(p, filename = "change-gdp-in-eu")




newggslopegraph(data2plot,time,values,geo,
                Title = "Changing GDP in EU",
                SubTitle = "data source: Eurostat",
                Caption = "")

# 
# DrawMaps.R
# 
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

# options(browser = 'false')
# api_create(p_map, filename = "change-gdp-in-eu-map")
