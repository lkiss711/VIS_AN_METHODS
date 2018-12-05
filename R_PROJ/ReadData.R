library("eurostat")
library("tidyr")
library("dplyr")
library("reshape2")
library("imputeTS")

id <-  "sdg_08_10"

data <- get_eurostat(id, time_format = "date")

data$time = substring(data$time,0,4)

# validcountries <- c("BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE","UK")
validcountries <- c("CY","CZ","EE","HU","LV","LT","MT","PL","SI","SK")
validyears <- c("2005","2008","2011","2014","2017")

data2plot <- dplyr::filter(data, (grepl(paste(validyears,collapse = '|'),time) & grepl(paste(validcountries,collapse = '|'),geo) & unit == "CLV10_EUR_HAB"))
data2plot <- data2plot[,3:5]

data2map <- dplyr::filter(data, (grepl(paste(validyears,collapse = '|'),time) & grepl(paste(validcountries,collapse = '|'),geo) & unit == "CLV10_EUR_HAB"))
data2map$time <- substring(data2map$time,1,4)
data2map$country_code <- data2map$geo
data2map[,3] <- label_eurostat(data2map[,3], fix_duplicated = TRUE)



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
