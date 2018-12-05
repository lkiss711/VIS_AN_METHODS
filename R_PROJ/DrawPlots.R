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

newggslopegraph(data2plot,time,values,geo,
                Title = "Changing GDP in EU",
                SubTitle = "data source: Eurostat",
                Caption = "")

