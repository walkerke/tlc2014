library(rCharts)
library(reshape2)

cproj <- read.csv("http://www.census.gov/population/projections/files/summary/NP2012-T6.csv")

keep <- c(20:25, 32)

sub <- cproj[keep,]

names.new <- c("race.ethnicity", "y2015", "y2020", "y2025", "y2030", "y2035",
               "y2040", "y2045", "y2050", "y2055", "y2060")

names(sub) <- names.new

sub.long <- melt(sub, 
                 id.vars = "race.ethnicity", 
                 variable.name = "year", 
                 value.name = "percent")

sub.long$year <- gsub("y", "", sub.long$year)
sub.long$year <- as.numeric(sub.long$year)
sub.long$race.ethnicity <- gsub("\\.", "", sub.long$race.ethnicity)
sub.long$race.ethnicity <- gsub("HISPANIC", "Hispanic", sub.long$race.ethnicity)
sub.long$percent <- as.numeric(sub.long$percent)

keep.years <- seq(2015, 2055, 10)

sub.years <- sub.long[sub.long$year %in% keep.years, ]

## Build charts

chart1 <- nPlot(
  percent ~ year, 
  group = "race.ethnicity", 
  data = sub.years, 
  type = "multiBarChart")

# Format chart options

chart1$yAxis(axisLabel = "Percent of total", width = 62)
chart1$xAxis(axisLabel = "Year")

chart1$chart(tooltipContent = "#! function(key, x, y){
        return '<h3>' + key + '</h3>' + 
        '<p>' + y + '% in ' + x + '</p>'
        } !#")

# Chart 2: designed to mirror the Census Bureau's chart

sub.census <- sub.long[(sub.long$year == 2015 | sub.long$year == 2060), ]

chart2 <- nPlot(
  percent ~ race.ethnicity, 
  group = "year", 
  data = sub.census, 
  type = "multiBarChart")

chart2$yAxis(axisLabel = "Percent of total", width = 62)
chart2$xAxis(axisLabel = "Race or ethnicity")
chart2$chart(color = c("navy", "red"))

chart2$chart(tooltipContent = "#! function(key, x, y){
        return '<h3>' + key + '</h3>' + 
        '<p>' + x + ': ' + y + '%' + '</p>'
        } !#")

# Publish charts to GitHub

# chart1$publish("Race & ethnicity projections for the U.S.", host = "gist")

# chart2$publish("Race & ethnicity, 2015 & 2060 compared", host = "gist")