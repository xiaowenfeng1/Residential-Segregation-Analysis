library(dplyr)
library(plotly)
setwd('~/Desktop/Class/INFO370/a2-data-wrangling-xiaowenfeng1/')
source('./metric_functions.R')

files <- list.files(path = "./data/prepped", pattern="*.csv", full.names = TRUE)
data.list <- lapply(files, read.csv)
# Found in http://stackoverflow.com/questions/5319839/read-multiple-csv-files-into-separate-data-frames
city.names <- names(data.list) <- gsub("_race.csv","",
                                       list.files("./data/prepped/",full.names = FALSE),
                                       fixed = TRUE)

#get 3 measures for each city
#dissimilarity
diss.index.list <- lapply(data.list, function(x) diss.index(x))
diss.index.list

# interaction 
# inter.index.list <- lapply(data.list, function(x) inter.index(x))
# inter.index.list

#isolation
isolation.index.list <- lapply(data.list, function(x) isolation.index(x))
isolation.index.list

#correlation 
correlation.index.list <- lapply(data.list, function(x) correlation.index(x))
correlation.index.list

dissimilarity<-unlist(diss.index.list)
isolation<-unlist(isolation.index.list)
correlation<-unlist(correlation.index.list)

# diss.bar <- plot_ly(
#   x = dissimilarity,
#   y = city.names,
#   name = "Dissimilarity Index Graph",
#   type = "bar"
# ) %>% layout(title = "Dissimilarity Index Graph",
#              xaxis = list(title = ""), yaxis = list(title = ""), 
#              margin = list(l = 70))

#Overlay historgram of all three indexes
overlay.hist <- plot_ly(alpha = 0.3) %>%
  add_histogram(x = dissimilarity, name = "Dissimilarity") %>%
  add_histogram(x = isolation,name = "Isolation") %>%
  add_histogram(x = correlation,name = "Correlation") %>%
  layout(barmode = "overlay", title="Distribution of Segregation Measures")

# put all three measures into a data frame
df <- data.frame(dissimilarity,isolation,correlation)

# Plot a grouped bar chart that have the 3 indexes for each city
group.bar <- plot_ly(df, x= city.names, y= ~dissimilarity, type = "bar", 
                     name="Dissimilarity", marker = list(color="rgb(55, 128, 191)")) %>%
  add_trace(y = ~isolation, name="Isolation", marker=list(color="rgb(219, 64, 82)")) %>%
  add_trace(y = ~correlation, name="Correlation", marker=list(color="rgb(50, 171, 96)")) %>%
  layout(xaxis = list(title = ""),
         yaxis = list(title = ""),
         margin = list(b = 100),
         barmode = 'group', title="3 Segregation Indexes for Each City")

#plot a scatter plot for isolation and dissimilarity
#dis.iso.df <- data.frame(dissimilarity,isolation)
# scatter.plot <- plot_ly(dis.iso.df, x = ~dissimilarity)%>%
#     add_markers(y = ~isolation, showlegend = FALSE) %>%
#     add_lines(y = ~fitted(loess(isolation ~ dissimilarity)), showlegend = FALSE) %>%
#     layout(xaxis = list(title = 'Dissimilarity Index'),
#             yaxis = list(title = 'Isolation Index'))
# scatter.plot

# get all standard deviation indexes 
stdev <- lapply(data.list, function(x) standard.dev(x))
summary(unlist(stdev))

# plot a bar graph for the standard deviation index
stdev.bar <- plot_ly(x=city.names, y=unlist(stdev), type="bar")%>%
  layout(xaxis = list(title = ""), yaxis = list(title = ""),
         margin = list(b = 70), title="Standard Deviation Index for Each City")

#add standard deviation to the existing data frame
df[["standard.deviation"]] <- unlist(stdev)

#plot a line graph with all four measures, highlighting the standard deviation one
comparison.plot <- plot_ly(df, x = city.names, y = ~dissimilarity, name = 'Dissimilarity', 
                           type = 'scatter', mode = 'lines', line = list(color = 'rgb(128, 128, 128)')) %>%
  add_trace(y = ~isolation, name = 'Isolation', line = list(color = 'rgb(128, 128, 128)')) %>%
  add_trace(y = ~correlation, name = 'Correlation', line = list(color = 'rgb(128, 128, 128)'))%>%
  add_trace(y = ~standard.deviation, name = 'Standard Deviation', 
            line = list(color = 'rgb(255, 90, 90)'))%>%
  layout(title = "Comparison of Four Segregation Measures",
         yaxis = list (title = "Index"), margin = list(b = 100))
