


library(fmsb)
###### For agility

###### For agility run combo

endurance <- 59

agility <- 49

power <- 23.5

fit <- 60

data <- t(c(endurance, agility, power, fit))

data <- as.data.frame(data)

improvThresh<- as.data.frame(t(rep(70, 4)))
equalThresh<- as.data.frame(t(rep(50, 4)))

min =as.data.frame(t(rep(0, 4)))
max = as.data.frame(t(rep(100,4)))
data <- rbind(max, min, improvThresh, equalThresh, data)

colnames(data) <- c("Endurance", "Agility", "Power", "Fit")

colors <- c("#C8C9C7","#53565A", "#00966C")

create_beautiful_radarchart <- function(data, color = "#00966C", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}


create_beautiful_radarchart(data = data, color = colors)

legend(x = "topright", inset = c(- 0.1, 0), legend = c("threshold for confidence in improvement", "V1 Performance", "V2 Performance"),
       bty = "n", pch = 20, col = colors, text.col = "black", cex = .7, pt.cex = 1)

## This code gives a visual representation of how a shoe performed against a baseline shoe in the form of a radar plot 
#This happens by assigning different averaged ratings to different segments 
#You are only adding the shoe being tested to the ratings, the baseline shoe is always set to 50 for a clear comparison
#The shoe being tested is rated by the average percentile confidence in each segment 
###For example, if a PFS shoe had 75% confidence in CMJ  and 50% in skater for contact time, the average rating for agility would be 62.5 -> 63 





###### For agility run combo

#Defining the ratings per segment
endurance <- 66
  
health <- 45
  
agility <- 61

power <- 44
  
fit <- 60


#Defining the radar plot's segments into a data frame
data <- t(c(endurance, health, agility, power, fit))

data <- as.data.frame(data)
 

###
improvThresh<- as.data.frame(t(rep(70, 5)))
equalThresh<- as.data.frame(t(rep(50, 5)))

#Mins and maxs of the chart 
#Including these with the thresholds
min =as.data.frame(t(rep(0, 5)))
max = as.data.frame(t(rep(100,5)))
data <- rbind(max, min, improvThresh, equalThresh, data)


#Defining the names on the chart and assigning the correct color
colnames(data) <- c("Endurance", "Health", "Agility", "Power", "Fit")

colors <- c("#C8C9C7","#53565A", "#00966C")

create_beautiful_radarchart <- function(data, color = "#00966C", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){ 
  # Scaling the chart and defining it's shape
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

#Radar chart output 
#Replace the labels with the baseline first for the chart key
create_beautiful_radarchart(data = data, color = colors) 

legend(x = "topright", inset = c(- 0.1, 0), legend = c("threshold for confidence in improvement", "Tri Panel Performance", "Dual Panel Performance"),
       bty = "n", pch = 20, col = colors, text.col = "black", cex = .7, pt.cex = 1)



############################## For Golf ##########################################
<<<<<<< Updated upstream:PerformanceTest_AllActivities_RaderPlots.R
#Defining the ratings per segment
power <- 16
=======

power <- 95
>>>>>>> Stashed changes:RaderPlots.R

precision <- 49

fit <- 25

#Defining the radar plot's segments into a data frame
data <- t(c(power, precision, fit))



data <- as.data.frame(data)

improvThresh<- as.data.frame(t(rep(70, 3)))
equalThresh<- as.data.frame(t(rep(50,3))) 


#Mins and maxs of the chart 
#Including these with the thresholds
min =as.data.frame(t(rep(0, 3)))
max = as.data.frame(t(rep(100,3)))
data <- rbind(max, min, improvThresh, equalThresh, data) 


<<<<<<< Updated upstream:PerformanceTest_AllActivities_RaderPlots.R
#Defining the names on the chart and assigning the correct color
colnames(data) <- c("Power: Drive Distance", "Precision: Drive consistency and Accuracy", "Fit: Overall Qualitative Score")
=======
colnames(data) <- c("Power", "Precision", "Fit")
>>>>>>> Stashed changes:RaderPlots.R

colors <- c("#C8C9C7","#53565A", "#00966C")

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){ 
  # Scaling the chart and defining it's shape
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

#Radar chart output  
#Replace the labels with the baseline first for the chart key
create_beautiful_radarchart(data = data, color = colors) 

legend(x = "topright", inset = c(0, 0), legend = c("threshold for confidence in improvement", "Monopanel Performance", "Overlapping Panels Performance"),
       bty = "n", pch = 20, col = colors, text.col = "black", cex = .7, pt.cex = 1)


############################## For Golf/Endurance Combo ##########################################
#Defining the ratings per segment
endurance <- 37
  
health <- 29
  
power <- 54

precision <- 66.5

fit <- 72

#Defining the radar plot's segments into a data frame
data <- t(c(endurance, health, power, precision, fit))



data <- as.data.frame(data)

improvThresh<- as.data.frame(t(rep(70, 5)))
equalThresh<- as.data.frame(t(rep(50,5))) 

#Mins and maxs of the chart 
#Including these with the thresholds
min =as.data.frame(t(rep(0, 5)))
max = as.data.frame(t(rep(100,5)))
data <- rbind(max, min, improvThresh, equalThresh, data)


#Defining the names on the chart and assigning the correct color
colnames(data) <- c("Endurance", "Health", "Power", "Precision", "Fit")

colors <- c("#C8C9C7","#53565A", "#00966C")

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){ 
  
  # Scaling the chart and defining it's shape
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

#Radar chart output  
#Replace the labels with the baseline first for the chart key
create_beautiful_radarchart(data = data, color = colors)

legend(x = "topright", inset = c(0, 0), legend = c("threshold for confidence in improvement", "Lace Performance", "BOA Performance"),
       bty = "n", pch = 20, col = colors, text.col = "black", cex = .7, pt.cex = 1)

