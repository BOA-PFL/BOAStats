library(fmsb)
<<<<<<< Updated upstream

agility <- 22
  
power <- 52
  
fit <- 50
  
endurance <- 41

data <- t(c(agility, power, fit, endurance))


=======
###### For agility

###### For agility run combo

endurance <- 59

agility <- 49

power <- 23.5

fit <- 60

data <- t(c(endurance, agility, power, fit))
>>>>>>> Stashed changes

data <- as.data.frame(data)

improvThresh<- as.data.frame(t(rep(70, 4)))
<<<<<<< Updated upstream
equalThresh<- as.data.frame(t(rep(50,4)))
=======
equalThresh<- as.data.frame(t(rep(50, 4)))
>>>>>>> Stashed changes

min =as.data.frame(t(rep(0, 4)))
max = as.data.frame(t(rep(100,4)))
data <- rbind(max, min, improvThresh, equalThresh, data)

<<<<<<< Updated upstream
colnames(data) <- c("Agility: Contact Time", "Power: Propulsive Force", "Fit: Overall Qualitative Score", "Endurance: Less wasted force")

colors <- c("#C8C9C7","#53565A", "#DC582A")

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
=======
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

###### For agility run combo

endurance <- 66
  
health <- 45
  
agility <- 61

power <- 44
  
fit <- 60

data <- t(c(endurance, health, agility, power, fit))

data <- as.data.frame(data)

improvThresh<- as.data.frame(t(rep(70, 5)))
equalThresh<- as.data.frame(t(rep(50, 5)))

min =as.data.frame(t(rep(0, 5)))
max = as.data.frame(t(rep(100,5)))
data <- rbind(max, min, improvThresh, equalThresh, data)

colnames(data) <- c("Endurance", "Health", "Agility", "Power", "Fit")

colors <- c("#C8C9C7","#53565A", "#00966C")

create_beautiful_radarchart <- function(data, color = "#00966C", 
>>>>>>> Stashed changes
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

<<<<<<< Updated upstream
legend(x = "topright", inset = c(- 0.1, 0), legend = c("threshold for confidence in improvement", "Tri Performance", "Asym Performance"),
=======
legend(x = "topright", inset = c(- 0.1, 0), legend = c("threshold for confidence in improvement", "Tri Panel Performance", "Dual Panel Performance"),
>>>>>>> Stashed changes
       bty = "n", pch = 20, col = colors, text.col = "black", cex = .7, pt.cex = 1)



############################## For Golf ##########################################

<<<<<<< Updated upstream
power <- 23

precision <- 84

fit <- 49
=======
power <- 95

precision <- 49

fit <- 25
>>>>>>> Stashed changes


data <- t(c(power, precision, fit))



data <- as.data.frame(data)

improvThresh<- as.data.frame(t(rep(70, 3)))
equalThresh<- as.data.frame(t(rep(50,3)))

min =as.data.frame(t(rep(0, 3)))
max = as.data.frame(t(rep(100,3)))
data <- rbind(max, min, improvThresh, equalThresh, data)

<<<<<<< Updated upstream
colnames(data) <- c("Power: Drive Distance", "Precision: Drive consistency and Accuracy", "Fit: Overall Qualitative Score")

colors <- c("#C8C9C7","#53565A", "#DC582A")
=======
colnames(data) <- c("Power", "Precision", "Fit")

colors <- c("#C8C9C7","#53565A", "#00966C")

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
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

legend(x = "topright", inset = c(0, 0), legend = c("threshold for confidence in improvement", "Monopanel Performance", "Overlapping Panels Performance"),
       bty = "n", pch = 20, col = colors, text.col = "black", cex = .7, pt.cex = 1)


############################## For Golf/Endurance Combo ##########################################

endurance <- 37
  
health <- 29
  
power <- 54

precision <- 66.5

fit <- 72


data <- t(c(endurance, health, power, precision, fit))



data <- as.data.frame(data)

improvThresh<- as.data.frame(t(rep(70, 5)))
equalThresh<- as.data.frame(t(rep(50,5)))

min =as.data.frame(t(rep(0, 5)))
max = as.data.frame(t(rep(100,5)))
data <- rbind(max, min, improvThresh, equalThresh, data)

colnames(data) <- c("Endurance", "Health", "Power", "Precision", "Fit")

colors <- c("#C8C9C7","#53565A", "#00966C")
>>>>>>> Stashed changes

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
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

<<<<<<< Updated upstream
legend(x = "topright", inset = c(- 0.1, 0), legend = c("threshold for confidence in improvement", "Lace Performance", "Heel Config A Performance"),
=======
legend(x = "topright", inset = c(0, 0), legend = c("threshold for confidence in improvement", "Lace Performance", "BOA Performance"),
>>>>>>> Stashed changes
       bty = "n", pch = 20, col = colors, text.col = "black", cex = .7, pt.cex = 1)

