library(fmsb)

endurance <- 22
  
impact <- 37
  
stability <- 91
  
fit <- 100

data <- t(c(endurance, impact, stability, fit))

data <- as.data.frame(data)

improvThresh<- as.data.frame(t(rep(70, 4)))
equalThresh<- as.data.frame(t(rep(50,4)))

min =as.data.frame(t(rep(0, 4)))
max = as.data.frame(t(rep(100,4)))
data <- rbind(max, min, improvThresh, equalThresh, data)

colnames(data) <- c("Endurance", "Health: Impact", "Health: Stability", "Qualitative Fit")

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

legend(x = "topright", inset = c(- 0.1, 0), legend = c("threshold for confidence in improvement", "Nothing Performance", "BOA Performance"),
       bty = "n", pch = 20, col = colors, text.col = "black", cex = .7, pt.cex = 1)



############################## For Golf ##########################################

power <- 16

precision <- 43

fit <- 83


data <- t(c(power, precision, fit))



data <- as.data.frame(data)

improvThresh<- as.data.frame(t(rep(70, 3)))
equalThresh<- as.data.frame(t(rep(50,3)))

min =as.data.frame(t(rep(0, 3)))
max = as.data.frame(t(rep(100,3)))
data <- rbind(max, min, improvThresh, equalThresh, data)

colnames(data) <- c("Power: Drive Distance", "Precision: Drive consistency and Accuracy", "Fit: Overall Qualitative Score")

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

legend(x = "topright", inset = c(0, 0), legend = c("threshold for confidence in improvement", "Lace Performance", "Heel Config A Performance"),
       bty = "n", pch = 20, col = colors, text.col = "black", cex = .7, pt.cex = 1)

