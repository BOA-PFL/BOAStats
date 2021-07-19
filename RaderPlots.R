library(fmsb)

agility <- 72
  
power <- 68
  
fit <- 85
  
endurance <- 57

data <- t(c(agility, power, fit, endurance))



data <- as.data.frame(data)

thresh<- as.data.frame(t(rep(60, 4)))
min =as.data.frame(t(rep(0, 4)))
max = as.data.frame(t(rep(100,4)))
data <- rbind(max, min, thresh, data)

colnames(data) <- c("Agility: Contact Time", "Power: Propulsive Force", "Fit: Overall Qualitative Score", "Endurance: Less wasted force")

colors <- c("#53565A", "#DC582A")

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

legend(x = "bottomleft", legend = c("threshold for improvement", "test shoe scores relative to baseline"),
       bty = "n", pch = 20, col = colors, text.col = "black", cex = 1, pt.cex = 1.5)


