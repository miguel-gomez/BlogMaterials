# ------------------------------------------------------------------------------
# Graphs & charts for the post "Pie charts and hemicycle charts"
# Post URL: https://miguel-gomez.github.io/blog/2020/06/30/Pie-charts-2
# Repo: https://github.com/miguel-gomez/BlogMaterials
# Author: Miguel Gomez
# Contact: miguel.gomez.contact@gmail.com
# ------------------------------------------------------------------------------

library(ggplot2)
library(scales) # rescale()
library(ggforce) # geom_arc_bar()


# Chart 1 - 2019 Madrid City Council Election

# initial data
Party <- as.factor(c("C´s", "Mas Madrid", "PP", "PSOE", "VOX"))
Seats <- c(11, 19, 15, 8, 4)
data <- data.frame(Party, Seats)

# data wrangling: calculate seat proportion and assign to a specific interval
data$proportion <- data$Seats / sum(data$Seats)
data$max <- cumsum(data$proportion)
data$min <- c(0, head(data$max, -1)) # starts with 0 and removes last element of max

# data rescale to hemicycle
data$max <- rescale(data$max, to = pi * c(-.5, .5), from = 0:1)
data$min <- rescale(data$min, to = pi * c(-.5, .5), from = 0:1)

# plot
chart1 <- ggplot(data) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.30, r = 1.0, start = min, end = max, fill = Party)) +
  coord_fixed() +
  theme_void() 

# annotation
chart1 + labs(title = "2019 Madrid City Council Election",
              subtitle = "PP candidate was elected mayor with C´s and VOX support",
              caption = "Source: Wikipedia")
  


# Chart 2 - 2011 Madrid Regional Election

# initial data
Party <- factor(c("IU", "PP", "PSOE", "UPyD"))
Seats <- c(13, 72, 36, 8)
data <- data.frame(Party, Seats)

# data wrangling: ascending order, calculate seat proportion and assign to a specific interval
data <- data[order(Seats),]
data$proportion <- data$Seats / sum(data$Seats)
data$max <- cumsum(data$proportion)
data$min <- c(0, head(data$max, -1)) # starts with 0 and removes last element of max

# data rescale to hemicycle
data$max <- rescale(data$max, to = pi * c(-.5, .5), from = 0:1)
data$min <- rescale(data$min, to = pi * c(-.5, .5), from = 0:1)

# plot
chart2 <- ggplot(data) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.30, r = 1.0, start = min, end = max, fill = Party)) +
  coord_fixed() +
  theme_void() 

# annotation
chart2 + labs(title = "2011 Madrid Regional Election",
              subtitle = "PP obtained a qualified majority.",
              caption = "Source: Wikipedia")



# Chart 3 - 2018 Andalucia Regional Election

# initial data: level order ensures correct color assignment
Party <- factor(c("AA", "PSOE", "C´s", "PP", "VOX"), levels = c("AA", "PSOE", "C´s", "PP", "VOX"))
Seats <- c(17, 33, 21, 26, 12)
Party_color <- c("palegreen", "red", "darkorange", "blue", "green")
data <- data.frame(Party, Seats, Party_color)

# data wrangling: calculate seat proportion and assign to a specific interval
data$proportion <- data$Seats / sum(data$Seats)
data$max <- cumsum(data$proportion)
data$min <- c(0, head(data$max, -1)) # starts with 0 and removes last element of max

# data rescale to hemicycle
data$max <- rescale(data$max, to = pi * c(-.5, .5), from = 0:1)
data$min <- rescale(data$min, to = pi * c(-.5, .5), from = 0:1)

# plot with custom colors
chart3 <- ggplot(data) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.30, r = 1.0, start = min, end = max, fill = Party)) +
  coord_fixed() +
  theme_void() +
  scale_fill_manual(values = Party_color)

# annotation
chart3 + labs(title = "2018 Andalucia Regional Election",
              subtitle = "PP, C´s and VOX formed a coalition government",
              caption = "Source: Wikipedia")



# Chart 4 - 2018 Bavarian State Election 

# initial data: level order ensures correct color assignment
Party <- factor(c("SPD", "GRÜNE", "FDP", "FW", "CSU", "AfD"), levels = c("SPD", "GRÜNE", "FDP", "FW", "CSU", "AfD"))
Seats <- c(22, 38, 11, 27, 85, 22)
Party_color <- c("red", "green", "yellow", "darkcyan", "blue", "cyan")
data <- data.frame(Party, Seats, Party_color)

# data wrangling: calculate seat proportion and assign to a specific interval
data$proportion <- data$Seats / sum(data$Seats)
data$max <- cumsum(data$proportion)
data$min <- c(0, head(data$max, -1)) # starts with 0 and removes last element of max
data$lab_pos <- (data$max - data$min) / 2 + data$min # label positions at the middle of each interval

# data rescale to hemicycle
data$max <- rescale(data$max, to = pi * c(-.5, .5), from = 0:1)
data$min <- rescale(data$min, to = pi * c(-.5, .5), from = 0:1)
data$lab_pos <- rescale(data$lab_pos, to = pi * c(-.5, .5), from = 0:1)

# plot with custom colors, extended radius
chart4 <- ggplot(data) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.45, r = 1.5, start = min, end = max, fill = Party)) +
  coord_fixed() +
  theme_void() +
  scale_fill_manual(values = Party_color)

# annotation
chart4 + 
  labs(title = "2018 Bavarian State Election",
       subtitle = "CSU and FW formed a coalition government",
       caption = "Source: Wikipedia") +
  geom_text(aes(x = sin(lab_pos), y = cos(lab_pos), label = Seats), color = "black", size = 5) + 
  geom_text(aes(x = 0, y = 0.16, label = "Total: \n 205 Seats"), color = "black", size = 4.5)



# Chart 5 - 2013 German Federal Election  

# initial data: level order ensures correct color assignment
Party <- factor(c("DIE LINKE", "SPD", "GRÜNE", "CDU", "CSU"), levels = c("DIE LINKE", "SPD", "GRÜNE", "CDU", "CSU"))
Seats <- c(64, 193, 63, 255, 56)
Party_color <- c("purple", "red", "green", "black", "blue")
data <- data.frame(Party, Seats, Party_color)

# data wrangling: calculate seat proportion and assign to a specific interval
data$proportion <- data$Seats / sum(data$Seats)
data$max <- cumsum(data$proportion)
data$min <- c(0, head(data$max, -1)) # starts with 0 and removes last element of max

# add number of seats to party names
data$Party <- paste(data$Party, "-", data$Seats, sep = " ") 
data$Party <- factor(data$Party, levels = data$Party)

# data rescale to hemicycle
data$max <- rescale(data$max, to = pi * c(-.5, .5), from = 0:1)
data$min <- rescale(data$min, to = pi * c(-.5, .5), from = 0:1)

# plot with custom colors
chart5 <- ggplot(data) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.45, r = 1.5, start = min, end = max, fill = Party)) +
  coord_fixed() +
  theme_void() +
  scale_fill_manual(values = Party_color)

# annotation
chart5 +
  labs(title = " 2013 German Federal election",
       subtitle = "CDU/CSU and SPD formed a coalition government",
       caption = "Source: Wikipedia") +
  geom_text(aes(x = 0, y = 0.16, label = "631 Seats"), color = "black", size = 4.5)
  
