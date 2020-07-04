# ------------------------------------------------------------------------------
# Graphs & charts for the post "Pie charts and other data visualization sins"
# Post URL: https://miguel-gomez.github.io/blog/2020/05/31/Pie-charts-1
# Repo: https://github.com/miguel-gomez/BlogMaterials
# Author: Miguel Gomez
# Contact: miguel.gomez.contact@gmail.com
# ------------------------------------------------------------------------------

library(ggplot2)

# NATO data
countries <- c("USA", "UK", "Germany")
expenditure <- c(1631, 816, 518)
nato <- data.frame(countries, expenditure)

# Chart 1 - NATO barplot
ggplot(data = nato, aes(x = countries, y = expenditure )) +
  geom_bar(stat = "identity", fill = "violetred", width = 0.8) + 
  labs(title = "Per capita military expenditure",
       subtitle = "selected NATO countries, as of 2020, in euros.", 
       caption = "Data Source: NATO.", 
       x = "Country", 
       y = "Military expenditure")



# Cloud data
providers <- as.factor(c("AWS", "Azure", "Google", "Alibaba", "Others"))
revenue_2019 <- c(34.6, 18.1, 6.2, 5.2, 43.0)
cloud <- data.frame(providers, revenue_2019)

# Chart 2 - barplot
ggplot(data = cloud, mapping = aes(x = reorder(providers, revenue_2019), y = revenue_2019, fill = providers)) +
  geom_bar(stat = "identity", width = 0.8) + 
  labs(title = "Worldwide cloud infrastructure revenue by provider",
       subtitle = "Estimate for 2019, in USD billion.", 
       caption = "Data Source: Canalys.", 
       x = "Cloud Provider", 
       y = "Revenue") +
  theme(legend.position = "none")
  


# Chart 3 - cleveland
ggplot(data = cloud, mapping = aes(x = reorder(providers, -revenue_2019), # maintains ascending order when coord_flip() 
                                   y = revenue_2019,
                                   color = providers)) +
  geom_point(size = 3) + 
  labs(title = "Worldwide cloud infrastructure revenue by provider",
       subtitle = "Estimate for 2019, in USD billion.", 
       caption = "Data Source: Canalys.", 
       x = "Cloud Provider", 
       y = "Revenue") +
  theme(legend.position = "none") +
  coord_flip() + 
  ylim(2, 45) # forces 0 to be shown on the x axis 



# Chart 4 - waffle
library(dplyr)
library(scales)
library(waffle)

# sort by descending order, calculate and round percentages
cloud_percentages <- cloud %>%
  arrange(revenue_2019) %>%
  transmute(providers, percentage = revenue_2019 * 100 / sum(revenue_2019)) %>%
  mutate(percentage = round(percentage, digits = 0))

# cloud percentage data as a named vector
waffle_vector <- cloud_percentages$percentage
names(waffle_vector) <- cloud_percentages$providers

# default ggplot2 colors, arranged to keep color-coding consistent 
waffle_colors <- hue_pal()(5)
waffle_colors <- waffle_colors[c(1, 4, 3, 2, 5)]

# waffle plot
waffle(parts = waffle_vector,
       rows = 10,
       colors = waffle_colors,
       title = "Cloud infrastructure revenue by provider" ,
       xlab = "Worldwide estimate for 2019, in USD billion.") +
  theme(plot.title = element_text(size = 14))



# Chart 5 - pie
ggplot(data = cloud, mapping = aes(x = "", y = reorder(revenue_2019, revenue_2019), fill = providers)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y", start = 0) + 
  theme_void() +
  labs(title = "Worldwide cloud infrastructure revenue by provider",
       subtitle = "Estimate for 2019, in USD billion.", 
       caption = "Data Source: Canalys.") 



# Chart 6 - Data preparation 

# trimmed barplot: No x lab, y lab, subtitle, caption, x axis text, x axis ticks.
cloud_barplot <- ggplot(data = cloud, mapping = aes(x = reorder(providers, revenue_2019), y = revenue_2019, fill = providers)) +
  geom_bar(stat = "identity", width = 0.8) + 
  labs(title = "Barplot",
       x = NULL,
       y = NULL) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# trimmed cleveland: No x lab, y lab, subtitle, caption, y axis text, y axis ticks.
cloud_cleveland <- ggplot(data = cloud, mapping = aes(x = reorder(providers, -revenue_2019), # maintains ascending order when coord_flip() 
                                                      y = revenue_2019,
                                                      color = providers)) +
  geom_point(size = 3) + 
  labs(title = "Cleveland",
       x = NULL, 
       y = NULL) +
  theme(legend.position = "none") +
  coord_flip() + 
  ylim(2, 45) + # forces 0 to be shown on the x axis 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# trimmed waffle: Decrease size to 1, keeps legend.
cloud_waffle <- waffle(parts = waffle_vector,
                       rows = 10,
                       colors = waffle_colors,
                       size = 1,
                       title = "Waffle") +
  theme(plot.title = element_text(size = 14))

# trimmed pie: No legend.
cloud_pie <- ggplot(data = cloud, mapping = aes(x = "", y = reorder(revenue_2019, revenue_2019), fill = providers)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y", start = 0) + 
  theme_void() +
  labs(title = "Pie") +
  theme(legend.position = "none")



# Chart 6 - Patchwork
library(patchwork)

cloud_barplot + cloud_cleveland + cloud_waffle + cloud_pie +
  plot_annotation(title = "Chart Comparison",
                  subtitle = "Worldwide cloud infrastructure revenue by provider.",
                  caption = "Data for 2019, in USD billion. Source: Canalys") +
  plot_layout(guides = 'collect') # will align our only remaining legend with the whole patchwork
