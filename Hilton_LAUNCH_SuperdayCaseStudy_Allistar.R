# Hilton - LAUNCH Program - Analytics (Previous Rotational Analyst)
# Superday Take Home Case Study
# Oct 2023
# Ximeng Deng

#install.packages("readxl")
library(readxl)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(patchwork)

################################################################################

# get the data and clean the data
Allistar <- read_excel("/Users/ximengdeng/Desktop/resume/Hilton/case/LAUNCH_Case Study Data_Allistar.xlsm")
str(Allistar)
#View(Allistar)

Allistar <- Allistar %>%
  mutate(
    Year = substr(`Stay Year_Month`, 1, 4),  
    Month = substr(`Stay Year_Month`, 5, 6), 
    `Stay Year_Month` = paste0(Year, "-", Month)
  ) %>%
  select(-Year, -Month)

Allistar$`Average Daily Revenue` <- Allistar$`Total Room Revenue` / Allistar$`Total Room Nights`


################################################################################

# plot by property format 1

plot1 <- ggplot(Allistar, aes(x = `Stay Year_Month`, y = `Total Room Revenue`, group = `Property Code`, color = `Property Code`)) +
  geom_line() +
  facet_grid(. ~ `Brand Group`) +
  labs(x = "Stay Year_Month", y = "Total Room Revenue") +
  scale_color_discrete(name = "Property Code") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot2 <- ggplot(Allistar, aes(x = `Stay Year_Month`, y = `Total Room Nights`, group = `Property Code`, color = `Property Code`)) +
  geom_line() +
  facet_grid(. ~ `Brand Group`) +
  labs(x = "Stay Year_Month", y = "Total Room Nights") +
  scale_color_discrete(name = "Property Code") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot3 <- ggplot(Allistar, aes(x = `Stay Year_Month`, y = `Avg. # Guests in Room`, group = `Property Code`, color = `Property Code`)) +
  geom_line() +
  facet_grid(. ~ `Brand Group`) +
  labs(x = "Stay Year_Month", y = "Avg. # Guests in Room") +
  scale_color_discrete(name = "Property Code") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot4 <- ggplot(Allistar, aes(x = `Stay Year_Month`, y = `Average Daily Revenue`, group = `Property Code`, color = `Property Code`)) +
  geom_line() +
  facet_grid(. ~ `Brand Group`) +
  labs(x = "Stay Year_Month", y = "Average Daily Revenue") +
  scale_color_discrete(name = "Property Code") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_plot <- grid.arrange(plot1, plot2, plot3, plot4, ncol = 1)
print(combined_plot)


################################################################################

# plot by property format 2

property_code_colors <- c(
  "NPTN" = "#85bedd",
  "PLTO" = "#133c3e",
  "STRN" = "#e29f33",
  "SUNN" = "#e990a5",
  "URNS" = "#718e4b",
  "MOON" = "#85bedd",
  "NEBL" = "#133c3e",
  "NOVA" = "#e29f33",
  "PLRS" = "#e990a5",
  "SRUS" = "#718e4b",
  "EART" = "#85bedd",
  "JPTR" = "#133c3e",
  "MARS" = "#e29f33",
  "MRCY" = "#e990a5",
  "VENS" = "#718e4b"
)

plot1 <- ggplot(Allistar, aes(x = `Stay Year_Month`, y = `Average Daily Revenue`, group = `Property Code`, color = `Property Code`)) +
  geom_line(size = 1.3, alpha = 0.8) + 
  facet_grid(. ~ `Brand Group`) +
  labs(x = "Stay Year_Month", y = "Average Daily Revenue") +
  scale_color_manual(name = "Property Code", values = property_code_colors) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot2 <- ggplot(Allistar, aes(x = `Stay Year_Month`, y = `Total Room Nights`, group = `Property Code`, color = `Property Code`)) +
  geom_line(size = 1.3, alpha = 0.8) + 
  facet_grid(. ~ `Brand Group`) +
  labs(x = "Stay Year_Month", y = "Total Room Nights") +
  scale_color_manual(name = "Property Code", values = property_code_colors) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot3 <- ggplot(Allistar, aes(x = `Stay Year_Month`, y = `Avg. # Guests in Room`, group = `Property Code`, color = `Property Code`)) +
  geom_line(size = 1.3, alpha = 0.8) + 
  facet_grid(. ~ `Brand Group`) +
  labs(x = "Stay Year_Month", y = "Avg. # Guests in Room") +
  scale_color_manual(name = "Property Code", values = property_code_colors) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot4 <- ggplot(Allistar, aes(x = `Stay Year_Month`, y = `Total Room Revenue`, group = `Property Code`, color = `Property Code`)) +
  geom_line(size = 1.3, alpha = 0.8) + 
  facet_grid(. ~ `Brand Group`) +
  labs(x = "Stay Year_Month", y = "Total Room Revenue") +
  scale_color_manual(name = "Property Code", values = property_code_colors) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_plot <- plot1 + plot2 + plot3 + plot4
print(combined_plot)

################################################################################

# plot by brand group
Allistar_summary1 <- Allistar %>%
  group_by(`Brand Group`, `Stay Year_Month`) %>%
  summarize(Avg_Avg_Daily_Revenue = mean(`Average Daily Revenue`))

Allistar_summary2 <- Allistar %>%
  group_by(`Brand Group`, `Stay Year_Month`) %>%
  summarize(Avg_Total_Room_Nights = mean(`Total Room Nights`))

Allistar_summary3 <- Allistar %>%
  group_by(`Brand Group`, `Stay Year_Month`) %>%
  summarize(Avg_Guests = mean(`Avg. # Guests in Room`))

Allistar_summary4 <- Allistar %>%
  group_by(`Brand Group`, `Stay Year_Month`) %>%
  summarize(Avg_Total_Room_Revenue = mean(`Total Room Revenue`))


brand_group_colors <- c("Astra" = "#46347f", 
                        "Grand Galactic" = "#1ea187", 
                        "StarSuites" = "#fde824")


plot1 <- ggplot(Allistar_summary1, aes(x = `Stay Year_Month`, y = Avg_Avg_Daily_Revenue, color = `Brand Group`, group = `Brand Group`)) +
  geom_line(size = 1.5) +
  labs(x = "Stay Year_Month", y = "Average Daily Revenue") +
  scale_color_manual(name = "Brand Group", values = brand_group_colors) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot2 <- ggplot(Allistar_summary2, aes(x = `Stay Year_Month`, y = Avg_Total_Room_Nights, color = `Brand Group`, group = `Brand Group`)) +
  geom_line(size = 1.5) +
  labs(x = "Stay Year_Month", y = "Average Total Room Nights") +
  scale_color_manual(name = "Brand Group", values = brand_group_colors) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot3 <- ggplot(Allistar_summary3, aes(x = `Stay Year_Month`, y = Avg_Guests, color = `Brand Group`, group = `Brand Group`)) +
  geom_line(size = 1.5) +
  labs(x = "Stay Year_Month", y = "Average Guests") +
  scale_color_manual(name = "Brand Group", values = brand_group_colors) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot4 <- ggplot(Allistar_summary4, aes(x = `Stay Year_Month`, y = Avg_Total_Room_Revenue, color = `Brand Group`, group = `Brand Group`)) +
  geom_line(size = 1.5) +
  labs(x = "Stay Year_Month", y = "Average Total Room Revenue") +
  scale_color_manual(name = "Brand Group", values = brand_group_colors) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

################################################################################

print(Allistar_summary1,n=60)
print(Allistar_summary2,n=60)
print(Allistar_summary3,n=60)
print(Allistar_summary4,n=60)
