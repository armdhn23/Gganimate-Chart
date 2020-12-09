library(tidyverse)
library(janitor)
library(png)
library(ggplot2)
library(gganimate)

data <- read.csv('data/saintek.csv')
gdp_formatted <- data %>%
   group_by(Tahun) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-Pendaftar)) %>%
  group_by(Prodi) %>% 
  filter(rank <=10) %>%
  ungroup()

staticplot = ggplot(gdp_formatted, aes(rank, group = Prodi, 
                                       fill = as.factor(Prodi), color = as.factor(Prodi))) +
  geom_tile(aes(y = Pendaftar/2,
                height = Pendaftar,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Prodi, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=Pendaftar,label = Pendaftar, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))
staticplot
anim = staticplot + transition_states(Tahun, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  
anim

animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif"))

