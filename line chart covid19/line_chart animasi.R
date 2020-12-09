#buat plot statis
library(ggplot2)
library(gganimate)
library(dplyr)
library(ggthemes)
#render
library(gifski)
library(av)

cases1 <- read.csv("data/data_covid.csv")
cases1$date <- as.Date(cases1$date)
cases1$kategori <- factor(cases1$kategori, levels=c("total", "kasus_aktif", "meninggal", "pulih"))
str(cases1)

plot_statis <- cases1 %>%
  ggplot( aes(x=date, y=Data, group= kategori, color= kategori)) +
  geom_line() +
  scale_color_viridis_d() +
  geom_point(aes(group = seq_along(kategori))) +
  ggtitle("KASUS COVID-19 DI INDONESIA 
          02 MARET 2020 - 15 APRIL 2020") +
  labs(x = "Tanggal", y = "Jumlah")
plot_statis
  
#menambahkan theme
plot_statis<- plot_statis + theme_solarized(light = FALSE)
plot_statis

plot_dinamis <- plot_statis + transition_reveal(date)

#ekspor ke video
animate(plot_dinamis, renderer = av_renderer('~/Documents/hasil/animation.mp4'),
              width = 700, height = 500, res = 104, fps = 25, duration = 10)

#Ekspor ke animasi
animate(plot_dinamis, renderer = gifski_renderer("~/Documents/hasil/covid19(1).gif"))

