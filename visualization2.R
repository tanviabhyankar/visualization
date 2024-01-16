library(dplyr)
library(openintro)
library(tidyverse)
library(ggplot2) 
ggplot(data = yrbss_samp)+ 
  geom_histogram(
    aes(x=height),
   fill="red"
     ) 
ggplot(data = yrbss_samp)+ 
  geom_density(
    aes(x = weight)
  ) 
ggplot(data = yrbss_samp)+
  geom_bar(
    aes(x = gender),
    fill="lightblue"
  )
ggplot(data = yrbss_samp)+
  geom_bar(
    aes(x = hispanic),
    fill="coral"
  )
ggplot(data = yrbss_samp)+
  geom_point(
    aes(x = height,y = weight ,
    colour= gender)
  ) 
View(yrbss_samp)
yrbss_samp |>
  group_by(gender) |>
  summarise(
  mean_height = mean(height)
   ) |>
  ggplot() +
  geom_col(
  aes(gender, mean_height)
     ) 
yrbss_samp |>
  group_by(gender) |>
  summarise(
    mean_st = mean(strength_training_7d)
  ) |>
  ggplot() +
  geom_point(
    aes(gender, mean_st),
      ) 
yrbss_samp |>
  filter(gender=="male") |>
  group_by(hispanic) |>
  summarise(
    mean_h=mean(height)
  ) |>
  ggplot() +
  geom_col(
    aes(hispanic, mean_h)
  )
ggplot(data = yrbss_samp)+
  geom_point(
    aes(x = height,y = weight ,
        shape= gender)
  ) 
ggplot(data = yrbss_samp)+
  geom_point(
    aes(x = height,y = weight ,
        colour= gender, shape= gender),
    alpha = 0.5
  ) 
ggplot(data = yrbss_samp)+
  geom_point(
    aes(x = height,y = weight ,
        colour= gender, shape= gender),
    alpha = 0.8
  ) 
ggplot(data = yrbss_samp)+
  geom_point(
    aes(x = height,y = weight ,
        size= gender),
    alpha = 0.5
  ) 
library(dplyr)
library(openintro)
library(tidyverse)
library(ggplot2) 
yrbss_samp |>
  mutate(
    stv_category= case_when(
      strength_training_7d==0 ~ "no training",
      strength_training_7d>=1 &
        strength_training_7d<=3 ~"low training",
      strength_training_7d>=3 &
        strength_training_7d<=5 ~ "moderate training",
      strength_training_7d>5 ~ "high training")
    ) |>
  group_by(stv_category) |>
  summarise(
    avg_weight=mean(weight)
    ) |>
  ggplot() +
  geom_col(
    aes(stv_category,avg_weight),
    fill="coral"
  )
yrbss_samp |>
  mutate(
    stv_category= case_when(
      strength_training_7d==0 ~ "no training",
      strength_training_7d>=1 &
        strength_training_7d<=3 ~"low training",
      strength_training_7d>=3 &
        strength_training_7d<=5 ~ "moderate training",
      strength_training_7d>5 ~ "high training")
  ) |>
  group_by(stv_category) |>
  summarise(
    avg_weight=mean(weight)
  ) |>
  ggplot( 
      aes(x = reorder(stv_category,avg_weight),
        y = avg_weight)
  )+
  geom_col(width=0.3)
yrbss_samp |>
  mutate(
    stv_category= case_when(
      strength_training_7d==0 ~ "no training",
      strength_training_7d>=1 &
        strength_training_7d<=3 ~"low training",
      strength_training_7d>=3 &
        strength_training_7d<=5 ~ "moderate training",
      strength_training_7d>5 ~ "high training")
  ) |>
  group_by(stv_category) |>
  summarise(
    avg_weight=mean(weight)
  ) |>
  ggplot( 
    aes(x = reorder(stv_category,avg_weight),
        y = avg_weight)
  )+
  geom_col(width=0.3)+
  coord_flip() +
  labs(title = "Training",
       subtitle = "Intensity of training against mean height",
       y = "Mean height",
       x = "Training intensity")

yrbss_samp |>
  mutate(
    stv_category= case_when(
      strength_training_7d==0 ~ "no training",
      strength_training_7d>=1 &
        strength_training_7d<=3 ~"low training",
      strength_training_7d>=3 &
        strength_training_7d<=5 ~ "moderate training",
      strength_training_7d>5 ~ "high training")
  ) |>
  group_by(stv_category) |>
  summarise(
    avg_weight=mean(weight)
  ) |>
  ggplot( 
    aes(x = reorder(stv_category,avg_weight),
        y = avg_weight)
  )+
  geom_col(width=0.3) #try descending later#
##diy6homework