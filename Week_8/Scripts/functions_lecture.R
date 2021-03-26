



### Load Libraries
library(tidyverse)
library(here)
library(palmerpenguins)
library(PNWColors)


df <- tibble::tibble( #create a dataframe
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10))



df<-df %>% #rescale every column individually
  mutate(a = (a-min(a, na.rm = TRUE))/(max(a, na.rm = TRUE)-min(a, na.rm = TRUE)),
         b = (b-min(b, na.rm = TRUE))/(max(b, na.rm = TRUE)-min(b, na.rm = TRUE)),
         c = (c-min(c, na.rm = TRUE))/(max(c, na.rm = TRUE)-min(c, na.rm = TRUE)),
         d = (d-min(d, na.rm = TRUE))/(max(d, na.rm = TRUE)-min(d, na.rm = TRUE)))

df

rescale01 <- function(x) {
  value <- (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  return(value)
}

df %>%
  mutate(a = rescale01(a),
         b = rescale01(b),
         c = rescale01(c),
         d = rescale01(d))


#make function to convert degrees F to degrees C
temp_C <- (Temp_F - 32) *5/9

F_to_C <- function(temp_F) { 
  temp_C <- (temp_F - 32) * 5 / 9 
  return(temp_C)
}

F_to_C(32)
F_to_C(212)

#make function to convert degrees C to K
C_to_K <- function(temp_C) { 
  temp_K <- (temp_C + 273.15) 
  return(temp_K)
}

C_to_K(0)


#make plot into function
pal <- pnw_palette("Lake", 3, type = "discrete")

ggplot(penguins,
       aes(x = body_mass_g,
           y = bill_length_mm,
           color = island)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual("Island", values = pal) +
  theme_bw()

myplot <- function(data, x, y) { #create function
  pal <- pnw_palette("Lake", 3, type = "discrete")
  
  ggplot(data,
         aes(x = x,
             y = y,
             color = island)) +
    geom_point() +
    geom_smooth(method = "lm") +
    scale_color_manual("Island", values = pal) +
    theme_bw()
}

myplot(data = penguins, x = body_mass_g, y = bill_length_mm) #doesn't work because we don't have an object named body_mass_g or bill_length_mm!!


myplot <- function(data, x, y) { #indicate that x and y are column names using curly-curlies
  pal <- pnw_palette("Lake", 3, type = "discrete")
  
  ggplot(data,
         aes(x = {{x}},
             y = {{y}},
             color = island)) +
    geom_point() +
    geom_smooth(method = "lm") +
    scale_color_manual("Island", values = pal) +
    theme_bw()
}

myplot(data = penguins, x = body_mass_g, y = bill_length_mm)

myplot(data = penguins, x = body_mass_g, y = flipper_length_mm)
#works now!


#create default in function so that it always uses penguins dataset
myplot <- function(data = penguins, x, y) { 
  pal <- pnw_palette("Lake", 3, type = "discrete")
  
  ggplot(data,
         aes(x = {{x}},
             y = {{y}},
             color = island)) +
    geom_point() +
    geom_smooth(method = "lm") +
    scale_color_manual("Island", values = pal) +
    theme_bw()
}

myplot(x = body_mass_g, y = flipper_length_mm)


#can layer onto plot function
myplot(x = body_mass_g, y = flipper_length_mm) +
  labs(x = "Body mass (g)",
       y = "Flipper length (mm)",
       color = "Island")


#creat if-else statement
a <- 4
b <- 5

if (a > b) { # my question
  f <- 20 # if it is true give me answer 1
} else { # else give me answer 2
  f <- 10
}

f

#plotting with if-else
myplot <- function(data = penguins, x, y, lines = TRUE) { #add new argument for lines
  pal <- pnw_palette("Lake", 3, type = "discrete")
  
  if(lines == TRUE) {
  ggplot(data,
         aes(x = {{x}},
             y = {{y}},
             color = island)) +
    geom_point() +
    geom_smooth(method = "lm") +
    scale_color_manual("Island", values = pal) +
    theme_bw()
}
  else{
    ggplot(data, aes(x = {{x}},
                     y = {{y}},
                     color = island)) +
      geom_point() +
      scale_color_manual("Island", values = pal) +
      theme_bw()
}
}

myplot(x = body_mass_g, y = flipper_length_mm, lines = FALSE) #don't include lines












