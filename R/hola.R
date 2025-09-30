library(tidyverse)

p_mpg <- ggplot(mtcars, aes(hp, mpg)) +
    geom_point() +
    theme_minimal()
print(p_mpg)
