# testing GitHub

data("mtcars")

ggplot(mtcars, aes(mpg, hp)) +
  geom_point() +
  theme_minimal() +
theme(legend.position="bottom")
