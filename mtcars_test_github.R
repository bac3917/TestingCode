# testing GitHub

data("mtcars")

ggplot(mtcars, aes(mpg, hp)) +
  geom_point() +
  lab(title="My MPG/HP Plot")+
  theme_minimal() +
theme(legend.position="bottom")
