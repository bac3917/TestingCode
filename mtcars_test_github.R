# testing GitHub

data("mtcars")

ggplot(mtcars, aes(mpg, hp)) +
  geom_point(color='red') +
  lab(title="My MPG/HP Plot")+
  theme_minimal() +
theme(legend.position="bottom")

# branching
ggplot(mtcars, aes(mpg, hp,color=factor(gear))) +
  geom_point(color='red') +
  labs(title="Relationship between MPG/HP")+
  facet_wrap(~gear)+
  theme_minimal() +
  theme(legend.position="bottom")
