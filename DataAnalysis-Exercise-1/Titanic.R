library("dplyr")
library("ggplot2")
library("titanic")

#  This just copies the data to the same variable to reduce the editing needed from the test
titanic <- titanic_train

# titanic is avaliable in your workspace
# 1 - Check the structure of titanic
str(titanic)

# Note on the test system the variable Sex was already a factor in the dataset.
# I factored it when it was needed, but if I was working on this set for real would update the dataset
# avoid this in further plots.
#
# 2 - Use ggplot() for the first instruction
ggplot(titanic, aes(x = Pclass, fill = factor(Sex))) +
  geom_bar(position = "dodge")

# 3 - Plot 2, add facet_grid() layer
ggplot(titanic, aes(x = Pclass, fill = factor(Sex))) +
  geom_bar(position = "dodge") +
  facet_grid(. ~ Survived)

# 4 - Define an object for position jitterdodge, to use below
posn.jd <- position_jitterdodge(0.5, 0, 0.6)

# 5 - Plot 3, but use the position object from instruction 4
ggplot(titanic, aes(x = Pclass, y = Age, color = factor(Sex))) +
  geom_point(position = posn.jd, size = 3, alpha = 0.5) +
  facet_grid(. ~ Survived)

