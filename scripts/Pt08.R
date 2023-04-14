## -------------------------------------------------------------------------
library(dplyr)
library(ggplot2)


## ----dev='svg'------------------------------------------------------------
library(dslabs)
data(murders)
library(ggthemes)
library(ggrepel)

r <- murders |> 
  summarize(pop=sum(population), tot=sum(total)) |> 
  mutate(rate = tot/pop*10^6) |> pull(rate)

murders |>
  ggplot(aes(x = population / 10^6,
             y = total,
             label = abb)) +
  geom_abline(intercept = log10(r),
              lty = 2,
              col = "darkgrey") +
  geom_point(aes(color = region),
             size = 3) +
  geom_text_repel(max.overlaps = 11) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()


## -------------------------------------------------------------------------
library(dslabs)
data(murders)


## -------------------------------------------------------------------------
murders |> ggplot()


## -------------------------------------------------------------------------
p <- ggplot(data = murders)
class(p)


## -------------------------------------------------------------------------
p + geom_point(aes(population / 10^6, total))


## -------------------------------------------------------------------------
p + 
  geom_point(aes(population / 10^6, total)) +
  geom_text(aes(population / 10^6,
                total,
                label = abb))


## -------------------------------------------------------------------------
p + 
  geom_point(aes(population / 10^6, 
                 total),
             size = 3) +
  geom_text(aes(population / 10^6,
                total,
                label = abb))


## -------------------------------------------------------------------------
p + 
  geom_point(aes(population / 10^6, 
                 total),
             size = 3) +
  geom_text(aes(population / 10^6,
                total,
                label = abb),
            nudge_x = 1.5)


## -------------------------------------------------------------------------
args(ggplot)


## -------------------------------------------------------------------------
murders |> 
  ggplot(aes(population / 10^6, 
             total, 
             label = abb)) -> p


## -------------------------------------------------------------------------
p +
  geom_point(size = 3) +
  geom_text(nudge_x = 1.5)


## -------------------------------------------------------------------------
p +
  geom_point(size = 3) +
  geom_text(aes(x = 10,
                y = 800,
                label = "Hello there!"))


## -------------------------------------------------------------------------
p +
  geom_point(size = 3) +
  geom_text(nudge_x = 0.05) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")


## -------------------------------------------------------------------------
p +
  geom_point(size = 3) +
  geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10()


## -------------------------------------------------------------------------
p +
  geom_point(size = 3) +
  geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") +
  ylab("Total number of murders (log scale") +
  ggtitle("US Gun Murders in 2010")


## -------------------------------------------------------------------------
murders |>
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_point(size = 3) +
  geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") +
  ylab("Total number of murders (log scale") +
  ggtitle("US Gun Murders in 2010") -> p


## -------------------------------------------------------------------------
p + geom_point(size = 3, color = "blue")


## -------------------------------------------------------------------------
p + geom_point(aes(col = region), size = 3)


## -------------------------------------------------------------------------
murders |>
  summarise(rate = sum(total) / sum(population) * 10^6) |>
  pull(rate) -> r


## -------------------------------------------------------------------------
p +
  geom_point(aes(col = region),
             size = 3) +
  geom_abline(intercept = log10(r))


## -------------------------------------------------------------------------
p <- p + 
  geom_abline(intercept = log10(r),
              lty = 2,
              color = "darkgrey") +
  geom_point(aes(col=region), size = 3)
p


## -------------------------------------------------------------------------
p <- p + scale_color_discrete(name = "Region")
p


## -------------------------------------------------------------------------
# install.packages(c("ggthemes","ggrepel"))


## -------------------------------------------------------------------------
ds_theme_set()


## -------------------------------------------------------------------------
library(ggthemes)
p + theme_economist()

## -------------------------------------------------------------------------
p + theme_fivethirtyeight()


## -------------------------------------------------------------------------
library(ggthemes)
library(ggrepel)

murders |>
  summarise(rate = sum(total) / sum(population) * 10^6) |>
  pull(rate) -> r

murders |> 
  ggplot(aes(population/10^6, total, label = abb)) +   
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col=region), size = 3) +
  geom_text_repel() + 
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") + 
  scale_color_discrete(name = "Region") +
  theme_economist()


## -------------------------------------------------------------------------
data(murders)
x <- log10(murders$population)
y <- murders$total


## -------------------------------------------------------------------------
qplot(x, y)


## -------------------------------------------------------------------------
# install.packages("gridExtra")
library(gridExtra)
p1 <- qplot(x)
p2 <- qplot(y)
grid.arrange(p1, p2, ncol=2)


## -------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(dslabs)
data(heights)
data(murders)


## -------------------------------------------------------------------------
p <- murders |> ggplot()


## -------------------------------------------------------------------------
class(p)


## -------------------------------------------------------------------------
p


## -------------------------------------------------------------------------
p <- heights |> ggplot()


## -------------------------------------------------------------------------
class(p)


## -------------------------------------------------------------------------
murders |> ggplot(aes(x = population, y = total)) +
  geom_point()


## -------------------------------------------------------------------------
murders |> ggplot(aes(total, population)) +
  geom_point()


## -------------------------------------------------------------------------
murders |> ggplot(aes(total, 
                      population), 
                  label = abb) +
  geom_point()


## -------------------------------------------------------------------------
murders |> ggplot(aes(total, 
                      population), 
                  label = abb) +
  geom_point(color = "blue")


## -------------------------------------------------------------------------
murders |> ggplot(aes(population, 
                      total,
                      label = abb,
                      color = region)) +
  geom_point()


## -------------------------------------------------------------------------
p <- murders |> 
  ggplot(aes(population, total, label = abb, color = region)) +
  geom_label()


## -------------------------------------------------------------------------
p <- murders |> 
  ggplot(aes(population, total, label = abb, color = region)) +
  geom_label() + 
  scale_x_log10()
p


## -------------------------------------------------------------------------
p <- murders |> 
  ggplot(aes(population, total, label = abb, color = region)) +
  geom_label() + 
  scale_x_log10() +
  scale_y_log10()
p


## -------------------------------------------------------------------------
p <- murders |> 
  ggplot(aes(population, total, label = abb, color = region)) +
  geom_label() + 
  scale_x_log10() +
  scale_y_log10() +
  ggtitle("Gun murder data")
p

