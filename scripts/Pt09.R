## -------------------------------------------------------------------------
library(tidyverse)
library(dslabs)
data(heights)


## -------------------------------------------------------------------------
prop.table(table(heights$sex))


## -------------------------------------------------------------------------
murders |>
  group_by(region) |>
  summarise(n = n()) |>
  mutate(Proportion = n/sum(n),
         region = reorder(region, Proportion)) |>
  ggplot(aes(x = region,
             y = Proportion,
             fill = region)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("")


## -------------------------------------------------------------------------
ds_theme_set()
heights |>
  filter(sex == "Male") |>
  ggplot(aes(height)) +
  stat_ecdf() +
  ylab("Proportion of heights less than or equal to a") +
  xlab("a")


## -------------------------------------------------------------------------
heights |>
  filter(sex == "Male") |>
  ggplot(aes(height)) +
  geom_histogram(binwidth = 1,
                 color = "black")


## -------------------------------------------------------------------------
heights |> 
  filter(sex=="Male") |> 
  ggplot(aes(height)) + 
  geom_density(alpha = .2, 
               fill= "#00BFC4", 
               color = 0)  +
  geom_line(stat='density')


## -------------------------------------------------------------------------
heights |>
  ggplot(aes(height,
             fill = sex)) +
  geom_density(alpha = 0.2,
               color = 0) +
  geom_line(stat = "density")


## -------------------------------------------------------------------------
mu <- 0
s <- 1
norm_dist <- 
  data.frame(x = seq(-4, 4, len=50)*s+mu) |>
  mutate(density = dnorm(x, mu, s))
norm_dist |>
  ggplot(aes(x, density)) +
  geom_line()


## -------------------------------------------------------------------------
index <- heights$sex == "Male"
x <- heights$height[index]

# calculate standard units

z <- scale(x)

# calculate proportion of values within 2 SD of mean
mean(abs(z) < 2)


## -------------------------------------------------------------------------
pnorm(70.5, mean(x), sd(x))


## -------------------------------------------------------------------------
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")


## -------------------------------------------------------------------------
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)


## -------------------------------------------------------------------------
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))


## -------------------------------------------------------------------------
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))


## -------------------------------------------------------------------------
index <- heights$sex == "Male"
x <- heights$height[index]


## -------------------------------------------------------------------------
m <- sum(x) / length(x)
m


## -------------------------------------------------------------------------
s <- sqrt(sum(x-mu^2) / length(x))


## -------------------------------------------------------------------------
m <- mean(x)
s <- sd(x)
c(average = m, sd = s)


## -------------------------------------------------------------------------
norm_dist <- 
  data.frame(x = seq(-4, 4, len = 50)*s + m) |>
  mutate(density = dnorm(x, m, s))
heights |>
  filter(sex == "Male") |>
  ggplot(aes(height)) +
  geom_density(fill = "#0099FF") +
  geom_line(aes(x, density),
            data = norm_dist,
            lwd = 1.5)


## -------------------------------------------------------------------------
summary(heights$height)


## -------------------------------------------------------------------------
p <- seq(0.01, 0.99, 0.01)
percentiles <- quantile(heights$height, p)


## -------------------------------------------------------------------------
percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]


## -------------------------------------------------------------------------
p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles <- qnorm(p, 69, 3)
theoretical_quantiles


## -------------------------------------------------------------------------
data(murders)
murders <-
  murders |>
  mutate(rate = total / population * 10^5)
library(gridExtra)
murders |>
  ggplot(aes(x = rate)) +
  geom_histogram(binwidth = 0.5,
                 color = "black") +
  ggtitle("Histogram")


## -------------------------------------------------------------------------
murders |>
  ggplot(aes("", rate)) +
  geom_boxplot() +
  coord_cartesian(xlim = c(0,2)) +
  xlab("")


## -------------------------------------------------------------------------
p <- seq(0.05, 0.95, 0.05)
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd(x))


## -------------------------------------------------------------------------
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)


## -------------------------------------------------------------------------
observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p)
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)


## -------------------------------------------------------------------------
heights |>
  ggplot(aes(x = sex, 
             y = height, 
             fill = sex)) +
  geom_boxplot()


## -------------------------------------------------------------------------
heights |>
  filter(sex == "Female") |>
  ggplot(aes(height)) +
  geom_density(fill = "#F8766D")


## -------------------------------------------------------------------------
heights |>
  filter(sex == "Female") |>
  slice_max(desc(height), n=5) |>
  pull(height)


## -------------------------------------------------------------------------
library(dslabs)
data(heights)
male <- heights$height[heights$sex == "Male"]
female <- heights$height[heights$sex == "Female"]


## -------------------------------------------------------------------------
length(male)
length(female)


## -------------------------------------------------------------------------
heights |>
  filter(sex == "Female") |>
  summarise(Females = quantile(height, c(10, 30, 50, 70, 90)/100)) -> fh
heights |>
  filter(sex == "Male") |>
  summarise(Males = quantile(height, c(10, 30, 50, 70, 90)/100)) -> mh
Percentiles <- c("10th","30th","50th","70th","90th")
data.frame(Percentiles, fh, mh)


## -------------------------------------------------------------------------
library(tidyverse)
library(dslabs)
ds_theme_set()
data(gapminder)
gapminder |> 
  filter(year == 2010) |> 
  group_by(continent) |> 
  select(continent, population) -> tab 
tab |> ggplot(aes(x=continent, y=population/10^6)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = c(1,10,100,1000)) + 
  ylab("Population in millions")


## -------------------------------------------------------------------------
library(tidyverse)
library(dslabs)
ds_theme_set()
data(gapminder)
gapminder |> 
  filter(year == 2010) |> 
  group_by(continent) |> 
  select(continent, population) -> tab 
tab |> ggplot(aes(x=continent, y=population/10^6)) + 
  geom_boxplot() + 
  scale_y_continuous(trans = "log10",
                     breaks = c(1,10,100,1000)) + 
  ylab("Population in millions")


## -------------------------------------------------------------------------
library(dslabs)
data(heights)
x <- heights$height[heights$sex=="Male"]


## -------------------------------------------------------------------------
heights |>
  filter(height < 69 & height <= 72) -> targets
tar_count <- length(targets)
tar_count / mean(heights$height)


## -------------------------------------------------------------------------
murders |>
  ggplot(aes(region)) +
  geom_bar()


## -------------------------------------------------------------------------
data(murders)
murders |>
  count(region) |>
  mutate(proportion = n/sum(n)) -> tab
tab


## -------------------------------------------------------------------------
tab |>
  ggplot(aes(region, proportion)) + 
  geom_bar(stat = "identity")


## -------------------------------------------------------------------------
heights |>
  filter(sex == "Female") |>
  ggplot(aes(height)) +
  geom_histogram()


## -------------------------------------------------------------------------
heights |>
  filter(sex == "Female") |>
  ggplot(aes(height)) +
  geom_histogram(binwidth = 1)


## -------------------------------------------------------------------------
heights |>
  filter(sex == "Female") |>
  ggplot(aes(height)) +
  geom_histogram(binwidth = 1, fill="blue", col="black") +
  xlab("Female heights in inches") +
  ggtitle("Histogram")


## -------------------------------------------------------------------------
heights |>
  filter(sex == "Female") |>
  ggplot(aes(height)) +
  geom_density(fill = "blue")


## -------------------------------------------------------------------------
heights |>
  filter(sex == "Female") |>
  ggplot(aes(height)) +
  geom_density(fill = "blue",
               adjust = 2)


## -------------------------------------------------------------------------
heights |> 
  ggplot(aes(sex,height)) +
  geom_boxplot()


## -------------------------------------------------------------------------
heights |>
  filter(sex == "Male") |>
  ggplot(aes(sample = height)) +
  geom_qq() +
  geom_qq_line()


## -------------------------------------------------------------------------
params <- 
  heights |>
  filter(sex == "Male") |>
  summarise(mean = mean(height),
            sd = sd(height))
heights |>
  filter(sex == "Male") |>
  ggplot(aes(sample = height)) +
  geom_qq(dparams = params) +
  geom_abline()


## -------------------------------------------------------------------------
heights |>
  filter(sex == "Male") |>
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()


## -------------------------------------------------------------------------
x <- expand.grid(x = 1:12, y = 1:10) |>
  mutate(z = 1:120)
x


## -------------------------------------------------------------------------
x |>
  ggplot(aes(x, y, fill = z)) +
  geom_raster()


## -------------------------------------------------------------------------
x |> 
  ggplot(aes(x, y, fill = z)) + 
  geom_raster() + 
  scale_fill_gradientn(colors =  terrain.colors(10))
x


## -------------------------------------------------------------------------
heights |>
  filter(sex == "Male") |>
  pull(height) -> x
qplot(x)


## -------------------------------------------------------------------------
qplot(sample = scale(x)) +
  geom_abline()


## -------------------------------------------------------------------------
height <- heights |> pull(height)
sex <- heights |> pull(sex)
qplot(sex, height)


## -------------------------------------------------------------------------
sex <- heights |> pull(sex)
qplot(sex, height, geom = "boxplot")


## -------------------------------------------------------------------------
qplot(x, geom = "density")


## -------------------------------------------------------------------------
qplot(x, 
      bins = 15,
      color = I("black"),
      xlab = "Population")

