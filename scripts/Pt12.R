## -------------------------------------------------------------------------
prop.table(table(state.region))


## -------------------------------------------------------------------------
library(tidyverse)
library(dslabs)
data("heights")


## -------------------------------------------------------------------------
heights |>
  filter(sex == "Male") |>
  ggplot(aes(height)) +
  stat_ecdf() +
  ylab("F(a)") + xlab("a")


## -------------------------------------------------------------------------
heights |>
  filter(sex == "Male") |>
  ggplot(aes(height)) +
  geom_histogram(binwidth = 1, 
                 color = "black")


## -------------------------------------------------------------------------
heights |>
  filter(sex == "Male") |>
  ggplot(aes(height)) +
  geom_density(fill = "darkgoldenrod", 
               alpha = .2)


## -------------------------------------------------------------------------
set.seed(1988)
x <- data.frame(height = c(rnorm(1000000, 69, 3),
                           rnorm(1000000, 65, 3)))
x |> ggplot(aes(height)) + geom_histogram(binwidth = 1, color = "black")


## -------------------------------------------------------------------------
xplt <- x |> ggplot(aes(height))
p1 <- xplt + geom_histogram(binwidth = 1) + ggtitle("binwidth = 1")
p2 <- xplt + geom_histogram(binwidth = 0.5) + ggtitle("binwidth = 0.5")
p3 <- xplt + geom_histogram(binwidth = 0.1) + ggtitle("binwidth = 0.1")
library(gridExtra)
grid.arrange(p1, p2, p3, nrow = 1)


## -------------------------------------------------------------------------
xplt +
  geom_histogram(aes(y = ..density..), binwidth = 0.1) +
  geom_line(stat = "density")


## -------------------------------------------------------------------------
hist1 <- 
  heights |>
  filter(sex == "Male") |>
  ggplot(aes(height)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1,
                 color = "black")
hist2 <- 
  hist1 +
  geom_line(stat = "density")
hist3 <- 
  hist1 +
  geom_point(data = ggplot_build(hist2)$data[[1]],
             aes(x,y),
             col = "blue")
hist4 <- 
  ggplot() +
  geom_point(data = ggplot_build(hist2)$data[[1]],
             aes(x,y),
             col = "blue") +
  xlab("height") + ylab("density")
hist5 <- 
  hist4 +
  geom_line(data = ggplot_build(hist2)$data[[2]], aes(x,y))
hist6 <- 
  heights |>
  filter(sex == "Male") |>
  ggplot(aes(height)) +
  geom_density(alpha = 0.2,
               fill = "darkgoldenrod",
               col = 0) +
  geom_line(stat = "density") +
  scale_y_continuous(limits = layer_scales(hist2)$y$range$range)

grid.arrange(hist1, hist2, hist3, hist4, hist5, hist6, nrow=2)


## -------------------------------------------------------------------------
tmp <- 
  heights |> 
  ggplot(aes(height)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1,
                 alpha = 0.5)
p1 <- tmp +
  geom_line(stat = "density", adjust = 0.5)
p2 <- tmp +
  geom_line(stat = "density", adjust = 2)

grid.arrange(p1, p2, ncol=2)


## -------------------------------------------------------------------------
d <- with(heights, density(height[sex == "Male"]))
tmp <- data.frame(height = d$x, density = d$y)
tmp |>
  ggplot(aes(height, density)) +
  geom_line() +
  geom_area(aes(x = height, density),
            data = filter(tmp, between(height, 65, 68)),
            alpha = 0.4,
            fill = "darkgoldenrod")


## -------------------------------------------------------------------------
heights |>
  filter(sex == "Male") |>
  ggplot(aes(height)) +
  geom_density(alpha = 0.4,
            fill = "darkgoldenrod")


## -------------------------------------------------------------------------
data("murders")
ds_theme_set()
str(murders)


## -------------------------------------------------------------------------
murders |>
  group_by(region) |>
  summarise(n = n()) |>
  mutate(Proportion = n / sum(n),
         region = reorder(region, Proportion)) |>
  ggplot(aes(region, Proportion, fill = region)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("")


## -------------------------------------------------------------------------
heights |>
  filter(sex == "Male") |>
  ggplot(aes(height)) +
  stat_ecdf() +
  ylab("F(a)") + xlab("a")


## -------------------------------------------------------------------------
str(murders)

## -------------------------------------------------------------------------
murders |>  
  mutate(murder_rate = total / population * 10^5) |>
  ggplot(aes(murder_rate)) +
  stat_ecdf()


## -------------------------------------------------------------------------
heights |>
  filter(sex == "Male") |>
  ggplot(aes(height)) +
  geom_histogram(binwidth = 1, color = "black")


## -------------------------------------------------------------------------
murders |>
  ggplot(aes(population/10^6)) +
  geom_density(fill = "darkgoldenrod") +
  scale_x_log10()


## -------------------------------------------------------------------------
murders |>
  ggplot(aes(population / 10^6)) +
  xlab("Population in millions") ->
  tmp
p1 <- 
  tmp +
  geom_density(bw = 5, fill = "grey") +
  ggtitle("1")
p2 <- 
  tmp +
  geom_density(bw = .05, fill = "grey") +
  scale_x_log10() +
  ggtitle("2")
p3 <- 
  tmp +
  geom_density(bw = 1, fill = "grey") +
  scale_x_log10() +
  ggtitle("3")
grid.arrange(p1, p2, p3, ncol = 2)


## -------------------------------------------------------------------------
m <- 0; s <- 1
norm_dist <- 
  data.frame(x = seq(-4, 4, len = 50) * s + m) |> 
  mutate(density = dnorm(x, m, s))
norm_dist |> 
  ggplot(aes(x,density)) + 
  geom_line()


## -------------------------------------------------------------------------
index <- heights$sex == "Male"
x <- heights$height[index]
str(x)

## -------------------------------------------------------------------------
y <- heights |>
  filter(sex == "Male") |>
  pull(height)
identical(x, y)


## -------------------------------------------------------------------------
options(digits = 3)
m <- mean(x)
s <- sd(x)
c(average = m, sd = s)


## -------------------------------------------------------------------------
norm_dist <- 
  data.frame(x = seq(-4, 4, len=50)*s + m) |> 
  mutate(density = dnorm(x, m, s))
heights |> 
  filter(sex == "Male") |> 
  ggplot(aes(height)) +
  geom_density(fill="#0099FF") +
  geom_line(aes(x, density),  
            data = norm_dist, 
            lwd=1.5) 


## -------------------------------------------------------------------------
z <- scale(x)
mean(abs(z) < 2)


## -------------------------------------------------------------------------
pnorm(-1.96)


## -------------------------------------------------------------------------
qnorm(0.975)


## -------------------------------------------------------------------------
qnorm(0.975,
      mean = 5,
      sd = 2)


## -------------------------------------------------------------------------
mean(x <= 69.5)


## -------------------------------------------------------------------------
p <- seq(0.05, 0.95, 0.05)


## -------------------------------------------------------------------------
sample_quantiles <- quantile(x, p)
sample_quantiles


## -------------------------------------------------------------------------
theoretical_quantiles <- qnorm(p, mean(x), sd(x))
theoretical_quantiles


## -------------------------------------------------------------------------
qplot(theoretical_quantiles, sample_quantiles) +
  geom_abline()


## -------------------------------------------------------------------------
sample_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p) 
qplot(theoretical_quantiles, sample_quantiles) + 
  geom_abline()


## -------------------------------------------------------------------------
heights |>
  filter(sex == "Male") |>
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()


## -------------------------------------------------------------------------
data(murders)
murders <- murders |>
  mutate(rate = total / population * 10^5)
library(gridExtra)
p1 <- 
  murders |>
  ggplot(aes(rate)) +
  geom_histogram(binwidth = 0.5, color = "black") +
  ggtitle("Histogram")
p2 <- 
  murders |> 
  ggplot(aes(sample=rate)) + 
  geom_qq(dparams=summarize(murders, mean=mean(rate), sd=sd(rate))) +
  geom_abline() + 
  ggtitle("QQ-plot")
grid.arrange(p1, p2, ncol = 2)


## -------------------------------------------------------------------------
murders |>
  ggplot(aes("", rate)) +
  geom_boxplot() +
  coord_cartesian(xlim = c(0, 2)) +
  xlab("")


## -------------------------------------------------------------------------
heights |> 
  ggplot(aes(x=sex, y=height, fill=sex)) +
  geom_boxplot()


## -------------------------------------------------------------------------
p1 <- heights |>
  ggplot(aes(height)) +
  geom_density(fill = "olivedrab2") +
  xlab("Women's heights")
p2 <- heights |>
  ggplot(aes(sample = scale(height))) +
  geom_qq() + geom_abline() +
  ylab("Standard Units")
grid.arrange(p1, p2, ncol = 2)


## -------------------------------------------------------------------------
heights |>
  filter(sex == "Female") |>
  slice_min(height, n = 5) |>
  pull(height)


## -------------------------------------------------------------------------
library(dslabs)
data("heights")
male <- heights |> filter(sex == "Male") |> pull(height)
male2 <- heights$height[heights$sex == "Male"]
female <- heights$height[heights$sex == "Female"]
identical(male, male2)


## -------------------------------------------------------------------------
length(male); length(female)


## -------------------------------------------------------------------------
mp <- heights |>
  filter(sex == "Male") |>
  reframe(male_percentiles = quantile(height, c(10, 30, 50, 70, 90)/100))
fp <- 
  heights |>
  filter(sex == "Female") |>
  reframe(female_percentiles = quantile(height, c(10, 30, 50, 70, 90)/100))
data.frame(mp, fp)


## -------------------------------------------------------------------------
ds_theme_set()
data(gapminder)
tab <- 
  gapminder |> 
  filter(year == 2010) |> 
  group_by(continent) |> 
  select(continent, population)  
tab |> 
  ggplot(aes(x=continent, y=population/10^6)) + 
  geom_boxplot() + 
  scale_y_continuous()
  ylab("Population in millions")


## -------------------------------------------------------------------------
tab |> 
  ggplot(aes(x=continent, y=population/10^6)) + 
  geom_boxplot() + 
  scale_y_continuous(trans = "log10", breaks = c(1,10,100,1000)) +
  ylab("Population in millions")


## -------------------------------------------------------------------------
data(heights)
x <- heights$height[heights$sex == "Male"]


## -------------------------------------------------------------------------
mean(69 < x & x < 72)


## -------------------------------------------------------------------------
m <- mean(x)
s <- sd(x)
pnorm(x, m, s)


## -------------------------------------------------------------------------
library(tidyverse)
library(dslabs)
data(outlier_example)
str(outlier_example)


## -------------------------------------------------------------------------
options(digits = 3)
mean(outlier_example); sd(outlier_example)


## -------------------------------------------------------------------------
boxplot(outlier_example)


## -------------------------------------------------------------------------
median(outlier_example)


## -------------------------------------------------------------------------
IQR(outlier_example) / 1.349


## -------------------------------------------------------------------------
q3 <- qnorm(0.75)
q1 <- qnorm(0.25)
iqr <- q3 - q1
r <- c(q1 - 1.5*iqr, q3 + 1.5*iqr)
r


## -------------------------------------------------------------------------
round(pnorm(r[2]) - pnorm(r[1]),3)*100


## -------------------------------------------------------------------------
max_height <- quantile(outlier_example, 0.75) + 3 * IQR(outlier_example)
max_height


## -------------------------------------------------------------------------
x <- outlier_example[outlier_example < max_height]
qqnorm(x)
qqline(x)


## -------------------------------------------------------------------------
mad(outlier_example)


## -------------------------------------------------------------------------
# install.packages("HistData")


## -------------------------------------------------------------------------
library(HistData)
data(Galton)
x <- Galton$child
str(x)


## -------------------------------------------------------------------------
mean(x)
median(x)


## -------------------------------------------------------------------------
median(x)
mad(x)


## -------------------------------------------------------------------------
x_with_error <- x
x_with_error[1] <- x_with_error[1]*10
mean(x_with_error) - mean(x)

## -------------------------------------------------------------------------
sd(x_with_error) - sd(x)


## -------------------------------------------------------------------------
median(x_with_error) - median(x)


## -------------------------------------------------------------------------
mad(x_with_error) - mad(x)


## -------------------------------------------------------------------------
error_avg <- function(x, k) {
  x_with_error <- x
  x_with_error[1] <- x_with_error[1]*k
  mean(x_with_error)
}
error_avg(x, 10000)
error_avg(x, -10000)


## -------------------------------------------------------------------------
library(dslabs)
data("reported_heights")
str(reported_heights)

## -------------------------------------------------------------------------
reported_heights <- 
  reported_heights |>
  mutate(original_heights = height,
         height = as.numeric(height))
str(reported_heights)


## -------------------------------------------------------------------------
reported_heights |> filter(is.na(height)) |> head()


## -------------------------------------------------------------------------
reported_heights <- filter(reported_heights, !is.na(height))
reported_heights |> filter(is.na(height)) |> head()


## -------------------------------------------------------------------------
reported_heights |>
  group_by(sex) |>
  summarise(average = mean(height), sd = sd(height),
            median = median(height), MAD = mad(height))


## -------------------------------------------------------------------------
reported_heights |>
  ggplot(aes(sex, height)) +
  geom_boxplot()


## -------------------------------------------------------------------------
reported_heights |>
  arrange(desc(height)) |>
  slice_max(height, n = 10)


## -------------------------------------------------------------------------
whisker <- 3*IQR(reported_heights$height)
max_height <- quantile(reported_heights$height, .75) + whisker
min_height <- quantile(reported_heights$height, .25) - whisker
reported_heights |> 
  filter(!between(height, min_height, max_height)) |> 
  select(original_heights) |>
  head(n=10) |> pull(original_heights)

