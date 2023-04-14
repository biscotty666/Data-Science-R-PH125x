## -------------------------------------------------------------------------
library(tidyverse)
library(dslabs)
library(gridExtra)


## -------------------------------------------------------------------------
browsers <- data.frame(Browser = rep(c("Opera", "Safari","Firefox",
                                       "IE", "Chrome"),
                                     2),
                       Year = rep(c(2000, 2015), 5),
                       Percentage = c(3,21,23,28,26,2,22,21,27,29)) |>
  mutate(Browser = reorder(Browser, Percentage))
head(browsers)

## -------------------------------------------------------------------------
library(ggthemes)
browsers |>
  ggplot(aes(x = "", y= Percentage, fill = Browser)) +
  geom_bar(width = 1, stat = "identity", col = "black") +
  coord_polar(theta = "y") +
  xlab("") + ylab("") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  facet_grid(. ~ Year) -> p1
p1


## -------------------------------------------------------------------------
browsers |> ggplot(aes(x = 2, y = Percentage, fill = Browser)) +
  geom_bar(width = 1, stat = "identity", col = "black")  + 
  scale_x_continuous(limits=c(0.5,2.5)) + 
  coord_polar(theta = "y") +
  xlab("") + ylab("") +
  theme(axis.text=element_blank(), 
        axis.ticks = element_blank(), 
        panel.grid  = element_blank()) +
  facet_grid(.~Year)


## -------------------------------------------------------------------------
browsers |>
  spread(Year, Percentage)


## -------------------------------------------------------------------------
browsers |>
  ggplot(aes(Browser, Percentage)) +
  geom_bar(stat = "identity", width = 0.5) +
  ylab("Percent using the Browser") +
  facet_grid(. ~ Year) -> p2
grid.arrange(p1, p2, nrow = 2)


## -------------------------------------------------------------------------
library(scales)
browsers <- filter(browsers, Year == 2015)
at <- with(browsers,
           100 - 
             cumsum(c(0,
                      Percentage[-length(Percentage)])) - 
                        0.5 * Percentage)
label <- percent(browsers$Percentage / 100)
at
label
browsers |> 
  ggplot(aes(x = "", y = Percentage, fill = Browser)) +
  geom_bar(width = 1, stat = "identity", col = "black") +
  coord_polar(theta = "y") +
  xlab("") + ylab("") + ggtitle("2015") +
  theme(axis.text=element_blank(), 
        axis.ticks = element_blank(), 
        panel.grid  = element_blank()) +
  annotate(geom = "text", 
              x = 1.62, y =  at, 
              label = label, size=4)


## -------------------------------------------------------------------------
data.frame(Year = as.character(c(2011, 2012, 2013)),
           Southwest_Border_Apprehensions = c(165244,170223,192298)) |>
  ggplot(aes(Year, Southwest_Border_Apprehensions )) +
  geom_bar(stat = "identity", fill = "yellow", col = "black", width = 0.65)


## -------------------------------------------------------------------------
data.frame(date = c("Now", "Jan 1, 2013"), tax_rate = c(35, 39.6)) |>
  mutate(date = reorder(date, tax_rate)) |>
  ggplot(aes(date, tax_rate)) + 
  ylab("") + xlab("") +
  geom_bar(stat = "identity", 
           fill = "yellow", 
           col = "black", width = 0.5) + 
  ggtitle("Top Tax Rate If Bush Tax Cut Expires")


## -------------------------------------------------------------------------
data.frame(Candidate = factor(c("Maduro", "Capriles"),
                              levels = c("Maduro", "Capriles")),
           Percent = c(50.66, 49.07)) |>
  ggplot(aes(Candidate, Percent, fill = Candidate)) +
  geom_bar(stat = "identity", width = 0.65, show.legend = FALSE)


## -------------------------------------------------------------------------
gapminder |>
  filter(year == 2012) |>
  ggplot(aes(continent, life_expectancy)) +
  geom_point() -> p1
p2 <- p1 + scale_y_continuous(limits = c(0, 84))
grid.arrange(p2, p1, ncol = 2)


## -------------------------------------------------------------------------
gdp <- c(14.6, 5.7, 5.3, 3.3, 2.5)
gdp_data <- data.frame(Country = rep(c("United States", "China",
                                       "Japan", "Germany",
                                       "France"),
                                     2),
           y = factor(rep(c("Radius","Area"),each=5), 
                      levels = c("Radius", "Area")),
           GDP= c(gdp^2/min(gdp^2), gdp/min(gdp))) |> 
   mutate(Country = reorder(Country, GDP))
gdp_data |> 
  ggplot(aes(Country, y, size = GDP)) + 
  geom_point(show.legend = FALSE, color = "blue") + 
  scale_size(range = c(2,25)) +
  coord_flip() + ylab("") + xlab("")



## -------------------------------------------------------------------------
gdp_data |>
  filter(y == "Area") |>
  ggplot(aes(Country, GDP)) +
  geom_bar(stat = "identity", width = 0.5) +
  ylab("GDP in trillions")


## -------------------------------------------------------------------------
data(murders)
p1 <- murders |>
  mutate(murder_rate = total / population * 10^5) |>
  ggplot(aes(state, murder_rate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 8)) +
  xlab("")
p2 <- murders |> 
  mutate(murder_rate = total / population * 100000) |>
  mutate(state = reorder(state, murder_rate)) |>
  ggplot(aes(state, murder_rate)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 8))  +
  xlab("")

grid.arrange(p1, p2, ncol = 2)  


## -------------------------------------------------------------------------
past_year <- 1970
gapminder |>
  mutate(dollars_per_day = gdp / population / 365) |>
  filter(year == past_year & !is.na(gdp)) |>
  ggplot(aes(region, dollars_per_day)) +
  geom_boxplot() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") -> p1
gapminder |>
  mutate(dollars_per_day = gdp / population / 365) |>
  filter(year == past_year & !is.na(gdp)) |>
  mutate(region = reorder(region, dollars_per_day, FUN = median)) |>
  ggplot(aes(region, dollars_per_day)) +
  geom_boxplot() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") -> p2

grid.arrange(p1, p2, nrow = 1)


## -------------------------------------------------------------------------
data(heights)
heights |>
  group_by(sex) |>
  summarise(average = mean(height), sd = sd(height)/sqrt(n())) |>
  ggplot(aes(sex, average)) +
  theme_excel() +
  geom_errorbar(aes(ymin = average - 2 * sd,
                    ymax = average + 2 * sd,
                    width = 0.25)) +
  geom_bar(stat = "identity",
           width = 0.5,
           fill = "burlywood",
           color = "black") +
  ylab("Height in inches") -> p1
p1


## -------------------------------------------------------------------------
heights |>
  ggplot(aes(sex, height)) +
  geom_point()


## -------------------------------------------------------------------------
heights |>
  ggplot(aes(sex, height)) +
  geom_jitter(width = 0.1, alpha = 0.2)


## -------------------------------------------------------------------------
heights |>
  ggplot(aes(height, ..density..)) +
  geom_histogram(binwidth = 1, color = "black") +
  facet_grid(. ~ sex, scales = "free_x")


## -------------------------------------------------------------------------
heights |>
  ggplot(aes(height, after_stat(density))) +
  geom_histogram(binwidth = 1, color = "black") +
  facet_grid(. ~ sex, scales = "free_x")


## -------------------------------------------------------------------------
heights |>
  ggplot(aes(height, after_stat(density))) +
  geom_histogram(binwidth = 1, color = "black") +
  facet_grid(. ~ sex)


## -------------------------------------------------------------------------
heights |>
  ggplot(aes(height, after_stat(density))) +
  geom_histogram(binwidth = 1, color = "black") +
  facet_grid(sex ~ .) -> p2
p2


## -------------------------------------------------------------------------
heights |>
  ggplot(aes(sex, height)) +
  geom_boxplot(coef = 3) +
  geom_jitter(width = 0.1, alpha = 0.2) +
  ylab("Height in inches") -> p3
p3


## -------------------------------------------------------------------------
grid.arrange(p1, p2, p3, ncol=3)


## -------------------------------------------------------------------------
data(gapminder)
gapminder |>
  filter(year == 2015) |>
  group_by(continent) |>
  summarise(population = mean(population)) |>
  mutate(continent = reorder(continent, population)) |>
  ggplot(aes(continent, population/10^6)) +
  geom_bar(stat = "identity", width = 0.5, fill = "blue") +
  theme_excel() +
  ylab("Population in Millions") +
  xlab("Continent") -> p1
p1


## -------------------------------------------------------------------------
gapminder |> filter(year == 2015) |> 
  mutate(continent = reorder(continent, population, median)) |>
  ggplot(aes(continent, population/10^6)) + 
  ylab("Population in Millions") +
  xlab("Continent") -> p2
p2 + geom_jitter(width = .1, alpha = .5) 


## -------------------------------------------------------------------------
p2 <- p2 +
  geom_boxplot(coef = 3) +
  geom_jitter(width = .1, alpha = .5) +
  scale_y_log10(breaks = c(1, 10, 100, 1000)) +
  theme(axis.text.x = element_text(size = 7))
grid.arrange(p1, p2, ncol = 2)


## -------------------------------------------------------------------------
gapminder |>
  filter(year %in% c(1970, 2010) & !is.na(gdp)) |>
  mutate(dollars_per_day = gdp / population / 365) |>
  mutate(labels = paste(year, continent)) |>
  ggplot(aes(labels, dollars_per_day)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  ylab("Income in dollars per day")


## -------------------------------------------------------------------------
gapminder |>
  filter(year %in% c(1970, 2010) & !is.na(gdp)) |>
  mutate(dollars_per_day = gdp / population / 365) |>
  mutate(labels = paste(continent, year)) |>
  ggplot(aes(labels, dollars_per_day)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  ylab("Income in dollars per day")


## -------------------------------------------------------------------------
gapminder |>
  filter(year %in% c(1970, 2010) & !is.na(gdp)) |>
  mutate(dollars_per_day = gdp / population / 365,
         year = factor(year)) |>
  mutate(labels = paste(continent, year)) |>
  ggplot(aes(labels, dollars_per_day, fill = year)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  ylab("Income in dollars per day")


## ---- eval=FALSE----------------------------------------------------------
## color_blind_friendly_cols <-
##   c("#999999", "#E69F00", "#56B4E9", "#009E73",
##     "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
## data.frame(x = 1:8,
##            y = rep(1, 8), col = as.character(1:8)) |>
##   ggplot(aes(x, y, color = col)) +
##   geom_point(size = 8, show.legend = FALSE) +
##   theme(axis.title = element_blank(),
##         axis.text = element_blank(),
##         axis.ticks = element_blank(),
##         panel.grid = element_blank()) ->
##   p1
## 
## p1 + scale_color_manual(values = color_blind_friendly_cols)


## -------------------------------------------------------------------------
west <- c("Western Europe","Northern Europe","Southern Europe",
          "Northern America","Australia and New Zealand")

gapminder |>
  filter(year %in% c(2010, 2015) & 
         region %in% west &
         !is.na(life_expectancy) &
         population > 10^7) -> 
  dat

dat |>
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 &
                             country %in% c("United Kingdom", "Portugal"),
                           location + 0.22,
                           location),
         hjust = ifelse(year == 2010, 1, 0)) |>
  mutate(year = as.factor(year)) |>
  ggplot(aes(year, 
             life_expectancy, 
             group = country)) +
  geom_line(aes(color = country), 
            show.legend = FALSE) +
  geom_text(aes(x = location, 
                label = country, 
                hjust = hjust),
            show.legend = FALSE) +
  xlab("") + ylab("Life Expectancy")


## -------------------------------------------------------------------------
library(ggrepel)
c("Western Europe","Northern Europe","Southern Europe",
          "Northern America","Australia and New Zealand") ->
  west

gapminder |> 
  filter(year%in% c(2010, 2015) & region %in% west & 
           !is.na(life_expectancy) & population > 10^7) ->
  dat

dat |> 
  mutate(year = paste0("life_expectancy_", year)) |>
  select(country, year, life_expectancy) |>
  spread(year, life_expectancy) |> 
  ggplot(aes(x=life_expectancy_2010,
             y=life_expectancy_2015, 
             label = country)) + 
  geom_point() + 
  geom_text_repel() +
  scale_x_continuous(limits=c(78.5, 83)) +
  scale_y_continuous(limits=c(78.5, 83)) +
  geom_abline(lty = 2) +
  xlab("2010") + 
  ylab("2015")


## -------------------------------------------------------------------------
library(ggrepel)
dat |> 
  mutate(year = paste0("life_expectancy_", year)) |>
  select(country, year, life_expectancy) |> 
  pivot_wider(names_from = "year", 
              values_from = "life_expectancy") |> 
  mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
         difference = life_expectancy_2015 - life_expectancy_2010) |>
  ggplot(aes(average, 
             difference, 
             label = country)) + 
  geom_point() +
  geom_text_repel() +
  geom_abline(lty = 2) +
  xlab("Average of 2010 and 2015") + 
  ylab("Difference between 2015 and 2010")


## -------------------------------------------------------------------------
present_year <- 2010

gapminder |>
  mutate(region = case_when(
    region %in% west ~ "The West",
    region %in% "Northern Africa" ~ "Northern Africa",
    region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    region == "Southern Asia"~ "Southern Asia",
    region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    continent == "Africa" & region != "Northern Africa" ~ "Sub-Saharan Africa",
    region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"),
    dollars_per_day = gdp / population / 365) |>
  filter(year %in% present_year & 
         !is.na(gdp) & !is.na(infant_mortality) & 
         !is.na(region) ) |>
  mutate(OPEC = ifelse(country%in%opec, "Yes", "No")) ->
  dat

dat |>
  ggplot(aes(dollars_per_day,
             1 - infant_mortality / 1000,
             col = region,
             size = population / 10^6,
             pch = OPEC)) +
  scale_x_continuous(trans = "log2",
                     limits = c(0.25, 150)) +
  scale_y_continuous(trans = "logit",
                     limit = c(0.875, .9981),
                     breaks = c(.85, .90, .95, .99, .995, .998)) +
  geom_point(alpha = 0.5) +
  ylab("Infant survival proportion")


## ----available-shapes, echo=FALSE, fig.height=2.25------------------------
dat=data.frame(x=c(0:25))
ggplot() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
scale_shape_identity() + scale_y_reverse() +
geom_point(dat, 
           mapping=aes(x%%9, 
                       x%/%9, 
                       shape=x), 
           size=4, 
           fill="blue") +
geom_text(dat, 
          mapping=aes(x%%9, 
                      x%/%9+0.25, 
                      label=x), 
          size=4) 


## -------------------------------------------------------------------------
library(RColorBrewer)
display.brewer.all(type="seq")


## -------------------------------------------------------------------------
display.brewer.all(type = "div")


## -------------------------------------------------------------------------
url <- "https://github.com/kbroman/Talk_Graphs/raw/master/R/fig8dat.csv"
dat <- read.csv(url)
dat |> 
  gather(drug, survival, -log.dose) |>
  mutate(drug = gsub("Drug.","",drug)) |>
  ggplot(aes(log.dose, survival, color = drug)) +
  geom_line()   


## -------------------------------------------------------------------------
data(us_contagious_diseases)
tmp <- options()$digits
options(digits=7)
dat <- us_contagious_diseases |>
  filter(year %in% seq(1940, 1980, 10) &  state == "California" &
          disease %in% c("Measles", "Pertussis", "Polio")) |>
  mutate(rate = count / population * 10000) |> 
  mutate(state = reorder(state, rate)) |> 
  select(state, year, disease, rate) |>
  spread(disease, rate)
dat
options(digits=tmp)


## -------------------------------------------------------------------------
dat <- dat |>
  mutate_at(c("Measles", "Pertussis", "Polio"), 
            ~signif(., digits = 1))
dat


## -------------------------------------------------------------------------
dat <- us_contagious_diseases |>
  filter(year %in% seq(1940, 1980, 10) &  state == "California" &
          disease %in% c("Measles", "Pertussis", "Polio")) |>
  mutate(rate = count / population * 10000) |> 
  mutate(state = reorder(state, rate)) |> 
  select(state, year, disease, rate) |>
  spread(year, rate) |> 
  mutate_if(is.numeric, round, digits=1) 
dat


## -------------------------------------------------------------------------
library(tidyverse)
library(RColorBrewer)
library(dslabs)
data(us_contagious_diseases)
str(us_contagious_diseases)

## -------------------------------------------------------------------------
head(us_contagious_diseases)


## -------------------------------------------------------------------------
the_disease <- "Measles"
us_contagious_diseases |>
  filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) |>
  mutate(rate = count / population * 10000 * 52 / weeks_reporting) |>
  mutate(state = reorder(state, 
                         ifelse(year <= 1963, rate, NA),
                         median,
                         na.rm = TRUE)) ->
  dat
dat


## -------------------------------------------------------------------------
dat |>
  filter(state == "California" & !is.na(rate)) |>
  ggplot(aes(year, rate)) +
  geom_line() +
  ylab("Cases per 10,000") +
  geom_vline(xintercept = 1963, col = "darkgoldenrod")


## -------------------------------------------------------------------------
dat |> ggplot(aes(year, state, fill = rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradientn(colors = brewer.pal(9, "OrRd"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 8)) +
  labs(title = the_disease, x = "", y = "")


## -------------------------------------------------------------------------
avg <- 
  us_contagious_diseases |>
  filter(disease == the_disease) |>
  group_by(year) |>
  summarise(us_rate = sum(count, na.rm = TRUE) / 
              sum(population, na.rm = TRUE) * 10000)
avg

## -------------------------------------------------------------------------
dat |>
  filter(!is.na(rate)) |>
  ggplot() +
  geom_line(aes(year, rate, group = state),
            color = "grey50",
            show.legend = FALSE,
            alpha = 0.2,
            linewidth = 1) +
  geom_line(aes(year, us_rate), 
            avg, 
            linewidth = 1) +
  scale_y_continuous(trans = "sqrt",
                     breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +  xlab("") + ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"),
            color = "black") +
  geom_vline(xintercept=1963, col = "blue")

