## -------------------------------------------------------------------------
# install.packages("tidyverse")
# install.packages("dslabs")
library(tidyverse)
library(dslabs)
data(gapminder)
gapminder |> 
  as_tibble() |>
  head()


## -------------------------------------------------------------------------
gapminder |>
  filter(year == 2015 & country %in% c("Sri Lanka", "Turkey")) |>
  select(country, infant_mortality)

## -------------------------------------------------------------------------
comp_table <-
  tibble(comparison = rep(1:5, each = 2),
         country = c("Sri Lanka", "Turkey", "Poland", "South Korea", "Malaysia", "Russia", "Pakistan","Vietnam","Thailand","South Africa"))

tmp <- 
  gapminder |>
  filter(year == 2015) |>
  select(country, infant_mortality) |>
  mutate(country = as.character(country))

tab <- 
  inner_join(comp_table, tmp, by = "country") |>
  select(-comparison)

tmp <- 
  bind_cols(slice(tab, seq(1, 9, 2)),
            slice(tab, seq(2, 10, 2)))
names(tmp) <- c("country", "infant mortality", "country", "infant mortality")

if(knitr::is_html_output()){
  knitr::kable(tmp, "html") |>
    kableExtra::kable_styling(bootstrap_options = "striped", full_width = FALSE)
} else{
  knitr::kable(tmp, "latex", booktabs = TRUE) |>
    kableExtra::kable_styling(font_size = 8)
}


## -------------------------------------------------------------------------
filter(gapminder, year == 1962) |>
  ggplot(aes(fertility, life_expectancy)) +
  geom_point()


## -------------------------------------------------------------------------
filter(gapminder, year == 1962) |>
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point()


## -------------------------------------------------------------------------
gapminder |>
  filter(year %in% c(1962, 2012)) |>
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(year ~ continent)


## -------------------------------------------------------------------------
gapminder |>
  filter(year %in% c(1962, 2012)) |>
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(. ~ year)


## -------------------------------------------------------------------------
years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
gapminder |>
  filter(year %in% years & continent %in% continents) |>
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_wrap(~year)


## -------------------------------------------------------------------------
gapminder |>
  filter(year %in% c(1962, 2012)) |>
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_wrap(. ~ year, scales = "free")


## -------------------------------------------------------------------------
gapminder |>
  filter(country == "United States") |>
  ggplot(aes(year, fertility)) +
  geom_point()


## -------------------------------------------------------------------------
gapminder |>
  filter(country == "United States") |>
  ggplot(aes(year, fertility)) +
  geom_line()


## -------------------------------------------------------------------------
countries <- c("South Korea", "Germany")

gapminder |>
  filter(country %in% countries) |>
  ggplot(aes(year, fertility)) +
  geom_line()


## -------------------------------------------------------------------------
countries <- c("South Korea", "Germany")

gapminder |>
  filter(country %in% countries & !is.na(fertility)) |>
  ggplot(aes(year, fertility, group = country)) +
  geom_line()

## -------------------------------------------------------------------------
countries <- c("South Korea", "Germany")

gapminder |>
  filter(country %in% countries & !is.na(fertility)) |>
  ggplot(aes(year, fertility, col = country)) +
  geom_line()


## -------------------------------------------------------------------------
# install.packages("geomtextpath")
library(geomtextpath)

countries <- c("South Korea", "Germany")

gapminder |>
  filter(country %in% countries & !is.na(fertility)) |>
  ggplot(aes(year, 
             life_expectancy, 
             col = country,
             label = country)) +
  geom_textpath() +
  theme(legend.position = "none")


## -------------------------------------------------------------------------
gapminder <- 
  gapminder |>
  mutate(dollars_per_day = gdp / population / 362)
head(gapminder)


## -------------------------------------------------------------------------
past_year <- 1970
gapminder |>
  filter(year == past_year & !is.na(gdp)) |>
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black")


## -------------------------------------------------------------------------
past_year <- 1970
gapminder |>
  filter(year == past_year & !is.na(gdp)) |>
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, color = "black")


## -------------------------------------------------------------------------
gapminder |>
  filter(year == past_year) |>
  summarise(min = min(population), max = max(population))

## -------------------------------------------------------------------------
gapminder |> 
  filter(year == past_year) |>
  ggplot(aes(log10(population))) +
  geom_histogram(binwidth = 0.5, color = "black")


## -------------------------------------------------------------------------
gapminder |>
  filter(year == past_year & !is.na(gdp)) |>
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2")


## -------------------------------------------------------------------------
gapminder |>
  filter(year == past_year & !is.na(gdp)) |>
  mutate(region = reorder(region, dollars_per_day, FUN = median)) |>
  ggplot(aes(dollars_per_day, region)) +
  geom_point() +
  scale_x_continuous(trans= "log2")


## -------------------------------------------------------------------------
gapminder <- 
  gapminder |>
  mutate(group = case_when(
    region %in% c("Western Europe", "Northern Europe","Southern Europe",                        "Northern America", "Australia and New Zealand") ~ "West",
    region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    region %in% c("Caribbean", "Central America", 
                  "South America") ~ "Latin America",
    continent == "Africa" & region != "Northern Africa" ~ "Sub-Saharan",
    TRUE ~ "Others"
  ))


## -------------------------------------------------------------------------
gapminder <- 
  gapminder |> 
  mutate(group = factor(group, levels = c("Others", "Latin America", 
                                          "East Asia", "Sub-Saharan",
                                          "West")))


## -------------------------------------------------------------------------
gapminder |>
  filter(year == past_year & !is.na(gdp)) |>
  ggplot(aes(group, dollars_per_day)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log2") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1)) -> p
p


## -------------------------------------------------------------------------
p + geom_point(alpha = 0.5)


## ----warning=FALSE, out.width="100%", message = FALSE, echo=FALSE, fig.height=3----
set.seed(1987)
z <- sample(c(0,1), 1000, replace = TRUE, prob = c(0.25, 0.75))
x <- rnorm(100)*z + rnorm(100, 5)*(1 - z)
p1 <- qplot(x, geom = "density", fill = 1, show.legend=FALSE, alpha = 0.2) +
  scale_x_continuous(limits=c(-4,8.5))
p2 <- qplot("", x, geom="boxplot")
gridExtra::grid.arrange(p1, p2, nrow = 1)


## -------------------------------------------------------------------------
# install.packages("ggridges")
library(ggridges)
gapminder |>
  filter(year == past_year & !is.na(dollars_per_day)) |>
  ggplot(aes(dollars_per_day, group)) +
  scale_x_continuous(trans = "log2") -> p
p + geom_density_ridges()


## -------------------------------------------------------------------------
p + geom_density_ridges(jittered_points = TRUE)


## -------------------------------------------------------------------------
p + geom_density_ridges(jittered_points = TRUE,
                        position = position_points_jitter(height = 0),
                        point_shape = '|', point_size = 3,
                        point_alpha = 1, alpha = 0.7)


## -------------------------------------------------------------------------
past_year <- 1970
present_year <- 2010
years <- c(past_year, present_year)
gapminder |>
  filter(year %in% years  & !is.na(gdp)) |>
  mutate(west = ifelse(group == "West",
                       "West",
                       "Developing")) |>
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ west)


## -------------------------------------------------------------------------
gapminder |>
  filter(year == past_year & !is.na(dollars_per_day)) |>
  pull(country) -> country_list_1

gapminder |>
  filter(year == present_year & !is.na(dollars_per_day)) |>
  pull(country) -> country_list_2

country_list <- intersect(country_list_1, country_list_2)

length(country_list)

gapminder |>
  summarise(pop = sum(population, na.rm = TRUE)) |>
  pull(pop) -> world_pop
gapminder |>
  filter(country %in% country_list) |>
  summarise(pop = sum(population, na.rm = TRUE)) |>
  pull(pop) -> sample_pop

sample_pop / world_pop


## -------------------------------------------------------------------------
gapminder |>
  filter(year %in% years  & !is.na(gdp) & country %in% country_list) |>
  mutate(west = ifelse(group == "West",
                       "West",
                       "Developing")) |>
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ west)


## -------------------------------------------------------------------------
gapminder |>
  filter(year %in% years & country %in% country_list) |>
  ggplot(aes(group, dollars_per_day)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  xlab("") +
  facet_grid(. ~ year)


## -------------------------------------------------------------------------
gapminder |>
  filter(year %in% years & country %in% country_list) |>
  mutate(year = factor(year)) |>
  ggplot(aes(group, dollars_per_day, fill = year)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  xlab("")


## -------------------------------------------------------------------------
gapminder |>
  filter(year %in% years & country %in% country_list) |>
  mutate(year = ifelse(year == past_year,
                       "past",
                       "present")) |>
  select(country, group, year, dollars_per_day) |>
  spread(year, dollars_per_day) |>
  mutate(percent_increase = (present - past) / past * 100) |>
  mutate(group = reorder(group, percent_increase, FUN = median)) |>
  ggplot(aes(group, percent_increase)) +
  geom_boxplot() +
  geom_point(show.legend = FALSE) +
  theme(axis.text = element_text(angle = 90, hjust = 1))+
  xlab("") +
  ylab(paste("Percent increase:", past_year, "to", present_year))


## -------------------------------------------------------------------------
gapminder |>
  filter(year %in% years & country %in% country_list) |>
  ggplot(aes(dollars_per_day)) +
  geom_density(fill = "burlywood") +
  scale_x_continuous(trans = "log2") +
  facet_grid(. ~ year)


## -------------------------------------------------------------------------
tmp <- gapminder |> 
  filter(year == past_year & country %in% country_list) |>
  mutate(group = ifelse(group == "West", "West", "Developing")) |> 
  group_by(group) |> 
  summarize(n=n()) |>
  spread(group, n)
tmp


## -------------------------------------------------------------------------
gapminder |>
  filter(year %in% years & country %in% country_list) |>
  mutate(group = ifelse(group == "West", "West", "Developing")) |>
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2) +
  facet_grid(year ~ .)


## -------------------------------------------------------------------------
gapminder |>
  filter(year %in% years & country %in% country_list) |>
  mutate(group = ifelse(group == "West", "West", "Developing")) |>
  ggplot(aes(dollars_per_day,
             y = ..count..,
             fill = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.125, 300)) -> p

p + geom_density(alpha = 0.2) +
  facet_grid(year ~ .)


## -------------------------------------------------------------------------
p + 
  geom_density(alpha = 0.2, bw = 0.75) + 
  facet_grid(year ~ .)


## -------------------------------------------------------------------------
gapminder |> 
  filter(year %in% years & !is.na(dollars_per_day)) |>
  ggplot(aes(dollars_per_day, group)) + 
  scale_x_continuous(trans = "log2") + 
  geom_density_ridges(bandwidth = 1.5) +
  facet_grid(. ~ year)


## -------------------------------------------------------------------------
gapminder |> 
  filter(year %in% years & country %in% country_list) |>
  group_by(year) |>
  mutate(weight = population/sum(population)*2) |>
  ungroup() |>
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.125, 300)) + 
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + 
  facet_grid(year ~ .) 


## -------------------------------------------------------------------------
gapminder |> 
  filter(year %in% years & country %in% country_list) |>
  group_by(year) |>
  mutate(weight = population/sum(population)*2) |>
  ungroup() |>
  ggplot(aes(dollars_per_day, 
             fill = group,
             weight = weight)) +
  scale_x_continuous(trans = "log2", limit = c(0.125, 300)) + 
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + 
  facet_grid(year ~ .) 



## ----ecological-fallacy-averages, echo=FALSE------------------------------
library(ggrepel)
gapminder <- gapminder |> 
  mutate(group = case_when(
    region %in% c("Western Europe", "Northern Europe",
                  "Southern Europe", "Northern America", 
                  "Australia and New Zealand") ~ "West",
    region %in% "Northern Africa" ~ "Northern Africa",
    region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    region == "Southern Asia"~ "Southern Asia",
    region %in% c("Central America", "South America", 
                  "Caribbean") ~ "Latin America",
    continent == "Africa" & 
      region != "Northern Africa" ~ "Sub-Saharan",
    region %in% c("Melanesia", "Micronesia", 
                  "Polynesia") ~ "Pacific Islands"))
surv_income <- gapminder |> 
  filter(year %in% present_year & !is.na(gdp) & 
           !is.na(infant_mortality) & !is.na(group)) |>
  group_by(group) |>
  summarize(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 
              1 - sum(infant_mortality/1000*population)/sum(population)) 
#surv_income |> arrange(income) |> print(n=nrow(surv_income))
surv_income |> ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
  scale_x_continuous(trans = "log2", limits = c(0.25, 150)) +
  scale_y_continuous(trans = "logit", limit = c(0.875, .9981), 
                     breaks = c(.85,.90,.95,.99,.995,.998)) +
  geom_label_repel(size = 3, show.legend = FALSE)


## ----ecological-fallacy-all-data, echo=FALSE------------------------------
library(ggrepel)
highlight <- c("Sierra Leone", "Mauritius",  "Sudan", "Botswana", "Tunisia",
               "Cambodia","Singapore","Chile", "Haiti", "Bolivia",
               "United States","Sweden", "Angola", "Serbia")
gapminder |> filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group) ) |>
  mutate(country_name = ifelse(country %in% highlight, as.character(country), "")) |>
  ggplot(aes(dollars_per_day, 1 - infant_mortality/1000, col = group, label = country_name)) +
  scale_x_continuous(trans = "log2", limits=c(0.25, 150)) +
  scale_y_continuous(trans = "logit",limit=c(0.875, .9981),
                     breaks=c(.85,.90,.95,.99,.995,.998)) + 
  geom_point(alpha = 0.5, size = 3) +
  geom_text_repel(size = 4, 
                  show.legend = FALSE, 
                  max.overlaps = 12)

