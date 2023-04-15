## -------------------------------------------------------------------------
beads <- rep(c("red", "blue"), times = c(2,3))
beads

## -------------------------------------------------------------------------
sample(beads, 1)


## -------------------------------------------------------------------------
B <- 10000
events <- replicate(B, sample(beads, 1))


## -------------------------------------------------------------------------
tab <- table(events)
tab


## -------------------------------------------------------------------------
prop.table(tab)


## -------------------------------------------------------------------------
set.seed(1986)


## -------------------------------------------------------------------------
sample(beads, 5)
sample(beads, 5)
sample(beads, 5)


## -------------------------------------------------------------------------
#sample(beads, 6)


## -------------------------------------------------------------------------
options(digits = 3)
set.seed(1986)
events <- sample(beads, B, replace = TRUE)
prop.table(table(events))


## -------------------------------------------------------------------------
set.seed(1986)
x <- sample(beads, 5)


## -------------------------------------------------------------------------
x[2:5]


## -------------------------------------------------------------------------
# install.packages("VennDiagram")
# install.packages("rafalib")
library(VennDiagram)
rafalib::mypar()
grid.newpage()
tmp <- draw.pairwise.venn(22, 20, 11, category = c("A", "B"), 
                   lty = rep("blank", 2), 
                   fill = c("lightblue", "pink"), 
                   alpha = rep(0.5, 2),  
                   cat.dist = rep(0.025, 2), 
                   cex=0, 
                   cat.cex = rep(2.5,2))


## -------------------------------------------------------------------------
number <- "Three"
suit <- "Hearts"
paste(number, suit)


## -------------------------------------------------------------------------
paste(letters[1:5], as.character(1:5), sep = ":", collapse = ",")


## -------------------------------------------------------------------------
expand.grid(pants = c("blue", "black"),
            shirt = c("white", "grey", "plaid"))


## -------------------------------------------------------------------------
ranks <- c("Ace", "Two", "Three", "Four", "Five", "Six", "Seven",
           "Eight", "Nine", "Ten", "Jack", "Queen", "King")
suits <- c("Hearts", "Spades", "Diamonds", "Clubs")
deck <- expand.grid(rank = ranks, suit = suits)
deck <- paste(deck$rank, deck$suit)


## -------------------------------------------------------------------------
kings <- paste("King", suits)
mean(deck %in% kings)


## -------------------------------------------------------------------------
# install.packages("gtools")
library(gtools)
permutations(3, 2)


## -------------------------------------------------------------------------
all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
set.seed(1986)
index <- sample(n, 5)
all_phone_numbers[index,]


## -------------------------------------------------------------------------
hands <- permutations(52, 2, v = deck)
str(hands)


## -------------------------------------------------------------------------
first_card <- hands[,1]
second_card <- hands[,2]


## -------------------------------------------------------------------------
kings <- paste("King", suits)
sum(first_card %in% kings)


## -------------------------------------------------------------------------
sum(first_card %in% kings & second_card %in% kings) / 
  sum(first_card %in% kings)


## -------------------------------------------------------------------------
mean(first_card%in%kings & second_card%in%kings) / mean(first_card%in%kings)


## -------------------------------------------------------------------------
combinations(3, 2)


## -------------------------------------------------------------------------
aces <- paste("Ace", suits)
facecard <- c("Ten", "Jack", "Queen", "King")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52, 2, v = deck)
mean(hands[,1] %in% aces & hands[,2] %in% facecard)


## -------------------------------------------------------------------------
mean((hands[,1] %in% aces & hands[,2] %in% facecard) | 
    (hands[,2] %in% aces & hands[,1] %in% facecard))


## -------------------------------------------------------------------------
hand <- sample(deck, 2)
hand


## -------------------------------------------------------------------------
(hands[1] %in% aces & hands[2] %in% facecard) | 
  (hands[2] %in% aces & hands[1] %in% facecard)


## -------------------------------------------------------------------------
blackjack <- function(){
   hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | 
    (hand[2] %in% aces & hand[1] %in% facecard)
}


## -------------------------------------------------------------------------
blackjack()


## -------------------------------------------------------------------------
B <- 10000
results <- replicate(B, blackjack())
mean(results)


## -------------------------------------------------------------------------
doors <- as.character(1:3)
prize <- sample(c("car", "goat", "goat"))
prize_door <- doors[prize == "car"]
my_pick  <- sample(doors, 1)
sample(doors[!doors %in% c(my_pick, prize_door)])


## -------------------------------------------------------------------------
B <- 10000
monty_hall <- function(strategy){
  doors <- as.character(1:3)
  prize_door <- doors[prize == "car"]
  my_pick  <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)
  switch <- doors[!doors%in%c(my_pick, show)]
  choice <- ifelse(strategy == "stick", my_pick, switch)
  choice == prize_door
}
stick <- replicate(B, monty_hall("stick"))
mean(stick)
switch <- replicate(B, monty_hall("switch"))
mean(switch)


## -------------------------------------------------------------------------



## -------------------------------------------------------------------------
n <- 50
bdays <- sample(1:365, n, replace = TRUE)


## -------------------------------------------------------------------------
duplicated(c(1,2,3,1,4,3,5))


## -------------------------------------------------------------------------
any(duplicated(bdays))


## -------------------------------------------------------------------------
B <- 10000
same_birthday <- function(n) {
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
}
results <- replicate(B, same_birthday(50))
mean(results)


## -------------------------------------------------------------------------
compute_prob <- function(n, B = 10000) {
  results <- replicate(B, same_birthday(n))
  mean(results)
}


## -------------------------------------------------------------------------
n <- seq(1, 60)
prob <- sapply(n, compute_prob)


## -------------------------------------------------------------------------
library(tidyverse)
# qplot(n, prob)  Depracated
data.frame(n = n, prob = prob) |>
  ggplot(aes(n, prob)) +
  geom_point()


## -------------------------------------------------------------------------
exact_prob <- function(n) {
  prob_unique <- seq(365, 365 - n + 1) / 365
  1 - prod(prob_unique)
}
eprob <- sapply(n, exact_prob)
data.frame(n = n, eprob = eprob) |>
  ggplot(aes(n, eprob)) +
  geom_line(col = "red") +
  geom_point(aes(n, prob))


## -------------------------------------------------------------------------
B <- 10^seq(1, 5, len = 100)
compute_prob <- function(B, n = 25) {
  same_day <- replicate(B, same_birthday(n))
  mean(same_day)
}
prob <- sapply(B, compute_prob)
data.frame(logB = log10(B), prob) |>
  ggplot(aes(logB, prob)) +
  xlab("log10(B)") +
  geom_line()


## -------------------------------------------------------------------------
# urn <- c(rep("cyan", 3), rep("magenta", 5), rep("yellow", 7))
urn <- rep(c("cyan", "magenta", "yellow"), times = c(3, 5, 7))
urn

## -------------------------------------------------------------------------
sample(urn, 1)

## -------------------------------------------------------------------------
B <- 10000
events <- replicate(B, sample(urn, 1))
tab <- table(events)
tab

## -------------------------------------------------------------------------
prop.table(tab)

## -------------------------------------------------------------------------
mean(events %in% "cyan")


## -------------------------------------------------------------------------
mean(!events %in% "cyan")


## -------------------------------------------------------------------------
events <- replicate(B, sample(urn, 2))
mean(events[1,] %in% "cyan" & !events[2,] %in% "cyan")


## -------------------------------------------------------------------------
events <- replicate(B, sample(urn, 2, replace = TRUE))
mean(events[1,] %in% "cyan" & !events[2,] %in% "cyan")


## -------------------------------------------------------------------------
events <- replicate(B, sample(urn, 5))
mean(events[1:4,] %in% "yellow" & events[5,] %in% "yellow")

## -------------------------------------------------------------------------
mean(events[1,] %in% "yellow" & 
       events[2,] %in% "yellow" & 
       events[3,] %in% "yellow" & 
       events[4,] %in% "yellow" & 
       events[4,] %in% "yellow")



## -------------------------------------------------------------------------
die <- as.character(c(1,2,3,4,5,6))
sample(die, 6, replace = TRUE)

## -------------------------------------------------------------------------
events <- replicate(B, sample(die, 6, replace = TRUE))
sum(!events == "6")

## -------------------------------------------------------------------------
(length(events) - sum(!events == "6")) / length(events)


## -------------------------------------------------------------------------
events <- replicate(B, sample(c(0,1), 
                              4, 
                              replace = TRUE, 
                              prob = c(0.6, 0.4)))
mean(events == 1)


## -------------------------------------------------------------------------
celtic_wins <- sample(c(0,1), 4, replace = TRUE, prob = c(0.6, 0.4))


## -------------------------------------------------------------------------
celtic_wins <- sample(c(0,1), 7, replace = TRUE, prob = c(0.5, 0.5))
str(celtic_wins)
all_perms <- permutations(2, 7, v = c(0,1), repeats.allowed = TRUE)
mean(all_perms[,1] == 0 & 
       (all_perms[,2] + all_perms[,3] + all_perms[,4] +
          all_perms[,5] + all_perms[,6] + all_perms[,7]) >= 4)


## -------------------------------------------------------------------------
simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))


## -------------------------------------------------------------------------
B <- 10000


## -------------------------------------------------------------------------
set.seed(1)


## -------------------------------------------------------------------------
celtic_wins <- replicate(B, {
  simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
  any(simulated_games=="win")
})


## -------------------------------------------------------------------------
mean(celtic_wins)


## -------------------------------------------------------------------------
library(tidyverse)
library(dslabs)
data(heights)
x <- 
  heights %>% 
  filter(sex == "Male") %>% 
  pull(height)


## -------------------------------------------------------------------------
F <- function(a) mean(x<=a)


## -------------------------------------------------------------------------
1 - F(70)


## -------------------------------------------------------------------------
cont <- 
  data.frame(x=seq(0,5, len = 300), 
             y=dgamma(seq(0,5, len = 300), 2, 2))
disc <- 
  data.frame(x=seq(0, 5, 0.075), 
             y=dgamma(seq(0, 5, 0.075), 2, 2))

ggplot(mapping = aes(x,y)) +
  geom_col(data =  disc) +
  geom_line(data = cont) +
  ylab("f(x)")


## -------------------------------------------------------------------------
m <- mean(x)
s <- sd(x)
1 - pnorm(70.5, m, s)


## -------------------------------------------------------------------------
rafalib::mypar()
plot(prop.table(table(x)), 
     xlab = "a = Height in inches", 
     ylab = "Pr(X = a)")


## -------------------------------------------------------------------------
options(digits = 3)
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)


## -------------------------------------------------------------------------
pnorm(68.5, m, s) - pnorm(67.5, m, s)
pnorm(69.5, m, s) - pnorm(68.5, m, s)
pnorm(70.5, m, s) - pnorm(69.5, m, s)


## -------------------------------------------------------------------------
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, m, s) - pnorm(70.1, m, s)


## -------------------------------------------------------------------------
1 - pnorm(76, m, s)


## -------------------------------------------------------------------------
dat <- tibble(x = seq(-4, 4, length=100) * s + m,
              y = dnorm(x, m, s))
dat_ribbon <- filter(dat, x >= 2 * s + m)
ggplot(dat, aes(x, y)) +
  geom_line() +
  geom_ribbon(aes(ymin = 0, ymax = y), data = dat_ribbon)


## -------------------------------------------------------------------------
n <- length(x)
m <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, m, s)


## -------------------------------------------------------------------------
data.frame(simulated_heights = simulated_heights) %>% 
  ggplot(aes(simulated_heights)) +
  geom_histogram(color = "black", binwidth = 1)


## -------------------------------------------------------------------------
B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, m, s)
  max(simulated_data)
})


## -------------------------------------------------------------------------
mean(tallest >= 7*12)

## -------------------------------------------------------------------------
data.frame(tallest = tallest) %>% 
  ggplot(aes(tallest)) +
  geom_histogram(color = "black", binwidth = 1)


## -------------------------------------------------------------------------
x <- seq(-4, 4, length.out = 100)
# qplot(x, f, geom = "line", data = data.frame(x, f = dnorm(x)))
data.frame(x = x, f = dnorm(x)) %>% 
  ggplot(aes(x, f)) +
  geom_line()


## -------------------------------------------------------------------------
mean <- 64
sd <- 3
pnorm(60, mean, sd)


## -------------------------------------------------------------------------
1 - pnorm(72, mean, sd)


## -------------------------------------------------------------------------
pnorm(67, mean, sd) - pnorm(61, mean, sd)


## -------------------------------------------------------------------------
pnorm(67*2.54, mean*2.54, sd*2.54) - pnorm(61*2.54, mean*2.54, sd*2.54)


## -------------------------------------------------------------------------
pnorm(mean+sd, mean, sd) - pnorm(mean-sd, mean, sd)


## -------------------------------------------------------------------------
qnorm(.99, 69, 3)


## -------------------------------------------------------------------------
B <- 1000
set.seed(1)

highestIQ <- replicate(B, {
  simulated_data <- rnorm(10000, 100, 15)
  max(simulated_data)
})

hist(highestIQ)

