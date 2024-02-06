#### Preamble ####
# Purpose: Simulation, Test and Visulization
# Author: Kelly Lyu
# Date: 6 February 2024
# Contact: kelly.lyu@mail.utoronto.ca
# Pre-requisites: None

### Simulation

# Load library
library(ggplot2)
install.packages("ggplot2")

# for reproducibility
set.seed(74)

# Number of days
days <- 100

# Generate reading data for Jacki, Rol, and Mike
jacki_page <- sample(10:100, days, replace = TRUE)
rol_page <- sample(10:100, days, replace = TRUE)
mike_page <- sample(10:100, days, replace = TRUE)

# Generate correlated reading data for Matt and Ash
matt_page <- sample(10:100, days, replace = TRUE)

# Create a correlation
ash_page <- matt_pages + rnorm(days, 0, 10)
ash_page <- ifelse(ash_pages < 10, 10, ash_pages) 

# Combine data into a data frame
reading_data <- data.frame(Day = 1:days, Matt = matt_page, Ash = ash_page,
                           Jacki = jacki_page, Rol = rol_page, Mike = mike_page)

### Test

# Test 1: No negative pages
test1 <- all(reading_data[, -1] >= 0)
print(paste("Test 1 - No negative pages:", test1))

# Test 2: Total days for each person is 100
test2 <- all(nrow(reading_data) == 100)
print(paste("Test 2 - Total days for each person is 100:", test2))

# Test 3: Reading data contains 5 people's pages
test3 <- ncol(reading_data) == 6 # 5 people + 1 'Day' column
print(paste("Test 3 - Reading data contains 5 people's pages:", test3))

# Test 4: Variability in pages read
test4 <- all(sapply(reading_data[, -1], function(x) length(unique(x)) > 1))
print(paste("Test 4 - Variability in pages read:", test4))

# Test 5: Minimum and maximum pages read per day for each person
min_pages_per_day <- sapply(reading_data[, -1], min)
max_pages_per_day <- sapply(reading_data[, -1], max)
test5_replacement <- all(min_pages_per_day >= 10) && all(max_pages_per_day <= 110)
print(paste("Test 5 - Min and Max pages read per day within range:", test5_replacement))


### Graph

# Plotting the line graph
ggplot() +
  geom_line(data = reading_data, aes(x = Day, y = Matt, colour = "Matt")) +
  geom_line(data = reading_data, aes(x = Day, y = Ash, colour = "Ash")) +
  geom_line(data = reading_data, aes(x = Day, y = Jacki, colour = "Jacki")) +
  geom_line(data = reading_data, aes(x = Day, y = Rol, colour = "Rol")) +
  geom_line(data = reading_data, aes(x = Day, y = Mike, colour = "Mike")) +
  scale_colour_manual(values = c("Matt" = "red", "Ash" = "blue", "Jacki" = "green", "Rol" = "purple", "Mike" = "orange")) +
  theme_minimal() +
  labs(title = "Daily Pages Read by Each Person Over 100 Days", x = "Day", y = "Pages Read", colour = "Person")


