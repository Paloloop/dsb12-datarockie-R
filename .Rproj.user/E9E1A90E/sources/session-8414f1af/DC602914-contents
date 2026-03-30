library(tidyverse)

## draw 10,000 samples from diamonds
View(diamonds)
qplot(price, data=diamonds, geom = "density") # right skew

# CLT -Central Limit Theorem
diamonds %>% 
  sample_n(100) %>% 
  summarise(avg_price = mean(price))

#replicate 1,000 samples
n_rep <- 1000
result <- replicate(n_rep, {
  diamonds %>%
    sample_n(100) %>% 
    summarise(avg_price = mean(price)) %>% 
    pull() #ดึงค ่าออกมาจาก tibble
})
plot(density(result))

#Mean
mean(diamonds$price)
mean(result)
