#### basic plots (base R)
hist(mtcars$mpg)

#### Analyzing horse power
### Histogram - One Quantitative Variable
hist(mtcars$hp)
mean(mtcars$hp)
median(mtcars$hp)

str(mtcars)
# Clean data - Convert am field to factor
mtcars$am <- factor(mtcars$am,
                    levels = c(0,1),
                    labels = c("Auto","Manual"))

### Bar Plot - One Qualitative Variable
barplot(table(mtcars$am))

### Box Plot - One Quantitative Variable / One Quantitative x One Qualitative
boxplot(mtcars$hp)
fivenum(mtcars$hp) #52  96 123 180 335 -> min Q1(bottom) Q2(median) Q3(top) max(outlier)

min(mtcars$hp) #52
max(mtcars$hp) #335

quantile(mtcars$hp, probs = c(0.25,0.5,0.75))
#25%   50%   75% 
#96.5 123.0 180.0 


summary(mtcars$hp)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#52.0    96.5   123.0   146.7   180.0   335.0 


## whisker calculation
Q3 <- quantile(mtcars$hp, probs = 0.75)
Q1 <- quantile(mtcars$hp, prob = .25)
IQR_hp <- Q3-Q1 #83.5

top_whisker <- Q3 + 1.5*IQR_hp #data point above this count as outliers
low_whisker <- Q1 - 1.5*IQR_hp #-28.75 but hp can't be negative so we use min hp instead

boxplot.stats(mtcars$hp, coef = 1.5) #this will show what data points are outliers so we can filter they out
#$stats
#[1]  52  96 123 180 264

#$n
#[1] 32

#$conf
#[1]  99.5382 146.4618

#$out
#[1] 335

## filter out outliers
mtcars_no_out <- mtcars %>% 
  filter(hp < 335)
boxplot(mtcars_no_out$hp)

## Boxplot 2 Variables : Qualitative x Quantitative
boxplot(mpg ~ am, data = mtcars,
        col = c("gold","salmon"))

## Scatter Plot
# 2 x Quantitative
plot(mtcars$hp, mtcars$mpg,
     pch=16,
     col="blue",
     main = "Relationship between HP and MPG",
     xlab = "Horse Power",
     ylab = "Miles Per Gallon")

cor(mtcars$hp, mtcars$mpg)
lm(mpg ~ hp, data = mtcars)

#### library tidyverse & ggplot2
library(tidyverse)

## Scatter plot
ggplot(data = mtcars, mapping = aes(x = hp, y = mpg)) +
  geom_point() +
  geom_smooth() +
  geom_rug()

ggplot(mtcars,aes(hp, mpg)) +
  geom_point(size=3, col="blue", alpha=0.6) # col = colour, alpha = opacity

sample_diamonds <- sample_n(diamonds, 5000)
ggplot(sample_diamonds, aes(carat,price)) + 
  geom_point()

### FACET : small multiples
ggplot(sample_diamonds, aes(carat,price)) + 
  geom_point() +
  geom_smooth(method="lm", col="red") +
  facet_wrap(~color, ncol=2) + 
  theme_minimal() +
  labs(title="Relationship between carat and price by color", x="Carat", y="Price USD", caption="Source: Diamonds from ggplot2 package")

ggplot(sample_diamonds, aes(carat,price, col=cut)) + 
  geom_point(size=3, alpha=0.4) +
  facet_wrap(~color, ncol=2) + 
  geom_smooth(method="lm", col="red",alpha=0.1) +
  theme_minimal()
  labs(title="Relationship between carat and price by color", x="Carat", y="Price USD", caption="Source: Diamonds from ggplot2 package")

## Histogram
ggplot(mtcars, aes(hp)) +
  geom_histogram(bins=10, fill="red", Alpha=0.5) #default=30 ควรเลือก bin เอง

## Box Plot
p <- ggplot(mtcars, aes(hp))
p + geom_boxplot()
p + geom_density()

#Box plot by groups
diamonds %>% 
  count(cut)

ggplot(diamonds, aes(cut)) +
  geom_bar(fill="#0366fc")

#3-variables
ggplot(diamonds, aes(cut, fill=color)) +
  geom_bar(position = "stack")

ggplot(diamonds, aes(cut, fill=color)) +
  geom_bar(position = "dodge")

ggplot(diamonds, aes(cut, fill=color)) +
  geom_bar(position = "fill")







