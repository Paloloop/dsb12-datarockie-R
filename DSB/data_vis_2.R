library(tidyverse)

## density, histogram - one variable, number
base <- ggplot(data=diamonds, mapping = aes(x=price))

base + geom_histogram(bins=10)
base + geom_density()

## bar chart - one variable, factor (discrete)
ggplot(data =diamonds,
       mapping = aes(cut)) + geom_bar()

## stacked bar - two variables, both discrete
ggplot(diamonds,aes(x=cut,fill=color))+geom_bar()+theme_minimal()

## stacked bar 100% - two variables, both discrete
ggplot(diamonds,aes(x=cut,fill=color))+geom_bar(position = "fill")+theme_minimal()+labs(caption="hello world",subtitle = "Made by peach",title="Stacked Bar Chart",x="Cut Quality",y="Proportion")

## dodge bar chart
ggplot(diamonds,aes(x=cut,fill=color))+geom_bar(position = "dodge")+theme_minimal()+labs(caption="hello world",subtitle = "Made by peach",title="Stacked Bar Chart",x="Cut Quality",y="Proportion")

## scatter plot - two variable, both number
ggplot(diamonds,  aes(x=price, y=carat)) + geom_point() + geom_smooth(method = "loess")

## facet -> the most powerful data viz technique in R
set.seed(42)
diamonds %>% sample_n(5000) %>% ggplot(., aes(x=carat, y=price)) + 
  geom_point() +
  facet_wrap(~cut, ncol=5) +
  theme_minimal()

set.seed(42)
diamonds %>% sample_n(5000) %>% ggplot(., aes(x=carat, y=price)) + 
  geom_point() +
  geom_smooth(method="loess",col="red",se=FALSE) +
  facet_grid(cut ~ color) +
  theme_minimal()

## Box Plot
diamonds %>% 
  group_by(cut) %>% 
  summarise(min = min(price),
            q1 = quantile(price, 0.25),
            q2 = median(price),
            q3 = quantile(price, 0.75),
            max = max(price))
ggplot(diamonds, aes(x=cut,y=price))+geom_boxplot()

diamonds %>% 
  group_by(cut) %>% 
  summarise(min = min(price),
            q1 = quantile(price, 0.25),
            q2 = median(price),
            q3 = quantile(price, 0.75),
            max = max(price))
ggplot(diamonds, aes(x=cut,y=price,fill=cut))+geom_violin()

##in case too many data
mini_dia <- sample_n(diamonds,2000)
ggplot(mini_dia,  aes(x=price, y=carat, color =cut, shape = color)) + geom_point(alpha=0.6)
ggplot(mini_dia,  aes(x=price, y=carat)) + geom_point(color="blue", alpha = 0.2)

##conditional color
set.seed(42)
diamonds %>% 
  mutate(price_segment = if_else(price >= 10000, "High","Low")) %>%
  sample_n(500) %>% 
  ggplot(data = ., aes(x=price, y=carat, color=price_segment)) +
  geom_point() +
  scale_color_manual(values = c("black", "gold"))+
  theme_minimal()

## method 02: call geom_...() twice
ggplot() + 
  geom_point(
    data = filter(sample_n(diamonds,500), price>=10000),
    mapping = aes(x=price, y=carat),
    color = "blue") +
  geom_point(
    data = filter(sample_n(diamonds,500), price<10000),
    mapping = aes(x=price, y=carat),
    color = "gold") +
  theme_minimal()

##shortcut function : qplot()
qplot(x=cut, data=diamonds, geom="bar") + theme_minimal()
qplot(x=cut, data=diamonds, geom="density")+ theme_minimal()

## Built-in themes in ggplot
library(ggthemes)
qplot(x=cut, data=diamonds, geom="bar") + theme_economist()
