## load package
library(dplyr)
library(readr)

## read csv file
imdb <- read.csv("imdb.csv",stringsAsFactors = FALSE)
View(imdb)

## review data structure
glimpse(imdb)
str(imdb)
head(imdb,10)

## select columns
select(imdb, MOVIE_NAME, RATING)
select(imdb,1,5)

select(imdb, movie_name = MOVIE_NAME, released_year = YEAR)

## pipe operator
imdb %>% 
  select(movie_name = MOVIE_NAME, released_year = YEAR) %>%
  head(10)


#rename header
names(imdb) <- tolower(names(imdb))

## filter data
filter(imdb, SCORE>=9.0)
imdb %>% filter(SCORE >= 9.0)

imdb %>% 
  select(movie_name, year, score) %>%
  filter(score >= 9 & year > 2000)

imdb %>%
  select(movie_name,length,score) %>%
  filter(score == 8.8 | score == 8.3 | score == 9.0)

imdb %>%
  select(movie_name,length,score) %>%
  filter(score %in% c(8.3,8.8,9.3))

imdb %>%
  select(movie_name, genre, rating) %>%
  filter(rating == "R" & grepl("Drama",imdb$genre))

mtcars %>%  select(model, hp, wt, am) %>%  filter(hp >=100 | we >3)

mtcars %>% select(model,hp,wt,am) %>% filter(grepl("^A",model))

## Create new columns
#condition
imdb %>%
  select(movie_name, score, length) %>%
  mutate(score_group = if_else(score >= 9, "High Rating", "Low Rating"),
         length_group = if_else(length >= 12, "Long Film", "Short Film"))

imdb %>%
  select(movie_name, score) %>%
  mutate(score_update = score+0.1,
         score_update_new = score_update+0.1) %>%
  head(10)

mtcars %>%  select(model,hp) %>% head(10) %>% mutate(hp_double = hp*2)

mtcars %>% select(model,hp,am) %>% mutate(segment = case_when(
  hp >= 200 ~ "high",
  hp >= 100 ~ "medium",
  hp < 100 ~ "low",
  TRUE ~ "other"
))

#เขียนทับ column เดิม
imdb %>%
  select(movie_name, score) %>%
  mutate(score = score+0.1) %>%
  head(10)

head(imdb)

## Arrange Data
imdb %>%
  arrange(length) %>%
  head(10)

imdb %>%
  arrange(desc(length)) %>%
  head(10)

mtcars %>% select(model, am, hp) %>%  filter(hp>100) %>% arrange(desc(hp),desc(am))

## summarize and group by
imdb %>%
  filter(rating != "") %>%
  group_by(rating) %>%
  summarize(mean_length = mean(length),
            sd_length = sd(length),
            min_length = min(length),
            max_length = max(length),
            n = n())

mtcars %>% mutate(am = if_else(am==0,"Auto","Manual"), AM = am) %>%  group_by(AM) %>% summarize(avg_hp = mean(hp),
                      sum_hp = sum(hp),
                      min_hp = min(hp),
                      max_hp = max(hp),
                      n = n())

##join data
favorite_films <- data.frame(id = c(5,10,25,30,98))

favorite_films %>%
  inner_join(imdb, by = c("id" = "no"))

df_tibble <- tibble(id = 1:3, name = c("peach","sud","lhor"))

sample_n(imdb, size=5)
sample_frac(imdb,size=0.1)

inner_join(band_members, band_instruments, by="name")

band_members %>%
  left_join(band_instruments, by="name")



# slice
imdb %>%
  slice(2:5)
imdb %>%
  slice(1,3,5)
imdb %>% slice(sample(nrow(imdb),10))

slice(imdb,5:9)

imdb %>%
  sample_n(5:9)

ืname <- peach

##regular expression
grepl("Drama",imdb$genre)

state.name
index1 <- grep("^A",state.name)
state.name[index1]
grep("^A",state.name, value=TRUE)

index2 <- grepl("^A",state.name)
state.name[index2]


grep("^New.+e$",state.name)


##lubridate
library(lubridate)

text_date <- "2025-10-11"
d1 <- ymd(text_date)

#extracting date part
year(d1) #2025
month(d1) #10
month(d1, label=TRUE) #Oct
month(d1, label=TRUE, abbr=FALSE) #October
day(d1) #11
wday(d1) #7
wday(d1, label=TRUE) #Sat
wday(d1, label=TRUE, abbr=FALSE) #Saturday

#messy date
dmy("11 oct 2025") #2025-10-11
dmy("11.10.2025") #2025-10-11
dmy("11102025") #2025-10-11
dmy("11/10/25")#2025-10-11
mdy("OCTOBER, 11 - 2025")
mdy("10/11/2025") #2025-10-11
myd("Oct, 2025,11") #2025-10-11
ymd("2025-10-11 11:32:00") #NA ไม่ได้

##select coluumn
select(mtcars, 1,3,5)
select(mtcars,1:3)
select(mtcars,contains("a"))
select(mtcars,starts_with("h"))
select(mtcars,ends_with("P"))
select(mtcars, hp, everything())


rename(imdb, new_movie_name=movie_name)

##SQL
sqldf("select avg(score) as avg_score, sum(score) from imdb")

##ggplot - diamonds
diamonds %>% 
  count(cut, sort=TRUE) %>% 
  mutate(pct = n/sum(n))

diamonds %>% 
  group_by(cut) %>% 
  summarize(n = n())

set.seed(42)
diamonds %>% 
  sample_n(5)




