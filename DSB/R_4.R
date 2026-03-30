#subsetting vector
ages <- c(26,27,32,31,28)
ages[ages>30] #32 31

friends <- c("Wan","Ink","Aan","Bee","Top")
names(ages) <- friends
ages
names(ages)
attributes(ages)

dim(ages)

varA <- data.frame(friends,ages)
head(varA)
varA$news <- 1:5
head(varA)
varA$news <- NULL


deck2$new <- 1:52

head(deck2)
##   face   suit value new
##   king spades    13   1
##  queen spades    12   2
##   jack spades    11   3
##    ten spades    10   4
##   nine spades     9   5
##  eight spades     8   6

library(googlesheets4)
url <- "https://docs.google.com/spreadsheets/d/18VIeuS3zkf001hO7ydcjjcGt3TF1Phk5Kr28J4gcmx8/edit?usp=sharing"
gs4_deauth()
read_sheet(url,sheet="Sheet1")


class(read_csv("student.csv"))






library(jsonlite)
data <- fromJSON("blackpink.json")
data
data.frame(data)



#read excel file
econ <- read_excel("students.xlsx", sheet=1)
business <- read_excel("students.xlsx", sheet=2)
data <- read_excel("students.xlsx", sheet=3)

#bind_rows
library(dplyr)
bind_rows(econ,business,data)

list_df <- list(econ,business,data)
bind_rows(list_df) #same output


ages <- c(26,27,32,31,28)
friends <- c("Wan","Ink","Aan","Bee","Top")
varA <- data.frame(friends,ages)
varA
varB <- bind_rows(friends,ages)

df1 <- data.frame(
  id = 1:3,
  name = c("peach","lisa","jiso")
)

df2 <- data.frame(
  id = 4:5,
  name = c("Davis","Backham")
)

df3 <- data_frame(
  id = 6:7,
  name = c("joey","anna")
)

bind_rows(df1,df2,df3)

df1 %>%
  bind_rows(df2) %>% 
  bind_rows(df3)

list_df <- list(df1,df2,df3)
bind_rows(list_df)

## long vs. wide format
wp <- WorldPhones %>% 
  data.frame() %>% 
  # . represents df in previous step
  mutate(year = rownames(.))
  rownames(wp) <- NULL
#tidyr
library(tidyr)
library(tibble)
long_wp <- WorldPhones %>%
           as.data.frame() %>% 
           rownames_to_column(var = "year") %>% 
           pivot_longer(N.Amer:Mid.Amer,
                       names_to = "Region",
                       values_to = "Sales")

wide_wp <- long_wp %>% 
           pivot_wider(names_from = "Region",
                       values_from = "Sales")

sum_long_wp <- long_wp %>% 
  group_by(Region) %>% 
  summarize (total_sales = sum(Sales))

## mean imputation
min_df <- mtcars %>% 
  select(model,hp,wt) %>% 
  tibble()
min_df[3,2] <- NA
min_df[6,3] <- NA

avg_hp <- mean(min_df$hp) #NA
avg_hp <- mean(min_df$hp, na.rm=TRUE) #148.4194

min_df %>% 
  mutate(hp = replace_na(hp, avg_hp),
         wt = replace_na(wt, mean(wt, na.rm=T)))

min_df %>%
  filter(if_any(everything(),is.na))

min_df %>%
  filter(if_any(c(hp, wt), is.na))

testdf <- data.frame(
  A = c("A","B","C"),
  B = 1:3,
  C = c(1,1000,1002)
)
testdf %>% filter(if_any(where(is.numeric), ~ .x > 120))
testdf %>% filter(if_any(everything(), ~ .x > 120))

#sql
schoold <- read_csv("school.csv")
sqldf("Select * FROM schoold")

sqldf("Select avg(student), sum(student) from schoold")

sqldf("select school_id, school_name, country from schoold")

# load library
library(RSQLite)
library(tidyverse)

# connect to SQLite database (.db file)
# 1. open connection
conn <- dbConnect(SQLite(), "chinook.db")

# 2. get data
dbListTables(conn)
dbListFields(conn, "customers")

df <- dbGetQuery(conn, "select * from customers where country = 'USA'")
df2 <- dbGetQuery(conn, "select * from customers where country = 'United Kingdom'")

df3 <- dbGetQuery(conn, "
                  select 
                    t1.customerid,
                    sum(total) as total_invoice
                  from customers t1
                  left join invoices t2
                    on t1.customerid = t2.customerid
                  group by 1
                  order by 2 desc
                  limit 20")
t1 <- dbGetQuery(conn, "select * from customers")
t2 <- dbGetQuery(conn,"select * from invoices")

t1 %>% 
  left_join(t2,by =  "CustomerId") %>% 
  group_by(CustomerId, FirstName, LastName) %>% 
  summarize(total_invoice = sum(Total)) %>% 
  View()

# 4.clean column name
library(janitor)
## to snake-case
new_table <- clean_names(t1) %>% 
  head() ##FirstName -> first_name
##rename
rename(new_table, latt_name = last_name) 

# 3. close connection
dbDisconnect(conn)





