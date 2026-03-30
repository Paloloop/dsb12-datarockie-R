#create variable
income <- 50000
expense <- 30000
saving <- income - expense
cost= 100
#remove variable
rm(saving)

as.logical("cost")
as.logical(200)

length(cost)
print(length(cost))

a <- 1:10
dim(a) <- c(2,5)
print(a)
matrix(a,ncol=5,byrow=TRUE)


#subsetting
ages <- c(26,27,32,31,28)
ages[ages>30] #32 31

friend <- c("Wan","Ink","Aan","Bee","Top")
friend[ages>30]

my_list = list(friends = friend,
               ages = ages)
dff <- data.frame(my_list)
names(ages) <- friend
ages
# Wan Ink Aan Bee Top
# 26  27  32  31  28

peach <- list(fav_book = "a", age = 23, sex = "male", a = ages)
peach["age"]
peach$age

peach[["age"]]

peach[["a"]][1]
peach$a[1]


paste0("I love ", "data")
paste("I love ", "data")
