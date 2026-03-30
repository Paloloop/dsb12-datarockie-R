1:10:2

a <- 25
ls()
sample(x = 1:3, size = 2,TRUE)
ok = 1:5
sample(ok, size = 1)
round(3.124,2)

die <- 1:6
dice <- sample(die, 2,TRUE)
sum(dice)

roll <- function(){
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE)
  sum(dice)
  500
}
v <- roll()

roll2 <- function(bones) {
  dice <- sample(bones, size = 2, replace = TRUE)
  sum(dice)
}
roll2(1:5)

roll2 <- function(bones = 1:6) {
  dice <- sample(bones, size = 2, replace = TRUE)
  sum(dice)
}
roll2(1:2)
## 9