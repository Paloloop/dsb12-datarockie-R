score <- 95
if(score >= 80){
  print("Pass")
}else if (score >= 50){
  print("OK")
}else{
  print("Failed")
}
new <- ifelse(score>=90,"Hello","World")
print(new)

friends <- c("Toy","John","Mary")

for(friend in friends){
  print(paste("Hi!",friend))
}

for(friend in friends){
  print("Hi!"&friend)
}

paste("Hi!", friends)


num <- 10
num <- num-2


x <- c(10, 25, 50, 100)
sum(x) #
mean(x)
sd(x)

greeting <- function(){
  print("Hello World!")
}

greeting()

greeting_name <- function(name ){
  print(paste("Hello!",name))
}

greeting_name("peach")

greeting_name <- function(name ="Toy", age = 25){
  print(paste("Hello:",name))
  print(paste("Ages:",age))
}
greeting_name() 
#"Hello: Toy"
#"Age: 25"
greeting_name("peach",30) 
#"Hello peach"

grading <- function(score){
  #score between 0-100
  #return Grade : A,B,C,Failed
  
  if (score > 100 & score <0){
    return ("errorr")
  }
  
  if(score >= 80){
    result <- "A : Excellent"
  }else if (score >= 70){
    result <- "B : Good"
  }else if (score >= 60){
    result <- "C : OK"
  } else{
    result <- "Failed"
  }
  result
}

a <- c(TRUE, TRUE, FALSE, FALSE)
b <- c(TRUE, FALSE, TRUE, FALSE)

# Performs TRUE&TRUE, TRUE&FALSE, FALSE&TRUE, FALSE&FALSE
result_and <- a & b

print(result_and)
# Output: [1]  TRUE FALSE FALSE FALSE



greeting_v2 <- function(friends){
  paste0("Helllo ", friends)
}

while(TRUE){
  print("Hello")
  exit <- readline("Are you bored? ")
  if(exit == "Yes"){
    print("bye")
    break
  }
}
  
  