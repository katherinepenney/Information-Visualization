# 
# Katherine Penney
# Lab 01
#

pie(c(7,6,7.2,12))

x <- c(7,6,7.2,12)

pie(x)

pie(x, main = "Jeff's Pie")

pie(x, main = "Jeff's Pie", col = c("red", "orange", "tan", "yellow"))

pie(x
    , main = "Jeff's Pie"
    , col = c("red", "orange", "tan", "yellow")
    , labels = c("a","b","c","d")
    )

plot(c(1,3,4,7))

plot(c(1,3,4,7), pch = 16, col = c("red", "orange", "tan", "yellow")
     , cex = 3)

x <- "jeff"
x
x <- rnorm(n=10)
plot(x)
plot(x, type = "l")
plot(x, type = "h")

plot(x, type = "h", lwd =5, lend=2, col="orange"
     , main = "change in net worth"
     , xlab= "time in years"
     , ylab= "in millions"
     , bty= "n")
par()


plot(x, type="h", lwd =20, col= c("red", "orange"), btw="n", lend =2)
par(bg="white")
my.par <- par()
par(my.par)

n <- 27

my.letters <- sample(letters[1:3], size =n, replace =T)

letters[7:9]
letters[c(8,3,1)]

my.letters

table(my.letters)
tab <- table(my.letters)
barplot(tab, col = c("brown", "tan", "orange")
        , names.arg = c("sales", "ops", "it")
        , border = "white"
        , xlab = "departments"
        , ylab = "employees"
        , main = "Company Employees"
        , horiz = TRUE
        , las =1)

barplot(tab, col = c("brown", "tan", "orange")
        , names.arg = c("sales", "ops", "it")
        , border = "white"
        , xlab = "departments"
        , ylab = "employees"
        , main = "Company Employees"
        , horiz = TRUE
        , las =1
        , density = 20
        , angle = c(45, 90, 12))

x= rnorm(n=1000, mean = 10, sd=1)
hist(x, main = "What is the distribution of x")

boxplot(x, horizontal = T)

x <- rlnorm(n=1000, meanlog = 1, sdlog = 1)

par(mfrow = c(1,1))

boxplot(x, horizontal = T)
hist(x)


hist(log10(x))

x^2
