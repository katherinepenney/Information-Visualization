## Author: Katherine Penney 
## Lab 3: Setting colors in R 


library(RColorBrewer)
my.dir <- "C:/Users/Katherine Penney/Documents/Data Visualization/"
sales <- read.csv(file=paste0(my.dir, "sales.csv")
                  ,header = TRUE
                  ,stringsAsFactors = FALSE)
colnames(sales)
display.brewer.all()

rand.data <- replicate(8, rnorm(35, 35, sd= 1.5))
boxplot(rand.data, col=brewer.pal(8,"Set1"))

num.colors <- 8 
FUN <- colorRampPalette(c("blue","red", "green"))
my.cols <- FUN(num.colors)
boxplot(rand.data, col = my.cols)

plot(sales$expenses, sales$income, pch=16, cex=1, col="orange")
col.vec <- rep("orange", nrow(sales))
plot(sales$expenses, sales$income, pch=16, cex=1, col=col.vec)
col.vec <- rep(rgb(30, 144, 255, maxColorValue = 255), nrow(sales))
plot(sales$expenses, sales$income, pch=16, cex=1, col=col.vec)

hist(sales$unit.price, col="white")
col.vec[sales$unit.price > 14] <- rgb(255, 64,64, maxColorValue = 255)
plot(sales$expenses, sales$income, pch=16, cex=1, col=col.vec)

col.vec <- rep(rgb(30, 144, 255, maxColorValue = 255), nrow(sales))
col.vec[sales$type == "red"] <- rgb(255, 64,64, maxColorValue = 255)
plot(sales$expenses, sales$income, pch=16, cex=1, col=col.vec)




## Overploting and Transperancy 



col.vec <- rep(rgb(.8, .15, .15), nrow(sales))
plot(sales$expenses, sales$income, pch=16, cex=1, col=col.vec)


col.vec <- rep(rgb(.8, .15, .15, alpha = .2), nrow(sales))
plot(sales$expenses, sales$income, pch=16, cex=.3, col=col.vec)

smoothScatter(sales$expenses, sales$income
              , colramp = colorRampPalette(c("black", "cyan", "pink", "red")))


smoothScatter(sales$expenses, sales$income
              , colramp = colorRampPalette(c("white", "cyan", "pink", "red")))

library(aplpack)
bagplot(sales$expenses, sales$income
        , show.whiskers = F
        , col.loophull = "#aaccff"
        , col.looppoints = "#3355ff"
        , col.baghull = "#7799ff"
        , col.bagpoints = #000088"
        , transparency = T)

my.alpha = 100 
col.vec <- rep(rgb(30,144,255, maxColorValue = 255, alpha = my.alpha)
                   , nrow(sales))
col.vec[sales$unit.price > 10] <- rgb(64, 255, 64, maxColorValue = 255
                                          , alpha = my.alpha)

col.vec[sales$unit.price > 14] <- rgb(255,64,64, maxColorValue = 255,
                                      alpha = my.alpha)
plot(sales$expenses, sales$income, col=col.vec)























n <- 1000 
x <- rnorm(n)
y <- x^2 + rnorm(n, mean = 1, sd = .25)
plot(c(x, -1.5, 1.5, 0),c(y, 14,14,0))


A <- sample(c("here", "there", "nowhere", "everywhere"), size= n, replace = T)
B <- sample(c("now", "later"), size= n, replace = T)
barplot(table(B,A), beside=T)


pie(table(A))
