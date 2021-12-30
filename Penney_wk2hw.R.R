library(ggplot2)

## Plot 1
hotdogs <- read.csv('http://datasets.flowingdata.com/hot-dog-contest-winners.csv', sep=",", header=TRUE)
hotdogs
barplot(hotdogs$Dogs.eaten)
barplot(hotdogs$Dogs.eaten, names.arg = hotdogs$Year, col="red", border=NA, xlab="Year", ylab="Hot dgos and buns (HDB) eaten")
fill_colors <- c()
for (i in 1:length(hotdogs$Country)) {
  if (hotdogs$Country[i] == "United States") {
    fill_colors <- c(fill_colors, "#821122")
  } else {
    fill_colors <- c(fill_colors, "#cccccc")
  }
}
fill_colors <- c()
for ( i in 1:length(hotdogs$New.record) ) {
  if (hotdogs$New.record[i] == 1) {
    fill_colors <- c(fill_colors, "#821122")
  } else {
    fill_colors <- c(fill_colors, "#cccccc")
  }
}
barplot(hotdogs$Dogs.eaten, names.arg = hotdogs$Year, col=fill_colors, border = NA, 
        xlab = "Year", ylab="Hot dogs and buns (HDB) eaten")

barplot(hotdogs$Dogs.eaten, names.arg = hotdogs$Year, col=fill_colors, 
        border = NA, space = 0.3, xlab = "Year", ylab = "Hot dogs and buns (HDB) eaten", 
        main = "Nathan's Hot Dog Eating Contest Results 1980 - 2010")

## Plot 2
hotdogplaces <- read.csv('http://datasets.flowingdata.com/hot-dog-places.csv', sep = ",", header=TRUE)
hotdogplaces
names(hotdogplaces) <- c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010")
hotdogmatrix <- as.matrix(hotdogplaces)

barplot(hotdogmatrix, border = NA, space = 0.25, ylim = c(0, 200), 
        xlab = "Year", ylab = "Hot dgos and buns (HDB) eaten",
        main = "Hot Dog Eating Contest Results, 1980-2010")
## Plot 3 
subscribers <- read.csv("http://datasets.flowingdata.com/flowingdata_subscribers.csv", sep=",", header=TRUE)

subscribers[1:5,]

plot(subscribers$Subscribers, type="h", ylim=c(0, 30000), xlab="Day", ylab="Subscribers")
points(subscribers$Subscribers, pch=19, col="black")

## Plot 4
population <- read.csv("http://datasets.flowingdata.com/world-population.csv", sep=",", header=TRUE)

plot(population$Year, population$Population, type="l", bty="n", ylim=c(0, 7000000000), xlab="Year", ylab="Population")

## Plot 5
postage <- read.csv("http://datasets.flowingdata.com/us-postage.csv", sep=",", header=TRUE)

plot(postage$Year, postage$Price, type="s", main="US Postage Rates for Letters, First Ounce, 1991-2010", 
     xlab="Year", ylab="Postage Rate (Dollars)")

#Part 2
data <- read.csv('C:/Users/Katherine Penney/Desktop/art.csv', header = TRUE, stringsAsFactors = FALSE)
data

## Part 2.1
par(mfrow = c(2,2))
boxplot(data$total.sale
        , main = "Distribution of total.sales"
        , col = "#FF3399"
        , pch = 8
        , ylab = "Amount (dollars)")

den <- density(data$total.sale)
plot(den,main = "Distribution of total.sales")
polygon (d, col = "#6633FF")



## Part 2.2
artdrawing<-data[data$paper == "drawing" , ]
artwater<-data[data$paper == "watercolor" , ]
artwater


#par(mfrow = c(2,1), mar = c(2,3,1,2))
hist(artdrawing$total.sale, xlab = "Total Sale", main = "Distribution of the Totals Sales for Drawing Paper", col="#33FFCC")
hist(artwater$total.sale, xlab = "Total Sale", main = "Distribution of the Totals Sales for Watercolor Paper", col="#FFFF33")

# Part 3 
## Part 3.1

par(mfrow = c(2,2))
cor(data$unit.price, data$units.sold)
plot(unit.price~units.sold,
     data=data,main="Relationship: Units Sold and Unit Price",
     xlab="Units Sold", ylim= c(0,30), ylab="Unit Price (Dollars)",
     col="#0033FF"
)

## Part 3.2

drawing2<-sum(artdrawing$units.sold)
water2<-sum(artwater$units.sold)
newplot <- cbind(drawing2,water2)
barplot(newplot,beside=T,names.arg = c("Drawing", "Watercolor"), col=c("#CC0066","#66FF33"),
        main="Comparison of Paper Units", ylab = "Units Sold", xlab = "Type of Unit")

library("tapply")

art3 <- tapply(data$total.sale,list(data$paper),FUN=sum)
barplot(art3, col="#FFCCCC",beside = T,
        main="Paper Sales Income", ylab = "Income (dollars)", ylim= c(1,120000))


art1 <- tapply(art$total.sale,list(art$paper),FUN=sum)
thematrix <- as.matrix(art1)
barplot(art1,col=c("#blue", "red"),beside = T,
        main="Paper Sales Income", ylab = "Income in US Dollars")



