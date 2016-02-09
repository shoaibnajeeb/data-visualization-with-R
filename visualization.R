author <- list(name = "Shoaib Najeeb Arayilakath", email = "shoaibnajeeb@gmail.com")

### Import datasets
#iris dataset
iris <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data", header = FALSE, stringsAsFactors = FALSE)
names(iris) <- c("sepal_length", "sepal_width", "petal_length", "petal_width", "class")

#bike dataset
temp <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00275/Bike-Sharing-Dataset.zip", temp)
bike <- read.csv(unz(temp, "day.csv"), stringsAsFactors = FALSE)

### Graphing using base R ###

### Primary plotting functions
plot(iris[-length(iris)]) #scatterplot matrix of numeric fields #Example of primary plotting function
title("Scatterplot Matrix") #If you have a plot already and want to add a title, 
#you can use the title command #Example of secondary plotting function

##Plot function
#generic function for plotting R objects
plot(x = iris$sepal_length, y = iris$sepal_width) #normal usage
plot(sepal_length ~ sepal_width, data = iris) #formula usuage

#plot with extra params
plot(x = iris$sepal_length, y = iris$sepal_width, 
     main = "Sepal: Length Vs Width", xlab = "Sepal Length", ylab = "Sepal Width")

#plot using color argument
plot(x = iris$sepal_length, y = iris$sepal_width, 
     col = factor(iris$class))

##Strip chart
stripchart(iris$petal_length) #bare bone
stripchart(iris$petal_length, vertical=TRUE)

#With labels
stripchart(iris$petal_length, 
           main = "Strip Chart", xlab = "Petal length")

##Histograms
#generic function for plotting hstogram of given data
hist(iris$sepal_length) #bare minimum call

hist(iris$sepal_length, col = "grey", border = "blue") #colour params

#hist with extra params
hist(iris$sepal_length, 
     main = "Histogram: Sepal Length", 
     xlab = "Sepal Length",
     labels = TRUE)

##Boxplots
boxplot(iris$sepal_width) #Single column
boxplot(iris$sepal_length, iris$sepal_width, iris$petal_length, iris$petal_width) #Comparitve Boxplot - 4 columns

#plotting for goups using formula
boxplot(sepal_length ~ class, data = iris, 
        main = "Boxplot - Sepal Length with Class breakup")

##Line plot
bike$dteday <- as.Date(bike$dteday) #convert char date to Date object
plot(y = bike$cnt, x = bike$dteday, type = "l", 
     main = "Bike Sharing Demand", xlab = "Time", ylab = "Demand")


### Secondary plotting functions

##Adding line to existing plots - using abline() function

plot(x = iris$sepal_length, y = iris$sepal_width, 
     main = "Sepal: Length Vs Width", xlab = "Sepal Length", ylab = "Sepal Width")

#horizontal line representing Mean Sepal Width
abline(h = mean(iris$sepal_width), col = "green")

#Regression line fitting the points
abline(coef = coefficients(lm(sepal_width ~ sepal_length, data = iris)), col = "blue")

##Adding points to the plot - using points() function
plot(x = iris$sepal_length, y = iris$sepal_width, 
     main = "Sepal: Length Vs Width", xlab = "Sepal Length", ylab = "Sepal Width",
     ylim = c(0, 5))

points(x = iris$sepal_length, y = iris$petal_width, col = "blue", pch = 18)

##Adding text to the plot - using text() function
plot(x = iris$sepal_length, y = iris$sepal_width, 
     main = "Sepal: Length Vs Width", xlab = "Sepal Length", ylab = "Sepal Width",
     ylim = c(0, 5))

text(x = iris$sepal_length, y = iris$petal_width, pos = 1, cex = .6)

##Adding legend to the plot - using the legend() function
plot(x = iris$sepal_length, y = iris$sepal_width, 
     main = "Sepal: Length Vs Width", xlab = "Sepal Length", ylab = "Sepal Width",
     ylim = c(0, 5))

points(x = iris$sepal_length, y = iris$petal_width, pch = 18)

legend(x = 5, c("Sepal Width", "Petal Width"), pch= c(1, 18))

### Parameter settings
par(mfcol = c(1, 2))

#par(mfcol = c(1, 1))

### Saving graphs
#(PNG), (BMP), (ps) and (JPEG)

##Portable Network Graphics (PNG)
png("hist.png")
plot(x = iris$sepal_length, y = iris$sepal_width, 
     main = "Sepal: Length Vs Width", xlab = "Sepal Length", ylab = "Sepal Width")
dev.off()
















