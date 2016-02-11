author <- list(name = "Shoaib Najeeb Arayilakath", email = "shoaibnajeeb@gmail.com")

### Import datasets
#iris dataset
iris <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data", header = FALSE, stringsAsFactors = FALSE)
names(iris) <- c("sepal_length", "sepal_width", "petal_length", "petal_width", "class")

#bike dataset
temp <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00275/Bike-Sharing-Dataset.zip", temp)
bike <- read.csv(unz(temp, "day.csv"), stringsAsFactors = FALSE)

#data prep - convert to factors
iris$class <- factor(iris$class)

bike$dteday <- as.Date(bike$dteday)
bike$weathersit <- factor(bike$weathersit)
bike$holiday <- factor(bike$holiday)
bike$season <- factor(bike$season)


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
     col = iris$class)

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

hist(iris$sepal_length, breaks = seq(from = 2, to = 9, by = 0.5)) #using break parameter to change the historgram

hist(iris$sepal_length, freq = FALSE) #plotting probabilities instead of frequency

#hist with extra params
hist(iris$sepal_length, 
     main = "Histogram: Sepal Length", 
     xlab = "Sepal Length",
     labels = TRUE)

attributes(hist(iris$sepal_length)) #attributes for histogram
hist(iris$sepal_length)$counts #counts

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

##Adding a curve to the plot - using the curve() function
hist(iris$sepal_length, density = 20, 
     probability = TRUE, ylim = c(0,1), xlim = c(3,9),
     main = "Histogram: Sepal Length", 
     xlab = "Sepal Length")

curve(dnorm(x, mean = mean(iris$sepal_length), sd = sd(iris$sepal_length)), y = 3, to = 9,
      col = "red", lwd = 2, add = TRUE)

##Adding secondary axis
par(mar = c(5,5,2,5))
with(bike, plot(x = dteday, y = cnt, type = "l", col="red",  
             ylab = "Sepal Width", xlab = "Date"))

par(new=TRUE)
with(bike, plot(x = dteday, y = temp, pch = 16, cex = 0.6,
                axes = FALSE, xlab = NA, ylab = NA))
axis(side=4)
mtext(side = 4, line = 3, "Temperature Recorded")
legend("topleft",
       legend = c("Count", "Temp"),
       lty = c(1,0), pch = c(NA, 16), col = c("red", "black"))

### Parameter settings
par(mfcol = c(2, 2))

#par(mfcol = c(1, 1))

### Saving graphs
#(PNG), (BMP), (ps) and (JPEG)

##Portable Network Graphics (PNG)
png("hist.png")
plot(x = iris$sepal_length, y = iris$sepal_width, 
     main = "Sepal: Length Vs Width", xlab = "Sepal Length", ylab = "Sepal Width")
dev.off()

#----#



### Graphing using ggplot2 ###
#require(ggplot2)

### qplot function
##Line plot
qplot(x = dteday, y = cnt, data = bike, 
      color = weathersit, geom = "line")

qplot(x = dteday, y = cnt, data = bike, 
      color = weathersit) + geom_line()

##Scatter plot
qplot(x = dteday, y = cnt, data = bike) #base call

qplot(x = dteday, y = cnt, data = bike, 
      color = weathersit, geom = "point")

qplot(y = cnt, x = dteday, geom = "point", data = bike,
      color = weathersit, shape = holiday)

qplot(x = dteday, y = cnt, data = bike, #smoother
      color = weathersit, geom = "smooth", method = "lm")

##Boxplot
qplot(x = class, y = sepal_length, formula = y ~ x, 
      data = iris, geom = "boxplot")

##Histogram
qplot(bike$cnt, geom = "histogram", binwidth = 500, xlim = c(0, 9000))

qplot(bike$cnt, geom = "histogram", breaks = seq(0, 9000, by = 500), 
      xlim = c(0, 9000))

##Density plot
qplot(casual, geom = "density", data = bike,
      colour = weathersit)

#using facets
qplot(cnt, geom = "density", data = bike, 
      facets = weathersit~.)

qplot(cnt, geom = "density", data = bike, 
      facets = .~weathersit)

### ggplot function

##Scatter plot
ggplot(data = bike) + geom_point(aes(x = hum, y = cnt)) #bare bone call

#multi-layered plots
p <- ggplot(data = bike, aes(x = hum, y = cnt))
p + geom_point()
p + geom_point(aes(color = season)) #colour aesthetics
p + geom_point(aes(shape = holiday)) #shape aesthetics

#set colours for points
p + geom_point(color = "green4")
p + geom_point(aes(color = season), shape = 5) + scale_color_manual(values = c("blue", "red", 
                                                                               "yellow", "green"))

#set shape for points
p + geom_point(shape = 4)
p + geom_point(aes(shape = holiday)) + scale_shape_manual(values = c(1, 4))

#scale axis limits
p + geom_point() + scale_x_continuous(limits = c(-1,1))

#set the legend
p + geom_point(aes(color = holiday)) + theme(legend.position = "top")

p + geom_point(aes(color = holiday)) + theme(legend.position = "none") #remove legend

##Histogram
ggplot(data = bike, aes(x = casual)) + geom_histogram() #bare bone

ggplot(data = bike) + geom_histogram(aes(x = casual), binwidth = 100, 
                                     fill = "grey", color = "black")

ggplot(data = bike, aes(x = casual, fill = weathersit)) + geom_histogram()

##Density plot
ggplot(data = bike) + geom_density(aes(x = casual), fill = "grey")

ggplot(data = bike, aes(x = casual)) + geom_line(stat = "density")

ggplot(data = bike, aes(x = casual, y = ..density..)) + 
  geom_histogram(binwidth = 100, fill = "grey", color = "black") + 
  geom_line(stat = "density", color = "blue3")

##Boxplot
ggplot(data = bike, aes(y = cnt, x = 1)) + geom_boxplot() #bare bone

ggplot(data = bike, aes(y = cnt, x = holiday)) + geom_boxplot() + coord_flip()

ggplot(data = bike, aes(y = cnt, x = holiday)) + geom_boxplot(fill = "lightblue") #outlier.size/shape

##Violin plot
ggplot(data = bike, aes(y = cnt, x = season)) + geom_violin()

p <- ggplot(data = bike, aes(y = cnt, x = factor(season)))
p + geom_violin()
p + geom_violin() + geom_point()

##Line plot
ggplot(data = bike, aes(y = cnt, x = dteday)) + geom_line()

bike$dteyear <- format(bike$dteday, "%Y")
bike$dtemonth <- format(bike$dteday, "%b")

p <- ggplot(data = bike, aes(y = cnt, x = dtemonth))
p + geom_line(aes(color = dteyear, group = dteyear))

##Add lines to the plots
p <- ggplot(data = bike, aes(x = windspeed, y = casual))
p + geom_point() + geom_vline(xintercept = 0.25, color = "red")
p + geom_point() + geom_hline(yintercept = 1500, color = "green")
cf <- coefficients(lm(casual ~ windspeed, data = bike))
p + geom_point() + geom_abline(intercept = cf[["(Intercept)"]], 
                               slope = cf[["windspeed"]], 
                               color = "blue3", linetype = "dashed")

##Manipulate text in the plots
p <- ggplot(data = bike, aes(x = windspeed, y = casual))

#add title
p + geom_point() + ggtitle("Casual Vs Windspeed")
p + geom_point() + labs(title = "Rented Casual Vs Windspeed")

#change x and y labels
p + geom_point() + xlab("Wind Speed") + ylab("Rented Casual")

p + geom_point() + theme(axis.title = element_text(size = 10, lineheight = .9, 
                                                   color = "green")) #formatting axis labels

#insert text in the plot with annotate
bike[bike$windspeed > 0.5, c("casual", "windspeed")]
p + geom_point() + annotate("text", x = 0.507463, y = 532, 
                            label = "Scatter", size = 2, color = "blue2")

#insert text labels with geom_text
p + geom_point() + geom_text(aes(label = windspeed), size = 2, color = "blue1")






#----#





























