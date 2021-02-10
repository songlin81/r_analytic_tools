##################### Pie Charts ########################

x <- c(21, 62, 10, 53)
labels <- c("London", "New York", "Singapore", "Mumbai")
png(file = "city.png")
pie(x,labels)
dev.off()

x <- c(21, 62, 10, 53)
labels <- c("London", "New York", "Singapore", "Mumbai")
png(file = "city_title_colours.jpg")
pie(x, labels, main = "City pie chart", col = rainbow(length(x)))
dev.off()

x <-  c(21, 62, 10,53)
labels <-  c("London","New York","Singapore","Mumbai")
piepercent<- round(100*x/sum(x), 1)
png(file = "city_percentage_legends.jpg")
pie(x, labels = piepercent, main = "City pie chart",col = rainbow(length(x)))
legend("topright", c("London","New York","Singapore","Mumbai"), cex = 0.8,
       fill = rainbow(length(x)))
dev.off()

install.packages("plotrix")
library(plotrix)
x <-  c(21, 62, 10,53)
lbl <-  c("London","New York","Singapore","Mumbai")
png(file = "3d_pie_chart.jpg")
pie3D(x,labels = lbl,explode = 0.1, main = "Pie Chart of Countries ")
dev.off()

##################### Bar Charts ########################

H <- c(7,12,28,3,41)
png(file = "barchart.png")
barplot(H)
dev.off()

H <- c(7,12,28,3,41)
M <- c("Mar","Apr","May","Jun","Jul")
png(file = "barchart_months_revenue.png")
barplot(H,names.arg=M,xlab="Month",ylab="Revenue",col="blue",
        main="Revenue chart",border="red")
dev.off()

colors = c("green","orange","brown")
months <- c("Mar","Apr","May","Jun","Jul")
regions <- c("East","West","North")
Values <- matrix(c(2,9,3,11,9,4,8,7,3,12,5,2,8,10,11), nrow = 3, ncol = 5, byrow = TRUE)
png(file = "barchart_stacked.png")
barplot(Values, main = "total revenue", names.arg = months, xlab = "month", ylab = "revenue", col = colors)
legend("topleft", regions, cex = 1.3, fill = colors)
dev.off()

##################### Boxplots ########################

input <- mtcars[,c('mpg','cyl')]
print(head(input))

png(file = "boxplot.png")
boxplot(mpg ~ cyl, data = mtcars, xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon", main = "Mileage Data")
dev.off()

png(file = "boxplot_with_notch.png")
boxplot(mpg ~ cyl, data = mtcars, 
        xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon", 
        main = "Mileage Data",
        notch = TRUE, 
        varwidth = TRUE, 
        col = c("green","yellow","purple"),
        names = c("High","Medium","Low")
)
dev.off()

##################### Histograms ########################

v <-  c(9,13,21,8,36,22,12,41,31,33,19)
png(file = "histogram.png")
hist(v,xlab = "Weight",col = "yellow",border = "blue")
dev.off()

v <- c(9,13,21,8,36,22,12,41,31,33,19)
png(file = "histogram_lim_breaks.png")
hist(v,xlab = "Weight",col = "green",border = "red", xlim = c(0,40), ylim = c(0,5),
     breaks = 5)
dev.off()

##################### Line Graphs ########################

v <- c(7,12,28,3,41)
png(file = "line_chart.jpg")
plot(v,type = "o")
dev.off()

v <- c(7,12,28,3,41)
png(file = "line_chart_label_colored.jpg")
plot(v,type = "o", col = "red", xlab = "Month", ylab = "Rain fall",
     main = "Rain fall chart")
dev.off()

v <- c(7,12,28,3,41)
t <- c(14,7,6,19,3)
png(file = "line_chart_2_lines.jpg")
plot(v,type = "o",col = "red", xlab = "Month", ylab = "Rain fall", 
     main = "Rain fall chart")
lines(t, type = "o", col = "blue")
dev.off()

##################### Scatterplots ########################

input <- mtcars[,c('wt','mpg')]
print(head(input))

png(file = "scatterplot.png")
plot(x = input$wt,y = input$mpg,
     xlab = "Weight",
     ylab = "Milage",
     xlim = c(2.5,5),
     ylim = c(15,30),		 
     main = "Weight vs Milage"
)
dev.off()

png(file = "scatterplot_matrices.png")
pairs(~wt+mpg+disp+cyl,data = mtcars,
      main = "Scatterplot Matrix")
dev.off()

