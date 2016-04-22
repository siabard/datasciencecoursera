#### Week 2

### Lattice Plotting System in R

## lattice graphic system : xyplot, bwplot, levelplot

# xyplot

# f , g : conditional variables
# data : data frame
#xypot( y ~ x | f * g, data)

library(lattice)
library(datasets)
xyplot(Ozone ~ Wind, data = airquality)

airquality <- transform(airquality, Month = factor(Month))
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5,1))

## Lattice graphis functions return an object of class trellis
## print methods for lattice forcuntions actually do the work of plotting the data on the graphics device.
## lattice functions return "plot objects" that can, in principle, be stored.
## On the command line trellis objects are auto-printed

p <- xyplot(Ozone ~ Wind, data = airquality)
print(p)

### panel functions
## lattice functions have a panel function
## panel functions receive the x/y coordinate of the data points in their panel

set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each = 50)
y <- x + f - f * x + rnorm(100, sd = 0.5)
f <- factor(f, labels = c("Group 1", "Group 2"))
xyplot(y ~ x | f, layout = c(2,1))

xyplot(y ~ x | f, panel = function(x, y, ...) {
  panel.xyplot(x, y, ...)
  panel.abline(h = median(y), lty=2)
  panel.lmline(x, y, col = 2)
})



# bwplot
# histrogram
# stripplot
# dotplot
# splom
# levelplot, contouplot

### ggplot2

## implementation Grammar of Graphics 
## thrid graphics system

## Grammar of graphics represents and abstra

# Qplot Swirl

str(mpg)

qplot(displ, hwy, data = mpg)

# aesthtic
qplot(displ, hwy, data = mpg, color = drv)

## add geom

qplot(displ, hwy, data = mpg, color = drv, geom = c("point", "smooth"))

# test qplot
qplot( y = hwy, data = mpg, color = drv)


# box and whisker plot
qplot(drv, hwy, data = mpg, geom = "boxplot")
qplot(drv, hwy, data = mpg, geom = "boxplot", color=manufacturer)

# histogram
qplot(hwy, data = mpg, fill = drv)


# subplots
qplot(displ, hwy, data = mpg, facets = . ~ drv)
qplot(hwy, data = mpg, facets = drv ~ ., binwidth = 2)


## GGPlot Part 2
qplot(displ, hwy, data = mpg, geom = c("point", "smooth"), facets = .~drv)

g <- ggplot(mpg, aes(displ, hwy))

summary(g)

# ggplot doesn't know how to display.
g + geom_point()

# smooth layer

g + geom_point() + geom_smooth()

# Linear regression
g + geom_point() + geom_smooth(method = "lm")

## Facesets
g + geom_point() + geom_smooth(method = "lm") + facet_grid(.~drv)

## Labels
g + geom_point() + geom_smooth(method = "lm") + facet_grid(.~drv) + ggtitle("Swirl Rules!")

# Modifying aesthetics
g + geom_point(color="pink", size=4, alpha=1/2)
g + geom_point(size=4, alpha=1/2, aes(color=drv))
g + geom_point(aes(color=drv)) + labs(title="Swirl Rules!") + labs(x = "Displacement", y = "Hwy Mileage")
g + geom_point(aes(color = drv), size = 2, alpha=1/2) + geom_smooth(size=4, linetype=3, method="lm", se = FALSE)

# Themes

g + geom_point(aes(color = drv)) + theme_bw(base_family = "Times")

# Layered 
plot(myx, myy, type="l", ylim=c(-3,3))
g <- ggplot(testdat, aes(myx, myy))
g + geom_line()
g + geom_line() + ylim(-3, 3)
g + geom_line() + coord_cartesian(ylim = c(-3, 3))

# Complex one : with mpg
g <- ggplot(mpg, aes(x = displ, y = hwy, color=factor(year)))
g + geom_point()
g + geom_point() + facet_grid(drv ~ cyl, margins = TRUE)
g + geom_point() + facet_grid(drv ~ cyl, margins = TRUE) + geom_smooth(method = "lm", se = FALSE, size=2, color="black")
g + geom_point() + 
  facet_grid(drv ~ cyl, margins = TRUE) +
  geom_smooth(method = "lm", se = FALSE, size=2, color="black") +
  labs(x = "Displacement", y = "Highway Mileage", title="Swirl Rules!")


### GGPlot2 extras
str(diamonds)
qplot(price, data = diamonds)

range(diamonds$price)

qplot(price, data = diamonds, binwidth = 18497 / 30)
qplot(price, data = diamonds, binwidth = 18497 / 30, fill = cut)
qplot(price, data = diamonds, geom = "density")

qplot(price, data = diamonds, geom = "density", color = cut)

## Scatter plot
qplot(carat, price, data = diamonds)
qplot(carat, price, data = diamonds, shape = cut)
qplot(carat, price, data = diamonds, color = cut)

qplot(carat, price, data = diamonds, color = cut) + geom_smooth(method = "lm")
qplot(carat, price, data = diamonds, color = cut, facets=.~cut) + geom_smooth(method = "lm")

## GGPlot
g <- ggplot(diamonds, aes(depth, price))
summary(g)
g + geom_point(alpha = 1/3)
cutpoints <- quantile(diamonds$carat, seq(0, 1, length=4), na.rm = TRUE)
diamonds$car2 <- cut(diamonds$carat, cutpoints)

g <- ggplot(diamonds, aes(depth, price))
g + geom_point(alpha = 1/3) + facet_grid(cut ~ car2)
diamonds[myd,]
g + geom_point(alpha = 1/3) + facet_grid(cut ~ car2) + geom_smooth(method = "lm", size = 3, color="pink")

ggplot(diamonds, aes(carat, price)) + geom_boxplot() + facet_grid(. ~ cut)