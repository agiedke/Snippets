rm(list=ls())

library(datasets)
library(lattice)


data(mtcars)
df <- mtcars
rm(mtcars)

df <- transform(df, cyl = factor(cyl))

# calling xyplot
xyplot(mpg ~ wt | cyl, df)

# adjusting panel functions
# vectorize arguments
y <- df$mpg
x <- df$wt
f <- df$cyl
# 1) adjusting panel function to add horizontal lines at medain(y)
xyplot(y ~ x|f, panel = function(x, y, ...){
  panel.xyplot(x, y, ...)
  panel.abline(h = median(y), lty = 2)
})
# 2) adjusting panel function to add regression line
xyplot(y ~ x|f, panel = function(x, y, ...){
  panel.xyplot(x, y, ...)
  panel.lmline(x, y, col = 2)
})

# other plots
bwplot(df$mpg)
histogram(df$mpg)
stripplot(df$mpg)
dotplot(mpg~wt, df)
splom(~df)

