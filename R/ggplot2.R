rm(list=ls())

library(ggplot2)
library(extrafont) # loading more fonts

df <- as.data.frame(unclass(mpg))

str(df)

# qplot
qplot(displ, hwy, data = mpg) # scatterplot hwy against displ
qplot(displ, hwy, data = mpg, color = drv) # color by drv
qplot(displ, hwy, data = mpg, shape = drv) # shape by drv
qplot(displ, hwy, data = mpg, geom = c("point", "smooth")) # adding smooth geom
qplot(hwy, data = mpg) # histogram
qplot(hwy, data = mpg, fill = drv) # color by drv
qplot(displ, hwy, data = mpg, facets = .~drv) # using facets to create scatterplot for each drv level
qplot(hwy, data = mpg, facets = drv~., binwidth = 2) # using facets to create histogram for each drv level
qplot(hwy, data = mpg, geom="density") # density plot
qplot(hwy, data = mpg, geom="density", color=drv) # density plot for each level of drv
qplot(displ, hwy, data = mpg, color = drv) + 
  geom_smooth(method="lm")# add overlayed regression lines for each level of drv
qplot(displ, hwy, data = mpg, facets = .~drv) + 
  geom_smooth(method="lm")# add seperate regression lines for each level of drv
qplot(displ, hwy, data = mpg, facets = .~drv, geom = c("point", "smooth"), method = "lm") # same plot but called in one function

# building up plot layer by layer
g <- ggplot(df, aes(displ, hwy))
p <- g + geom_point() + geom_smooth(method = "lm") # scatterplot with regression line
print(p)
g + geom_point() + facet_grid(. ~ drv) + geom_smooth(method = "lm") # scatterplot with regression line for each drv level
g + geom_point() + facet_grid(. ~ drv) + geom_smooth(method = "lm") + theme_bw() # change background to black and white
g + geom_point() + facet_grid(class ~ drv) + geom_smooth(method = "lm") # scatterplot with regression line for each combination of class and drv level

# changing aesthetics
g + geom_point(color = "steelblue", size = 4, alpha = 1/2) # changed color, size and transparency (alpha)
g + geom_point(aes(color = drv), size = 4, alpha = 1/2) # color by drv

# modifying labels
g + geom_point(aes(color = drv), size = 4, alpha = 1/2) + 
  labs(title="Scatterplot hwy agains displ") +
  labs(x = expression("prescript " * Displacement[subscript]), y = "HWY") +
  theme(plot.title = element_text(hjust = 0.5)) # added title, axes labels (incluidng subscripts), and centered title

# customizing the smooth
g + geom_point(aes(color = drv), size = 4, alpha = 1/2) + 
  geom_smooth(linetype = "dotted", size = 3, method = "lm", se = FALSE) # dotted, thicker line without confidence intervals (se = F)

# changing theme and fond
# windowsFonts() # determining available fonts
g + geom_point(aes(color = drv), size = 4, alpha = 1/2) + 
  theme_bw(base_family = "Times New Roman") # changing theme to black and white and font to times

# axis limits
g + geom_point(aes(color = drv), size = 4, alpha = 1/2) + ylim(0, 40) # outliers above 40 are omitted
g + geom_point(aes(color = drv), size = 4, alpha = 1/2) + coord_cartesian(ylim= c(0, 40)) # outliers above 40 are not omitted (difference is more apparent when using line plots)

# boxplot
ggplot(df, aes(x="", y=hwy)) + 
  geom_boxplot(color = "magenta", fill = "pink") + 
  labs(x="") +
  labs(y="HWY") + 
  theme_bw(base_family = "Calibri") +
  theme(axis.ticks = element_blank()) # single continuous variable
ggplot(df, aes(class, hwy)) + 
  geom_boxplot(color = "magenta", fill = "pink") + 
  theme_bw(base_family = "Calibri") # continuous & factor variable 
ggplot(df, aes(class, hwy, fill = class)) + 
  geom_boxplot() + 
  labs(x="") + 
  theme_bw(base_family = "Calibri") # continuous & factor variable, colored by factor
ggplot(df, aes(class, hwy, fill = class)) + 
  geom_boxplot() + 
  labs(x="") +
  coord_flip() +
  theme_bw(base_family = "Calibri") # flipped coordinates