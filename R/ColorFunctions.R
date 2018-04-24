# Color Functions

# load packages
library(grDevices)
library(RColorBrewer)
library(ggplot2)

# available colors
colors()

# using colorRamp function
pal <- colorRamp(c("navyblue", "maroon"))
pal(0)
pal(.5)
pal(1)

# using colorRampPalette function
pal <- colorRampPalette(c("navyblue", "maroon")) # colorRampPalette returns function
pal(1) # pal(n) returns hexadecimal vector of length n
pal(3)
pal(10)
g<-ggplot(mpg, aes(x=displ, y=hwy, color=drv)) + geom_point()
g # default colors
g + scale_color_manual(values=pal(3)) # changing colors using colroRampPalette function
g + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) # changning colors manually

# using RcolorBrewer function
g + scale_color_brewer(palette="Dark2") # changing colors using RcolorBrewer

# combining RColorBrewer and colorRampPalette function
cols <- brewer.pal(3, "BuGn")
cols
pal <- colorRampPalette(cols)
image(volcano)
image(volcano, col = pal(2))

# other ggplot functions
g + scale_color_hue(l=40, c=35) # changing intensity of the color
g + scale_color_grey() + theme_classic() # using gray colors
