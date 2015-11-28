
####UDACITY problem set 5 for submission to SR & Anirban

##Install and load packages and their dependencies (as needed)

install.packages("ggplot2", dependencies=TRUE)
install.packages("dplyr")
install.packages ("scales")
install.packages("xlsx")
install.packages("reshape2")
install.packages("lubridate")
install.packages ("ggthemes")
install.packages ("gridExtra")

library(ggplot2)
library(dplyr)
library(scales)
library(xlsx)
library(reshape2)
library(lubridate)
library(ggthemes)
library(gridExtra)

###Price Histograms with Facet and Color

library (ggplot2)
ggplot(diamonds, aes(x = price, fill = cut)) +
  geom_histogram() +
  facet_wrap(~ color) +
  scale_fill_brewer(type = 'qual') +
  scale_x_log10(labels=dollar, expression(paste(Log[10], " of Price"))) +
  ylab("Count")

##Price vs. Table Colored by Cut

ggplot(diamonds, aes(x = table, y = price, color = cut)) + 
  geom_jitter(size = 5) +
  scale_x_continuous(breaks = seq(50, 80, 2), limits = c(50, 80)) +
  scale_color_brewer(type = 'qual') +
  theme_minimal()

## 53 -57 is the table range for ideal cut diamonds
## 58 - 62 is the table range for premium cut diamonds

###Price vs. Volume and Diamond Clarity

library(dplyr)
library (scales)
  diamondsVol <- diamonds %>%
  mutate(volume=x*y*z)

  ggplot(diamondsVol, aes(x = volume, y = price, color = clarity)) + 
  geom_point() +
  scale_color_brewer(type = "div") +
  scale_x_continuous(limits = c(0, quantile(diamondsVol$volume, 0.99, na.rm = TRUE))) +
  scale_y_continuous(trans = "log10")
                     
 
### Proportion of Friendships Initiated
  
## Your task is to create a new variable called 'prop_initiated'
## in the Pseudo-Facebook data set. The variable should contain
## the proportion of friendships that the user initiated. 

  pf$prop_initiated <- 
    with (pf, ifelse(friend_count == 0,as.numeric(NA),  
    as.numeric(friendships_initiated) / as.numeric(friend_count)))
   
## Create a line graph of the median proportion of
## friendships initiated ('prop_initiated') vs.
## tenure and color the line segment by
# #  year_joined.bucket.

  pf$year_joined = floor(2014 - pf$tenure / 365)
  pf$year_joined.bucket = cut(pf$year_joined,c(2004,2009,2011,2012,2014))
 
  ggplot(aes(x = tenure, y = prop_initiated ), data = pf) +
  geom_line(aes(color=year_joined.bucket),stat = 'summary', fun.y=median)
  
  
### Smoothing prop_initiated vs. tenure  
  
  # Smooth the last plot you created of
  # of prop_initiated vs tenure colored by
  # year_joined.bucket. You can bin together ranges
  # of tenure or add a smoother to the plot.
  
    ggplot(aes(x = tenure, y = prop_initiated ), data = pf) +
    geom_line(linetype="dashed", size = 1, aes(color=year_joined.bucket),stat = 'summary', fun.y=median) +
    geom_smooth(method='lm', color='purple')
  
### Create a scatter plot of the price/carat ratio 
    
# of diamonds. The variable x should be
# assigned to cut. The points should be colored
# by diamond color, and the plot should be
# faceted by clarity.
# scale_color_brewer(type = 'div')   
    
    range(diamonds$carat)
    
    diamonds$price_per_carat <- diamonds$price/ diamonds$carat
    
    ggplot(aes(x = cut, y = price_per_carat), data = diamonds) +
    geom_point(aes(color=color)) 
    ggplot(aes(x = cut, y = price_per_carat), data = diamonds) +
    geom_point(aes(color=color)) +
    scale_color_brewer(type = 'div') +
    facet_wrap(~clarity)
    
    
### Your task is to continue the investigation you did at the end of Problem Set 4
# In your investigation, examine 3 or more variables and create 2-5 plots that make
# use of the techniques from Lesson 5.
    
##    Scatter Plot with Several Regression Lines
     
       ggplot(diamonds, aes(carat, price, group=color)) +
       geom_point(aes(color=color), size=2) +
       geom_smooth(aes(color=color), method = "lm", se=FALSE)
    
##     Jitter Plot 
       ggplot(diamonds, aes(color, price/carat)) +
       geom_jitter(alpha = I(1/2), aes(color=color))
       

       
##     Density Plot
       ggplot(diamonds, aes(carat)) + geom_density(aes(fill = color))

       
       
  

