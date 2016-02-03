
###UDACITY problem set 3 for submission to SR & Anirban


# Investigate the price per carat of diamonds across
# the different colors of diamonds using boxplots.
# Save images by using the ggsave() command.
# ggsave() will save the last plot created.
# For example...
#                  qplot(x = price, data = diamonds)
#                  ggsave('priceHistogram.png')



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

####For Reference only
#carat cut    color  clarity depth table price x     y    z
#0.2   Ideal   E      SI2    61.5  55.0  326   3.95 3.98 2.43
#0.2   Premium E      SI1    59.8  61.0  326   3.89 3.84 2.31
#0.2   Good    E      VS1    56.9  65.0  327   4.05 4.07 2.31
#0.3   Premium I      VS2    62.4  58.0  334   4.20 4.23 2.63
#0.3   Good    J      SI2    63.3  58.0  335   4.34 4.35 2.75
#0.2  VeryGood J      VVS2   62.8  57.0  336   3.94 3.96 2.48



##checking and setting working directory, listing files, checking datasets available
getwd()
setwd("C:/Users/tigre/Documents")
list.files()
data()

##checking basics of diamonds dataset
library(ggplot2)
library(dplyr)
str(diamonds)
glimpse(diamonds)
head(diamonds)
str(diamonds$price)



##box plot diamonds by price per carat
library(scales)
library(ggthemes)

##Submission 3
ggplot(diamonds, aes(x = color, y = price/carat, fill = color)) +
  geom_boxplot() +
  coord_cartesian(ylim=c(1000, 6000)) +
  scale_y_continuous(labels = dollar) + 
  xlab("Colour") + ylab("Price per Carat")

getwd()

##save to C:/Users/tigre/Documents
ggsave('stefanipriceboxplot.png')




###UDACITY problem set 3 for submission to SR & Anirban

## Price vs. X

ggplot(diamonds, aes(x = x, y = price)) +
  geom_point(alpha = 1/20) +
  coord_cartesian(xlim=c(3.5, 12)) + 
  scale_y_continuous(breaks=seq(1000, 19000, 2000), labels = dollar)

## Correlation between price and x, y, z
## There is a high correlation between price and all three variables
cor.test(diamonds$price, diamonds$x)
cor.test(diamonds$price, diamonds$y)
cor.test(diamonds$price, diamonds$z)

##Price vs. depth 
ggplot(diamonds, aes(x = depth, y = price)) + geom_point(alpha = 1/20)

## Price vs. depth w adjustments in transparency 

names(diamonds)
max(diamonds$depth)
min(diamonds$depth)

ggplot(data = diamonds, aes(x = depth, y = price)) + 
  geom_point(alpha = 1/100) + 
  scale_x_continuous(breaks = seq(min(diamonds$depth), max(diamonds$depth), 2),
                     labels = seq(min(diamonds$depth), max(diamonds$depth), 2))
 
## depth range falls within 58 and 64 based on scatterplot

## Is there a correlation between Depth vs. Price? No. it has a negative correlation
cor.test(diamonds$price, diamonds$depth)

## Price vs. Carat, omit top 1%
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(alpha=1/20) +
  scale_x_continuous(limits=c(0, quantile(diamonds$carat, 0.99))) +
  scale_y_continuous(breaks=seq(0, 18000, 2000), 
                     limits=c(0 , quantile(diamonds$price, 0.99)),
                     labels=dollar)


## Price vs. volume. There is a Very High Correlation as expected
diamondsVol <- diamonds %>%
  mutate(volume=x*y*z)

ggplot(diamondsVol, aes(x = volume, y = price)) + 
  geom_point()

cor.test(diamonds$price,diamondsVol$volume)

#Exclude Diamonds w volume of 0 or greater than 800. There is still a high correlation to price vs. volume

min(diamondsVol$volume)
max(diamondsVol$volume)

with(diamondsVol[diamondsVol$volume > 0 & diamondsVol$volume < 800, ], cor(volume, price))
## or with(subset(diamondsVol, !(volume == 0 | volume >= 800) ), cor.test(price, volume))#


##Mean price by Clarity


library (dplyr)
diamondsByClarity<- diamonds %>%
  group_by(clarity) %>%
  summarise(mean_price = mean(price),
            median_price = median(price),
            min_price = min(price),
            max_price = max(price),
            n = n() ) %>%
  arrange(clarity)


## Bar charts of mean price
library(gridExtra)
diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))


p1 <- ggplot(diamonds_mp_by_clarity, aes(x=clarity, y=mean_price, fill=clarity)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_brewer(palette="Set3") + 
  guides(fill = guide_legend(ncol=2, title.hjust=0.3))


p2 <- ggplot(diamonds_mp_by_color, aes(x=color, y=mean_price, fill=color)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_brewer(palette="Set2") + 
  guides(fill = guide_legend(ncol=2, title.hjust=0.4))

grid.arrange(p1,p2)

###UDACITY problem set 4 for submission to SR & Anirban, continued w Diamonds dataset.
# In your investigation, examine pairs of variables and create 2-5 plots that make
# use of the techniques from Lesson 4 above.


## Histogram of price per caret

ggplot(diamonds, aes(x = price/carat)) + 
  geom_histogram(color = "black", fill = "DarkBlue", binwidth = .05) + 
  theme(axis.text.x = element_text(angle = 45)) +
  scale_x_log10(expression(paste(Log[10], " of Price")),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  facet_grid(cut~., scale = "free") + ylab("Count")

## Histogram of price range of diamonds in dataset

ggplot(diamonds, aes(x = price)) + 
  geom_histogram(color = "black", fill = "ForestGreen", binwidth = 500) + 
  scale_x_continuous(labels = dollar, breaks = seq(0, 20000, 1000)) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Price") + ylab("Count")
