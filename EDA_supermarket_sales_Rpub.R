library(tidyverse)

# DATA CLEANING

df <- read.csv("D:/edubridge_R_progamming/predictive_analysis_in_r/supermarket_sales - Sheet1.csv")

df[order(df$Date),]

# To check the dimensions of our dataset

dim(df)

# summary stats of our datasets

summary(df)

# Converting the date to date format

library(lubridate)

df$Date <- parse_date_time(x = df$Date, orders = c("d m y", "d B Y", "m/d/y"), locale = 'eng')

df[order(df$Date),]

class(df$Date)

# CHECKING THE NULL VALUES

summary(is.na(df))

# We use the naniar package to get the knowledge of the missing values 

library(naniar)

gg_miss_var(df)

miss_var_summary(df)


# DATA ANALYSIS AND EXPLORATION

# We will start looking for the most correlated variables using the polychoric relation

library(magrittr)

library(dplyr)

my_data <- df %>% select(7,8,9,10,14,15,16,17)

# Correlations with significance levels 

library(polycor)

polychor(my_data)

head(my_data)

# We will use plotly dash to draw the best correlated variables.

# Round to two decimal places 

round_df <- function(x, digits){
  # round all numeric variables
  # x : dataframes
  # digits : number of digits to round
  
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <- round(x[numeric_columns], digits)
  x
}

cleaned_df <- round_df(my_data, 2)

cleaned_df


# The sales dataset

Sales_data <- as.matrix(cleaned_df)

# Plotting a heatmap for the cleaned data that we have worked on

if(!require("gplots")){
  install.packages('gplots', dependencies = TRUE)
  library(gplots)
}

if(!require("RcolorBrewer")){
  install.packages('RcolorBrewer', dependencies = TRUE)
  library(RColorBrewer)
}

# Reading in data and transforming it into matrix format

rname <- Sales_data[,1]

mat_data <- Sales_data

rownames(mat_data) <- rname

# Customizing and plotting the heat map.

# Creates an own color pattern from red to green.

my_palette <- colorRampPalette(c("red", "yellow","green"))(n = 299)

# (optional) defined a color range manually 

col_breaks <- c(seq(-1,0, length = 100),
                seq(0.01,0.8, length = 100),
                seq(0.81,1, length = 100))

# Creates a 5 * 5 inch image 

# png("./image/reda.png",     # Create a png for the heatmap
#     width = 5*300,         # 5 * 300 pixels
#     height = 5*300,       # 5 * 300 pixels
#     res = 300,           # 300 pixels per inch
#     pointsize = 8) # smaller font size


heatmap.2(mat_data,
          cellnote = mat_data,  # same dataset for cell labels
          main = "correlation",  # The title of the heatmap
          notecol = 'black',     # change fontcolors of cell labels to black
          denisty.info = 'none',  # turns off density plot inside the color legend
          trace = 'none',  # turns off trace lines inside the heatmap
          margins = c(12,9),  # widens margins around the plot
          col = my_palette,  # use our color palette defined earlier
          breaks = col_breaks,   # enables color transistion at specified limits
          dendrogram = "row",   # only draw a row dendrogram
          Colv = 'NA') # turn off the clustering

heatmap

head(df)

x<- df$Tax.5.

y <- df$gross.income

plot(x,y, col = 'red', main = "Tax 5% as a function of gross income.", xlab = "Tax 5%", ylab = "Gross income")


x <- df$Quantity

y <- df$cogs

plot(x,y, col = 'Dark Green', main = "Qauantity vs Cost of goods sale.", xlab = "Quantity", ylab = "Cost of good sales")

x <-  df$Unit.price

y <- df$gross.income

plot(x, y, col = "Blue", main="Unit Price Vs Gross Income", xlab="Unit Price", ylab="Gross Income")

Rating <- df$Rating

hist(Rating, col = 'Gold')

# Lets find the mean of rating

avg <- mean(Rating)

hist(Rating, col = 'Gold')

abline(v = avg, col = 'Blue', lwd = 2)  # abline introduces a line at the average of the plot

# Plotting histogram for all the columns

library(Hmisc)

hist.data.frame(cleaned_df)

# Analysis of branch, City, Product type

# Counting the branches

counts <- df %>% count(Branch)

counts

# Import ggplot2 package

library(ggplot2)

ggplot(data = df, aes(x = Branch)) + geom_bar(aes(fill = Branch)) + labs(title = "Branch Analysis", subtitle = "Which branch is the busiest ?", x = "Branches", y = "Counts")

#  Now lets check the payment methods

ggplot(data = df,aes(x = Payment)) + geom_bar(aes(fill = Payment)) + labs(title = "Payment Method analysis", subtitle = "Which payment method is most used?", x = "Payment methods", y = 'Counts')

# Print the results

summary <- count(df, Payment)

summary$Percentage <- summary$n / sum(summary$n) * 100

summary

#  Now checking the busiest cities

ggplot(data = df, aes(x = City)) +
  geom_bar(aes(fill = City)) + 
  labs(title = "Geo Analysis",
       subtitle = "Which city is most busy?.",
       x = "City",
       y = "Count")

# Print results

summary <- count(df, City)

summary$percentage <- summary$n / sum(summary$n)  * 100

summary


ggplot(data = df, aes(x = Gender)) + geom_bar(aes(fill = Gender)) + labs(title = "Gender analysis", subtitle = "Gender Count", x = "Gender", y = "Count")

# showcasing the results per gender

summary <- count(df, Gender)

summary$percentage <- summary$n / sum(summary$n) * 100

summary

# build gender based comparison for product type

ggplot() + geom_point(data = df, aes(x = Product.line, y = Unit.price, color = Gender)) 

#  Using countplot method to better showcase the gender plot

ggplot(df, aes(y=Product.line)) + geom_bar(aes(fill = Gender)) + labs(title = "Gender Analysis per Product Type", x = "Count", y = "Product Type")


ggplot(df, aes(y=Product.line)) + 
  geom_bar(aes(fill = Payment)) + 
  labs(title = "payment method for Product Type",
       x = "Count",
       y = "Product Type")

# And now we check the branches that mostly promote a specific type of product

ggplot(df, aes(y=Branch)) + 
  geom_bar(aes(fill = Payment)) + 
  labs(title = "payment method for Branch Type",
       x = "Count",
       y = "Branch")
# These cities we mostly use a specific type of payment

ggplot(df, aes(y=City)) + 
  geom_bar(aes(fill = Payment)) + 
  labs(title = "payment method for City Type",
       x = "Count",
       y = "City")

# Finding which branch has better sale for a particular product type

ggplot(data = df, aes(y = Product.line)) + 
  geom_bar(aes(fill = Branch)) + 
  labs(title = "Branches having better sales for a Product type",
       x = 'Count',
       y = "Product type")


# Boxen plot for rating and quantity

couleur <- c("gold","darkgreen","red", "pink", "yellow", "brown","orange","gray", "green", "silver")

boxplot(Rating~Quantity, data = df, main = "Boxen Plot for Quantity and rating", notch  = TRUE, xlab = "Quantity", ylab = "Rating")

# IMPLEMENTING THE WORDCLOUD TO SHOWCASE THE MOST SOLD PRODUCT

library(wordcloud)

library(wordcloud2)

library(tm)

# Create a vector containing only the text

text <- df$Product.line

# Create a corpus

docs <- Corpus(VectorSource(text))

dtm <- TermDocumentMatrix(docs)

matrix <- as.matrix(dtm)

words <- sort(rowSums(matrix),decreasing=TRUE) 

df <- data.frame(word = names(words),freq=words)

wordcloud2(data = df, size = 1.6, color = 'random-dark')

set.seed(1234)

wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words =  200,
          random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))
