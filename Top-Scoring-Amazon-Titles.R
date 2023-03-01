# This analysis aims to create a score for the top 50 Amazon books from 2009 to 2019. Overall, scoring is done based on: 
# * No of Amazon Reviews
# * Amazon User Ratings
# * How recent (based on the year) the book got on the best selling list
# * The number of years the book got on the best selling list


# Import packages and load in the data.
library(tidyverse)
library(ggplot2)

df <- read.csv("../input/amazon-top-50-bestselling-books-2009-2019/bestsellers with categories.csv")
head(df)

# # Exploratory Analysis
# Data consists of 351 different book titles over a period from 2009 to 2019. A book title can appear on the the best selling list in more than 1 year.


length(unique(df$Name))
summary(df)


# Now let's see if we can get a sense of any correlation between user ratings and reviews.

df1 <- df %>%
  group_by(Name) %>%
  summarise(
    avg_rating = mean(User.Rating),                 
    avg_review = mean(Reviews)
  )

p <- ggplot(df1, aes(x=avg_rating, y=avg_review)) + geom_bin2d() 
newdat <- ggplot_build(p)$data[[1]]
p + geom_text(data=newdat, aes((xmin + xmax)/2, (ymin + ymax)/2, label=count), col="white")


# From the above, we can see that: 
# * Most of the titles with high ratings tend to have less reviews.
# * Both ratings and reviews need to be taken into account in the overall scoring.


# Data Preprocessing
# * As a book title can be on the best selling list for more than a year, we also want to take into account how recent it has been on the list. The more recent the book is on the best selling list, the higher the score.
# * We also want to take into account the number of years (frequency) the title has been on the best selling list.
# * Feature scaling: Data adjusted to a max of 10.
# * Overall score is a range from 0 to 5.

df$Recency <- df$Year - min(df$Year)
scores <- df %>%
  group_by(Name) %>%
  summarise(
    Frequency_score = length(unique(Year)),                 
    Ratings_score = mean(User.Rating),
    Ratings_score = Ratings_score/max(df$User.Rating)*10,
    Recency_score = mean(Recency),
    Reviews_score = mean(Reviews),
    Reviews_score = mean(Reviews)/max(df$Reviews)*10
      )

scores$Overall_score = (scores$Frequency_score+scores$Ratings_score+scores$Recency_score+scores$Reviews_score)/40*5


# Let's look at the top 5 titles sorted by reviews
# * 'Where the Crawdads Sing' has the highest reviews, but it has only been on the best selling list for 1 year in 2019. Recency score is 10, as this is the most recent year in the data.
# * 'The Girl on the Train', on the other hand, has slightly less reviews, but has been on the best selling list for 2 years (2015 and 2016). As 2015 and 2016 has been some time back, recency score is [(2015-2009)+(2016-2009)]/2 = 6.5.


scores[order(-scores$Reviews_score),][1:5,]


# Let's look at the top 5 titles sorted by ratings
# * We see that there are many books with an assigned ratings score of 10 (they have the same max rating of 4.9). However, in line with what we saw earlier, highly rated books tend to have relatively fewer reviews.


Max_rating = scores[scores$Ratings_score == 10,]
Max_rating[order(-Max_rating$Reviews_score),][1:5,]


# Here's the top 5 titles sorted by frequency. These are the books that have been best selling for the most number of years

scores[order(-scores$Frequency_score),][1:5,]

# Taking all the following factors into consideration, these are the top 10 scoring books on Amazon:
# * Frequency
# * Ratings
# * Recency
# * Reviews

scores[order(-scores$Overall_score),][1:10,]

