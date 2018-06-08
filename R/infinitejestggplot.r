#infinitejestggplot.r
#Author: Ciera Martinez
#Plot character occurance by postion in book.
#There are two dataframes used to make this visualization, 
#1. Chapter Position (used to delinate chapter structuring)
#2. Character Postion 

#Required Libraries
library(ggplot2)
library(dplyr)
library(scales)
library(data.table)
library(tidyverse)

#1. Chapter Position
# read in ch.parser.py output
chapPos <- read_csv("../data/pyOutputs/chapterPosition.csv")
tail(chapPos)
dim(chapPos)
chapPos[65,1] <- "endnotes" #rename endnotes (last row)

#need to clean up position
chapPos$chapter <- gsub("<", "", chapPos$chapter)
chapPos$chapter <- gsub(">", "", chapPos$chapter)
chapPos$chapter <- gsub(" ", "", chapPos$chapter)

# to get chapter range
# make new vector independently of chapPos dataframe

#essentially removing first instance in vector and replacing last
#instance with NA, so row #'s stays the same. (to-do re-do with loop)
pos2 <- (chapPos$position - 1)
pos2 <- pos2[-1] 
pos2[65] <- NA #add a NA at the end so vector is length of chapPos rows
chapPos$position2 <- pos2 #bring back

#2. Character Postion 
#To get characterPosition.txt I ran ch.parser.py
charPos <- read_csv("../data/pyOutputs/characterPosition.csv")

#first deal with endnotes
chapPos[65,3] <- max(charPos$position)

#length column
chapPos$length <- (chapPos$position2 - chapPos$position)

#Now add a chapter column to specify where is each character position
charPos <- transform(charPos, 
                 chapter = chapPos$chapter[
                   findInterval(position, 
                                chapPos$position)])


## data summary
str(charPos)
dim(charPos)

#########################
## Distance matrix
## Using only charPos
###Part 1 playing
############################
by_chapter <- charPos %>% 
  group_by(term, chapter) %>%
  count() %>%
  spread(., chapter, n) %>% 
  mutate_if(is.integer, replace_na, replace = 0)

## Number of times a character is mention
charPos %>% 
  group_by(chapter, term) %>%
  count() %>%
  ggplot(., aes(chapter, n)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

############################
## Building edge lists
###########################

df <- charPos %>% 
  group_by(chapter, term) %>%
  select(-one_of("position")) %>%
  distinct() 

## All this to get groups of terms
df$ID <- 1:nrow(df)  #unique variable

## Clunky weird part that works but looks ugly
lst <- df %>%
  spread(chapter,term) %>%
  select(-ID) %>% 
  as.list() 

lst <- lapply(lst,function(x)x[!is.na(x)])

lst <- lst %>%
  lapply(function(x) {
    expand.grid(x, x, w = 1, stringsAsFactors = FALSE)}) %>%
    bind_rows



## Find co-occurnace

lst <- apply(lst[, -3], 1, str_sort) %>%
  t %>%
  data.frame(stringsAsFactors = FALSE) %>%
  mutate(w = lst$w)

e <- group_by(lst, X1, X2) %>%
  summarise(w = sum(w)) %>%
  filter(X1 == X2)



#Visualization 1
#Quickly just get a sense for how many times a character is mentioned

ggplot(charPos, aes(reorder_size(term))) +
  geom_bar(stat = "count") +
    coord_flip() +
  theme_bw() +
  theme(text = element_text(),
        axis.text.x = element_text(angle = 90, 
                                   vjust = 1)) 


reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x))))
}

ggplot() + 
  geom_point(
    data = charPos, 
    aes(x = position, y = reorder_size(term), alpha = 1/40))	+
  ylab("") + 
  xlab("") +
  theme_bw() +
  theme(legend.position="none", 
        axis.text.x  = element_text(size=8))

# Merge together to make new plot.  Basically I just want chapter on the X axis
head(charPos)
head(chapPos)


# Plot that shows the length
str(chapPos)
chapPos$chapter <- gsub("ch", "", chapPos$chapter)

# Make NA endnotes
chapPos$chapter[is.na(chapPos$chapter)] <- "endnotes"

# add words per page value
chapPos$page <- chapPos$length / 250

ggplot(chapPos, aes(chapter, page)) + 
  geom_bar(stat = "identity") +
  ylab("Pages") +
  xlab("Chapter") +
  theme_bw() +
  scale_y_continuous(labels=comma) +
  theme(legend.position="none", 
        text = element_text(size=20),
        axis.text.x  = element_text(angle = 45, hjust = 1, size = 14)) 
  
# Words per minute Avg = 200
chapPos$time <- (chapPos$length / 200) / 60 
number_ticks <- 20

ggplot(chapPos, aes(chapter, time)) + 
  geom_bar(stat = "identity") +
  ylab("Time (hours)") +
  xlab("Chapter") +
  theme_bw() +
  scale_y_continuous(labels=comma, breaks = pretty_breaks(20)) +
  theme(legend.position="none", 
        text = element_text(size=20),
        axis.text.x  = element_text(angle = 45, hjust = 1, size = 14)) 

sum(chapPos$time)

## Resources

[co-occurance](https://www.r-bloggers.com/turning-keywords-into-a-co-occurrence-network/)
https://matthewlincoln.net/2014/12/20/adjacency-matrix-plots-with-r-and-ggplot2.html
