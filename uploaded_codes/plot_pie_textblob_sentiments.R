################################################
# A project done in March 2021 to:
# Use data mined from Twitter to visualize sentiments on various vaccines

# Sentiment results for each type of related tweet are plot in pie charts
# Sentiment polarity function determined with Python's textblob library
################################################

#### ==== Import libraries  ==== ####
library(tidyverse)
library(data.table)
library(ggplot2)
library(anytime)
library(scales) # Scale geom_text labels to the right position
library(gridExtra) # Plot subplots
library(tm)
library(igraph)

#### ==== Set directories  ==== ####
home_dir<-"/Users/sawleen/Documents/Leen/Online courses/Coursera/Social Media Data Analytics/vaccine_analysis/Sentiment Analysis results"
setwd(home_dir)

################ ==== Start of functions  ==== ################
#### ==== Create column indicating whether sentiment is negative or positive  ==== ####
classify_sentiment <- function(dataset) {
  dataset <- dataset %>% mutate(
    sentiment = case_when(
      polarity<0 ~ "negative",
      polarity==0 ~ "neutral",
      polarity>0 ~ "positive"
    )
  )
}

#### ==== Indicate position of labels in pie chart ==== ####
calc_ypos <- function(dataset) {
  sentiment_group <- dataset %>% group_by(sentiment) %>% summarise(prop=n()/nrow(dataset)*100)
  sentiment_group <- sentiment_group %>%
    arrange(desc(sentiment)) %>%
    mutate(ypos = cumsum(prop) - 0.5*prop)
  
  return(sentiment_group)
}

#### ==== Prepare the blank theme to remove axis tick mark labels on pie chart ==== ####
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=10, face="bold", hjust=0.5),
    legend.text=element_text(size=8),
    legend.title=element_text(size=8, face="bold")
  )

#### ==== Plot pie chart showing polarity of tweets ==== ####
plot_pie <- function(sentiment_group, blank_theme, topic) {
  #To make pie chart
  pie <-ggplot(data = sentiment_group, 
               aes(x="", y=prop,fill=factor(sentiment))) +
    geom_bar(width=1, stat='identity') + coord_polar(theta="y")
  #To add percentage labels
  pie <- pie +  blank_theme + theme(axis.text.x=element_blank()) + 
    geom_text(aes(y=ypos,label = percent(prop/100)),color="black",size=3, hjust=0.5) 
  #To label
  pie<- pie + ggtitle(topic)  + labs(fill="Sentiment") 

  return(pie)
}

#### ==== Function to string them all ==== ####
main <- function(dataset, blank_theme, topic) {
  dataset <- dataset %<>% classify_sentiment() %>% 
    calc_ypos() 
  pie <- plot_pie(dataset, blank_theme, topic)
  
  return(pie)
}
################ ==== End of functions  ==== ################

#### ==== Read datasets generated from twitter_sentiment.py run in Spyder ==== ####
# Read sentiments of all vaccine-related tweets
all_vacc<-read.csv('vaccine_all.csv')
all_pfizer <-read.csv('pfizer_all.csv')
all_moderna <- read.csv('moderna_all.csv')
all_sinovac <- read.csv('sinovac_all.csv')

# Eyeball data to check for abnormalities
summary(all_vacc)
nrow(all_vacc)
length(unique(all_vacc$username)) # Eyeball to check for repeated data in downloaded tweets

#### ==== Create pie charts  ==== ####
# Classify sentiment of tweets and plot in pie chart
pie_allvacc <- main(all_vacc, blank_theme, 'Vaccine')
pie_allpfizer <- main(all_pfizer, blank_theme, 'Pfizer')
pie_allmoderna <- main(all_moderna,blank_theme, 'Moderna')
pie_allsinovac <- main(all_sinovac,blank_theme, 'Sinovac')

#### ==== Display plot  ==== ####
# Display plot for Twitter-related sentiments on Covid
overall_title = "Twitter sentiments on Vaccines"
grid.arrange(pie_allvacc, pie_allpfizer, 
             pie_allmoderna, pie_allsinovac, 
             ncol = 2,
             top = textGrob(overall_title,just="centre",gp=gpar(fontsize=12, lineheight = 0.8, font=2))
             )
# For more details check this out: https://www.rdocumentation.org/packages/grid/versions/3.6.2/topics/gpar

# Save to jpeg
dev.copy(jpeg,'pie_sent_vaccines.jpg')
dev.off()

#### ==== Eyeball to see if any frequent users, or users who belong in particular areas, who skew tweets towards particular sentiments  ==== ####
# Analyse by users 
tweeters_pfizer <- all_pfizer %>% 
  group_by(username) %>% 
  summarise(tweeters=n()) %>%
  arrange(desc(tweeters))
max(tweeters_pfizer$tweeters)

# Analyse by area
tweetArea_pfizer <- all_pfizer %>% 
  group_by(tweetloc) %>% 
  summarise(tweetCount=n()) %>%
  arrange(desc(tweetCount))
head(tweetArea_pfizer,2)
