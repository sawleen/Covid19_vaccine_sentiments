################################################
# A project done in March 2021 to:
# Use data mined from Twitter to visualize tweets on various vaccines
# eg. Frequencies of various words and their sentiments 
# in tweets about various vaccines
################################################

library(data.table)
library(tidyverse)
library(tm)
library(igraph)
library(textstem) #For stemDocument
library(qdap)
library(wordcloud) # Plot word cloud
library(tidytext) # Library for sentiment analysis
library(gridExtra)
library(grid)
library(ggplot2)
library(scales)
rm(list=ls())

#### Read data
home_dir<-"/Users/sawleen/Documents/Leen/Online courses/Coursera/Social Media Data Analytics/vaccine_analysis/Sentiment Analysis results/"
setwd(home_dir)

###### ====== Start of functions ===== ######
#### ==== Common words in tweets and frequencies of appearance ==== ####
# Return dataframe with two columns: "term" and "freq"
# Collection words should include words that were used to run the Twitter API query eg. c("virus","covid")
create_df_wordFreq <- function(fileToRead, collection_words = NULL) {
  tweets <- read.csv(fileToRead, stringsAsFactors = FALSE)
  #### Text cleaning
  text = tweets$text
  text_clean = replace_contraction(text)
  text_clean = replace_abbreviation(text_clean)
  text_clean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text_clean) #Remove retweet entries
  text_clean = gsub("amp","",text_clean)
  text_clean = gsub("@\\w+", "", text_clean) #Remove @people
  #text_clean = gsub("[[:punct:]]", "", text_clean) #Remove punctuation
  #text_clean = gsub("[[:digit:]]", "", text_clean) #Remove numbers
  text_clean = gsub("http\\w+", "", text_clean) #Remove links
  text_clean = gsub("://.*", "", text_clean) #Remove links
  text_clean = gsub("#", "", text_clean) #Remove hashtags
  text_clean <- lemmatize_words(text_clean)
    
  #### Create corpus of words
  text_corpus = Corpus(VectorSource(text_clean)) #Create corpus
  text_corpus = tm_map(text_corpus, tolower) #Convert to lower case
  text_corpus = tm_map(text_corpus, removeWords, stopwords("english")) #Remove stopwords eg. "a", "the", etc.
  text_corpus = tm_map(text_corpus, removePunctuation) #Remove punctuation
  text_corpus = tm_map(text_corpus, stripWhitespace) #Remove extra spaces
  text_corpus = tm_map(text_corpus, removeNumbers) #Remove numbers
  
  # custom function to remove URLs
  removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
  ### myCorpus <- tm_map(myCorpus, removeURL, lazy=TRUE) 
  text_corpus <- tm_map(text_corpus, content_transformer(removeURL))
  
  #text_corpus = tm_map(text_corpus, stemDocument) #Removes last few letters of similar words eg. "gets", "getting", "get"
  my_stopwords <- c("can", "will", "may", "shall", "also", "just", "still")
  text_corpus = tm_map(text_corpus, removeWords, my_stopwords) #Remove stopwords eg. "a", "the", etc.
  
  # Remove collection words if any
  text_corpus = tm_map(text_corpus, removeWords, collection_words)
  
  #text_corpus = tm_map(text_corpus, PlainTextDocument) #May interfere with TermDocumentMatrix function below; comment out if nec
  tdm = TermDocumentMatrix(text_corpus) #TDM puts terms (words) as rows and documents as columns; for DTM it's the other way round
  #m = as.matrix(tdm)
  
  # Remove sparse terms (word frequency > 99% percentile)
  notSparse = removeSparseTerms(tdm, 0.999) # extracts frequently occuring words
  m1 = as.matrix(notSparse) # most frequent words remain in a dataframe, with one column per word
  
  # Create word-frequency table
  # inspect(tdm)                        
  freq<-rowSums(m1)
  df <- data.frame(term = names(freq), freq = freq)
  df <- arrange(df, desc(freq))
  
  return(df)
}

#### ====  Form word cloud with df generated ==== ####
# Displays max. 20 most frequent words in cloud
# If Twitter query was not limited to any search word, use NULL for collection_words
form_cloud <- function(fileToRead, collection_words = NULL, maxWsize=2) {
  df<-create_df_wordFreq(fileToRead, collection_words)
  my_wordCloud <- wordcloud(df$term,df$freq, 
                            scale = c(maxWsize,0.5), #min.freq = numRows/40, 
                            max.words=25,random.order = FALSE, 
                            rot.per = 0.35,
                            colors=brewer.pal(8, "Dark2"), #c("indianred1","indianred2","indianred3","indianred"))
                            use.r.layout = FALSE)
  
  return(my_wordCloud)
}

#### ==== Sentiment analysis using R packages and plot bar charts ==== ####
# Choose either "afinn", "bing" or "nrc" as method
# "nrc" returns 10 emotions and associated words (some words have >1 emotion)
analyse_sentiments <- function(fileToRead, collection_words = NULL) {
  df<-create_df_wordFreq(fileToRead, collection_words)
  sentiments<-get_sentiments("nrc")
  df_new <- inner_join(df,sentiments,by=c("term"="word"))
  total_words <- sum(df_new$freq)
  
  # Analyse proportion of words with each sentiment
  analysis <- df_new %>% 
    group_by(sentiment) %>%
    summarise(prop = sum(freq)/total_words*100) %>%
    arrange(desc(prop))
  
  # Sort words based on sentiment with highest proportion (descending order)
  df_sort <- left_join(df_new, analysis, by = c("sentiment")) 
  df_sort <- df_sort %>% arrange(desc(prop), sentiment,desc(freq))
  list_result <- list(df_sort, analysis, df)
  
  return(list_result)
}

#### ==== Get common words and associated sentiments ==== ####
get_commonWords <-function(list_result) {
  return (as.data.frame(list_result[3]))
}
  
get_sentiWords<-function(list_result) {
  return (as.data.frame(list_result[1]))
}

#### ==== Plot NRC sentiments ==== ####
# Plot bar chart of sentiments based on 10 NRC categories
plot_bar_sentimentCategory<-function(fileToRead, collection_words = NULL, YMax=100, Ybreak=10, title) {
  list_result<-analyse_sentiments(fileToRead, collection_words)
  analysis<-as.data.frame(list_result[2])
  # Feed into ggplot x-value as sentiment, ordered by wordCount in descending frequency
  # y-value is wordCount and colour bars according to wordCount
  plot<-ggplot(analysis, aes(reorder(sentiment,-prop), prop, fill = prop)) 
  # Plot bar chart
  bar <- plot + geom_bar(stat="identity")
  # Set fill limits
  bar <- bar + scale_fill_continuous(
    high = "#132B43", low = "#56B1F7", #swaps high and low colours
    limits = c(0,YMax), 
    breaks = seq(0,YMax,by=Ybreak))
  # Format text
  bar <- bar + theme(
    axis.title.x=element_text(size=9,face="bold"),
    axis.title.y=element_text(size=9,face="bold"),
    axis.text.x=element_text(size=8, angle=30),
    plot.title=element_text(size=10,face="bold",hjust=0.5),
    legend.title=element_text(size=8,face="bold"))
  # Label x-axis, y-axis and legend title
  bar <- bar + xlab("Emotion") + ylab("Freq of words with emotion (%)") + labs(fill="Percentage")
  # Add title and justify to centre
  bar <- bar + ggtitle(title) 
  # Set limits to ensure all charts on same axis scale
  # bar <- bar + ylim(0,YMax)
  bar <- bar + scale_y_continuous(
    limits = c(0,YMax), 
    breaks = seq(0,YMax,by=Ybreak))
  
  return(bar)
}

#### Show top 20 most common words by sentiment (using "bing") ####
plot_words <- function(df, title) {
  sentiments_bing <- get_sentiments("bing")
  df_bing <- inner_join(df, sentiments_bing, by = c("term" = "word"))
  # Keep only top 20 words
  df_bing_subset <- df_bing %>% top_n(20, freq)
  
  # Plot horizontal bar chart
  plot <- ggplot(df_bing_subset, aes(freq,reorder(term,freq),fill=sentiment)) + geom_col()
  # Format text
  plot <- plot + theme(
    axis.title.x=element_text(size=9,face="bold"),
    axis.title.y=element_text(size=9,face="bold"),
    axis.text.x=element_text(size=8),
    plot.title=element_text(size=10,face="bold",hjust=0.5),
    legend.title=element_text(size=8,face="bold"))
  # Format legend
  plot <- plot + scale_fill_manual(values=c("positive"="slateblue1","negative"="salmon"),limits=c("positive","negative"),name="Sentiment",labels=c("Positive","Negative"),guide="legend",drop=FALSE)
  # Add labels
  plot <- plot + xlab("Frequency in tweets") + ylab("Word")
  plot <- plot + ggtitle(title)
  
  return(plot)
}
###### ====== End of functions ===== ######

###### ====== Results - all tweets ===== ######
#### ==== Bar charts ==== ####
all_vacc <-'vaccine_all.csv' #File to read
# Get table of sentiments and common words
analysis_vacc <- analyse_sentiments(all_vacc, collection_words=NULL)
df_vacc <-get_sentiWords(analysis_vacc)
df_vacc_common<-get_commonWords(analysis_vacc)
bar_words_vacc <- plot_words(df_vacc_common, "vaccine")
bar_words_vacc 
# Get bar chart of sentiments of words
bar_sent_vacc<-plot_bar_sentimentCategory(all_vacc, collection_words = NULL, YMax=50, Ybreak=10, title='Vaccine') # Amend YMax accordingly
bar_sent_vacc # Visualize

# Save to JPEG when satisfied
jpeg(file="bar_sent_vacc.jpeg")
bar_sent_vacc
dev.off()

all_pfizer <-'pfizer_all.csv' #File to read
analysis_pfizer <- analyse_sentiments(all_pfizer, collection_words=NULL)
df_pfizer <-get_sentiWords(analysis_pfizer)
df_pfizer_common<-get_commonWords(analysis_pfizer)
bar_words_pfizer <- plot_words(df_pfizer_common, "Pfizer")
bar_words_pfizer
bar_sent_pfizer<-plot_bar_sentimentCategory(all_pfizer, collection_words = NULL, YMax=30, Ybreak=5, title='Pfizer')
bar_sent_pfizer # Visualize

# Save to JPEG when satisfied
jpeg(file="bar_sent_pfizer.jpeg")
bar_sent_vacc
dev.off()

all_moderna <-'moderna_all.csv' #File to read
analysis_moderna <- analyse_sentiments(all_moderna, collection_words=NULL)
df_moderna <-get_sentiWords(analysis_moderna)
df_moderna_common<-get_commonWords(analysis_moderna)
bar_words_moderna <- plot_words(df_moderna_common, "Moderna")
bar_words_moderna # Visualize

bar_sent_moderna<-plot_bar_sentimentCategory(all_moderna, collection_words = NULL, YMax=30, Ybreak=5, title='Moderna')
bar_sent_moderna # Visualize - then amend Ymax/Ybreak in line above

# Save to JPEG when satisfied
jpeg(file="bar_sent_moderna.jpeg")
bar_sent_moderna
dev.off()

all_sinovac <-'sinovac_all.csv' #File to read
analysis_sinovac <- analyse_sentiments(all_sinovac, collection_words=NULL)
df_sinovac <-get_sentiWords(analysis_sinovac)
df_sinovac_common<-get_commonWords(analysis_sinovac)
bar_words_sinovac <- plot_words(df_sinovac_common, "Sinovac")
bar_sent_sinovac<-plot_bar_sentimentCategory(all_sinovac, collection_words = NULL, 
                                             YMax=30, Ybreak=5,title='Sinovac') #Set Ymax to 100 first then tweak
bar_sent_sinovac # Visualize - then amend Ymax/Ybreak in line above

# Save to JPEG when satisfied
jpeg(file="bar_sent_sinovac.jpeg")
bar_sent_sinovac
dev.off()

# Show on screen all bar charts
sentiment_nrc_title <- "General sentiments of tweets (vaccine-related)"
bar_sent_combined<-grid.arrange(bar_sent_vacc, bar_sent_pfizer,
                                   bar_sent_moderna, bar_sent_sinovac,
                                   ncol=2,
                                   top = textGrob(sentiment_nrc_title,just="centre",gp=gpar(fontsize=10, lineheight = 0.8, font=2)))
# Save to JPEG when satisfied
ggsave(file="bar_sent_combined.jpeg",plot=bar_sent_combined)

# Word frequencies in tweets
words_title <- "Common words in tweets (vaccine-related)"
words_sent_combined<- grid.arrange(bar_words_vacc, bar_words_pfizer,
                     bar_words_moderna, bar_words_sinovac,
                     ncol=2,
                     top = textGrob(words_title,just="centre",gp=gpar(fontsize=12, lineheight = 0.8, font=2)))
# Save to JPEG when satisfied
ggsave(file="words_sent_combined.jpeg",plot=words_sent_combined)

##### ===== Word clouds ===== ####
dev.off()  # To reset screen
par(mfrow=c(2,2)) #Change to grid of (row, column)
par(mar=c(0,0,0,0))
cloud_vacc <- form_cloud(all_vacc, NULL, maxWsize=3.5)
wordcloud_title <- "Vaccine"
text(x=0.5, y=0.99, wordcloud_title, cex=1.4, font=2) #font=2 sets bold

cloud_pfizer <- form_cloud(all_pfizer, NULL, maxWsize=3.5)
wordcloud_title <- "Pfizer"
text(x=0.5, y=0.99, wordcloud_title, cex=1.4, font=2) #font=2 sets bold

cloud_moderna <- form_cloud(all_moderna, NULL, maxWsize=3.5)
wordcloud_title <- "Moderna"
text(x=0.5, y=0.99, wordcloud_title, cex=1.4, font=2) #font=2 sets bold

cloud_sinovac <- form_cloud(all_sinovac, NULL, maxWsize=3.5)
wordcloud_title <- "Sinovac"
text(x=0.5, y=0.99, wordcloud_title, cex=1.4, font=2) #font=2 sets bold

# Save to jpeg
dev.copy(jpeg,'wordclouds_vaccines.jpg')
dev.off()
###### ====== End of results - all tweets ===== ######