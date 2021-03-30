<h1> Sentiment analysis of Twitter tweets on Covid-19 Vaccines </h1>

<hr>
<head>
<h3> Description </h3>
With different vaccines rolling out in 2021, public sentiment and confidence towards various vaccines varies.  To understand public receptiveness to different types of vaccines, this project was done in March 2021 to analyze if there is any particular preference towards Pfizer, Moderna or Sinovac.
  </head>
<hr>  
<body>
<h3> Methodology </h3>
  
1) Mine tweets using Twitter API for different Covid-19 vaccines (Python)
- Data is saved into a CSV file to be read in steps 2 and 3
- Python's textblob library is used to classify polarity of each tweet

2) Create pie charts of sentiment classification of vaccine-related tweets (R)
- Classify sentiment of tweets as "positive', "neutral" or "negative" based on Python's textblob polarity
- Classify sentiment of words in tweets using the "bing" emotion lexicon in R (tidytext), which classifies words as "positive" or "negative" sentiment

3) Generate visualizations of words in vaccine-related tweets, and quantify their sentiments (R)
- Classify sentiment of words in tweets to 10 predetermined emotions using the "NRC" emotion lexicon in R (tidytext), and visualize in a bar chart
- Form word clouds

Codes are in uploaded_codes. 
</body>


<hr>
<h3> Results </h3>

The results show that tweeters are:
- Generally receptive towards Covid-19 vaccines (majority ~85%)
 - Agnostic in terms of preference towards Pfizer or Moderna, although many experienced soreness upon vaccination
- Have less trust and more negative sentiment towards Sinovac vaccines compared to Pfizer/ Moderna

For detailed charts, refer to "Results" PDF.
