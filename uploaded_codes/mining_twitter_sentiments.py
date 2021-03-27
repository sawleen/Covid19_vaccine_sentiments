"""
Mine Twitter data based on a specified search query and geolocations
Adapted from Coursera and https://bhaskarvk.github.io/2015/01/how-to-use-twitters-search-rest-api-most-effectively./
Data is later analysed using R
"""

### Import libraries
from textblob import TextBlob
import os
import pandas as pd
import csv
import tweepy
import unidecode

###################################################################################################
### Set home directory
home_dir = "/Users/sawleen/Documents/Leen/Online courses/Coursera/Social Media Data Analytics/vaccine_analysis"
os.chdir(home_dir)

### Set authentication
# auth.k has these numbers written in 4 lines:
# 1. consumer key
# 2. consumer secret
# 3. access token
# 4. access secret
# Please save auth.k in the same directory as this script
f = open('auth.k','r')
ak = f.readlines()
f.close()
auth1 = tweepy.OAuthHandler(ak[0].replace("\n",""), ak[1].replace("\n",""))
auth1.set_access_token(ak[2].replace("\n",""), ak[3].replace("\n",""))
api = tweepy.API(auth1,  wait_on_rate_limit=True,
				   wait_on_rate_limit_notify=True)
###################################################################################################
### Set query - to modify queries as preferred
## Set keywords to query
all_query = "*" #'*' searches for all queries
covid_query = 'covid OR covid-19 OR covid19 OR coronavirus OR virus OR #covid OR #virus OR #covid-19 OR #covid19 OR #coronavirus'
politician_query = 'obama OR #obama'
transport_query = 'mrt OR bus OR #mrt OR #bus'
vaccine_query = 'sinovac'

## Set place of query
# Getting Geo ID for relevant city/country
# See more details at: https://developer.twitter.com/en/docs/geo/places-near-location/api-reference/get-geo-search
city = "China" # Modify where nec
places = api.geo_search(query=city, granularity="country") # Modify as nec - 'city' is more encompassing/less granula than 'country'

# Check that place(s) is correctly extracted
num=0
for num in range(0,1):
    print(num)
    print('full name: ', places[num].full_name)
    print('country: ', places[num].country)
    print('id: ', places[num].id)

# Set place ID for query
place_id = places[0].id # Modify to below if nec
print('{} place ID is: '.format(city),place_id)
# Overwrite if above check looks off
#place_id = '6416b8512febefc9' #Overwritten for UK
place_query = 'place:{} '.format(place_id)
# city = 'all'

# Specify parameters for query
query = vaccine_query + ' ' + place_query  # Modify query as nec
target_num = 5000 # Set max number of total tweets to be downloaded
countPerQuery = 100 # Set max number of tweets per page; maximum that can be done in API is 100
fileName = 'Sentiment Analysis results/{}_{}.csv'.format(vaccine_query, city) #Modify name of CSV file to write to

###################################################################################################
### Mine Twitter results and write to CSV
# Initialize
csvFile = open(fileName,'w')
csvWriter = csv.writer(csvFile)
csvWriter.writerow(["username","authorid","created", "userloc", "text", "retwc", "hashtag", "tweetloc","followers", "friends","polarity","subjectivity"])

counter = 0
sinceId = None
sinceId_prev = None

### Start mining
# If console says rate limit reached and it goes to sleep, just let it wait for awhile (~15 min) and it will run again
print('Current query: {}'.format(query))
print('Saving to file: {}_{}.csv'.format(vaccine_query, city))
# For more info on the parameters used for API search, visit: http://docs.tweepy.org/en/latest/api.html
while counter <= target_num:
    # Exit if insufficient existing tweets to hit target num
    if sinceId_prev != None and sinceId_prev == sinceId:
        break
    sinceId_prev = sinceId

    # Start
    print('Since id is currently: ', sinceId)
    # Modify query as necessary below
    tweet_search = tweepy.Cursor(api.search, q = query, #geocode = locords_nyc,
                               lang = "en", result_type = "mixed",
                               since_id = sinceId,
                               count = countPerQuery).items(target_num-counter)
    # Save results
    for tweet in tweet_search:
        created = tweet.created_at
        text = tweet.text
        text = unidecode.unidecode(text)
        retwc = tweet.retweet_count
        # Geographic co-ordinates
        if tweet.place:
            cords = tweet.place.country + ', ' + tweet.place.name
        elif tweet.coordinates:
            cords = tweet.coordinates
        else:
            cords = None
        try:
            hashtag = tweet.entities[u'hashtags'][0][u'text'] #hashtags used
        except:
            hashtag = "None"

        # Set fields to save
        username  = tweet.author.name            #author/user name
        authorid  = tweet.author.id              #author/user ID#
        followers = tweet.author.followers_count #number of author/user followers (inlink)
        friends = tweet.author.friends_count     #number of author/user friends (outlink)
        userloc = tweet.author.location        #author/user location

        text_blob = TextBlob(text)
        polarity = text_blob.polarity
        subjectivity = text_blob.subjectivity

        # Write to CSV
        csvWriter.writerow([username, authorid, created, userloc,text, retwc, hashtag, cords, followers, friends, polarity, subjectivity])

        # Printing progress to console
        counter = counter + 1
        if (counter<=target_num) and counter%100 == 0:
            print('Downloaded {} tweets'.format(counter))

        if counter >= target_num:
            break

        if sinceId == None:
            sinceId = tweet.id
        elif sinceId < tweet.id:
            sinceId = tweet.id

    # Allow for different number of query results returned
    if counter == 0:
        print('No tweet results from specified query')
        break

    if counter >= target_num:
        break

# Close file and print progress to console
csvFile.close()
print('Total of {} tweets saved to {}'.format(counter, fileName))

