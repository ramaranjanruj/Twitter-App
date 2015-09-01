library(shiny)                  # To build the shiny app
library(tm)                     # package for text analytics
library(ggplot2)                # makes visually aesthetic plots
library(twitteR)                # library to get tweets using the twitter API
library(stringr)                # package for string manipulation
library(RCurl)                  # Makes HTTP connection in the R interface
library(reshape)                # for using the plyr function
library(RJSONIO)                # for converting JavaScript to R objects
library(wordcloud)              # creates fancy word cloud
library(gridExtra)              # Addon for ggplot
library(plyr)                   # for the plyr function
library(shinyIncubator)

# First we need to make a developer account in twitter and create a app. Once the app is created, the keys are made available. We will use the key and secret to extract tweets from twitter. More information about the twitter APi can be found in https://dev.twitter.com/overview/api

api_key <- "YvkutpGKLE5GF7tTgj6C6Rl2N"
api_secret <- "S4X411AEQTkageVCDkkJ1gubLofTwOn8wf2ls9O78EpxKCorTZ"
access_token <- "69119995-zW2hPiy02doO1MEF7FcOu16k8GTqyp49BhNACiYG1"
access_token_secret <- "U8qdm7Z83W3mzIYPQ12RW2x9wKVuALYxEjzgb3xTpfRKD"

#origop <- options("httr_oauth_cache")
#options(httr_oauth_cache=TRUE)
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
#options(httr_oauth_cache=origop)


# Once the connection is created, start extracting tweets.

shinyServer(function(input, output, session){

        
# The raw tweets from twitter are cleaned and all redundant inforamtion are removed.
CleanTweets<-function(tweets){
        tweets <- str_replace_all(tweets," "," ")
        tweets <- str_replace_all(tweets, "http://t.co/[a-z,A-Z,0-9]{10}","")
        tweets <- str_replace_all(tweets, "https://t.co/[a-z,A-Z,0-9]{10}","")
        tweets <- str_replace(tweets,"RT @[a-z,A-Z]*: ","")
        tweets <- str_replace_all(tweets,"#[a-z,A-Z]*","")
        tweets <- str_replace_all(tweets,"@[a-z,A-Z]*","")
        return(tweets)
}

# This function extracts tweets from twitter
TweetFrame<-function(searchTerm, maxTweets){
        twtList<-searchTwitter(searchTerm,n=maxTweets, since = as.character(input$date), lang="en")
        twtList1<- do.call("rbind",lapply(twtList,as.data.frame))
        twtList1$text<-iconv(twtList1$text, 'UTF-8', 'ASCII') 
        return(twtList1)
}

# To create a data frame for the number of tweets of both hashtags
numoftweets<-function(entity1,entity2,entity1entry,entity2entry){
        ent1numtweets<-nrow(entity1)
        ent2numtweets<-nrow(entity2)
        notweets<-c(ent1numtweets,ent2numtweets)
        names(notweets)<-c(entity1entry,entity2entry)
        notweets
}


# Functiom to create a word cloud.
wordcloudentity<-function(entitycleantext){
        tweetCorpus<-Corpus(VectorSource(CleanTweets(entitycleantext)))
        tweetTDM<-TermDocumentMatrix(tweetCorpus,
                                     control=list(removePunctuation=TRUE,
                                                              stopwords=c(stopwords('english')),
                                                              removeNumbers=TRUE,tolower=TRUE))
        tdMatrix <- as.matrix(tweetTDM)
        sortedMatrix<-sort(rowSums(tdMatrix),decreasing=TRUE)
        cloudFrame<-data.frame(word=names(sortedMatrix),freq=sortedMatrix)
        
        wcloudentity<-wordcloud(
                cloudFrame$word,
                cloudFrame$freq,
                max.words=100,
                colors=brewer.pal(8,"Dark2"),
                scale=c(8,1),
                random.order=TRUE)
        
        print(wcloudentity)
}

# Calculates the sentiment score for each tweet using the breen's alorithm.
score.sentiment = function(sentences, pos.words, neg.words){
        
                scores = laply(sentences, function(sentence, pos.words, neg.words) {
                        sentence = gsub('[[:punct:]]', '', sentence)
                        sentence = gsub('[[:cntrl:]]', '', sentence)
                        sentence = gsub('\\d+', '', sentence)
                        sentence = tolower(sentence)
                

                        word.list = str_split(sentence, '\\s+')
                        words = unlist(word.list)
                        pos.matches = match(words, pos.words)
                        neg.matches = match(words, neg.words)
                        pos.matches = !is.na(pos.matches)
                        neg.matches = !is.na(neg.matches)
                        score = sum(pos.matches) - sum(neg.matches)
                
                        return(score)
        }, pos.words, neg.words)
        
        scores.df = data.frame(score=scores, text=sentences, size=seq(length(scores)))
        return(scores.df)
}

# calls the score.sentiment function to calculate the sentiment
sentimentalanalysis<-function(entity1text,entity2text,entity1entry,entity2entry){
        positivewords=readLines("positive_words.txt")
        negativewords=readLines("negative_words.txt")
        entity1score = score.sentiment(CleanTweets(entity1text),positivewords,negativewords)
        entity2score = score.sentiment(CleanTweets(entity2text),positivewords,negativewords)
        entity1score$entity = entity1entry
        entity2score$entity = entity2entry
        entityscores<-rbind(entity1score,entity2score)
}



entity1<-reactive({
        if(input$actb>=0 )
        entity1<-TweetFrame(input$entity1, input$maxTweets)}
)
entity2<-reactive({
        if(input$actb>=0 )
        entity2<-TweetFrame(input$entity2, input$maxTweets)}
)


#Creating sentiment scores
entityscores<-reactive({
        if(input$actb>=0 )
        entityscores<-sentimentalanalysis(entity1()$text,entity2()$text,input$entity1,input$entity2)})

#Preparing the output in a series of tabs

#tab 1  - number of tweets for the two entities and also plotting the probability of arrival of a new tweet 
#within a particular time t

#number of tweets
output$notweets<-renderPrint({
        if(input$actb>=0 )
        numoftweets(entity1(),entity2(),input$entity1,input$entity2)})


#tab 1: Not all chatter may be good. So a box plot to see the distribution of scores of sentiments 

output$sentiboxplot<-renderPlot({
        if(input$actb>=0 )
        cutoff <- data.frame(yintercept=0, cutoff=factor(0))
        sentiboxplot<-ggplot(entityscores(),aes(x=size,y=score))+
                facet_grid(. ~ entity)+
                geom_violin(aes(fill=entity)) +
                geom_hline(yintercept=0) +
                ylab('Sentiment Score')+
                theme_grey()
        print(sentiboxplot)})



#tab 2 - Word Clouds to highlight terms used in tweets associated with the two entities
output$entity1wc<-renderText({input$entity1})

output$entity1wcplot<-renderPlot({
        if(input$actb>=0 )
        wordcloudentity(entity1()$text)})

output$entity2wc<-renderText({input$entity2})

output$entity2wcplot<-renderPlot({
        if(input$actb>=0 )
        wordcloudentity(entity2()$text)})


#tab  3: Raw tweets of entity 1
output$tableentity1 <- renderTable({tab<-entity1()[1]})

#tab 4: Raw tweets of entity 2

output$tableentity2<-renderTable({tab<-entity2()[1]})

})