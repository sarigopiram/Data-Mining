getwd();

setwd("E:\\Spring 2015\\Visual Analytics\\Project\\Sentiment")

pos.words = scan('positive.txt',what='character')-
neg.words = scan('negative.txt',what='character')

vice.words = scan('vice.txt',what='character')
virtue.words = scan('virtue.txt',what='character')


space.words = scan('place.txt',what='character')
price.words = scan('time.txt',what='character')

quant.words = scan('quantity.txt',what='character')
qualt.words = scan('quality.txt',what='character')


#install.packages("XLConnect")
#require(XLConnect)
reviewdata <- readWorksheet(loadWorkbook("Reviews_saritha1.xlsx"),sheet=1)
#head(reviewdata);
reviewdata<-as.data.frame(reviewdata)

df <- data.frame(reviewdata$review_id,
                 reviewdata$business_id,
                 reviewdata$text)

posscore<-score.sentiment(reviewdata$text,reviewdata$review_id,pos.words)
print(posscore)
df$posscore<-posscore$score


negscore<-score.sentiment(reviewdata$text,reviewdata$review_id,neg.words)
print(negscore)
df$negscore<-negscore$score




vicescore<-score.sentiment(reviewdata$text,reviewdata$review_id,vice.words)
print(vicescore)
df$vicescore<-vicescore$score

virtuescore<-score.sentiment(reviewdata$text,reviewdata$review_id,virtue.words)
print(virtuescore)
df$virtuescore<-virtuescore$score

spacescore<-score.sentiment(reviewdata$text,reviewdata$review_id,space.words)
print(spacescore)
df$spacescore<-spacescore$score

pricescore<-score.sentiment(reviewdata$text,reviewdata$review_id,price.words)
print(pricescore)
df$timescore<-pricescore$score

quantscore<-score.sentiment(reviewdata$text,reviewdata$review_id,quant.words)
print(quantscore)
df$quantscore<-quantscore$score

qualtcore<-score.sentiment(reviewdata$text,reviewdata$review_id,qualt.words)
print(qualtcore)
df$qualtcore<-qualtcore$score


head(df)



write.csv(df, file = "reviews1.csv",row.names=FALSE)




head(pos.words)

score.sentiment = function(sentences,idnum, lexiconset, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, lexiconset) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, lexiconset)
  #  neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
   # neg.matches = !is.na(neg.matches)
    
    print(idnum)
    print(sum(pos.matches))
   # print(neg.matches)
    
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
  if(sum(pos.matches)>=5)
    score=1
  else
    score=0
  
    return(score)
  }, lexiconset, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}
