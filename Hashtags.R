###Data extraction - please note: it was done in SQL and the results were saved in csv file (and then - analysed in R)


select *
  from soma_twitter_interactions
where created_at>='2015-04-12'  ##change date
and created_at<'2015-04-13'  ##change date
and heard_or_spoken='spoken'
and text like '#'

##Pull all hashtags

library(dplyr)
library(magrittr) 
library(data.table)

tw<-read.csv("filename.csv")

###Create a vector with tweets
vec1<-tw$text



####Function extracting hashtags

extract.hashes = function(vec){
  
  hash.pattern = "#[[:alpha:]]+"
  have.hash = grep(x = vec, pattern = hash.pattern)
  
  hash.matches = gregexpr(pattern = hash.pattern,
                          text = vec[have.hash])
  extracted.hash = regmatches(x = vec[have.hash], m = hash.matches)
  
  df = data.frame(table(tolower(unlist(extracted.hash))))
  colnames(df) = c("tag","freq")
  df = df[order(df$freq,decreasing = TRUE),]
  return(df)
}


###Pull top 100 hashtags

dat = head(extract.hashes(vec1),100)
dat2 = transform(dat,tag = reorder(tag,freq))



####Write csv file

write.csv(dat2, file = "bes.11apr.csv",row.names=FALSE)