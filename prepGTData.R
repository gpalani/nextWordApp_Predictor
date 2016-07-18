
### Temp palceholder for app related code
# library(wordcloud)
# library(RColorBrewer)
# library(reshape2)
# # library(ggplot2)
# library(slam)
# library(cluster)
# library(SnowballC)

# Load the sample set data

# load("sampleSetData.RData")
# load("nGram1to5TDM.RData")
# load("nGram1to5Freq.RData")

### Code to see if we need to clean the data frame of special chars or covert to different encoding.

substApostrophe  <- function(x) {
  x <- stri_replace_all_regex(x, "['`´\u2018\u2019\u2027\u201B]", "__AP__")
  return(x)
}

View(iconv(head(bigramDF$word,1000),from = "Latin-9", to = "UTF-8", sub = ""))

View(stri_replace_all_regex(head(bigramDF$word,1000),"['`´\u0027\u0060\u00B4\u2018\u2019\u2027\u201B]", ""))

sentence <- stri_replace_all_regex(sentence, "[\u0027\u0060\u00B4\u2019\u2027\u201B]", APO)



# ---- beloew is important


## Reuires the bigram, trigram frequncy and creates the data frames

# Bigram 
#create a dataframe ( the lists and the word start and ends to the DF)
bigramDF <- data.frame(word=names(bigram_frequency),count=bigram_frequency,stringsAsFactors=FALSE)
#find the start and end of bigrams 
biList <- str_split(bigramDF$word," ")
bigramDF$start <- sapply(biList,FUN=function(x) x[1]) #starting unigram
bigramDF$end <- sapply(biList,FUN=function(x) x[2]) #ending unigram
#setkey(bigramDFkey="start")

### Trigram

#create a dataframe ( the lists and the word start and ends to the DF)
trigramDF <- data.frame(word=names(trigram_frequency),count=trigram_frequency,stringsAsFactors=FALSE)
#expland the trigram DF -- store the starting bigram and the ending word
triList <- str_split(trigramDF$word," ")
trigramDF$start <- sapply(triList,FUN=function(x) paste(x[1],x[2]))
trigramDF$end <- sapply(triList,FUN=function(x) x[3])
#setkey(trigramDF,key="start")

# generate the GoodTuring for the bigram and trigram

biSGTDT <- generateGoodTuringDF(table(bigram_frequency))
triSGTDT <- generateGoodTuringDF(table(trigram_frequency))

# Save the data required for the predictNextWord - this will be loaded in the global.R file
save(bigramDF, trigramDF, biSGTDT, triSGTDT, file = "prepDataforApp.RData")

