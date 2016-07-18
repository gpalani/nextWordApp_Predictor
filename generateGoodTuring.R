### Helper Functions to support the shiny app in predictions

predictNextWord <- function(sentence) {
  # given a sentence/phrase, extract the last two words
  sentence <- stri_replace_all_regex(sentence, "[\u0027\u0060\u00B4\u2019\u2027\u201B]", "")
  sentenceToken <- unlist(str_split(tolower(sentence)," "))
  sentenceLength <- length(sentenceToken)
  
  #if(sentenceLength == 1){return("")}
  
  sentenceBigram <- paste(sentenceToken[sentenceLength-1],sentenceToken[sentenceLength])
  
  # First search the trigram data using the bigram
  tempDT <- trigramDF[trigramDF$start == sentenceBigram,]
  
  # Initiliaze the vector & flags
  predictionsList <- vector()
  trigramFlag <- F
  bigramFlag <- F
  
  # If the bigram is found in the trigram Good Turing data frame 
  if ( nrow(tempDT) > 0 ){
    #get the proabilities 
    tempDT$p <- sapply(tempDT$count,FUN=function(x) triSGTDT$p[triSGTDT$r==x])  
    trigramFlag <- T
    } else {
    
    ## Backoff and look for unigrams in the bigram GT data frame
    bigramFlag <- T
    sentenceUnigram <- sentenceToken[sentenceLength]
    tempDT <- bigramDF[bigramDF$start == sentenceUnigram,]
    #get the proabilities 
    tempDT$p <- sapply(tempDT$count,FUN=function(x) biSGTDT$p[biSGTDT$r==x])
  }
  
  
# get the subset of the ngram data table with a matching n - 1 gram start
# makesure the sentence length is > 0 before the next test..
#    if(sentenceLength > 0){
        if(nrow(tempDT) > 0) {
          
        # sort by probability
        tempDT <- tempDT[order(-tempDT$p),]
        # get the largest probability from the trigram/Bigram data frame
        largestProb = max(tempDT$p)
        
        predictList <- tempDT$end[tempDT$p == largestProb]

        for(i in 1:length(predictList)) {
          predictionsList[i] <- paste(sentence,predictList[i])
        }
    
          returnListofPredictions <- data.frame(words=tempDT$end,probs=tempDT$p)
          return(returnListofPredictions[1:5,])
       } # end of if
        else{
          returnListofPredictions <- data.frame(words=paste("No match for words in",sentence))
          return(returnListofPredictions)
          #print(paste("No match for words in",sentence)
          }
     #} # end of if


} # end of function

generateGoodTuringDF <- function(freqTable){
  
  #Reference - Simple Good Turing Algorithm - Gale And Simpson
  # Acknowledgment
  
  # freqTable -  is a table of frequency of frequencies for bigrams and trigrams
  # In Good Turing Smoothing, we are concerned with the frequency of frequencies
  # extract the number of times that words of frequency n occur in the training set
  # In a tables with a number of holes or non-contiguous sequence of frequency of words,
  # we can compute N(c+1) as the mean of all frequencies that occur more than Nc times
  # to do this, create a vector that is in the numerical form of the names of the table
  
  # create a data table
  # r is the frequencies of various trigrams
  # n is the frequency of frquencies
  
  goodTuringDF <- data.frame(r=as.numeric(names(freqTable)),n=as.vector(freqTable),Z=vector("numeric",length(freqTable)), 
                             logr=vector("numeric",length(freqTable)),
                             logZ=vector("numeric",length(freqTable)),
                             rStar=vector("numeric",length(freqTable)),
                             p=vector("numeric",length(freqTable)))
  
  numOfgtDFRows <- nrow(goodTuringDF)
  for (j in 1:numOfgtDFRows) {
    if(j==1) {r_i<-0} else {r_i <- goodTuringDF$r[j-1]}
    if(j==numOfgtDFRows){r_k<-goodTuringDF$r[j]} else {r_k <- goodTuringDF$r[j+1]}
    goodTuringDF$Z[j] <- 2*goodTuringDF$n[j] / (r_k-r_i)
  }
  
  goodTuringDF$logr <- log(goodTuringDF$r)
  goodTuringDF$logZ <- log(goodTuringDF$Z)
  linearFit <- lm(goodTuringDF$logZ ~ goodTuringDF$logr)
  print(linearFit$coefficients)
  c0 <- linearFit$coefficients[1]
  c1 <- linearFit$coefficients[2]
  
  use_y = FALSE
  for (j in 1:(numOfgtDFRows-1)) {
    r_plus_1 <- goodTuringDF$r[j] + 1
    
    s_r_plus_1 <- exp(c0 + (c1 * goodTuringDF$logr[j+1]))
    s_r <- exp(c0 + (c1 * goodTuringDF$logr[j]))
    y<-r_plus_1 * s_r_plus_1/s_r
    
    if(use_y) {
      goodTuringDF$rStar[j] <- y
    } else { 
      n_r_plus_1 <- goodTuringDF$n[goodTuringDF$r == r_plus_1]
      n_r <- goodTuringDF$n[j]
      x<-(r_plus_1) * n_r_plus_1/n_r
      
      if (abs(x-y) > 1.96 * sqrt(((r_plus_1)^2) * (n_r_plus_1/((n_r)^2))*(1+(n_r_plus_1/n_r)))) {
        goodTuringDF$rStar[j] <- x
      }else {
        goodTuringDF$rStar[j] <- y
        use_y = TRUE
      }
    }
    if(j==(numOfgtDFRows-1)) {
      goodTuringDF$rStar[j+1] <- y
    }
    
  }
  N <- sum(goodTuringDF$n * goodTuringDF$r)
  nHat <- sum(goodTuringDF$n * goodTuringDF$rStar)
  Po <- goodTuringDF$n[1] / N
  goodTuringDF$p <- (1-Po) * goodTuringDF$rStar/nHat
  
  return(goodTuringDF)
  
}


