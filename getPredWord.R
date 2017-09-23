setwd("F:\\R\\capston_proj\\Data\\en_US")


load("bigrams.RData") 
load("trigrams.RData") 
load("quadgrams.RData")

getPredWord <- function(phraseIn, wordsOut = 5, flag = TRUE) { 
  
  lastWord <- NA 
  #print("phraseIn")
  in_token <- tokens(phraseIn)
  numWords <- length(in_token$text1)
  phraseIn <- gsub(",", " ", phraseIn)
  phraseIn <- gsub("  ", " ", phraseIn)
  #print(numWords)
  
  # If user input is blank space or is filtered out, return common unigrams 
  # if (numWords==0){ 
  #     lastWord <- as.data.frame(as.character(data1gram$word[1:wordsOut])) 
  #     return(lastWord) 
  #   } 
  
  if (numWords > 3) { 
    startp <- numWords-3
    newStr <- in_token$text1[startp+1:numWords]
    
    newStr1 <- newStr[1:3]
    phraseIn <- toString(newStr1)
    phraseIn <- gsub(",", " ", phraseIn)
    
    in_token <- tokens(phraseIn)
    #print(in_token)
    numWords <- length(in_token$text1)
    #print(numWords)
  }
  
  # searching 3 words in quadgram
  if (numWords == 3) { 
    
    #print("in quad")
    #print(phraseIn)
    phraseIn <- gsub("  ", " ", phraseIn)
    
    pos<-grep(phraseIn,quadgrams[, 1],ignore.case = TRUE) 
    
    #print(pos)
    length(pos)
    if (length(pos) > 0) {
      
      str<- quadgrams[pos, 2]
      
      #result<-cbind("text"=str,"freq"=1)
      result<-str
      #print("quad run")
      
    }
    else {  newStr1 <- newStr[2:3]
    phraseIn <- toString(newStr1)
    phraseIn <- gsub(",", " ", phraseIn)
    phraseIn <- gsub("  ", " ", phraseIn)
    
    in_token <- tokens(phraseIn)
    #print(in_token)
    numWords <- length(in_token$text1)
    #print(numWords) 
    
    }
  }
  # searching 2 words in trigram
  if (numWords == 2) { 
    
    
    phraseIn <- gsub("  ", " ", phraseIn)
    #print(phraseIn)
    pos<-grep(phraseIn,trigrams[, 1],ignore.case = TRUE) 
    #print(pos)
    
    length(pos)
    if (length(pos) > 0) {
      
      str<- trigrams[pos, 2]
      
      #result<-cbind("text"=str,"freq"=1)
      result<-str
      #print("tri run")
    }
    else { 
      
      newStr1 <- in_token$text1[2:2]
      
      #newStr1 <- newStr[1:3]
      phraseIn <- toString(newStr1)
      phraseIn <- gsub(",", " ", phraseIn)
      
      in_token <- tokens(phraseIn)
      #print(in_token)
      numWords <- length(in_token$text1)
      #print(numWords)  
    }
  }
  
  # searching 1 words in bigram
  if (numWords == 1) {
    #print("bi run")
    #print(phraseIn)
    pos<-grep(phraseIn,bigrams[, 1],ignore.case = TRUE) 
    
    length(pos)
    if (length(pos) > 0) {
      
      str<- bigrams[pos, 2]
      
      #result<-cbind("text"=str,"freq"=1)
      result<-str
      
    } 
  }
  if (is.null(result)) {
    phraseIn_new<-substr(phraseIn,regexpr(" ", phraseIn)+1,nchar(phraseIn))
    if(phraseIn_new != phraseIn) {getPredWord(phraseIn_new)}
    else return("No Results")
  }
  else {
    
    #colnames(result)<-c("nextword","freq")
    result<-head(result,5)
    
    return(result)
    
  }
}

getPredWord("i love") #to foolishly pick

