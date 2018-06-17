# A lookup function: given a string, look up the most probable next word

alt_table <<- NULL

predictWord <- function(string) {
  
  # QDAP clean string
  string <- replace_ordinal(string)
  string <- replace_number(string, remove = TRUE)
  string <- replace_abbreviation(string)
  string <- replace_contraction(string)
  string <- replace_symbol(string, # except for # and @
                           pound = FALSE, # as we have twitter
                           at = FALSE) # texts to process later
  string <- qdap::qprep(string)
  
  # parse string: remove punctiation, lowercase, split into words
  words <- str_to_lower(string) %>%
    str_remove_all(pattern = "[:punct:]") %>%
    str_split(pattern = " ")
  
  # check length
  words_n <- length(words[[1]])
  
  if(words_n >= 5){ start <- words_n - 3 } else start <- 1
  
  end <- words_n
  selection <- words[[1]][start:end]
  str(selection)
  lookup_n <- length(selection)
  
  # What to look up in the lookup table
  print(paste("Lookup: ", selection))
  
  # Lookup ngram in data.table organised by ngram probability
  
  start_level <- lookup_n + 1
  print(paste("Start level:", start_level))
  
  ngram_lookup <- function(selection, level){
    
    backoff <- FALSE
    lookup <- str_c(selection, collapse = " ")
    
    if(level == 5){ # fivegram lookup
      print("Fivegram lookup!")
      
      # lookup fivegram with highest MLE
      prediction <- fivegram_lookup[base == lookup][1]$follow
      
      if(!is.na(prediction)){ # if found
        print("FOUND FIVEGRAM!")
        print("Alternatives:")
        alt_table <<- head(fivegram_lookup[base == lookup], 10)
        print(alt_table)
        return(list(prediction, alt_table))
        break
      }
      else {
        print("BACKING OFF!!")
        level <- 4
        backoff <- TRUE
      }
    }
    
    if(level == 4){ # fourgram lookup
      print("Fourgram lookup!")
      
      if(backoff){
        # shorten search string
        lookup <- str_c(str_split(lookup, pattern = " ")[[1]][2:4], collapse = " ") # remove last word from search string
        last <- str_c(str_split(lookup, pattern = " ")[[1]][4], collapse = " ")
        print(paste("New lookup:", lookup))
        
        # retrieve matching fourgrams
        fourgrams <- fourgram_lookup[base == lookup]
        str(fourgrams)
        print("Old values:")
        print(fourgrams[follow == last, .(base, follow, mle)])
        # discount MLE of failed fivegram
        fourgrams[follow == last, mle := 0.4 * mle] 
        print("New values:")
        print(fourgrams[follow == last, .(base, follow, mle)])
      }
      else {
        fourgrams <- fourgram_lookup[base == lookup]
      }
      
      # lookup fourgram with highest MLE
      prediction <- fourgrams[1]$follow
      print(paste("Prediction:", prediction))
      
      if(!is.na(prediction)){
        print("FOUND FOURGRAM!")
        print("Alternatives:")
        alt_table <<- head(fourgrams[base == lookup], 10)
        print(alt_table)
        return(list(prediction, alt_table))
        break
      }
      else {
        level <- 3
        backoff <- TRUE
      }
      
    }
    
    if(level == 3){ # trigram lookup
      print("Trigram lookup!")
      
      if(backoff){
        # shorten search string
        lookup <- str_c(str_split(lookup, pattern = " ")[[1]][2:3], collapse = " ") # remove last word from search string
        last <- str_c(str_split(lookup, pattern = " ")[[1]][3], collapse = " ")
        print(paste("New lookup:", lookup))
        
        # retrieve matching trigrams
        trigrams <- trigram_lookup[base == lookup]
        str(trigrams)
        print("Old values:")
        print(trigrams[follow == last, .(base, follow, mle)])
        # discount MLE of failed fourgram
        trigrams[follow == last, mle := 0.4 * mle]
        print("New values:")
        print(trigrams[follow == last, .(base, follow, mle)])
      }
      else {
        trigrams <- trigram_lookup[base == lookup]
      }
      
      # lookup trigram with highest MLE
      prediction <- trigrams[1]$follow
      print(paste("Prediction:", prediction))
      
      if(!is.na(prediction)){
        print("FOUND TRIGRAM!")
        print("Alternatives:")
        alt_table <<- head(trigrams[base == lookup], 10)
        print(alt_table)
        return(list(prediction, alt_table))
        break
      }
      else {
        level <- 2
        backoff <- TRUE
      }
    }
    
    if(level == 2){ # bigram lookup
      print("Bigram lookup!")
      
      if(backoff){
        # shorten search string
        lookup <- str_c(str_split(lookup, pattern = " ")[[1]][1:1], collapse = " ") # remove last word from search string
        last <- str_c(str_split(lookup, pattern = " ")[[1]][2], collapse = " ")
        print(paste("New lookup:", lookup))
        
        # retrieve matching bigrams
        bigrams <- bigram_lookup[base == lookup]
        print("Old values:")
        print(bigrams[follow == last, .(base, follow, mle)])
        
        # discount MLE of failed trigram
        bigrams[follow == last, mle := 0.4 * mle]
        print("New values:")
        print(bigrams[follow == last, .(base, follow, mle)])
      }
      else {
        bigrams <- bigram_lookup[base == lookup]
      }
      
      # lookup bigram with highest MLE
      prediction <- bigrams[1]$follow
      print(paste("Prediction:", prediction))
      
      if(!is.na(prediction)){
        print("FOUND BIGRAM!")
        print("Alternatives:")
        alt_table <<- head(bigrams[base == lookup], 10)
        print(alt_table)
        return(list(prediction, alt_table))
        break
      }
      else {
        print("PLEASE ENTER SOME MORE WORDS!!")
      }
    }
    
  } # end of ngram_lookup function
  
  ngram_lookup(selection, start_level)
  
} # end of predictWord function