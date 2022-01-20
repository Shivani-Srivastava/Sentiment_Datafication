

library(shiny)
library(tidytext)
library(dplyr)
library(tidyverse)
library(stringr)
library(ggplot2)
library(plotly)
library(tools)
library(wordcloud)
library(tibble)

senti_df = read.csv("https://raw.githubusercontent.com/sudhir-voleti/sample-data-sets/master/sentiments.csv")

afinn_senti = senti_df %>% filter(., lexicon == "AFINN") %>%
  select(X, word, score) %>% rename("value"="score")

nrc_senti = senti_df %>% filter(., lexicon == "nrc") %>%
  select(X, word, sentiment) 

nrc_emots = table(nrc_senti$sentiment) %>% names(); nrc_emots # list of 10

split_corpus <- function(corpus0, # raw chronological corpus
                         token0){ # split type among c("sentence", "paragraphs", "lines") 
  
  corpus0 = data.frame(txt = as.character(corpus0))
  
  if (token0 == "lines"){
    a01 = corpus0 %>% str_split(., "\n") %>%
     unlist(.) %>% data.frame(sentence = .) %>%
    filter(., sentence != "") # drop empty rows 
  }
  else{
    a01 = unnest_tokens(corpus0, sentence, txt, token=token0, to_lower = FALSE) # 0s!
  }
  
  # define empty output DF
  a02 = data.frame(percentile = numeric(100), 
                   txt_percentile = character(100), 
                   senti_score=numeric(100))
  
  # main loop
  if (nrow(a01) > 100){
    
    k0 = round(nrow(a01)/100, 0)
    for (i0 in 1:99){  # upto 99 only. last will take whatever is left
      a02$percentile[i0] = i0
      start0 = (i0-1)*k0 + 1; stop0 = (i0*k0); start0; stop0
      a02$txt_percentile[i0] = a01$sentence[start0:stop0] %>% str_c(., collapse=" ")
    } # i0 loop ends
    a02$txt_percentile[100] = a01$sentence[(99*k0+1):nrow(a01)] %>% str_c(., collapse=" ")    
    
  } else {
    
    for (i0 in 1:nrow(a01)){
      
      a02$percentile[i0] = i0
      a02$txt_percentile[i0] = a01$sentence[i0] # %>% str_c(., collapse=" ")
      a02 = a02[1:nrow(a01),]   # drop trailing empty rows
    } # i0 loop ends
    
  } # else loop ends.
  
  
  
  return(a02) }  # split_corpus func ends


afinn_score <- function(subcorpus0, afinn_senti){
  
  b0 = tibble(txt = subcorpus0) %>% unnest_tokens(word, txt) %>%
    inner_join(afinn_senti) %>% select(word, value)
  
  if (nrow(b0)>0) {
    b1 = b0 %>% select(value) %>% sum(.)
    b0a = str_c(b0$word, collapse=" ") %>% str_replace_all(., " ", ", ")
  } else {b1 = 0; b0a = "none"}
  
  return(list(b0a, b1))  }

# below for NRC
nrc_score <- function(subcorpus0, nrc_senti, emots0="joy"){
  
  b1 = tibble(txt = subcorpus0) %>% unnest_tokens(word, txt) %>%
    
    inner_join(nrc_senti) %>% filter(sentiment == emots0)
  
  if (nrow(b1)>0) {
    b2 = nrow(b1);
    b1a = str_c(b1$word, collapse=" ") %>% str_replace_all(., " ", ", "); b1a
  } else {b2 = 0; b1a = "none"}
  
  return(list(b1a, b2)) } # func ends



split2ggplotly <- function(a00){ # , lexicon0="nrc", emots0="joy"
  
  # build ggplot object
  p <- ggplot(a00, aes(x=percentile, y=senti_score)) +
    
    # below extracts only first 150 chars to display in plotly
    geom_point(aes(text = senti_tooltip), color="black") + 
    
    # build line and fill area below it
    geom_line(color="black", lty=1) +
    geom_area(aes(y=senti_score), fill="light blue") +
    
    # build neutral wala line
    geom_hline(aes(yintercept=0), color="black") 
  
  gply0 = ggplotly(p, tooltip = c("x", "y", "text"))    
  
  return(gply0)  } # split2ggplotly func ends

# display plot

dtm_build <- function(raw_corpus, tfidf=FALSE)
{                  # func opens
  
  require(tidytext); require(tibble); require(tidyverse)
  
  # converting raw corpus to tibble to tidy DF
  textdf = data_frame(text = raw_corpus);    textdf  
  
  tidy_df = textdf %>%   
    mutate(doc = row_number()) %>%
    unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>%
    group_by(doc) %>%
    count(word, sort=TRUE)
  tidy_df
  
  # evaluating IDF wala DTM
  if (tfidf == "TRUE") {
    textdf1 = tidy_df %>% 
      group_by(doc) %>% 
      count(word, sort=TRUE) %>% ungroup() %>%
      bind_tf_idf(word, doc, nn) %>%   # 'nn' is default colm name
      rename(value = tf_idf)} else { textdf1 = tidy_df %>% rename(value = n)  } 
  
  textdf1
  
  dtm = textdf1 %>% cast_sparse(doc, word, value);    dtm[1:9, 1:9]
  
  # order rows and colms putting max mass on the top-left corner of the DTM
  colsum = apply(dtm, 2, sum)    
  col.order = order(colsum, decreasing=TRUE)
  row.order = order(rownames(dtm) %>% as.numeric())
  
  dtm1 = dtm[row.order, col.order];    dtm1[1:8,1:8]
  
  return(dtm1)  } 

dtm.word.count <- function(dtm) {
  
  if (ncol(dtm) > 1000) {
    tst = round(ncol(dtm)/100)  # divide DTM's cols into 100 manageble parts
    a = rep(tst,99)
    b = cumsum(a);rm(a)
    b = b[-which(b >= ncol(dtm))]
    b = c(0,b,ncol(dtm))
    
    ss.col = c(NULL)
    for (i in 1:(length(b)-1)) {
      tempdtm = dtm[,(b[i]+1):(b[i+1])]
      s = colSums(as.matrix(tempdtm))
      ss.col = c(ss.col,s)
    }
  } else {
    ss.col = colSums(as.matrix(dtm))
  }
  
  tsum = ss.col
  # tsum = tsum[order(tsum, decreasing = T)]       #terms in decreasing order of freq
  return(tsum)
}


dtm.word.cloud <- function(count = count, max.words = 100,title = "Title"){
  
  if (class(count)[1] == "DocumentTermMatrix"|class(count)[1] == "simple_triplet_matrix")
  {
    tsum = dtm.word.count(count)
  } else {
    tsum = count
  }
  
  if (class(tsum) != "numeric") stop("Give input as wordcount or DocumentTermMatrix")
  
  wordcloud(names(tsum), tsum,     # words, their freqs 
            scale = c(4, 1),     # range of word sizes
            min.freq = .01,                     # min.freq of words to consider
            max.words = max.words,       # max #words
            colors = brewer.pal(8, "Dark2"))    # Plot results in a word cloud 
  title(sub = title)     # title for the wordcloud display
}   

calc_senti_score <- function(corpus0,  # raw chronological corpus
                             token0,    # split type among c("sentence", "paragraphs", "lines")
                             lexicon0 ="afinn",  # afinn or nrc
                             afinn_senti, # downloaded from git
                             nrc_senti,   # downloaded from git
                             emots0="joy"){
  
  corpus0 = data.frame(txt=as.character(corpus0))
  
  a02 = split_corpus(corpus0, token0)
  
  #lexicon0 = unlist(lexicon0)
  
  
  if (lexicon0 != "afinn") {
    
    for (i in 1:nrow(a02)){
      a02a = suppressMessages(nrc_score(a02$txt_percentile[i], nrc_senti, emots0))
      a02$senti_score[i] = a02a[[2]] %>% as.numeric()
      a02$senti_tooltip[i] = a02a[[1]] } # i loop ends
    
      } else {
    
        for (i in 1:nrow(a02)){
          a02a = suppressMessages(afinn_score(a02$txt_percentile[i], afinn_senti))
          a02$senti_score[i] = a02a[[2]] %>% as.numeric()
          a02$senti_tooltip[i] = a02a[[1]]   } # i loop ends

  } # else ends
  
  p <- split2ggplotly(a02)
  
  outp_list = list(a02, p)
  
  return (outp_list)} # func ends

# test-drive abv func
#system.time({
#outp_list = calc_senti_score(speech, "lines", "afinn", afinn_senti, nrc_senti, "trust")  
#}); head(a00) # 1.25s full corpus


