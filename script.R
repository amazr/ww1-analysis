library(tidyverse)
library(gutenbergr)
library(tidytext)
library(textdata)
library(pdftools)
library(widyr)
library(ggpubr)
library(tm)
library(eply)
data("stop_words")
SMART <- stop_words %>% filter(lexicon == "SMART")
BING <- get_sentiments("bing")

####################
## Create Corpora
####################

Pre_1<- gutenberg_download(3659)%>% 
  mutate(title = "The Rosary")%>% 
  mutate(author = "Florence L. Barclay")
Pre_1 <- Pre_1[67:11606,]

Pre_2<- gutenberg_download(5382)%>% 
  mutate(title = "A Modern Chronicle")%>% 
  mutate(author = "Winston Churchill")
Pre_2 <- Pre_2[78:19465,]

Pre_3<- gutenberg_download(13212)%>% 
  mutate(title = "The Wild Olive")%>% 
  mutate(author = "Basil King")
Pre_3 <- Pre_3[49:10592,]

Pre_4<- gutenberg_download(13054)%>% 
  mutate(title = "Max")%>% 
  mutate(author = "Katherine Cecil Thurston")
Pre_4 <- Pre_4[18:7143,]

Pre_5<- gutenberg_download(42427)%>% 
  mutate(title = "The Kingdom of Slender Swords")%>% 
  mutate(author = "Hallie Erminie Rives")
Pre_5 <- Pre_5[266:10952,]

Pre_6<- gutenberg_download(5257)%>% 
  mutate(title = "The Broad Highway")%>% 
  mutate(author = "Jeffrey Farnol")
Pre_6 <- Pre_6[23:20607,]

Pre_7<- gutenberg_download(5129)%>% 
  mutate(title = "The Prodigal Judge")%>% 
  mutate(author = "Vaughan Kester")
Pre_7 <- Pre_7[12:14108,]

Pre_8<- gutenberg_download(6997)%>% 
  mutate(title = "The Winning of Barbara Worth")%>% 
  mutate(author = "Harold Bell Wright")
Pre_8 <- Pre_8[167:14370,]

Pre_9<- gutenberg_download(14303)%>% 
  mutate(title = "Queed")%>% 
  mutate(author = "Henry Sydnor Harrison")
Pre_9 <- Pre_9[209:16250,]

Pre_10<- gutenberg_download(349)%>% 
  mutate(title = "The Harvester")%>% 
  mutate(author = "Gene Stratton Porter")
Pre_10 <- Pre_10[69:15941,]

Pre_11<- gutenberg_download(14394)%>% 
  mutate(title = "The Street Called Straight")%>% 
  mutate(author = "Basil King")
Pre_11 <- Pre_11[42:12113,]

Pre_12<- gutenberg_download(6105)%>% 
  mutate(title = "Their Yesterdays")%>% 
  mutate(author = "Harold Bell Wright")
Pre_12 <- Pre_12[38:6605,]

Pre_13<- gutenberg_download(15817)%>% 
  mutate(title = "The Melting of Molly")%>% 
  mutate(author = "Maria Thompson Davies")
Pre_13 <- Pre_13[82:2939,]

Pre_14<- gutenberg_download(15138)%>% 
  mutate(title = "A Hoosier Chronicle")%>% 
  mutate(author = "Meredith Nicholson")
Pre_14 <- Pre_14[46:16822,]

Pre_15<- gutenberg_download(14581)%>% 
  mutate(title = "The Just and the Unjust")%>% 
  mutate(author = "Vaughan Kester")
Pre_15 <- Pre_15[76:11629,]

Pre_16<- gutenberg_download(5364)%>% 
  mutate(title = "The Inside of the Cup")%>% 
  mutate(author = "Winston Churchill")
Pre_16 <- Pre_16[72:19286,]

Pre_17<- gutenberg_download(13985)%>% 
  mutate(title = "V.V.'s Eyes")%>% 
  mutate(author = "Henry Sydnor Harrison")
Pre_17 <- Pre_17[261:20979,]

Pre_18<- gutenberg_download(286)%>% 
  mutate(title = "Laddie")%>% 
  mutate(author = "Gene Stratton Porter")
Pre_18 <- Pre_18[78:16198,]

Pre_19<- gutenberg_download(3746)%>% 
  mutate(title = "The Judgment House")%>% 
  mutate(author = "Gilbert Parker")
Pre_19 <- Pre_19[120:16598,]

Pre_20<- gutenberg_download(5145)%>% 
  mutate(title = "Heart of the Hills")%>% 
  mutate(author = "John Fox Jr.")
Pre_20 <- Pre_20[41:9669,]

Pre_21<- gutenberg_download(11715)%>% 
  mutate(title = "The Eyes of the World")%>% 
  mutate(author = "Harold Bell Wright")
Pre_21 <- Pre_21[149:12709,]

Pre_22<- gutenberg_download(1450)%>% 
  mutate(title = "Pollyanna")%>% 
  mutate(author = "Eleanor H. Porter")
Pre_22 <- Pre_22[63:7444,]

Pre_23<- gutenberg_download(36355)%>% 
  mutate(title = "The Salamander")%>% 
  mutate(author = "Owen Johnson")
Pre_23 <- Pre_23[444:16691,]

Pre_24<- gutenberg_download(4379)%>% 
  mutate(title = "The Fortunate Youth")%>% 
  mutate(author = "William J. Locke")
Pre_24 <- Pre_24[16:11440,]

Pre_25<- gutenberg_download(2514)%>% 
  mutate(title = "T. Tembarom")%>% 
  mutate(author = "Frances Hodgson Burnett")
Pre_25 <- Pre_25[11:20384,]


Post_1<- gutenberg_download(1484)%>% 
  mutate(title = "The Four Horseman of the Apocalypse")%>% 
  mutate(author = "Vicente Blasco Ibáñez")

Post_1<- Post_1[48:15049,]

Post_2 <-  gutenberg_download(1083) %>% 
  mutate(title = "The Arrow of Gold")%>% 
  mutate(author = "Joseph Conrad")

Post_2 <- Post_2[156:10795, ]

Post_3 <-  gutenberg_download(10201) %>% 
  mutate(title = "The Desert of Wheat")%>% 
  mutate(author = "Zane Grey")

Post_3 <- Post_3[17:13845, ]

Post_4 <-  gutenberg_download(1693) %>% 
  mutate(title = "Dangerous Days")%>% 
  mutate(author = "Mary Roberts Rinehar")

Post_4 <- Post_4[8:15528, ]

Post_5 <-  gutenberg_download(3288) %>% 
  mutate(title = "The Sky Pilot in No Man's Land")%>% 
  mutate(author = "Ralph Connor")

Post_5 <- Post_5[62:13343, ]

Post_6 <-  gutenberg_download(3457) %>% 
  mutate(title = "The Man of the Forest")%>% 
  mutate(author = "Zane Grey")

Post_6 <- Post_6[17:14857, ]

Post_7 <-  gutenberg_download(13532) %>% 
  mutate(title = "Kindred of the Dust")%>% 
  mutate(author = "Peter B. Kyne")

Post_7 <- Post_7[48:11460, ]

## NOTE that "Kent" is really short compared to the rest of the novels I
## have looked at so far. This might be a point of contention, but I'll
## leave that up to the group to discuss later to see if we want to 
## include a different novel from the same year that is a little longer than this
Post_8 <-  gutenberg_download(3256) %>% 
  mutate(title = "The Re-Creation of Brian Kent")%>% 
  mutate(author = "Harold Bell Wrigh")

Post_8 <- Post_8[21:733, ]

Post_9 <-  gutenberg_download(4747) %>% 
  mutate(title = "The River's End")%>% 
  mutate(author = "James Oliver Curwood")

Post_9 <- Post_9[13:5612, ]

Post_10 <-  gutenberg_download(17237) %>% 
  mutate(title = "A Man for the Ages")%>% 
  mutate(author = "Irving Bacheller")

Post_10 <- Post_10[276:11673, ]

Post_11 <-  gutenberg_download(543) %>% 
  mutate(title = "Main Street")%>% 
  mutate(author = "Sinclair Lewis")

Post_11 <- Post_11[41:19628, ]

Post_12 <-  gutenberg_download(19457) %>% 
  mutate(title = "The Brimming Cup")%>% 
  mutate(author = "Dorothy Canfield")

Post_12 <- Post_12[111:8205, ]

Post_13 <-  gutenberg_download(13937) %>% 
  mutate(title = "The Mysterious Rider")%>% 
  mutate(author = "Zane Grey")

Post_13 <- Post_13[50:11704, ]

Post_14 <-  gutenberg_download(541) %>% 
  mutate(title = "The Age of Innocence")%>% 
  mutate(author = "Edith Wharton")

Post_14 <- Post_14[18:11540, ]

Post_15 <-  gutenberg_download(4707) %>% 
  mutate(title = "The Valley of Silent Men")%>% 
  mutate(author = "James Oliver Curwood")

Post_15 <- Post_15[26:7499, ]

Post_16 <- gutenberg_download(14145) %>% 
  mutate(title = "If Winter Comes")%>% 
  mutate(author = "A.S.M. Hutchinson")

Post_16 <- Post_16[80:13190, ]

Post_17 <- gutenberg_download(7031) %>% 
  mutate(title = "The Sheik")%>% 
  mutate(author = "Edith M. Hull")

Post_17 <- Post_17[16:8446, ]

Post_18 <- gutenberg_download(18259) %>% 
  mutate(title = "Gentle Julia")%>% 
  mutate(author = "Booth Tarkington")

Post_18 <- Post_18[56:8832, ]

Post_19 <- gutenberg_download(6491) %>% 
  mutate(title = "The Head of the House of Coombe")%>% 
  mutate(author = "Frances Hodgson Burnett")

Post_19 <- Post_19[14:12883, ]

Post_20 <- gutenberg_download(14579) %>% 
  mutate(title = "Simon Called Peter")%>% 
  mutate(author = "Robert Keable")

Post_21 <- gutenberg_download(25542) %>% 
  mutate(title = "Black Oxen")%>% 
  mutate(author = "Gertrude Atherton")
Post_21 <- Post_21[78:13177, ]

Post_22 <- pdf_text("His_childrens_children.pdf") %>% 
  readr::read_lines()
Post_22 <- Post_22[81:13470]
Post_22 <- tibble(Post_22) %>% 
  mutate(gutenberg_id = 0)%>% 
  mutate(title = "His Children's Children")%>% 
  mutate(author = "Arthur Train")  
Post_22 <- Post_22[, c(2,1,3,4)]
names(Post_22)[2] <- "text"


Post_23 <- gutenberg_download(16389) %>% 
  mutate(title = "The Enchanted April")%>% 
  mutate(author = "Elizabeth von Arnim")
Post_23 <- Post_23[13:8833, ]

Post_24 <- gutenberg_download(1156) %>% 
  mutate(title = "Babbitt")%>% 
  mutate(author = "Sinclair Lewis")

Post_24 <- Post_24[16:14178, ]

Post_25 <- gutenberg_download(60090) %>% 
  mutate(title = "The Dim Lantern")%>% 
  mutate(author = "Temple Bailey")

Post_25 <- Post_25[118:10276, ]

## Function to unnest tokens and remove stop words
clean <- function(corpus)
{
  unnested_tokens <- corpus %>% unnest_tokens(word, text)
  return (unnested_tokens %>% anti_join(SMART))
}

## Build the final two corpora
Pre_Corpora <- rbind(Pre_1,Pre_2,Pre_3,Pre_4,Pre_5,Pre_6,Pre_7,Pre_8,Pre_9,Pre_10,Pre_11,Pre_12,Pre_13,Pre_14,Pre_15,Pre_16,Pre_17,Pre_18,Pre_19,Pre_20,Pre_21,Pre_22,Pre_23,Pre_24,Pre_25)
Post_Corpora <- rbind(Post_1,Post_2,Post_3,Post_4,Post_5,Post_6,Post_7,Post_8,Post_9,Post_10,Post_11,Post_12,Post_13,Post_14,Post_15,Post_16,Post_17,Post_18,Post_19,Post_20,Post_21,Post_22,Post_23,Post_24,Post_25)
Clean_Pre_Corpora <- clean(Pre_Corpora)
Clean_Post_Corpora <- clean(Post_Corpora)

#######################
## Functions
#######################

## Returns the top 15 words of a given emotion
emotion_Count <- function(corpus, emotion_list)
{
  temp <- corpus%>%
    inner_join(emotion_list)%>%
    count(word, sort = TRUE)%>%
    top_n(15)
  return(temp)
}

# Returns the percentage of fear words in the corpus
emotion_Percent <- function(corpus,emotion_list)
{
  temp <- corpus%>%
    inner_join(emotion_list)
  return((nrow(temp)/nrow(corpus))*100)
}

## Returns 
get_AFINN_Sentiment <- function(corpus, lexicon)
{
  temp <- corpus%>%
    inner_join(lexicon)%>%
    group_by(title)%>%
    summarise(sentiment = sum(value))%>%
    arrange(sentiment, .by_group = TRUE)
  return(temp)
}

get_Contributing_Negative_Words <- function(corpus)
{
  temp <- corpus%>%
    inner_join(BING)%>%
    count(word, sentiment)%>%
    group_by(sentiment)%>%
    top_n(15)%>%
    ungroup()%>%
    mutate(word = reorder(word,n))%>%
    group_by(sentiment)%>%
    arrange(desc(n), .by_group = TRUE)
  return(temp)
}

create_Word_Prob_Corpus <- function(corpus)
{
  
  temp <- corpus%>%
    mutate(lineID = row_number())%>%
    unnest_tokens(word, text, token ="ngrams", n=1)%>%
    count(word, sort = TRUE)%>%
    mutate(p = n/sum(n))
  return(temp)
}

create_Skipgrams <- function(corpus)
{
  temp <- corpus%>%
    unnest_tokens(ngram, text, token = "ngrams", n = 8) %>%     # create 8-grams
    mutate(ngramID = row_number()) %>%                          # create an index of each 8-gram
    mutate(skipgramID = ngramID)%>%                       # create a nex cobined index joining lineID and ngramID
    unnest_tokens(word, ngram)                                  # re-tokenize the text by word
  return(temp)
}

word_Occur_Prob <- function(corpus)
{
  temp <- corpus%>%
    pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE)%>%   # counting word co-occurence
    mutate(p= n/sum(n))
  return(temp)
}

get_Normalized_Prob <- function(corpus, corpus_prob)
{
  temp <- corpus %>%
    filter(n>10) %>%                            # select word pairs that occur more than 10 times
    rename(word1 = item1, word2 = item2) %>%    # rename the columns of data for clarity
    left_join(corpus_prob %>%              # use a left_join() to add probabilties
                select(word1 = word, p1 = p),   # instructing r which columns of data to match
              by = "word1") %>%
    left_join(corpus_prob %>%              # now do the same thing for word #2
                select(word2 = word, p2 = p),
              by = "word2") %>%
    mutate(p_together = p/p1/p2) #determining probability of words appearing together
  return(temp)
}  

get_Skipgrams <- function(corpus)
{
  corpus_prob <- create_Word_Prob_Corpus(corpus)
  skipgrams <- create_Skipgrams(corpus)
  skipgram_prob <- word_Occur_Prob(skipgrams)
  normalized_skipgrams <- get_Normalized_Prob(skipgram_prob, corpus_prob)
  return(normalized_skipgrams)
}

get_Female_Probs <- function(normalized_corpus)
{
  female_probs <- normalized_corpus%>%
    filter(word1 == "she" | word1 == "her" | word1 == "hers" | word1 == "woman" | word1 == "girl")%>%
    arrange(-p_together)
  female_probs = female_probs[-1,c(2,7)]%>%
    top_n(25)
  return(female_probs)
}

get_Word_Probs <- function(normalized_corpus,word)
{
  this_probs <- normalized_corpus%>%
    filter(word1 == word & word2 != word)%>%
    arrange(-p_together)
  this_probs = this_probs[-1,c(2,7)]%>%
    top_n(25)
  return(this_probs)
}


######################
## Corpora analysis
######################

# Emotion Percentages
nrc <- lexicon_nrc()
nrc_fear <- nrc%>%
  filter(sentiment == "fear")
nrc_sadness <- nrc%>%
  filter(sentiment == "sadness")
nrc_anger <- nrc%>%
  filter(sentiment == "anger")

pre_fear_percent <- emotion_Percent(Clean_Pre_Corpora, nrc_fear)
post_fear_percent <- emotion_Percent(Clean_Post_Corpora, nrc_fear) 

pre_sadness_percent <- emotion_Percent(Clean_Pre_Corpora, nrc_sadness)
post_sadness_percent <- emotion_Percent(Clean_Post_Corpora, nrc_sadness)

pre_anger_percent <- emotion_Percent(Clean_Pre_Corpora, nrc_anger)
post_anger_percent <- emotion_Percent(Clean_Post_Corpora, nrc_anger) 

#AFINN Sentiment
AFINN <- lexicon_afinn()
pre_AFINN_sentiment <- get_AFINN_Sentiment(Clean_Pre_Corpora, AFINN)
post_AFINN_sentiment <- get_AFINN_Sentiment(Clean_Post_Corpora, AFINN)

#Negative Words
pre_top_negative <- get_Contributing_Negative_Words(Clean_Pre_Corpora)
post_top_negative <- get_Contributing_Negative_Words(Clean_Post_Corpora)

#Create Probability Corporas
Pre_Skipgrams <- get_Skipgrams(Pre_Corpora)
Post_Skipgrams <- get_Skipgrams(Post_Corpora)

#Get skipgrams based on the word "war"
Pre_Wargram <- get_Word_Probs(Pre_Skipgrams,"war")
Post_Wargram <- get_Word_Probs(Post_Skipgrams,"war")

#Get skipgrams based on the word "death"
Pre_Deathgram <- get_Word_Probs(Pre_Skipgrams,"death")
Post_Deathgram <- get_Word_Probs(Post_Skipgrams,"death")

Pre_Femalegram <- get_Female_Probs(Pre_Skipgrams)
Post_Femalegram <- get_Female_Probs(Post_Skipgrams)

Pre_Race <- get_Word_Probs(Pre_Skipgrams,"race")
Post_Race <- get_Word_Probs(Post_Skipgrams,"race")

Pre_American <- get_Word_Probs(Pre_Skipgrams,"american")
Post_American <- get_Word_Probs(Post_Skipgrams,"american")
View(Pre_American)
View(Post_American)

Pre_german <- get_Word_Probs(Pre_Skipgrams,"german")
Post_german <- get_Word_Probs(Post_Skipgrams,"german")
View(Pre_german)
View(Post_german)

Pre_American <- get_Word_Probs(Pre_Skipgrams,"america")
Post_American <- get_Word_Probs(Post_Skipgrams,"america")
View(Pre_America)
View(Post_America)

Pre_germany <- get_Word_Probs(Pre_Skipgrams,"germany")
Post_germany <- get_Word_Probs(Post_Skipgrams,"germany")
View(Pre_germany)
View(Post_germany)


######################
## GGplots
######################

#WARARARAWAWRARRWAWR
ggplot(Pre_Wargram)+  #tells which data to use
  aes(x=reorder(word2,-p_together,sum), y=p_together)+ #Using the word2 and probability data
  coord_flip()+
  geom_col(color="firebrick4", fill="lightcoral")+ #extra code to make the graph colorful
  ggtitle("Pre War SkipGram")+ #adding a title
  ylab("Probability of Close Occurrence")+ #adding a y label
  theme(plot.title = element_text(hjust = 0.5))+ #centering title
  xlab(NULL)

ggplot(Post_Wargram)+  #tells which data to use
  aes(x=reorder(word2,-p_together,sum), y=p_together)+ #Using the word2 and probability data
  coord_flip()+
  geom_col(color="firebrick4", fill="lightcoral")+ #extra code to make the graph colorful
  ggtitle("Post War SkipGram")+ #adding a title
  ylab("Probability of Close Occurrence")+ #adding a y label
  theme(plot.title = element_text(hjust = 0.5))+ #centering title
  xlab(NULL)


#DEATH!!!!!!!!!!!!
ggplot(Pre_Deathgram)+  #tells which data to use
  aes(x=reorder(word2,-p_together,sum), y=p_together)+ #Using the word2 and probability data
  coord_flip()+
  geom_col(color="firebrick4", fill="lightcoral")+ #extra code to make the graph colorful
  ggtitle("Pre Death SkipGram")+ #adding a title
  ylab("Probability of Close Occurrence")+ #adding a y label
  theme(plot.title = element_text(hjust = 0.5))+ #centering title
  xlab(NULL)

ggplot(Post_Deathgram)+  #tells which data to use
  aes(x=reorder(word2,-p_together,sum), y=p_together)+ #Using the word2 and probability data
  coord_flip()+
  geom_col(color="firebrick4", fill="lightcoral")+ #extra code to make the graph colorful
  ggtitle("Post Death SkipGram")+ #adding a title
  ylab("Probability of Close Occurrence")+ #adding a y label
  theme(plot.title = element_text(hjust = 0.5))+ #centering title
  xlab(NULL)

# Female Pronouns
ggplot(Pre_Femalegram)+  #tells which data to use
  aes(x=reorder(word2,-p_together,sum), y=p_together)+ #Using the word2 and probability data
  coord_flip()+
  geom_col(color="firebrick4", fill="lightcoral")+ #extra code to make the graph colorful
  ggtitle("Pre Female SkipGram")+ #adding a title
  ylab("Probability of Close Occurrence")+ #adding a y label
  theme(plot.title = element_text(hjust = 0.5))+ #centering title
  xlab(NULL)

ggplot(Post_Femalegram)+  #tells which data to use
  aes(x=reorder(word2,-p_together,sum), y=p_together)+ #Using the word2 and probability data
  coord_flip()+
  geom_col(color="firebrick4", fill="lightcoral")+ #extra code to make the graph colorful
  ggtitle("Post Female SkipGram")+ #adding a title
  ylab("Probability of Close Occurrence")+ #adding a y label
  theme(plot.title = element_text(hjust = 0.5))+ #centering title
  xlab(NULL)

#Race
ggplot(Pre_Race)+  #tells which data to use
  aes(x=reorder(word2,-p_together,sum), y=p_together)+ #Using the word2 and probability data
  coord_flip()+
  geom_col(color="firebrick4", fill="lightcoral")+ #extra code to make the graph colorful
  ggtitle("Pre Race SkipGram")+ #adding a title
  ylab("Probability of Close Occurrence")+ #adding a y label
  theme(plot.title = element_text(hjust = 0.5))+ #centering title
  xlab(NULL)

ggplot(Post_Race)+  #tells which data to use
  aes(x=reorder(word2,-p_together,sum), y=p_together)+ #Using the word2 and probability data
  coord_flip()+
  geom_col(color="firebrick4", fill="lightcoral")+ #extra code to make the graph colorful
  ggtitle("Post Race SkipGram")+ #adding a title
  ylab("Probability of Close Occurrence")+ #adding a y label
  theme(plot.title = element_text(hjust = 0.5))+ #centering title
  xlab(NULL)

#America
ggplot(Pre_America)+  #tells which data to use
  aes(x=reorder(word2,-p_together,sum), y=p_together)+ #Using the word2 and probability data
  coord_flip()+
  geom_col(color="firebrick4", fill="lightcoral")+ #extra code to make the graph colorful
  ggtitle("Pre America SkipGram")+ #adding a title
  ylab("Probability of Close Occurrence")+ #adding a y label
  theme(plot.title = element_text(hjust = 0.5))+ #centering title
  xlab(NULL)

ggplot(Post_America)+  #tells which data to use
  aes(x=reorder(word2,-p_together,sum), y=p_together)+ #Using the word2 and probability data
  coord_flip()+
  geom_col(color="firebrick4", fill="lightcoral")+ #extra code to make the graph colorful
  ggtitle("Post America SkipGram")+ #adding a title
  ylab("Probability of Close Occurrence")+ #adding a y label
  theme(plot.title = element_text(hjust = 0.5))+ #centering title
  xlab(NULL)

#Germany
ggplot(Pre_german)+  #tells which data to use
  aes(x=reorder(word2,-p_together,sum), y=p_together)+ #Using the word2 and probability data
  coord_flip()+
  geom_col(color="firebrick4", fill="lightcoral")+ #extra code to make the graph colorful
  ggtitle("Pre German SkipGram")+ #adding a title
  ylab("Probability of Close Occurrence")+ #adding a y label
  theme(plot.title = element_text(hjust = 0.5))+ #centering title
  xlab(NULL)

ggplot(Post_german)+  #tells which data to use
  aes(x=reorder(word2,-p_together,sum), y=p_together)+ #Using the word2 and probability data
  coord_flip()+
  geom_col(color="firebrick4", fill="lightcoral")+ #extra code to make the graph colorful
  ggtitle("Post German SkipGram")+ #adding a title
  ylab("Probability of Close Occurrence")+ #adding a y label
  theme(plot.title = element_text(hjust = 0.5))+ #centering title
  xlab(NULL)
