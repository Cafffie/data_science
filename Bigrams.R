#Different ways of creating bigrams
#step 1
ola <- ai_sample_papers$abstract %>% 
  str_replace_all("[^A-Za-z]"," ") 

#Remove stopwords
tokens <- tokens(ola) %>%
  tokens_remove(pattern = stopwords("en"))

#Create bigrams 
bigrams <- tokens_ngrams(tokens, n = 2) 
unlist_bigrams <- unlist(bigrams)

#Convert to dataframe
df_bigrams <- data.frame(words= unlist_bigrams )
head(df_bigrams)

#Generating word frequencies
count_df <- count(df_bigrams, words, sort=T)
count_df_subset <- subset(count_df, count_df$n>30)
slice_max(count_df,n, n=30)

set.seed(234)
w_cloud <- wordcloud(count_df$words,count_df$n, min.freq = 20,
                     colors=brewer.pal(8, "Dark2"),
                     scale=c(5,1), random.order=F)


#Plot a barplot to view bigrams
count_df_subset  %>%
  ggplot(aes(words, n, fill= words)) +
  geom_col(show.legend=F)+
  labs(y="Word frequency", x=NULL)+
  coord_flip()




#Step 2
ola <- ai_sample_papers$abstract %>% 
  str_replace_all("[^A-Za-z]"," ") 

#Remove stopwords
tokens <- tokens(ola) %>%
  tokens_remove(pattern = stopwords("en"))

#Create bigrams and plot its frequencies
bigrams <- tokens_ngrams(tokens, n = 2) 
unlist_bigrams <- unlist(bigrams)
bigram_freq <- table(unlist_bigrams)
bigram_freq2 <- data.frame(count=bigram_freq)
head(bigram_freq2)
bigram_subset <- subset(bigram_freq2, bigram_freq2$count.Freq>30)
bigram_subset2 <- bigram_subset[order(-bigram_subset$count.Freq),]

bigram_freq <- table(unlist_bigrams)
df <- data.frame(bigram_freq)
count(df, unlist_bigrams, sort=)



#GEnerating single words
ola <- ai_sample_papers$abstract %>% 
  str_replace_all("[^A-Za-z]"," ") 

#Remove stopwords
tokens <- tokens(ola) %>%
  tokens_remove(pattern = stopwords("en"))

#Create bigrams 
single_words <- tokens_ngrams(tokens, n = 1) 
unlist_single_words <- unlist(single_words)

#Convert to dataframe
single_words_df <- data.frame(word= unlist_single_words )
head(single_words_df)

#Generating word frequencies
count_df <- count(single_words_df, word, sort=T)
count_df_subset <- subset(count_df, count_df$n>99)
slice_max(count_df,n, n=30)

set.seed(234)
w_cloud <- wordcloud(count_df$word,count_df$n, min.freq = 20,
                     colors=brewer.pal(8, "Dark2"),
                     scale=c(5,1), random.order=F)


#Plot a barplot to view bigrams
count_df_subset  %>%
  ggplot(aes(word, n, fill= word)) +
  geom_col(show.legend=F)+
  labs(y="Word frequency", x=NULL)+
  coord_flip()







library(wordcloud)
wordcloud(words, freq, min.freq = 3, scale = c(3, 0.5), colors = brewer.pal(8, "Dark2"))

# Sample data: Words and their frequencies
words <- c("apple", "banana", "cherry", "date", "elderberry")
freq <- c(10, 15, 8, 5, 12)

# Create a word cloud
wordcloud(words, freq, scale = c(3, 0.5), colors = brewer.pal(8, "Dark2"))


library(ggplot2)

# Sample data
data <- data.frame(
  Category = c("A", "B", "C", "D"),
  Value = c(20, 35, 15, 42)
)

# Create a bar chart using ggplot2
ggplot(data, aes(x = Category, y = Value)) +
  geom_col(fill = "skyblue") +
  labs(title = "Simple Bar Chart", x = "Category", y = "Value")