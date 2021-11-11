#install.packages("quanteda")
library(quanteda)
library(ggplot2)
library(lubridate)
library(dplyr)

clothes <- read.csv('Womens Clothing E-Commerce Reviews.csv')
str(clothes)
clothes <- clothes[-1]
colnames(clothes) <- c('ID', 'Age', 'Title', 'Review', 'Rating', 'Recommend', 'Liked', 'Division', 'Dept', 'Class')
library(purrr)
unlist(map(map(clothes, is.na), sum))
# Arranging by Department reviews/ratings
ggplot(data.frame(prop.table(table(clothes$Dept))), 
       aes(x=Var1, y = Freq*100)) + geom_bar(stat = 'identity') + 
  xlab('Department Name') + ylab('Percentage of Reviews/Ratings (%)') + 
  geom_text(aes(label=round(Freq*100,2)), vjust=-0.25) + 
  ggtitle('Percentage of Reviews By Department')
# Distribution of ratings within each department excluding Trend
phisto <- clothes %>% filter(!is.na(Dept), Dept != 'Trend') %>% 
  mutate(Dept = factor(Dept)) %>% group_by(Dept) %>% count(Rating) %>% mutate(perc = n/sum(n))

phisto %>% ggplot(aes(x=Rating, y = perc*100, fill = Dept)) + 
  geom_bar(stat = 'identity', show.legend = FALSE) + facet_wrap(~Dept) + 
  ylab('Percentage of reviews (%)') + geom_text(aes(label=round(perc*100,2)))

# Departments by Age(grouped the age into categories (18-29, 30-39, 40-49, etc.))
#facet wrap by age and look at Dept distribution in each
ages <- clothes %>% filter(!is.na(Age), !is.na(Dept), Dept != 'Trend') %>% 
  select(ID, Age, Dept) %>% 
  mutate(Age_group = ifelse(Age < 30, '18-29', ifelse(Age < 40, '30-39', 
  ifelse(Age < 50, '40-49', ifelse(Age < 60, '50-59', ifelse(Age < 70, '60-69', 
ifelse(Age < 80, '70-79', ifelse(Age < 90, '80-89', '90-99')))))))) 

ages <- ages %>% mutate(Age_group = factor(Age_group), 
Dept = factor(Dept, levels = rev(c('Tops', 'Dresses', 'Bottoms', 'Intimate', 'Jackets'))))

ages %>% filter(Age < 80) %>% group_by(Age_group) %>% count(Dept) %>% 
  ggplot(aes(Dept, n, fill = Age_group)) + geom_bar(stat='identity', show.legend = FALSE) + 
  facet_wrap(~Age_group, scales = 'free') + xlab('Department') + ylab('Number of Reviews') +
  geom_text(aes(label = n), hjust = .73) + coord_flip()

ages %>% filter(Age >= 80) %>% group_by(Age_group) %>% count(Dept) %>% 
  ggplot(aes(Dept, n, fill = Age_group)) + geom_bar(stat='identity', show.legend = FALSE) + 
  facet_wrap(~Age_group, scales = 'free') + xlab('Department') + ylab('Number of Reviews') + 
  geom_text(aes(label = n), hjust = .73) + coord_flip()

# Bigram Analysis and Visualization(removed the entries that have no reviews and 
# combined the title with the review to get all the words into one section)
clothesr <- clothes %>% filter(!is.na(Review))
notitle <- clothesr %>% filter(is.na(Title)) %>% select(-Title)
library(tidyr)
wtitle <- clothesr %>% filter(!is.na(Title)) %>% unite(Review, c(Title, Review), sep = ' ')
main <- bind_rows(notitle, wtitle)
# sorting out the stop words and removing any digits.Also grouping the words according 
# to their Ratings
#install.packages("tidytext")
library(tidytext)
library(stringr)
bigramming <- function(data){
  cbigram <- data %>% unnest_tokens(bigram, Review, token = 'ngrams', n = 2)
  cbigram_sep <- cbigram %>% separate(bigram, c('first', 'second'), sep = ' ')
  cbigram2 <- cbigram_sep %>% filter(!first %in% stop_words$word, !second %in% stop_words$word, !str_detect(first,      
 '\\d'), !str_detect(second, '\\d')) %>% unite(bigram, c(first, second), sep = ' ') 
  return(cbigram2)
}

top_bigrams <- bigramming(main) %>% mutate(Rating = factor(Rating, levels <- c(5:1))) %>% 
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% group_by(Rating) %>% 
  count(bigram, sort=TRUE) %>% top_n(10, n) %>% ungroup() 

top_bigrams  %>% ggplot(aes(bigram, n, fill = Rating)) + geom_col(show.legend = FALSE) + 
  facet_wrap(~Rating, ncol = 3, scales = 'free') + labs(x=NULL, y = 'frequency') + 
  ggtitle('Most Common Bigrams (By Ratings)') + coord_flip()

# focussing on the 5 star and 1 star reviews
fivestar <- main %>% filter(Rating == 5)
onestar <- main %>% filter(Rating == 1)
fivebi <- bigramming(fivestar) %>% count(bigram, sort = TRUE)
onebi <- bigramming(onestar) %>% count(bigram, sort = TRUE)
library(igraph)
#install.packages("ggraph")
library(ggraph)

network_fivebi <- fivebi %>% separate(bigram, c('first','second'), sep = ' ') %>% 
  filter(n > 70) %>% graph_from_data_frame()
network_onebi <- onebi %>% separate(bigram, c('first','second'), sep = ' ') %>% 
  filter(n > 5) %>% graph_from_data_frame()

# Network of Popular Bigrams of 1-star Reviews
set.seed(11111)
ggraph(network_onebi, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = 'aquamarine1', size = 4) +
  geom_node_text(aes(label = name), vjust = 1.1, hjust = 1.1) + 
  scale_x_continuous(limits = c(2, 18)) + theme_void()
# Network of Popular Bigrams of 5-star Reviews
set.seed(22222)
ggraph(network_fivebi, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = 'red', size = 4) +
  geom_node_text(aes(label = name), vjust = 1.1, hjust = 1.1) + 
  scale_x_continuous(limits = c(2, 18)) + theme_void()

# wordcloud of Rating 1
library(wordcloud2)
wordcloud2(onebi %>% filter(n>5) %>% mutate(n = sqrt(n)), size = .4)
# wordcloud of Rating 5
wordcloud2(fivebi %>% filter(n>5) %>% mutate(n = sqrt(n)), size = .4)
# Latent Dirchlet Allocation (LDA) on Trend reviews
trend_count <- main %>% filter(Dept == 'Trend') %>% unnest_tokens(word, Review) %>% 
  anti_join(stop_words, by = 'word') %>% filter(!str_detect(word, '\\d')) %>% 
  count(ID, word, sort = TRUE) %>% ungroup()

trend_dtm <- trend_count %>% cast_dtm(ID, word, n)
install.packages("MASS")
library(ggforce)
library(MASS)
library(broom)
library(tidytext)
#install.packages("topicmodels")
library(topicmodels)
trendy <- tidy(LDA(trend_dtm, k = 5, method = 'GIBBS', control = list(seed = 4444, alpha = 1)), 
               matrix = 'beta')
top_trendy <- trendy %>% group_by(topic) %>% top_n(5, beta) %>% ungroup() %>% 
  arrange(topic, desc(beta))
top_trendy %>% mutate(term = reorder(term, beta)) %>%   
  ggplot(aes(term, beta, fill = factor(topic))) + geom_col(show.legend = FALSE) +   
  facet_wrap(~ topic, scales = "free") + ggtitle('LDA Analysis (k = 5)') + coord_flip()
#Conclusion
#The key takeaways from the above analysis of this dataset are:
# 5-star reviews are dominant in each department with jackets having the highest proportion 
#within its department
# Customers in their 30's and 40's leave the most reviews.
# Fit, comfortability/quality of the material, aesthetics of the clothing item influence 
#the rating
#Arm holes can be a problem
# By performing exploratory data analysis and bigram analysis, companies can focus on 
# what works and what doesn't. Knowing the demographics of the reviewers can inform 
# marketing decisions (e.g. online advertisements on sites most accessed by 30 and 40 year olds).
# Selecting items that have flexible and comfortable fabric can lead to higher customer 
# satisfaction. A higher number of positive reviews become a form of advertisement and 
# can lead to higher sales.









