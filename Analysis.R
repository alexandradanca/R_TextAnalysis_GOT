########## FORMATAREA DATELOR ##########
########################################
### incarcam pachetele pentru text
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(textdata)
library(stringr)
library(tidytext)
library(wordcloud) 
library(reshape2)
library(scales)
library(igraph)
library(ggraph)
library(topicmodels)
library(widyr)

### importam baza de date
Game_of_Thrones<- read_csv("D:/OneDrive/Desktop/Master/An 2/PSDT/PROIECT/Game_of_Thrones_Script.csv")
head(Game_of_Thrones)

## Top 10 personaje care au avut cele mai multe linii de dialog
Game_of_Thrones %>% 
  count(Name) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  ggplot(aes(y=reorder(Name, n), x=n)) +
  geom_bar(stat="identity", aes(fill=n), show.legend=FALSE) + 
  geom_label(aes(label=n)) +
  scale_fill_gradient(low="dodgerblue", high="dodgerblue4") +
  labs(x="Linii de dialog", y="Personaj",
       title="Linii de dialog per personaj") +  
  theme_bw()

##### sistematizam textul
### TOKENIZARE, grupat pe sezoane
GOT_tidy <- Game_of_Thrones %>%
  group_by(Season) %>%
  mutate(linenumber = row_number())%>%
  ungroup() %>%
  unnest_tokens(word, Sentence)
GOT_tidy

### eliminarea cuvintelor comune (ex: the, of, etc.)
data(stop_words)
GOT_tidy <- GOT_tidy%>%
  anti_join(stop_words)

### cele mai intalnite cuvinte
GOT_tidy%>%
  count(word, sort = TRUE)

### grafic cele mai intalnite cuvinte care au fost intalnite mai mult de 500 ori
GOT_tidy%>%
  count(word, sort = TRUE)%>%
  filter(n>500)%>%
  mutate(word = reorder(word, n))%>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

### cloud cu top 50 cuvinte
GOT_tidy %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 50,rot.per=0.25, colors = brewer.pal(8,"Dark2"), size = 1))

### calculam frecventele cuvintelor care apar atat Episod 1 din S1 cat si din S2 si sa le comparam cu S3
GOT_frequency_episode1<- GOT_tidy%>%
  filter(Episode == 'Episode 1') %>%
  mutate(word = str_extract(word, "[a-z']+"))%>%
  count(Season, word)%>%
  group_by(Season)%>%
  mutate(proportion = n / sum(n))%>%
  select(-n)%>%
  spread(Season, proportion)%>%
  gather(Season, proportion, 'Season 1','Season 2')

### grafic pentru ce este mai sus
ggplot(GOT_frequency_episode1, aes(x = proportion, y = `Season 3`,
                                   color = abs(`Season 3`- proportion)))+
  geom_abline(color = "gray40", lty = 2)+
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3)+
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5)+
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels = percent_format())+
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkslategray4", high = "gray75")+
  facet_wrap(~Season, ncol = 2)+
  theme(legend.position = "none")+
  labs(y="Season 3", x = NULL)

### Calculam corelatia intre aceste frecvente ale cuvintelor 
cor.test(data = GOT_frequency_episode1[GOT_frequency_episode1$Season == "Season 1",],
         ~proportion+`Season 3`)
cor.test(data = GOT_frequency_episode1[GOT_frequency_episode1$Season == "Season 2",],
         ~proportion+`Season 3`)


######## ANALIZA SENTIMENTELOR #########
########################################
sentiments
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

### lista cu cei mai frecventi termeni din fiecare emotie a nrc. 
nrc <- get_sentiments("nrc")%>% 
  mutate(lexicon = "nrc", 
         words_in_lexicon = n_distinct(word))
### Apoi am aranjat cuvintele în ordine descrescatoare top 10 cuvinte.
GOT_tidy %>% 
  inner_join(nrc, "word") %>%
  count(sentiment, word, sort=T) %>%
  group_by(sentiment) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  facet_wrap(~sentiment, scales="free_y") +
  labs(y="Frecventa", x="Cuvinte", 
       title="Cele mai frecvente cuvinte pentru fiecare sentiment NRC") +
  coord_flip() +
  theme_bw()

### salvam intr-un df cuvintele pentru sentimentul de frica
nrc_fear <- get_sentiments("nrc") %>%
filter(sentiment == "fear")

GOT_tidy %>%
  filter(Season == "Season 1") %>%
  inner_join(nrc_fear) %>%
  count(word, sort = TRUE)

###Examinam care este ordinea celor mai pozitive sezoane
ratio_seasons <- GOT_tidy %>% 
  inner_join(get_sentiments("bing")) %>%
  group_by(Season, sentiment) %>%
  summarize(score = n()) %>%
  spread(sentiment, score) %>% 
  ungroup() %>%
  mutate(ratio = positive / (positive + negative), 
         Season = reorder(Season, ratio))

ratio_seasons %>%
  ggplot(aes(x = Season, y = ratio)) +
  geom_point(color = "blue", size = 4) +
  coord_flip() +
  labs(title = "Top sezoane pozitive",
       x = "",
       caption = "ratio = positive to positive and negative words jointly") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        panel.grid = element_line(linetype = "dashed", color = "darkgrey", size = .5))

### Examinam care este ordinea celor mai negative episoade
ratio_episodes <- GOT_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(`Episode Title`, sentiment) %>%
  summarize(score = n()) %>%
  spread(sentiment, score) %>% 
  ungroup() %>%
  mutate(ratio = positive / (positive + negative), 
         `Episode Title` = reorder(`Episode Title`, ratio))

ratio_episodes %>%
  mutate(ratio = 1 - ratio, 
         `Episode Title` = reorder(`Episode Title`, ratio)) %>%
  top_n(20) %>%
  ggplot(aes(x = `Episode Title`, y = ratio)) +
  geom_point(color = "red", size = 4) +
  coord_flip() +
  labs(title = "Top 20 episoade negative",
       x = "",
       caption = "ratio = negative to positive and negative words jointly") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        panel.grid = element_line(linetype = "dashed", color = "darkgrey", size = .5))

### Frecventa cuvintelor negative si pozitive
### analizam care sunt cele mai frecvente cuvinte care influenteaza un anumit sentiment
#### pentru BING contorizam cuvintele
bing_word_counts <- GOT_tidy %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  ungroup() 
bing_word_counts 

### reprezentare grafica
bing_word_counts %>% 
  group_by(sentiment) %>% 
  top_n(5) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "Contribution to sentiment", x = NULL) + 
  coord_flip()

#### Wordclouds 
GOT_tidy %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("red", "green"), 
                   max.words = 100)

### analiza sentimentelor pentru 10 personaje
GOT_tidy %>%
  filter(Name %in% c("tyrion lannister","jon snow","daenerys targaryen", "cersei lannister", 
                           "jaime lannister","sansa stark","arya stark","davos",            
                           "theon greyjoy","petyr baelish")) %>%
  inner_join(nrc, "word") %>%
  count(Name, sentiment, sort=TRUE) %>%
  ggplot(aes(x=sentiment, y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  facet_wrap(~Name, scales="free_x") +
  labs(x="Sentiment (NRC)", y="Frecventa", 
       title="Analiza sentimentelor pentru 10 personaje din serial") +
  coord_flip() +
  theme_bw()

######## ANALIZA FRECVENTELOR ##########
########################################
####--> IMPREUNA CU CUVINTE CHEIE
### TOKENIZARE fara eliminarea cuvintelor cheie (stop_words)
GOT_words <- Game_of_Thrones%>% 
  unnest_tokens(word, Sentence) %>% 
  count(Season, word, sort = TRUE) %>% 
  ungroup() 

### df cu totalul cuvintelor pentru fiecare sezon
total_words <- GOT_words %>% 
  group_by(Season) %>% 
  summarize(total = sum(n))

### analizam frecventa cuvintelor
GOT_words <- left_join(GOT_words, total_words) 
GOT_words

### putem reprezenta grafic distributia
ggplot(GOT_words, aes(n/total, fill = Season)) + 
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) + 
  facet_wrap(~Season, ncol = 2, scales = "free_y") 

####--> FARA CUVINTE CHEIE
### df cu totalul cuvintelor pentru fiecare sezon
### TOKENIZARE cu eliminarea cuvintelor cheie (stop_words)
GOT_words_2 <- Game_of_Thrones%>% 
  unnest_tokens(word, Sentence) %>% 
  count(Season, word, sort = TRUE) %>% 
  ungroup() 

GOT_words_2<-GOT_words_2%>%
  anti_join(stop_words)

### df cu totalul cuvintelor pentru fiecare sezon
total_words <- GOT_words_2 %>% 
  group_by(Season) %>% 
  summarize(total = sum(n))

### analizam frecventa cuvintelor
GOT_words_2 <- left_join(GOT_words_2, total_words) 
GOT_words_2

### putem reprezenta grafic distributia
ggplot(GOT_words_2, aes(n/total, fill = Season)) + 
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) + 
  facet_wrap(~Season, ncol = 2, scales = "free_y") 

###Legea lui Zipf
# generam rangul cuvintelor 
freq_by_rank <- GOT_words_2 %>% 
  group_by(Season) %>%
  mutate(rank = row_number(), `term frequency` = n/total)
freq_by_rank 

### Grafic 
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = Season)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = TRUE) + 
  scale_x_log10() +
  scale_y_log10()

######## RELATII INTRE CUVINTE ##########
########################################
### facem tokenizarea
GOT_bigrams <- Game_of_Thrones%>%
  unnest_tokens(bigram, Sentence, token = "ngrams", n = 2)
GOT_bigrams

### separam cele 2 cuvinte
bigrams_separat <- GOT_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

### Scoatem cuvintele cheie
bigrams_filtru <- bigrams_separat %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

### Numaram noile bigram
bigram_nr <- bigrams_filtru %>% 
  count(word1, word2, sort = TRUE)

### Unim bigramele pentru a forma cuvinte
bigrams_unit <- bigrams_filtru %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_unit

### Cele mai comune bigrame, excluzând cuvintele oprite
bigram_tf_idf <- bigrams_unit %>% 
  count(Season, bigram) %>% 
  bind_tf_idf(bigram, Season, n) %>%
  arrange(desc(tf_idf)) 
bigram_tf_idf

### Utilizarea Bi-grams în Analiza Sentimentelor de negatie
negation_words <- c("not", "no", "never", "without") 

### ne ofera scorul sentimentelor numerice, care indica directia sentimentului
AFINN <- get_sentiments("afinn")

not_words <- bigrams_separat %>% 
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE) 
not_words

# grafic
not_words %>% 
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>% 
  head(20) %>% mutate(word2 = reorder(word2, contribution)) %>% 
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) + 
  xlab("Words preceded by negation") +
  ylab("Sentiment value * number of occurrences") + 
  coord_flip()

#### retele ale perechilor de cuvinte
# detalii ale structurii textului
bigram_graph <- bigram_nr %>% 
  filter(n > 20) %>% 
  graph_from_data_frame() 
bigram_graph
### grafic
set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

#### Numararea si analiza corelatiei perechilor de cuvinte 
### dorim o analiza pe sezonul 1
GOT_s1_words <- GOT_tidy %>%
  filter(Season == "Season 1")
### permite sa numaram seturi frecvente de cuvinte care apar recitate de acelasi personaj
word_pairs <- GOT_s1_words %>%
  pairwise_count(word, Name , sort = TRUE)
word_pairs

### Putem gasi cuvintele care apar cel mai des alaturi de lord
word_pairs %>%
  filter(item1 == "lord")

### examinam corelatia (asocierea) dintre cuvinte, ceea ce indica cât de des ele apar împreuna, 
# în raport cu cât de des apar separat.
word_cors <- GOT_s1_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, Name, sort = TRUE)
word_cors

### Putem sa alegem ti alte cuvinte interesante si sa gasim celelalte cuvinte cele mai asociate cu ele
word_cors %>%
  filter(item1 %in% c("king", "blood", "kill", "throne")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~item1, scales = 'free') +
  coord_flip()

#### CONVERTIREA IN DIFERSE FORMATE ####
tidy_lda <- Game_of_Thrones %>% 
  ungroup() %>% 
  unnest_tokens(word, Sentence) %>%
  distinct() %>%
  anti_join(stop_words) %>%
  filter(nchar(word) > 2) %>% 
  select(Name,Episode,Season,word)


topics <- LDA(cast_dtm(data = tidy_lda %>% 
                         count(Name, word) %>% 
                         ungroup(),
                       term = word,
                       document = Name, 
                       value = n),
              k = 8, control = list(seed = 1234)) %>% 
  tidy(matrix = "beta") %>% 
  group_by(topic) %>%
  arrange(desc(beta)) %>% 
  top_n(12, beta) %>% 
  ungroup()

topics %>% 
  arrange(topic, -beta) %>% 
  mutate(term = reorder(term, beta)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = 'free') +
  coord_flip() +
  ggtitle("Topic modeling using LDA")

### evaluam doar sezonul 8
tidy_lda_S8 <- Game_of_Thrones %>% 
  filter(Season=="Season 8") %>% 
  ungroup() %>% 
  unnest_tokens(word, Sentence) %>%
  distinct() %>%
  anti_join(stop_words) %>%
  filter(nchar(word) > 2) %>% 
  select(Name,Episode,Season,word)


topics_S8 <- LDA(cast_dtm(data = tidy_lda_S8 %>% 
                             count(Name, word) %>% 
                             ungroup(),
                           term = word,
                           document = Name, 
                           value = n),
                  k = 7, control = list(seed = 1234)) %>% 
  tidy(matrix = "beta") %>% 
  group_by(topic) %>%
  arrange(desc(beta)) %>% 
  top_n(7, beta) %>% 
  ungroup()

topics_S8 %>% 
  arrange(topic, -beta) %>% 
  mutate(term = reorder(term, beta)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = 'free') +
  coord_flip() +
  ggtitle("Topic modeling using LDA in the S8")

