theme_set(theme_gray(base_family='NanumGothic'))

#텍스트 전처리 (필요한 전처리 수행)
install.packages("dplyr")
install.packages("stringr")
library(dplyr)
library(stringr)

songs <- read.csv("Song Lyrics.csv")
songs_text <- songs %>% mutate(Lyrics = str_replace_all(Lyrics, "[^가-힣]", " "), 
                               Lyrics = str_squish(Lyrics))

#토큰화 수행
install.packages("tidytext")
install.packages("KoNLPy")
install.packages("ggplot2")
install.packages("tidyr")
library(tidytext)
library(KoNLP)
library(ggplot2)
library(tidyr)

songs_token <- songs_text %>% unnest_tokens(input = Lyrics, output = word, token = extractNoun)

#빈도 분석(단어, 문장, 형태소 등등 1가지 필수)
freq <- songs_token %>% count(Singer, word) %>% filter(str_count(word) > 1)
top10 <- freq %>% group_by(Singer) %>% slice_max(n, n=10, with_ties = F)

ggplot(top10, aes(x=reorder(word, n), y = n, fill=Singer)) + geom_col() + coord_flip() + 
  facet_wrap(~Singer, scales = "free_y")

#오즈비 분석
df_wide <- freq %>% pivot_wider(names_from = Singer, values_from = n, values_fill = list(n=0))
df_wide <- df_wide %>% mutate(ratio_park=((박재범+1)/(sum(박재범+1))),
                              ratio_Zi = ((지코+1)/(sum(지코+1))))

df_wide <- df_wide %>% mutate(odds_ratio=ratio_park/ratio_Zi)
df_wide

#의미망 분석
install.packages("readr")
install.packages("tidygraph")
install.packages("textclean")
install.packages("widyr")
install.packages("ggraph")
install.packages("showtext")
install.packages("tm")
install.packages("topicmodels")

library(tidygraph)
library(readr)
library(textclean)
library(widyr)
library(ggraph)
library(showtext)
library(tm)
library(topicmodels)

jay_data <- read_csv("Jay_Lyrics.csv")
jay_lyrics <- jay_data %>% select(Lyrics) %>% mutate(Lyrics = str_replace_all(Lyrics, "[^가-힣]", " "), 
                                                     Lyrics = str_squish(Lyrics), id=row_number())
lyrics_pos <- jay_lyrics %>% unnest_tokens(input = Lyrics, output = word, token = SimplePos22, drop=F)
lyrics_pos %>% select(word, Lyrics)
lyrics_pos <- lyrics_pos %>% separate_rows(word, sep = "[+]")
lyrics_pos %>% select(word, Lyrics)
noun <- lyrics_pos %>% filter(str_detect(word, "/n")) %>% mutate(word = str_remove(word, "/.*$"))
noun %>% select(word, Lyrics)
noun %>% count(word, sort = T)
pvpa <- lyrics_pos %>% filter(str_detect(word, "/pa|/pv")) %>% mutate(word = str_replace(word, "/.*$", "다"))
pvpa %>% select(word, Lyrics)
pvpa %>% count(word, sort = T)
lyrics <- bind_rows(noun, pvpa) %>% filter(str_count(word)>=2) %>% arrange(id)
lyrics %>% select(word, Lyrics)
pair <- lyrics %>% pairwise_count(item = word, feature = id, sort = T)
graph_lyrics <- pair %>% filter(n >= 1) %>% as_tbl_graph()
ggraph(graph_lyrics) + geom_node_point() + geom_edge_link() + geom_node_text(aes(label = name))
font_add_google(name = "Nanum Gothic", family = "ng")
showtext_auto()
set.seed(1234)
ggraph(graph_lyrics, layout = "fr") + geom_edge_link(color = "gray50", alpha = 0.5) + 
  geom_node_point(color = "lightcoral", size = 5) +
  geom_node_text(aes(label = name), repel = T, size = 5, family = "ng") + theme_graph()

#토픽 모델링
raw_jay_lyrics <- jay_data %>% mutate(id = row_number())
jay_lyrics <- raw_jay_lyrics %>% 
  mutate(Lyrics = str_replace_all(Lyrics, "[^가-힣]", " "), Lyrics = str_squish(Lyrics)) %>% 
  distinct(Lyrics, .keep_all = T) %>% filter(str_count(Lyrics, boundary("word")) >= 3)
lyrics <- jay_lyrics %>% unnest_tokens(input = Lyrics, output = word, token = extractNoun, drop = F) %>%
  filter(str_count(word) > 1) %>% group_by(id) %>% distinct(word, .keep_all = T) %>% 
  ungroup() %>% select(id, word)
lyrics %>% count(word, sort = T) %>% print(n = 200)
stopword <- c("괜찮", "깨달았", "떠나겠", "만해", "말하", "멈춰버", "모르", "믿겠", "배어있", 
              "상상했", "아끼", "이제부", "있었잖", "준비되어있", "해달")
lyrics <- lyrics %>% filter(!word %in% stopword) %>% 
  mutate(word = recode(word, "괜찮" = "괜찮음", "깨달았" = "깨달음", "떠나겠" = "떠남", "만해" = " ", 
                       "말하" = "말", "멈춰버" = "멈춤", "모르" = "모름", "믿겠" = "믿음", "상상했" = "상상", "아끼" = "아낌", "이제부" = "이제",
                       "있었잖" = "있음", "준비되어있" = "준비", "해달" = " "))

count_lyrics <- lyrics %>% count(id, word, sort = T)
dtm_lyrics <- count_lyrics %>% cast_dtm(document = id, term = word, value = n)
as.matrix(dtm_lyrics[1:7, 1:7])
lda_model <- LDA(dtm_lyrics, k = 8, method = "Gibbs", control = list(seed = 1234))
term_topic <- tidy(lda_model, matrix = "beta")
term_topic %>% filter(topic == 6) %>% arrange(-beta)
terms(lda_model, 10) %>% data.frame()
doc_topic <- tidy(lda_model, matrix = "gamma")
doc_topic %>% filter(document == 1) %>% summarise(sum_gamma = sum(gamma))
doc_class <- doc_topic %>% group_by(document) %>% slice_max(gamma, n = 1)
doc_class$document <- as.integer(doc_class$document)
doc_class %>% arrange(document)

doc_class
raw_jay_lyrics
new_lyrics <- raw_jay_lyrics %>% left_join(doc_class, by = c("id" = "document"))
new_lyrics
new_lyrics <- new_lyrics %>% na.omit()
top_terms <- term_topic %>% group_by(topic) %>% slice_max(beta, n = 6, with_ties = F) %>% summarise(term = paste(term, collapse = ", "))
count_topic <- new_lyrics %>% count(topic)
count_topic_word <- count_topic %>% left_join(top_terms, by = "topic") %>% mutate(topic_name = paste("TOPIC", topic))
ggplot(count_topic_word, aes(x = reorder(topic_name, n), y = n, fill = topic_name)) + geom_col(show.legend = F) +
  coord_flip() + geom_text(aes(label = n), hjust = -0.2) + geom_text(aes(label = term), hjust = 1.04, col = "white", fontface = "bold") +
  scale_y_continuous(limits = c(0, 30)) + labs(x = NULL)
