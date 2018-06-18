## Explore some source text descriptives

# How many entries?

descriptives$size <- list(c("Number of tweets", length(twitter_txt)),
                          c("Number of news items", length(news_txt)),
                          c("Number of blog entries", length(blogs_txt)))

# What size vocabulary?

collection_txt <- bind_rows("twitter" = as_data_frame(twitter_txt),
                            "news" = as_data_frame(news_txt),
                            "blogs" = as_data_frame(blogs_txt),
                            .id = "source")
collection_txt$source <- factor(collection_txt$source)

collection_source_tokens <- collection_txt %>%
                            group_by(source) %>%
                            unnest_tokens(token, value) %>%
                            ungroup()

descriptives$vocs <- collection_source_tokens %>%
                     group_by(source) %>%
                     distinct() %>%
                     count()

# Most frequent words (and their relative frequency) (histogram)

collection_count <- collection_source_tokens %>%
                    group_by(source) %>%
                    count(token, sort = TRUE) %>%
                    top_n(10) %>%
                    arrange(source, desc(n)) %>%
                    mutate(token = factor(token,
                                          levels = rev(unique(token))))

# Most meaningfull words

collection_stop <- collection_source_tokens %>%
                   mutate(word = token) %>%
                   anti_join(stop_words) %>%
                   group_by(source) %>%
                   count(token, sort = TRUE) %>%
                   top_n(10) %>%
                   arrange(source, desc(n))

collection_words <- collection_source_tokens %>%
                    count(source, token, sort = TRUE) %>%
                    ungroup()

total_words <- collection_words %>% 
               group_by(source) %>% 
               summarize(total = sum(n))

collection_words <- left_join(collection_words, total_words)

collection_tf_idf <- collection_words %>%
                     bind_tf_idf(token, source, n) %>%
                     group_by(source) %>%
                     arrange(desc(tf_idf)) %>%
                     top_n(10) %>%
                     ungroup()

# Frequency plots

twitter_n <- collection_count %>%
             filter(source == "twitter") %>%
             mutate(token = reorder(token, n)) %>%
             ggplot(aes(token, n)) +
             geom_col(fill = "#8da0cb") +
             coord_flip() +
             ggtitle("Twitter")

news_n <- collection_count %>%
          filter(source == "news") %>%
          mutate(token = reorder(token, n)) %>%
          ggplot(aes(token, n)) +
          geom_col(fill = "#66c2a5") +
          coord_flip() +
          ggtitle("News")

blogs_n <- collection_count %>%
           filter(source == "blogs") %>%
           mutate(token = reorder(token, n)) %>%
           ggplot(aes(token, n)) +
           geom_col(fill = "#fc8d62") +
           coord_flip() +
           ggtitle("Blogs")

sources_n <- arrangeGrob(twitter_n, news_n, blogs_n, ncol = 3)
ggsave(file = "WordPredictor/www/sources_n.png", sources_n)

# Excluding stopwords frequency plots

twitter_stop <- collection_stop %>%
                filter(source == "twitter") %>%
                mutate(token = reorder(token, n)) %>%
                ggplot(aes(token, n)) +
                geom_col(fill = "#8da0cb") +
                coord_flip() +
                ggtitle("Twitter")

news_stop <- collection_stop %>%
             filter(source == "news") %>%
             mutate(token = reorder(token, n)) %>%
             ggplot(aes(token, n)) +
             geom_col(fill = "#66c2a5") +
             coord_flip() +
             ggtitle("News")

blogs_stop <- collection_stop %>%
              filter(source == "blogs") %>%
              mutate(token = reorder(token, n)) %>%
              ggplot(aes(token, n)) +
              geom_col(fill = "#fc8d62") +
              coord_flip() +
              ggtitle("Blogs")

sources_stop <- arrangeGrob(twitter_stop, blogs_stop, news_stop, ncol = 3)
ggsave(file = "WordPredictor/www/sources_stop.png", sources_stop)

# By TF-IDF plots

twitter_tf_idf <- collection_tf_idf %>%
                  filter(source == "twitter") %>%
                  mutate(token = reorder(token, tf_idf)) %>%
                  ggplot(aes(token, tf_idf)) +
                  geom_col(fill = "#8da0cb") +
                  coord_flip() +
                  ggtitle("Twitter")

news_tf_idf <- collection_tf_idf %>%
               filter(source == "news") %>%
               mutate(token = reorder(token, tf_idf)) %>%
               ggplot(aes(token, tf_idf)) +
               geom_col(fill = "#66c2a5") +
               coord_flip() +
               ggtitle("News")

blogs_tf_idf <- collection_tf_idf %>%
                filter(source == "blogs") %>%
                mutate(token = reorder(token, tf_idf)) %>%
                ggplot(aes(token, tf_idf)) +
                geom_col(fill = "#fc8d62") +
                coord_flip() +
                ggtitle("Blogs")

sources_tf_idf <- arrangeGrob(twitter_tf_idf, blogs_tf_idf, news_tf_idf, ncol = 3)
ggsave(file = "WordPredictor/www/sources_tf_idf.png", sources_tf_idf)