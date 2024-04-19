library(topicmodels)
library(tm)
library(dplyr)
library(tidytext)
library(ggplot2)
library(xtable)

# Turn to corpus object
corpus <- Corpus(VectorSource(combined_lyrics$lyrics))

# Document-term matrix
dtm <- DocumentTermMatrix(corpus)

# Apply LDA
lda_out <- LDA(
  dtm,
  k = 2,
  method = "Gibbs",
  control = list(seed=42)
)

lda_topics <- tidy(lda_out, matrix = "beta")

top_terms <- lda_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

# Plot
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  theme(text = element_text(size = 14))


# Report the topics in a dataframe (and then latex table) for easier visualization
topic_words <- top_terms %>%
  group_by(topic) %>%
  summarize(words = paste(term, collapse = ", "))
latex_table2 <- xtable(topic_words, include.rownames = F)
