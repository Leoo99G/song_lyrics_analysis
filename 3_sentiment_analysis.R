library(syuzhet)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidytext)
library(textdata)
library(xtable) # to convert a dataframe to a latex table automatically


# We will use the lyrics in songs$clean_lyrics  

# parsing a text into a vector of sentences
s_v <- syuzhet::get_sentences(songs$clean_lyrics)
sent_vector <- get_sentiment(s_v, method="syuzhet")
songs$sentiment <- sent_vector

# Ensemble metrics
sum(sent_vector)
mean(sent_vector)
median(sent_vector)
summary(sent_vector)

# Create a dataframe of sentiment scores to plot them
sent_df <- data.frame(x = 1:length(sent_vector),
                      year = songs$year,
                      sent = sent_vector)

# Sentiment scores (syuzhet method)
# Scores between -20 and 20 are included
ggplot(data = sent_df, aes(y = sent_vector, x = x)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = F, color = "red", linewidth = 1.2) +
  labs(x = "Song index", y = "Sentiment score") +
  ylim(-20, 20) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


# Sentiment scores (syuzhet method) by year
# Scores between -20 and 20 are included
ggplot(data = sent_df, aes(y = sent_vector, x = year)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = F, color = "red", linewidth = 1.2) +
  labs(x = "Year", y = "Sentiment score") +
  ylim(-20, 20) +
  scale_x_continuous(breaks = seq(min(sent_df$year), max(sent_df$year), by = 5))  +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16))


# SENTIMENT SCORES SUMMARY STATISTICS EVERY 5 YEARS

# Create a function to assign periods of 5 years
assign_period <- function(year) {
  return (paste0(floor(year / 5) * 5, " - ", floor(year / 5) * 5 + 4))
}

# Apply the function to create a new column for periods
songs$period <- assign_period(songs$year)

# Group the data by period and calculate summary statistics
summary_stats <- songs %>%
  group_by(period) %>%
  summarise(mean_sentiment = mean(sentiment, na.rm = TRUE),
            median_sentiment = median(sentiment, na.rm = TRUE),
            sum_sentiment = sum(sentiment, na.rm = TRUE))

# View the summary statistics
print(summary_stats)

# Convert results into latex table
latex_table1 <- xtable(summary_stats, include.rownames = F)


# EMOTIONS USING THE NRC LEXICON

# Emotion classification (based on the 8 basic emotions of the Plutchik’s wheel)
nrc_sent <- get_nrc_sentiment(s_v)

# Add emotions to songs dataframe
songs$anger <- nrc_sent$anger
songs$anticipation <- nrc_sent$anticipation
songs$disgust <- nrc_sent$disgust
songs$fear <- nrc_sent$fear
songs$joy <- nrc_sent$joy
songs$sadness <- nrc_sent$sadness
songs$surprise <- nrc_sent$surprise
songs$trust <- nrc_sent$trust
songs$negative <- nrc_sent$negative
songs$positive <- nrc_sent$positive


# Select the 8 detailed emotions from the data frame:
# anger, anticipation, disgust, fear, joy, sadness, surprise, trust
emo_df <- data.frame(colSums(songs[,11:18]))
# rename var
names(emo_df) <- "Score"
emo_df <- cbind("sentiment"=rownames(emo_df),emo_df)


# Histogram of detailed emotions
ggplot(data=emo_df,
       aes(x=fct_reorder(sentiment, Score, .desc = TRUE), y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity", show.legend = F)+
  theme(legend.position="none")+
  xlab("Emotions")+ylab("Total score")+
  ggtitle("")+
  theme_minimal() + 
  theme(text = element_text(size = 22) )


# we select the positive/negative emotions from the data frame
sent_pos_neg <- data.frame(colSums(songs[,17:18]))
# rename var
names(sent_pos_neg) <- "Score"
sent_pos_neg <- cbind("sentiment"=rownames(sent_pos_neg),sent_pos_neg)

ggplot(data = sent_pos_neg,
       aes(fill = sentiment, x = fct_reorder(sentiment, Score, .desc = TRUE), y = Score)) +
  geom_bar(stat = "identity", show.legend = F, width = 0.5) +
  xlab("Sentiment") + ylab("Sum of scores") +
  ggtitle("Total positivity and negativity across all songs") +
  theme_minimal()


# EMOTIONS BY DECADE

# Function to convert year to its corresponding decade
get_decade <- function(year) {
  decade <- as.character(year - (year %% 10))
  return(paste(decade, "s", sep = ""))
}

# Add a column decade to songs df
songs$decade <- get_decade(songs$year)

# Recreate the emo_df dataframe with the decade information
emo_df_decade <- songs %>%
  group_by(decade) %>%
  summarise(across(anger:trust, sum))

# Melt the dataframe to long format for easier plotting
emo_df_long <- emo_df_decade %>%
  pivot_longer(cols = anger:trust, names_to = "sentiment", values_to = "Score")

# Plot the histogram faceted by decade
ggplot(data = emo_df_long,
       aes(x = fct_reorder(sentiment, Score, .desc = TRUE), y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  facet_wrap(~decade, scales = "free_x") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  labs(x = '', y = "Total score") +
  ggtitle("") +
  theme(text = element_text(size = 14))


# POSITIVITY RATIO

# We introduce a column "positivity ratio", which is computed as
# positive / (positive + negative)
# It is the percentage of positivity of a song
songs$positivity_ratio <- songs$positive / (songs$positive + songs$negative)

# Top positive songs
top20pos <- songs %>%
  arrange(desc(positivity_ratio)) %>%
  head(40) %>%
  ggplot(aes(x = reorder(song, positivity_ratio), y = positivity_ratio)) +
  geom_point(color = "blue", size = 4) +
  coord_flip() +
  labs(title = "Top 40 Most Positive Songs",
       x = "",
       y = "Positivity Ratio") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        panel.grid = element_line(linetype = "dashed", color = "darkgrey", size = .5))


# Top negative songs
top20neg <- songs %>%
  arrange(positivity_ratio) %>%
  head(40) %>%
  ggplot(aes(x = reorder(song, -positivity_ratio), y = positivity_ratio)) +
  geom_point(color = "red", size = 4) +
  coord_flip() +
  labs(title = "Top 40 Most Negative Songs",
       x = "",
       y = "Positivity Ratio") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        panel.grid = element_line(linetype = "dashed", color = "darkgrey", size = .5))

# Plot
grid.arrange(top20neg, top20pos, ncol = 2)


# Plot positivity ratio over time
ggplot(songs, aes(x = year, y = positivity_ratio - 0.5)) +
  geom_point() +
  geom_smooth(col = 'red')


# Most frequent words for each type of sentiment (we'll use the df_expanded df)

# words tagged with emotions and sentiment polarity (positive/negative)
# A tibble: 13,872 × 2                                                                                                             
# word        sentiment
(nrc = get_sentiments(lexicon = 'nrc'))


# Count frequency of words
unigram_tidy <- df_expanded %>%
  group_by(words) %>%
  count() %>% 
  ungroup () %>%
  arrange(desc(n)) 


unigram_tidy %>% 
  inner_join(nrc, by = c("words" = "word")) %>% 
  ungroup() %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  arrange(desc(n)) %>%
  group_by(sentiment) %>% 
  slice(1:10) %>%
  ggplot(aes(
    x = reorder(words, n),
    y = n,
    fill = sentiment)) + 
  geom_col(show.legend = FALSE) +
  theme(axis.text.y = element_text(size = 20)) +
  facet_wrap( ~ sentiment, scales = "free") +
  coord_flip() +
  labs(x = NULL, y = NULL, title = 'Top 10 most frequent words per each sentiment category') +
  theme_bw()
