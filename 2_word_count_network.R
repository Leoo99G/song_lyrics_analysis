library(tidyverse)
library(tidytext)
library(widyr)
library(tm)
library(dplyr)
library(igraph)
library(ggraph)
library(ggplot2)

# Recombine words into a cleaned lyrics column
# This step requires aggregating the words back into sentences for each song
combined_lyrics <- filtered_songs %>%
  group_by(id) %>%
  summarise(
    year = first(year),
    rank = first(rank),
    artist = first(artist),
    song = first(song),
    lyrics = paste(words, collapse = " ")
  )

# Stemming: collapsing words to a common root to aid comparison of vocabulary
combined_lyrics$st_cl_lyrics <- tm::stemDocument(combined_lyrics$lyrics, language = 'english')


######################### WORD COUNT ####################################

# Count the frequency of each word
stemmed_words <- stemDocument(filtered_songs$words)
word_freq <- table(stemmed_words)

# Convert the table to a dataframe
word_freq_df <- as.data.frame(word_freq)
names(word_freq_df) <- c("Word", "Frequency")

# Sort the dataframe by frequency in descending order
word_freq_df <- word_freq_df[order(-word_freq_df$Frequency), ]

# top N most frequent words using ggplot
N <- 20  # Number of top words to plot
top_words <- head(word_freq_df, N)

# Bar plot of word frequency
p <- ggplot(top_words, aes(x = reorder(Word, Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "#2171B5") +
  labs(title = "Top 20 Most Frequent Words", x = "Word", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12)) +
  coord_flip()  # Flip coordinates to make it vertical

# Add annotations (count of each word)
p + geom_text(aes(label = Frequency), vjust = 0.25, hjust = 1.1)



# Most frequent words by decade (using filtered_songs df)
filtered_songs$st_words <- stemDocument(filtered_songs$words)
filtered_songs <- filtered_songs %>%
  mutate(decade = paste0(substr(year, 1, 3), "0s"))

common_words <- filtered_songs %>%
  group_by(decade) %>%
  count(st_words, decade, sort = TRUE) %>%
  slice(seq_len(7)) %>%
  ungroup() %>%
  arrange(decade, n) %>%
  mutate(row = row_number())


common_words %>%
  ggplot(aes(x = row, y = n, fill = decade)) +
  geom_col() +
  labs(title = "",
       x = '', 
       y = '') +
  theme_bw() +  
  theme(legend.position = "None",
        axis.text.y = element_text(size = 16)) +
  facet_wrap(~decade, scales = "free", ncol = 4, nrow = 2) +
  scale_x_continuous(breaks = common_words$row,
                     labels = common_words$st_words) +
  coord_flip()







###################### RICHNESS OF VOCABULARY ##########################


avg_counts <- numeric(0)
med_counts <- numeric(0)
std <- numeric(0)

for (y in 1950:2019) {
  current_year_df <- filtered_songs[filtered_songs$year == y, ]
  ids_current_year <- unique(current_year_df$id)
  counts_current_year <- numeric(0)
  
  for (i in ids_current_year) {
    l <- length(unique(current_year_df$words[current_year_df$id == i]))
    counts_current_year <- c(counts_current_year, l)
  }
  
  if (length(counts_current_year) > 0) {
    avg_counts <- c(avg_counts, mean(counts_current_year))
    med_counts <- c(med_counts, median(counts_current_year))
    std <- c(std, sd(counts_current_year))
    
  } else {
    avg_counts <- c(avg_counts, NA)  # Append NA if no songs for the year
    med_counts <- c(med_counts, NA)
    std <- c(std, NA)
  }
}

# Store results in a dataframe
voc_diversity_df <- data.frame(year = 1950:2019, 
                               avg_count = avg_counts, 
                               median_counts = med_counts,
                               std = std)


# Plot the results
ggplot(voc_diversity_df, aes(x = year)) +
  geom_point(aes(y = med_counts), color = "black", size = 2) +
  geom_smooth(aes(y = med_counts), method = "lm", color = "red", linetype = "dashed", se = T, size = 1, alpha = 0.2) +
  scale_x_continuous(breaks = seq(min(voc_diversity_df$year), max(voc_diversity_df$year), by = 5), 
                     labels = as.character(seq(min(voc_diversity_df$year), max(voc_diversity_df$year), by = 5))) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "top",
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16)) +
  labs(x = "Year", 
       y = "Median Word Count", 
       title = "Diversity of Vocabulary Over Years",
       subtitle = "Median count of unique words in song lyrics by year,  with regression line and standard deviation") +
  theme(plot.background = element_rect(fill = "#F5F5F5"),
        panel.background = element_rect(fill = "#FAFAFA", colour = "#EAEAEA"),
        panel.grid.major = element_line(color = "#EAEAEA"),
        panel.grid.minor = element_blank())


# Create a song dataset that counts the number of unique words in each song
# Create an empty dataframe
count_df <- data.frame(song = combined_lyrics$song,
                       id = combined_lyrics$id,
                       year = combined_lyrics$year,
                       word_count = NA  # Initialize with NA values
)

# Loop through each row and count the words in lyrics
for (i in 1:nrow(combined_lyrics)) {
  # Split the lyrics into words
  words <- strsplit(combined_lyrics$st_cl_lyrics[i], "\\s+")
  # Count the number of unique words
  count <- length(unique(unlist(words)))
  # Assign the count to the corresponding row in count_df
  count_df$word_count[i] <- count
}

# Filter out values above 250
filtered_count_df <- count_df %>%
  filter(word_count <= 200)

# Plot the filtered data
ggplot(filtered_count_df, aes(x = year, y = word_count)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add regression line
  labs(x = "Year", y = "Word Count", title = "Word Count in Song Lyrics Over Years (Excluding Outliers)")







########################## WORD NETWORK #################################

# Generate bigrams dataframe (from original lyrics, then we will filter out bigrams containing stop words)
words <- songs %>%
  unnest_tokens(bigrams, clean_lyrics, token = "ngrams", n = 2)  %>%
  count(bigrams, sort = TRUE)

# split couples of words in variables
words <- words %>%
  separate(bigrams, c("word1", "word2"), sep = " ") %>%
  filter(!(word1 %in% stop_words$word) & !(word2 %in% stop_words$word)) %>%
  filter(word1 != word2)


set.seed(7)
words %>%
  filter(n >= 14) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.4, edge_width = 0.2*n , color = n), show.legend = FALSE) +  # Adjust edge aesthetics
  geom_node_point(color = "darkslategray4", size = 5, alpha = 0.3) +  # Increase node size
  geom_node_text(aes(label = name), vjust = 1.2, size = 3.9, color = "black") +  # Adjust node label aesthetics
  scale_edge_alpha_continuous(range = c(0.1, 0.5)) +  # Adjust edge transparency
  scale_edge_color_continuous(low = "red", high = "darkred") +  
  theme_void() +  # Remove axis and background
  labs(title = "Lyrics Word Network: Top 10 US Songs, 1950 - 2019",
       subtitle = "")
