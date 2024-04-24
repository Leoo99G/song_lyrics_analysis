library(cld3)
library(qdapDictionaries)
library(stringr)
library(dplyr)
library(tidytext)
library(stopwords)


# Set working directory
setwd("../DSD_Project")
# Import the dataset
songs <- read.csv2("filled_okTopSongsLyrics1950_2019.csv", row.names=NULL, encoding="UTF8")

# Check dimensions (700 songs (top 10 songs for 70 years), 5 features)
cat('The dataset has', dim(songs)[1], 'observations and', dim(songs)[2], 'variables.')

# Check how many empty lyrics there are
songs[songs$lyrics == "",]


####################################################################
########################   PRE-PROCESSING   ########################
####################################################################


# Function to process song lyrics obtained from Genius (to have the same format of other songs)
genius_lyrics_process <- function(song_lyrics) {
  
  if (is.null(song_lyrics) || song_lyrics == "") {
    stop("Input song lyrics is NULL or empty.")
  }
  
  # Remove substrings within square brackets
  song_lyrics <- gsub("\\[.*?\\]", "", song_lyrics, perl = TRUE)
  
  # Replace "\n" with "|", trim leading and trailing whitespace
  song_lyrics <- gsub("\\n", "|", song_lyrics)
  song_lyrics <- trimws(song_lyrics)
  
  # Replace consecutive "|" with just one, remove "|" if at beginning or end
  song_lyrics <- gsub("\\|+", "|", song_lyrics)
  song_lyrics <- gsub("^\\||\\|$", "", song_lyrics)
  
  # Return the processed lyrics
  return(song_lyrics)
}


# Positions of songs with nonsensical lyrics that have been replaced by genius lyrics
wrong_lyrics_pos <- c(42, 63, 65, 67, 72, 104, 156, 158, 241, 261, 272, 322, 
                      325, 349, 356, 363, 596, 618, 635, 641, 650, 656, 659, 
                      661, 662, 663, 666, 667, 673, 674, 675, 677, 688)
# Update these songs to share the same format as the other ones
for (pos in wrong_lyrics_pos) {
  songs$lyrics[pos] <- genius_lyrics_process(songs$lyrics[pos])
}


# DETECTING LANGUAGE with Chrome language detector #

# Vector of detected language for each song
lan <- detect_language(songs$lyrics)

# Add this as a column to the songs dataset
songs$language <- lan

# Inspect songs whose language is NA
songs$lyrics[is.na(songs$language)]

# Notice that empty lyrics get assigned "ja" language. I'll change those with NA
songs$language[songs$lyrics==""] <- NA

# Position of observations (songs) whose language is not detected as "en" or is NA
which(songs$language!="en" | is.na(songs$language))

# Manually correct wrong language detection
songs$language[51] <- "en"
songs$language[279] <- "en"
songs$language[338] <- "en"

# Check that the remaining ones are actually non-English (we'll remove these observations)
to_remove <- which(songs$language!="en" | is.na(songs$language))
songs <- songs[-to_remove, ]

# Check that the remaining songs have language detected as "en"
sum(songs$language=="en") == dim(songs)[1]
dim(songs)[1]


## TEXT CLEANING
clean_lyrics <- gsub("\\b\\d+\\b", "", songs$lyrics)    # Remove numbers
clean_lyrics <- gsub("\\s+", " ", clean_lyrics)         # Remove extra whitespaces
clean_lyrics <- tolower(clean_lyrics)                   # Convert to lowercase
clean_lyrics <- gsub("\\|", " ", clean_lyrics)          # Replace | with space


# Find all contractions using regular expressions
contractions <- unique(unlist(regmatches(clean_lyrics, gregexpr("\\b[a-z]+'[a-z]+\\b", clean_lyrics, ignore.case = TRUE))))
# Print the contractions found
print(contractions)

contractions <- list(
  "ain't" = "be not",
  "'m" = " am",
  "'re" = " are",
  "can't" = "cannot",
  "won't" = "will not",
  "n't" = " not",
  "'ll" = " will",
  "'d" = " would",
  "'ve" = " have",
  "y'all" = "you all",
  "c'mon" = "come on",
  "what's" = "what is",
  "it's" = "it is",
  "she's" = "she is",
  "he's" = "he is",
  "how's" = "how is",
  "let's" = "let us",
  "i'mma" = "i am going to",
  "i'ma" = "i am going to",
  "there's" = "there is"
  )

# Function to replace contractions in a string using the provided list
replace_contractions <- function(text, contractions_list) {
  for (contraction in names(contractions_list)) {
    text <- gsub(contraction, contractions_list[[contraction]], text, ignore.case = TRUE)
  }
  return(text)
}

# Apply the function to the 'lyrics' column of the 'songs' dataframe
clean_lyrics <- lapply(clean_lyrics, replace_contractions, contractions)

# Remove all remaining " 's " (they should be possessive case)
clean_lyrics <- gsub("'s", "", clean_lyrics)

# Remove punctuation
clean_lyrics <- gsub("[[:punct:]]", " ", clean_lyrics)


# Words ending in "in"

# Find all words ending with "in" using regular expressions
words_ending_in_in <- unique(unlist(regmatches(clean_lyrics, gregexpr("\\b\\w*in\\b", clean_lyrics, ignore.case = TRUE))))

# Output the words ending with "in"
print(words_ending_in_in)

# Function that checks whether an input word exists in the English dictionary
is.word  <- function(x) {x %in% DICTIONARY$word}

# Words ending in -in to leave unchanged
already_in_dict <- character(0)
for (w in words_ending_in_in) {
  if (is.word(w)){
    already_in_dict <- c(already_in_dict, w)
  }
}

# print("Words ending with -in that are words in the dictionary (to leave unchanged)")
print(already_in_dict)

# -ing verbs with truncated g (we will add a g only to these words)
words_to_add_g <- character(0)
for (w in words_ending_in_in) {
  if (!w %in% already_in_dict) {
    words_to_add_g <- c(words_to_add_g, w)
  }
}
 print(words_to_add_g)

# Function to add a "g" to ing verbs with final "g" truncated
add_g_where_needed <- function(text, exceptions = already_in_dict) {
 # Define a function to add 'g' to words ending with "in"
 add_g <- function(sentence) {
   words <- unlist(strsplit(sentence, "\\s+"))  # Split sentence into words
   modified_words <- lapply(words, function(word) {
     if (substr(word, nchar(word) - 1, nchar(word)) == "in" && !(word %in% exceptions)) {
       return(paste0(substr(word, 1, nchar(word) - 2), "ing"))
     } else {
       return(word)
     }
   })
   return(paste(modified_words, collapse = " "))  # Reconstruct the sentence
 }
 
 # Apply the add_g function to each sentence in the text
 result <- lapply(text, add_g)
 
 return(result)
}
 

clean_lyrics <- add_g_where_needed(clean_lyrics)

# Second check
# Find all words ending with "in" using regular expressions
words_ending_in_in_2 <- unique(unlist(regmatches(clean_lyrics, gregexpr("\\b\\w*in\\b", clean_lyrics, ignore.case = TRUE))))
words_ending_in_in_2
words_ending_in_ing <- unique(unlist(regmatches(clean_lyrics, gregexpr("\\b\\w*ing\\b", clean_lyrics, ignore.case = TRUE))))
words_ending_in_ing
# Everything seems okay now

# Remove multiple spaces using regular expressions
clean_lyrics <- gsub("\\s+", " ", clean_lyrics)

# Add a column with cleaned lyrics to the dataframe
songs$clean_lyrics <- clean_lyrics

# Add unique identifiers
songs$id <- row.names(songs)

################################################################################


# # Splitting the lyrics column into individual words (each word in a row of the dataframe)
df <- songs
df$words <- str_split(df$clean_lyrics, "\\s+")

# Explode the list of words into multiple rows
df_expanded <- df[rep(seq_len(nrow(df)), lengths(df$words)), ]
df_expanded$words <- unlist(df$words)

# Remove the original lyrics column
df_expanded$lyrics <- NULL
df_expanded$clean_lyrics <- NULL

# Reset row names to ensure integer row numbers
rownames(df_expanded) <- NULL


# Stopwords

# Get the English stopwords
english_stopwords <- stopwords::stopwords("en") # few stopwords (175)
english_stopwords_2 <- tidytext::stop_words$word # more stopwords (1149)

# Filter out English stopwords and words not appearing in the English dictionary
filtered_songs <- df_expanded %>%
  filter(!(words %in% english_stopwords_2) & (words %in% DICTIONARY$word))

# Filter out rows containing words with 1 letter
to_delete <- length(filtered_songs$words) == 1
filtered_songs <- filtered_songs[!to_delete,]

# Filter out rows  of words containing numbers
contains_number <- function(text) {
  grepl("[0-9]", text)
}
to_delete <- contains_number(filtered_songs$words)
filtered_songs <- filtered_songs[!to_delete,]
