### 1. First Foray

## 1.1. Loading the first text file

library(quanteda)
library(readtext)
data_macbeth <- texts(readtext("https://www.gutenberg.org/files/1533/1533-0.txt"))
names(data_macbeth) <- "Macbeth"

library(stringi)
stri_sub(data_macbeth, 1, 65)

## 1.2. Separate content from metadata

# Extract the header information
(start_v <- stri_locate_first_fixed(data_macbeth, "SCENE I. An open Place.")[1])
(end_v <- stri_locate_last_fixed(data_macbeth, "[_Flourish. Exeunt._]")[1])

# Verify that "[_Flourish. Exeunt._]" is the end of the novel
kwic(tokens(data_macbeth), "[_Flourish. Exeunt._]")

stri_count_fixed(data_macbeth, "\n")

stri_sub(data_macbeth, from = start_v, to = end_v) %>%
  stri_count_fixed("\n")

novel_v <- stri_sub(data_macbeth, start_v, end_v)
novel_v = gsub("???", "", novel_v)
novel_v = gsub("T", "", novel_v)
length(novel_v)

stri_sub(novel_v, 1, 70) %>% cat()

## 1.3. Reprocessing the content

# Lowercase text
novel_lower_v <- char_tolower(novel_v)

macbeth_word_v <- tokens(novel_lower_v, remove_punct = TRUE) %>% as.character()
(total_length <- length(macbeth_word_v))

macbeth_word_v[1:11]

macbeth_word_v[9999] 

macbeth_word_v[c(6,7,8)]

# Check positions of "love"
which(macbeth_word_v == "love") %>% head()

## 1.4. Beginning the analysis

length(macbeth_word_v[which(macbeth_word_v == "love")])

# Same thing using kwic()
nrow(kwic(novel_lower_v, pattern = "love"))

nrow(kwic(novel_lower_v, pattern = "love*")) # Includes words like "whalemen"

(total_love_hits <- nrow(kwic(novel_lower_v, pattern = "^love{0,1}$", valuetype = "regex")))

total_love_hits / ntoken(novel_lower_v, remove_punct = TRUE)  

# Total unique words
length(unique(macbeth_word_v))

ntype(char_tolower(novel_v), remove_punct = TRUE)

# Ten most frequent words
macbeth_dfm <- dfm(novel_lower_v, remove_punct = TRUE)

head(macbeth_dfm, nf = 10)

library("quanteda.textstats")
textstat_frequency(macbeth_dfm, n = 10)

# Plot frequency of 50 most frequent terms 
library(ggplot2)
theme_set(theme_minimal())
textstat_frequency(macbeth_dfm, n = 50) %>% 
  ggplot(aes(x = rank, y = frequency)) +
  geom_point() +
  labs(x = "Frequency rank", y = "Term frequency")

sorted_macbeth_freqs_t <- topfeatures(macbeth_dfm, n = nfeat(macbeth_dfm))

### 2. Accessing and Comparing Word Frequency Data

## 2.1. Accessing Word Data

# Frequencies of "he" and "she" - these are matrixes, not numerics
sorted_macbeth_freqs_t[c("he", "she", "him", "her")]

# Another method: indexing the dfm
macbeth_dfm[, c("he", "she", "him", "her")]

sorted_macbeth_freqs_t[1]

sorted_macbeth_freqs_t["the"]

# Term frequency ratios
sorted_macbeth_freqs_t["him"] / sorted_macbeth_freqs_t["her"]

sorted_macbeth_freqs_t["he"] / sorted_macbeth_freqs_t["she"]

ntoken(macbeth_dfm)

sum(sorted_macbeth_freqs_t)

## 2.2. Recycling

sorted_macbeth_rel_freqs_t <- sorted_macbeth_freqs_t / sum(sorted_macbeth_freqs_t) * 100
sorted_macbeth_rel_freqs_t["the"]

# By weighting the dfm directly
macbeth_dfm_pct <- dfm_weight(macbeth_dfm, scheme = "prop") * 100

dfm_select(macbeth_dfm_pct, pattern = "the")

plot(sorted_macbeth_rel_freqs_t[1:10], type = "b",
     xlab = "Top Ten Words", ylab = "Percentage of Full Text", xaxt = "n")
axis(1,1:10, labels = names(sorted_macbeth_rel_freqs_t[1:10]))

textstat_frequency(macbeth_dfm_pct, n = 10) %>% 
  ggplot(aes(x = reorder(feature, -rank), y = frequency)) +
  geom_bar(stat = "identity") + coord_flip() + 
  labs(x = "", y = "Term Frequency as a Percentage")

### 3. Token Distribution Analysis

## 3.1. Dispersion plots