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
novel_v = gsub("€", "", novel_v)
novel_v = gsub("™", "", novel_v)
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

# Using words from tokenized corpus for dispersion
library("quanteda.textplots")
textplot_xray(kwic(novel_v, pattern = "macbeth")) + 
  ggtitle("Lexical dispersion")

textplot_xray(
  kwic(novel_v, pattern = "macbeth"),
  kwic(novel_v, pattern = "macduff")) + 
  ggtitle("Lexical dispersion")

## 3.2. Searching with regular expression

# Identify the chapter break locations
chap_positions_v <- kwic(novel_v, phrase(c("SCENE")), valuetype = "regex")$from

head(chap_positions_v)
chap_positions_v

## 3.3. Identifying chapter breaks

chapters_corp <- 
  corpus(novel_v) %>%
  corpus_segment(pattern = "SCENE\\s*.*\\n", valuetype = "regex")
summary(chapters_corp, 10)

docvars(chapters_corp, "pattern") <- stringi::stri_trim_right(docvars(chapters_corp, "pattern"))
summary(chapters_corp, n = 3)

docnames(chapters_corp) <- docvars(chapters_corp, "pattern")

## 3.4. Barplots of Macbeth and Macduff

# Create a dfm
chap_dfm <- dfm(chapters_corp)

# Extract row with count for "whale"/"ahab" in each chapter and convert to data frame for plotting
macbeth_macduff_df <- chap_dfm %>% 
  dfm_keep(pattern = c("macbeth", "macduff")) %>% 
  convert(to = "data.frame")

macbeth_macduff_df$chapter <- 1:nrow(macbeth_macduff_df)

ggplot(data = macbeth_macduff_df, aes(x = chapter, y = macbeth)) + 
  geom_bar(stat = "identity") +
  labs(x = "Chapter", 
       y = "Frequency",
       title = 'Occurrence of "Macbeth"')

ggplot(data = macbeth_macduff_df, aes(x = chapter, y = macduff)) + 
  geom_bar(stat = "identity") +
  labs(x = "Chapter", 
       y = "Frequency",
       title = 'Occurrence of "Macduff"')

rel_dfm <- dfm_weight(chap_dfm, scheme = "prop") * 100
head(rel_dfm)

# Subset dfm and convert to data.frame object
rel_chap_freq <- rel_dfm %>% 
  dfm_keep(pattern = c("macbeth", "macduff")) %>% 
  convert(to = "data.frame")

rel_chap_freq$chapter <- 1:nrow(rel_chap_freq)
ggplot(data = rel_chap_freq, aes(x = chapter, y = macbeth)) + 
  geom_bar(stat = "identity") +
  labs(x = "Chapter", y = "Relative frequency",
       title = 'Occurrence of "Macbeth"')

ggplot(data = rel_chap_freq, aes(x = chapter, y = macduff)) + 
  geom_bar(stat = "identity") +
  labs(x = "Chapter", y = "Relative frequency",
       title = 'Occurrence of "Macduff"')

### 4. Correlation

## 4.1. Correlation Analysis

dfm_weight(chap_dfm, scheme = "prop") %>% 
  textstat_simil(selection = c("macbeth", "macduff"), method = "correlation", margin = "features") %>%
  as.matrix() %>%
  head(2)

## 4.2. Testing Correlation with Randomization+

cor_data_df <- dfm_weight(chap_dfm, scheme = "prop") %>% 
  dfm_keep(pattern = c("macbeth", "macduff")) %>% 
  convert(to = "data.frame")

# Sample 1000 replicates and create data frame
n <- 1000
samples <- data.frame(
  cor_sample = replicate(n, cor(sample(cor_data_df$macbeth), cor_data_df$macduff)),
  id_sample = 1:n
)

# Plot distribution of resampled correlations
ggplot(data = samples, aes(x = cor_sample, y = ..density..)) +
  geom_histogram(colour = "black", binwidth = 0.01) +
  geom_density(colour = "red") +
  labs(x = "Correlation Coefficient", y = NULL,
       title = "Histogram of Random Correlation Coefficients with Normal Curve")

### 5. Measures of Lexical Variety

## 5.1. Mean word frequency

# Length of the book in chapters
ndoc(chapters_corp)

# Chapter names
docnames(chapters_corp) %>% head()

# For first few chapters
ntoken(chapters_corp) %>% head()

# Average
(ntoken(chapters_corp) / ntype(chapters_corp)) %>% head()

## 5.2. Extracting Word Usage Means

(ntoken(chapters_corp) / ntype(chapters_corp)) %>%
  plot(type = "h", ylab = "Mean word frequency")

(ntoken(chapters_corp) / ntype(chapters_corp)) %>%
  scale() %>%
  plot(type = "h", ylab = "Scaled mean word frequency")

## 5.3. Ranking the values

mean_word_use_m <- (ntoken(chapters_corp) / ntype(chapters_corp))
sort(mean_word_use_m, decreasing = TRUE) %>% head()

## 5.4. Calculating the TTR

dfm(chapters_corp) %>% 
  textstat_lexdiv(measure = "TTR") %>%
  head(n = 10)

### 6. Hapax Richness

# Hapaxes per document
rowSums(chap_dfm == 1) %>% head()

# As a proportion
hapax_proportion <- rowSums(chap_dfm == 1) / ntoken(chap_dfm)
head(hapax_proportion)

barplot(hapax_proportion, beside = TRUE, col = "grey", names.arg = seq_len(ndoc(chap_dfm)))