# LDA
# Required libraries
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textstats)
library(quanteda.textplots)
library(topicmodels)
library(tidyverse)
library(reticulate)

setwd("/Users/laratuermer/Documents/Studies/Master/WiSe 2425/Quantitative Textanalysis/TermpaperQTA/Sources/bundestag_xml_20wp")

# Load required libraries
library(udpipe)
library(tidyverse)
library(quanteda)
library(topicmodels)


# 1. Load cleaned speeches
df_rede_218 <- readRDS("df_rede_218.rds")

# 2. Load or download German UDPipe model
ud_model <- udpipe_download_model(language = "german", model_dir = "models/")
ud_model <- udpipe_load_model(file = ud_model$file_model)

# 3. POS-tagging + lemmatization
anno <- udpipe_annotate(ud_model, x = df_rede_218$Text_Corpus, doc_id = df_rede_218$Speech_ID)
anno_df <- as.data.frame(anno)

# 4. Filter only NOUNs
nouns_only <- anno_df %>%
  filter(upos == "NOUN") %>%
  select(doc_id, lemma)

# 5. Collapse lemmas back to documents
nouns_grouped <- nouns_only %>%
  group_by(doc_id) %>%
  summarise(text_nouns = paste(lemma, collapse = " "))

# 6. Merge with party info
nouns_grouped <- nouns_grouped %>%
  left_join(df_rede_218 %>% select(Speech_ID, Party), by = c("doc_id" = "Speech_ID"))

# 7. Create corpus and DFM
corp_nouns <- corpus(nouns_grouped, text_field = "text_nouns")
docvars(corp_nouns, "Party") <- nouns_grouped$Party

# Tokenize corpus before creating DFM
toks_nouns <- tokens(corp_nouns) %>%
  tokens_remove(pattern = parliament_stopwords)

dfm_nouns <- dfm(toks_nouns) %>%
  dfm_trim(min_termfreq = 5)

# 8. Topic Modeling (LDA)
dtm_nouns <- convert(dfm_nouns, to = "topicmodels")
lda_model_nouns <- LDA(dtm_nouns, k = 5, method = "Gibbs", control = list(seed = 1234))

# 9. Extract topics per doc
topics_nouns <- topics(lda_model_nouns)
nouns_grouped$Topic_LDA_NOUNS <- as.factor(topics_nouns)

# 10. Save for reuse
saveRDS(lda_model_nouns, "lda_model_nouns.rds")
saveRDS(nouns_grouped, "df_nouns_topics.rds")



# 11. View top words
terms(lda_model_nouns, 10)
# NEW: Plot topic distribution across parties (absolute + optional relative)
# Definieren der Parteifarben
#party_colors <- c(
 # "CDU/CSU" = "#000000",
  #"SPD" = "#E3000F",
  #"B90/DIE GRÜNEN" = "#00A646",
  #"FDP" = "#FFEF00",
  #"AfD" = "#0489DB",
  #"DIE LINKE" = "#A6006B",  # Alternativer Farbton für Unterscheidung
  #"BSW" = "#520085"
#)


# 9b. Count how many documents each party has in each topic
topic_party_counts <- nouns_grouped %>%
  filter(!is.na(Topic_LDA_NOUNS)) %>%
  count(Party, Topic_LDA_NOUNS)



# Plot absolute counts
ggplot(topic_party_counts, aes(x = Party, y = n, fill = Party)) +
  geom_col(position = "dodge", color = "white", width = 0.7) +
  facet_wrap(~ Topic_LDA_NOUNS, scales = "free_y") +
  scale_fill_manual(values = party_colors) +
  labs(
    title = "Number of Speeches per Topic and Party",
    x = "Party",
    y = "Count",
    fill = "Party"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "right"
  )

ggsave("topic_party_distribution_absolute.png", width = 10, height = 6, dpi = 300)


# Calculate relative share within each topic
topic_party_relative <- topic_party_counts %>%
  group_by(Topic_LDA_NOUNS) %>%
  mutate(Relative = n / sum(n)) %>%
  ungroup()

# Plot: relative frequency per party in each topic (like absolute)
ggplot(topic_party_relative, aes(x = Party, y = Relative, fill = Party)) +
  geom_col(position = "dodge", color = "white", width = 0.7) +
  facet_wrap(~ Topic_LDA_NOUNS, scales = "free_y") +
  scale_fill_manual(values = party_colors) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Relative Share of Speeches by Party per Topic",
    x = "Party",
    y = "Share",
    fill = "Party"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none"
  )

# Save as PNG
ggsave("topic_party_distribution_relative.png", width = 10, height = 6, dpi = 300)




ggplot(topic_party_relative, aes(x = Party, y = Relative, fill = "gray")) +
  geom_col(position = "dodge", color = "white", width = 0.7) +
  facet_wrap(~ Topic_LDA_NOUNS, scales = "free_y") +
  scale_fill_manual(values = c("gray" = "#A9A9A9"), guide = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Relative Share of Speeches per Topic and Party",
    x = "Party",
    y = "Share of Topic (in %)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

# Save it
ggsave("topic_party_distribution_relative_gray.png", width = 10, height = 6, dpi = 300)




# 1. Load sentiment results
sentiment_df <- readRDS("sentiment_by_chunk.rds") %>%
  rename(Sentiment = Label)

# 2. Merge into nouns_grouped
df_topics_nouns_sentiment <- nouns_grouped %>%
  left_join(sentiment_df %>% select(Speech_ID, Sentiment, Score), by = c("doc_id" = "Speech_ID"))

# 3. Aggregate sentiment by topic & party
sentiment_summary_nouns <- df_topics_nouns_sentiment %>%
  filter(!is.na(Topic_LDA_NOUNS)) %>%
  group_by(Party, Topic_LDA_NOUNS, Sentiment) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Party, Topic_LDA_NOUNS) %>%
  mutate(Proportion = Count / sum(Count))

# 4. Plot
library(ggplot2)

ggplot(sentiment_summary_nouns, aes(x = Party, y = Proportion, fill = Sentiment)) +
  geom_bar(stat = "identity", position = "stack", color = "white", width = 0.7) +
  facet_wrap(~ Topic_LDA_NOUNS, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "positive" = "#F4A300",
      "neutral"  = "#046c7c",
      "negative" = "#C14000"
    )
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Sentiment by Topic and Party (Noun-based)",
    y = "Proportion",
    x = "Party",
    fill = "Sentiment"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "right"
  )

# 5. Save plot
ggsave("sentiment_by_topic_party_nouns.png", width = 10, height = 6, dpi = 300)




## determine k selection
k_values <- 2:10
perplexities <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  model <- LDA(dtm_nouns, k = k_values[i], method = "Gibbs", control = list(seed = 1234))
  perplexities[i] <- perplexity(model, dtm_nouns)
}

# Beispiel: DataFrame
df_perplexity <- data.frame(
  k = k_values,
  perplexity = perplexities
)

# Plot
ggplot(df_perplexity, aes(x = k, y = perplexity)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue", size = 2) +
  labs(
    title = "Perplexity by Number of Topics",
    x = "Number of Topics (k)",
    y = "Perplexity"
  ) +
  theme_minimal()

ggsave("perplexity_by_k.png", width = 7, height = 5, dpi = 300)


library(tibble)
library(kableExtra)

# Step 1: Define topic labels in English
topic_labels <- c(
  "Topic 1" = "Feminist Discourse & Role of Women",
  "Topic 2" = "Reproductive Rights & Regulation",
  "Topic 3" = "Life, Children & Self-Determination",
  "Topic 4" = "Political Proposals & Parliamentary Debate",
  "Topic 5" = "Government & Party Politics"
)

# Step 2: Define top terms per topic
top_terms <- list(
  `Topic 1` = c("frauen", "fraktion", "schluß", "lieb", "schutz", "tagen", "präsidentin", "rednerin", "welt", "situation"),
  `Topic 2` = c("§", "entscheidung", "information", "frau", "regelung", "seit", "abtreibung", "beratung", "gesetzentwurf", "kommission"),
  `Topic 3` = c("recht", "kind", "leb", "selbstbestimmung", "fragen", "kinder", "unterstützung", "lieb", "abschaffung", "familie"),
  `Topic 4` = c("jahr", "thema", "antrag", "lieb", "gesetz", "verantwortung", "fall", "dank", "punkt", "situation"),
  `Topic 5` = c("bundesregierung", "fraktion", "koalition", "regierung", "jahren", "zeit", "union", "politik", "ampelkoalition", "ergebnis")
)

# Step 3: Build the data frame
topic_table <- tibble(
  Topic = names(top_terms),
  `Topic Label` = topic_labels[names(top_terms)],
  `Top Terms` = sapply(top_terms, function(words) paste(words, collapse = ", "))
)

# Step 4: Create a pretty table with kableExtra
topic_table <- topic_table %>%
  kable("html", caption = "LDA Topics: Descriptions and Top Terms", escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE)

# Save the kable table as a standalone HTML file
save_kable(
  topic_table,
  file = "topics_LDA.html"
)
pagedown::chrome_print("topics_LDA.html", output = "topics_LDA.pdf")
