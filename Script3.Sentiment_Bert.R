# ============================================
# SENTIMENT ANALYSIS
# ============================================

# If not installed yet:
# install.packages("reticulate")

setwd("/Users/laratuermer/Documents/Studies/Master/WiSe 2425/Quantitative Textanalysis/TermpaperQTA/Sources/bundestag_xml_20wp")

df_218_raw <- readRDS("df_218_raw.rds")

library(dplyr)
library(pbapply)
library(reticulate)

# Remove previous virtualenv if not needed
virtualenv_remove("r-reticulate")

# Create and activate a fresh virtual environment
virtualenv_create("r-reticulate")

virtualenv_install("r-reticulate", 
                   packages = c("transformers", "torch", "sentencepiece"), 
                   pip = TRUE,
                   pip_options = NULL)

use_virtualenv("r-reticulate", required = TRUE)

use_python("/Users/laratuermer/.virtualenvs/r-reticulate/bin/python", required = TRUE)

# Check if Python is available
py_available()
py_config()

# Load pretrained transformer model
transformers <- import("transformers")
tokenizer <- transformers$AutoTokenizer$from_pretrained("oliverguhr/german-sentiment-bert")
model <- transformers$AutoModelForSequenceClassification$from_pretrained("oliverguhr/german-sentiment-bert")
pipeline <- transformers$pipeline("sentiment-analysis", model = model, tokenizer = tokenizer)

# Quick test
pipeline("Ich finde dieses Gesetz wirklich gefährlich und unverantwortlich.")


# ============================================
# Helper Functions
# ============================================

# Split long texts into ~510-token chunks
split_text_into_chunks_by_tokens <- function(text, max_tokens = 510) {
  encoding <- tokenizer$encode_plus(
    text,
    add_special_tokens = FALSE,
    return_attention_mask = FALSE,
    return_token_type_ids = FALSE
  )
  
  input_ids <- encoding$input_ids
  chunked_ids <- split(input_ids, ceiling(seq_along(input_ids) / max_tokens))
  
  lapply(chunked_ids, function(ids) {
    tokenizer$decode(ids, skip_special_tokens = TRUE)
  })
}

# Analyze a single speech
split_and_analyze <- function(text, party, id) {
  clean_text <- function(text) {
    text %>%
      gsub("\\[.*?\\]", "", .) %>%
      gsub("\\s+", " ", .) %>%
      trimws()
  }
  
  # Lookup party if missing
  if (is.na(party)) {
    party_lookup <- df_218_raw %>%
      filter(Speech_ID == id) %>%
      pull(Party)
    
    if (length(party_lookup) > 0 && !is.na(party_lookup)) {
      party <- party_lookup
      message(sprintf("Party filled from df_218_raw for ID %s: %s", id, party))
    } else {
      message(sprintf("Party NOT found for ID %s", id))
    }
  }
  
  text <- clean_text(text)
  
  if (nchar(text) < 20) {
    message(sprintf("Text too short: %s", id))
    return(data.frame(Party = party, Label = NA, Score = NA, Num_Chunks = 0, Speech_ID = id))
  }
  
  chunks <- tryCatch({
    if (length(tokenizer$tokenize(text)) > 512) {
      split_text_into_chunks_by_tokens(text)
    } else {
      list(text)
    }
  }, error = function(e) {
    message("Tokenization failed for ID ", id)
    list(text)
  })
  
  results <- lapply(chunks, function(chunk) {
    tryCatch({
      res <- pipeline(chunk)
      if (!is.null(res[[1]]$label) && !is.null(res[[1]]$score)) {
        data.frame(label = res[[1]]$label, score = res[[1]]$score)
      } else {
        data.frame(label = NA, score = NA)
      }
    }, error = function(e) {
      message(sprintf("Error in chunk for ID %s: %s", id, e$message))
      data.frame(label = NA, score = NA)
    })
  })
  
  results_df <- bind_rows(results)
  valid <- results_df %>% filter(!is.na(label))
  
  if (nrow(valid) == 0) {
    return(data.frame(Party = party, Label = NA, Score = NA, Num_Chunks = length(chunks), Speech_ID = id))
  }
  
  agg <- valid %>%
    group_by(label) %>%
    summarise(n = n(), avg_score = mean(score), .groups = "drop") %>%
    slice_max(n, n = 1)
  
  data.frame(
    Party = party,
    Label = agg$label,
    Score = agg$avg_score,
    Num_Chunks = length(chunks),
    Speech_ID = id
  )
}



# ============================================
# Sentiment Analysis Execution
# ============================================

# Run analysis with progress bar
sentiment_results <- pblapply(1:nrow(df_218_raw), function(i) {
  split_and_analyze(
    text = df_218_raw$Text_Corpus[i],
    party = df_218_raw$Party[i],
    id = df_218_raw$Speech_ID[i]
  )
})

# Combine results
results_df <- bind_rows(sentiment_results)



# ============================================
# Aggregate and Save Summary
# ============================================

sentiment_summary <- results_df %>%
  group_by(Party, Label) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Party) %>%
  mutate(Share = Count / sum(Count))


write.csv(sentiment_summary, "sentiment_summary.csv", row.names = FALSE)



# ============================================
# Visualization
# ============================================

library(ggplot2)
library(viridis) 
library(dplyr)

# -------- Plot 1: Absolute Counts --------

ggplot(sentiment_summary, aes(x = Party, y = Count, fill = Label)) +
  geom_col(position = "stack", color = "white", width = 0.5) +
  scale_fill_manual(
    values = c(
      "positive" = "#F4A300",  # amber
      "neutral"  = "#046c7c",  # blue-gray
      "negative" = "#C14000"   # rust
    )
  ) +
  labs(
    title = "Number of Speech Segments by Sentiment and Party",
    subtitle = "Stacked bar chart showing absolute counts",
    x = "Party",
    y = "Count",
    fill = "Sentiment"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "right"
  )

# Save the plot
ggsave("sentiment_absolute.png", width = 10, height = 6, dpi = 300)



# -------- Plot 2: Sentiment Shares --------

ggplot(sentiment_summary, aes(x = Party, y = Share, fill = Label)) +
  geom_col(position = "fill", color = "white", width = 0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(
    values = c(
      "positive" = "#F4A300",  # amber
      "neutral"  = "#046c7c",  # blue-gray
      "negative" = "#C14000"   # rust
    )
  ) +
  labs(
    title = "Sentiment Distribution by Party",
    subtitle = "Proportion of positive, neutral, and negative speech content",
    x = "Party",
    y = "Proportion",
    fill = "Sentiment"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "right"
  )

# Save the plot
ggsave("sentiment_share.png", width = 10, height = 6, dpi = 300)

