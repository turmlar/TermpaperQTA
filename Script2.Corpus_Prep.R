# =======================================================
# SCRIPT: Term Paper – Quantitative Text Analysis (QTA)
# Author: Lara Türmer
# Course: Quantitative Text Analysis – WiSe 2024/25
# Topic: Bundestag Debates – Abortion (§218) & Gender-based Violence
# =======================================================

# -------------------------------------------------------
# OPTIONAL: Installation (uncomment if needed)
# -------------------------------------------------------
# install.packages(c("quanteda", "quanteda.textmodels", "tm", "textclean", "textstem", "readr"))
# install.packages("reticulate")

# -------------------------------------------------------
# Load Required Libraries
# -------------------------------------------------------
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)
library(SnowballC)
library(tidyverse)
library(textclean)
library(textstem)
library(knitr)
library(kableExtra)
install.packages("pagedown")
library(pagedown)
# -------------------------------------------------------
# Set Working Directory and Load Data
# -------------------------------------------------------
setwd("/Users/laratuermer/Documents/Studies/Master/WiSe 2425/Quantitative Textanalysis/TermpaperQTA/Sources/bundestag_xml_20wp")
df_rede_final <- readRDS("df_rede_final.rds") # constructed df from plenary protocolls

# Check encoding
unique(Encoding(df_rede_final$Text_Corpus))


# -------------------------------------------------------
# Define Keyword Sets
# -------------------------------------------------------
abtreibung_keywords <- c("abtreibung", "schwangerschaftsabbruch", "schwangerschaftsabbrüche", 
                         "§218", "§219", "218", "219a", "paragraf 218", "paragraf 219a", 
                         "beratungspflicht", "fristenregelung", "werbeverbot", 
                         "selbstbestimmung", "ungeboren", "pro choice", "pro-life", 
                         "lebensschutz", "embryo", "fötus", "medizinische indikation", 
                         "ethische indikation", "frauenarzt", "gynäkologe", "gynäkologin")

gewalt_keywords <- c("gewalt gegen frauen", "gewalt an frauen", "häusliche gewalt", "femizid", 
                     "frauenmord", "ehrenmord", "zwangsheirat", "stalking", 
                     "körperverletzung", "misshandlung", "opferhilfe", "frauenhaus", 
                     "prostitution", "menschenhandel", "prostitutionsschutzgesetz")


# -------------------------------------------------------
# Thematic Filtering And Standardization of Party Names
# -------------------------------------------------------
df_base <- df_rede_final %>%
  mutate(Thema = case_when(
    str_detect(Text_Corpus, regex(paste(abtreibung_keywords, collapse = "|"), ignore_case = TRUE)) ~ "Abtreibung",
    str_detect(Text_Corpus, regex(paste(gewalt_keywords, collapse = "|"), ignore_case = TRUE)) ~ "Gewalt",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Thema))

# Handle missing party affiliation
#na_partei_redner <- df_base %>%
#  filter(is.na(Party)) %>%
#  distinct(Speaker_ID, First_Name, Last_Name)
#print(na_partei_redner)

# party mapping
partei_mapping <- c(
  "11004127" = "B90/DIE GRÜNEN",   # Lisa Paus
  "11004023" = "FDP",             # Marco Buschmann
  "11004801" = "B90/DIE GRÜNEN",   # Sven Lehmann
  "11004902" = "FDP",               # Bettina Stark-Watzinger
  "999990122" = "B90/DIE GRÜNEN"   # Anne Spiegel
)

df_base <- df_base %>%
  mutate(Party = ifelse(
    is.na(Party) & Speaker_ID %in% names(partei_mapping),
    partei_mapping[as.character(Speaker_ID)],
    Party
  ))

# Standardize party labels
df_base <- df_base %>%
  mutate(Party = case_when(
    str_detect(str_to_lower(Party), "linke") ~ "DIE LINKE",
    str_detect(str_to_lower(Party), "fraktionslos") ~ "Fraktionslos",
    str_detect(str_to_lower(Party), "cducsu|cdu/csu") ~ "CDU/CSU",
    str_detect(str_to_lower(Party), "grüne|bündnis 90/die grünen") ~ "B90/DIE GRÜNEN",
    str_to_lower(Party) == "spd" ~ "SPD",
    str_to_lower(Party) == "fdp" ~ "FDP",
    str_to_lower(Party) == "afd" ~ "AfD",
    TRUE ~ Party
  ))

df_base <- df_base %>%
  filter(Party != "Fraktionslos")

# Remove false positive speeches
df_base <- df_base %>%
  filter(!Speech_ID %in% c(
    "ID204509200", "ID203504500", "ID2016902000", "ID2016916200",
    "ID2017412200", "ID2017412300", "ID207513500", "ID207514000",
    "ID2011906400", "ID201112400", "ID2020715400", "ID2020718900", 
    "ID2020713500"
  ))


# -------------------------------------------------------
# Prepare Raw Version (for BERT etc.)
# -------------------------------------------------------
df_218_raw <- df_base %>%
  mutate(Text_Corpus = gsub("\\[.*?\\]", "", Text_Corpus))
saveRDS(df_218_raw, file = "df_218_raw.rds")


# -------------------------------------------------------
# Prepare Cleaned Version (for Quanteda)
# -------------------------------------------------------

df_rede_218 <- df_base %>%
  mutate(Text_Corpus = gsub("\\[.*?\\]", "", Text_Corpus)) %>%
  mutate(Text_Corpus = tolower(Text_Corpus)) %>%
  mutate(Text_Corpus = gsub("\\b\\d+\\b", "", Text_Corpus)) %>%
  mutate(Text_Corpus = gsub("[.,!?;:„“–/\\-]", " ", Text_Corpus)) %>%
  mutate(Text_Corpus = str_squish(Text_Corpus))
saveRDS(df_rede_218, file = "df_rede_218.rds")

# -------------------------------------------------------
# Define Stopwords for Plenary Debate Context (DE)
# -------------------------------------------------------
parliament_stopwords <- c(
  "meine", "sehr", "geehrte", "damen", "herren", "herr", "frau",
  "präsident", "präsidentin", "vizepräsidentin", "vizepräsident",
  "kollege", "kollegin", "kollegen", "kolleginnen", "liebe", "werte",
  "bitte", "danke", "vielen", "herzlichen", "dank",
  "bundestag", "bundesregierung", "regierung", "minister", "ministerin", 
  "staatssekretär", "staatssekretärin", "abgeordnete", "abgeordneten", 
  "deutschland", "gesetz", "gesetzgebung", "ausschuss",
  "dass", "ja", "doch", "halt", "eben", "eigentlich", "nun", "schon",
  "also", "auch", "nur", "mal", "wohl", "vielleicht", "irgendwie", "genau", "und"
)

parliament_stopwords <- c(stopwords("de", source = "snowball"), parliament_stopwords)


# -------------------------------------------------------
# STEP 5: Create and Explore DFM
# -------------------------------------------------------
corp <- corpus(df_rede_218, text_field = 'Text_Corpus')
df_tokens <- quanteda::tokens(corp)
df_tokens <- tokens_remove(df_tokens, pattern = parliament_stopwords)
dfm <- dfm(df_tokens)
saveRDS(dfm, file = "dfm.rds")

# Top words & wordcloud
#print(topfeatures(dfm, 20))
#textplot_wordcloud(dfm, max_words = 50)

# Bar chart of top 20 words
top_words_df <- data.frame(Word = names(topfeatures(dfm, 20)),
                           Frequency = topfeatures(dfm, 20)) %>%
  mutate(Word = factor(Word, levels = rev(Word)))

ggplot(top_words_df, aes(x = Word, y = Frequency)) +
  geom_col(fill = "#046c7c") +
  coord_flip() +
  labs(title = "Top 20 Most Frequent Words (Full Corpus)",
       x = NULL, y = "Frequency") +
  theme_minimal(base_size = 14)

ggsave("top20_words_fullcorpus.png", width = 8, height = 6, dpi = 300)


# Word cloud of top 50 words
png("wordcloud_fullcorpus.png", width = 800, height = 600)
textplot_wordcloud(dfm, max_words = 50, color = "#C14000")
dev.off()


# -------------------------------------------------------
# STEP 6: Party-Specific Analysis
# -------------------------------------------------------
# Wordcloud for AfD
is_afd <- docvars(dfm)$Party == 'AfD'
dfm_afd <- dfm[is_afd, ]
textplot_wordcloud(dfm_afd, max_words = 25)

# Top Words by Party #
# Get unique parties

# get unique party names from dfm
parties <- unique(docvars(dfm)$Party)

# top 10 most frequent words for each party
top_words_per_party <- lapply(parties, function(party) {
  dfm_party <- dfm[docvars(dfm)$Party == party, ]
  topfeatures(dfm_party, 10)  # get top 10 words
})

# Assign party names 
names(top_words_per_party) <- parties

# long-format data frame and add rank
top_words_table <- bind_rows(
  lapply(names(top_words_per_party), function(party) {
    tibble(
      Rank = 1:length(top_words_per_party[[party]]),
      Word = names(top_words_per_party[[party]]),
      Party = party
    )
  })
)

# sort by party and rank
top_words_table <- top_words_table %>%
  arrange(Rank, Party)

# pivot to wide format 
top_words_wide <- top_words_table %>%
  pivot_wider(
    names_from = Party,
    values_from = Word
  )


# table (HTML output)
top_words_wide %>%
  kable("html", caption = "Top Words by Party (Ranked by Frequency)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                font_size = 11)


# PDF exportieren – nutze echten Chrome im Hintergrund
pagedown::chrome_print("top_words_table.html", output = "top_words_table.pdf")




# Keyness analysis
ts <- textstat_keyness(dfm, is_afd)
print(head(ts, 20))


dfm_grouped <- dfm_group(dfm, groups = docvars(dfm, "Party"))

# Run keyness test, AfD is the target
keyness_ts <- textstat_keyness(dfm_grouped, target = "AfD", measure = "chi2")

# Define  custom color palette
keyness_colors <- c("Other" = "#046c7c", "AfD" = "#C14000")

textplot_keyness(keyness_ts, n = 10,
                 color = c(keyness_colors["Other"], keyness_colors["AfD"])) +
  labs(
    title = "Keyness Plot: Distinctive Words in AfD vs. Other Parties",
    subtitle = "Top 10 most distinctive terms per side (Chi-squared test)",
    x = "Chi-squared Score (signed)",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13),
    axis.text.y = element_text(size = 12),
    legend.position = "bottom"
  )




# KWIC: Abortion

# 1. Create corpus from cleaned dataframe
corp_kwic <- corpus(df_218_raw, text_field = "Text_Corpus")

# 2. Tokenize with minimal preprocessing (keep original structure)
tokens_kwic <- quanteda::tokens(corp_kwic)

# 3. Perform KWIC search for the term "abtreibung" with a context window of 8 words
kwic_abtreibung <- kwic(tokens_kwic, pattern = "abtreibung*", window = 10)

# 4. Convert KWIC results to a data frame
kwic_df <- as.data.frame(kwic_abtreibung)

# 5. Match document names to original row indices
doc_index <- match(kwic_df$docname, docnames(corp_kwic))

# 6. Add metadata (Speech_ID and Party) from the original corpus
kwic_df$Speech_ID <- docvars(corp_kwic, "Speech_ID")[doc_index]
kwic_df$Party <- docvars(corp_kwic, "Party")[doc_index]

# 7. Display KWIC results with metadata in a styled HTML table

kwic_table <- kwic_df %>%
  select(Speech_ID, Party, pre, keyword, post) %>%
  arrange(Party) %>%
  kable("html", caption = "KWIC Results for 'abtreibung' (Sorted by Party)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), font_size = 11)
# Save the kable table as a standalone HTML file
save_kable(
  kwic_table,
  file = "kwic_table_abtreibung.html"
)
pagedown::chrome_print("kwic_table_abtreibung.html", output = "kwic_table_abtreibung.pdf")


