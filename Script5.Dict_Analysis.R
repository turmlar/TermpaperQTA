# ================================================================
# Framing Analysis Using Dictionary-Based Content Categorization 
# in Parliamentary Speeches (Lemmatized with UDPipe)
# ================================================================

# Load required libraries
library(quanteda)
library(quanteda.textplots)
library(tidyverse)
library(dplyr)
library(udpipe)

# ---------------------------------------
# 1. Prepare lowercase text for UDPipe
# ---------------------------------------
df_218_dict <- df_218_raw %>%
  mutate(Text_Corpus = tolower(Text_Corpus))  # Lowercasing for consistency

# ---------------------------------------
# 2. Define Framing Dictionaries
# ---------------------------------------

# A. Paternalistic Framing (State protection & guidance)
paternalistic_dict <- c(
  "schutzpflicht", "schutzbedürftig", "fürsorge", "beratungsbedürftig", 
  "beratungspflicht", "wohlwollend", "führung", "beistand", "beistehen", 
  "verantwortung", "verantworten", "verantwortlich", "pflicht", "bedenkzeit",
  "hilfe", "unterstützung", "begleitung", "prävention", "präventiv", 
  "staatlich", "staatliche verantwortung", "staatliche fürsorge", 
  "ermutigen", "bestärken", "auszutragen", "entscheidungshilfe",
  "wohl der frau", "interesse der frau", "interesse des kindes", "mütterlich", 
  "wohlmeinend", "begleiteter entscheidungsprozess"
)

# B. Moral-Emotive Framing (Religious & ethical judgment)
moral_emotive_dict <- c(
  "sünde", "sündhaft", "schande", "schändlich", "frevel", "verwerflich", 
  "unmoralisch", "unethisch", "unrecht", "moralisch falsch", "schuld", "schuldig", 
  "schuldgefühl", "gewissen", "gewissensbelastung", "bereuen", "reue",
  "tabu", "tabubruch", "dammbruch", "würde", "menschenwürde", "heilig", "heiligkeit", 
  "unantastbar", "leben heilig", "verbrechen", "mord", "töten", "tötung", 
  "gottgewollt", "lebensfeindlich", "lebensverachtung", "lebensverachtend", 
  "gott", "herzlos", "moralischer verfall"
)

# C. Pro-Life Framing (Protection of unborn life)
pro_life_dict <- c(
  "ungeboren", "ungeborenes leben", "menschliches leben", "lebensschutz", 
  "lebensrecht", "lebensschützer", "schützen des lebens", "lebenswert",
  "lebensinteresse", "kindesleben", "kindesmord", "kindestötung",
  "tötung ungeboren", "tötung verhindern", "schwangerschaftsabbruch verhindern",
  "abtreibung verbieten", "strafbarkeit", "rechtswidrigkeit",
  "lebensbeginn", "schwangerschaft schützen", "embryonenschutz", "fötus schützen",
  "unschuldig", "unschuldiges kind", "wehrlos", "schutzlos", "traditionell"
)

# D. Pro-Choice Framing (Autonomy & rights)
pro_choice_dict <- c(
  "fötus", "embryo", "abbruch", "medizinischer eingriff",
  "eingriff", "versorgung", "gesundheitsversorgung", "medizinisch", "patientin",
  "selbstbestimmung", "selbstbestimmungsrecht", "körperliche autonomie", "autonomie",
  "freiheit", "reproduktive freiheit", "wahlfreiheit", "frauenrechte", "menschenrechte",
  "entstigmatisierung", "stigma abbauen", "stigmatisieren", "liberalisierung",
  "legal", "legale zugang", "reproduktive gesundheit", "reproduktive gerechtigkeit",
  "versorgungssicherheit", "zugang gewährleisten", "recht auf abbruch",
  "entscheidungsfreiheit", "wahlrecht", "freie entscheidung", "reproduktive rechte"
)

# Combine into quanteda dictionary
dictionary_frames <- dictionary(list(
  pro_choice = pro_choice_dict,
  pro_life = pro_life_dict,
  moral_emotive = moral_emotive_dict,
  paternalistic = paternalistic_dict
))

# ================================================================
# 3. Lemmatize the Corpus with UDPipe
# ================================================================



# Download and load the German UDPipe model
ud_model <- udpipe_download_model(language = "german", model_dir = "models/")
ud_model <- udpipe_load_model(file = ud_model$file_model)

# Annotate speeches using UDPipe (Lemmatization + POS tagging)
anno <- udpipe_annotate(
  ud_model, 
  x = df_218_dict$Text_Corpus,
  doc_id = df_218_dict$Speech_ID
)
anno_df <- as.data.frame(anno)

# Collapse all lemmatized tokens back into a single string per document
lemma_texts <- anno_df %>%
  group_by(doc_id) %>%
  summarise(text_lemma = paste(lemma, collapse = " "), .groups = "drop")

# Merge back party info
lemma_texts <- lemma_texts %>%
  left_join(df_rede_218 %>% select(Speech_ID, Party), by = c("doc_id" = "Speech_ID"))

# ================================================================
# 4. Create Quanteda Corpus & Apply Dictionary
# ================================================================

# Create quanteda corpus
corp <- corpus(lemma_texts, text_field = "text_lemma")
docvars(corp, "Party") <- lemma_texts$Party

# Tokenize lemmatized text
tokens_lemma <- quanteda::tokens(corp)

# Create DFM and apply dictionary
dfm_dict <- dfm(tokens_lemma) %>%
  dfm_lookup(dictionary = dictionary_frames)

# Group DFM by party
dfm_grouped <- dfm_group(dfm_dict, groups = docvars(dfm_dict, "Party"))

# ================================================================
# 5. Visualization: Absolute and Relative Frequencies
# ================================================================
# Define your color palette
frame_colors <- c(
  "pro_choice" = "#F4A300",      # amber
  "pro_life" = "#046c7c",        # blue-gray
  "moral_emotive" = "#C14000",   # rust
  "paternalistic" = "#8C564B"    # optional 4th tone (dark brown)
)

# A. Absolute Frequencies
df_abs <- convert(dfm_grouped, to = "data.frame") %>%
  pivot_longer(cols = -doc_id, names_to = "Frame", values_to = "Count") %>%
  rename(Party = doc_id)

ggplot(df_abs, aes(x = reorder(Party, -Count), y = Count, fill = Frame)) +
  geom_col(position = "dodge", color = "white", width = 0.7) +
  scale_fill_manual(values = frame_colors) +
  labs(
    title = "Framing by Party (Absolute)",
    x = "Party", y = "Count", fill = "Framing Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "right"
  )

ggsave("DictAnalysisAbsolute.png")

# B. Relative Frequencies
df_rel <- df_abs %>%
  group_by(Party) %>%
  mutate(Relative = Count / sum(Count)) %>%
  ungroup()

ggplot(df_rel, aes(x = reorder(Party, -Relative), y = Relative, fill = Frame)) +
  geom_col(position = "dodge", color = "white", width = 0.7) +
  scale_fill_manual(values = frame_colors) +
  labs(
    title = "Framing by Party (Relative)",
    x = "Party", y = "Proportion", fill = "Framing Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "right"
  )


ggsave("DictAnalysisRel.png")





# Plot with custom colors (for df_rel grouped by topic etc.)

#ggplot(df_rel, aes(x = Party, y = Relative, fill = Frame)) +
#  geom_col(position = "fill", color = "white", width = 0.7) +
#  scale_fill_manual(values = frame_colors) +
#  labs(
#    title = "Framing by Party (Relative)",
#    y = "Proportion", x = "Party", fill = "Framing Category"
#  ) +
#  theme_minimal(base_size = 14) +
#  theme(
#    plot.title = element_text(face = "bold"),
#   axis.text.x = element_text(angle = 30, hjust = 1),
#    legend.position = "right"
#  )


# ggsave("DictAnalysisRel1.png")





## Table of Framing and the Terms

# Step 1: Construct the data frame
framing_table <- data.frame(
  Frame = c("Pro-Choice", "Pro-Life", "Moral-Emotive", "Paternalistic"),
  Terms = c(
    paste(pro_choice_dict, collapse = ", "),
    paste(pro_life_dict, collapse = ", "),
    paste(moral_emotive_dict, collapse = ", "),
    paste(paternalistic_dict, collapse = ", ")
  )
)

# Step 2: Save as a clean standalone HTML file
framing_table %>%
  kable("html", caption = "Framing Categories and Lexical Terms") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE, font_size = 12) %>%
  save_kable("framing_table_clean.html", self_contained = TRUE)


pagedown::chrome_print("framing_table_clean.html", output = "framing_table.pdf")
