library(XML)
library(tidyverse)
library(readxl)

setwd("/Users/laratuermer/Documents/Studies/Master/WiSe 2425/Quantitative Textanalysis/TermpaperQTA/Sources/bundestag_xml_20wp")

# Step 1: Load the CSV file containing topics and corresponding XML files
df_long <- read_csv("Themen_XML_Liste.csv")  # Adjust file name if necessary

# Step 2: Directly filter for a specific topic while selecting XML files
xml_files <- df_long %>%
  filter(Thema == "stgb 218") %>%  # Change the topic here
  pull(XML_File)

# Step 3: Function to process a single XML file
process_xml_file <- function(xml_file, topic) {
  if (!file.exists(xml_file)) {
    message(paste("File not found:", xml_file))
    return(NULL)
  }
  
  xml_data <- xmlParse(xml_file)
  root_node <- xmlRoot(xml_data)
  
  # Extract session date
  session_date <- xmlGetAttr(root_node, "sitzung-datum", default = NA)
  
  # Extract agenda items and titles
  agenda_nodes <- getNodeSet(root_node, "//tagesordnungspunkt")
  agenda_dict <- list()
  
  for (agenda in agenda_nodes) {
    agenda_id <- xmlGetAttr(agenda, "top-id", default = "Unknown")
    agenda_title_node <- getNodeSet(agenda, ".//p[@klasse='T_fett']")
    agenda_title <- ifelse(length(agenda_title_node) > 0, xmlValue(agenda_title_node[[1]]), NA)
    agenda_dict[[agenda_id]] <- agenda_title
  }
  
  # Map speeches to agenda items
  speech_agenda_dict <- list()
  for (agenda in agenda_nodes) {
    agenda_id <- xmlGetAttr(agenda, "top-id", default = "Unknown")
    speech_nodes <- getNodeSet(agenda, ".//rede")
    
    for (speech in speech_nodes) {
      speech_id <- xmlGetAttr(speech, "id", default = "Unknown")
      speech_agenda_dict[[speech_id]] <- agenda_id
    }
  }
  
  # Extract all speech nodes
  speech_nodes <- getNodeSet(root_node, "//rede")
  data_list <- list()
  
  for (i in seq_along(speech_nodes)) {
    record <- speech_nodes[[i]]
    
    speech_id <- xmlGetAttr(record, "id", default = NA)
    agenda_item <- ifelse(!is.null(speech_agenda_dict[[speech_id]]), speech_agenda_dict[[speech_id]], NA)
    
    speaker_node <- getNodeSet(record, ".//redner")[[1]]
    
    if (!is.null(speaker_node)) {
      speaker_id <- xmlGetAttr(speaker_node, "id", default = NA)
      first_name <- ifelse(length(getNodeSet(speaker_node, ".//vorname")) > 0, xmlValue(getNodeSet(speaker_node, ".//vorname")[[1]]), NA)
      last_name <- ifelse(length(getNodeSet(speaker_node, ".//nachname")) > 0, xmlValue(getNodeSet(speaker_node, ".//nachname")[[1]]), NA)
      party <- ifelse(length(getNodeSet(speaker_node, ".//fraktion")) > 0, xmlValue(getNodeSet(speaker_node, ".//fraktion")[[1]]), NA)
      role <- ifelse(length(getNodeSet(speaker_node, ".//rolle/rolle_lang")) > 0, xmlValue(getNodeSet(speaker_node, ".//rolle/rolle_lang")[[1]]), NA)
    } else {
      speaker_id <- NA
      first_name <- NA
      last_name <- NA
      party <- NA
      role <- NA
    }
    
    # Extract comments
    comment_nodes <- getNodeSet(record, ".//kommentar")
    comments <- c()
    for (c in comment_nodes) {
      comment_text <- xmlValue(c)
      if (!is.na(comment_text) && nchar(comment_text) > 0) {
        comments <- c(comments, comment_text)
      }
    }
    
    collected_comments <- ifelse(length(comments) > 0, paste(comments, collapse = " // "), NA)
    
    # Extract speech text
    p_nodes <- getNodeSet(record, ".//p[@klasse]")
    text_corpus <- c()
    for (p in p_nodes) {
      p_class <- xmlGetAttr(p, "klasse", default = "Unknown")
      p_text <- xmlValue(p)
      if (nchar(p_text) > 0) {
        text_corpus <- c(text_corpus, paste0("[", p_class, "] ", p_text))
      }
    }
    
    cleaned_text_corpus <- text_corpus[!grepl("\\[redner\\]", text_corpus)]
    
    # Store data
    data_list[[i]] <- list(
      Topic = topic,  
      XML_File = xml_file,  
      Speech_ID = speech_id,
      Agenda_Item = agenda_item,
      Speaker_ID = speaker_id,
      First_Name = first_name,
      Last_Name = last_name,
      Party = party,
      Role = role,
      Comments = collected_comments,
      Text_Corpus = paste(cleaned_text_corpus, collapse = " "),
      Session_Date = session_date
    )
  }
  
  return(bind_rows(data_list))
}

# Step 4: Process all XML files for the selected topic
df_rede_final <- map_dfr(xml_files, ~ process_xml_file(.x, "stgb 218"))


# Step 6: Print the final dataset
print(df_rede_final)




saveRDS(df_rede_final, file = "df_rede_final.rds")



