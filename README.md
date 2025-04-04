# Replication Material – QTA Term Paper

This repository contains replication material for the QTA Term Paper.

## Dataset

The dataset files is df_rede_final (in CSV or RDS format). It was constructed with the ScriptXML_to_Dataset.R. So this does not need to be run again.
The plenary debate protocolls, on which this dataset is based on, are **not included in this repository** due to file size limitations (49 MB). It can be downloaded via Dropbox: https://www.dropbox.com/scl/fo/qlx2g4f126ogqzm1g5naz/AAaAVTWtQg4FaqkZhPjC3M0?rlkey=r01nk9lg77oy9xs1evrw3ru5k&st=zbb5le23&dl=0 

## Getting Started

After downloading all materials and setting the correct **working directory**, you can begin the replication process.

The R scripts are **numbered and should be executed in order** (e.g., `Script1...` etc.). They are written in a modular way and will load all necessary files from the working directory automatically.

## Script Structure

The analysis is split across several R scripts. This structure was chosen because combining everything into one script caused performance issues in RStudio, especially when using virtual Python environments and loading many packages. Running the scripts sequentially avoids these problems.

- Each script runs independently and loads its own data.
- The **Python script** for web scraping is included for informational purposes only. It does **not need to be run**, as the final dataset (scraped `.xml` files) is already provided.

## KWIC Table

The file `kwic_table_abtreibung.pdf` is provided for informational purposes. It displays the keyword-in-context (KWIC) results for the term *Abtreibung* across all parties. This table illustrates how different political actors use the term rhetorically in parliamentary speeches. Due to its length, it was not included directly in the paper's appendix but is available here for reference.
