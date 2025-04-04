# Replication Material – QTA Term Paper

This repository contains replication material for the QTA Term Paper.

## Dataset

The dataset is **not included in this repository** due to file size limitations (49 MB). It was sent separately as a `.zip` file alongside the paper. Please download and extract the dataset manually.

## Getting Started

After downloading all materials and setting the correct **working directory**, you can begin the replication process.

The R scripts are **numbered and should be executed in order** (e.g., `Script1...` etc.). They are written in a modular way and will load all necessary files from the working directory automatically.

## Script Structure

The analysis is split across several R scripts. This structure was chosen because combining everything into one script caused performance issues in RStudio, especially when using virtual Python environments and loading many packages. Running the scripts sequentially avoids these problems.

- Each script runs independently and loads its own data.
- The **Python script** for web scraping is included for informational purposes only. It does **not need to be run**, as the final dataset (scraped `.xml` files) is already provided.

## Included Scripts

Each script is fully commented and self-contained.
