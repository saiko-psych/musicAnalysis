# musicAnalysis R Package - Project Overview

## Purpose
The musicAnalysis package is a specialized R toolkit for processing music psychology research data, focusing on automated data extraction and preparation from KLAWA PDFs, PPPT, AAT, and musical experience questionnaires.

## Core Components

### 1. KLAWA Module
- **Purpose**: Extract experimental data from PDF reports
- **Input**: Hierarchical folder structure with PDF files
- **Output**: Tidy tibble with participant codes and measurements
- **Key measurements**:
  - Volume difference
  - Pitch
  - Onset difference
  - Pitch duration difference

### 2. Musical Experience Module
- **Purpose**: Process LimeSurvey musical background data
- **Components**:
  - Time-based practice data (instruments, singing, other music)
  - Profile data (musical status, early exposure, preferences)
- **Output**: Wide format with aggregated practice hours and profile information

### 3. PPPT Module (To be implemented)
- **Purpose**: Process PPPT data

### 4. AAT Module (To be implemented)
- **Purpose**: Process AAT data

### 5. Data Integration
- **merge_by_code()**: Safe left-join by participant code

## Technical Architecture

### Design Principles
- Robust parsing (handles variations in formats)
- Configurable labels via options system
- Defensive programming with validation
- Tidy data output (tibbles)
- Comprehensive error handling

### Key Dependencies
- tidyverse ecosystem (dplyr, tidyr, stringr)
- pdftools for PDF extraction
- fs for file system operations
- readr for CSV reading

## Usage Workflow
```r
# 1. Extract KLAWA data
klawa_data <- klawa_scan("data/KLAWA")

# 2. Process musical experience
music_exp <- musical_experience("data/musical_experience.csv")

# 3. Merge datasets
final_data <- merge_by_code(klawa_data, music_exp$wide)
```