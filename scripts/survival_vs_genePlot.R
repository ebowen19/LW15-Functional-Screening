# Load necessary libraries
library(TCGAbiolinks)
library(DESeq2)
library(survival)
library(survminer)
library(tidyverse)
library(org.Hs.eg.db)
library(rvest)

setwd('~/Library/CloudStorage/Box-Box/Wu Lab/Project - 2024/Functional screening/Elizabeth')

html_content <- read_html('EB_functional_screening.html')

# Extract the table directly
genes <- read.csv('gene_to_human_mapping.csv')


# Set up working directory
setwd('~/Library/CloudStorage/Box-Box/Wu Lab/Project - statin/8. RNA-seq/Elizabeth/TCGA') 

# Build query for TCGA-KIRC RNA-Seq data
kirc_query <- GDCquery(project = 'TCGA-KIRC',
                       data.category = 'Transcriptome Profiling',
                       access = 'open',
                       experimental.strategy = 'RNA-Seq',
                       sample.type = c('Primary Tumor','Solid Tissue Normal'))

# Download and prepare the data
# GDCdownload(kirc_query)
kirc_data <- GDCprepare(kirc_query)

# Create a DESeq2 dataset object
dds <- DESeqDataSetFromMatrix(countData = assay(kirc_data),
                              colData = colData(kirc_data),
                              design = ~ sample_type)

# Perform normalization
dds <- estimateSizeFactors(dds)
normalized_counts <- counts(dds, normalized = TRUE)

# Modify the ENSEMBL IDs in dds to remove the version numbers
base_ensembl_ids <- gsub("\\..*$", "", rownames(dds))

# ********************************************************************************
setwd('~/Library/CloudStorage/Box-Box/Wu Lab/Project - 2024/Functional screening/Elizabeth/LW15-Functional-Screening/TCGA')
p_vals <- read.csv('gene_p_values.csv')

data <- inner_join(p_vals, genes, by = "gene_name")
data <- data[-c(3,4)]

# Initialize variables to track the process
processed_count <- 0
skipped_due_to_id <- 0
skipped_due_to_row <- 0
skipped_due_to_no_events <- 0

# Initialize an empty dataframe to store results
survival_summary2 <- data.frame(gene_name = character(), 
                               higher_survival_group = character(), 
                               stringsAsFactors = FALSE)

# Loop through each gene
for (i in seq_along(data$gene_name)) {
  gene_name <- data$gene_name[i]
  ensembl_id <- data$human_symbols[i]
  
  # Only proceed if p_value is significant
  if (data$p_value[i] < 0.05) {
    if (!is.na(ensembl_id) && ensembl_id != "NA") {
      
      gene_row <- which(base_ensembl_ids == ensembl_id)
      
      if (length(gene_row) > 0) {
        processed_count <- processed_count + 1
        
        gene_expression <- normalized_counts[gene_row, ]
        gene_expression_df <- data.frame(sample = colnames(dds), expression = gene_expression)
        tumor_indices <- grep('01A', gene_expression_df$sample)
        tumor_goi <- gene_expression_df[tumor_indices, ]
        
        median_expression <- median(tumor_goi$expression)
        tumor_goi$strata <- ifelse(tumor_goi$expression >= median_expression, 'High', 'Low')
        
        clinical_data <- GDCquery_clinic('TCGA-KIRC', type = "clinical")
        clinical_data$deceased <- ifelse(clinical_data$vital_status == 'Alive', FALSE, TRUE)
        clinical_data$overall_survival <- ifelse(clinical_data$deceased == FALSE, clinical_data$days_to_last_follow_up, clinical_data$days_to_death)
        
        tumor_goi$submitter_id <- gsub('-01A.*', '', tumor_goi$sample)
        merged_data <- inner_join(tumor_goi, clinical_data, by = c("submitter_id" = "bcr_patient_barcode"))
        
        surv_obj <- Surv(time = merged_data$overall_survival, event = merged_data$deceased)
        fit <- survfit(surv_obj ~ tumor_goi$strata, data = merged_data)
        
        surv_summary <- summary(fit)
        no_events <- all(is.na(surv_summary$n.event))
        
        if (!no_events) {
          surv_probs <- tapply(surv_summary$surv, surv_summary$strata, tail, n = 1)
          
          if (length(surv_probs) == 2 && !any(is.na(surv_probs))) {
            higher_survival_group <- ifelse(surv_probs[[1]] > surv_probs[[2]], "High", "Low")
          } else {
            higher_survival_group <- "NA"
          }
        } else {
          skipped_due_to_no_events <- skipped_due_to_no_events + 1
          higher_survival_group <- "NA"
        }
        
        survival_summary2 <- rbind(survival_summary2, 
                                  data.frame(gene_name = gene_name, 
                                             higher_survival_group = higher_survival_group))
        print(paste("Processed gene:", gene_name, "| Higher survival group:", higher_survival_group))
      } else {
        skipped_due_to_row <- skipped_due_to_row + 1
        print(paste("Skipped:", gene_name, "- No matching row in DESeq2 dataset"))
      }
    } else {
      skipped_due_to_id <- skipped_due_to_id + 1
      print(paste("Skipped:", gene_name, "- NA or missing ENSEMBL ID"))
    }
  }
}

# Summary of processing
cat("Summary of processing:\n")
cat("Processed genes:", processed_count, "\n")
cat("Skipped due to missing ENSEMBL ID:", skipped_due_to_id, "\n")
cat("Skipped due to no matching row in DESeq2 dataset:", skipped_due_to_row, "\n")
cat("Skipped due to no events:", skipped_due_to_no_events, "\n")

# Display the final summary of results
print(survival_summary)

write.csv(survival_summary2, "survival_summary.csv")


