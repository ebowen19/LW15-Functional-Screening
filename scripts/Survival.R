# Load necessary libraries
library(TCGAbiolinks)
library(DESeq2)
library(survival)
library(survminer)
library(tidyverse)
library(org.Hs.eg.db)
library(ggtext)

setwd('~/Library/CloudStorage/Box-Box/Wu Lab/Project - 2024/Functional screening/Elizabeth')

html_content <- read_html('EB_functional_screening.html')
# Extract the table directly
table <- html_content %>% html_table(fill = TRUE)
genes <- table[[1]]
genes <- genes[,c(1,5)] # keep gene name & ID 
genes <- genes[1:320,] # remove unnecessary rows
genes$gene_name <- toupper(genes$gene_name)

human_symbols <- c(0)
missing_from_human <- c(0)

for (gene in genes$gene_name) {
  # Attempt to query the ENSEMBL ID for the current gene symbol
  gene_id <- tryCatch({
    select(org.Hs.eg.db, keys = gene, columns = "ENSEMBL", keytype = "SYMBOL")
  }, error = function(e) {
    NULL # Return NULL if an error occurs (e.g., gene symbol not found)
  })
  
  # Check if the query was successful and if any ENSEMBL IDs were found
  if (!is.null(gene_id) && nrow(gene_id) > 0) {
    # Assuming we want the first ENSEMBL ID if there are multiple
    ensembl_id <- gene_id$ENSEMBL[1]
  } else {
    # If no conversion was found, print a message with the gene symbol
    cat("No conversion found for gene:", gene, "\n")
    missing_from_human <- c(missing_from_human, gene)
    ensembl_id <- 'NA'
  }
  # Concatenate this ENSEMBL ID to the human_symbols vector
  human_symbols <- c(human_symbols, ensembl_id)
}

human_symbols <- human_symbols[-1]
genes$human_symbols <- human_symbols
write.csv(genes, 'gene_to_human_mapping.csv')

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

setwd('survival_curves')

# Initialize an empty vector to store ENSEMBL IDs of genes not found
genes_not_found <- character(0)

# Modify the ENSEMBL IDs in dds to remove the version numbers
base_ensembl_ids <- gsub("\\..*$", "", rownames(dds))

# take away the genes we've already completed:
genes2 <- genes[-(1:121), ]

# ********************************************************************************

# Assuming the first ENSEMBL ID returned is the correct one
for (i in seq_along(genes2$human_symbols)) {
  ensembl_id <- genes2$human_symbols[i]
  gene_name <- genes2$gene_name[i]
  if (ensembl_id  != 'NA') {
    i <- i+1
    
    # Match the gene symbol to modified ENSEMBL ID and extract expression data
    gene_row <- which(base_ensembl_ids == ensembl_id)
    
    # Match the gene symbol to modified ENSEMBL ID and extract expression data
    gene_base_id <- ensembl_id 
    
    gene_expression <- normalized_counts[gene_row, ]
    
    # Create a data frame for the gene expression with patient identifiers
    gene_expression_df <- data.frame(sample = colnames(dds), expression = gene_expression)
    
    # Retrieve and prepare clinical data
    clinical_data <- GDCquery_clinic('TCGA-KIRC', type = "clinical")
    clinical_data$deceased <- ifelse(clinical_data$vital_status == 'Alive', FALSE, TRUE)
    clinical_data$overall_survival <- ifelse(clinical_data$deceased == FALSE, clinical_data$days_to_last_follow_up, clinical_data$days_to_death)
    
    # Filter out txn datasets from solid tissue normals
    tumor_indices <- grep('01A.*',gene_expression_df$sample)
    # some patients have normal tissue & cancer tissue, we're interested in the tumor tissues
    # TCGA barcode 01-09 is cancer tissue (i think?)
    
    tumor_goi <- gene_expression_df[tumor_indices,]
    
    # Separate our tumor group into 2 based on the median txn level of goi
    median_goi <- median(tumor_goi$expression)
    
    tumor_goi$strata <- ifelse(tumor_goi$expression >= median_goi,'High','Low')
    
    table(tumor_goi$strata)
    
    # Merge clinical data
    tumor_goi$submitter_id <- gsub('-01A.*','',tumor_goi$sample) # trim off additional barcodes after patient id
    
    
    # Merge gene expression data with clinical data
    merged_data <- inner_join(tumor_goi, clinical_data, by = c("submitter_id" = "bcr_patient_barcode"))
    
    # Determine high and low expression based on the median
    merged_data$expression_group <- ifelse(merged_data$expression >= median(merged_data$expression, na.rm = TRUE), 'High', 'Low')
    
    # Perform survival analysis
    surv_obj <- Surv(time = merged_data$overall_survival, event = merged_data$deceased)
    fit <- survfit(surv_obj ~ expression_group, data = merged_data)
    
    # Plot survival curves
    surv_plot <- ggsurvplot(fit,
                            data = merged_data,
                            pval = TRUE,
                            risk.table = F,
                            palette = c("#00BA38", "#F8766D"),
                            ggtheme = theme_minimal(),
                            title = paste("Survival Analysis for", gene_name, "Expression in TCGA-KIRC"))
    
    # Dynamically generate the file name based on the ENSEMBL ID
    file_name <- paste0("survival_curve_", gene_name, ".png")
    
    # Save the plot
    ggsave(file_name, plot = surv_plot$plot, width = 7, height = 6)
  }
  else next
}

print(genes_not_found)

# note: there should be 284 plots in the end bc 320 - 36 missing from human

# *********************************************************************************
# calculate pval separately & categorize survival curves into significant & 
# insignificant & organize figures into folders 
# Initialize a dataframe to store gene names and p-values
p_values_df <- data.frame(gene_name = character(), p_value = numeric(), stringsAsFactors = FALSE)

for (i in seq_along(genes$human_symbols)) {
  ensembl_id <- genes$human_symbols[i]
  gene_name <- genes$gene_name[i]
  print(paste(i,gene_name))
  if (ensembl_id != 'NA') {
    gene_row <- which(base_ensembl_ids == ensembl_id)
    if (length(gene_row) > 0) {
      gene_expression <- normalized_counts[gene_row, ]
      gene_expression_df <- data.frame(sample = colnames(dds), expression = gene_expression)
      
      clinical_data <- GDCquery_clinic('TCGA-KIRC', type = "clinical")
      clinical_data$deceased <- ifelse(clinical_data$vital_status == 'Alive', FALSE, TRUE)
      clinical_data$overall_survival <- ifelse(clinical_data$deceased == FALSE, clinical_data$days_to_last_follow_up, clinical_data$days_to_death)
      
      tumor_indices <- grep('01A.*',gene_expression_df$sample)
      tumor_goi <- gene_expression_df[tumor_indices,]
      median_goi <- median(tumor_goi$expression)
      tumor_goi$strata <- ifelse(tumor_goi$expression >= median_goi,'High','Low')
      
      if (length(unique(tumor_goi$strata)) < 2) {
        cat("Skipping gene", gene_name, "- not enough groups for survival analysis.\n")
        next
      }
      
      tumor_goi$submitter_id <- gsub('-01A.*','',tumor_goi$sample)
      merged_data <- inner_join(tumor_goi, clinical_data, by = c("submitter_id" = "bcr_patient_barcode"))
      merged_data$expression_group <- ifelse(merged_data$expression >= median(merged_data$expression, na.rm = TRUE), 'High', 'Low')
      
      surv_obj <- Surv(time = merged_data$overall_survival, event = merged_data$deceased)
      fit <- survfit(surv_obj ~ expression_group, data = merged_data)
      surv_diff <- survdiff(surv_obj ~ expression_group, data = merged_data)
      p_value <- 1 - pchisq(surv_diff$chisq, df = 1)
      
      p_values_df <- rbind(p_values_df, data.frame(gene_name = gene_name, p_value = p_value))
    }
  }
}

# double check the p-vals generated with the plots -- looks good 

# Save the p-values dataframe to a CSV file
write.csv(p_values_df, "../TCGA/gene_p_values.csv", row.names = FALSE)

# Move survival curve plots based on p-value significance
for (i in 1:nrow(p_values_df)) {
  file_name <- paste0("survival_curve_", p_values_df$gene_name[i], ".png")
  target_subdir <- ifelse(p_values_df$p_value[i] <= 0.05, "significant", "non-significant")
  
  # Move the file
  file_path <- file_name 
  target_path <- file.path(target_subdir, file_name)
  file.rename(file_path, target_path)
}



