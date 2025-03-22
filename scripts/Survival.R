# Load necessary libraries
library(TCGAbiolinks)
library(DESeq2)
library(survival)
library(survminer)
library(tidyverse)
library(org.Hs.eg.db)
library(ggtext)
library(rvest)

setwd('~/Library/CloudStorage/Box-Box/Wu Lab/Project - statin/8. RNA-seq/Elizabeth/LW15 analysis/LW15-Functional-Screening')

html_content <- read_html('variance_genes.html')
# Extract the table directly
table <- html_content %>% html_table(fill = TRUE)
genes <- table[[1]]
genes <- genes[,c(1,5)] # keep gene name & ID 
# genes <- genes[1:320,] # remove unnecessary rows
names(genes) <- c('gene_name', 'gene_id')

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
write.csv(genes, 'gene_to_human_mapping_variance.csv')

# Set up working directory
setwd('~/Library/CloudStorage/Box-Box/Wu Lab/Project - statin/8. RNA-seq/Elizabeth/LW15 analysis/LW15-Functional-Screening/TCGA') 

# Build query for TCGA-KIRC RNA-Seq data
kirc_query <- GDCquery(project = 'TCGA-KIRC',
                       data.category = 'Transcriptome Profiling',
                       access = 'open',
                       experimental.strategy = 'RNA-Seq',
                       sample.type = c('Primary Tumor','Solid Tissue Normal'))

# Download and prepare the data
GDCdownload(kirc_query)
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
# genes2 <- genes[-(1:121), ]

# ********************************************************************************

# Query clinical data once outside the loop
# Load the individual TSV files (adjust file paths as needed)
setwd('/Users/elizabeth\ 1/Downloads/clinical.project-tcga-kirc.2025-03-21') # downloaded folder for TCGA-KIRC clinical data from GDC portal online
clinical     <- read.delim("clinical.tsv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
follow_up    <- read.delim("follow_up.tsv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
pathology    <- read.delim("pathology_detail.tsv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
exposure     <- read.delim("exposure.tsv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
family_hist  <- read.delim("family_history.tsv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Inspect the core clinical file
dim(clinical)
head(clinical)


# Convert the join column in follow_up (and others if needed) to character
follow_up <- follow_up %>%
  mutate(cases.submitter_id = as.character(cases.submitter_id))
pathology <- pathology %>%
  mutate(cases.submitter_id = as.character(cases.submitter_id))
exposure <- exposure %>%
  mutate(cases.submitter_id = as.character(cases.submitter_id))
family_hist <- family_hist %>%
  mutate(cases.submitter_id = as.character(cases.submitter_id))

# Now join using "cases.submitter_id" as the common key
clinical_full <- clinical %>%
  left_join(follow_up, by = "cases.submitter_id", relationship = "many-to-many") %>%
  left_join(pathology, by = "cases.submitter_id", relationship = "many-to-many") %>%
  left_join(exposure, by = "cases.submitter_id", relationship = "many-to-many") %>%
  left_join(family_hist, by = "cases.submitter_id", relationship = "many-to-many")



# Check the merged data frame
dim(clinical_full)
head(clinical_full)

clinical_data <- clinical_full

clinical_data$deceased <- ifelse(clinical_data$`demographic.vital_status` == "Alive", FALSE, TRUE)
clinical_data$overall_survival <- ifelse(
  clinical_data$`demographic.vital_status` == "Alive",
  clinical_data$`diagnoses.days_to_last_follow_up`,  # adjust this column as needed
  clinical_data$`demographic.days_to_death`
)

# Debug: print clinical_data dimensions and first few rows
print(dim(clinical_data))
print(head(clinical_data))

# Change working directory for saving survival curves
setwd('~/Library/CloudStorage/Box-Box/Wu Lab/Project - statin/8. RNA-seq/Elizabeth/LW15 analysis/LW15-Functional-Screening/TCGA/survival_curves')

# Loop over each gene
for (i in seq_along(genes$human_symbols)) {
  ensembl_id <- genes$human_symbols[i]
  gene_name <- genes$gene_name[i]
  
  # Proceed only if ensembl_id is not 'NA'
  if (ensembl_id != 'NA') {
    # Find the row(s) in normalized_counts that match the gene
    gene_row <- which(base_ensembl_ids == ensembl_id)
    if (length(gene_row) != 1) next  # skip if ambiguous or not found
    
    # Extract gene expression for this gene
    gene_expression <- normalized_counts[gene_row, ]
    gene_expression <- as.numeric(gene_expression)
    
    # Create a data frame with sample identifiers and gene expression values
    gene_expression_df <- data.frame(
      sample = colnames(dds),
      expression = gene_expression,
      stringsAsFactors = FALSE
    )
    
    # Filter tumor samples using barcode pattern (assumes tumor samples contain '01A')
    tumor_indices <- grep('01A.*', gene_expression_df$sample)
    tumor_goi <- gene_expression_df[tumor_indices, , drop = FALSE]
    if (nrow(tumor_goi) == 0) next  # skip if no tumor samples
    
    # Calculate the median expression for the gene and assign strata
    median_goi <- median(tumor_goi$expression, na.rm = TRUE)
    tumor_goi$strata <- ifelse(tumor_goi$expression >= median_goi, 'High', 'Low')
    print(table(tumor_goi$strata))
    
    # Create a 'submitter_id' by trimming extra barcode info from the sample identifier
    tumor_goi$submitter_id <- gsub('-01A.*', '', tumor_goi$sample)
    
    # Debug: check a few rows of tumor_goi and clinical_data
    print(head(tumor_goi))
    
    # Merge gene expression data with clinical data (by patient barcode)
    merged_data <- inner_join(tumor_goi, clinical_data,
                              by = c("submitter_id" = "cases.submitter_id"))
    
    if (nrow(merged_data) == 0) next  # skip if merge yields no rows
    
    # Reassign expression groups based on the median in merged_data
    merged_data$expression_group <- ifelse(
      merged_data$expression >= median(merged_data$expression, na.rm = TRUE),
      'High', 'Low'
    )
    
    # Perform survival analysis
    surv_obj <- Surv(time = merged_data$overall_survival, event = merged_data$deceased)
    fit <- survfit(surv_obj ~ expression_group, data = merged_data)
    
    # Plot survival curves
    surv_plot <- ggsurvplot(
      fit,
      data = merged_data,
      pval = TRUE,
      risk.table = FALSE,
      palette = c("#00BA38", "#F8766D"),
      ggtheme = theme_minimal(),
      title = paste("Survival Analysis for", gene_name, "Expression in TCGA-KIRC")
    )
    
    # Create a filename based on the gene name
    file_name <- paste0("survival_curve_", gene_name, ".png")
    
    # Save the survival plot
    ggsave(file_name, plot = surv_plot$plot, width = 7, height = 6)
  } else {
    next
  }
}
print(genes_not_found)

# note: there should be 284 plots in the end bc 320 - 36 missing from human

# *********************************************************************************
# calculate pval separately & categorize survival curves into significant & 
# insignificant & organize figures into folders 
# Initialize a dataframe to store gene names and p-values
# Initialize a dataframe to store gene names and p-values
p_values_df <- data.frame(gene_name = character(), p_value = numeric(), 
                          stringsAsFactors = FALSE)

# Loop over each gene
for (i in seq_along(genes$human_symbols)) {
  ensembl_id <- genes$human_symbols[i]
  gene_name  <- genes$gene_name[i]
  
  # Proceed only if ensembl_id is not "NA"
  if (ensembl_id != "NA") {
    # Find the row(s) in normalized_counts that match the gene
    gene_row <- which(base_ensembl_ids == ensembl_id)
    if (length(gene_row) != 1) next  # Skip if ambiguous or not found
    
    # Extract gene expression and ensure it is numeric
    gene_expression <- as.numeric(normalized_counts[gene_row, ])
    
    # Create a data frame with sample identifiers and gene expression values
    gene_expression_df <- data.frame(
      sample     = colnames(dds),
      expression = gene_expression,
      stringsAsFactors = FALSE
    )
    
    # Filter tumor samples using barcode pattern (assumes tumor samples contain "01A")
    tumor_indices <- grep("01A.*", gene_expression_df$sample)
    tumor_goi <- gene_expression_df[tumor_indices, , drop = FALSE]
    if (nrow(tumor_goi) == 0) next  # Skip if no tumor samples
    
    # Calculate the median expression for the gene and assign strata
    median_goi <- median(tumor_goi$expression, na.rm = TRUE)
    tumor_goi$strata <- ifelse(tumor_goi$expression >= median_goi, "High", "Low")
    print(table(tumor_goi$strata))
    
    # Create a "submitter_id" by trimming extra barcode info from the sample identifier
    tumor_goi$submitter_id <- gsub("-01A.*", "", tumor_goi$sample)
    
    # Merge gene expression data with clinical data using "cases.submitter_id" as the key.
    merged_data <- inner_join(
      tumor_goi,
      clinical_data,
      by = c("submitter_id" = "cases.submitter_id"),
      relationship = "many-to-many"
    )
    if (nrow(merged_data) == 0) next  # Skip if merge yields no rows
    
    # If overall_survival is not present, try to compute it from available columns
    if (!"overall_survival" %in% names(merged_data)) {
      if ("demographic.vital_status" %in% names(merged_data) &&
          "demographic.days_to_death" %in% names(merged_data) &&
          "diagnoses.days_to_last_follow_up" %in% names(merged_data)) {
        merged_data$overall_survival <- ifelse(
          merged_data$`demographic.vital_status` == "Alive",
          merged_data$`diagnoses.days_to_last_follow_up`,
          merged_data$`demographic.days_to_death`
        )
      } else {
        warning("No overall_survival info available for gene ", gene_name)
        next
      }
    }
    
    # Convert overall_survival to numeric
    merged_data$overall_survival <- suppressWarnings(as.numeric(as.character(merged_data$overall_survival)))
    if (all(is.na(merged_data$overall_survival))) {
      warning("Overall survival values are all NA for gene ", gene_name)
      next
    }
    
    # Reassign expression groups based on the median expression in merged_data
    merged_data$expression_group <- ifelse(
      merged_data$expression >= median(merged_data$expression, na.rm = TRUE),
      "High", "Low"
    )
    
    # Perform survival analysis
    surv_obj <- Surv(time = merged_data$overall_survival, event = merged_data$deceased)
    fit <- survfit(surv_obj ~ expression_group, data = merged_data)
    
    # Plot survival curves using ggsurvplot
    surv_plot <- ggsurvplot(
      fit,
      data = merged_data,
      pval = TRUE,
      risk.table = FALSE,
      palette = c("#00BA38", "#F8766D"),
      ggtheme = theme_minimal(),
      title = paste("Survival Analysis for", gene_name, "Expression in TCGA-KIRC")
    )
    
    # Use surv_pvalue() to extract the p-value from the survival analysis
    pval_df <- surv_pvalue(fit, data = merged_data)
    if(nrow(pval_df) > 0) {
      p_val <- as.numeric(pval_df$pval[1])
    } else {
      p_val <- NA
    }
    
    print(paste('p-val:', p_val))
    
    # Append gene name and p-value to the dataframe
    p_values_df <- rbind(p_values_df, 
                         data.frame(gene_name = gene_name, p_value = p_val, stringsAsFactors = FALSE))
    
    # Create a filename based on the gene name and save the survival plot
    file_name <- paste0("survival_curve_", gene_name, ".png")
    ggsave(file_name, plot = surv_plot$plot, width = 7, height = 6)
    
  } else {
    next
  }
}

print(genes_not_found)
write.csv(p_values_df, "~/Library/CloudStorage/Box-Box/Wu Lab/Project - statin/8. RNA-seq/Elizabeth/LW15 analysis/LW15-Functional-Screening/TCGA/variance_gene_p_vals.csv", row.names = FALSE)


# Move survival curve plots based on p-value significance******************************************************
for (i in 1:nrow(p_values_df)) {
  file_name <- paste0("survival_curve_", p_values_df$gene_name[i], ".png")
  target_subdir <- ifelse(p_values_df$p_value[i] <= 0.05, "significant", "non-significant")
  
  # Move the file
  file_path <- file_name 
  target_path <- file.path(target_subdir, file_name)
  file.rename(file_path, target_path)
}



