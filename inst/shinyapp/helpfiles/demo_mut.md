**Data Preview (Mutation Matrix: Genes × Samples)**

| **rowname** | **TCGA-3M-AB46** | **...** | **TCGA-B7-5816** |
| :--- | :---: | :---: | :---: |
| **A1BG** | 1 | ... | 0 |
| **A1CF** | 1 | ... | 0 |
| **A2M** | 0 | ... | 1 |

**Description:**  
This dataset is a **mutation matrix** with **genes as rows** and **samples as columns**.  
Values are typically **binary mutation indicators** (e.g., 1 = mutated, 0 = not mutated).

*Notes:*  
* *Supported formats: Excel (.xlsx), CSV, TXT, or TSV*  
* *The first column should be named **rowname** and contain gene symbols*  
* *Sample IDs should be used as column names (e.g., TCGA barcodes)*  
* *Please retain the rowname header in the first column to avoid column shift during import*  