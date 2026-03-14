## TCGA Cohorts Workflow

This module provides a **database-driven workflow** for **TCGA cohort analysis** using cloud/DuckDB data, including **signature scoring / TME deconvolution results** and **downstream analysis** (visualization, survival, correlation, and group comparison).

### Usage Notes
- **Part 1 (Data Preparation) must be completed first** (**Data Selection → TME Cluster**).  
  Downstream tabs rely on the prepared cohort data and clustering results generated here.
- This workflow is designed for **TCGA cloud/database data** (DuckDB-backed), so downstream modules mainly use the selected dataset from Part 1.
- Most modules support **parameter tuning** and **plot size adjustment** (e.g., width/height or related options) for better visualization and export.

### Recommended Order
1. Data Selection (choose TCGA cohort and data type)
2. TME Cluster
3. Visualization / Survival / Correlation / Group Comparison

### Tips
- **Cell Bar Plot** is mainly available when the selected data type is **TME deconvolution** output.
- If a downstream tab shows missing-data prompts, check whether **Data Selection** and **TME Cluster** have been completed successfully.