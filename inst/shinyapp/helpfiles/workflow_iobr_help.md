## IOBR Workflow (Signature + TME)

This module provides an integrated pipeline for **signature scoring**, **TME deconvolution**, and **downstream analysis** (visualization, survival, correlation, and group comparison).

### Usage Notes
- **Part 1 (Preprocessing & Features) must be completed first** (Counts → TPM → optional outlier detection → feature calculation → optional clustering → combine pdata).  
  Downstream tabs rely on the processed outputs generated here.
- After Part 1 is done, **Parts 2–5 can run directly** using the pipeline outputs.  
- Most modules support **parameter tuning** and **plot size adjustment** (e.g., width/height or related options) for better visualization and export.

### Recommended Order
1. Counts to TPM  
2. Detect Outliers
3. Calculate Features (choose: Signature scores / TME deconvolution)  
4. TME Cluster 
5. Combine Pdata  
6. Visualization / Survival / Correlation / Group Comparison