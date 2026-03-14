## Signature-Gene Workflow

This module provides an integrated workflow to **calculate signature scores** and **explore correlations between signatures and genes**.

### Usage Notes
- **Part 1 (Preprocessing & Features) must be completed first** (Counts → TPM → optional outlier detection → signature calculation).  
  The correlation analyses in Part 2 rely on the processed TPM matrix and calculated signature scores.
- In this workflow, the system builds a combined matrix (**samples × [signatures + genes]**) for downstream correlation analysis.
- Most modules support **parameter tuning** and **plot size adjustment** (e.g., width/height or related options) for better visualization and export.

### Recommended Order
1. Counts to TPM  
2. Detect Outliers  
3. Calculate Signatures  
4. Batch Correlation / Single Correlation / Correlation Matrix

### Tips
- This workflow is intended for **signature–gene relationship analysis** (not TME deconvolution).
- If a correlation tab shows missing-data prompts, check whether **TPM conversion** and **signature calculation** have been completed successfully.