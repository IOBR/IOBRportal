**Signature-Gene Workflow**

This workflow supports **signature calculation** and **correlation analysis between signatures and genes**.

**🔄 Recommended Order**
1. Counts to TPM
2. Detect Outliers
3. Calculate Signatures
4. Batch Correlation / Single Correlation / Correlation Matrix

**📌 Usage Notes**
- **Complete Part 1 first** (**Counts → TPM → optional outlier detection → signature calculation**).
- Correlation analysis depends on the processed TPM matrix and calculated signature scores.
- Most modules support **parameter tuning** and **plot size adjustment**.