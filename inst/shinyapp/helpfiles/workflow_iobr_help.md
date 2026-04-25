**IOBR Workflow**

This workflow supports **signature scoring**, **TME deconvolution**, and **downstream analysis** (visualization, survival, correlation, and group comparison).

**🔄 Recommended Order**
1. Counts to TPM
2. Detect Outliers
3. Calculate Features
4. TME Cluster
5. Combine Pdata
6. Visualization / Survival / Correlation / Group Comparison

**📌 Usage Notes**
- **Complete Part 1 first** (**Counts → TPM → optional outlier detection → feature calculation → optional clustering → combine pdata**).
- Parts 2–5 use the processed outputs generated in Part 1.
- Most modules support **parameter tuning** and **plot size adjustment**.