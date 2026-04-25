**TCGA Cohorts Workflow**

This workflow supports **TCGA cohort analysis** based on cloud/DuckDB data, including **signature scoring / TME deconvolution** and **downstream analysis** (visualization, survival, correlation, and group comparison).

**🔄 Recommended Order**
1. Data Selection
2. TME Cluster
3. Visualization / Survival / Correlation / Group Comparison

**📌 Usage Notes**
- **Complete Part 1 first** (**Data Selection → TME Cluster**).
- Downstream tabs depend on the prepared data and clustering results.
- Most modules support **parameter tuning** and **plot size adjustment**.