**Mutation Workflow**

This workflow provides a 3-part analysis for mutation matrix construction, mutation–signature association, and survival analysis.

**🔄 Recommended Order**
1. Build Mutation Matrix
2. Find Mutations
3. Survival Analysis

**📌 Step Details**
- **Build Mutation Matrix**: Convert MAF data into binary mutation matrices.
- **Find Mutations**: Identify mutations associated with a selected signature or feature by comparing mutated and wild-type samples
- **Survival Analysis**: Upload clinical data and compare survival between WT and Mutated groups using Kaplan–Meier analysis.

**📌 Usage Notes**
- Signature scores can be calculated using the **IOBR Workflow** and then uploaded to **Find Mutations**.
- Clinical data must contain an **ID** column matching the sample IDs in the mutation matrix.
