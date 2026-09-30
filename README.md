## Abstract
 
Granular administrative data are increasingly used in official statistics and public policy, but their integration and release raise important confidentiality concerns. This paper proposes a municipal-level Composite Risk Indicator (CRI) for assessing territorial disclosure vulnerability by integrating statistical composite-indicator methods with interpretable machine learning.
 
The proposed approach operates at two levels. At the record level, a disclosure-risk score combining categorical uniqueness and continuous-attribute vulnerability is modelled using Random Forest. SHAP (SHapley Additive exPlanations) values are then used to derive global predictor-importance weights. At the municipal level, this information is transferred to aggregate representations and combined with a simple mean component and a Principal Component Analysis-based component.
 
The approach is applied to all 7,904 Italian municipalities. The machine-learning component is assessed using SAFE Rank-Graduation diagnostics, while the final CRI is evaluated in terms of explainability, territorial fairness, and robustness. Results reveal substantial territorial heterogeneity in disclosure vulnerability.
 
The CRI is intended as a relative diagnostic and screening tool rather than as a direct probability of individual re-identification or a privacy-preserving mechanism. The proposed approach illustrates how interpretable machine learning can complement statistical methods for disclosure-risk assessment in official statistics.
