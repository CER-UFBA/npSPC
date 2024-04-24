# NonparametricSP

This repository is associated with the publication entitled "NONPARAMETRIC SPC: SOLVING COMPLEX MINING COMPANY MONITORING TASKS WITH LOW-COMPUTATIONAL COST". This work aimed to implement the univariate nonparametric SPC control charts for the statistical monitoring of continuous data, although appropriated to small samples and asymetric/skewed data. Control charts are known for being a process visualization tool that presents the control limits of a process and the dynamics of observations:

| File | Function | 
| --- | --- |
| CASE K | - KNOWN PARAMETERS - |
| `cusum_sn.R` | CUSUM Control Chart for Grouped Data - Signs |
| `cusum_sn_arl_calc.R` | CUSUM-SN Average Run Length (ARL) Calculation |
| `cusum_sr.R` | CUSUM Control Chart for Grouped Data - Rank Statistics |
| `cusum_sr_arl_calc.R` | CUSUM-SR Average Run Length (ARL) Calculation |
| `ewma_sn.R` | EWMA Control Chart for Grouped Data - Signs |
| `ewma_sn_arl_calc.R` | EWMA-SN Average Run Length (ARL) Calculation |
| `ewma_sr.R` | EWMA Control Chart for Grouped Data - Rank Statistics |
| `ewma_sr_arl_calc.R` | EWMA-SR Average Run Length (ARL) Calculation |
| `shewhart_sn.R` | Shewhart Control Chart for Grouped Data - Signs |
| `shewhart_sr.R` | Shewhart Control Chart for Grouped Data - Rank Statistics |
| CASE U | - UNKNOWN PARAMETERS - |
| `cusum_ex.R` | CUSUM Control Chart for Grouped Data - Exceedance | 
| `cusum_wr.R` | CUSUM Control Chart based on the Wilcoxon Rank Sum Test |
| `ewma_ex.R` | EWMA Control Chart for Grouped Data - Exceedance | 
| `ewma_wr.R` | EWMA Control Chart based on the Wilcoxon Rank Sum Test |
| `shewhart_mw.R` | Shewhart Chart Based on Precedence Statistic |
| `shewhart_mw_arl_calc.R` | Shewhart-SR Average Run Length (ARL) Calculation |
| `shewhart_prec.R` | Shewhart Chart Based on Precedence Statistic |

- The files `d_precedence.R` and `exact_distribution.R` calculate a test statistics, `limits_asymptotical.R` calculates nonparametric upper- and lower-limits, and `plot_chart.R` arranges the estimated statistics, bounds-limit and graphics them based on the selected control chart.

- The `data1.R` is the dataset originated from the Table 4.7a (page 226) - Chakraborti and Graham book, and `data2.R` from the XXX.
