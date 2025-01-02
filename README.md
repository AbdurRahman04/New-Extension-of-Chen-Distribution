# New-Extension-of-Chen-Distribution

1. Introduction

The Weibull distribution is widely used in reliability engineering, biological studies, and lifetime data modeling due to its flexibility in capturing monotonic hazard rates. However, many real-world datasets, particularly in reliability engineering, exhibit non-monotonic hazard rates, such as bathtub-shaped patterns. To address this limitation, this study introduces the Weibull-Chen (W–C) distribution, which combines the Weibull and Chen distributions into a flexible four-parameter model capable of handling such patterns.

2. Objectives

Introduce the Weibull-Chen distribution to model flexible hazard rates.

Analyze the statistical properties of the new distribution.

Estimate parameters using the maximum likelihood estimation (MLE) method.

Conduct a Monte Carlo simulation to assess estimator performance.

Compare the Weibull-Chen distribution with other models using real lifetime datasets.

3. Methodology

3.1 Probability and Hazard Functions

The Weibull-Chen (W–C) distribution combines Weibull's decreasing hazard rate with Chen's increasing hazard rate, enabling the modeling of bathtub-shaped hazard functions.

The probability density function (PDF) and cumulative distribution function (CDF) are defined with four parameters:

Alpha (α) and Gamma (γ) as scale parameters.

Beta (β) and Theta (θ) as shape parameters.

3.2 Parameter Estimation

Parameters are estimated using the MLE method, providing approximate confidence intervals based on asymptotic normality. Since closed-form solutions are unavailable, estimates are obtained through numerical optimization.

3.3 Simulation Study

A Monte Carlo simulation evaluates the performance of the MLEs under different scenarios:

Sample sizes: 10, 30, 80, 150.

Parameter combinations: (α, β, λ, θ).

5000 replications for each scenario.

Metrics: Average estimates and mean squared errors (MSE).

3.4 Applications

Real datasets, including Aarset's 50-component lifetime data, were analyzed to assess the model's fit using Kolmogorov–Smirnov (K–S) statistics and p-values. The Weibull-Chen distribution's performance was compared to sub-models (Weibull and Chen) and four-parameter distributions.

4. Results and Findings

Hazard Rate Analysis: Demonstrated the flexibility of the W–C distribution to capture bathtub-shaped hazard rates.

Simulation Study: MLEs provided accurate estimates with low MSEs across sample sizes.

Real Data Applications: Weibull-Chen outperformed competing models, evidenced by higher goodness-of-fit measures.

5. Future Directions

Robustness Analysis: Investigate performance under outliers and non-normal data.

Extensions: Explore hybrid models by integrating additional parameters or combining with other distributions.

Applications: Extend the use of W–C distribution in fields such as healthcare, finance, and engineering.

6. Conclusion

This study demonstrates that the Weibull-Chen distribution effectively models non-monotonic hazard rates, particularly bathtub-shaped patterns. Its flexibility, supported by strong parameter estimation methods and simulation results, highlights its potential for real-world applications. Future research can focus on extending its flexibility and robustness, broadening its scope across various domains.
