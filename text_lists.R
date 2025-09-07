

learnings_checklist_text <- c(
  
  "An analysis adjusting for prognostic covariates tends to reduce the treatment effect standard error estimate, resulting in narrower confidence intervals, and more powerful hypothesis tests.",
  
  "The greater the correlation between the prognostic covariates and the outcome, the greater the efficiency gains.",
  
  "Despite randomization, chance imbalance in the baseline covariate may occur. The direction and magnitude of covariate imbalance impacts the direction and magnitude of shift in treatment effect point estimate when the covariate is adjusted for in analysis.", 
  
  "The concept of a non-collapsible summary measure (demonstrated with the odds ratio in the binary outcome scenario).",
  
  'The difference between the terms “adjusted”/”unadjusted” (relating to analysis method) and “conditional”/”marginal” (relating to target estimand). We can obtain a marginal estimate from an adjusted analysis.',
  
  "The standardization (g-computation) method is one approach for targeting a marginal estimand when estimating a non-collapsible measure with a covariate adjusted non-linear model (see the binary outcome scenario).",
  
  "The difficulty in interpreting conditional estimates when the model is misspecified (try changing the binary outcome input response rates to simulate heterogenous treatment effects).",
  
  "The observation that with a collapsible summary measure, or when targeting a marginal estimand, the gains in efficiency when adjusting for prognostic covariates are due to smaller standard error estimates. However with a non-collapsible summary measure (such as odds ratio) and a conditional estimand, the standard error estimate increases, while the gains in efficiency are due to the point estimate shifting away from the null."
  
)


about_descriptions <- list(
  "Aims" = "To develop intuition about how adjustment for prognostic covariates impacts treatment effect 
  estimation in simple trial settings with continuous or binary outcomes.
  ",
  
  "Data-generating mechanisms" = "For both continuous and binary scenarios, we simulate data assuming simple 1:1 randomization to two treatment arms. 
  <br><br>
  In the continuous outcome setting, the outcome Y is generated from 
  $$Y_{continuous} = 1 + \\beta_1 \\cdot \\text{treatment} + \\beta_2 \\cdot X + \\varepsilon$$
  $$\\varepsilon \\sim N(0,1)$$
  $$X_{continuous} \\sim N(0,1)$$
  Treatment effect beta_1 is set to user input. Covariate effect is set to beta_2 = rho / sqrt(1 - rho^2) where rho is the user input correlation between X and Y. 
  <br><br>
  In the binary outcome setting, we simulate a binary covariate X drawn from a Bernoulli distribution with p=0.5. The binary outcome Y is then drawn from a Bernoulli distribution with probabilities set to the conditional response rates provided as user input.
  $$X_{binary} \\sim Bernoulli(0.5)$$
  $$Y_{binary} \\sim Bernoulli(P(Y=1|\\text{treatment}, X))$$
  ",
  
  "Estimands" = "In this simplified simulation scenario, we consider two estimands which may be of interest. 
  One is a marginal estimand, also termed Population Average Treatment Effect (PATE).
  Another is a conditional estimand, also termed Conditional Average Treatment Effect (CATE).
  Using potential outcomes notation, they can be expressed as
  $$PATE = E(Y(1) - Y(0))$$
  $$CATE(x) = E(Y(1) - Y(0) | X = x)$$
  ",
  
  "Methods" = "In each scenario, we fit two models shown at the top of the continuous or binary outcome panels. 
  The unadjusted model only includes an intercept and a treatment term. 
  The adjusted model additionally adjusts for the baseline covariate X. 
  <br><br>
  For continuous outcomes, we always assume a correctly specified model. 
  For binary, the user has the option to create a simulation scenario with heterogenous treatment effects. In this case the fitted working model is misspecified.
  <br><br>
  We additionally apply the standardization (g-computation) method to the adjusted logistic regression model, with variance estimated via the delta method with robust sandwich estimator (HC3), to target a marginal estimand. 
  The implementation in R package {beeca} was used.
  ",
  
  "Performance measures" = "The treatment effect estimates for both models are presented for visual comparison. 
  Key concepts are provided in the Learnings Checklist in the top-right hand corner.
  "
  
)

