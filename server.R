
library(shiny)
library(ggplot2)
library(dplyr)
library(shinyMatrix)
library(detectseparation)

source("text_lists.R")


server <- function(input, output, session) {
  
  ##============================================================================
  ## Top menu panel
  
  # About modal
  about_modal <- modalDialog(
    title = "About",
    tagList(
      selectInput("about_menu", NULL, choices = names(about_descriptions)),
      uiOutput("about_description")
    ),
    easyClose = TRUE,
    footer = modalButton("Close")
  )
  
  # Show About modal on startup
  session$onFlushed(function() {
    showModal(about_modal)
  }, once = TRUE)
  
  ## About button
  observeEvent(input$about_btn, {
    showModal(about_modal)
  })
  
  output$about_description <- renderUI({
    req(input$about_menu)
    withMathJax(HTML(about_descriptions[[input$about_menu]]))
  })
  
 
  
  ## Learnings Checklist 
  learnings_checklist_state <- reactiveVal(NULL)
  
  # Show modal with checklist
  observeEvent(input$learnings_btn, {
    showModal(modalDialog(
      tags$style(HTML("
        #learnings_checklist {width: 100%;}
        #learnings_checklist .checkbox {width: 100%;}
        #learnings_checklist .checkbox label {width: 100%; display: block;}
      ")),
      tags$div(
        style = "padding: 20px; width: 100%;",
        title = "Learnings Checklist",
        checkboxGroupInput(
          "learnings_checklist",
          "Use the simulations to explore these concepts:",
          choices = learnings_checklist_text,
          selected = learnings_checklist_state()  # restore previous state if any
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Save checklist state when modal closes 
  observe({
    # This will run whenever the checklist changes
    # If the modal is not open, save the state
    if (is.null(input$learnings_btn) || !isTruthy(input$learnings_btn)) return()
    # Save the state when the modal closes
    learnings_checklist_state(input$learnings_checklist)
  })
  
  
  
  ##============================================================================
  ## Continuous Outcome Tab
  
  
  # Equations
  output$cont_intro <- renderUI({
    withMathJax(
      tags$div(
        class = "left-mathjax",
        style = "font-size: 15px; margin-bottom: 16px;",
        tags$div("We fit two outcome working models", style = "margin-top: 10px;"),
        "$$\\text{Unadjusted:}\\quad Y = \\beta_0 + \\beta_1 \\cdot \\text{treatment} + \\varepsilon$$",
        "$$\\text{Adjusted:}\\quad Y = \\beta_0 + \\beta_1 \\cdot \\text{treatment} + \\beta_2 \\cdot X + \\varepsilon$$"
      )
    )
  })
  
  
  ## info buttons
  observeEvent(input$infoSampleSize, {
    showModal(modalDialog(
      title = "Sample size",
      p("Total sample size, assuming 1:1 randomization to two treatment arms."),
      easyClose = TRUE,
      footer = NULL,
      icon = icon("info-circle")
    ))
  })
  
  
  observeEvent(input$infoTrtEffect, {
    showModal(modalDialog(
      title = "True treatment effect",
      p("The true treatment effect size expressed as the mean difference between treatment arms.
        Note that for a continuous outcome analysed with a linear regression model (ANCOVA), the marginal and 
        conditional effects are equivalent due to collapsibility of the mean difference summary measure."),
      easyClose = TRUE,
      footer = NULL,
      icon = icon("info-circle")
    ))
  })
  
  
  observeEvent(input$infoCorrelation, {
    showModal(modalDialog(
      title = "Baseline covariate prognostic strength",
      p("Health authority guidance (FDA, EMA) recommends to pre-specify adjustment for known baseline prognostic factors (covariates measured at baseline that are associated with the outcome). 
        The covariate prognostic strength for the continuous outcome scenario corresponds to the correlation between a single continuous baseline covariate (X) and the outcome (Y)."),
      easyClose = TRUE,
      footer = NULL,
      icon = icon("info-circle")
    ))
  })
  
  
  observeEvent(input$infoRseed, {
    showModal(modalDialog(
      title = "Random seed",
      p("The impact of covariate adjustment can vary from trial to trial. Chance imbalance in the baseline covariate can impact results."),
      easyClose = TRUE,
      footer = NULL,
      icon = icon("info-circle")
    ))
  })
  
  
  
  simulate_trigger <- reactiveVal(0)
  
  observeEvent(input$simulate, {
    
    # Enforce sampleSize constraint
    if (input$sampleSize < 10 || input$sampleSize > 10000) {
      showModal(modalDialog(
        title = "Invalid sample size",
        p("Sample size must be between 10 and 10,000."),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }
    
    # Handle random seeds
    if (!(input$fix_seed)) {
      new_seed <- sample(1:999, 1)
      updateNumericInput(session, "rseed", value = new_seed)
      # Wait for the input to update, then increment the trigger
      # Use a delay to ensure the input is updated before triggering
      invalidateLater(100, session)
      observeEvent(input$rseed, {
        simulate_trigger(isolate(simulate_trigger()) + 1)
      }, once = TRUE, ignoreInit = TRUE)
    } else {
      simulate_trigger(isolate(simulate_trigger()) + 1)
    }
  })
  
  observe({
    session$sendCustomMessage(
      "toggleNumeric",
      list(id = "rseed", disabled = isTRUE(input$fix_seed))
    )
  })
  
  
  simulate_data <- eventReactive(simulate_trigger(), {
    #isolate({
    set.seed(input$rseed)
    n <- input$sampleSize
    n_treatment <- floor(n/2)
    n_control <- n - n_treatment
    
    # covariate
    x <- rnorm(n)
    
    # Ensure 1:1 randomization (as balanced as possible)
    treatment <- c(rep(0, n_control), rep(1, n_treatment))
    treatment <- sample(treatment)  # Shuffle to randomize order
    
    beta_0 <- 1
    beta_1 <- input$treatmentEffect
    
    # handle case where abs(cor)==1
    eps <- 1e-10
    cor <- pmax(pmin(input$correlation, 1 - eps), -1 + eps)
    
    # user input is correlation (rho) between X and Y. 
    # This needs to be converted to beta_2 = rho / sqrt(1 - rho^2) before simulating Y
    beta_2 <- cor / sqrt(1 - cor^2)
    
    epsilon <- rnorm(n, mean=0, sd=1)
    
    y <- beta_0 + beta_1 * treatment + beta_2 * x + epsilon
    
    data.frame(x = x, y = y, treatment = treatment)
    
    
  })
  
  
  continuous_models <- eventReactive(simulate_trigger(), {
    data <- simulate_data()
    unadjusted_model <- lm(y ~ treatment, data=data)
    adjusted_model <- lm(y ~ treatment + x, data=data)
    list(unadjusted = unadjusted_model, adjusted = adjusted_model)
  })
  
  
  
  
  make_effect_table <- function(unadjusted_model, adjusted_model) {
    
    test_stat <- "t"
    test_stat_column <- paste0(test_stat, " value")
    pval_column <- paste0("Pr(>|", test_stat, "|)")
    
    # Unadjusted model
    unadjusted_summary <- summary(unadjusted_model)
    unadjusted_estimate <- coef(unadjusted_summary)["treatment", "Estimate"]
    unadjusted_se <- coef(unadjusted_summary)["treatment", "Std. Error"]
    unadjusted_ci <- confint(unadjusted_model)["treatment", ]
    unadjusted_p <- coef(unadjusted_summary)["treatment", pval_column]
    unadjusted_test_stat <- coef(unadjusted_summary)["treatment", test_stat_column]
    
    
    # Adjusted model
    adjusted_summary <- summary(adjusted_model)
    adjusted_estimate <- coef(adjusted_summary)["treatment", "Estimate"]
    adjusted_se <- coef(adjusted_summary)["treatment", "Std. Error"]
    adjusted_ci <- confint(adjusted_model)["treatment", ]
    adjusted_p <- coef(adjusted_summary)["treatment", pval_column]
    adjusted_test_stat <- coef(adjusted_summary)["treatment", test_stat_column]
    
    
    # Combine results into a table
    results <- data.frame(
      Model = c("Unadjusted", "Adjusted"),
      Estimate = c(unadjusted_estimate, adjusted_estimate),
      `Std.Error` = c(unadjusted_se, adjusted_se),
      `t` = c(unadjusted_test_stat, adjusted_test_stat),
      `P.value` = c(unadjusted_p, adjusted_p)
    )
    
    results %>%
      dplyr::mutate(across(where(is.numeric) & !matches("P.value"), ~round(.x, 3))) %>%
      dplyr::mutate(`P.value` = ifelse(`P.value` < 0.001,
                                       formatC(`P.value`, format = "e", digits = 2),
                                       round(`P.value`, 3)))
    
  }
  
  output$scatterPlot <- renderPlot({
    simulate_trigger()
    data <- simulate_data()
    
    ggplot(data, aes(x=x, y=y, color = as.factor(treatment))) +
      geom_point(size=2) +
      xlab("Baseline Covariate (X)") +
      ylab("Outcome (Y)") +
      ggtitle("Scatter plot of continuous outcome vs baseline covariate") +
      scale_color_manual(values=c("0" = "brown1", "1"="darkblue")) +
      labs(color = "Treatment") +
      theme_minimal() + 
      theme(text = element_text(size=15))
    
  }) 
  
  output$densityPlot <- renderPlot({
    simulate_trigger()
    data <- simulate_data()
    data$treatment <- factor(data$treatment)
    g <- ggplot(data, aes(x=x, fill=treatment)) +
      geom_density(alpha=0.5) + 
      ggtitle("Distribution of baseline covariate by treatment group") +
      xlab("Baseline covariate (X)") + 
      scale_fill_manual(values=c("0" = "brown1", "1"="darkblue")) +
      labs(fill = "Treatment") +
      ylab("Density") +
      theme_minimal() + 
      theme(text = element_text(size=15))
    # add rugs if small sample size
    if (nrow(data) < 100) {
      g <- g + 
        geom_rug(aes(color=treatment), size=2)
    }
    g
  })
  
  output$modelSummary <- renderPrint({
    simulate_trigger()
    data <- simulate_data()
    model <- lm(y ~ treatment + x, data = data)
    summary(model)
  })
  
  
  output$effectTable <- DT::renderDataTable({
    simulate_trigger()
    models <- continuous_models()
    df <- make_effect_table(models$unadjusted, models$adjusted)
    
    tbl <- DT::datatable(
      df,
      options = list(
        dom = 't', # don't show search box, dropdown, etc. (just main table)
        paging = FALSE, # disable pagination
        ordering = FALSE # disable column sorting 
      ),
      rownames = FALSE
    ) 
    tbl
  })
  
  output$forestPlot <- renderPlot({
    simulate_trigger()
    #input$simulate
    isolate({
      data <- simulate_data()
      unadjusted_model <- lm(y ~ treatment, data = data)
      adjusted_model <- lm(y ~ treatment + x, data = data)
      results <- data.frame(
        Model = c("Unadjusted", "Adjusted"),
        Estimate = c(coef(unadjusted_model)["treatment"], coef(adjusted_model)["treatment"]),
        CI_Lower = c(confint(unadjusted_model)["treatment", 1], confint(adjusted_model)["treatment", 1]),
        CI_Upper = c(confint(unadjusted_model)["treatment", 2], confint(adjusted_model)["treatment", 2])
      )
      ggplot(results, aes(x = Model, y = Estimate, ymin = CI_Lower, ymax = CI_Upper)) +
        geom_pointrange() +
        geom_hline(yintercept = input$treatmentEffect, linetype = "dashed", color = "red") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
        coord_flip() +
        labs(title = "Treatment Effect Estimates", 
             y = expression(paste(beta[1], " Estimate (95% CI)")), 
             x = "") +
        theme_minimal() + 
        theme(text = element_text(size=15))
    })
  })
  
  
  ##============================================================================
  # Binary Outcome Tab
  
  
  # Equations
  output$binary_intro <- renderUI({
    withMathJax(
      tags$div(
        class = "left-mathjax",
        style = "font-size: 15px; margin-bottom: 16px;",
        tags$div("We fit two outcome working models", style = "margin-top: 10px;"),
        "$$\\text{Unadjusted:}\\quad logit\\{P(Y=1|\\text{treatment},X)\\} = \\beta_0 + \\beta_1 \\cdot \\text{treatment}$$",
        "$$\\text{Adjusted:}\\quad logit\\{P(Y=1|\\text{treatment},X)\\} = \\beta_0 + \\beta_1 \\cdot \\text{treatment} + \\beta_2 \\cdot X $$",
        tags$div("and also apply g-computation to the adjusted model to obtain a marginal estimate.")
      )
    )
  })
  
  
  ## info buttons
  observeEvent(input$infoBinarySampleSize, {
    showModal(modalDialog(
      title = "Sample size",
      p("Total sample size, assuming 1:1 randomization to two treatment arms. 
        Note, reducing sample size too low may result in convergence or complete separation issues when fitting the logistic regression models."),
      easyClose = TRUE,
      footer = NULL,
      icon = icon("info-circle")
    ))
  })
  
  observeEvent(input$infoBinaryTrt, {
    showModal(modalDialog(
      title = "True outcome response rates",
      p("In this case we have a binary baseline covariate X. Enter the conditional outcome response probabilities (%) for each level of X and treatment arm."),
      easyClose = TRUE,
      footer = NULL,
      icon = icon("info-circle")
    ))
  })
  
  observeEvent(input$infoBinaryRseed, {
    showModal(modalDialog(
      title = "Random seed",
      p("The impact of covariate adjustment can vary from trial to trial. Chance imbalance in the baseline covariate can impact results."),
      easyClose = TRUE,
      footer = NULL,
      icon = icon("info-circle")
    ))
  })
  
  
  # Check matrix inputs
  
  observe({
    req(input$trt_matrix)
    
    # Clamp values to [0, 100]
    val_matrix <- pmin(pmax(input$trt_matrix, 0), 100)
    
    # Optionally, update the UI if user enters out-of-bounds values:
    if (!identical(val_matrix, input$trt_matrix)) {
      updateMatrixInput(session, "trt_matrix", value = val_matrix)
    }
    
  })
  
  
  binary_simulate_trigger <- reactiveVal(0)
  
  observeEvent(input$simulateBinary, {
    
    # Enforce binarySampleSize constraint
    if (input$binarySampleSize < 10 || input$binarySampleSize > 10000) {
      showModal(modalDialog(
        title = "Invalid sample size",
        p("Sample size must be between 10 and 10,000."),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }
    
    # Handle random seed
    if (!(input$fix_seed_binary)) {
      new_seed <- sample(1:999, 1)
      updateNumericInput(session, "rseed_binary", value = new_seed)
      # Wait for the input to update, then increment the trigger
      # Use a delay to ensure the input is updated before triggering
      invalidateLater(100, session)
      observeEvent(input$rseed_binary, {
        binary_simulate_trigger(isolate(binary_simulate_trigger()) + 1)
      }, once = TRUE, ignoreInit = TRUE)
    } else {
      binary_simulate_trigger(isolate(binary_simulate_trigger()) + 1)
    }
  })
  
  observe({
    session$sendCustomMessage(
      "toggleNumeric",
      list(id = "rseed_binary", disabled = isTRUE(input$fix_seed_binary))
    )
  })
  
  
  trt_matrix_snapshot <- eventReactive(binary_simulate_trigger(), {
    input$trt_matrix
  })
  
  # Simulate binary data
  sim_binary_data <- function(n, input_matrix) {
    set.seed(input$rseed_binary)
    
    x <- rbinom(n, 1, 0.5)
    trt <- rbinom(n, 1, 0.5)
    probs <- unlist(lapply(1:n, 
                           \(i) input_matrix[as.numeric(!x[i])+1, 
                                             as.numeric(!trt[i])+1]
    )) / 100
    
    y <- rbinom(n, 1, probs)
    
    data.frame(
      x = x,
      treatment = trt,
      y = y
    )
    
  }
  
  simulate_binary_data <- eventReactive(binary_simulate_trigger(), {
    
    sim_binary_data(n = input$binarySampleSize, 
                    input_matrix = input$trt_matrix)
    
  })
  
  
  get_or_table <- function(input_matrix) {
    
    tbl <- data.frame(
      index = c("X=1", "X=0", "Combined"),
      pop = c(50, 50, 100),
      trt = c(input_matrix[1,1], input_matrix[2,1], NA),
      con = c(input_matrix[1,2], input_matrix[2,2], NA)
    )
    
    tbl[tbl$index=="Combined", ]$trt <- round(mean(c(input_matrix[1,1], input_matrix[2,1])), 1)
    tbl[tbl$index=="Combined", ]$con <- round(mean(c(input_matrix[1,2], input_matrix[2,2])), 1)
    
    tbl <- tbl %>%
      mutate(or = round(((trt/100)*(1-(con/100)))/((con/100)*(1-(trt/100))), 1)) 
    
    return(tbl)
    
  }
  
  
  get_marginal_binary_trteffect <- function(conditional_logor, b_x) {
    set.seed(input$rseed_binary)
    x <- rnorm(1e5)
    
    # counterfactual means
    p_1 <- mean(plogis(-1 + conditional_logor * 1 + b_x * x))
    p_0 <- mean(plogis(-1 + conditional_logor * 0 + b_x * x))
    
    # return marginal odds ratio
    marginal_or <- log((p_1 / (1 - p_1)) / (p_0 / (1 - p_0)))
    round(marginal_or, 2)
  }
  
  
  binary_models <- eventReactive(binary_simulate_trigger(), {
    data <- simulate_binary_data()
    unadjusted_model <- glm(y ~ treatment, data=data, family = binomial)
    adjusted_model <- glm(y ~ treatment + x, data=data, family = binomial)
    
    unadjusted_model_separation <- glm(y ~ treatment, data=data, family = binomial,
                                       method = "detect_separation")
    
    adjusted_model_separation <- glm(y ~ treatment + x, data=data, family = binomial,
                                     method = "detect_separation")
    
    marginal_Ge_mb <- glm(y ~ treatment + x, 
                          data=data |> dplyr::mutate(treatment = as.factor(treatment)), 
                          family = binomial) |>
      beeca::get_marginal_effect(trt="treatment", method = "Ge", type = "HC3",
                                 contrast = "logor",
                                 reference = "0")
    
    list(unadjusted = unadjusted_model, 
         adjusted = adjusted_model,
         marginal_Ge_mb = marginal_Ge_mb,
         unadjusted_separation = unadjusted_model_separation,
         adjusted_separation = adjusted_model_separation)
  })
  
  
  ## binary outputs 
  
  make_binary_effect_table <- function(binary_models) {
    
    test_stat <- "z"
    test_stat_column <- paste0(test_stat, " value")
    pval_column <- paste0("Pr(>|", test_stat, "|)")
    
    unadjusted_model <- binary_models$unadjusted
    adjusted_cond_model <- binary_models$adjusted
    adjusted_marginal_model <- binary_models$marginal_Ge_mb
    
    # Unadjusted model
    unadjusted_summary <- summary(unadjusted_model)
    unadjusted_estimate <- coef(unadjusted_summary)["treatment", "Estimate"]
    unadjusted_se <- coef(unadjusted_summary)["treatment", "Std. Error"]
    unadjusted_ci <- confint(unadjusted_model)["treatment", ]
    unadjusted_p <- coef(unadjusted_summary)["treatment", pval_column]
    unadjusted_test_stat <- coef(unadjusted_summary)["treatment", test_stat_column]
    
    
    # Adjusted model (conditional)
    conditional_summary <- summary(adjusted_cond_model)
    conditional_estimate <- coef(conditional_summary)["treatment", "Estimate"]
    conditional_se <- coef(conditional_summary)["treatment", "Std. Error"]
    conditional_ci <- confint(adjusted_cond_model)["treatment", ]
    conditional_p <- coef(conditional_summary)["treatment", pval_column]
    conditional_test_stat <- coef(conditional_summary)["treatment", test_stat_column]
    
    
    # Adjusted model (marginal)
    marginal_results <- adjusted_marginal_model$marginal_results
    marginal_logor_est <- marginal_results[marginal_results$STAT == "logor", "STATVAL"][[1]]
    marginal_logor_se <- marginal_results[marginal_results$STAT == "logor_se", "STATVAL"][[1]]
    
    ## 95% confidence interval
    marginal_ci_l <- marginal_logor_est - (qnorm(0.975) * marginal_logor_se)
    marginal_ci_u <- marginal_logor_est + (qnorm(0.975) * marginal_logor_se)
    
    ## Two-sided p-value
    marginal_z_score <- marginal_logor_est / marginal_logor_se
    marginal_p_value <- 2 * (1 - pnorm(abs(marginal_z_score)))
    
    
    # Combine results into a table
    results <- data.frame(
      Model = c("Unadjusted (marginal)", "Adjusted (marginal)", "Adjusted (conditional)"),
      `OR` = c(exp(unadjusted_estimate), exp(marginal_logor_est), exp(conditional_estimate)),
      `log.OR` = c(unadjusted_estimate, marginal_logor_est, conditional_estimate),
      `Std.Error` = c(unadjusted_se, marginal_logor_se, conditional_se),
      `z` = c(unadjusted_test_stat,  marginal_z_score, conditional_test_stat),
      `P.value` = c(unadjusted_p, marginal_p_value, conditional_p)
    )
    
    # round to 3 digits
    #results %>%
    #  dplyr::mutate(across(where(is.numeric), ~round(.x, 3)))
    
    results %>%
      dplyr::mutate(across(where(is.numeric) & !matches("P.value"), ~round(.x, 3))) %>%
      dplyr::mutate(`P.value` = ifelse(`P.value` < 0.001,
                                       formatC(`P.value`, format = "e", digits = 2),
                                       round(`P.value`, 3)))
    
  }
  
  
  output$binaryTableOne <- DT::renderDataTable({
    binary_simulate_trigger()
    
    df <- get_or_table(trt_matrix_snapshot())
    
    # Add a flag column to indicate estimand type
    df$estimand <- c(0, 0, 1)
    
    
    # rename columns
    df <- df %>%
      rename(`Subgroup` = index,
             `% of target population` = pop,
             `Treatment \n success rate` = trt,
             `Placebo \n success rate` = con,
             `Odds ratio` = or)
    
    tbl <- DT::datatable(
      df,
      options = list(
        columnDefs = list(list(targets="estimand", visible=FALSE)), # hide the last column (model_failed)
        dom = 't', # don't show search box, dropdown, etc. (just main table)
        paging = FALSE, # disable pagination
        ordering = FALSE # disable column sorting 
      ),
      rownames = FALSE,
      caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left; color:black;  font-size:100% ;',
                                        'Simulated data are drawn from this hypothetical target population')
    ) 
    
    
    # conditional formatting
    tbl |> DT::formatStyle(
      ncol(df)-1,
      'estimand', 
      backgroundColor = DT::styleEqual(c(0, 1), c("#F8766D", "#00BFC4"))
    )
    
  })
  
  output$binaryDensityPlot <- renderPlot({
    binary_simulate_trigger()
    
    data <- simulate_binary_data()
    data$treatment <- factor(data$treatment)
    g <- ggplot(data, aes(x=x, fill=treatment)) +
      geom_density(alpha=0.4) + 
      ggtitle("Distribution of baseline covariate (x) by treatment group") + 
      xlab("Baseline covariate (x)")
    # add rugs if small sample size
    if (nrow(data) < 100) {
      g <- g + 
        geom_rug(aes(color = treatment), size=2)
    }
    g
  })
  
  output$binaryScatterPlot <- renderPlot({
    binary_simulate_trigger()
    
    data <- simulate_binary_data()
    plot(data$x, jitter(data$y), col = ifelse(data$treatment == 1, "blue", "red"),
         pch = 19, xlab = "Baseline Covariate (x)", ylab = "Binary Outcome (y)",
         main = "Scatter plot of binary outcome vs baseline covariate",
         yaxt = "n")
    axis(2, at = c(0, 1), labels = c("0", "1"))
    legend("topright", legend = c("Treatment = 1", "Treatment = 0"), 
           col = c("blue", "red"), pch = 19)
  })
  
  
  
  output$binaryBarPlot <- renderPlot({
    binary_simulate_trigger()
    
    data <- simulate_binary_data()
    
    res_prop <- data %>%
      group_by(treatment, x, y) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(treatment) %>%
      mutate(prop = n / sum(n))
    
    treatment_labels <- c(
      "0" = paste0("Control group (N=", sum(data$treatment==0), ")"),
      "1" = paste0("Treatment group (N=", sum(data$treatment==1), ")")
    )
    
    ggplot(res_prop, aes(x = factor(y), y = prop, fill = factor(x))) +
      geom_bar(stat = "identity", position = "stack") +
      facet_wrap(~treatment, labeller = as_labeller(treatment_labels)) +
      ylab("Proportion of total in group") +
      xlab("Outcome (Y)") +
      scale_fill_discrete(name = "Covariate (X)") + 
      theme_minimal()
    
  })
  
  
  output$binaryBarPlotImbalance <- renderPlot({
    binary_simulate_trigger()
    
    data <- simulate_binary_data()
    
    x_prop <- data %>% 
      group_by(treatment, x) %>% 
      summarise(n=n(), .groups = "drop") %>%
      group_by(treatment) %>%
      mutate(prop = n / sum(n))
    
    ggplot(x_prop, aes(x = factor(treatment), y = prop, fill = factor(x))) +
      geom_bar(stat = "identity", position = "stack", alpha=0.7) +
      ylab("Proportion") +
      xlab("Treatment group") +
      ggtitle("Observed covariate distribution by treatment group") + 
      scale_fill_manual(values = c("0"="chartreuse3", "1"="darkmagenta")) +
      labs(fill = "Covariate X") +
      theme_minimal() + 
      theme(text = element_text(size=15))
    
  })
  
  
  output$binaryEstimandExplanation <- renderUI({
    tags$div(
      style = "border: 1px solid #ccc; border-radius: 8px; padding: 16px; margin-bottom: 20px; background: #f9f9f9;",
      
      tags$details(
        tags$summary(
          style = "font-weight: bold; font-size: 16px; cursor: pointer;",
          "▼ Explanation: conditional vs marginal effects"
        ),
        tags$div(
          style = "margin-top: 10px;",
          "Conditional and marginal odds ratios do not always coincide, even when subgroup treatment effects are identical. This is termed non-collapsibility.\n
          We must be explicit about which estimand we are targeting prior to performing analysis.\n
          \n
          A typical choice of analysis method for binary outcome data is logistic regression. If we take the coefficient of the treatment term as the treatment effect estimate, then with an unadjusted model (only treatment term), this will correspond to the marginal (log) odds ratio. With an adjusted model (adjusting for treatment and covariates), this will correspond to a conditional (log) odds ratio.\n
          Adjusting for different covariate sets will target different conditional estimands.\n
          We can adjust for covariates and still target a marginal estimand by applying g-computation (standardization).
          "
        )
      )
      
    )
    
  })
  
  
  output$binaryEffectExplanation <- renderUI({
    binary_simulate_trigger()
    
    #true_tbl <- get_or_table(input$trt_matrix)
    true_tbl <- get_or_table(trt_matrix_snapshot())  
    
    true_conditional <- unique(round(true_tbl[true_tbl$index!="Combined", ]$or, 1))
    true_marginal <- round(true_tbl[true_tbl$index=="Combined", ]$or, 1)
    
    if (length(true_conditional)>1) {
      misspec_text <- "<p><b>Note:</b> in this simulation scenario, the treatment effect varies across strata of X in the target population. The logistic regression model is misspecified in this case as it does not account for treatment by covariate interactions.</p>"
      cond_text <- paste0("<p>The true <b style='color: #F8766D;'>conditional</b> treatment effects (odds ratio) are: <b style='color: #F8766D;'>", paste0(true_conditional, collapse = " and "), " (log OR: ", paste0(round(log(true_conditional), 1), collapse="; "), ")</b>.</p>")
      cond_text <- paste0(misspec_text, cond_text)      
    } else {
      cond_text <- paste0("<p>The true <b style='color: #F8766D;'>conditional</b> treatment effect (odds ratio) is: <b style='color: #F8766D;'>", true_conditional, " (log OR: ", round(log(true_conditional), 1), ")</b>.</p>")
    }
    
    # Generate explanatory text
    HTML(paste0(
      cond_text,
      "<p>The corresponding true <b style='color: #00BFC4;'>marginal</b> treatment effect (odds ratio) is: <b style='color: #00BFC4;'>", true_marginal, " (log OR: ", round(log(true_marginal),1), ")</b>.</p>",
      "<br>"
    ))
    
  })
  
  
  output$binaryModelSummary <- renderPrint({
    binary_simulate_trigger()
    
    data <- simulate_binary_data()
    model <- glm(y ~ treatment + x, data = data, family = binomial)
    summary(model)
  })
  
  output$binaryEffectTable <- DT::renderDataTable({
    binary_simulate_trigger()
    
    models <- binary_models()
    df <- make_binary_effect_table(models)  
    
    # check if model converged or complete separation detected
    unadjusted_model <- models$unadjusted
    unadjusted_separation <- models$unadjusted_separation$outcome
    unadjusted_failed <- !unadjusted_model$converged || unadjusted_separation
    
    adjusted_cond_model <- models$adjusted
    adjusted_separation <- models$adjusted_separation$outcome
    conditional_failed <- !adjusted_cond_model$converged || adjusted_separation
    
    # if conditional failed then marginal failed (same logistic model)
    marginal_failed <- conditional_failed
    
    # Add a flag column to indicate if the model failed
    df$Model_Failed <- c(unadjusted_failed, marginal_failed, conditional_failed)
    
    tbl <- DT::datatable(
      df,
      options = list(
        columnDefs = list(list(targets="Model_Failed", visible=FALSE)), # hide the last column (model_failed)
        dom = 't', # don't show search box, dropdown, etc. (just main table)
        paging = FALSE, # disable pagination
        ordering = FALSE # disable column sorting 
      ),
      rownames = FALSE
    ) 
    
    
    # conditional formatting
    tbl |> DT::formatStyle(
      2:(ncol(df)-1),
      'Model_Failed', 
      backgroundColor = DT::styleEqual(c(0, 1), c('white', '#ff8787'))
    )
    
  })
  
  output$binaryForestPlot <- renderPlot({

    binary_simulate_trigger()
    
    # Add dependency on input$binary_forest_scale so plot updates when toggled
    input$binary_forest_scale
    
    isolate({
    data <- simulate_binary_data()
    
    true_tbl <- get_or_table(input$trt_matrix)
    
    binary_models <- binary_models()
    unadjusted_model <- binary_models$unadjusted
    adjusted_cond_model <- binary_models$adjusted
    adjusted_marginal_model <- binary_models$marginal_Ge_mb
    
    adjusted_separation <- binary_models$adjusted_separation$outcome
    
    if (adjusted_separation) {
      validate(
        need(FALSE, "Complete separation detected!")
      )
    }
    
    if (!adjusted_cond_model$converged) {
      validate(
        need(FALSE, "Model did not converge!")
      )
    }
    
    # get marginal stats
    marginal_results <- adjusted_marginal_model$marginal_results
    marginal_logor_est <- marginal_results[marginal_results$STAT == "logor", "STATVAL"][[1]]
    marginal_logor_se <- marginal_results[marginal_results$STAT == "logor_se", "STATVAL"][[1]]
    marginal_ci_l <- marginal_logor_est - (qnorm(0.975) * marginal_logor_se)
    marginal_ci_u <- marginal_logor_est + (qnorm(0.975) * marginal_logor_se)
    
    # get truth
    true_marginal_logor <- log(true_tbl[true_tbl$index=="Combined", ]$or)
    true_conditional_logor <- log(true_tbl[true_tbl$index!="Combined", ]$or)
    null_effect <- 0 # (OR scale)
    
    # results on log-OR scale
    results_logor <- data.frame(
      Model = c("Unadjusted (marginal)", "Adjusted (conditional)", "Adjusted (marginal)"),
      Estimate = c(coef(unadjusted_model)["treatment"], coef(adjusted_cond_model)["treatment"], marginal_logor_est),
      CI_Lower = c(confint(unadjusted_model)["treatment", 1], confint(adjusted_cond_model)["treatment", 1], marginal_ci_l),
      CI_Upper = c(confint(unadjusted_model)["treatment", 2], confint(adjusted_cond_model)["treatment", 2], marginal_ci_u),
      estimand = c("marginal", "conditional", "marginal")
    )
    
    # results on OR scale
    results_or <- results_logor
    results_or$Estimate <- exp(results_or$Estimate)
    results_or$CI_Lower <- exp(results_or$CI_Lower)
    results_or$CI_Upper <- exp(results_or$CI_Upper)
    
    scale_type <- input$binary_forest_scale
    if (is.null(scale_type) || scale_type == "or") {
      results <- results_or
      ylab <- "OR Estimate (95% CI)"
      hline_conditional <- exp(true_conditional_logor)
      hline_marginal <- exp(true_marginal_logor)
      hline_null <- exp(null_effect)
      title <- "Treatment Effect Estimates (Odds Ratio)"
    } else {
      results <- results_logor
      ylab <- "Log-OR Estimate (95% CI)"
      hline_conditional <- true_conditional_logor
      hline_marginal <- true_marginal_logor
      hline_null <- null_effect
      title <- "Treatment Effect Estimates (Log Odds Ratio)"
    }
    
    ggplot(results, aes(x = Model, y = Estimate, ymin = CI_Lower, ymax = CI_Upper, color=estimand)) +
      geom_pointrange() +
      geom_hline(yintercept = hline_conditional , linetype = "dashed", color = "#F8766D") +
      geom_hline(yintercept = hline_marginal, linetype = "dashed", color = "#00BFC4") +
      geom_hline(yintercept = hline_null, linetype = "dashed", color = "black") +
      coord_flip() +
      labs(title = title, y = ylab, x = "") +
      guides(color="none") +
      theme_minimal() +
      theme(text = element_text(size=15))
    })
  })
  
  
  output$binaryCounterfactualSection <- renderUI({
    binary_simulate_trigger()
    
    tags$div(
      style = "border: 1px solid #ccc; border-radius: 8px; padding: 16px; margin-bottom: 20px; background: #f9f9f9;",
      tags$details(
        tags$summary(
          style = "font-weight: bold; font-size: 16px; cursor: pointer;",
          "▼ Show counterfactual predictions"
        ),
        tags$div(
          style = "margin-bottom: 10px;",
          "To obtain marginal point estimates from an adjusted logistic regression model, the standardization (g-computation) procedure can be applied. 
          The adjusted logistic regression model is used to predict the probability of response for all subjects in the trial under both possible counterfactual treatments (assuming all were treated (TRT=1) or all were on control (TRT=0)).
          The counterfactual means can then be estimated for each treatment arm by averaging the predictions, which are then used to estimate the marginal treatment effect (in this case presented as an odds ratio).
          "
        ),
        plotOutput("binary_counterfactuals_plot", height = "300px")
      )
    )
  })
  
  output$binary_counterfactuals_plot <- renderPlot({
    
    binary_simulate_trigger()
    
    # get beeca model fit
    binary_models <- binary_models()
    fit <- binary_models$marginal_Ge_mb
    
    # create dataframe for plotting
    cf_df <- cbind(
      fit$model,
      data.frame(
        pred_0 = fit$counterfactual.predictions$`0`,
        pred_1 = fit$counterfactual.predictions$`1`
      )
    ) |> 
      mutate(id = 1:nrow(fit$model)) |>
      tidyr::pivot_longer(cols = c("pred_0", "pred_1")) %>%
      mutate(lbl = "patient\nprediction")  %>%
      mutate(name=case_match(name, 
                             "pred_1" ~ "TRT=1",
                             "pred_0" ~ "TRT=0"))
    
    
    mean_vals <- cf_df %>% group_by(name) %>% summarise(mean_pred = mean(value)) %>%
      mutate(lbl = "mean")
    y_bottom <- 0.5
    mean_diff <- abs(mean_vals$mean_pred[2] - mean_vals$mean_pred[1])
    
    odds1 <- mean_vals$mean_pred[1] / (1 - mean_vals$mean_pred[1])
    odds2 <- mean_vals$mean_pred[2] / (1 - mean_vals$mean_pred[2])
    odds_ratio <- odds2 / odds1
    
    mean_y_shift <- 0
    
    
    cf_df %>%
      mutate(x=factor(x)) %>%
      ggplot(aes(x = value, y = name, color = x, shape=lbl)) +
      geom_point(alpha = 0.7, position = position_jitter(height = 0.1, width=0)) +
      
      geom_point(data=mean_vals, aes(x=mean_pred, y=name), color="black", 
                 #shape=18, 
                 size=4,
                 position = position_nudge(y=mean_y_shift)) +
      
      # Add a line from y = -0.1 (bottom of axis) to each mean point
      geom_segment(
        data = mean_vals,
        aes(x = mean_pred, xend = mean_pred, y = 0, yend = as.numeric(factor(name)) + mean_y_shift),
        inherit.aes = FALSE,
        linetype = "dashed"
      ) + 
      
      geom_segment(
        aes(
          x = mean_vals$mean_pred[1],
          xend = mean_vals$mean_pred[2],
          y = y_bottom,
          yend = y_bottom
        ),
        color = "black",
        arrow = arrow(ends = "both", type = "closed", length = unit(0.15, "inches")),
        inherit.aes = FALSE
      ) +
      
      # Add text under the double-arrowed line showing the risk difference
      # annotate(
      #   "text",
      #   x = mean(mean_vals$mean_pred),
      #   y = y_bottom - 0.1,
      #   label = paste0("Diff = ", round(mean_diff, 3)),
      #   vjust = 1,
      #   size = 4
      # ) +
      
    # Add text under the double-arrowed line showing the odds ratio
    annotate(
      "text",
      x = mean(mean_vals$mean_pred),
      y = y_bottom - 0.1,
      label = paste0("OR = ", round(odds_ratio, 3)),
      vjust = 1,
      size = 4
    ) +
      
      labs(x = "Predicted response", y = "Counterfactual treatment", 
           color = "Covariate",
           shape = element_blank()) +
      
      scale_color_manual(values=c("0" = "chartreuse3", "1"="darkmagenta")) +
      scale_shape_manual(values=c("mean" = 4, "patient\nprediction"=5)) +
      
      theme_bw() +
      theme(text = element_text(size=15))
    
    
  })
  
}