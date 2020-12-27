ggplotColours <- function(n = 6, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

c2 <- function(...) {
  vals <- c(...)
  
  if (is.null(names(vals))) {
    missing_names <- rep(TRUE, length(vals))
  } else {
    missing_names <- names(vals) == ""
  }
  if (any(missing_names)) {
    names <- vapply(substitute(list(...))[-1], deparse, character(1))
    names(vals)[missing_names] <- names[missing_names]
  }
  
  vals
}

valid <- function(formula, data, fold){
  
  pred_vec <- rep(NA, nrow(data))
  
  for(i in 1:length(fold)){
    
    p <- rep(NA, length(fold[[i]]))
    
    model <- glmer(formula,
                   data=data[-fold[[i]],],
                   family=binomial)
    
    p <- (predict(model, data[fold[[i]],], type="response",
                  allow.new.levels=T)>.5)
    
    for(j in 1:length(fold[[i]])){
      pred_vec[as.numeric(names(p[j]))] <- p[j]
    }
    
  }
  pred_vec <- as.numeric(pred_vec)
  
  true_pos <- rep(NA, nrow(data))
  
  for(j in 1:nrow(data)){
    if(pred_vec[j]==data$recruit_yoy[j]){
      true_pos[j] <- 1
    }else{true_pos[j] <- 0}
  }
  
  return(true_pos)
}

model.valid.re <- function(data.re, fit.re,
                           B=5, seed=1){
  test_list <- list()
  one_test_list <- list()
  roc_list <- list()
  # Random effect
  auc.train.re <- vector(mode = "numeric", length = B)
  auc.test.re <- vector(mode = "numeric", length = B)
  data.re$pred.prob.full <- fitted(fit.re)
  auc.full.re <- roc(data.re$recruit_yoy,
                     data.re$pred.prob.full, data=data.re)$auc
  
  # Core loop
  set.seed(seed)
  for(i in 1:B){
    sample <- sample.split(data.re$recruit_yoy, 
                           SplitRatio = .7)
    
    # Random effect
    train.re <- subset(data.re, sample == TRUE)
    test.re <- subset(data.re, sample == FALSE)
    
    fit.train.re <- glmer(formula(fit.re),
                          data = train.re,
                          family = "binomial",
                          nAGQ=0)
    
    train.re$pred.prob <- fitted(fit.train.re)
    roc.train.re <- roc(train.re$recruit_yoy, 
                        train.re$pred.prob,
                        data=train.re)
    auc.train.re[i] <- roc.train.re$auc
    
    test.re$pred.prob.back <- predict(fit.train.re,
                                      newdata=test.re,
                                      type="response",
                                      allow.new.levels=T)
    roc.test.re <- roc(test.re$recruit_yoy,
                       test.re$pred.prob.back,
                       data=test.re)
    auc.test.re[i] <- roc.test.re$auc
    
    ## Return coords of both ROC ##
    tr.re <- roc.train.re %>%
      coords(ret = c("sensitivity", "specificity"), 
             transpose=F) %>%
      add_column("train_or_test"="train",
                 "FE_or_ME"="ME") %>%
      mutate("point"=1:n())
    te.re <- roc.test.re %>%
      coords(ret = c("sensitivity", "specificity"),
             transpose=F) %>%
      add_column("train_or_test"="test",
                 "FE_or_ME"="ME") %>%
      mutate("point"=1:n())
    
    roc_list[[i]] <- rbind(tr.re, te.re) %>%
      add_column("Loop"=i)
    
    ## One-ROC test ##
    # Train
    auc.test.train.re <- roc.area(obs=as.numeric(
      as.character(train.re$recruit_yoy)),
      pred=train.re$pred.prob) %>%
      as_tibble() %>%
      add_column("train_or_test"="train",
                 "ME_or_FE"="ME")
    
    # Test
    auc.test.test.re <- roc.area(obs=as.numeric(
      as.character(test.re$recruit_yoy)),
      pred=test.re$pred.prob.back) %>%
      as_tibble() %>%
      add_column("train_or_test"="test",
                 "ME_or_FE"="ME")
    
    # Combine and put in list
    one_test_list[[i]] <- rbind(auc.test.train.re,
                                auc.test.test.re)
    
  }
  
  ## Putting results together for return() ##
  # Tibble of all roc.test() results
  one_test_tibble <- bind_rows(one_test_list)
  
  # Tibble of summary statistics for each point along ROC curve
  # Right now, only sensitivity and specificity
  # Add precision and recall?
  roc_tibble <- bind_rows(roc_list) %>% 
    group_by(train_or_test, point) %>%
    summarize(mean_sens = mean(sensitivity),
              mean_spec = mean(specificity),
              L_sens = quantile(sensitivity, 0.025),
              U_sens = quantile(sensitivity, 0.975))
  
  ## Summary statistics for AUC ##
  ## Clean this up using dplyr::summarize()
  
  # Random effects
  auc.train.min.re <- round(min(auc.train.re), digits=4)
  auc.train.max.re <- round(max(auc.train.re), digits=4)
  auc.train.q1.re <- round(quantile(auc.train.re, 0.25), digits=4)
  auc.train.q3.re <- round(quantile(auc.train.re, 0.75), digits=4)
  auc.train.median.re <- round(median(auc.train.re), digits=4)
  auc.train.mean.re <- round(mean(auc.train.re), digits=4)
  auc.train.var.re <- round(var(auc.train.re), digits=4)
  
  auc.test.min.re <- round(min(auc.test.re), digits=4)
  auc.test.max.re <- round(max(auc.test.re), digits=4)
  auc.test.q1.re <- round(quantile(auc.test.re, 0.25), digits=4)
  auc.test.q3.re <- round(quantile(auc.test.re, 0.75), digits=4)
  auc.test.median.re <- round(median(auc.test.re), digits=4)
  auc.test.mean.re <- round(mean(auc.test.re), digits=4)
  auc.test.var.re <- round(var(auc.test.re), digits=4)
  
  # Combine all tibbles into list for return()
  ret_list <- lst(roc_tibble,
                  one_test_tibble)
  
  return(ret_list)
}
