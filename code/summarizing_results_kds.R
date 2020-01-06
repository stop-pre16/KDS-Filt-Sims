####    Summarizing results from SNCP sims    ####

library(dplyr)
library(ggplot2)
library(reshape2)
library(pbapply)
library(Hmisc)

results_path = "~/Documents/dissertation/messing_w_denclue/filtering_sims/sncp/results/"

###  beta0  results
res_beta0 <- readRDS(paste0(results_path, "res_list_0.RDS"))

beta0_error <- bind_rows(pblapply(res_beta0, function(x){
  # x = res_beta0[[1]]
  ret <- bind_rows(lapply(3:6, function(i){
    # i = 3
    method_sub <- strsplit(names(x$res_pattern)[i], split = "_")[[1]][2]
    res_sub <- mean(x$res_pattern[, i] == "noise")
    ret2 <- data.frame(method = method_sub, t1e = res_sub)
    return(ret2)
  }))
  return(ret)
}))

beta0_error_sum <- beta0_error %>% group_by(method) %>% 
  summarise(mean_t1e = mean(t1e, na.rm = T),
            median_t1e = median(t1e, na.rm = T))

print(beta0_error_sum)
beta0_error_sum_out <- beta0_error_sum
beta0_error_sum_out[, 2:3] <- round(beta0_error_sum_out[, 2:3], digits = 4)
# latex(beta0_error_sum_out, file = "", title = "", rowname = NULL)

###  beta001  results
res_beta001 <- readRDS(paste0(results_path, "res_list_001.RDS"))

beta001_error <- bind_rows(pblapply(res_beta001, function(x){
  # x = res_beta001[[1]]
  ret <- bind_rows(lapply(3:6, function(i){
    # i = 3
    method_sub <- strsplit(names(x$res_pattern)[i], split = "_")[[1]][2]
    tab_sub <- table(truth = factor(x$res_pattern$class_true, levels = c("feature", "noise")), 
                     est = factor(x$res_pattern[, i], levels = c("feature", "noise")))
    ret2 <- data.frame(method = method_sub, 
                       t1e = tab_sub[1, 2] / sum(tab_sub[1, ]),
                       power = tab_sub[2, 2] / sum(tab_sub[2, ]),
                       fdr = tab_sub[1, 2] / sum(tab_sub[, 2]))
    return(ret2)
  }))
  return(ret)
}))

beta001_error_sum <- beta001_error %>% group_by(method) %>% 
  summarise(mean_t1e = mean(t1e, na.rm = T), mean_power = mean(power, na.rm = T), mean_fdr = mean(fdr, na.rm = T),
            median_t1e = median(t1e, na.rm = T), median_power = median(power, na.rm = T), median_fdr = median(fdr, na.rm = T))

print(beta001_error_sum)

beta001_error_sum_out <- beta001_error_sum
beta001_error_sum_out[, 2:7] <- round(beta001_error_sum_out[, 2:7], digits = 4)
# latex(beta001_error_sum_out, file = "", title = "", rowname = NULL)

###  beta005  results
res_beta005 <- readRDS(paste0(results_path, "res_list_005.RDS"))

beta005_error <- bind_rows(pblapply(res_beta005, function(x){
  # x = res_beta005[[1]]
  ret <- bind_rows(lapply(3:6, function(i){
    # i = 3
    method_sub <- strsplit(names(x$res_pattern)[i], split = "_")[[1]][2]
    tab_sub <- table(truth = factor(x$res_pattern$class_true, levels = c("feature", "noise")), 
                     est = factor(x$res_pattern[, i], levels = c("feature", "noise")))
    ret2 <- data.frame(method = method_sub, 
                       t1e = tab_sub[1, 2] / sum(tab_sub[1, ]),
                       power = tab_sub[2, 2] / sum(tab_sub[2, ]),
                       fdr = tab_sub[1, 2] / sum(tab_sub[, 2]))
    return(ret2)
  }))
  return(ret)
}))

beta005_error_sum <- beta005_error %>% group_by(method) %>% 
  summarise(mean_t1e = mean(t1e, na.rm = T), mean_power = mean(power, na.rm = T), mean_fdr = mean(fdr, na.rm = T),
            median_t1e = median(t1e, na.rm = T), median_power = median(power, na.rm = T), median_fdr = median(fdr, na.rm = T))

print(beta005_error_sum)

beta005_error_sum_out <- beta005_error_sum
beta005_error_sum_out[, 2:7] <- round(beta005_error_sum_out[, 2:7], digits = 4)
# latex(beta005_error_sum_out, file = "", title = "", rowname = NULL)

###  beta01  results
res_beta01 <- readRDS(paste0(results_path, "res_list_01.RDS"))

beta01_error <- bind_rows(pblapply(res_beta01, function(x){
  # x = res_beta01[[1]]
  ret <- bind_rows(lapply(3:6, function(i){
    # i = 3
    method_sub <- strsplit(names(x$res_pattern)[i], split = "_")[[1]][2]
    tab_sub <- table(truth = factor(x$res_pattern$class_true, levels = c("feature", "noise")), 
                     est = factor(x$res_pattern[, i], levels = c("feature", "noise")))
    ret2 <- data.frame(method = method_sub, 
                       t1e = tab_sub[1, 2] / sum(tab_sub[1, ]),
                       power = tab_sub[2, 2] / sum(tab_sub[2, ]),
                       fdr = tab_sub[1, 2] / sum(tab_sub[, 2]))
    return(ret2)
  }))
  return(ret)
}))

beta01_error_sum <- beta01_error %>% group_by(method) %>% 
  summarise(mean_t1e = mean(t1e, na.rm = T), mean_power = mean(power, na.rm = T), mean_fdr = mean(fdr, na.rm = T),
            median_t1e = median(t1e, na.rm = T), median_power = median(power, na.rm = T), median_fdr = median(fdr, na.rm = T))

print(beta01_error_sum)

beta01_error_sum_out <- beta01_error_sum
beta01_error_sum_out[, 2:7] <- round(beta01_error_sum_out[, 2:7], digits = 4)
# latex(beta01_error_sum_out, file = "", title = "", rowname = NULL)

###  beta05  results
res_beta05 <- readRDS(paste0(results_path, "res_list_05.RDS"))

beta05_error <- bind_rows(pblapply(res_beta05, function(x){
  # x = res_beta05[[1]]
  ret <- bind_rows(lapply(3:6, function(i){
    # i = 3
    method_sub <- strsplit(names(x$res_pattern)[i], split = "_")[[1]][2]
    tab_sub <- table(truth = factor(x$res_pattern$class_true, levels = c("feature", "noise")), 
                     est = factor(x$res_pattern[, i], levels = c("feature", "noise")))
    ret2 <- data.frame(method = method_sub, 
                       t1e = tab_sub[1, 2] / sum(tab_sub[1, ]),
                       power = tab_sub[2, 2] / sum(tab_sub[2, ]),
                       fdr = tab_sub[1, 2] / sum(tab_sub[, 2]))
    return(ret2)
  }))
  return(ret)
}))

beta05_error_sum <- beta05_error %>% group_by(method) %>% 
  summarise(mean_t1e = mean(t1e, na.rm = T), mean_power = mean(power, na.rm = T), mean_fdr = mean(fdr, na.rm = T),
            median_t1e = median(t1e, na.rm = T), median_power = median(power, na.rm = T), median_fdr = median(fdr, na.rm = T))

print(beta05_error_sum)

beta05_error_sum_out <- beta05_error_sum
beta05_error_sum_out[, 2:7] <- round(beta05_error_sum_out[, 2:7], digits = 4)
# latex(beta05_error_sum_out, file = "", title = "", rowname = NULL)

###  beta10  results
res_beta10 <- readRDS(paste0(results_path, "res_list_10.RDS"))

beta10_error <- bind_rows(pblapply(res_beta10, function(x){
  # x = res_beta10[[1]]
  ret <- bind_rows(lapply(3:6, function(i){
    # i = 3
    method_sub <- strsplit(names(x$res_pattern)[i], split = "_")[[1]][2]
    tab_sub <- table(truth = factor(x$res_pattern$class_true, levels = c("feature", "noise")), 
                     est = factor(x$res_pattern[, i], levels = c("feature", "noise")))
    ret2 <- data.frame(method = method_sub, 
                       t1e = tab_sub[1, 2] / sum(tab_sub[1, ]),
                       power = tab_sub[2, 2] / sum(tab_sub[2, ]),
                       fdr = tab_sub[1, 2] / sum(tab_sub[, 2]))
    return(ret2)
  }))
  return(ret)
}))

beta10_error_sum <- beta10_error %>% group_by(method) %>% 
  summarise(mean_t1e = mean(t1e, na.rm = T), mean_power = mean(power, na.rm = T), mean_fdr = mean(fdr, na.rm = T),
            median_t1e = median(t1e, na.rm = T), median_power = median(power, na.rm = T), median_fdr = median(fdr, na.rm = T))

print(beta10_error_sum)

beta10_error_sum_out <- beta10_error_sum
beta10_error_sum_out[, 2:7] <- round(beta10_error_sum_out[, 2:7], digits = 4)
# latex(beta10_error_sum_out, file = "", title = "", rowname = NULL)
