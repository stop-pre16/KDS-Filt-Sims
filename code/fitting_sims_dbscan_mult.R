####    Fitting the no noise patterns with DBSCAN     #####
library(ggplot2)
library(MCMCpack)
library(dplyr)
library(ROCR)
library(spatstat)
library(pbapply)
library(dbscan)

save_path <- "path/to/folder/where/results/are/saved/dbscan/"
sim_path <- "path/to/sim/datasets/"
xwin <- c(-100, 100)
ywin <- c(-100, 100)

for(beta_sub in c('0', '001', '005', '01', '05', '10')){
  # beta_sub = '0'
  data_path <- paste0(sim_path, "beta_",
                      beta_sub, 
                      "/")
  res_list <- pblapply(1:100, function(idx_sim){
    # idx_sim = 1
    simPattern <- readRDS(paste0(data_path, "sim_set_", idx_sim, ".RDS"))
    sim_data_tot <- simPattern$pointPattern[ ,1:2]
    sim_data_tot2 <- simPattern$pointPattern
    ideal_eps <- 2 * sqrt(20)
    low_eps <- ideal_eps / 2
    high_eps <- 2 * ideal_eps
    eps_vec <- c(low_eps, ideal_eps, high_eps)
    clust_sizes <- sim_data_tot2 %>% filter(cluster > 0) %>% group_by(cluster) %>% 
      summarize(n_pts = n())
    ideal_npts <- min(clust_sizes$n_pts)
    low_npts <- round(ideal_npts / 2)
    high_npts <- ideal_npts * 2
    npts_vec <- c(low_npts, ideal_npts, high_npts)
    param_grid <- expand.grid(eps = eps_vec, 
                              npts = npts_vec)
    label_grid <- expand.grid(eps = c('low', 'ideal', 'high'),
                              npts = c('low', 'ideal', 'high'))
    
    ###   using default minPts
    res_dbscan <- bind_rows(lapply(1:nrow(param_grid), function(idx_sub){
      # eps_sub <- eps_vec[1]
      # idx_sub = 1
      eps_sub <- param_grid$eps[idx_sub]
      npts_sub <- param_grid$npts[idx_sub]
      res_dbscan_sub <- dbscan(sim_data_tot2[, 1:2], eps = eps_sub, borderPoints = F, minPts = npts_sub)
      res_dbscan2 <- data.frame(sim_data_tot2[, 1:2]) %>%
        mutate(class_dbscan = ifelse(res_dbscan_sub$cluster == 0, yes = "noise", no = "feature")) %>% 
        mutate(class_true = ifelse(sim_data_tot2$cluster == 0, yes = "noise", no = 'feature'))
      tab_sub <- table(truth = factor(res_dbscan2$class_true, levels = c("feature", "noise")), 
                       est = factor(as.matrix(res_dbscan2$class_dbscan)[, 1], levels = c("feature", "noise")))
      ret_sub <- data.frame(eps = label_grid$eps[idx_sub], 
                            npts = label_grid$npts[idx_sub],
                            t1e = tab_sub[1, 2] / sum(tab_sub[1, ]),
                            power = tab_sub[2, 2] / sum(tab_sub[2, ]),
                            fdr = tab_sub[1, 2] / sum(tab_sub[, 2]))
    }))
    
    ret <- list(res_dbscan = res_dbscan)
  })
  
  saveRDS(res_list, file = paste0(save_path, "res_list_", beta_sub, ".RDS"))
  
  res_tab <- bind_rows(lapply(res_list, function(x){return(x$res_dbscan)}))
  res_tab_sum <- res_tab %>% group_by(eps, npts) %>% 
    summarise(mean_t1e = mean(t1e, na.rm = T))
  
  print(res_tab_sum)
  saveRDS(res_tab_sum, file = paste0(save_path, "sum_tab_", beta_sub, ".RDS"))
}