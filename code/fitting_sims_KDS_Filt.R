####    Fitting the SNCP patterns with KDS-Filt and NNClean     #####
library(ggplot2)
library(MCMCpack)
library(dplyr)
library(ROCR)
library(spatstat)
library(pbapply)
library(sncp)


save_path <- "path/to/folder/where/results/are/saved/kdsfilt/"
sim_path <- "path/to/sim/datasets/"
xwin <- c(-100, 100)
ywin <- c(-100, 100)

for(beta_sub in c('0', '001', '005', '01', '05', '10')){
  # beta_sub = '0'
  data_path <- paste0(sim_path, "beta_",
                      beta_sub, 
                      "/")
  res_list <- pblapply(1:10, function(idx_sim){
    # idx_sim = 1
    
    simPattern <- readRDS(paste0(data_path, "sim_set_", idx_sim, ".RDS"))
    sim_data_tot <- simPattern$pointPattern[ ,1:2]
    sim_data_tot2 <- simPattern$pointPattern
    
    dat_ppp <- ppp(sim_data_tot$x, sim_data_tot$y, owin(xwin, ywin))
    
    #####     ppl     #####
    dens_ppp_ppl <- density(x = dat_ppp, sigma = bw.ppl, edge = T, diggle = T, leaveoneout = T)
    sigma2_ppl <- attributes(dens_ppp_ppl)$sigma^2
    res_denoise_sim_r_ppl <- sncp::KDS_filt(sim_pattern = sim_data_tot, 
                                            xwin = xwin, 
                                            ywin = ywin, 
                                            bw_method_c = "bw.ppl",
                                            n_sim_data = 5, 
                                            thresh = .85)
    sim_data_tot4 <- sim_data_tot 
    sim_data_tot4$class_ppl <- res_denoise_sim_r_ppl$type
    
    #####     diggle     #####
    dens_ppp_diggle <- density(x = dat_ppp, sigma = bw.diggle, edge = T, diggle = T, leaveoneout = T)
    sigma2_diggle <- attributes(dens_ppp_diggle)$sigma^2
    res_denoise_sim_r_diggle <- sncp::KDS_filt(sim_pattern = sim_data_tot, 
                                                 xwin = xwin, 
                                                 ywin = ywin, 
                                                 bw_method_c = "bw.diggle",
                                                 n_sim_data = 5, 
                                                 thresh = .85)
    sim_data_tot4$class_diggle <- res_denoise_sim_r_diggle$type
    
    #####     CvL     #####
    dens_ppp_CvL <- density(x = dat_ppp, sigma = bw.CvL, edge = T, diggle = T, leaveoneout = T)
    sigma2_CvL <- attributes(dens_ppp_CvL)$sigma
    
    res_denoise_sim_r_CvL <- sncp::KDS_filt(sim_pattern = sim_data_tot, 
                                              xwin = xwin, 
                                              ywin = ywin, 
                                              bw_method_c = "bw.CvL",
                                              n_sim_data = 5, 
                                              thresh = .85)
    
    sim_data_tot4$class_CvL <- res_denoise_sim_r_CvL$type
    
    #####   nnclean     #####
    k_ideal <- min(table(simPattern$pointPattern$cluster))
    res_nnclean <- tryCatch(nnclean(X = dat_ppp, k = k_ideal, verbose = F), error = function(e) {return(NA)})
    if(any(is.na(res_nnclean))){
      sim_data_tot4$class_nnclean <- NA
    }
    else{
      sim_data_tot4$class_nnclean <- res_nnclean$marks$class
    }
    
    
    sim_data_tot4$class_true <- ifelse(simPattern$pointPattern$cluster == 0, yes = "noise", no = 'feature')
    ret <- list(sigma2_ppl = sigma2_ppl,
                sigma2_diggle = sigma2_diggle,
                sigma2_CvL = sigma2_CvL,
                res_pattern = sim_data_tot4)
    return(ret)
  })
  
  saveRDS(res_list, file = paste0(save_path, "res_list_", beta_sub, ".RDS"))
}

