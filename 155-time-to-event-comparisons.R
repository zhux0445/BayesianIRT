

adni.survival.cn.mci <- readRDS(file=path(r_objects_folder, "025_adni_survival_cn_mci.rds"))
adni.survival.cn <- readRDS(file=path(r_objects_folder, "025_adni_survival_cn.rds"))
adni.survival.mci <- readRDS(file=path(r_objects_folder, "025_adni_survival_mci.rds"))

adni.survival.cn.apoe <- adni.survival.cn %>%
  filter(!is.na(apoe4))
my_surv_object_cn_apoe <- with(adni.survival.cn.apoe, Surv(time_in_adni, event))

adni.survival.mci.apoe <- adni.survival.mci %>%
  filter(!is.na(apoe4))
my_surv_object_mci_apoe <- with(adni.survival.mci.apoe, Surv(time_in_adni, event))

adni.survival.cn.csf <- adni.survival.cn %>%
  filter(!is.na(cluster_mixtmodel))
my_surv_object_cn_csf <- with(adni.survival.cn.csf, Surv(time_in_adni, event))

adni.survival.mci.csf <- adni.survival.mci %>%
  filter(!is.na(cluster_mixtmodel))
my_surv_object_mci_csf <- with(adni.survival.mci.csf, Surv(time_in_adni, event))

adni.survival.cn.ent <- adni.survival.cn %>%
  filter(!is.na(entorhinal_z))
my_surv_object_cn_ent <- with(adni.survival.cn.ent, Surv(time_in_adni, event))

adni.survival.mci.ent <- adni.survival.mci %>%
  filter(!is.na(entorhinal_z))
my_surv_object_mci_ent <- with(adni.survival.mci.ent, Surv(time_in_adni, event))

adni.survival.cn.hcv <- adni.survival.cn %>%
  filter(!is.na(hcv_z))
my_surv_object_cn_hcv <- with(adni.survival.cn.hcv, Surv(time_in_adni, event))

adni.survival.mci.hcv <- adni.survival.mci %>%
  filter(!is.na(hcv_z))
my_surv_object_mci_hcv <- with(adni.survival.mci.hcv, Surv(time_in_adni, event))



foo.ind <- tribble(
  ~name,             ~funcs, ~models_cn_apoe,                                                                                   
                             ~models_mci_apoe,                                                                                   
                             ~models_cn_csf,                                                                                               
                             ~models_mci_csf,                                                                                               
                             ~models_cn_ent,                                                                                          
                             ~models_mci_ent,                                                                                           
                             ~models_cn_hcv,                                                                                   
                             ~models_mci_hcv,                                                                                               
  "biomarker",      'coxph', formula = my_surv_object_cn_apoe  ~  agebl + ptsex + pteducat + apoe4, 
                             formula = my_surv_object_mci_apoe ~  agebl + ptsex + pteducat + apoe4, 
                             formula = my_surv_object_cn_csf   ~  agebl + ptsex + pteducat + cluster_mixtmodel, 
                             formula = my_surv_object_mci_csf  ~  agebl + ptsex + pteducat + cluster_mixtmodel, 
                             formula = my_surv_object_cn_ent   ~  agebl + ptsex + pteducat + entorhinal_z, 
                             formula = my_surv_object_mci_ent  ~  agebl + ptsex + pteducat + entorhinal_z,  
                             formula = my_surv_object_cn_hcv   ~  agebl + ptsex + pteducat + hcv_z, 
                             formula = my_surv_object_mci_hcv  ~  agebl + ptsex + pteducat + hcv_z,         
  "mem",            'coxph', formula = my_surv_object_cn_apoe  ~ mem_q2  + mem_q3  + mem_q4  + agebl + ptsex + pteducat,         
                             formula = my_surv_object_mci_apoe ~ mem_q2  + mem_q3  + mem_q4  + agebl + ptsex + pteducat,         
                             formula = my_surv_object_cn_csf   ~ mem_q2  + mem_q3  + mem_q4  + agebl + ptsex + pteducat,                     
                             formula = my_surv_object_mci_csf  ~ mem_q2  + mem_q3  + mem_q4  + agebl + ptsex + pteducat,                     
                             formula = my_surv_object_cn_ent   ~ mem_q2  + mem_q3  + mem_q4  + agebl + ptsex + pteducat,                
                             formula = my_surv_object_mci_ent  ~ mem_q2  + mem_q3  + mem_q4  + agebl + ptsex + pteducat,                 
                             formula = my_surv_object_cn_hcv   ~ mem_q2  + mem_q3  + mem_q4  + agebl + ptsex + pteducat,         
                             formula = my_surv_object_mci_hcv  ~ mem_q2  + mem_q3  + mem_q4  + agebl + ptsex + pteducat,                                   
  "ef",             'coxph', formula = my_surv_object_cn_apoe  ~ ef_q2   + ef_q3   + ef_q4   + agebl + ptsex + pteducat,         
                             formula = my_surv_object_mci_apoe ~ ef_q2   + ef_q3   + ef_q4   + agebl + ptsex + pteducat,         
                             formula = my_surv_object_cn_csf   ~ ef_q2   + ef_q3   + ef_q4   + agebl + ptsex + pteducat,                     
                             formula = my_surv_object_mci_csf  ~ ef_q2   + ef_q3   + ef_q4   + agebl + ptsex + pteducat,                     
                             formula = my_surv_object_cn_ent   ~ ef_q2   + ef_q3   + ef_q4   + agebl + ptsex + pteducat,                
                             formula = my_surv_object_mci_ent  ~ ef_q2   + ef_q3   + ef_q4   + agebl + ptsex + pteducat,                 
                             formula = my_surv_object_cn_hcv   ~ ef_q2   + ef_q3   + ef_q4   + agebl + ptsex + pteducat,         
                             formula = my_surv_object_mci_hcv  ~ ef_q2   + ef_q3   + ef_q4   + agebl + ptsex + pteducat,                                  
  "lang",           'coxph', formula = my_surv_object_cn_apoe  ~ lang_q2 + lang_q3 + lang_q4 + agebl + ptsex + pteducat,         
                             formula = my_surv_object_mci_apoe ~ lang_q2 + lang_q3 + lang_q4 + agebl + ptsex + pteducat,         
                             formula = my_surv_object_cn_csf   ~ lang_q2 + lang_q3 + lang_q4 + agebl + ptsex + pteducat,                     
                             formula = my_surv_object_mci_csf  ~ lang_q2 + lang_q3 + lang_q4 + agebl + ptsex + pteducat,                     
                             formula = my_surv_object_cn_ent   ~ lang_q2 + lang_q3 + lang_q4 + agebl + ptsex + pteducat,                
                             formula = my_surv_object_mci_ent  ~ lang_q2 + lang_q3 + lang_q4 + agebl + ptsex + pteducat,                 
                             formula = my_surv_object_cn_hcv   ~ lang_q2 + lang_q3 + lang_q4 + agebl + ptsex + pteducat,         
                             formula = my_surv_object_mci_hcv  ~ lang_q2 + lang_q3 + lang_q4 + agebl + ptsex + pteducat,                                  
  "vsp",            'coxph', formula = my_surv_object_cn_apoe  ~ vsp_q2  + vsp_q3  + vsp_q4  + agebl + ptsex + pteducat,         
                             formula = my_surv_object_mci_apoe ~ vsp_q2  + vsp_q3  + vsp_q4  + agebl + ptsex + pteducat,         
                             formula = my_surv_object_cn_csf   ~ vsp_q2  + vsp_q3  + vsp_q4  + agebl + ptsex + pteducat,                     
                             formula = my_surv_object_mci_csf  ~ vsp_q2  + vsp_q3  + vsp_q4  + agebl + ptsex + pteducat,                     
                             formula = my_surv_object_cn_ent   ~ vsp_q2  + vsp_q3  + vsp_q4  + agebl + ptsex + pteducat,                
                             formula = my_surv_object_mci_ent  ~ vsp_q2  + vsp_q3  + vsp_q4  + agebl + ptsex + pteducat,                 
                             formula = my_surv_object_cn_hcv   ~ vsp_q2  + vsp_q3  + vsp_q4  + agebl + ptsex + pteducat,         
                             formula = my_surv_object_mci_hcv  ~ vsp_q2  + vsp_q3  + vsp_q4  + agebl + ptsex + pteducat,                                   
  "mem_biomarker",  'coxph', formula = my_surv_object_cn_apoe  ~ mem_q2  + mem_q3  + mem_q4  + agebl + ptsex + pteducat + apoe4, 
                             formula = my_surv_object_mci_apoe ~ mem_q2  + mem_q3  + mem_q4  + agebl + ptsex + pteducat + apoe4, 
                             formula = my_surv_object_cn_csf   ~ mem_q2  + mem_q3  + mem_q4  + agebl + ptsex + pteducat + cluster_mixtmodel, 
                             formula = my_surv_object_mci_csf  ~ mem_q2  + mem_q3  + mem_q4  + agebl + ptsex + pteducat + cluster_mixtmodel, 
                             formula = my_surv_object_cn_ent   ~ mem_q2  + mem_q3  + mem_q4  + agebl + ptsex + pteducat + entorhinal_z, 
                             formula = my_surv_object_mci_ent  ~ mem_q2  + mem_q3  + mem_q4  + agebl + ptsex + pteducat + entorhinal_z,  
                             formula = my_surv_object_cn_hcv   ~ mem_q2  + mem_q3  + mem_q4  + agebl + ptsex + pteducat + hcv_z, 
                             formula = my_surv_object_mci_hcv  ~ mem_q2  + mem_q3  + mem_q4  + agebl + ptsex + pteducat + hcv_z,        
  "ef_biomarker",   'coxph', formula = my_surv_object_cn_apoe  ~ ef_q2   + ef_q3   + ef_q4   + agebl + ptsex + pteducat + apoe4, 
                             formula = my_surv_object_mci_apoe ~ ef_q2   + ef_q3   + ef_q4   + agebl + ptsex + pteducat + apoe4, 
                             formula = my_surv_object_cn_csf   ~ ef_q2   + ef_q3   + ef_q4   + agebl + ptsex + pteducat + cluster_mixtmodel, 
                             formula = my_surv_object_mci_csf  ~ ef_q2   + ef_q3   + ef_q4   + agebl + ptsex + pteducat + cluster_mixtmodel, 
                             formula = my_surv_object_cn_ent   ~ ef_q2   + ef_q3   + ef_q4   + agebl + ptsex + pteducat + entorhinal_z, 
                             formula = my_surv_object_mci_ent  ~ ef_q2   + ef_q3   + ef_q4   + agebl + ptsex + pteducat + entorhinal_z,  
                             formula = my_surv_object_cn_hcv   ~ ef_q2   + ef_q3   + ef_q4   + agebl + ptsex + pteducat + hcv_z, 
                             formula = my_surv_object_mci_hcv  ~ ef_q2   + ef_q3   + ef_q4   + agebl + ptsex + pteducat + hcv_z,       
  "lang_biomarker", 'coxph', formula = my_surv_object_cn_apoe  ~ lang_q2 + lang_q3 + lang_q4 + agebl + ptsex + pteducat + apoe4, 
                             formula = my_surv_object_mci_apoe ~ lang_q2 + lang_q3 + lang_q4 + agebl + ptsex + pteducat + apoe4, 
                             formula = my_surv_object_cn_csf   ~ lang_q2 + lang_q3 + lang_q4 + agebl + ptsex + pteducat + cluster_mixtmodel, 
                             formula = my_surv_object_mci_csf  ~ lang_q2 + lang_q3 + lang_q4 + agebl + ptsex + pteducat + cluster_mixtmodel, 
                             formula = my_surv_object_cn_ent   ~ lang_q2 + lang_q3 + lang_q4 + agebl + ptsex + pteducat + entorhinal_z, 
                             formula = my_surv_object_mci_ent  ~ lang_q2 + lang_q3 + lang_q4 + agebl + ptsex + pteducat + entorhinal_z,  
                             formula = my_surv_object_cn_hcv   ~ lang_q2 + lang_q3 + lang_q4 + agebl + ptsex + pteducat + hcv_z, 
                             formula = my_surv_object_mci_hcv  ~ lang_q2 + lang_q3 + lang_q4 + agebl + ptsex + pteducat + hcv_z,      
  "vsp_biomarker",  'coxph', formula = my_surv_object_cn_apoe  ~ vsp_q2  + vsp_q3  + vsp_q4  + agebl + ptsex + pteducat + apoe4, 
                             formula = my_surv_object_mci_apoe ~ vsp_q2  + vsp_q3  + vsp_q4  + agebl + ptsex + pteducat + apoe4, 
                             formula = my_surv_object_cn_csf   ~ vsp_q2  + vsp_q3  + vsp_q4  + agebl + ptsex + pteducat + cluster_mixtmodel, 
                             formula = my_surv_object_mci_csf  ~ vsp_q2  + vsp_q3  + vsp_q4  + agebl + ptsex + pteducat + cluster_mixtmodel, 
                             formula = my_surv_object_cn_ent   ~ vsp_q2  + vsp_q3  + vsp_q4  + agebl + ptsex + pteducat + entorhinal_z, 
                             formula = my_surv_object_mci_ent  ~ vsp_q2  + vsp_q3  + vsp_q4  + agebl + ptsex + pteducat + entorhinal_z,  
                             formula = my_surv_object_cn_hcv   ~ vsp_q2  + vsp_q3  + vsp_q4  + agebl + ptsex + pteducat + hcv_z, 
                             formula = my_surv_object_mci_hcv  ~ vsp_q2  + vsp_q3  + vsp_q4  + agebl + ptsex + pteducat + hcv_z,      
)
foo.all <- tribble(
  ~name,             ~funcs,  ~models_cn_apoe,                                                                                                                                                                       
                              ~models_mci_apoe,                                                                                                                                                                      
                              ~models_cn_csf,                                                                                                                                                                                  
                              ~models_mci_csf,                                                                                                                                                                                 
                              ~models_cn_ent,                                                                                                                                                                             
                              ~models_mci_ent,                                                                                                                                                                             
                              ~models_cn_hcv,                                                                                                                                                                       
                              ~models_mci_hcv,   
  "all",             "coxph", formula = my_surv_object_cn_apoe   ~ mem_q2  + mem_q3  + mem_q4  + ef_q2 + ef_q3 + ef_q4 + lang_q2 + lang_q3 + lang_q4 + vsp_q2  + vsp_q3  + vsp_q4 + agebl + ptsex + pteducat,          
                              formula = my_surv_object_mci_apoe  ~ mem_q2  + mem_q3  + mem_q4  + ef_q2 + ef_q3 + ef_q4 + lang_q2 + lang_q3 + lang_q4 + vsp_q2  + vsp_q3  + vsp_q4 + agebl + ptsex + pteducat,         
                              formula = my_surv_object_cn_csf    ~ mem_q2  + mem_q3  + mem_q4  + ef_q2 + ef_q3 + ef_q4 + lang_q2 + lang_q3 + lang_q4 + vsp_q2  + vsp_q3  + vsp_q4 + agebl + ptsex + pteducat,                     
                              formula = my_surv_object_mci_csf   ~ mem_q2  + mem_q3  + mem_q4  + ef_q2 + ef_q3 + ef_q4 + lang_q2 + lang_q3 + lang_q4 + vsp_q2  + vsp_q3  + vsp_q4 + agebl + ptsex + pteducat,                     
                              formula = my_surv_object_cn_ent    ~ mem_q2  + mem_q3  + mem_q4  + ef_q2 + ef_q3 + ef_q4 + lang_q2 + lang_q3 + lang_q4 + vsp_q2  + vsp_q3  + vsp_q4 + agebl + ptsex + pteducat,                
                              formula = my_surv_object_mci_ent   ~ mem_q2  + mem_q3  + mem_q4  + ef_q2 + ef_q3 + ef_q4 + lang_q2 + lang_q3 + lang_q4 + vsp_q2  + vsp_q3  + vsp_q4 + agebl + ptsex + pteducat,                
                              formula = my_surv_object_cn_hcv    ~ mem_q2  + mem_q3  + mem_q4  + ef_q2 + ef_q3 + ef_q4 + lang_q2 + lang_q3 + lang_q4 + vsp_q2  + vsp_q3  + vsp_q4 + agebl + ptsex + pteducat,         
                              formula = my_surv_object_mci_hcv   ~ mem_q2  + mem_q3  + mem_q4  + ef_q2 + ef_q3 + ef_q4 + lang_q2 + lang_q3 + lang_q4 + vsp_q2  + vsp_q3  + vsp_q4 + agebl + ptsex + pteducat,         
  "all_biomarker",   "coxph", formula = my_surv_object_cn_apoe   ~ mem_q2  + mem_q3  + mem_q4  + ef_q2 + ef_q3 + ef_q4 + lang_q2 + lang_q3 + lang_q4 + vsp_q2  + vsp_q3  + vsp_q4 + agebl + ptsex + pteducat + apoe4,  
                              formula = my_surv_object_mci_apoe  ~ mem_q2  + mem_q3  + mem_q4  + ef_q2 + ef_q3 + ef_q4 + lang_q2 + lang_q3 + lang_q4 + vsp_q2  + vsp_q3  + vsp_q4 + agebl + ptsex + pteducat + apoe4, 
                              formula = my_surv_object_cn_csf    ~ mem_q2  + mem_q3  + mem_q4  + ef_q2 + ef_q3 + ef_q4 + lang_q2 + lang_q3 + lang_q4 + vsp_q2  + vsp_q3  + vsp_q4 + agebl + ptsex + pteducat + cluster_mixtmodel, 
                              formula = my_surv_object_mci_csf   ~ mem_q2  + mem_q3  + mem_q4  + ef_q2 + ef_q3 + ef_q4 + lang_q2 + lang_q3 + lang_q4 + vsp_q2  + vsp_q3  + vsp_q4 + agebl + ptsex + pteducat + cluster_mixtmodel, 
                              formula = my_surv_object_cn_ent    ~ mem_q2  + mem_q3  + mem_q4  + ef_q2 + ef_q3 + ef_q4 + lang_q2 + lang_q3 + lang_q4 + vsp_q2  + vsp_q3  + vsp_q4 + agebl + ptsex + pteducat + entorhinal_z, 
                              formula = my_surv_object_mci_ent   ~ mem_q2  + mem_q3  + mem_q4  + ef_q2 + ef_q3 + ef_q4 + lang_q2 + lang_q3 + lang_q4 + vsp_q2  + vsp_q3  + vsp_q4 + agebl + ptsex + pteducat + entorhinal_z, 
                              formula = my_surv_object_cn_hcv    ~ mem_q2  + mem_q3  + mem_q4  + ef_q2 + ef_q3 + ef_q4 + lang_q2 + lang_q3 + lang_q4 + vsp_q2  + vsp_q3  + vsp_q4 + agebl + ptsex + pteducat + hcv_z, 
                              formula = my_surv_object_mci_hcv   ~ mem_q2  + mem_q3  + mem_q4  + ef_q2 + ef_q3 + ef_q4 + lang_q2 + lang_q3 + lang_q4 + vsp_q2  + vsp_q3  + vsp_q4 + agebl + ptsex + pteducat + hcv_z,    
)
foo.data <- tribble(
  ~n, ~data_cn_apoe,         ~data_mci_apoe,         ~data_cn_csf,         ~data_mci_csf,          ~data_cn_ent,         ~data_mci_ent,         ~data_cn_hcv,         ~data_mci_hcv,        
  1,  adni.survival.cn.apoe, adni.survival.mci.apoe, adni.survival.cn.csf, adni.survival.mci.csf,  adni.survival.cn.ent, adni.survival.mci.ent, adni.survival.cn.hcv, adni.survival.mci.hcv,
  
)

foo <- foo.ind %>%
  bind_rows(foo.all) %>%
  mutate(n=1) %>%
  left_join(foo.data, by = "n") %>%
  select(-n)

fitted.cox.models <- foo %>%
  mutate(params_cn_apoe  = pmap(list(models_cn_apoe, data_cn_apoe), list),
         params_mci_apoe = pmap(list(models_mci_apoe, data_mci_apoe), list),
         params_cn_csf   = pmap(list(models_cn_csf, data_cn_csf), list),
         params_mci_csf  = pmap(list(models_mci_csf, data_mci_csf), list),
         params_cn_ent   = pmap(list(models_cn_ent, data_cn_ent), list),
         params_mci_ent  = pmap(list(models_mci_ent, data_mci_ent), list),
         params_cn_hcv   = pmap(list(models_cn_hcv, data_cn_hcv), list),
         params_mci_hcv  = pmap(list(models_mci_hcv, data_mci_hcv), list),
         fit_cn_apoe  = invoke_map(funcs, params_cn_apoe),
         fit_mci_apoe = invoke_map(funcs, params_mci_apoe),
         fit_cn_csf   = invoke_map(funcs, params_cn_csf),
         fit_mci_csf  = invoke_map(funcs, params_mci_csf),
         fit_cn_ent   = invoke_map(funcs, params_cn_ent),
         fit_mci_ent  = invoke_map(funcs, params_mci_ent),
         fit_cn_hcv   = invoke_map(funcs, params_cn_hcv),
         fit_mci_hcv  = invoke_map(funcs, params_mci_hcv),
         tidy_cn_apoe  = map(fit_cn_apoe, tidy, exp = TRUE),
         tidy_mci_apoe = map(fit_mci_apoe, tidy, exp = TRUE),
         tidy_cn_csf   = map(fit_cn_csf, tidy, exp = TRUE),
         tidy_mci_csf  = map(fit_mci_csf, tidy, exp = TRUE),
         tidy_cn_ent   = map(fit_cn_ent, tidy, exp = TRUE),
         tidy_mci_ent  = map(fit_mci_ent, tidy, exp = TRUE),
         tidy_cn_hcv   = map(fit_cn_hcv, tidy, exp = TRUE),
         tidy_mci_hcv  = map(fit_mci_hcv, tidy, exp = TRUE),
         glance_cn_apoe  = map(fit_cn_apoe, glance),
         glance_mci_apoe = map(fit_mci_apoe, glance),
         glance_cn_csf   = map(fit_cn_csf, glance),
         glance_mci_csf  = map(fit_mci_csf, glance),
         glance_cn_ent   = map(fit_cn_ent, glance),
         glance_mci_ent  = map(fit_mci_ent, glance),
         glance_cn_hcv   = map(fit_cn_hcv, glance),
         glance_mci_hcv  = map(fit_mci_hcv, glance))

saveRDS(fitted.cox.models,   file=path(r_objects_folder, "155_fitted_cox_models.rds"))  

fitted.cox.models %>%
  select(name, tidy_cn_apoe) %>%
  unnest(c(tidy_cn_apoe)) %>%
  select(term, name, estimate, p.value, conf.low, conf.high) %>%
  gather(-c(1:2), key = "statistic", value = "value") %>%
  unite("xyz", c(statistic, name)) %>%
  spread(key = xyz, value = value)

fitted.cox.models %>%
  select(name, glance_cn_apoe) %>%
  unnest(c(glance_cn_apoe)) %>%
  select(name, n)

