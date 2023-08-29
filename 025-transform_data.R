rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("R", "002-folder_paths_and_options.R"))

# It's input is the following R objects:
adni.retest <- readRDS(file=path(r_objects_folder, "020_adni_retest.rds"))
adni.filtered <- readRDS(file=path(r_objects_folder, "020_adni_filtered.rds"))

# The current analysis only plans on using the first two records for each subject
# If we want to include more records we'll need to think more carefully about how to create those variables
# By design, subjects didn't get cognitive testing at every visit, so these created variables will have missing values because of the missing cognitive testing
adni.recoded <- adni.retest %>%
  group_by(rid) %>%
  arrange(rid, visnum) %>%
  mutate(adni_ef_change = lead(adni_ef) - adni_ef,
         adni_mem_change = lead(adni_mem) - adni_mem,
         VSP_score_change = lead(VSP_score) - VSP_score,
         LAN_score_change = lead(LAN_score) - LAN_score) %>%
  ungroup() %>%
  mutate(adni_ef_change_quantile   = ntile(adni_ef_change, 4),
         adni_mem_change_quantile  = ntile(adni_mem_change, 4),
         VSP_score_change_quantile = ntile(VSP_score_change, 4),
         LAN_score_change_quantile = ntile(LAN_score_change, 4),
         adni_ef_change_quantile   = factor(adni_ef_change_quantile,   levels = c(4:1), labels = c("1 (best)", "2", "3", "4 (worst)"), ordered = TRUE),
         adni_mem_change_quantile  = factor(adni_mem_change_quantile,  levels = c(4:1), labels = c("1 (best)", "2", "3", "4 (worst)"), ordered = TRUE),
         VSP_score_change_quantile = factor(VSP_score_change_quantile, levels = c(4:1), labels = c("1 (best)", "2", "3", "4 (worst)"), ordered = TRUE),
         LAN_score_change_quantile = factor(LAN_score_change_quantile, levels = c(4:1), labels = c("1 (best)", "2", "3", "4 (worst)"), ordered = TRUE))


adni.quantiles <- adni.recoded %>%
  filter(visnum==0) %>%
  filter(bl_dx_factor %in% c("Normal", "MCI")) %>%
  filter(!is.na(adni_mem_change_quantile)) %>%
  filter(!is.na(adni_ef_change_quantile)) %>%
  filter(!is.na(VSP_score_change_quantile)) %>%
  filter(!is.na(LAN_score_change_quantile)) %>%
  select(rid, phase_bl, bl_dx, contains("change_quantile")) %>%
  rename(mem_quantile = adni_mem_change_quantile) %>%
  rename(ef_quantile = adni_ef_change_quantile) %>%
  rename(vsp_quantile = VSP_score_change_quantile) %>%
  rename(lang_quantile = LAN_score_change_quantile) %>%
  mutate(quantile_group = case_when((mem_quantile == "4 (worst)" & ef_quantile < "4 (worst)" & 
                                       vsp_quantile < "4 (worst)" & lang_quantile < "4 (worst)") ~ "Mem only",
                                    (mem_quantile < "4 (worst)" & ef_quantile == "4 (worst)" & 
                                       vsp_quantile < "4 (worst)" & lang_quantile < "4 (worst)") ~ "EF only",
                                    (mem_quantile < "4 (worst)" & ef_quantile < "4 (worst)" & 
                                       vsp_quantile == "4 (worst)" & lang_quantile < "4 (worst)") ~ "VSP only",
                                    (mem_quantile < "4 (worst)" & ef_quantile < "4 (worst)" & 
                                       vsp_quantile < "4 (worst)" & lang_quantile == "4 (worst)") ~ "Lang only",
                                    (mem_quantile == "4 (worst)" & ef_quantile == "4 (worst)" & 
                                       vsp_quantile < "4 (worst)" & lang_quantile < "4 (worst)") ~ "Mem, EF",
                                    (mem_quantile == "4 (worst)" & ef_quantile < "4 (worst)" & 
                                       vsp_quantile == "4 (worst)" & lang_quantile < "4 (worst)") ~ "Mem, VSP",
                                    (mem_quantile == "4 (worst)" & ef_quantile < "4 (worst)" & 
                                       vsp_quantile < "4 (worst)" & lang_quantile == "4 (worst)") ~ "Mem, Lang",
                                    (mem_quantile < "4 (worst)" & ef_quantile == "4 (worst)" & 
                                       vsp_quantile == "4 (worst)" & lang_quantile < "4 (worst)") ~ "EF, VSP",
                                    (mem_quantile < "4 (worst)" & ef_quantile == "4 (worst)" & 
                                       vsp_quantile < "4 (worst)" & lang_quantile == "4 (worst)") ~ "EF, Lang",
                                    (mem_quantile < "4 (worst)" & ef_quantile< "4 (worst)" & 
                                       vsp_quantile == "4 (worst)" & lang_quantile == "4 (worst)") ~ "VSP, Lang",
                                    (mem_quantile == "4 (worst)" & ef_quantile == "4 (worst)" & 
                                       vsp_quantile == "4 (worst)" & lang_quantile < "4 (worst)") ~ "Mem, EF, VSP",
                                    (mem_quantile == "4 (worst)" & ef_quantile == "4 (worst)" & 
                                       vsp_quantile < "4 (worst)" & lang_quantile == "4 (worst)") ~ "Mem, EF, Lang",
                                    (mem_quantile == "4 (worst)" & ef_quantile < "4 (worst)" & 
                                       vsp_quantile == "4 (worst)" & lang_quantile == "4 (worst)") ~ "Mem, VSP, Lang",
                                    (mem_quantile < "4 (worst)" & ef_quantile == "4 (worst)" & 
                                       vsp_quantile == "4 (worst)" & lang_quantile == "4 (worst)") ~ "EF, VSP, Lang",
                                    (mem_quantile == "4 (worst)" & ef_quantile == "4 (worst)" & 
                                       vsp_quantile == "4 (worst)" & lang_quantile == "4 (worst)") ~ "All",
                                    (mem_quantile < "4 (worst)" & ef_quantile < "4 (worst)" & 
                                       vsp_quantile < "4 (worst)" & lang_quantile < "4 (worst)") ~ "None"),
         quantile_group = factor(quantile_group, ordered = TRUE,
                                 levels = c("None",
                                            "Mem only", "EF only", 
                                            "VSP only", "Lang only",
                                            "Mem, EF", "Mem, VSP", 
                                            "Mem, Lang",
                                            "EF, VSP", "EF, Lang",
                                            "VSP, Lang",
                                            "Mem, EF, VSP", 
                                            "Mem, EF, Lang", 
                                            "Mem, VSP, Lang", 
                                            "EF, VSP, Lang",
                                            "All")),
         quantile_group_collapsed = fct_collapse(quantile_group,
                                                 two_impaired_domains = c("Mem, EF",
                                                                          "Mem, VSP",
                                                                          "Mem, Lang",
                                                                          "EF, VSP", 
                                                                          "EF, Lang",
                                                                          "VSP, Lang"),
                                                 three_impaired_domains = c("Mem, EF, VSP",
                                                                            "Mem, EF, Lang",
                                                                            "Mem, VSP, Lang",
                                                                            "EF, VSP, Lang"),
                                                 four_impaired_domains = c("All")))

# Getting the id's used in the analysis so Shannon can match some brain imaging variables
adni.ids <- adni.quantiles %>%
  select(rid)
write_csv(adni.ids, path = path(up_one_level, "adni_rids.csv"))



# Left censoring subjects if they convert at the 6M visit
adni.survival.cn.mci <- adni.filtered %>%
  filter(bl_dx_factor %in% c("Normal", "MCI")) %>%
  group_by(rid) %>%
  mutate(event = case_when(bl_dx==1 & dxcurren %in% c(2, 3)  ~ 1 ,
                           bl_dx==2 & dxcurren==3  ~ 1 ,
                           TRUE ~ 0),
         time_in_adni = lubridate::as.duration(examdate - examdate_bl) / lubridate::dyears(1),
         event_ever = max(event)) %>%
  arrange(rid, desc(event), visnum) %>%
  mutate(n = row_number(),
         first_event = case_when(event==1 & n==1 ~ 1,
                                 TRUE ~ 0),
         first_event_left_censored = case_when(first_event==1 & visnum==6 ~ 1)) %>%
  select(-n) %>%
  arrange(rid, desc(visnum)) %>%
  mutate(n = row_number(),
         censored = case_when(event_ever==0 & n==1 ~ 1,
                                 TRUE ~ 0)) %>%
  fill(first_event_left_censored, .direction = "downup") %>%
  filter(is.na(first_event_left_censored)) %>%
  select(-n) %>%
  arrange(rid, visnum) %>%
  ungroup() %>%
  filter(first_event==1 | censored==1) %>%
  select(rid, bl_dx_factor, time_in_adni, event, phase_bl) 


# Choosing the covariates at the specific visits to use in the analysis
# baseline covariates
adni.bl <- adni.filtered %>%
  filter(visnum==0) %>%
  select(rid, 
         agebl, ptsex, pteducat, pthisp, ptrace3c,
         bl_dx_factor, apoe4, cluster_mixtmodel)

# 6M covariates
adni.6M <- adni.filtered %>%
  filter(visnum == 6) %>%
  select(rid, 
         st24ta_d, st83ta_d, hcv, hcv3) %>%
  mutate(entorhinal = (st24ta_d + st83ta_d)/2,
         entorhinal_z = (entorhinal-mean(entorhinal, na.rm = TRUE))/(2*sd(entorhinal, na.rm = TRUE)),
         hcv_z = (hcv-mean(hcv, na.rm = TRUE))/(2*sd(hcv, na.rm = TRUE)),
         hcv3_z = (hcv3-mean(hcv3, na.rm = TRUE))/(2*sd(hcv3, na.rm = TRUE)))

# Creating the merged data set
# Data set will be nonmissing for retest and also not convert to MCI/AD prior to 6M visit
adni.survival.cn.mci <- adni.survival.cn.mci %>%
  inner_join(adni.quantiles, by = c("rid" = "rid", "phase_bl" = "phase_bl")) %>%
  left_join(adni.bl, by = c("rid" = "rid", "bl_dx_factor" = "bl_dx_factor")) %>%
  left_join(adni.6M, by = c("rid" = "rid"))

# Creating dummy variables for the quantiles
adni.survival.cn.mci <- adni.survival.cn.mci %>%
  mutate(mem_q1 = case_when(mem_quantile=="1 (best)" ~ 1,
                            mem_quantile %in% c("2", "3", "4 (worst)") ~ 0),
         mem_q2 = case_when(mem_quantile=="2" ~ 1,
                            mem_quantile %in% c("1 (best)", "3", "4 (worst)") ~ 0),
         mem_q3 = case_when(mem_quantile=="3" ~ 1,
                            mem_quantile %in% c("1 (best)", "2", "4 (worst)") ~ 0),
         mem_q4 = case_when(mem_quantile=="4 (worst)" ~ 1,
                            mem_quantile %in% c("1 (best)", "2", "3") ~ 0),
         ef_q1 = case_when(ef_quantile=="1 (best)" ~ 1,
                           ef_quantile %in% c("2", "3", "4 (worst)") ~ 0),
         ef_q2 = case_when(ef_quantile=="2" ~ 1,
                           ef_quantile %in% c("1 (best)", "3", "4 (worst)") ~ 0),
         ef_q3 = case_when(ef_quantile=="3" ~ 1,
                           ef_quantile %in% c("1 (best)", "2", "4 (worst)") ~ 0),
         ef_q4 = case_when(ef_quantile=="4 (worst)" ~ 1,
                           ef_quantile %in% c("1 (best)", "2", "3") ~ 0),
         lang_q1 = case_when(lang_quantile=="1 (best)" ~ 1,
                             lang_quantile %in% c("2", "3", "4 (worst)") ~ 0),
         lang_q2 = case_when(lang_quantile=="2" ~ 1,
                             lang_quantile %in% c("1 (best)", "3", "4 (worst)") ~ 0),
         lang_q3 = case_when(lang_quantile=="3" ~ 1,
                             lang_quantile %in% c("1 (best)", "2", "4 (worst)") ~ 0),
         lang_q4 = case_when(lang_quantile=="4 (worst)" ~ 1,
                             lang_quantile %in% c("1 (best)", "2", "3") ~ 0),
         vsp_q1 = case_when(vsp_quantile=="1 (best)" ~ 1,
                            vsp_quantile %in% c("2", "3", "4 (worst)") ~ 0),
         vsp_q2 = case_when(vsp_quantile=="2" ~ 1,
                            vsp_quantile %in% c("1 (best)", "3", "4 (worst)") ~ 0),
         vsp_q3 = case_when(vsp_quantile=="3" ~ 1,
                            vsp_quantile %in% c("1 (best)", "2", "4 (worst)") ~ 0),
         vsp_q4 = case_when(vsp_quantile=="4 (worst)" ~ 1,
                            vsp_quantile %in% c("1 (best)", "2", "3") ~ 0),
         
         g_none = case_when(quantile_group_collapsed=="None" ~ 1,
                            quantile_group_collapsed %in% c("Mem only", "EF only", "VSP only", "Lang only", "two_impaired_domains", "three_impaired_domains", "four_impaired_domains") ~ 0),
         g_mem = case_when(quantile_group_collapsed=="Mem only" ~ 1,
                           quantile_group_collapsed %in% c("None", "EF only", "VSP only", "Lang only", "two_impaired_domains", "three_impaired_domains", "four_impaired_domains") ~ 0),
         g_ef = case_when(quantile_group_collapsed=="EF only" ~ 1,
                          quantile_group_collapsed %in% c("None", "Mem only", "VSP only", "Lang only", "two_impaired_domains", "three_impaired_domains", "four_impaired_domains" ) ~ 0),
         g_vsp = case_when(quantile_group_collapsed=="VSP only" ~ 1,
                           quantile_group_collapsed %in% c("None", "Mem only", "EF only", "Lang only", "two_impaired_domains", "three_impaired_domains", "four_impaired_domains") ~ 0),
         g_lang = case_when(quantile_group_collapsed=="Lang only" ~ 1,
                            quantile_group_collapsed %in% c("None", "Mem only", "EF only", "VSP only", "two_impaired_domains", "three_impaired_domains", "four_impaired_domains") ~ 0),
         g_two = case_when(quantile_group_collapsed=="two_impaired_domains" ~ 1,
                           quantile_group_collapsed %in% c("None", "Mem only", "EF only", "VSP only", "Lang only", "three_impaired_domains", "four_impaired_domains") ~ 0),
         g_three = case_when(quantile_group_collapsed=="three_impaired_domains" ~ 1,
                             quantile_group_collapsed %in% c("None", "Mem only", "EF only", "VSP only", "Lang only", "two_impaired_domains", "four_impaired_domains" ) ~ 0),
         g_four = case_when(quantile_group_collapsed=="four_impaired_domains" ~ 1,
                            quantile_group_collapsed %in% c("None", "Mem only", "EF only", "VSP only", "Lang only", "two_impaired_domains", "three_impaired_domains") ~ 0))

# Separating the data into CN at baseline and MCI at baseline subsets
adni.survival.cn <- adni.survival.cn.mci %>%
  filter(bl_dx_factor %in% c("Normal"))

adni.survival.mci <- adni.survival.cn.mci %>%
  filter(bl_dx_factor %in% c("MCI"))


saveRDS(adni.recoded,   file=path(r_objects_folder, "025_adni_recoded.rds"))  
saveRDS(adni.quantiles,   file=path(r_objects_folder, "025_adni_quantiles.rds"))
saveRDS(adni.survival.cn.mci,   file=path(r_objects_folder, "025_adni_survival_cn_mci.rds"))
saveRDS(adni.survival.cn,   file=path(r_objects_folder, "025_adni_survival_cn.rds"))
saveRDS(adni.survival.mci,   file=path(r_objects_folder, "025_adni_survival_mci.rds"))


