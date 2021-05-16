## code to prepare `cor_mat` dataset goes here

# Rates taken preferentially from: 
#   1. Early vs expectant cochrane (most similar to control arm)
#   2. https://jamanetwork.com/journals/jama/article-abstract/2676110
#   3. CNN annual report (doesn't report stratified by PDA)
#   
#   # We can ultimately get something close to view of what an adjusted diff
#   might look like by using these values as intercepts with a small number
#   of centered intercepts. Given the only real purpose would be for displaying
#   example results from code we can just skip this part and show example impact
#   on power for a single outcome.

bin_mus <- c(
  "mort" = 0.174, #2
  "ivh_any" = 0.13, #2
  "sev_ivh" = 0.1, #1
  "sepsis" = 0.3, # 4
  "cld" = 0.36, #2
  "steroids" = 0.1, #1
  "pulm_hem" = 0.02, # Assumption
  "pvl" = 0.07, #1
  "nec" = 0.03, #1
  "sip" = 0.03, #1
  "gibleed7days" = 0.1, # Assumption
  "rop3" = 0.197, # 4
  "oliguria" = 0.023, # 1
  "prot_dev" = 0.1, # Assumption
  "rescue" = 0.2, #1
  "procpda" = 0.08, #2
  "any_rx" = 0.5, #1
  "neur_imp_mot" = 0.05, #1
  "neur_imp_cog" = 0.1, #1
  "neur_imp_lan" = 0.1 #1
)

tes <- c(log(0.8), log(0.8), log(0.9), log(0.7), log(0.7), log(1), log(1),
         log(1), log(0.7), log(0.6), log(1), log(1), log(1), log(1), log(1),
         log(1), log(1), log(1), log(1), log(1)
)

cont_mus <- c(
  "dur_mv" = 31.6, #1 
  "dur_rs" = 45.3, #1
  "full_feed" = 12.9, #1
  "reg_bw" = 19, #1
  "dur_hosp" = 80 #1
)

cont_sds <- c(
  "dur_mv" = 61.5, #1
  "dur_rs" = 80.4, #1
  "full_feed" = 7, #1
  "reg_bw" = 6.4, #1
  "dur_hosp" = 20 #1
)

n_trial <- 1350

# Treating these as ORs even though they are really SMDs. Magnitudes are
# about the same

tes <- c(
  mort = log(0.7),
  ivh_any = log(0.7),
  sev_ivh = log(0.8),
  sepsis = log(0.6),
  cld = log(0.8),
  steroids = 0,
  pulm_hem = 0,
  pvl = 0,
  nec = log(0.8),
  sip = log(0.6),
  gibleed7days = 0,
  rop3 = 0,
  oliguria = 0,
  prot_dev = 0,
  rescue = 0,
  procpda = 0,
  any_rx = 0,
  neur_imp_mot = 0,
  neur_imp_cog = 0,
  neur_imp_lan = 0
)

ex_dat <- sim_mv(cor_mat, 
                   bin_mus,
                   cont_mus, 
                   cont_sds, 
                   tes = tes, 
                   n_trial)

cor(ex_dat, method = "spearman")

usethis::use_data(ex_dat, overwrite = TRUE)

