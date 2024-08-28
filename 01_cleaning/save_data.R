## データの読み込み

pacman::p_load(tidyverse, haven,fastDummies)

df_house <- read_dta("replication_data/ipums2000h_wacw_albouy.dta")
df_indvi_raw <- read_dta("replication_data/ipums2000p_wacw_albouy.dta")
df_match <- read_dta("replication_data/match_puma2000_pmsa99.dta")
df_4 <- read_dta("replication_data/stcmsa2000_wacw_albouy.dta")


# prepare wage data -------------------------------------------------------



### AGE FROM 25 TO 55, NON-FARM, NON-GROUP QUARTERS
### EMPLOYMENT STATUS: MUST BE EMPLOYED and EARN WAGE, 30+ HOURS, 26+ WEEKS

df_indvi <- 
  df_indvi_raw %>% 
  select(year, statefip, puma, perwt, sex, age, marst,
         race, hispan, educ, empstat, occ1950, ind1950,
         classwkrd, wkswork1, uhrswork, speakeng, yrimmig,
         farm, gq, incwage, vetstat) %>% 
  filter(age>=25&age<=55) %>% 
  filter(farm!=2) %>% 
  filter(!(gq %in% c(3, 4))) %>%
  filter(classwkrd >= 22 & classwkrd <= 28) %>% 
  filter(empstat == 1) %>% 
  filter(uhrswork >=30 & uhrswork <= 99) %>%
  filter(wkswork1 >= 26 & wkswork1 <= 52) %>%
  mutate(annhours = wkswork1 * uhrswork,
         lhrwage = log(incwage/annhours)) %>% 
  group_by(statefip) %>% 
  mutate(stmaxinc = max(incwage)) %>% 
  ungroup() %>% 
  mutate(stmaxwage =stmaxinc/2000) %>% 
  filter(lhrwage >= log(2)) %>%
  filter(lhrwage <= log(2*stmaxinc)) %>% 
  mutate(lhrwage=if_else(exp(lhrwage)>=stmaxinc & exp(lhrwage)<=2*stmaxinc,
                         log(stmaxinc),lhrwage)) %>% 
  select(-annhours)

df_indvi <- 
  df_indvi %>% 
  mutate(schyrs=case_when(educ==0~0,
                          educ==1~2.5,
                          educ==2~6.5,
                          educ==3~9,
                          educ==4~10,
                          educ==5~11,
                          educ==6~12,
                          educ==7~13,
                          educ==8|educ==9~14,
                          educ==10~16,
                          educ==11~18)) %>% 
  mutate(sch_1to4 = if_else(educ==1, 1, 0),
         sch_5to8 = if_else(educ==2, 1, 0),
         sch_9 = if_else(educ==3, 1, 0),
         sch_10 = if_else(educ==4, 1, 0),
         sch_11 = if_else(educ==5, 1, 0),
         sch_12 = if_else(educ==6, 1, 0),
         sch_1col = if_else(educ==7, 1, 0),
         sch_scol = if_else(educ==8|educ==9, 1, 0),
         sch_cdeg = if_else(educ==10, 1, 0),
         sch_post = if_else(educ==11, 1, 0)) %>% 
  mutate(potexp = age - schyrs - 5) %>% 
  mutate(potexp1 = potexp^1,
         potexp2 = potexp^2,
         potexp3 = potexp^3,
         potexp4 = potexp^4) %>% 
  select(-potexp) %>% 
  mutate(expsch = potexp1 * schyrs) %>% 
  mutate(mar_pre = if_else(marst == 1, 1, 0),
         mar_abs = if_else(marst == 2, 1, 0),
         mar_sep = if_else(marst == 3, 1, 0),
         mar_div = if_else(marst == 4, 1, 0),
         mar_wid = if_else(marst == 5, 1, 0)) %>%
  select(-marst) %>% 
  mutate(vet = if_else(vetstat == 2, 1, 0),
         vetage = vet * age) %>% 
  select(-vetstat) %>% 
  mutate(female = if_else(sex==2, 1, 0)) %>% 
  select(-sex) %>% 
  mutate(min_blac = if_else(race == 2,1,0),
         min_nati = if_else(race == 3,1,0),
         min_asia = if_else(race >= 4 & race <= 6, 1,0),
         min_othe = if_else(race >= 7 & race <= 10, 1,0)) %>% 
  select(-race) %>% 
  mutate(min_hisp = if_else(hispan == 0 | hispan == 4, 0, 1)) %>%
  mutate(immig = if_else(yrimmig != 00, 1, 0),
         im_usyrs = (year - yrimmig)*immig) %>% 
  select(-yrimmig) %>% 
  mutate(im_hisp = immig*min_hisp,
         im_black = immig*min_blac,
         im_asian =immig*min_asia,
         im_other = immig*min_othe) %>% 
  mutate(eng_no=if_else(speakeng==1,1,0),
         eng_well=if_else(speakeng==5,1,0),
         eng_bad=if_else(speakeng==6,1,0)) %>% 
  select(-speakeng) %>% 
  mutate(occ=floor(occ1950/100)) %>% 
  select(-occ1950) 
  
df_indvi <- 
  df_indvi %>% 
  fastDummies::dummy_cols("occ") %>% 
  select(-c(occ, occ_0)) %>% 
  mutate(ind=floor(ind1950/100)) %>% 
  select(-ind1950) %>% 
  fastDummies::dummy_cols("ind") %>% 
  select(-ind)


# join data ---------------------------------------------------------------

df_indvi_match <- 
  df_indvi %>% 
  left_join(., df_match, by = c("statefip", "puma"))


  
  
  

  
