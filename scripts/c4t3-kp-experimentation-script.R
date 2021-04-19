### libraries/pkgs #######
# i decided not to use the ggbiplot package b/c it installs plyr which introduces conflicts with dplr unless you install plyr first, apparently. but the plot of PCA components created by ggbiplot was not helpful. so rather than try to deal with those conflicts i'm just not going to use it

library(devtools)
# installed pkg ggbiplot w/ install_github("vqv/ggbiplot") but when i loaded it there were conflicts with dplyr, so i unloaded it w/ detach("package:ggbiplot", unload=TRUE)





#i have identified the wap vars with >= .05 percent unique and i am trying to figure out how to remove the wap vars that fall below that threshold

wifi02 %>% 
  select_if(var("wap001","wap512") >= .05)
#nope


wifi02 %>% 
  mutate(var = var())


################# i'm having problems converting locationid into a factor 

unique(wifi03$locationid)


##### PURRR (REPURRRSIVE) #########
# there are 520 wap vars in the wifi dataset, i need to reduce the number. i created a pipe 

wifi02 %>% 
  select(wap001:wap519) %>% 
  map(c("wap001":"wap519"),var())


# try different strategies to get rid of wap vars with zero variance. i think using select-(which(apply(b0, 2 var)==0)) i think is causing 'na's introducted by coercion'

boa <- b0 %>% 
  select(starts_with("wap"))




b1test <- wifi04 %>% 
  filter(str_detect(locationid, "^1_"))

 
b1test %>% select(-(which(apply(b1, 2, var) == 0)), 
         -c(buildingid, floor, spaceid, relativeposition))


nzvtest <- wifi01 %>% 
  nearZeroVar(saveMetrics = TRUE)

zeros <- which(nzvtest$zeroVar == TRUE)


zwifi02 <- wifi01 %>% 
  select(-zeros)

zwifi03 <- zwifi02 %>%
  unite("locationid", c(buildingid, floor, spaceid, relativeposition))


#--------------------------------------------
zb1 <- zwifi03 %>% 
  filter(str_detect(locationid, "^1_"))


nzv2 <- zb1 %>% 
  nearZeroVar(saveMetrics = TRUE)

zeros2 <- which(nzv2$zeroVar == TRUE)

zb112 <- zb1 %>% 
  select(-all_of(zeros2))



tb0 <- wifi02 %>% 
  filter(buildingid == 0)

names(tb0)

tb01 <- tb0 %>% 
  select(-(which(apply(tb0, 2, var) == 0))) 

tb02 <- tb01 %>% 
  unite("locationid", c(floor, spaceid, relativeposition)) %>% 
  mutate(locationid = as_factor(locationid))




