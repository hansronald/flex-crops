library(tidyverse)
library(janitor)

active_ingredients = read_csv("/Users/robinlindstrom/Google Drive/Skola/SRC/Thesis/Code/Data/Input/top20_pesticides_active_ingredients_per_crop.csv") %>% 
  clean_names()

str = paste(active_ingredients$top_20_crop_specific_active_ingredients, collapse = ", ")

#str = "glyphosate (HBC), atrazine (HBC), dichloropropene (HBC, NEM), metolachlor(-s) (HBC), chlorothalonil (FUN), chloropicrin (NEM), bacillus amyloliquifacien (FUN), 2,4-d (HBC, PGR), pendimethalin (HBC), metam (FUN, HBC, INS, NEM), acetochlor (HBC), metribuzin (HBC), dicamba (HBC), phorate (INS), chlorpyrifos (ACA, INS), flutolanil (FUN), paraquat (HBC), propazine (HBC), dimethenamid(-p) (HBC, FUN), bromoxynil (HBC)"

#vec = unlist(unlist(strsplit(str, " ()")))
#vec_clean = gsub('^\\(|\\),|\\,|)$', '', vec)

#matrix(vec_clean,nrow = 24,ncol = 2, byrow = TRUE)

separated_AI = as.data.frame(stringr::str_match_all(str, ",?\\s?(.*?)\\s\\((.*?)\\),")[[1]][, -1])

separated_AI %>% 
  as_tibble() %>% 
  count(V1, V2) %>% 
  View()

