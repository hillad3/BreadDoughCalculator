rm(list = ls())
gc()

library(data.table)
library(stringr)

# this table was copy from https://www.kingarthurbaking.com/learn/ingredient-weight-chart
# and pasted into a Excel, then saved as a CSV
dt <- fread("King Arthur Volumetric Conversions - Original.csv")
setnames(
  dt,
  old = c("Ingredient","Volume","Ounces", "Grams"),
  new = c("ingredient","volume","ounces","grams")
)

# remove non-printables
dt[,volume:=str_replace_all(volume,"\u00ad","")]

# create column to determine escaped unicode characters
dt[,tmp:=stringi::stri_escape_unicode(volume)]

if(length(dt[str_detect(tmp,"u00|uA0"),tmp])>0){
  print(dt[str_detect(tmp,"u00|uA0")])
  stop("There are non-unicode items in the volume column.")
} else {
  dt[,tmp:=NULL]
}

# remove duplicate ingredient
dt <- dt[!(ingredient=="Yeast (instant)" & volume=="2 1/4 teaspoons")]
dt <- dt[!(ingredient=="Yeast (instant)" & volume=="2 teaspoons")]

# clean ingredient descriptions
dt[,volume:=dplyr::case_when(
  volume=="8 tablespoons (1/2 cup)" ~ "8 tablespoons",
  volume=="1 large" & str_detect(ingredient,"Egg") ~ "1 large egg",
  volume=="1 large head" & str_detect(ingredient,"Garlic \\(cloves") ~ "1 large head",
  .default = volume
)]

dt[,ounces:=dplyr::case_when(
  ingredient=="Baking powder" & ounces=="" ~ "1/6",
  ingredient=="Baking soda" & ounces=="" ~ "1/10",
  ingredient=="Lemon juice" & ounces=="" ~ "1/2",
  ingredient=="Pizza Dough Flavor" & ounces=="" ~ "1/4",
  ingredient=="Rye Bread Improver" & ounces=="" ~ "1/2",
  ingredient=="Salt (Kosher, Diamond Crystal)" & ounces=="" ~ "3/4",
  ingredient=="Salt (Kosher, Morton's)" & ounces=="" ~ "3/4",
  ingredient=="Salt (table)" & ounces=="" ~ "3/4",
  .default = ounces
)]

if(length(dt[ounces=="",ounces])>0){
  print(dt[ounces==""])
  stop("There are blanks in the ounces column.")
}

if(length(dt[grams=="",grams])>0){
  print(dt[grams==""])
  stop("There are blanks in the ounces column.")
}

dt[,volume:=ifelse(volume=="1 cup (6 to 7 crackers)","1 cup",volume)]

# convert things that excel converted to month
dt[,ounces:=ifelse(ounces=="2-Jan","1/2",ounces)]
dt[,ounces:=ifelse(ounces=="3-Jan","1/3",ounces)]
dt[,ounces:=ifelse(ounces=="4-Jan","1/4",ounces)]
dt[,ounces:=ifelse(ounces=="5-Jan","1/5",ounces)]
dt[,ounces:=ifelse(ounces=="3-Feb","2/3",ounces)]
dt[,ounces:=ifelse(ounces=="4-Mar","3/4",ounces)]
dt[,ounces:=ifelse(ounces=="8-Mar","3/8",ounces)]
dt[,ounces:=ifelse(ounces=="8-Jul","7/8",ounces)]
dt[,ounces:=ifelse(ounces=="8-May","5/8",ounces)]

if(length(dt[str_detect(ounces,"Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec"),ounces])>0){
  print(dt[str_detect(ounces,"Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec"),ounces])
  stop("There are dates in the ounces column")
}

dt[,default_vol_unit:=str_extract(volume,"cups|cup|teaspoons|teaspoon|tablespoons|tablespoon|large head|large egg")]

if(length(dt[is.na(default_vol_unit),ounces])>0){
  print(dt[is.na(default_vol_unit)])
  stop("There are unknown vol_units in the volume column.")
}

dt[,vol_amt_tmp:=str_remove(volume,paste0(" ",default_vol_unit))]

dt[,default_vol_unit:=ifelse(default_vol_unit=="cups","cup",default_vol_unit)]
dt[,default_vol_unit:=ifelse(default_vol_unit=="teaspoons","teaspoon",default_vol_unit)]
dt[,default_vol_unit:=ifelse(default_vol_unit=="tablespoons","tablespoon",default_vol_unit)]

dt[,vol_conv_factor_to_cups:=dplyr::case_when(
  default_vol_unit=="cup" ~ 1,
  default_vol_unit=="large head" ~ 3,
  default_vol_unit=="large egg" ~ 4,
  default_vol_unit=="tablespoon" ~ 16,
  default_vol_unit=="teaspoon" ~ 48,
  .default = NA_real_
)]

dt[,default_vol_amt:=dplyr::case_when(
  vol_amt_tmp=="1/4" ~ 0.25,
  vol_amt_tmp=="1/3" ~ 1/3,
  vol_amt_tmp=="1/2" ~ 0.5,
  vol_amt_tmp=="1" ~ 1,
  vol_amt_tmp=="2" ~ 2,
  vol_amt_tmp=="4" ~ 4,
  vol_amt_tmp=="8" ~ 8,
  .default = NA_real_
)]

if(length(dt[is.na(default_vol_amt),vol_amt_tmp])>0){
  print(dt[is.na(default_vol_amt),vol_amt_tmp])
  stop("There are unknown ratios as text in the vol_amt column.")
} else {
  dt[,vol_amt_tmp:=NULL]
}

dt[,"ounces_tmp1"] <- str_split_fixed(dt$ounces," to ",2)[,1]
dt[,"ounces_tmp1_1"] <- str_split_fixed(dt$ounces_tmp1," ",2)[,1]
dt[,"ounces_tmp1_2"] <- str_split_fixed(dt$ounces_tmp1," ",2)[,2]

dt[,"ounces_tmp2"] <- str_split_fixed(dt$ounces," to ",2)[,2]
dt[,"ounces_tmp2_1"] <- str_split_fixed(dt$ounces_tmp2," ",2)[,1]
dt[,"ounces_tmp2_2"] <- str_split_fixed(dt$ounces_tmp2," ",2)[,2]


parse_fraction <- function(x){
  if(str_detect(x,"/")){
    y <- as.numeric(str_split_fixed(x,"/",2)[1]) /
      as.numeric(str_split_fixed(x,"/",2)[2])
  } else if(x=="") {
    y <- 0
  } else {
    y <- x
  }
  return(as.numeric(y))
}


dt[,ounces_tmp1_1:=lapply(ounces_tmp1_1, parse_fraction) |> unlist()]
dt[,ounces_tmp1_2:=lapply(ounces_tmp1_2, parse_fraction) |> unlist()]
dt[,ounces_tmp2_1:=lapply(ounces_tmp2_1, parse_fraction) |> unlist()]
dt[,ounces_tmp2_2:=lapply(ounces_tmp2_2, parse_fraction) |> unlist()]
dt[,ounces_tmp1:=ounces_tmp1_1+ounces_tmp1_2]
dt[,ounces_tmp2:=ounces_tmp2_1+ounces_tmp2_2]
dt[,ounces_tmp1_1:=NULL]
dt[,ounces_tmp1_2:=NULL]
dt[,ounces_tmp2_1:=NULL]
dt[,ounces_tmp2_2:=NULL]
dt[,ounces:=ifelse(dplyr::near(ounces_tmp2,0),ounces_tmp1,(ounces_tmp1+ounces_tmp2)/2)]
dt[,ounces_tmp1:=NULL]
dt[,ounces_tmp2:=NULL]

dt[,"grams_tmp1"] <- str_split_fixed(dt$grams," to ",2)[,1]
dt[,"grams_tmp2"] <- str_split_fixed(dt$grams," to ",2)[,2]
dt[,grams:=ifelse(
  grams_tmp2=="",
  as.numeric(grams_tmp1),
  (as.numeric(grams_tmp1)+as.numeric(grams_tmp2))/2)
]
dt[,grams_tmp1:=NULL]
dt[,grams_tmp2:=NULL]
dt[,volume:=NULL]

# dt[,cups:=ifelse(default_vol_unit=="cup",default_vol_amt/default_vol_amt,1)] # not needed
dt[,ounces:=ounces/default_vol_amt*vol_conv_factor_to_cups]
dt[,grams:=grams/default_vol_amt*vol_conv_factor_to_cups]

dt <- melt(dt, measure.vars = c("ounces","grams"))

setnames(dt, old = c("variable","value"), new = c("target_uom", "target_quantity"))

dt[,target_uom:=ifelse(target_uom=="grams","gram","ounce")]

setcolorder(
  dt,c(
    "ingredient",
    "default_vol_amt",
    "default_vol_unit",
    "vol_conv_factor_to_cups",
    "target_uom",
    "target_quantity"
  )
)

setorder(dt, ingredient, target_uom)

# classification tags
dt[,flour_tag:=str_detect(ingredient,regex("flour|cornmeal",ignore_case=TRUE))]
dt[,fats_tag:=str_detect(ingredient,regex("oil|lard|shortening",ignore_case=TRUE))]
dt[,butter_tag:=str_detect(ingredient,regex("butter",ignore_case=TRUE))]
dt[,nut_tag:=str_detect(ingredient,regex("almond|cacao nibs|cashew|grape nut|hazelnut|macadamia nut|marzipan|peanuts|pine nut|pistachio nut|walnut",ignore_case=TRUE))]
dt[,fruit_tag:=str_detect(ingredient,regex("apple|apricot|banana|berries|berry|cherries|cherry|cranberr|dates|fruit|guava|lemon|lime|orange",ignore_case=TRUE))]
dt[,chocolate_tag:=str_detect(ingredient,regex("chocolate|cacao nibs",ignore_case=TRUE))]
dt[,coconut_tag:=str_detect(ingredient,regex("coconut",ignore_case=TRUE))]
dt[,gf_tag:=str_detect(ingredient,regex("gluten-free",ignore_case=TRUE))]
dt[,veg_tag:=str_detect(ingredient,regex("basil|bell pepper|carrot|celery|chives|corn \\(fresh or frozen\\)|garlic|leeks|mushroom|onion|potato|pumpkin|scallion|tomato|zucchini",ignore_case=TRUE))]
dt[,sugar_tag:=str_detect(ingredient,regex("agave|caramel|honey|marshmallow|molasses|sugar",ignore_case=TRUE))]
dt[,dairy_tag:=str_detect(ingredient,regex("butter|cheese|cream|fraiche|milk|yogurt",ignore_case=TRUE))]
dt[,powders_tag:=str_detect(ingredient,regex("baking powder|baking soda|powder",ignore_case=TRUE))]
dt[,egg_tag:=str_detect(ingredient,regex("egg",ignore_case=TRUE))]
dt[,seed_tag:=str_detect(ingredient,regex("seed",ignore_case=TRUE))]
dt[,rice_tag:=str_detect(ingredient,regex("rice",ignore_case=TRUE))]
dt[,salt_tag:=str_detect(ingredient,regex("salt",ignore_case=TRUE))]

fwrite(dt,"King Arthur Volumetric Conversions - Cleaned.csv",)

