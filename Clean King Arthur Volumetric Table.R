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
dt[,ingredient:=dplyr::case_when(
  str_detect(ingredient,'"{2,}') ~ str_replace_all(ingredient,'"{2,}',' inch'),
  ingredient=="Egg (fresh)" ~ "Egg (3 tablespoons from 1 large egg; fresh)",
  ingredient=="Egg white (fresh)" ~ "Egg white (2 tablespoons from 1 large egg; fresh)",
  ingredient=="Egg yolk (fresh)" ~ "Egg yolk (1 tablespoons from 1 large egg; fresh)",
  ingredient=="Garlic (cloves, in skin for roasting)" ~ "Garlic (1 large head = 3/4 cup; in skin for roasting)",
  .default = ingredient
)]

# clean volume descriptions
dt[,volume:=dplyr::case_when(
  volume=="8 tablespoons (1/2 cup)" ~ "8 tablespoons",
  ingredient=="Egg (3 tablespoons from 1 large egg; fresh)" ~ "3 tablespoons",
  ingredient=="Egg white (2 tablespoons from 1 large egg; fresh)" ~ "2 tablespoons",
  ingredient=="Egg yolk (1 tablespoons from 1 large egg; fresh)" ~ "1 tablespoons",
  ingredient=="Garlic (1 large head = 3/4 cup; in skin for roasting)" ~ "3/4 cup",
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

dt[,default_vol_uom:=str_extract(volume,"cups|cup|teaspoons|teaspoon|tablespoons|tablespoon")]

if(length(dt[is.na(default_vol_uom),ounces])>0){
  print(dt[is.na(default_vol_uom)])
  stop("There are unknown vol_uoms in the volume column.")
}

dt[,vol_amt_tmp:=str_remove(volume,paste0(" ",default_vol_uom))]

dt[,default_vol_uom:=ifelse(default_vol_uom=="cups","cup",default_vol_uom)]
dt[,default_vol_uom:=ifelse(default_vol_uom=="teaspoons","teaspoon",default_vol_uom)]
dt[,default_vol_uom:=ifelse(default_vol_uom=="tablespoons","tablespoon",default_vol_uom)]

dt[,vol_conv_factor_to_cups:=dplyr::case_when(
  default_vol_uom=="cup" ~ 1,
  default_vol_uom=="tablespoon" ~ 16,
  default_vol_uom=="teaspoon" ~ 48,
  .default = NA_real_
)]

dt[,default_vol_amt:=dplyr::case_when(
  vol_amt_tmp=="1/4" ~ 0.25,
  vol_amt_tmp=="1/3" ~ 1/3,
  vol_amt_tmp=="1/2" ~ 0.5,
  vol_amt_tmp=="3/4" ~ 0.75,
  vol_amt_tmp=="1" ~ 1,
  vol_amt_tmp=="2" ~ 2,
  vol_amt_tmp=="3" ~ 3,
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

# dt[,cups:=ifelse(default_vol_uom=="cup",default_vol_amt/default_vol_amt,1)] # not needed
dt[,ounces:=ounces/default_vol_amt*vol_conv_factor_to_cups]
dt[,grams:=grams/default_vol_amt*vol_conv_factor_to_cups]

dt <- melt(dt, measure.vars = c("ounces","grams"))

setnames(dt, old = c("variable","value"), new = c("target_uom", "target_amt"))

dt[,target_uom:=ifelse(target_uom=="grams","gram","ounce")]

setcolorder(
  dt,c(
    "ingredient",
    "default_vol_amt",
    "default_vol_uom",
    "vol_conv_factor_to_cups",
    "target_uom",
    "target_amt"
  )
)

setorder(dt, ingredient, target_uom)

# classification tags
dt[,baking_tag:=str_detect(ingredient,regex("dough|fiber|clearjel|vanilla extract|water|yeast \\(instant\\)",ignore_case=TRUE))]
dt[,butters_tag:=str_detect(ingredient,regex("butter",ignore_case=TRUE))]
dt[,chocolates_tag:=str_detect(ingredient,regex("chocolate|cacao nibs|cocoa",ignore_case=TRUE))]
dt[,coconuts_tag:=str_detect(ingredient,regex("coconut",ignore_case=TRUE))]
dt[,dairy_tag:=str_detect(ingredient,regex("butter|cheese|cream|fraiche|ghee|milk|queso|yogurt",ignore_case=TRUE))]
dt[,eggs_tag:=str_detect(ingredient,regex("egg",ignore_case=TRUE))]
dt[,fats_tag:=str_detect(ingredient,regex("oil|lard|shortening",ignore_case=TRUE))]
dt[,flours_tag:=str_detect(ingredient,regex("flour|cornmeal|masa harina",ignore_case=TRUE))]
dt[,fruits_tag:=str_detect(ingredient,regex("apple|apricot|banana|berries|berry|cherries|cherry|cranberr|currants|dates|fruit|guava|lemon|lime|orange|peach|^pears|raisins",ignore_case=TRUE))]
dt[,gf_tag:=str_detect(ingredient,regex("gluten-free",ignore_case=TRUE))]
dt[,grains_tag:=str_detect(ingredient,regex("bran|barley|buckwheat|bulgur|corn \\(popped\\)|crumbs|grain blend|grains blend|granola|millet|oats|quinoa|rye|super 10 blend|wheat",ignore_case=TRUE))]
dt[,nuts_tag:=str_detect(ingredient,regex("almond|cacao nibs|cashew|^fig|grape nut|hazelnut|macadamia nut|marzipan|peanuts|pecan|pine nut|pistachio|walnut",ignore_case=TRUE))]
dt[,pizza_tag:=str_detect(ingredient,regex("pizza",ignore_case=TRUE))]
dt[,powders_tag:=str_detect(ingredient,regex("baking powder|baking soda|cornstarch|powder",ignore_case=TRUE))]
dt[,rices_tag:=str_detect(ingredient,regex("rice",ignore_case=TRUE))]
dt[,salts_tag:=str_detect(ingredient,regex("salt",ignore_case=TRUE))]
dt[,sauces_tag:=str_detect(ingredient,regex("mayonnaise|sauce",ignore_case=TRUE))]
dt[,seeds_tag:=str_detect(ingredient,regex("flax|seed|tahini",ignore_case=TRUE))]
dt[,sugars_tag:=str_detect(ingredient,regex("agave|caramel|corn syrup|honey|marshmallow|malt syrup|maple syrup|molasses|sugar|toffee",ignore_case=TRUE))]
dt[,toppings_tag:=str_detect(ingredient,regex("cake enhancer|filling|ginger|jammy bits|preserves|pie filling|seasoning|sweet bits|topping",ignore_case=TRUE))]
dt[,veggies_tag:=str_detect(ingredient,regex("basil|bell pepper|carrot|celery|chives|corn \\(fresh or frozen\\)|garlic|leeks|mushroom|olive|onion|potato|pumpkin|rhubarb|scallion|shallot|tomato|zucchini",ignore_case=TRUE))]

dt[,unassigned_tag:=
     !(baking_tag |
     butters_tag |
     chocolates_tag |
     coconuts_tag |
     dairy_tag |
     eggs_tag |
     fats_tag |
     flours_tag |
     fruits_tag |
     gf_tag |
     grains_tag |
     nuts_tag |
     pizza_tag |
     powders_tag |
     rices_tag |
     salts_tag |
     sauces_tag |
     seeds_tag |
     sugars_tag |
     toppings_tag |
     veggies_tag)
]

fwrite(dt,"King Arthur Volumetric Conversions - Cleaned.csv",)

