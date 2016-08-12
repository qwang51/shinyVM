setwd('~/Github/shinyVM/project/sr28docs')
data_dir <- "data"


sr28_tables <- list(
  NUT_DATA = list(
    title="Nutrient Data",
    column_types=c(
      food_id="text",
      nutrient_id="text",	# Nutrient Descriptions table
      nutrient_val = 'double',
      nutrient_data_pts = 'double',
      std_error = 'double',
      src_code = 'text',
      derivation_code = 'text',
      ref_ndb_no = 'text',
      add_nutr_mark = 'text',
      num_studies = 'integer',
      min = 'double',
      max = 'double',
      df = 'integer',
      low_eb = 'double',
      up_eb = 'double',
      stat_cmt = 'text',
      addMod_date = 'text',
      CC = 'text'
    ),
    sep="^"
  ),
  WEIGHT = list(
    title="Food Weights",
    column_types=c(
      food_id="text",	
      seq_num="text",
      amount = 'double',
      measure_desc = 'text',
      gram_weight="double",
      num_data_pts = 'integer', # number of data points
      std_dev = 'double' # standard deviation
    ),
    sep="^"
  ),
  FOOD_DES = list(
    title="Food Descriptions",
    column_types=c(
      food_id="text", 
      fdGrp_id = 'text',
      long_food_desc="text", 
      short_food_desc="text",
      common_name = 'text',
      manufacture_name = 'text',
      survey = 'character',
      refuse_desc = 'integer', # Description of inedible parts of a food item (refuse),
                            # such as seeds or bone.
      refuse = 'double',
      scientific_name = 'text',
      nitrogen2protein_factor = 'double',
      cal2protein_factor = 'double',
      cal2fat_factor = 'double',
      cal2carb_factor = 'double'
    ),
    sep="^"
  ),
  NUTR_DEF = list(
    title="Nutrient Definition",
    column_types=c(
      nutrient_id="text",
      unit="text",
      tagname="text",
      nutrient_description="text",
      decimals="integer",	# decimal places
      sr_order = 'integer' # sort nutrient records in the same order as various reports produced from SR
    ),
    sep="^"
  ),
  FD_GROUP = list(
    title = 'Food Group',
    column_types=c(
      fdGrp_id = 'text',
      fdGrp_desc = 'text'
    ),
    sep = "^"
  )
)


# flat file to a data frame: call for each table
assign_data_frame <- function(tbl_name){
  tbl <- read.table(
    file.path(data_dir, paste0(tbl_name, ".txt")), 
    sep="^",
    quote="~",
    stringsAsFactors=FALSE)
  names(tbl) <- names(sr28_tables[[tbl_name]][["column_types"]])
  assign(tbl_name, tbl, envir = .GlobalEnv)
}

for (tbl in c("NUTR_DEF", "FOOD_DES", "WEIGHT", "NUT_DATA", "FD_GROUP"))
  assign_data_frame(tbl)


NUTR_DEF$nutrient_description[5] <- 'Energy_kCal'
NUTR_DEF$nutrient_description[17] <- 'Energy_kJ'


library(sqldf)
library(tidyr)
library(dplyr)
long_food_nutrients <- sqldf("SELECT f.food_id, nd.nutrient_description, nv.nutrient_val
                             FROM FOOD_DES f 
                             INNER JOIN NUT_DATA nv ON f.food_id = nv.food_id
                             INNER JOIN NUTR_DEF nd ON nv.nutrient_id = nd.nutrient_id") 

food_group_desc <- sqldf("SELECT f.food_id, f.long_food_desc, fd.fdGrp_desc
                             FROM FOOD_DES f 
                             INNER JOIN FD_GROUP fd ON f.fdGrp_id = fd.fdGrp_id") 

nutrient_food_df <- spread(long_food_nutrients, food_id, nutrient_val, fill = 0)
food_nutrient_mat <- t(as.matrix(nutrient_food_df[-1]))
colnames(food_nutrient_mat) <- nutrient_food_df$nutrient_description
food_nutrient_df <- as.data.frame(food_nutrient_mat)
food_nutrient_df <- cbind(food_id = rownames(food_nutrient_df), food_nutrient_df)
rownames(food_nutrient_df) <- 1:nrow(food_nutrient_df)


food_group <- sqldf("SELECT f.food_id, f.long_food_desc AS food_desc, fd.fdGrp_desc AS food_group
                      FROM FOOD_DES f
                      INNER JOIN FD_GROUP fd ON f.fdGrp_id = fd.fdGrp_id")

# Final table
food_df <- sqldf("SELECT fn.*, fd.food_desc, fd.food_group
                FROM food_nutrient_df fn
                INNER JOIN food_group fd ON fn.food_id = fd.food_id")
food_df$omega_3 <- rowSums(food_df[,grep('n-3', names(food_df))])
food_df$omega_6 <- rowSums(food_df[,grep('n-6', names(food_df))])


# Create sub_nutrients tables
Proximates <- c('food_id', 'food_desc', 'food_group', 
                'Protein', 'Adjusted Protein', 'Total lipid (fat)', 'Ash', 'Water',
                'Carbohydrate, by difference', 'Fiber, total dietary', 
                'Sugars, total', 'Starch', 'Energy_kCal', 'Energy_kJ')
Minerals <- c('food_id', 'food_desc', 'food_group',
              'Calcium, Ca', 'Iron, Fe', 'Magnesium, Mg', 'Phosphorus, P',
              'Potassium, K', 'Sodium, Na', 'Zinc, Zn', 'Copper, Cu',
              'Selenium, Se', 'Fluoride, F', 'Manganese, Mn')
Vitamins <- c('food_id', 'food_desc', 'food_group',
              'Vitamin C, total ascorbic acid', 'Thiamin', 'Riboflavin',
              'Niacin', 'Pantothenic acid', 'Vitamin B-6', 'Vitamin B-12',
              'Vitamin B-12, added', 'Folate, total', 'Folate, DFE',
              'Choline, total', 'Betaine', 'Vitamin A, RAE',
              'Vitamin A, IU', 'Carotene, alpha', 'Carotene, beta',
              'Vitamin E (alpha-tocopherol)', 'Vitamin E, added', 
              'Vitamin D', 'Vitamin D (D2 + D3)', 'Menaquinone-4',
              'Dihydrophylloquinone', 'Vitamin K (phylloquinone)')
Lipid.Components <- c('food_id', 'food_desc', 'food_group', 
                      'omega_3', 'omega_6',
                      'Fatty acids, total monounsaturated', 'Fatty acids, total polyunsaturated', 
                      'Fatty acids, total saturated', 'Fatty acids, total trans',
                      'Fatty acids, total trans-monoenoic', 'Fatty acids, total trans-polyenoic',
                      'Cholesterol', 'Phytosterols')

Amino.Acids <- c('food_id', 'food_desc', 'food_group',
                 'Tryptophan', 'Methionine', 'Cystine')

proximates <- food_df[,Proximates]
minerals <- food_df[,Minerals]
vitamins <- food_df[,Vitamins]
lipid.components <- food_df[,Lipid.Components]
amino.acids <- food_df[,Amino.Acids]
foods <- cbind(proximates, minerals[-c(1,2,3)], vitamins[-c(1,2,3)], 
              lipid.components[-c(1,2,3)], amino.acids[-c(1,2,3)])

names(proximates) <- make.names(names(proximates), unique = TRUE)
names(minerals) <- make.names(names(minerals), unique = TRUE)
names(vitamins) <- make.names(names(vitamins), unique = TRUE)
names(lipid.components) <- make.names(names(lipid.components), unique = TRUE)
names(amino.acids) <- make.names(names(amino.acids), unique = TRUE)
names(foods) <- make.names(names(foods), unique = TRUE)

# Create Nutrient table
sub.nutr.def <- NUTR_DEF[NUTR_DEF$nutrient_description %in% 
                c(Proximates[-c(1,2,3)], Minerals[-c(1,2,3)], 
                  Vitamins[-c(1,2,3)], Lipid.Components[-c(1,2,3)],
                  Amino.Acids[-c(1,2,3)]),]
sub.nutr.def <- sub.nutr.def[,c('nutrient_description', 'unit')]
sub.nutr.def <- rbind(sub.nutr.def, data.frame(nutrient_description=c('omega-3','omega-6'), unit=c('g','g')))
# NUTR_DEF[grep('n-3',NUTR_DEF$nutrient_description),]

# Save tables to .rds file
saveRDS(foods, file='~/GitHub/shinyVM/project/sample-apps/data/foods.rds')
saveRDS(proximates, file='~/GitHub/shinyVM/project/sample-apps/data/proximates.rds')
saveRDS(minerals, file='~/GitHub/shinyVM/project/sample-apps/data/minerals.rds')
saveRDS(vitamins, file='~/GitHub/shinyVM/project/sample-apps/data/vitamins.rds')
saveRDS(lipid.components, file='~/GitHub/shinyVM/project/sample-apps/data/lipid.components.rds')
saveRDS(amino.acids, file='~/GitHub/shinyVM/project/sample-apps/data/amino.acids.rds')
saveRDS(WEIGHT, file='~/GitHub/shinyVM/project/sample-apps/data/weight.rds')
saveRDS(sub.nutr.def, file='~/GitHub/shinyVM/project/sample-apps/data/nutr.def.rds')






