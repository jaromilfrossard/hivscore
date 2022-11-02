rm(list= ls())
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)
library(feather)

library(xml2)
library(XML)

library(hivscore)
library(ggplot2)

hivscore::vacs1%>%
  ggplot(aes(y=vacs1,x=wbc))+
  geom_point()
hivscore::vacs1%>%glimpse()

# hivscore::vacs1%>%
#   writexl::write_xlsx(path="vacs1.xlsx")
# hivscore::vacs2%>%
#   writexl::write_xlsx(path="vacs2.xlsx")

hivscore::vacs1%>%
  filter(vacs1==29)

hivscore::vacs1$cd4_mm3%>%range()

walk(list.files("C:/Users/Jaro/Documents/r_project/trajectoires/function/",full.names = TRUE),source)

source("C:/Users/Jaro/Documents/r_project/validaids/function/read_hicep.R")


list_tb <- read_hicep()

tb_var <- variable_name_shcs()

tb_var%>%
  filter(str_starts(variable,"C"))%>%
  arrange(variable)

tb_var_clinical <- read_shcs_var_clinical()

tb_clinical <- read_shcs_clinical()


tb_dis <- read_shcs_dis()

tb_var_disease <- read_shcs_var_disease()




tb_var_disease%>%
  print.data.frame()




list_tb[sapply(list_tb,ncol)>3]

tibble(table = names(list))

variable_name_shcs()%>%
  arrange(variable)%>%
  View()



ud <-
tb_dis%>%
  select(DISEASE)%>%
  distinct()
  anti_join(tb_var_disease,ud,by = "DISEASE")

tb_var_disease

tb_var_clinical%>%
  filter(CLIN_GROUP=="L")

tb_clinical


tb_lab2 <- read_shcs_lab2()


tb_lab2$ITEM%>%unique()

tb_lab <- read_shcs_lab()


tb_lab%>%
  glimpse()


list_xml <-
  xml%>% xml_find_all('//Entry')%>%
  as_list()


tibble(
  col1 = sapply(list_xml,function(ei){
    if(length(ei$col_1)==0|is.null(ei$col_1)){NA_character_}else{unlist(ei$col_1)}}),
  col2 = sapply(list_xml,function(ei){
    if(length(ei$col_2)==0|is.null(ei$col_2)){NA_character_}else{unlist(ei$col_2)}}))%>%
  filter(str_detect(col2,"cancer"))%>%
  print.data.frame()


sapply(list_xml,function(ei){
  if(length(ei$col_1)==0){NA_character_}else{ei$col_1[[1]]}})%>%
  sapply(FUN=class)%>%
  unique()


unlist(list_xml[[1]]$col_1)

list_xml[[714]]
