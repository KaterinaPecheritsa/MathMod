Задание. Произвести "очистку" таблицы таким образом, что:
  
  
##################################################  
  Все переменные имеют корректный тип данных
Повторяющиеся переменные убраны
Из имен переменных убраны размерности
Всем переменам заданы их реальные размерности
Если какая-то переменная является ошибкой другой переменной, она должна быть убрана и добавлена в виде ошибки к основной переменной
Категориальные переменные должны быть факторами
Категории переменной из имени должны быть убраны
Коды категориальных переменных заменены их категориями
Должны быть созданы переменные координат(lat,lon) в британской системе координат(с учетом кодов квадратов) и в WGS84
Виды должны быть переименованы на латыне
#################################################
install.packages("tideverse")
install.packages("tidyverse")
library(tidyverse)
str(TreeParametr)

library(readr)
TreeParametr <- read_delim("TreeParametr.csv", 
                           ";", escape_double = FALSE, col_types = cols(Tno = col_double(), 
                                                                        `dbh (m)` = col_double(), `Stem diameter Jan 2017 (mm)` = col_character(), 
                                                                        `% Variation` = col_double(), Diference = col_double()), 
                           trim_ws = TRUE)
View(TreeParametr)

library(readr)
TreeParametr <- read_delim("TreeParametr.csv", 
                           ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                           trim_ws = TRUE)
View(TreeParametr)
#Удаление колонки из данных
TreeParametr = TreeParametr %>% select(-`dbh (mm)`, -HR)
#Изменение имени переменной
TreeParametr = TreeParametr %>% rename(dbh = `dbh (m)`)
TreeParametr = TreeParametr %>% rename(Ht = `Ht (m)`)
TreeParametr = TreeParametr %>% rename(Clearance_Ht = `Clearance Ht (m)`)
TreeParametr = TreeParametr %>% rename(Crown_Depth = `Crown Depth (m)`)
TreeParametr = TreeParametr %>% rename(Average_Radial_Crown_spread = `Average Radial Crown spread (m)`)
TreeParametr = TreeParametr %>% rename(Total_Mean_Radial_Crown_Spread = `Total Mean Radial Crown Spread (m)`)
TreeParametr = TreeParametr %>% rename(Crown_Diameter = `Crown Diameter (m)`)
TreeParametr = TreeParametr  %>% rename(Stem_diameter_Jan_2017 = `Stem diameter Jan 2017 (mm)`)
TreeParametr = TreeParametr %>% rename(Annual_Girth_Increment = `Annual Girth Increment (mm)`)
TreeParametr = TreeParametr %>% rename(Two_yr_dia_gain = `2yr dia gain (mm)`)
TreeParametr = TreeParametr %>% rename(Total_NSEW_Radial_Crown_Spread = `Total N,S,E,W Radial Crown Spread (m)`)

units(TreeParametr$dbh) = as_units("m")
TreeParametr %% as.data.frame()
library(units)

units(TreeParametr$Ht) = as_units("m")
units(TreeParametr$Clearance_Ht) = as_units("m")

install.packages("units")
#Добавление размерности переменной
units(TreeParametr$dbh) = as_units("m")
units(TreeParametr$Ht) = as_units("m")
units(TreeParametr$Clearance_Ht) = as_units("m")
units(TreeParametr$Crown_Depth) = as_units("m")
units(TreeParametr$Average_Radial_Crown_spread) = as_units("m")
units(TreeParametr$Total_NSEW_Radial_Crown_Spread) = as_units("m")
units(TreeParametr$Total_Mean_Radial_Crown_Spread) = as_units("m")
units(TreeParametr$Crown_Diameter) = as_units("m")
units(TreeParametr$Stem_diameter_Jan_2017) = as_units("mm")
units(TreeParametr$Two_yr_dia_gain) = as_units("mm")
units(TreeParametr$Annual_Girth_Increment) = as_units("mm")
units(TreeParametr$`Predicted crown diamet using combined formulla`) = as_units("m")
units(TreeParametr$`Predicted crown diamet using combined formulla`) = as_units("m")

TreeParametr %>% as.data.frame()

#переделка переменной в категориальную
#1 - Y, 2 - SM, 3 - EM, 4 - M
TreeParametr = TreeParametr %>% rename(AgeIndex = `Age Index 1=Y 2=SM 3=EM 4=M`)
TreeParametr$AgeIndex = as.factor(TreeParametr$AgeIndex)
levels(TreeParametr$AgeIndex)
TreeParametr %% as.data.frame()
#проверить пустые значения
