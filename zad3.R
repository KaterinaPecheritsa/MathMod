library(tidyverse)
library(readr)
library(sf)
# �������� ������ � �������
dafr = read_csv2("city_trees2.csv")
# ������� �� �������� � �������
a = data.frame(dafr)
a
#������� ����� ������� ������ -- ��� ���� ����������
colnames(a)
# 1. ��� ���������� ����� ���������� ��� ������
# ��
# 2. ������� ������������� ����������
a$HR <- NULL
a$dbh_mm <- NULL
a$X1 <- NULL
# 3. ������ ����������� �� ���� ����������
names(a)[names(a) == "dbh_m"] = "dbh"
# � ��������� ���������� ����������� �� �����������
# 4. ���� ���������� ������ �������� �����������
library(units) #������������� �������������� �����
#������������� ������ ����������� 
units(a$Ht) = as_units("m")
units(a$dbh) = as_units("m")
units(a$Clearance) = as_units("m")
units(a$Crown_Depth) = as_units("m")
units(a$Total_NSEW_Radial_Crown_Spread) = as_units("m")
units(a$Average_Radial_Crown_spread) = as_units("m")
units(a$Crown_Diameter) = as_units("m")
units(a$Predicted_CD_comb_f) = as_units("m")
units(a$Predicted_CD) = as_units("m")
# 5. ���� �����-�� ���������� �������� ������� ������ ����������, ��� ������ ���� ������ � ��������� � ���� ������ � �������� ����������
# ������� ����� ���������� � ����������� �� � ������
a = a %>% mutate(error = Predicted_CD_comb_f - Crown_Diameter)
a = a %>% mutate(error2 = Predicted_CD - Crown_Diameter)
# ������� ������ ��������
a$Diference <- NULL
a$Difference_comb_f <- NULL

# 6, 7, 8.  �������������� ���������� ������ ���� ���������, ��������� ���������� �� ����� ������ ���� ������, ���� �������������� ���������� �������� �� �����������
#library(forcats)
#library(sf)
#a$Data_Set1
#a = a %>%
#  mutate(Data_Set = as.factor("1", "0")) %>%
#  mutate(Data_Set = fct_recode(Data_Set, Norwich = "1", Peterborough = "0"))
#a$Data_Set
#---��� �� ��������. ��������� ������ ������
a$Data_Set[a$Data_Set == "0"] = "Peterborough"
a$Data_Set[a$Data_Set == "1"] = "Norwich"
a$Data_Set
#---� ��� ��� ��������. ������ ����� �� �����, ��������� �� ���� ��� �� :)

# 10. ������� ���� �� ������
# maple - Acer platanoides, 
# Oak - Quercus robur,
# Silver birch - Betula pendula, 
# Sycamore - Platanus occidentalis
a$Species[a$Species == "Oak"] = "Quercus robur"
a$Species[a$Species == "Norway_maple"] = "Acer platanoides"
a$Species[a$Species == "Norway_Maple"] = "Acer platanoides"
a$Species[a$Species == "Silver_Birch"] = "Betula pendula"
a$Species[a$Species == "Sycamore"] = "Platanus occidentalis"

# 9. ������ ���� ������� ���������� ���������(lat,lon) � ���������� ������� ���������(� ������ ����� ���������) � � WGS84
library(stringr)
a$Grid_Reference
coord1 = str_replace_all(a$Grid_Reference, ' ', '')
coord_north = str_trunc(coord1, 12, "right", ellipsis = "") %>% str_trunc(5, "left", ellipsis = "")
coord_north
coord_e = str_trunc(coord1, 7, "right", ellipsis = "") %>% str_trunc(5, "left", ellipsis = "")
quadr = str_trunc(coord1, 2, "right", ellipsis = "")
table = data.frame(as.integer(coord_e), as.integer(coord_north), quadr)
names(table) = c("E", "N", "Quadr")
table = na.exclude(table)
#----------
table = table %>% mutate("Easting_BC" = case_when(
  quadr == "TF" ~ E + 600000,
  quadr == "TG" ~ E + 700000,
  quadr == "TL" ~ E + 600000, 
)) %>% mutate("Northing_BC" = case_when(
  quadr == "TF" ~ N + 300000,
  quadr == "TG" ~ N + 300000,
  quadr == "TL" ~ N + 200000,
))
table = na.exclude(table)
#-----------
#����������� ���������� ��� ��������
table_WGS = table %>% st_as_sf(coords = c("Easting_BC", "Northing_BC"), crs = 27700) %>% st_transform(4326) %>% st_coordinates() %>% as.data.frame()
write.csv2(a, file = "city_trees3.csv")

b = read_csv2("city_trees3.csv")
b2 = data.frame(b)
bp = b %>% filter(
  Species == "Betula pendula"
)
write.csv2(b, file = "bp.csv")

#����� ���
a = a %>% mutate(stem_volume = (3.14*(dbh^2)/4)*Ht) #�������� ����� ������
units(a$stem_volume) = as_units("m^3") #������, ��� ����� ������ ���������� � ����������
#��������� ���� 690 ��/�^3
#��������� ����� 670 ��/�^3
#��������� ������ 650 ��/�^3
#��������� ������� 700 ��/�^3
#�� ���������� https://krovli.club/buildm/plotnost-drevesiny
a = a %>% mutate(biomass = case_when(
  Species == "Quercus robur" ~ stem_volume * 690,
  Species == "Acer platanoides" ~ stem_volume * 670,
  Species == "Betula pendula" ~ stem_volume * 650,
  Species == "Platanus occidentalis" ~ stem_volume * 700,
))
units(a$biomass) = as_units(NULL) #���� ��� ������, ����� �������� �����
units(a$biomass) = as_units("kg")
#��� ���� ��������� �������� ������� ���� �������� � ����� ��� � ����� �������
#����� ����� �������� ������������� ������� ��������� -- ����������

Model_common = lm(data = a, log(as.double(biomass), base = exp(1)) ~ (as.double(Crown_Diameter)))
                                                                           

a = a %>% mutate(Soil_index = case_when(
  Soil_Code == 1 ~ "Sand",
  Soil_Code == 2 ~ "Clay",
  Soil_Code == 3 ~ "Silt",
))
#grafik lineynoy regressii
ggplot(data = a, aes(x = as.double(Crown_Diameter)^0.25, y=log(as.double(biomass), base = exp(1))))+
  geom_point(aes(color=Soil_index))+
  geom_smooth(method = "lm")+
  theme_bw()
#��������� ������������� ������
# ���������� �������� ������
anova(Model_common)

# �������� ������������
summary(Model_common)

# �������� ������������ � ��������� ��������� ����
broom::tidy((Model_common))


biom_mod = function(a){lm(as.double(biomass) ~ as.double(Crown_Diameter), data = a)}

citymodels = a %>% group_by(Soil_index) %>% nest %>% filter(!is.na(Soil_index)) %>%
  mutate(model = data %>% map(biom_mod))  %>%
  mutate(resids = map2(data, model, modelr::add_residuals)) %>%
  mutate(glance = map(model, broom::glance))

results = citymodels %>% unnest(glance)
results %>% filter(r.squared < 0.999) %>% arrange(desc(r.squared)) %>% select(Soil_index, r.squared)
