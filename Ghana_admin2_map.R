
pacman::p_load(tidyverse,tidylog, shiny, flextable, magrittr,tmap,sf,DT)
gh_adm0<-read_sf("Step_up_education_in_Ghana/data/gha_admin_shp/gha_admbnda_adm0_gss_20210308.shp")
gh_adm1<-read_sf("Step_up_education_in_Ghana/data/gha_admin_shp/gha_admbnda_adm1_gss_20210308.shp")
gh_adm2<-read_sf("Step_up_education_in_Ghana/data/gha_admin_shp/gha_admbnda_adm2_gss_20210308.shp")


#load edu facilities
edu_facilities<-read_sf("Step_up_education_in_Ghana/data/hotosm_gha_edu_facilities_points_shp/hotosm_gha_education_facilities_points.shp")

edu_facilities$amenity<-str_to_title(edu_facilities$amenity)

edu_facilities$amenity<-if_else((str_detect(edu_facilities$name,
                                            "Basic|Preparatory|Primary")),"Primary",edu_facilities$amenity)


#try<-read_sf("data/hotosm_gha_education_facilities_gpkg/hotosm_gha_education_facilities.gpkg")
#try<-read_sf("data/hotosm_gha_edu_facilities_points_shp/hotosm_gha_education_facilities_points.shp")

#edu_facilities <-read_sf("data/hotosm_gha_edu_facilities_polygons_shp/hotosm_gha_education_facilities_polygons.shp")


#function to remove nulls in name and amenity in edu_facilities data set
na_name=function(x){
  x|>
    mutate(across(c(amenity),factor))|>
    filter(!is.na(name),
           !is.na(amenity))}

edu_new<-na_name(edu_facilities)
edu_new%<>%
  mutate(across(c(amenity),droplevels))



#left=st_join(polygon, try)
#leftj=st_join(polygon, try, left=T)
#leftj2=st_join(try,polygon, left=T)

#summary(duplicated(edu_new$name))
#edu_new$name[duplicated(edu_new$name)]
#summary(duplicated(edu_n1$name))
#summary(duplicated(edu_n2$name))


#map1=tm_shape(gh_adm2)+tm_polygons(col="ADM1_EN")

#map2=tm_shape(edu_new)+tm_symbols(size=0.1,col="amenity")

#map1+map2
tmap_mode("view")
choose_region=function(x,y){
  filter(x,ADM1_EN==y)
}
 choose_school=function(x){
   filter(edu_new,amenity==x)
   #filter(y) if region was in edu_new data set can filter by coordinate bounding box
 }

choices_reg=factor(gh_adm2$ADM1_EN)


#choose_region("Greater Accra")



#levels(choices_reg)

 #tm_shape(choose_region("Greater Accra"))+tm_polygons(col="ADM1_EN")+
   #tm_shape( choose_school("Primary"))+tm_symbols(size=0.1,col="amenity")
