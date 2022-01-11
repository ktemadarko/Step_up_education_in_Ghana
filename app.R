#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(magrittr)
library(tmap)
library(sf)
library(DT)

gh_adm2<-read_sf("data/gha_admin_shp/gha_admbnda_adm2_gss_20210308.shp")


#load edu facilities
edu_facilities<-read_sf("data/hotosm_gha_edu_facilities_points_shp/hotosm_gha_education_facilities_points.shp")

edu_facilities$amenity<-str_to_title(edu_facilities$amenity)

edu_facilities$amenity<-if_else((str_detect(edu_facilities$name,
                                            "Basic|Preparatory|Primary")),"Primary",edu_facilities$amenity)


na_name=function(x){
  x|>
    mutate(across(c(amenity),factor))|>
    filter(!is.na(name),
           !is.na(amenity))}

edu_new<-na_name(edu_facilities)
edu_new%<>%
  mutate(across(c(amenity),droplevels))


tmap_mode("view")
choose_region=function(x,y){
  filter(x,ADM1_EN==y)
}
choose_school=function(x){
  filter(edu_new,amenity==x)
  #filter(y) if region was in edu_new data set can filter by coordinate bounding box
}

choices_reg=factor(gh_adm2$ADM1_EN)



#load Ghana school attendance data 3 years and above from the Ghana 2021 Census Literacy reports
gh_school_attend=read_csv("data/GHA_2021_Census_Pop_3yrs_school_attendance.csv")

gh_school_attend%<>%
  mutate(Percent_dropout=round(((gh_school_attend$Dropped_out_of_School/
                                   gh_school_attend$`Sum_of_Respondents_(3_years_and_above)`)
                                *100),2),
         Percent_Currently_in_School=round(((gh_school_attend$Currently_Attending_School/
                                               gh_school_attend$`Sum_of_Respondents_(3_years_and_above)`)
                                            *100),2),
         Percent_Never_Attended_School=round(((gh_school_attend$Never_Attended_School/
                                                 gh_school_attend$`Sum_of_Respondents_(3_years_and_above)`)
                                              *100),2))

#rename Region to ADM1_EN so can share common functions
gh_school_attend%<>%
  mutate(across(c(ADM1_EN,Gender,Locality),factor))


# Define UI for application that draws a histogram
ui <- navbarPage("Step up Education in Ghana",

                 # Application title
                 #titlePanel("Distribution of Schools in Ghana"),
                 tabPanel("Educational facilities",
                          # Sidebar with a radio and select input
                          sidebarLayout(
                            sidebarPanel(
                              helpText("Create maps displaying the different types of education facilities in Ghana."),

                              selectInput("region", "Region", choices=levels(choices_reg),selected = "Greater Accra",
                                          multiple=F, selectize=T),
                              radioButtons("school", "School", choices=levels(edu_new$amenity))),
                            mainPanel(tmapOutput("tmap"))
                          )
                 ),
                 tabPanel("School Attendance",

                          sidebarLayout(
                            sidebarPanel(
                              helpText("Choose School attendance indicator by Gender"),
                              selectInput("school_indicator", "School_indicator",
                                          choices=names(gh_school_attend[,c(8:10)]),
                                          selected = "Percent_dropout",multiple=F, selectize=T),
                              radioButtons("locality", "Locality", choices=levels(gh_school_attend$Locality))
                            ),

                            mainPanel(plotOutput("school_attend"))
                          )
                 )
)

#DTOutput("count_table")),

#check plotting all 3 indicators at once
# Show a plot of the generated distribution



# Define server logic required to draw a histogram
server <- function(input, output) {

  output$tmap <- renderTmap({
    # tm_shape(gh_adm0)+tm_borders()+ # the national borders
    map1=tm_shape(choose_region(gh_adm2,input$region))+
      tm_polygons(id="ADM2_EN",col="ADM1_EN",palette="RdPu")#tm_text("ADM1_EN")
    map2=tm_shape(choose_school(input$school))+
      tm_symbols(id="name",size=0.1,col="amenity", palette = "viridis")#tm_text("name")
    map1+map2
  })
  #cant count number of schools in region because edu_new has no region column

  #cntable=function(x){
  #filter(edu_new,)
  #}
  #output$count_table=renderDT()

  #ploting only urban or rural data
  output$school_attend<-renderPlot(

          gh_school_attend |>
              filter(Locality==input$locality) |>
              ggplot( aes_string(x="ADM1_EN", y=input$school_indicator, fill="ADM1_EN"))+
              geom_col(position = position_dodge(0.9))+scale_y_continuous(limits = c(0,80))+
              geom_text(aes_string(label=input$school_indicator), size=3,hjust=-0.3,
                        position = position_dodge(0.9))+
              scale_fill_viridis_d()+coord_flip()+facet_wrap(~Gender, ncol=2)+
              labs(title=paste("A plot showing the distribution of ", input$school_indicator," by ",input$locality))


  )
}

# Run the application
shinyApp(ui = ui, server = server)
