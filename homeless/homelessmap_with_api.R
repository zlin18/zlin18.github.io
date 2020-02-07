library(sp)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(dplyr)
library(jsonlite)

# Make map
##############################
# This function takes in 3 anc datasets, as well as 39 homelessness service datasets 
# and makes a leaflet interactive map.
##############################
makemap <- function(anc, anc6, anc2, food_groceries0, meals0, SNAP_food_stamps0, 
                    housing0, housing_navigation0, public_restaurooms0, clothing0,
                    refreshments0, showers0, laundry0, haircuts0, employment0,
                    transportation0, phone0, mail0, library0, accessibility0, 
                    adult_literacy0, assessment0, computers0, vocational_training0,
                    art_therapy0, dental_services0, mental_health_services0,
                    HIV_testing0, substance_abuse_treatment0, medical_benefits0,
                    medical_services0, childcare0, case_management0, documentation_assistance0,
                    income_tax_help0, TANF_financial_assistance, domestic_violence_services0,
                    legal_services0, harm_reduction0, storage0, groups0, ministry0){
  homeless_interactive_map_final_with_api <- leaflet()  %>%
    # background
    addProviderTiles(providers$CartoDB.Positron) %>% 
    # ANC borders
    addPolygons(data =anc, fillOpacity = 0.1, weight = 1,
                highlight = highlightOptions(weight = 1.6, fillOpacity = 0, color = "black", opacity = 1.0, sendToBack = TRUE),
                label =  anc$NAME, labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px")) %>%
    addPolygons(data =anc6, fillOpacity = 0.2, weight = 3,
                highlight = highlightOptions(weight = 3, fillOpacity = 0, color = "black", opacity = 1.0, sendToBack = TRUE),
                label =  anc6$NAME, labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px")) %>%
    addPolygons(data =anc2, fillOpacity = 0.2, weight = 3,
                highlight = highlightOptions(weight = 3, fillOpacity = 0, color = "black", opacity = 1.0, sendToBack = TRUE),
                label =  anc6$NAME, labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px")) %>%
    # points
    addCircleMarkers(data = food_groceries0, 
                     label = lapply(create_labs(food_groceries0), htmltools::HTML), 
                     lng = food_groceries0$x, lat = food_groceries0$y,
                     radius = 4, 
                     color = "grey",
                     group = "food_groceries") %>% 
    addCircleMarkers(data = meals0, 
                     label = lapply(create_labs(meals0), htmltools::HTML), 
                     lng = meals0$x, lat = meals0$y,
                     radius = 4, 
                     color = "navy",
                     group = "meals") %>%
    addCircleMarkers(data = SNAP_food_stamps0, 
                     label = lapply(create_labs(SNAP_food_stamps0), htmltools::HTML), 
                     lng = SNAP_food_stamps0$x, lat = SNAP_food_stamps0$y,
                     radius = 4, 
                     color = "purple",
                     group = "SNAP_food_stamps") %>%
    addCircleMarkers(data = housing0, 
                     label = lapply(create_labs(housing0), htmltools::HTML), 
                     lng = housing0$x, lat = housing0$y,
                     radius = 4, 
                     color = "green",
                     group = "housing") %>%
    addCircleMarkers(data = housing_navigation0, 
                     label = lapply(create_labs(housing_navigation0), htmltools::HTML), 
                     lng = housing_navigation0$x, lat = housing_navigation0$y,
                     radius = 4, 
                     color = "orange",
                     group = "housing_navigation") %>%
    addCircleMarkers(data = public_restaurooms0, 
                     label = lapply(create_labs(public_restaurooms0), htmltools::HTML), 
                     lng = public_restaurooms0$x, lat = public_restaurooms0$y,
                     radius = 4, 
                     color = "blue",
                     group = "public_restaurooms") %>%
    addCircleMarkers(data = clothing0, 
                     label = lapply(create_labs(clothing0), htmltools::HTML), 
                     lng = clothing0$x, lat = clothing0$y,
                     radius = 4, 
                     color = "yellow",
                     group = "clothing") %>%
    addCircleMarkers(data = refreshments0, 
                     label = lapply(create_labs(refreshments0), htmltools::HTML), 
                     lng = refreshments0$x, lat = refreshments0$y,
                     radius = 4, 
                     color = "violet",
                     group = "refreshments") %>%
    addCircleMarkers(data = showers0, 
                     label = lapply(create_labs(showers0), htmltools::HTML), 
                     lng = showers0$x, lat = showers0$y,
                     radius = 4, 
                     color = "palegreen",
                     group = "showers") %>%
    addCircleMarkers(data = laundry0, 
                     label = lapply(create_labs(laundry0), htmltools::HTML), 
                     lng = laundry0$x, lat = laundry0$y,
                     radius = 4, 
                     color = "paleturquoise",
                     group = "laundry") %>%
    addCircleMarkers(data = haircuts0, 
                     label = lapply(create_labs(haircuts0), htmltools::HTML), 
                     lng = haircuts0$x, lat = haircuts0$y,
                     radius = 4, 
                     color = "hotpink",
                     group = "haircuts") %>%
    addCircleMarkers(data = employment0, 
                     label = lapply(create_labs(employment0), htmltools::HTML), 
                     lng = employment0$x, lat = employment0$y,
                     radius = 4, 
                     color = "lightskyblue",
                     group = "employment_support") %>%
    addCircleMarkers(data = transportation0, 
                     label = lapply(create_labs(transportation0), htmltools::HTML), 
                     lng = transportation0$x, lat = transportation0$y,
                     radius = 4, 
                     color = "lightseagreen",
                     group = "transportation") %>%
    addCircleMarkers(data = phone0, 
                     label = lapply(create_labs(phone0), htmltools::HTML), 
                     lng = phone0$x, lat = phone0$y,
                     radius = 4, 
                     color = "sienna",
                     group = "phone") %>%
    addCircleMarkers(data = mail0, 
                     label = lapply(create_labs(mail0), htmltools::HTML), 
                     lng = mail0$x, lat = mail0$y,
                     radius = 4, 
                     color = "olivedrab",
                     group = "mail") %>%
    addCircleMarkers(data = library0, 
                     label = lapply(create_labs(library0), htmltools::HTML), 
                     lng = library0$x, lat = library0$y,
                     radius = 4, 
                     color = "tan",
                     group = "library") %>%
    addCircleMarkers(data = accessibility0, 
                     label = lapply(create_labs(accessibility0), htmltools::HTML), 
                     lng = accessibility0$x, lat = accessibility0$y,
                     radius = 4, 
                     color = "orangered",
                     group = "accessibility_services") %>%
    addCircleMarkers(data = adult_literacy0, 
                     label = lapply(create_labs(adult_literacy0), htmltools::HTML), 
                     lng = adult_literacy0$x, lat = adult_literacy0$y,
                     radius = 4, 
                     color = "plum",
                     group = "adult_literacy") %>%
    addCircleMarkers(data = assessment0, 
                     label = lapply(create_labs(assessment0), htmltools::HTML), 
                     lng = assessment0$x, lat = assessment0$y,
                     radius = 4, 
                     color = "darkred",
                     group = "assessment") %>%
    addCircleMarkers(data = computers0, 
                     label = lapply(create_labs(computers0), htmltools::HTML), 
                     lng = computers0$x, lat = computers0$y,
                     radius = 4, 
                     color = "darkcyan",
                     group = "computers") %>%
    addCircleMarkers(data = vocational_training0, 
                     label = lapply(create_labs(vocational_training0), htmltools::HTML), 
                     lng = vocational_training0$x, lat = vocational_training0$y,
                     radius = 4, 
                     color = "chartreuse",
                     group = "vocational_training") %>%
    addCircleMarkers(data = art_therapy0, 
                     label = lapply(create_labs(art_therapy0), htmltools::HTML), 
                     lng = art_therapy0$x, lat = art_therapy0$y,
                     radius = 4, 
                     color = "deeppink",
                     group = "art_therapy") %>%
    addCircleMarkers(data = dental_services0, 
                     label = lapply(create_labs(dental_services0), htmltools::HTML),
                     lng = dental_services0$x, lat = dental_services0$y,
                     radius = 4, 
                     color = "gold",
                     group = "dental_services") %>%
    addCircleMarkers(data = mental_health_services0, 
                     label = lapply(create_labs(mental_health_services0), htmltools::HTML), 
                     lng = mental_health_services0$x, lat = mental_health_services0$y,
                     radius = 4, 
                     color = "cornflowerblue",
                     group = "mental_health_services") %>%
    addCircleMarkers(data = HIV_testing0, 
                     label = lapply(create_labs(HIV_testing0), htmltools::HTML), 
                     lng = HIV_testing0$x, lat = HIV_testing0$y,
                     radius = 4, 
                     color = "thistle",
                     group = "hiv_testing") %>%
    addCircleMarkers(data = substance_abuse_treatment0, 
                     label = lapply(create_labs(substance_abuse_treatment0), htmltools::HTML), 
                     lng = substance_abuse_treatment0$x, lat = substance_abuse_treatment0$y,
                     radius = 4, 
                     color = "wheat",
                     group = "substance_abuse_treatments") %>%
    addCircleMarkers(data = medical_benefits0, 
                     label = lapply(create_labs(medical_benefits0), htmltools::HTML), 
                     lng = medical_benefits0$x, lat = medical_benefits0$y,
                     radius = 4, 
                     color = "peru",
                     group = "medical_benefits") %>%
    addCircleMarkers(data = medical_services0, 
                     label = lapply(create_labs(medical_services0), htmltools::HTML), 
                     lng = medical_services0$x, lat = medical_services0$y,
                     radius = 4, 
                     color = "springgreen",
                     group = "medical_services") %>%
    addCircleMarkers(data = childcare0, 
                     label = lapply(create_labs(childcare0), htmltools::HTML), 
                     lng = childcare0$x, lat = childcare0$y,
                     radius = 4, 
                     color = "magenta",
                     group = "childcare") %>%
    addCircleMarkers(data = case_management0, 
                     label = lapply(create_labs(case_management0), htmltools::HTML), 
                     lng = case_management0$x, lat = case_management0$y,
                     radius = 4, 
                     color = "khaki",
                     group = "case_management") %>%
    addCircleMarkers(data = documentation_assistance0, 
                     label = lapply(create_labs(documentation_assistance0), htmltools::HTML), 
                     lng = documentation_assistance0$x, lat = documentation_assistance0$y,
                     radius = 4, 
                     color = "mediumpurple",
                     group = "documentation_services") %>%
    addCircleMarkers(data = income_tax_help0, 
                     label = lapply(create_labs(income_tax_help0), htmltools::HTML), 
                     lng = income_tax_help0$x, lat = income_tax_help0$y,
                     radius = 4, 
                     color = "firebrick",
                     group = "income_tax_help") %>%
    addCircleMarkers(data = TANF_financial_assistance, 
                     label = lapply(create_labs(TANF_financial_assistance), htmltools::HTML), 
                     lng = TANF_financial_assistance$x, lat = TANF_financial_assistance$y,
                     radius = 4, 
                     color = "coral",
                     group = "tanf_financial_assistance") %>%
    addCircleMarkers(data = domestic_violence_services0, 
                     label = lapply(create_labs(domestic_violence_services0), htmltools::HTML), 
                     lng = domestic_violence_services0$x, lat = domestic_violence_services0$y,
                     radius = 4, 
                     color = "darkorange",
                     group = "domestic_violence_services") %>%
    addCircleMarkers(data = legal_services0, 
                     label = lapply(create_labs(legal_services0), htmltools::HTML), 
                     lng = legal_services0$x, lat = legal_services0$y,
                     radius = 4, 
                     color = "blue3",
                     group = "legal_services") %>%
    addCircleMarkers(data = harm_reduction0, 
                     label = lapply(create_labs(harm_reduction0), htmltools::HTML), 
                     lng = harm_reduction0$x, lat = harm_reduction0$y,
                     radius = 4, 
                     color = "dimgrey",
                     group = "harm_reduction") %>%
    addCircleMarkers(data = storage0, 
                     label = lapply(create_labs(storage0), htmltools::HTML), 
                     lng = storage0$x, lat = storage0$y,
                     radius = 4, 
                     color = "aquamarine",
                     group = "storage") %>%
    addCircleMarkers(data = groups0, 
                     label = lapply(create_labs(groups0), htmltools::HTML), 
                     lng = groups0$x, lat = groups0$y,
                     radius = 4, 
                     color = "palevioletred",
                     group = "groups") %>%
    addCircleMarkers(data = ministry0, 
                     label = lapply(create_labs(ministry0), htmltools::HTML), 
                     lng = ministry0$x, lat = ministry0$y,
                     radius = 4, 
                     color = "yellowgreen",
                     group = "ministry") %>%
    addLayersControl(overlayGroups = c("food_groceries", "meals", "SNAP_food_stamps", "housing", 
                                       "housing_navigation",  "public_restaurooms", "clothing", "refreshments", 
                                       "showers", "laundry", "haircuts", "employment_support", "transportation",
                                       "phone", "mail", "library", "accessibility_services", "adult_literacy",
                                       "assessment", "computers", "vocational_training", "art_therapy", 
                                       "dental_services", "mental_health_services", "hiv_testing", 
                                       "substance_abuse_treatments", "medical_benefits", "medical_services", 
                                       "childcare", "case_management", "documentation_services", "income_tax_help", 
                                       "tanf_financial_assistance", "domestic_violence_services", "legal_services", 
                                       "harm_reduction", "storage", "groups", "ministry"),
                     options = layersControlOptions(collapsed = FALSE))
  
  cat("Success!")
  saveWidget(homeless_interactive_map_final_with_api, file="index.html", selfcontained = FALSE)
  cat("Saved the html map to the current directory.")
  return(homeless_interactive_map_final_with_api)
  
}



# Make the 39 homeless service facilities datasets
##############################
# This function takes in the homeless service facilities dataset 
# requested through the DC open data API, and transforms it into 39 
# sub datasets for later use in the makemap function.
##############################

make39subsets <- function(homeless){
  # get the homeless data frame
  homeless = build_homeless_df(url)
  
  # transform the data frame with extra columns
  homeless = transformer(homeless)
  
  # make the 39 sub sets
  food_groceries0 = homeless %>%
    filter(FOOD_GROCERIES == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  meals0 = homeless %>%
    filter(MEALS == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  SNAP_food_stamps0 = homeless %>%
    filter(SNAP_FOOD_STAMPS == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  housing0 = homeless %>%
    filter(HOUSING == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  housing_navigation0 = homeless %>%
    filter(HOUSING_NAVIGATION == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  public_restaurooms0 = homeless %>%
    filter(PUBLIC_RESTROOMS == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  clothing0 = homeless %>%
    filter(CLOTHING == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  refreshments0 = homeless %>%
    filter(REFRESHMENTS == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  showers0 = homeless %>%
    filter(SHOWERS == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  laundry0 = homeless %>%
    filter(LAUNDRY == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  haircuts0 = homeless %>%
    filter(HAIRCUTS == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  employment0 = homeless %>%
    filter(SUPPORTED_EMPLOYMENT== "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  transportation0 = homeless %>%
    filter(TRANSPORTATION == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  phone0 = homeless %>%
    filter(PHONE == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  mail0 = homeless %>%
    filter(MAIL == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  library0 = homeless %>%
    filter(LIBRARY_CARD == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  accessibility0 = homeless %>%
    filter(ACCESSIBILITY_SERVICES == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  adult_literacy0 = homeless %>%
    filter(ADULT_LITERACY == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  assessment0 = homeless %>%
    filter(ASSESSMENT == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  computers0 = homeless %>%
    filter(COMPUTERS == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  clothing0 = homeless %>%
    filter(CLOTHING == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  vocational_training0 = homeless %>%
    filter(VOCATIONAL_TRAINING == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  art_therapy0 = homeless %>%
    filter(ART_THERAPY == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  dental_services0 = homeless %>%
    filter(DENTAL_SERVICES == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  mental_health_services0 = homeless %>%
    filter(MENTAL_HEALTH == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  HIV_testing0 = homeless %>%
    filter(HIV_TESTING == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  substance_abuse_treatment0 = homeless %>%
    filter(SUBSTANCE_ABUSE_TREATMENT == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  medical_benefits0 = homeless %>%
    filter(MEDICAL_BENEFITS == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  medical_services0 = homeless %>%
    filter(MEDICAL_SERVICES == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  childcare0 = homeless %>%
    filter(CHILD_CARE == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  case_management0 = homeless %>%
    filter(CASE_MANAGEMENT == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  documentation_assistance0 = homeless %>%
    filter(DOCUMENTATION_ASSISTANCE == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  income_tax_help0 = homeless %>%
    filter(INCOME_TAX_HELP == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  TANF_financial_assistance = homeless %>%
    filter(TANF_FINANCIAL_ASSISTANCE == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  domestic_violence_services0 = homeless %>%
    filter(DOMESTIC_VIOLENCE_SERVICES == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  legal_services0 = homeless %>%
    filter(LEGAL_SERVICES == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  harm_reduction0 = homeless %>%
    filter(HARM_REDUCTION == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  storage0  = homeless %>%
    filter(STORAGE == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  groups0  = homeless %>%
    filter(GROUPS == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  ministry0  = homeless %>%
    filter(MINISTRY == "Yes") %>%
    select(c("x","y","OBJECTID","ORGANIZATION_NAME","PROGRAM_NAME","STREET_ADDRESS","CLOTHING","CLIENTS_SERVED_PER_DAY","TARGET","WARD","service_type","info"))
  
  out = list(food_groceries0, meals0, SNAP_food_stamps0, 
             housing0, housing_navigation0, public_restaurooms0, clothing0,
             refreshments0, showers0, laundry0, haircuts0, employment0,
             transportation0, phone0, mail0, library0, accessibility0, 
             adult_literacy0, assessment0, computers0, vocational_training0,
             art_therapy0, dental_services0, mental_health_services0,
             HIV_testing0, substance_abuse_treatment0, medical_benefits0,
             medical_services0, childcare0, case_management0, documentation_assistance0,
             income_tax_help0, TANF_financial_assistance, domestic_violence_services0,
             legal_services0, harm_reduction0, storage0, groups0, ministry0)
  
  return(out)
}


# Dump the JSON data into a dataframe
build_homeless_df <- function(url){
  # fetch JSON data and dump into a dataframe
  D = fromJSON(url)
  homeless = as.data.frame(cbind(as.matrix(D$features$attributes),as.matrix(D$features$geometry)))
  return(homeless)
}


# Dump the JSON ANC data into three data frames
build_anc_dfs <- function(ancurl){
  anc <- rgdal::readOGR(ancurl)
  anc6 <- anc[anc$ANC_ID %in% c("6A", "6B","6C","6D","6E"), ]
  anc2 <- anc[anc$ANC_ID %in% c("2A", "2B","2C","2D","2E","2f"), ]
  output = list(anc, anc6, anc2)
  return(output)
}


# Add info box and service type
#####################################
# This function takes in the homeless data frame, and add some columns to it for map use
#####################################
transformer <- function(homeless){
  
  # Transformer
  x = rep("",115)
  for(i in c(1:115)){
    for(j in c(12:51)){
      if(!is.na(homeless[i,j])){
        x[i]=paste(x[i], colnames(homeless)[j], "<br>")
      } 
    }
  }
  
  homeless$service_type = x
  homeless$info = paste(homeless$ORGANIZATION_NAME, "-",homeless$STREET_ADDRESS, homeless$service_type)
  homeless$info[2]
  return(homeless)
} 
  
  
# create labels for the popup boxes
#######################################
# This function create some labels for the popup boxes in the map
#######################################
create_labs <- function(dataset){
  labs <- lapply(seq(nrow(dataset)), function(i) {
    paste0( '<p>', dataset[i, "ORGANIZATION_NAME"], '</p><p>', 
            dataset[i, "STREET_ADDRESS"],'</p><p>', 
            dataset[i, "service_type"], '</p>' ) 
  })
  return(labs)
}

# Change latitude and longitude to numeric
##############################################
# Helper function that changes latitude and longitude to numeric
##############################################
latlong_numeric <- function(s1){
  s1$x = as.numeric(levels(s1$x))[s1$x]
  s1$y = as.numeric(levels(s1$y))[s1$y]
  return(s1)
}

# Wrapper to apply to all sub sets
##############################################
# Wrap the function above
##############################################
wrapper <- function(sub39_df){
  for(i in c(1:39)){
    sub39_df[[i]] = latlong_numeric(sub39_df[[i]])
  }
  return(sub39_df)
}
  

url = "https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Public_Service_WebMercator/MapServer/6/query?where=1%3D1&outFields=*&outSR=4326&f=json"
ancurl = "https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Administrative_Other_Boundaries_WebMercator/MapServer/1/query?where=1%3D1&outFields=*&outSR=4326&f=json"

ancout = build_anc_dfs(ancurl)
anc = ancout[[1]]
anc6 = ancout[[2]]
anc2 = ancout[[3]]
homelessout = build_homeless_df(url)
sub39 = make39subsets(homelessout)
sub39 = wrapper(sub39)
#s1 = sub39[[1]]

makemap(anc, anc6, anc2, sub39[[1]],sub39[[2]],sub39[[3]],sub39[[4]],sub39[[5]],sub39[[6]],sub39[[7]],sub39[[8]],
        sub39[[9]],sub39[[10]],sub39[[11]],sub39[[12]],sub39[[13]],sub39[[14]],sub39[[15]],sub39[[16]],sub39[[17]],
        sub39[[18]],sub39[[19]],sub39[[20]],sub39[[21]],sub39[[22]],sub39[[23]],sub39[[24]],sub39[[25]],sub39[[26]],
        sub39[[27]],sub39[[28]],sub39[[29]],sub39[[30]],sub39[[31]],sub39[[32]],sub39[[33]],sub39[[34]],sub39[[35]],
        sub39[[36]],sub39[[37]],sub39[[38]],sub39[[39]])











