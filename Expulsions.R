

# I contact you with a kind request. Every three years the county office must spearhead an effort to publish a plan for services to expelled students. This plan has three major sections; an analysis of the services provided by all school districts, an analysis of gaps and areas of focus, and a plan to address the gaps. In the introductory section, we must include an analysis of the expulsion data by district. In the past, this data was very basic and retrieved from data quest.
# 
# For this plan, I would like to go deeper into the data and then be able to facilitate conversations with the districts.
# 
# Would you be able to retrieve some data reports for us? This is what we are thinking:
#     
#     1. Number of expelled students per district for the last three years (17-18, 18-19, 19-20).
# 2. For the same periods, disaggregated by grade, gender, ethnicity, EL status, socio-economic status and SPED.
# 
# I'm planning a meeting with districts in February and would like to have the data analysis activity.
# 
# I'm not sure if the data is available so we will take whatever you can produce for us. What do you think? 
# 



library(MCOE)
library(tidyverse)
library(janitor)
library(ggthemes)
library(here)
library(googlesheets4)


con <- mcoe_sql_con()


sheet_id <- "https://docs.google.com/spreadsheets/d/1_EmvLQq-cUe8Ist_WYGJFgdjD27dajpfSbEd_djvbsc/edit#gid=0"


codebook <- read_sheet(sheet_id,
                       col_types = "ccccccD")  %>%
    filter(table == "EXP", 
           field_name == "reporting_category") %>%
    select(reporting_category = variable, definition)



exp <- tbl(con, "EXP") %>% 
        filter(county_code == 27,
               academic_year %in% c("2016-17","2017-18","2018-19"),
               aggregate_level != "S",
               aggregate_level != "D2",  #  D2 seems to exclude charters.  I am including them. 
            is.na(charter_yn) | charter_yn == "All"
               ) %>%
 #   head(20) %>%
    collect() %>% 
    left_join(codebook) %>%
    mutate(district_name_short = str_replace_all(district_name, "Unified", "U") %>%
               str_replace_all("Union", "U") %>%
               str_replace_all("Elementary", "E") %>%
               str_replace_all("Monterey County", "MC") 
           
    )



exp.ta <- exp %>%
    filter(reporting_category == "TA",
 #          aggregate_level != 
               ) %>%
    group_by(academic_year,aggregate_level) %>%
    summarise(sum(total_expulsions))




exp %>%
    filter(reporting_category == "TA",
                     aggregate_level != "C"
    )  %>%
    ggplot( aes(x= academic_year, y = total_expulsions)) + 
    geom_col() + 
    facet_wrap(vars(district_name_short)) + 
    theme_hc() + 
    labs(title =  "Expulsions over time by District",
         y = "Total Expulsions",
         x = "Academic Year", 
         caption = "Source: CDE Downloadable Data Files \n https://www.cde.ca.gov/ds/sd/sd/filesed.asp\nCreated by: David Dobrowski, MCOE")






exp %>%
    filter(reporting_category %in% c("GM", "GF") ,
           aggregate_level != "C"
    ) %>%
    ggplot( aes(x= academic_year, y = total_expulsions, fill = definition)) + 
    geom_col() + 
    facet_wrap(vars(district_name_short)) + 
    theme_hc() + 
    labs(title =  "Expulsions over time by District",
         y = "Total Expulsions",
         x = "Academic Year", 
         caption = "Source: CDE Downloadable Data Files \n https://www.cde.ca.gov/ds/sd/sd/filesed.asp\nCreated by: David Dobrowski, MCOE")



exp %>%
    filter(str_starts(reporting_category,"R") ,
           aggregate_level != "C"
    ) %>%
    ggplot( aes(x= academic_year, y = total_expulsions, fill = definition)) + 
    geom_col() + 
    facet_wrap(vars(district_name_short)) + 
    theme_hc() + 
    labs(title =  "Expulsions over time by District",
         y = "Total Expulsions",
         x = "Academic Year", 
         caption = "Source: CDE Downloadable Data Files \n https://www.cde.ca.gov/ds/sd/sd/filesed.asp\nCreated by: David Dobrowski, MCOE")





