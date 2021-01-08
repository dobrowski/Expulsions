

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


### Libraries ---------

library(MCOE)
library(tidyverse)
library(janitor)
library(ggthemes)
library(here)
library(googlesheets4)


### Setup -------

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
#               aggregate_level != "S",
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


gglist2  <-   list(   
    theme_hc() , 
    theme(legend.position = "top"),
    scale_fill_stata() ,
    labs(title =  paste0( "Expulsions over time by District"),
         y = "Total Expulsions",
         x = "Academic Year", 
         fill = "",
         caption = "Source: CDE Downloadable Data Files \n https://www.cde.ca.gov/ds/sd/sd/filesed.asp\nCreated by: David Dobrowski, MCOE") 
)

gglist <- list(gglist2,
               geom_col() 
               )


gglist.district <- list(gglist,
                     facet_wrap(vars(district_name_short)) 
                     )

gglist.school <- list(gglist,
                        facet_wrap(vars(school_name)) 
)

####  Graphs ----------

exp %>%
    filter(reporting_category == "TA",
                     aggregate_level != "C"
    )  %>%
    ggplot( aes(x= academic_year, y = total_expulsions)) + 
    gglist

ggsave(here("figs","Total Expulsions by District.png"), width = 10, height = 8)



exp %>%
    filter(reporting_category %in% c("GM", "GF") ,
           aggregate_level != "C"
    ) %>%
    ggplot( aes(x= academic_year, y = total_expulsions, fill = definition)) + 
    gglist

ggsave(here("figs","Expulsions by Gender by District.png"), width = 10, height = 8)




exp %>%
    filter(str_starts(reporting_category,"R") ,
           aggregate_level != "C"
    ) %>%
    ggplot( aes(x= academic_year, y = total_expulsions, fill = definition)) + 
    gglist

ggsave(here("figs","Expulsions by Race by District.png"), width = 10, height = 8)


###  Dodging -------

exp %>%
    filter(reporting_category %in% c( "SD","TA"),
           aggregate_level != "C"
    )  %>%
    ggplot( aes(x= academic_year, y = total_expulsions, fill = definition, position = "dodge" ) ) +
    gglist2 +
    geom_col(position = "dodge") +
    labs(title =  paste0( "Expulsions over time by District"," - Students with Disabilities") )
         
ggsave(here("figs","Expulsions of Students with Disabilities by District.png"), width = 10, height = 8)


exp %>%
    filter(reporting_category %in% c( "SE","TA"),
           aggregate_level != "C"
    )  %>%
    ggplot( aes(x= academic_year, y = total_expulsions, fill = definition, position = "dodge" ) ) +
    gglist2 +
    geom_col(position = "dodge") +
    labs(title =  paste0( "Expulsions over time by District"," - English Learners") )

ggsave(here("figs","Expulsions of English Learners by District.png"), width = 10, height = 8)


exp %>%
    filter(reporting_category %in% c( "SS","TA"),
           aggregate_level != "C"
    )  %>%
    ggplot( aes(x= academic_year, y = total_expulsions, fill = definition, position = "dodge" ) ) +
    gglist2 +
    geom_col(position = "dodge") +
    labs(title =  paste0( "Expulsions over time by District"," - Socioeconomically Disadvantaged"))

ggsave(here("figs","Expulsions of Socioeconomically Disadvantaged by District.png"), width = 10, height = 8)


###  Reason -------



exp.c <- exp %>%
    filter(reporting_category %in% c( "TA"),
           aggregate_level != "C"
    ) %>%
    pivot_longer(cols = starts_with("expulsion_count")) %>%
    mutate(name = str_remove_all(name,"expulsion_count_")) %>%
    filter( !str_detect(name,"defiance_only") )


ggplot(exp.c, aes(x= academic_year, y= value, fill = name))+
    geom_col() +
    facet_wrap(vars(district_name_short)) +
    theme_hc() +
    theme(legend.position = "top") +
    scale_fill_stata() +
    labs(title =  paste0( "Countywide Expulsions by Reason"),
     y = "Total Expulsions",
     x = "Academic Year", 
     fill = "",
     caption = "Source: CDE Downloadable Data Files \n https://www.cde.ca.gov/ds/sd/sd/filesed.asp\nCreated by: David Dobrowski, MCOE") 


ggsave(here("figs","Countywide Expulsions by Reason.png"), width = 10, height = 8)
ggsave(here("figs","Expulsions by Reason by District.png"), width = 10, height = 8)


#### with functions -----


plot.it.all <- function(df, filename){
    
    title <- "Countywide"
    
    df %>%
        filter(aggregate_level == "C") %>%
        ggplot( aes(x= academic_year, y = total_expulsions, fill = definition)) + 
        gglist + 
        labs(title = paste0("Expulsions", filename, title ))
    
    ggsave(here("figs",paste0("Expulsions", filename, title, ".png" )), width = 10, height = 8)
    
    
    title <- "by District"
    
    df %>%
        filter(aggregate_level %in% c("D1","D") ) %>%
        ggplot( aes(x= academic_year, y = total_expulsions, fill = definition)) + 
        gglist.district+ 
        labs(title = paste0("Expulsions", filename, title ))
    
    
    ggsave(here("figs",paste0("Expulsions", filename, title, ".png" )), width = 10, height = 8)
    
    title <- "by School"
    
    df %>%
        filter(aggregate_level == "S" ,
               total_expulsions > 0) %>%
        ggplot( aes(x= academic_year, y = total_expulsions, fill = definition)) + 
        gglist.school+ 
        labs(title = paste0("Expulsions", filename, title ))
    
    
    ggsave(here("figs",paste0("Expulsions", filename, title, ".png" )), width = 10, height = 8)
    
}


exp %>%
    filter(reporting_category == "TA")  %>%
    plot.it.all(" Overall ")


exp %>%
    filter(reporting_category %in% c("GM", "GF") ) %>%
    plot.it.all(" by Gender ")


exp %>%
    filter(str_starts(reporting_category,"R")   ) %>%
    plot.it.all(" by Race ")




plot.it.all.dodge <- function(df, filename){
    
    title <- "Countywide"
    
    df %>%
        filter(aggregate_level == "C") %>%
        ggplot( aes(x= academic_year, y = total_expulsions, fill = definition)) + 
        gglist2 +
        geom_col(position = "dodge") +
        labs(title = paste0("Expulsions", filename, title ))
    
    ggsave(here("figs",paste0("Expulsions", filename, title, ".png" )), width = 10, height = 8)
    
    
    title <- "by District"
    
    df %>%
        filter(aggregate_level %in% c("D1","D") ) %>%
        ggplot( aes(x= academic_year, y = total_expulsions, fill = definition)) + 
        gglist2 +
        geom_col(position = "dodge") +
        facet_wrap(vars(district_name_short)) +
        labs(title = paste0("Expulsions", filename, title ))
    
    
    ggsave(here("figs",paste0("Expulsions", filename, title, ".png" )), width = 10, height = 8)
    
    title <- "by School"
    
    df %>%
        filter(aggregate_level == "S" ,
               total_expulsions > 0) %>%
        ggplot( aes(x= academic_year, y = total_expulsions, fill = definition)) + 
        gglist2+ 
        geom_col(position = "dodge") +
        facet_wrap(vars(school_name)) +
        labs(title = paste0("Expulsions", filename, title ))
    
     ggsave(here("figs",paste0("Expulsions", filename, title, ".png" )), width = 10, height = 8)
    
}


exp %>%
    filter(reporting_category %in% c( "SD","TA") )  %>%
    plot.it.all.dodge(" of Students with Disabilities ")


exp %>%
    filter(reporting_category %in% c( "SE","TA") )  %>%
    plot.it.all.dodge(" of English Learners ")


exp %>%
    filter(reporting_category %in% c( "SS","TA") )  %>%
    plot.it.all.dodge(" of Socioeconomically Disadvantaged ")




plot.it.all.reason <- function(df, filename){
    
    title <- "Countywide"
    
    df %>%
        filter(aggregate_level == "C") %>%
        ggplot( aes(x= academic_year, y= value, fill = name)) + 
        gglist2 +
        geom_col() +
        labs(title = paste0("Expulsions", filename, title ))
    
    ggsave(here("figs",paste0("Expulsions", filename, title, ".png" )), width = 10, height = 8)
    
    
    title <- "by District"
    
    df %>%
        filter(aggregate_level %in% c("D1","D") ) %>%
        ggplot( aes(x= academic_year, y= value, fill = name)) + 
        gglist2 +
        geom_col() +
        facet_wrap(vars(district_name_short)) +
        labs(title = paste0("Expulsions", filename, title ))
    
    
    ggsave(here("figs",paste0("Expulsions", filename, title, ".png" )), width = 10, height = 8)
    
    title <- "by School"
    
    df %>%
        filter(aggregate_level == "S" ,
               total_expulsions > 0) %>%
        ggplot( aes(x= academic_year, y= value, fill = name)) + 
        gglist2+ 
        geom_col() +
        facet_wrap(vars(school_name)) +
        labs(title = paste0("Expulsions", filename, title ))
    
    ggsave(here("figs",paste0("Expulsions", filename, title, ".png" )), width = 10, height = 8)
    
}


exp %>%
    filter(reporting_category %in% c( "TA")) %>%
    pivot_longer(cols = starts_with("expulsion_count")) %>%
    mutate(name = str_remove_all(name,"expulsion_count_")) %>%
    filter( !str_detect(name,"defiance_only") ) %>%
    plot.it.all.reason(" by Reason ")


