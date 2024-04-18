#!/usr/bin/env Rscript

library(magrittr)
library(tidyverse)
library(DT)
library(here)

#' Format the tibble/table to bold names, link links, add icons, and rearrange/select columns
#' 
#' @description This function uses the CourseName, Funding, CurrentOrFuture, GithubLink, BookdownLink, CourseraLink, and LeanpubLink columns in order to make the first two columns
#' of the outputdf. For the first column of outputdf: CourseName input column value is bolded and icons are added below the name to denote the funding source(s) based on the values in the Funding input column.
#' For the second column of outputdf: If the course is a current course, GithubLink, BookdownLink, CourseraLink, and LeanpubLink are linked to corresponding icons. If either CourseraLink or LeanpubLink are NA values, then those icons/links are not added
#' If the course is a future course, the second column of outputdf instead has an under construction icon.
#' For the third and fourth columns of outputdf: we select the WebsiteDescription and BroadAudience columns. 
#' 
#' @param inputdf the input data frame or tibble
#' @param current a boolean; if TRUE (default), works with current courses; if FALSE, works with future courses by filtering on values in the CurrentOrFuture column
#' @param keep_category a boolean; if FALSE (default), it won't keep the Category column. If TRUE, the Category column is kept after Links but before WebsiteDescription
#' @return outputdf the formatted table 
 

prep_table <- function(inputdf, current=TRUE, keep_category = FALSE){
  if (current){
    outputdf <- inputdf %>%
      filter(CurrentOrFuture == "Current") %>%
      mutate(CourseName = paste0("<b>", CourseName, "</b>")) %>% #mutate the name to be bolded
      mutate(CourseName =
               case_when(
                 Funding == "ITN" ~ paste0(CourseName, '<br></br>', '<img src="resources/images/ITCRLogo.png" height="40"></img>'),
                 Funding == "ITN; Hutch" ~ paste0(CourseName, '<br></br>', '<img src="resources/images/ITCRLogo.png" height="40"></img>', '<br></br>', '<img src="resources/images/fhlogo.png" height="40"></img>')
               )
      ) %>%
      mutate(Links =
               case_when(
                 (!is.na(LeanpubLink) & !is.na(CourseraLink)) ~ paste0('<a href="', GithubLink ,'"style="color: #be3b2a"',' target="_blank"','<div title="Github Source Material Link"> </div>','<img src="resources/images/githublogo.png"  height="30"> </img>', '</a>', '<br></br>',
                                                                       '<a href="', BookdownLink ,'"style="color: #be3b2a"',' target="_blank"','<div title="Bookdown Link"> </div>','<img src="resources/images/bookstack.png"  height="30"> </img>', '</a>', '<br></br>',
                                                                       '<a href="', CourseraLink ,'"style="color: #be3b2a"',' target="_blank"','<div title="Coursera Link"></div>','<img src="resources/images/courseralogo.png" height="30"> </img>', "</a>", '<br></br>',
                                                                       '<a href="', LeanpubLink ,'"style="color: #be3b2a"',' target="_blank"','<div title="Leanpub Link"> </div>','<img src="resources/images/leanpublogo.png"  height="30"> </img>', '</a>'
                 ), #Fill in all 4 logos and links
                 (!is.na(LeanpubLink) & is.na(CourseraLink)) ~ paste0('<a href="', GithubLink ,'"style="color: #be3b2a"',' target="_blank"','<div title="Github Source Material Link"> </div>','<img src="resources/images/githublogo.png"  height="30"> </img>', '</a>', '<br></br>',
                                                                      '<a href="', BookdownLink ,'"style="color: #be3b2a"',' target="_blank"','<div title="Bookdown Link"> </div>','<img src="resources/images/bookstack.png"  height="30"> </img>', '</a>', '<br></br>',
                                                                      '<a href="', LeanpubLink ,'"style="color: #be3b2a"',' target="_blank"','<div title="Leanpub Link"> </div>','<img src="resources/images/leanpublogo.png"  height="30"> </img>', '</a>'
                 ), #Fill in Leanpub, Bookdown, and github logos and links
                 (is.na(LeanpubLink) & !is.na(CourseraLink)) ~ paste0('<a href="', GithubLink ,'"style="color: #be3b2a"',' target="_blank"','<div title="Github Source Material Link"> </div>','<img src="resources/images/githublogo.png"  height="30"> </img>', '</a>', '<br></br>',
                                                                      '<a href="', BookdownLink ,'"style="color: #be3b2a"',' target="_blank"','<div title="Bookdown Link"> </div>','<img src="resources/images/bookstack.png"  height="30"> </img>', '</a>', '<br></br>',
                                                                      '<a href="', CourseraLink ,'"style="color: #be3b2a"',' target="_blank"','<div title="Coursera Link"></div>','<img src="resources/images/courseralogo.png" height="30"> </img>', "</a>"
                 ) #Fill in Coursera, Bookdown, and github logos and links
               )
              )
  } else{
    outputdf <- inputdf %>%
      filter(CurrentOrFuture == "Future") %>% #select just future classes
      mutate(CourseName = paste0("<b>", CourseName, "</b>")) %>% #mutate the name to be bolded
      mutate(CourseName =
               case_when(
                 Funding == "ITN" ~ paste0(CourseName, '<br></br>', '<img src="resources/images/ITCRLogo.png" height="40"></img>'),
                 Funding == "ITN; Hutch" ~ paste0(CourseName, '<br></br>', '<img src="resources/images/ITCRLogo.png" height="40"></img>', '<br></br>', '<img src="resources/images/fhlogo.png" height="40"></img>')
               )
            ) %>%
      mutate(Links = '<img src="resources/images/underconstruction.png" height="40"></img>')
  }
  
  if (keep_category){
    outputdf %<>% select(c(CourseName, Links, Category, WebsiteDescription, BroadAudience))
  } else{
    outputdf %<>% select(c(CourseName, Links, WebsiteDescription, BroadAudience))
  }
  return(outputdf)
}


#' A function to setup the DT datatable
#' 
#' @description
#' A short description...
#' 
#' 
#' @param inputdf input dataframe or tibble to be displayed with the DT library
#' @param some_caption a caption describing the table
#' 
#' @return output_table the DT datatable ready to display version of the inputdf
#' 

setup_table <- function(inputdf, some_caption){
  output_table <- inputdf %>%
    DT::datatable(
      style = 'default',
      width="100%",
      rownames = FALSE,
      escape = FALSE,
      caption = some_caption,
      filter = "top",
      options = list(scrollX = TRUE, autoWidth = TRUE, pageLength = 10,
                     scrollCollapse = TRUE, fillContainer = TRUE,
                     order = (list(0, 'asc')),
                     columnDefs = list(list(className = 'dt-center', targets = "_all")),
                     initComplete = JS(
                       "function(settings, json) {",
                       #"$('body').css({'font-family': 'Calibri'});",
                       "$(this.api().table().header()).css({'backgroundColor': '#3f546f'});",
                       "$(this.api().table().header()).css({'color': '#fff'});",
                       "}"))
    )
  return(output_table)
}
