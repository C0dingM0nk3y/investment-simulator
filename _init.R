# Dependencies ####
library(shiny)
library(shinythemes)
library(quantmod)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)

# Options ####
options(scipen = 99)

# Functions ####
u <- function(text){ #quick formatting
  underlinedText <- span(style="text-decoration:underline",
                         text)
  return(underlinedText)
}

text_col <- function(text, color="#84b0fa"){ #quick formatting "#375a7f"
  formatted <- span(style=paste0("color:",color),
                    text)
  return(formatted)
}

code_col <- function(text, text_col="#FFFFFF", bg_color="#FFFFFF47"){ #quick formatting "#375a7f"
  formatted <- code(style=paste0("color:", text_col, ";background-color:",bg_color),
                    text)
  return(formatted)
}

span_col <- function(text, text_col="#FFFFFF", bg_color="orange"){ #quick formatting "#375a7f"
  formatted <- span(style=paste0("color:", text_col, 
                                 ";background-color:",bg_color,
                                 ";border-radius:4px;padding:2px 4px;font-size:90%"),
                    text)
  return(formatted)
}

sstrong <- function(text, text_col="orange"){ #quick formatting "#375a7f"
  formatted <- strong(style=paste0("color:", text_col, 
                                   ";font-size:110%"),
                      text)
  return(formatted)
}

#suppress label from shiny elements
labelSuppress <- function(element_id){
  tags$style(
    sprintf("#%s-label {
      display: none;  
    }", element_id)
  )
}

styleEdit <- function(style_element, text){
  tags$style(
    paste0(style_element,"{",text,"}")
  )
}