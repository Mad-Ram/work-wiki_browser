#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(stringdist)
library(lubridate)
library(collapsibleTree)
library(shinyalert)

#ui ====
ui <- dashboardPage(
  skin = 'blue'
  ,header = dashboardHeader(
    title = span(tagList(icon("sitemap"), "Wiki Browser"))
    #,dropdownMenuOutput('kb_links')
  )
  ,sidebar = dashboardSidebar(
    sidebarMenu(id="mytabs",sidebarMenuOutput("menu"))
    ,disable = TRUE  
  )
  ,body = dashboardBody(
    #tags$style(type = "text/css", "#tab_items {height: calc(100vh - 80px) !important;}")
    useShinyalert()
    ,fluidPage(
      fluidRow(
        box(
          title = 'Options and filters'
          ,numericInput('views','filter by # of views',1000)
          ,numericInput('cluster_greediness','cluster greediness (lower for more clusters)',50)
        )
      )
      ,fluidRow(
        box(
          title = 'Wiki Browser'
          ,collapsibleTreeOutput('make_my_tree')
          #,h4('Click on a leaf node to go to the wiki page.')
          ,width = 12
        ) 
      )
    )
  )
)

# server ====
server <- function(input, output, session) {
  
  load('./env_clean.RData')
  rv <- reactiveValues()
  
  output$make_my_tree <- renderCollapsibleTree({
    important_wiki_pages <- MainWikiPage %>% #81k
      filter(deleted != TRUE) %>% #68k
      filter(views > input$views) #451 (> 100 is 4k, do that next)
    
    iw_with_tags_groups <- important_wiki_pages %>% #451 (> 100 is 4k, do that next)
      left_join(MainWikiPage2WikiTag, by = c('id' = 'page_id')) %>% #%>% #2.8k
      left_join(MainWikiTagGroup2WikiTag, by = 'tag_id') #3.3k
    
    iw_with_urls <- iw_with_tags_groups
    
    kb_articles_urls <- str_detect(iw_with_tags_groups$page_id,'(^[:digit:]{4}osi[:digit:])|(^kb[:digit:]{5})')
    
    iw_url_1 <- iw_with_tags_groups %>% filter(kb_articles_urls) %>%
      mutate(url = paste0('https://tswiki.osisoft.int/kb_articles/',page_id))
    
    iw_url_2 <- iw_with_tags_groups %>% filter(!kb_articles_urls) %>%
      mutate(url = paste0('https://tswiki.osisoft.int/tswiki/',page_id))
    
    iw <- bind_rows(iw_url_1,iw_url_2) %>% select(id,page_name,url,tag_id,group_id) %>% #3.3k
      filter(!is.na(tag_id)) %>% #3.2k
      filter(!is.na(group_id)) #2.7k
    
    iw_no_1 <- iw %>% filter(group_id != 1) %>% # take out the 'all' tag group, whatever that means.
      left_join(MainWikiTagGroup, by = c('group_id' = 'id'))
    
    iw_no_1_17_18 <- iw_no_1 %>% filter(group_id != 17, group_id != 18) %>% #281 #take out 17 and 18, because I think different category.
      distinct(page_name,url,group_id) #192
    
    iw_no_1_17_18
    
    rv$clustered <- iw_no_1_17_18 %>% group_by(group_id) %>% arrange(group_id) %>% 
      mutate(cluster_id = cluster(page_name, input$cluster_greediness)) %>%
      left_join(select(MainWikiTagGroup,-category_name), by = c('group_id' = 'id')) 
    
    rv$clustered %>% my_tree(c('group_name','cluster_id','page_name'),'tree')
  })
  
  observeEvent({
    input$tree
  },{
    if(!is.null(input$tree[['page_name']])){
      rv$clustered %>% filter(page_name == input$tree[['page_name']]) %>% .$url %>% .[1] %>%   browseURL()  
    }
  })
  
  # functions ====
  my_tree <- function(df,architecture,ID){
    if(is.null(architecture)){return(NULL)}
    if(nrow(df)==0){return(NULL)}
    collapsibleTree(
      df
      ,inputId = ID
      ,hierarchy = architecture
      #,width = 800
      ,linkLength = 100
      #,hieght = 1600
      #,attribute='n'
      ,tooltip = TRUE
      #,nodeSize = 'log_n'
      ,collapsed = TRUE
    )
  }
  
  cluster <- function(message, cut_height = 50) {
    l <- length(message)
    if(l < 2) {
      return(1)
    }
    message %>% stringdistmatrix() %>% hclust() %>% cutree(h = cut_height) %>% return()
  }
  
  
  
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)

