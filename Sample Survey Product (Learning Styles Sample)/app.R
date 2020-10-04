
# Learning Styles Dashbaord
# This app is to demonstrate deployment from an active survey to an online shiny app automatically.
# This app also hopefully serves a a place for me to look at later as home of pretty graphs


# Preamble ----------------------------------------------------------------
# Insert Section — Ctrl+Shift+R
# Jump To — Shift+Alt+J

library(tidyverse)
library(forcats)
library(kableExtra)
library(googlesheets4)
library(shiny)
library(shinydashboard)

theme_update(
  plot.margin= unit(c(0.25,0.25,0.25,0.25), "cm"),
  plot.title = element_text (colour="black", size=14,hjust = 0.5, face = "bold"),
  plot.subtitle = element_text (colour="black", size=10,hjust = 0.5),
  
  panel.background = element_rect(fill="NA"),
  panel.border = element_blank(),
  panel.spacing = unit(1, "lines"),
  
  panel.grid.major.y = element_line(colour="grey90"), #got x/y lines
  panel.grid.minor.y = element_line(colour="NA"),
  panel.grid.major.x = element_line(colour="NA"),
  panel.grid.minor.x = element_line(colour="NA"),
  
  axis.text.y = element_text (colour="black", size=10, hjust=1),
  axis.title.y = element_text (colour="black", size=12, angle=90),
  axis.text.x = element_text (colour="black", size=10,angle=0),
  axis.title.x = element_text (colour="black", size=12),
  axis.ticks = element_blank(),
  
  legend.text = element_text (colour="black", size = 12), 
  legend.position = ("right"),
  legend.title = element_blank(),
  legend.key = element_blank()
)


# Commonly Referenced Objects ---------------------------------------------

gedsbGreen <- "#59AD46"
gedsbBlue <- "#04559F"
gedsbGreen2 <- "#8CE079"
gedsbBlue2 <- "#51A2EC"
proper_scale <- c(0.25, 0.5, 0.75,1)


# Specifically Referenced Objects -----------------------------------------

column_names <- c("timestamp", "ethnicity", "age", "district", "planning_school", 
                  "teacher", "after_hs", "after_s_school", "personality_type", "temperment", 
                  "intrapersonal", "interpersonal", "kinesthetic", "linguistic", "logical", 
                  "musical", "naturalist", "visual")

#function/objects for ethnicity
detfx <- function(x, w) {
  y<- str_detect(x, regex(paste(w, collapse = '|'), ignore_case = TRUE))
  return(y)
}

pre_eth <- c("eth_American", "eth_British", "eth_Taiwanese", "eth_Chinese", "eth_Filipino", "eth_Japanese", "eth_Korean", "eth_other")
name_eth <- c("American", "British", "Taiwanese", "Chinese", "Filipino", "Japanese", "Korean", "Other")

#legend order for school plot
school_order <- c("No", "Maybe", "Yes")

#names of personality cols
ps_types <- c("intrapersonal", "interpersonal", "kinesthetic", "linguistic", "logical", "musical", "naturalist", "visual")


# Importing the Data ------------------------------------------------------

gs4_deauth()
ls <- read_sheet("https://docs.google.com/spreadsheets/d/1RUi8kDGkVGEe9UoiEqqWToZqIIe22J-dI5pw_9XwVc0/edit?usp=sharing")
colnames(ls) <- column_names

#Adding ethnicity columns
ls <- ls %>% 
  mutate(
    eth_Taiwanese = detfx(ethnicity, "Taiwan"),
    eth_Korean = detfx(ethnicity, "Korean"),
    eth_Chinese = detfx(ethnicity, "Chinese"),
    eth_American = detfx(ethnicity, "American"),
    eth_British = detfx(ethnicity, "British"),
    eth_Filipino = detfx(ethnicity, "Filipino"),
    eth_Japanese = detfx(ethnicity, "Japanese"),
    eth_other = ifelse(eth_American+ 
                         eth_British+ 
                         eth_Taiwanese+ 
                         eth_Chinese+ 
                         eth_Filipino+ 
                         eth_Japanese+ 
                         eth_Korean == 0, TRUE, FALSE),
    check_hs = fct_lump(after_hs, 4)
  )
  
  
# UI Sidebar ----------------------------------------------------------------------

Sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Demographics", tabName = "Demographics", icon = icon("address-card")), #this makes tabs. tabcontents in the body
    menuItem("School", tabName = "School", icon = icon("school")),
    menuItem("Intelligences", tabName = "Intelligences", icon = icon("award")),
    #menuItem("Tables", tabName = "Tables", icon = icon("table")),
    valueBoxOutput("samp_size", width = 12),
    box(
      title = "Ethnicity", 
      solidHeader = TRUE,
      collapsible = TRUE, 
      collapsed = TRUE,
      background = "black", 
      width = 12,
      checkboxGroupInput("checketh", label = NULL,
                         choices = list("American", "British", "Taiwanese", "Chinese", "Filipino", "Japanese", "Korean", "Other"),
                         selected = name_eth),
      actionLink("selectalleth","Select All")
    ),
    box(
      title = "Age",
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,
      background = "black",
      sliderInput("sliderage", label = NULL, min = min(ls$age), 
                  max = max(ls$age), value = c(min(ls$age), max(ls$age)))
    ),
    box(
      title = "Districts",
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,
      background = "black",
      checkboxGroupInput(
        "checkdistrict",
        label = NULL,
        choices = as.list(unique(ls$district)),  ###getting the unique choices from a long list of choices makes the list dynamic
        selected = as.list(unique(ls$district))
        ),
      actionLink("selectalldistrict","Select All")
    )
    
    
  )
)


# UI Bodyy -----------------------------------------------------------------

Bodyy <- dashboardBody(
  tabItems( #holds all the tabItems which are where the contents of the sidebar tabs are stored
    #demographics tab
    tabItem(tabName = "Demographics", # a tabItem is the contents of a tab (from the idebar menuItems)
            plotOutput("plot_ethnicity"),
            #"Refer to Appendix A for a list of other ethnicities chosen.",
            plotOutput("plot_age"),
            plotOutput("plot_district")#,
            #"Refer to Appendix B for a complete of student districts."
            ),
    
    #school tab
    tabItem(tabName = "School",
            plotOutput("plot_planning"),
            plotOutput("plot_teacher"),
            plotOutput("plot_attend")#,
            #"Refer to Appendix C for a list of 'other' responses as well as open responses to 'What school do you hope to attend?'"
            ),
    
    #Intelligences tab
    tabItem(tabName = "Intelligences",
            plotOutput("plot_ptype"),
            plotOutput("plot_prank")
            )#,
    
    # Tables tab
    # tabItem(tabName = "Tables",
    #         h2("Appendix Tables")
    #         )
  )
)




# UI Assemble -------------------------------------------------------------

ui=dashboardPage(
  dashboardHeader(title = "Learning Styles Survey"),
  Sidebar,
  Bodyy
)


# Server ------------------------------------------------------------------

server=function(input, output, session) {
  
  #this ensures that the program stops functioning when its quit (necessary if using RInno)
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
  
  #this makes the checkbox output into something usable in normal code
  #remember to call data_eth() like a funtion even w no arguments
  data_eth <- reactive({
    input$checketh
  })
  
  data_age <- reactive({
    input$sliderage
  })
  
  data_district <- reactive({
    input$checkdistrict
  })

  ls_selected <- reactive({
    ls %>% 
      filter (age >= min(data_age()), 
              age <= max(data_age()),
              detfx(ethnicity, data_eth())== TRUE,
              district %in% data_district())
  })
  
  output$samp_size <- renderValueBox({
    valueBox("",
             paste0("Students Selected:", nrow(ls_selected())),
             color = "black")
  })
  
# Server Demographic Plots ------------------------------------------------------------

  #this will render plots to the output name
  #ethnicity plot
  output$plot_ethnicity <- renderPlot({
    ls_selected() %>% 
      select(starts_with("eth"), -ethnicity) %>% 
      gather(ethnicity, value) %>%
      dplyr::filter(value == TRUE) %>%
      dplyr::count(ethnicity) %>%
      mutate(ethnicity = plyr::mapvalues(ethnicity, from = pre_eth, to = name_eth),
             prop = round(n/nrow(ls_selected()),2),
             lbl = paste0(prop*100, "%")) %>%
      ggplot(aes(x=reorder(ethnicity,prop), y =prop))+
      geom_col(fill = gedsbBlue, color = "black")+
      geom_text(aes(x= reorder(ethnicity,prop),
                    y=prop,
                    label =lbl), hjust = -0.5)+
      labs(title = "What is your ethnicity?",
           subtitle = "(select all that apply)",
           x = "",
           y = "Proportion of Selected Students")+
      coord_flip()+
      scale_y_continuous(labels=scales::percent(proper_scale, accuracy = 1), limits = c(0,1), breaks = proper_scale)+
      theme(panel.grid.major.y = element_line(colour="NA"),
            panel.grid.major.x = element_line(colour="grey90"))
  })
  
  #age plot
  output$plot_age <- renderPlot({
    ls_selected() %>%
      select(age) %>% 
      dplyr::count(age) %>% 
      mutate(prop = round(n/nrow(ls_selected()),2),
             lbl = paste0(prop*100, "%")) %>% 
      ggplot(aes(x=age, y =prop))+
      geom_col(fill = gedsbBlue, color = "black")+
      geom_text(aes(x= age,
                    y=prop,
                    label =lbl), vjust = -1)+
      labs(title = "What is your age as of January 1 this year?",
           x = "",
           y = "Proportion of Selected Students")+
      scale_y_continuous(labels=scales::percent(proper_scale, accuracy = 1), limits = c(0,1), breaks = proper_scale)+
      theme(panel.grid.major.y = element_line(colour="grey90"),
            panel.grid.major.x = element_line(colour="NA"))
  })
  
  #district plot
  output$plot_district <- renderPlot({
    ls_selected() %>%
    mutate(lumped = fct_lump(district, n = 5, other_level = "Other")) %>% 
    dplyr::count(lumped) %>% 
    mutate(prop = round(n/nrow(ls_selected()),2),
           lbl = paste0(prop*100, "%")) %>% 
    ggplot(aes(x=reorder(lumped,prop), y =prop))+
    geom_col(fill = gedsbBlue, color = "black")+
    geom_text(aes(x= reorder(lumped,prop),
                  y=prop,
                  label =lbl), hjust = -0.5)+
    labs(title = "Student Districts",
         subtitle = "Districts less frequent than the top 5 were compressed into the 'Other' category",
         x = "District",
         y = "Proportion of Selected Students")+
    coord_flip()+
    scale_y_continuous(labels=scales::percent(proper_scale, accuracy = 1), limits = c(0,0.5), breaks = proper_scale)+
    theme(panel.grid.major.y = element_line(colour="grey90"),
          panel.grid.major.x = element_line(colour="NA"))
  })
  

# Server School Plots -----------------------------------------------------

  #planning plot
  output$plot_planning <- renderPlot({
    ls_selected() %>%
      mutate(plan_binary = ifelse(planning_school=="Yes", 
                                  "Yes",
                                  ifelse(planning_school=="Maybe", "Maybe", "No"))) %>% 
      count(plan_binary) %>% 
      mutate(plan_binary = factor(plan_binary, levels = school_order, ordered = TRUE),
             prop = round(n/nrow(ls_selected()),2),
             lbl = paste0(prop*100, "%"),
             temp = "") %>% 
      slice(match(school_order, plan_binary)) %>% #### this lets you reorder the rows acroding to the list
      mutate(pos = 1- ((cumsum(prop) - (0.5 * prop)))) %>% 
      ggplot(aes(x=reorder(temp,prop), y =prop, fill =plan_binary))+
      geom_bar(stat="identity", position=position_stack())+ 
      geom_text(aes(x = temp,
                    y = pos,
                    label = lbl))+
      coord_flip()+
      labs(title = "Are you planning to attend this school next year?",
           x = "",
           y = "Proportion of Students")+
      theme(legend.position = "bottom", 
            legend.box.spacing = unit(-10,"mm"),
            legend.justification = "center")+
      scale_fill_manual(values=c(gedsbGreen, gedsbBlue2, gedsbGreen2), 
                        breaks = rev(school_order))+ #####This is how you change legend order
      scale_y_continuous(labels=NULL, limits = c(0,1.01))+
      theme(panel.grid.major.y = element_line(colour="NA"),
            panel.grid.major.x = element_line(colour="NA"))
  })
  
  #teacher plot
  output$plot_teacher <- renderPlot({
    ls_selected() %>%
      count(teacher) %>%
      mutate(teacher = factor(teacher),
             prop = round(n/nrow(ls_selected()),2),
             lbl = paste0(prop*100, "%"),
             temp = "") %>%
      mutate(pos = 1- ((cumsum(prop) - (0.5 * prop)))) %>%
      ggplot(aes(x=temp, y =prop, fill =teacher))+
      geom_bar(stat="identity", position=position_stack())+
      geom_text(aes(x = temp,
                    y = pos,
                    label = lbl))+
      coord_flip()+
      labs(title = "Who was your science teacher this year?",
           x = "",
           y = "Proportion of Students")+
      theme(legend.position = "bottom",
            legend.box.spacing = unit(-10,"mm"),
            legend.justification = "center")+
      scale_fill_manual(values=c(gedsbGreen, gedsbGreen2), breaks = rev(c("Dr. Nikki", "Mr. Dawson")))+ #####This is how you change legend order
      scale_y_continuous(labels=NULL, limits = c(0,1.01))+
      theme(panel.grid.major.y = element_line(colour="NA"),
            panel.grid.major.x = element_line(colour="NA"))
  })
  
  #attend plot
  output$plot_attend <- renderPlot({
    ls_selected() %>%
      count(check_hs) %>%
      mutate(prop = round(n/nrow(ls_selected()),2),
             lbl = paste0(prop*100, "%")) %>%

      ggplot(aes(x=reorder(check_hs,prop), y =prop))+
      geom_col(fill = gedsbBlue, color = "black")+
      geom_text(aes(x= reorder(check_hs,prop),
                    y=prop,
                    label =lbl), hjust = -0.5)+
      labs(title = "Where do you plan to go (or where would you really like to go)\nfor school after high school?",
           x = "",
           y = "Proportion of Selected Students")+
      coord_flip()+
      scale_y_continuous(labels=scales::percent(c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), accuracy = 1), limits = c(0,1), breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))+
      theme(panel.grid.major.y = element_line(colour="NA"),
            panel.grid.major.x = element_line(colour="grey90"))
  })
  

# Server Intelligence Plots -----------------------------------------------

  #ptype plot
  output$plot_ptype <- renderPlot({
    ls_selected() %>%
      count(personality_type) %>% 
      mutate(prop = round(n/nrow(ls_selected()),2),
             lbl = paste0(prop*100, "%")) %>% 
      
      ggplot(aes(x=reorder(personality_type,prop), y =prop))+
      geom_col(fill = gedsbBlue, color = "black")+
      geom_text(aes(x= reorder(personality_type,prop),
                    y=prop,
                    label =lbl), hjust = -0.5)+
      labs(title = "Personality Types",
           x = "",
           y = "Proportion of Students")+
      coord_flip()+
      scale_y_continuous(labels=scales::percent(c(0,0.1,0.2,0.3,0.4,0.5), accuracy = 1), limits = c(0,0.5), breaks = c(0,0.1,0.2,0.3,0.4,0.5))+
      theme(panel.grid.major.y = element_line(colour="NA"),
            panel.grid.major.x = element_line(colour="grey90"))
  })
  
  #prank plot
  # ps_types <- c("intrapersonal", "interpersonal", "kinesthetic", "linguistic", "logical", "musical", "naturalist", "visual")
  output$plot_prank <- renderPlot({
    ls_selected() %>%
      select(all_of(ps_types)) %>% 
      gather(personality_type) %>% 
      group_by(personality_type) %>% 
      summarise(mean_score = round(mean(value),2)) %>% 
      ggplot(aes(x = reorder(personality_type, -mean_score), y = mean_score))+
      geom_col(fill = gedsbBlue)+
      geom_text(aes(label = mean_score), hjust = -0.5)+
      coord_flip()+
      labs(title = "Mean ranking of Intelligences",
           x = "",
           y = "Mean Score",
           subtitle = "A lower mean score indicates a higher ranking on average\n(eg being ranked 3.5 on average is better than ranked 4th on average)")+
      scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8))+
      theme(panel.grid.major.y = element_line(colour="NA"),
            panel.grid.major.x = element_line(colour="grey90"))
  })
  
# Server Action Buttons ---------------------------------------------------

  #this is the ethnicity select all action button
  observe({ #running this requires the session argument in the main server function
    if(input$selectalleth == 0) return(NULL)
    else if (input$selectalleth%%2 == 0)
    {
      updateCheckboxGroupInput(session,
                               "checketh",
                               choices=list("American", "British", "Taiwanese", "Chinese", "Filipino", "Japanese", "Korean", "Other"))
    }
    else
    {
      updateCheckboxGroupInput(session,
                               "checketh",
                               choices=list("American", "British", "Taiwanese", "Chinese", "Filipino", "Japanese", "Korean", "Other"), 
                               selected=name_eth)
    }
  })
  
  observe({ #running this requires the session argument in the main server function
    if(input$selectalldistrict == 0) return(NULL)
    else if (input$selectalldistrict%%2 == 0)
    {
      updateCheckboxGroupInput(session,
                               "checkdistrict",
                               choices=as.list(unique(ls$district)))
    }
    else
    {
      updateCheckboxGroupInput(session,
                               "checkdistrict",
                               choices=as.list(unique(ls$district)), 
                               selected=as.list(unique(ls$district)))
    }
  })
  
}


# App ---------------------------------------------------------------------

shinyApp(ui, server)























