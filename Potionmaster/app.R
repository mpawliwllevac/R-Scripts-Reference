library(shiny)
library(tidyverse)
library(DT)
library(shinydashboard)
library(kableExtra)
library(dplyr)

# To upload:
# library(rsconnect)
# deployApp("C:/Users/Matthew/Dropbox/R Working Directory/RShiny/folder")

theme_update(
  plot.margin= unit(c(0.25,0.25,0.25,0.25), "cm"),
  plot.title = element_text (colour="black", size=14,hjust = 0.5, face = "bold"),
  plot.subtitle = element_text (colour="black", size=10,hjust = 0.5),
  
  panel.background = element_rect(fill="NA"),
  panel.border = element_blank(),
  panel.spacing = unit(1, "lines"),
  
  panel.grid.major.y = element_line(colour="grey90"),
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

gedsbGreen <- "#59AD46"
gedsbBlue <- "#04559F"
gedsbGreen2 <- "#8CE079"
gedsbBlue2 <- "#51A2EC"

new <- c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland\n& Labrodor", "Nova Scotia", "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan")

old <- c("AB", "BC", "MB", "NB", "NL", "NS", "ON", "PE", "QC", "SK")

race_list <- c("Black", "Indigenous", "Latino", "Middle Eastern", "South-East Asian", "South Asian", "White")

samp_race <- sample(race_list, 500, replace = TRUE)

assign1_range <-c(40:100, 50:90, 70:85, 70:85)
assign1_grade <- sample(assign1_range, 500, replace=T)

assign2_range <- c(40:100, 60:90, 70:95, 75:85)
assign2_grade <- sample(assign2_range, 500, replace=T)

assign3_range <- c(40:100, 50:100, 65:100, 75:100)
assign3_grade <- sample(assign3_range, 500, replace=T)

assign4_range <- c(40:100, 50:100, 65:100, 75:100)
assign4_grade <- sample(assign4_range, 500, replace=T)

samples <-read.csv("ca-500.csv") %>% 
  bind_cols(race = samp_race, assign1 = assign1_grade, assign2 = assign2_grade, assign3 = assign3_grade, assign4 = assign4_grade) %>% 
  mutate(`Current Average` = (assign1+assign2+assign3+assign4)/4) %>% 
  select(-company_name, -phone2, -phone1, -web)

icon(name = "pencil", class = "small_icon_test")

server <- function(input, output, session) {
  
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
  
  data_race <- reactive({ # we use data from reactive like a function but we can just have it output the finished product
    input$checkGroup
    #print(test)
    #test
  })
  
  data_prov <- reactive({
    input$checkprov
  })
  
  output$sampleplot <- renderPlot({
    samples %>% 
      filter(race %in% data_race(), province %in% data_prov()) %>% 
      ggplot(aes(x=`Current Average`))+
      geom_density(fill = gedsbBlue2, color = gedsbBlue)+
      scale_y_continuous(limits = c(0,0.08))+
      scale_x_continuous(labels=c("50%", "60%", "70%", "80%", "90%", "100%"), limits = c(45,100), breaks = c(50, 60, 70, 80, 90, 100))+
      labs(title = "Distribution of Student Grades",
           x= "Grade", y= "Density")
  })
  
  output$histogram <- renderPlot({
    samples %>% 
      filter(race %in% data_race(), province %in% data_prov()) %>% 
      ggplot(aes(x= `Current Average`))+
      geom_histogram(bins = 15, fill = gedsbBlue2, color = gedsbBlue)+
      scale_x_continuous(labels=c("50%", "60%", "70%", "80%", "90%", "100%"),limits = c(45,100), breaks = c(50, 60, 70, 80, 90, 100))+
      labs(title = "Distribution of Student Grades",
           x= "Grade", y= "Count")
  })
  
  output$ibox <- renderInfoBox({
    infoBox(
      "Title",
      input$count,
      icon = icon("percentage")
    )
  })
  output$vbox <- renderValueBox({
    valueBox(
      paste0("Class Avg: ",
             samples %>%
               filter(race %in% data_race(), province %in% data_prov()) %>% 
               select(`Current Average`) %>% 
               summarise(temp = round(mean(`Current Average`),2)), "%  "
      ),
      subtitle = NULL,
      icon = icon("pencil")
    )
  })
  
  output$samp_size <- renderValueBox({
    valueBox("",
             paste0("Students Selected:",
                    samples %>%
                      filter(race %in% data_race(), province %in% data_prov()) %>%
                      select(`Current Average`) %>%
                      summarise(temp = n())
             ),
             color = "black"
    )
    # valueBox("title", 2+2)
  })
  
  # output$grade_boxplots <- renderPlot({
  #   samples %>% 
  #     filter(race %in% data_race(), province %in% data_prov()) %>% 
  #     select(starts_with("assign"), `Current Average`) %>% 
  #     gather(key = assignment, value = mark) %>% 
  #     mutate(mark = mark/100) %>% 
  #     ggplot(aes(x=assignment, y=mark))+
  #     geom_boxplot(color = "#222D32", fill = gedsbBlue2)+
  #     labs(title = "Comparisons of Student Evaluations",
  #          x= "",
  #          y="Grade"
  #     )+
  #     scale_y_continuous(labels=scales::percent, limits = c(0.45,1), breaks = c(0.5, 0.6,0.7,0.8,0.9,1))
  # })
  # 
  output$tableDT <- DT::renderDataTable(samples %>% filter(race %in% data_race(), province %in% data_prov()) ,
                                        options = list(paging = F),
                                        rownames = F,
                                        filter = "top")
  
  
  output$grade_violin <- renderPlot({
    samples %>%
      filter(race %in% data_race(), province %in% data_prov()) %>%
      select(starts_with("assign"), `Current Average`) %>% 
      gather(key = assignment, value = mark) %>% 
      mutate(mark = mark/100) %>% 
      ggplot(aes(x=assignment, y=mark))+
      labs(title = "Comparisons of Student Evaluations",
           x= "",
           y="Grade"
      )+
      scale_y_continuous(labels=scales::percent, limits = c(0.45,1), breaks = c(0.5, 0.6,0.7,0.8,0.9,1))+
      geom_violin(color = "#222D32", fill = gedsbBlue2)+
      geom_boxplot(alpha = 0.01, width = 0.15)
  })
  
  
  output$kable_prov <- function() {
    samples %>% 
      filter(race %in% data_race(), province %in% data_prov()) %>%
      mutate(province = plyr::mapvalues(province, from = old, to = new),
             province = factor(province, levels =new)) %>%
      group_by(province, .drop=FALSE) %>% 
      summarise(n=n()) %>% 
      mutate(prop = n/nrow(samples %>% filter(race %in% data_race(), province %in% data_prov())),
             temp = paste0(n, " (", round(prop, 2)*100, "%)")
      ) %>% 
      arrange(desc(n)) %>%
      transmute(Province = province, `n(%)` = temp) %>% 
      #spread(key = province, value = temp) %>% 
      knitr::kable("html", caption = "Selected Students by Province") %>%
      kable_styling("striped", full_width = F)
  }
  
  output$kable_race <- function(){
    samples %>% 
      mutate(race = factor(race, levels = race_list)) %>% 
      filter(race %in% data_race(), province %in% data_prov()) %>% 
      group_by(race, .drop=FALSE) %>% 
      summarise(n=n()) %>% 
      mutate(prop = n/nrow(samples %>% filter(race %in% data_race(), province %in% data_prov())),
             temp = paste0(n, " (", round(prop, 2)*100, "%)")) %>% 
      arrange(desc(n)) %>%
      transmute(Race = race, `n(%)` = temp) %>% 
      #spread(key = race, value = temp) %>% 
      knitr::kable("html", caption = "Selected Students by Race") %>%
      kable_styling("striped", full_width = F)
  }
  
  output$kable_grade <- function(){
    samples %>% 
      filter(race %in% data_race(), province %in% data_prov()) %>%
      mutate(grade_bin = ifelse(`Current Average` <50, "<50%",
                                ifelse(
                                  `Current Average` <60, "50% - 59%",
                                  ifelse(
                                    `Current Average` <70, "60% - 69%",
                                    ifelse(
                                      `Current Average` <80, "70% - 79%",
                                      ifelse(
                                        `Current Average` <90, "80% - 89%", "90% - 100%"))))),
             grade_bin = factor(grade_bin, levels = c("90% - 100%", "80% - 89%", "70% - 79%", "60% - 69%", "50% - 59%", "<50%"))
      ) %>% 
      group_by(grade_bin, .drop = FALSE) %>% 
      summarise(n=n()) %>% 
      mutate(prop = n/nrow(samples %>% filter(race %in% data_race(), province %in% data_prov())),
             temp = paste0(n, " (", round(prop, 2)*100, "%)")) %>% 
      transmute(`Current Grade` = grade_bin, `n(%)` = temp) %>% 
      #spread(key = grade_bin, value = temp) %>% 
      knitr::kable("html", caption = "Selected Students by Grade") %>%
      kable_styling("striped", full_width = F)
  }
  
  observe({ #running this requires the session argument in the main server function
    if(input$selectall == 0) return(NULL)
    else if (input$selectall%%2 == 0)
    {
      updateCheckboxGroupInput(session,"checkGroup",choices=race_list)
    }
    else
    {
      updateCheckboxGroupInput(session,"checkGroup",choices=race_list, selected=race_list)
    }
  })
  
  observe({ #running this requires the session argument in the main server function
    if(input$selectallprov == 0) return(NULL)
    else if (input$selectallprov%%2 == 0)
    {
      updateCheckboxGroupInput(session,
                               "checkprov",
                               choices=list("Alberta" = "AB","British Columbia"= "BC","Manitoba"= "MB","New Brunswick"= "NB","Newfoundland & Labrodor"= "NL","Nova Scotia"= "NS","Ontario"= "ON","Prince Edward Island"= "PE","Quebec"= "QC","Saskatchewan"= "SK"))
    }
    else
    {
      updateCheckboxGroupInput(session,
                               "checkprov",
                               choices=list("Alberta" = "AB","British Columbia"= "BC","Manitoba"= "MB","New Brunswick"= "NB","Newfoundland & Labrodor"= "NL","Nova Scotia"= "NS","Ontario"= "ON","Prince Edward Island"= "PE","Quebec"= "QC","Saskatchewan"= "SK"), 
                               selected=old)
    }
  })
  
}
####
ui <- dashboardPage(
  dashboardHeader(title = "Potionmaster Dashboard", titleWidth = 300),
  dashboardSidebar(
    valueBoxOutput("samp_size", width = 11),
    selectInput("classname", label = h3("Class"), choices = c("Grade 10 AP", "Grade 8 Honors")),
    hr(),
    checkboxGroupInput("checkGroup", label = h3("Race"),
                       choices = list("Black", "Indigenous", "Latino", "Middle Eastern", "South-East Asian", "South Asian", "White"),
                       selected = race_list),
    actionLink("selectall","Select All"),
    checkboxGroupInput("checkprov", label = h3("Province"),
                       choices = list("Alberta" = "AB","British Columbia"= "BC","Manitoba"= "MB","New Brunswick"= "NB","Newfoundland & Labrodor"= "NL","Nova Scotia"= "NS","Ontario"= "ON","Prince Edward Island"= "PE","Quebec"= "QC","Saskatchewan"= "SK"),
                       selected = c("AB", "BC", "MB", "NB", "NL", "NS", "ON", "PE", "QC", "SK")),
    actionLink("selectallprov","Select All")
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel(
        "Class Overview",
        fluidRow(
          tags$head( 
            tags$style(HTML(".fa { font-size: 35px; }"))
          ),
          valueBoxOutput("vbox", width = 6)
        ),
        plotOutput("histogram"),
        plotOutput("sampleplot")
      ),
      tabPanel("Evaluations",
               # plotOutput("grade_boxplots"),
               plotOutput("grade_violin")
      ),
      tabPanel(
        "Summary Data",
        column(tableOutput("kable_prov"),width =  4), 
        column(tableOutput("kable_race"),width =  4),
        
        tableOutput("kable_grade")
      ),
      tabPanel("Raw Student Data",
               DT::dataTableOutput("tableDT")
      )
    
  )
))


####
shinyApp(ui, server)