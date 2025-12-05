
library(shiny)
library(NHANES)
library(ggplot2)
library(patchwork)
library(dplyr)
library(cowplot)


ui <- fluidPage(
  titlePanel('Substance Use Versus Demographic Features'),
  
  tabsetPanel(
    tabPanel('Summary',
             sidebarPanel(
               width = 3,
               actionButton('displStats',' Display Statistics'),
               checkboxGroupInput('varStats','Choose Variables',
                                  choices = c('HardDrugs', 'SmokeAge',
                                              'Marijuana', 'AlcoholYear', 'Race3','Age',
                                              'Gender', 'MaritalStatus', 'Poverty', 
                                              'Education'))
             ),
             mainPanel(
               verbatimTextOutput('stats'),
               plotOutput('plotStats')
             ),
    ),
    tabPanel('Hard Drugs',
             h3('What participants have used hard drugs and how does
                participation vary by race, gender, and age?'),
             sidebarPanel(
               width = 3,
               actionButton('vizHD', 'Visualize'),
               checkboxGroupInput('raceHD','Select Race', choices = c('Asian','Black','Hispanic','Mexican','White','Other'),
                                  selected = c('Asian','Black','Hispanic','Mexican','White','Other')),
               checkboxGroupInput('ageHD','Select Age', choices = c('18-29','30-39','40-49','50-59','60-69'),
                                  selected = c('18-29','30-39','40-49','50-59','60-69')),
               checkboxGroupInput('genderHD','Select Gender',
                                  choiceNames = list('male', 'female'),
                                  choiceValues = list(2,1), selected = list(2,1))
             ),
             mainPanel(
               plotOutput('plotHD')
             )
    ),
    tabPanel('Cigarettes',
             h3('What age do participants first smoke cigarettes across poverty, marital status, and race?'),
             sidebarPanel(
               width = 3,
               actionButton('vizCig', 'Visualize'),
               checkboxGroupInput('raceCig','Select Race', choices = c('Asian','Black','Hispanic','Mexican','White','Other'),
                                  selected = c('Asian','Black','Hispanic','Mexican','White','Other')),
               checkboxGroupInput('maritalCig','Select Marital Status',
                                  choices = c('Married','Not Married'),
                                  selected = c('Married','Not Married'))
             ),
             mainPanel(
               plotOutput('plotCig', height = '700px')
             )
    )
  )
)

server <- function(input, output) {
  
  ################### Summary ##################
  df_stats <- eventReactive(input$displStats, {
    NHANESraw |>
      select(input$varStats)
  })
  output$stats <- renderPrint(summary(df_stats()))
  output$plotStats <- renderPlot({
    plotsStats <- lapply(names(df_stats()), function(x){
      p <- ggplot(df_stats())+aes_string(x)
      
      if(is.numeric(df_stats()[[x]])){
        p <- p + geom_density()
      }else{
        p <- p + geom_bar() + theme(
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      }
    })
    plot_grid(plotlist = plotsStats)
  })
  
  
  ################### Hard Drugs ###############
  df_HD <- NHANESraw |>
    select(HardDrugs, Race3, Gender, Age)|>
    filter(HardDrugs != '<NA>' & !is.na(Race3) & Age < 70 & Age >= 18) |>
    mutate(HDYes = case_when(
      HardDrugs == 'Yes' ~ 1,
      TRUE ~ 0
    ),
    ageGroup = case_when(
      Age < 30 ~ '18-29',
      Age < 40 ~ '30-39',
      Age < 50 ~ '40-49',
      Age < 60 ~ '50-59',
      Age < 70 ~ '60-69'
    ))
  
  df_HD_filt <- eventReactive(input$vizHD, {
    df_HD|>
      filter(Race3 %in% input$raceHD)|>
      filter(ageGroup %in% input$ageHD)
  })
  
  pie<- reactive({
    df_HD_filt()|>
      count(Race3,Gender, ageGroup, HDYes, name = "count")|>
      group_by(Race3, Gender, ageGroup)|>
      mutate(percentage = round(count / sum(count)*100, digits = 1))|>
      ungroup()
  })
  gender <- eventReactive(input$vizHD, {
    sum(as.numeric(input$genderHD))
  })
  
  output$plotHD <- renderPlot({
    HD_theme <- theme_void() +
      theme(
        plot.margin = margin(2, 2, 2, 2),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        panel.spacing = unit(0.3, "lines"),
      )
    
    p_m <- ggplot(subset(pie(), Gender == "male"), 
                  aes(x = "", y = count, fill = as.factor(HDYes))) + 
      geom_bar(stat = "identity", position = position_fill(), width = 1,
               color = "white") +
      geom_text(aes(label = paste0(percentage,'%')),
                position = position_fill(vjust = 0.60),
                size = 1.75)+
      coord_polar("y")+
      facet_grid(ageGroup ~ Race3)+
      scale_fill_brewer(palette = "Set1", name  = "Hard Drug Use", labels = c('No','Yes'))+
      ggtitle("Male")+
      labs(subtitle = 'Faceted by Race and Age Group (Years)')+
      HD_theme
    
    p_f <- ggplot(subset(pie(), Gender == "female"), 
                  aes(x = "", y = count, fill = as.factor(HDYes))) + 
      geom_bar(stat = "identity", position = position_fill(), 
               width = 1, color = "white") +
      geom_text(
        aes(label = paste0(percentage,'%')),
        position = position_fill(vjust = 0.60),
        size = 1.75)+
      coord_polar("y")+
      facet_grid(ageGroup ~ Race3)+
      scale_fill_brewer(palette = "Set1", name = 'Hard Drug Use', labels = c('No','Yes'))+
      ggtitle("Female")+
      labs(subtitle = 'Faceted by Race and Age Group (Years)')+
      HD_theme
    
    
    if(gender() == 3){
      (p_m | p_f) +
        #inset_element(blank, left = 0, bottom = 0.5, right = 2.5, top = 0.5)+
        plot_layout(guides = "collect") &
        theme(
          legend.position = "bottom",
          plot.margin = margin(2, 2, 2, 2)  
        )
    }else if (gender() == 2){
      (p_m) +
        plot_layout(guides = "collect") &
        theme(
          legend.position = "bottom",
          plot.margin = margin(2, 2, 2, 2)  
        )
    }else if(gender() == 1){
      (p_f) +
        plot_layout(guides = "collect") &
        theme(
          legend.position = "bottom",
          plot.margin = margin(2, 2, 2, 2)
        )
    }else{
      ggplot()
    }
  })
  ############### Cigarettes ###############
  
  df_Cig <- eventReactive(input$vizCig, {
    df_cigg <- NHANESraw |>
      select(SmokeAge, Poverty, MaritalStatus, Race3)|>
      mutate(MaritalStatus2 = case_when(
        MaritalStatus %in% c("NeverMarried", "Divorced","LivePartner") ~ "Not Married",
        MaritalStatus %in% c("Married") ~ "Married"),
        MaritalStatus2 = factor(MaritalStatus2, levels = c("Married", "Not Married")))|>
      filter(!is.na(MaritalStatus2), !is.na(Race3))|>
      filter(MaritalStatus2 %in% input$maritalCig, Race3 %in% input$raceCig)
  })
  
  
  output$plotCig <- renderPlot({
    p_cig1 <-ggplot(df_Cig(), aes(x = Poverty, y = SmokeAge,col = MaritalStatus2))+
      geom_density2d(color = 'black', linewidth = 0.75)+
      geom_point(alpha = 0.3, size = 0.8)+
      theme_bw() +
      theme(axis.line = element_line(color='black'),
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank())+
      scale_color_brewer(palette = "Set1")
    
    p_cig2 <- ggplot(df_Cig(), aes(x = Poverty, y = SmokeAge, col = MaritalStatus2))+
      geom_point(alpha = 0.3, size = 0.8)+
      geom_smooth()+
      facet_wrap(~Race3, scale = 'free')+
      theme_bw()+
      scale_color_brewer(palette = "Set1")+
      labs(caption = '95% Confidence Interval for Smoothing Model(s)')
    
    
    (p_cig1/p_cig2)
    
    
  })
  
}

shinyApp(ui = ui, server = server)

