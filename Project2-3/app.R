
library(shiny)
library(NHANES)
library(ggplot2)
library(patchwork)
library(dplyr)
library(cowplot)


ui <- fluidPage(
  titlePanel('Substance Use Explained by Demographics and Contributing Factors'),
  
  tabsetPanel(
    tabPanel('Summary',
             sidebarPanel(
               width = 3,
               actionButton('displStats',' Display Statistics'),
               checkboxGroupInput('varStats','Choose Variables',
                                  choices = c('HardDrugs', 'SmokeAge',
                                              'Marijuana', 'AlcoholYear', 'Race3','Age',
                                              'Gender', 'MaritalStatus', 'Poverty', 
                                              'Education','BMI','TotChol'))
             ),
             mainPanel(
               verbatimTextOutput('glimpse'),
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
                                  choiceNames = list('Male', 'Female'),
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
    ),
    tabPanel('Alcohol and Marijuana',
             h3('How does marijauna use relate to alcohol use, cholesterol and BMI across demographics such as race and gender?'),
             sidebarPanel(
               width = 3,
               actionButton('vizMarj', 'Visualize'),
               selectInput(
                 inputId = "y_var",
                 label = "Select Y-axis Variable:",
                 choices = c("BMI" = "BMI", 
                             "Cholesterol" = "TotChol"),
                 selected = "BMI"
               ),
               checkboxGroupInput('raceMarj','Select Race', 
                                  choices = c(
                                    "White"    = "White",
                                    "Mexican"  = "Mexican",
                                    "Asian"    = "Asian",
                                    "Black"    = "Black",
                                    "Other"    = "Other",
                                    "Hispanic" = "Hispanic"
                                  ),
                                  selected = c("White", "Mexican", "Asian", "Black", "Other", "Hispanic")
               ),
               checkboxGroupInput('genderMarj','Select Gender',
                                  choices = unique(NHANESraw$Gender),
                                  selected = unique(NHANESraw$Gender))
             ),
             mainPanel(
               plotOutput('plotMarj', height = '700px')
             )
    ), 
    ############## Alcohol ################
    tabPanel("Alcohol", 
             h3("How does reported alcohol consumption vary by marriage status, race, and gender?"),
             sidebarPanel(
               width = 3, 
               actionButton("viz_alc", "Visualize"), 
               checkboxGroupInput("race_alc", "Select Race" , choices = c("Asian", "Black", "Hispanic", "Mexican", "White", "Other"), 
                                  selected = c("Asian", "Black", "Hispanic", "Mexican", "White", "Other")), 
               checkboxGroupInput("gender_alc", "Select Gender",
                                  choiceNames = list("Male", "Female"), 
                                  choiceValues = list(2,1), selected = list(2,1)),
               checkboxGroupInput("marriage_alc", "Select Marriage Status", 
                                  choices = c("Married", "Never Married", "Separated", "Divorced", "Widowed"), 
                                  selected = c("Married", "Never Married", "Separated", "Divorced", "Widowed"))
             ),
             mainPanel(
               plotOutput('plot_alc', height = "500px", width = "1000px")
             )
    )
  )
)

server <- function(input, output) {
  
  ################### Summary ##################
  output$glimpse <- renderPrint({
    df <- NHANESraw|>
      select('HardDrugs','SmokeAge','Marijuana','AlcoholYear','Race3','Age',
             'Gender','MaritalStatus','Poverty','Education','BMI','TotChol')|>
      slice_sample(n = 25)
    glimpse(df)
  })
  df_stats <- eventReactive(input$displStats, {
    NHANESraw |>
      select(input$varStats)
  })
  output$stats <- renderPrint(summary(df_stats()))
  output$plotStats <- renderPlot({
    plotsStats <- lapply(names(df_stats()), function(x){
      p <- ggplot(df_stats())+aes_string(x)
      
      if(is.numeric(df_stats()[[x]])){
        p <- p + geom_density(fill = 'grey', alpha = 0.8)
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
  
  size <- eventReactive(input$vizHD, {
    max <- max(length(input$raceHD), length(input$ageHD))
    if(max <=1 ){
      size = 8
    }else if(max <= 2){
      size = 5.5
    }else if (max <= 3){
      size = 4
    }else if (max <= 4){
      size = 2.75
    }else if( max <= 6){
      size = 1.8
    }
    
    size
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
                size = size())+
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
        size = size())+
      coord_polar("y")+
      facet_grid(ageGroup ~ Race3)+
      scale_fill_brewer(palette = "Set1", name = 'Hard Drug Use', labels = c('No','Yes'))+
      ggtitle("Female")+
      labs(subtitle = 'Faceted by Race and Age Group (Years)')+
      HD_theme
    
    
    if(gender() == 3){
      (p_m | p_f) +
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
      labs(title = 'Smoking Age by Poverty Across Marital Status and Race',
           y = 'Initial Smoking Age (Years)',
           col = 'Marital Status')+
      scale_color_brewer(palette = "Set1")
    
    p_cig2 <- ggplot(df_Cig(), aes(x = Poverty, y = SmokeAge, col = MaritalStatus2))+
      geom_point(alpha = 0.3, size = 0.8)+
      geom_smooth(aes(fill = MaritalStatus2))+
      facet_wrap(~Race3, scale = 'free')+
      theme_bw()+
      scale_color_brewer(palette = "Set1")+
      labs(caption = '95% Confidence Interval for Smoothing Model(s) \n
           "Not Married": represents all marital statuses other than "Married" \n
           "Poverty": Ratio of family income to poverty guidelines (Smaller values indicate more poverty)',
           y= 'Initial Smoking Age (Years)',
           col = 'Marital Status')
    
    (p_cig1/p_cig2)+
      guides(fill = FALSE)
  })
  
  ###################### Alcohol + Marijuana ######################
  df_marj <- eventReactive(input$vizMarj, {
    df_mar <- NHANESraw |>
      select(RegularMarij, Alcohol12PlusYr, BMI, TotChol, Gender, Race3) |>
      filter(
        !is.na(RegularMarij),
        !is.na(Alcohol12PlusYr),
        !is.na(BMI),
        !is.na(TotChol),
        !is.na(Gender),
        !is.na(Race3)
      ) |>
      filter(
        Gender %in% input$genderMarj,
        Race3 %in% input$raceMarj,
      )
      
  })
  
  output$plotMarj <- renderPlot({
    print(nrow(df_marj()))
    print(table(df_marj()$Gender))
    print(table(df_marj()$Race3))
    p_marj <- ggplot(df_marj(),
                      aes(x = RegularMarij,
                          y = .data[[input$y_var]],
                          fill = Alcohol12PlusYr,
                          color = Alcohol12PlusYr)) +
      geom_boxplot(alpha = 0.6, trim = FALSE) +
      geom_point(alpha = 0.35, size = 0.9) +
      facet_wrap(Gender ~ Race3) +
      guides(color = "none") +
      labs(
        title = sprintf("%s by Marijuana Use and Alcohol Use Across Race and Gender", input$y_var),
        x = "Regular Marijuana Use (Yes / No)",
        y = input$y_var,
        fill = "Alcohol Use"
      )
    
    p_marj
  })
  #################### Alcohol ######################
  df_alc <- eventReactive(input$viz_alc, {
    df_alcohol <- NHANESraw |>
      select(Race3, AlcoholYear, Gender, MaritalStatus, Age, SurveyYr)|>
      filter(SurveyYr == "2011_12" & Age >= 20)|>
      filter(
        !is.na(MaritalStatus), 
        !is.na(Race3),
        !is.na(Gender),
        !is.na(AlcoholYear)) |> 
      mutate(MaritalStatus = case_when(
        MaritalStatus == "NeverMarried" ~ "Never Married",
        TRUE ~ MaritalStatus)) |>
      mutate(LogAlc = log(AlcoholYear))|>
      filter(Race3 %in% input$race_alc, 
             MaritalStatus %in% input$marriage_alc)
  })
  
  gender <- eventReactive(input$viz_alc, {
    sum(as.numeric(input$gender_alc))
  })
  
  output$plot_alc <- renderPlot({
    plot_alc_m <- ggplot(subset(df_alc(), Gender == "male"), aes(x = LogAlc, fill = MaritalStatus)) +
      geom_density(alpha = 0.3) +
      facet_wrap(~Race3) + ggtitle("Male") + labs(subtitle = "Faceted by Race", 
                                x = "Estimated Number of Days Drinking in the Past Year (log)",
                                y = "Density", 
                                fill = "Marital Status", 
                                caption = "This graph shows KDE plots of the log-transformed self-reported 
                                number of alcoholic beverages consumed in the past year by male participants across 
                                the Race3 variable from the 2011-2012 survey year. Log transformation was applied
                                to normalize the distribution and improve visual clarity.")
    plot_alc_f <- ggplot(subset(df_alc(), Gender == "female"), aes(x = LogAlc, fill = MaritalStatus)) +
      geom_density(alpha = 0.3) +
      facet_wrap(~Race3) + ggtitle("Female") + labs(subtitle = "Faceted by Race", 
                                x = "Estimated Number of Days Drinking in the Past Year (log)", 
                                y = "Density", 
                                fill = "Marital Status", 
                                caption = "This graph shows KDE plots of the log-transformed self-reported 
                                number of alcoholic beverages consumed in the past year by female participants across 
                                the Race3 variable from the 2011-2012 survey year. Log transformation was applied
                                to normalize the distribution and improve visual clarity.")
    
    if(gender() == 3){
      (plot_alc_m | plot_alc_f) +
        plot_layout(guides = "collect") &
        theme(
          legend.position = "bottom"
        ) 
    }else if (gender() == 2){
      (plot_alc_m) +
        plot_layout(guides = "collect") &
        theme(
          plot.margin = margin(2, 2, 2, 2)  
        )
    }else if(gender() == 1){
      (plot_alc_f) +
        plot_layout(guides = "collect") &
        theme(
          plot.margin = margin(2, 2, 2, 2)
        )
    }else{
      ggplot() 
    }
  })
}

shinyApp(ui = ui, server = server)
