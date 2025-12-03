library(shiny)
library(NHANES)
library(ggplot2)
library(patchwork)
library(dplyr)


ui <- fluidPage(
  titlePanel('Substance Use Versus Demographic Features'),
  
  tabsetPanel(
    tabPanel('Summary'),
    tabPanel('Hard Drugs',
      h3('What participants have used hard drugs and how does
                participation vary by race, gender, and age?'),
      sidebarPanel(
        width = 3,
        actionButton('viz', 'Visualize'),
        checkboxGroupInput('race','Select Race', choices = c('Asian','Black','Hispanic','Mexican','White','Other'),
                           selected = c('Asian','Black','Hispanic','Mexican','White','Other')),
        checkboxGroupInput('age','Select Age', choices = c('18-29','30-39','40-49','50-59','60-69'),
                           selected = c('18-29','30-39','40-49','50-59','60-69')),
        checkboxGroupInput('gender','Select Gender',
                           choiceNames = list('male', 'female'),
                           choiceValues = list(2,1),selected = list(2,1))
      ),
      mainPanel(
        plotOutput('plot')
      )
    ),
  )
)

server <- function(input, output) {
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
  
  df_HD_filt <- eventReactive(input$viz, {
    df_HD|>
      filter(Race3 %in% input$race)|>
      filter(ageGroup %in% input$age)
  })
  
  pie<- reactive({
    df_HD_filt()|>
      count(Race3,Gender, ageGroup, HDYes, name = "count")|>
      group_by(Race3, Gender, ageGroup)|>
      mutate(percentage = round(count / sum(count)*100, digits = 1))|>
      ungroup()
  })
  gender <- eventReactive(input$viz, {
    sum(as.numeric(input$gender))
  })
  
  output$plot <- renderPlot({
    library(patchwork)
    
    tight_theme <- theme_void() +
      theme(
        plot.margin = margin(2, 2, 2, 2),
        plot.title = element_text(
          size = 14, face = "bold", hjust = 0.5,
          margin = margin(b = 2)
        ),
        strip.text.x = element_text(
          size = 7, angle = 30, face = 'bold',
          margin = margin(t = 2)
        ),
        strip.text.y = element_text(
          size = 11,
        ),
        panel.spacing = unit(0.3, "lines")
      )
    
    p_m <- ggplot(subset(pie(), Gender == "male"), 
                  aes(x = "", y = count, fill = as.factor(HDYes))) + 
      geom_bar(stat = "identity", position = position_fill(), 
               width = 1, color = "white") +
      geom_text(
        aes(label = paste0(percentage,'%')),
        position = position_fill(vjust = 0.60),
        size = 1.5
      )+
      coord_polar("y") +
      facet_grid(ageGroup ~ Race3) +
      ggtitle("Male") +
      tight_theme
    
    
    p_f <- ggplot(subset(pie(), Gender == "female"), 
                  aes(x = "", y = count, fill = as.factor(HDYes))) + 
      geom_bar(stat = "identity", position = position_fill(), 
               width = 1, color = "white") +
      geom_text(
        aes(label = paste0(percentage,'%')),
        position = position_fill(vjust = 0.60),
        size = 1.5
      )+
      coord_polar("y") +
      facet_grid(ageGroup ~ Race3) +
      ggtitle("Female") +
      tight_theme
    
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

}

shinyApp(ui = ui, server = server)
