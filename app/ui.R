library(shiny)
library(leaflet)


shinyUI(fluidPage(
  
  # app title
  titlePanel("Калькулятор статистики забрудненості"),
  
  # sidebar with inputs
  sidebarLayout(
    sidebarPanel(width = 4,
                 selectInput("pollutant", label = h4("Оберіть забрудник"), 
                              choices = c("АмонійІониМгДм3", "БСК5МгОДм3", "ЗависліРечовиниМгДм3", "КисеньРозчиненийМгО2Дм3", "НітратІониМгДм3", "НітритІониМгДм3",
                                          "ПерманганатнаОкислюваністьМгОДм3", "СинтетичніПоверхневоАктивніРечовиниМгДм3", "СульфатІониМгДм3", "ФосфатІониМгДм3", 
                                          "ХімічнеСпоживанняКиснюМгОДм3", "ХлоридІониМгДм3", "ФітопланктонТисКлітинДм3", "АтразинМкгДм3", "СимазинМкгДм3", 
                                          "АзотЗагальнийМгДм3"),
                              selected = "АмонійІониМгДм3"),
                  selectInput("valueType", label = h4("Оберіть показник для візуалізації"),
                              choices = c("mean", "median", "min", "max"), 
                              selected = "mean")
                  ),
  # output panel
  mainPanel(width = 8,
            leafletOutput("map"))
  
  )
)
)
