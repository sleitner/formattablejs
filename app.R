library(shiny)
library(DT)
library(formattable)

df = iris
# some other df types for the breadth of filters
df['SpeciesCh'] = as.character(df$Species)
make_int = function(x) round(x*10)
df['Sepal.Length.ID'] = factor(as.character(lapply(df$Sepal.Length, make_int)))

# min/max for the coloring
lo = lapply(df[,c(rep(0:3))],min)
hi = lapply(df[,c(rep(0:3))],max)

ui <- fluidPage(
  titlePanel("Fancy Table Variations"),
  tags$head(tags$script(src="rainbowvis.js")),
  tabsetPanel(type = "tabs",
              tabPanel("Welcome", verbatimTextOutput("welcome")),
              tabPanel("Iris Data JS", DT::dataTableOutput("iris_js")),
              tabPanel("Iris Data Formattable", DT::dataTableOutput("iris_f"))
  )
)

server <- function(input, output) {
  output$iris_js <- DT::renderDataTable(
    server = TRUE,
    df, 
    rownames = FALSE, 
    filter = list(position = 'top', clear = FALSE),
    caption = htmltools::tags$caption(
      style = 'caption-side: top; text-align: left;',
      htmltools::em('This is the Javascript version of formattable tiles, allowing filter functionality.')
    ), 
    escape = FALSE, 
    options = list(
      dom='ltipr',
      # columnDefs = list(
      #   list(targets = c(rep(1:7)), className='dt-center', width='200px', searchable = TRUE)
      # ),
      rowCallback = DT::JS(paste0(
        'function(row, data) {
          
          var rainbow = new Rainbow();
          rainbow.setSpectrum("orangered", "lightgray", "skyblue");

          rainbow.setNumberRange(',lo[1],',',hi[1],'); 
          var v = 0;
          $("td:eq("+v+")", row).html("<span style=\'display: block; padding: 0 4px; border-radius: 4px; background-color: " + "#" + rainbow.colourAt(data[v]) + ";\'>" + data[v] + "</span>")

          rainbow.setNumberRange(',lo[2],',',hi[2],'); 
          var v = 1;
          $("td:eq("+v+")", row).html("<span style=\'display: block; padding: 0 4px; border-radius: 4px; background-color: " + "#" + rainbow.colourAt(data[v]) + ";\'>" + data[v] + "</span>")

          rainbow.setNumberRange(',lo[3],',',hi[3],'); 
          var v = 2;
          $("td:eq("+v+")", row).html("<span style=\'display: block; padding: 0 4px; border-radius: 4px; background-color: " + "#" + rainbow.colourAt(data[v]) + ";\'>" + data[v] + "</span>")
        }'
      )) #JS( paste0
    ) #options
  ) #output$iris_js
  
  
  output$iris_f <- DT::renderDataTable(
    server = TRUE,
    as.datatable(formattable({df}, 
        list(
          Sepal.Length=color_tile("orangered","skyblue"),
          Sepal.Width=color_tile("orangered","skyblue"),
          Petal.Length=color_tile("orangered","skyblue")
        )
      ),
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: left;',
        htmltools::em('This uses the R formattable library.')
        ), 
      rownames = FALSE, 
      filter = list(position = 'top', clear = FALSE),
      escape = FALSE,  
      options = list(dom='ltipr')
      )) # output$engagement
  
  
} # server


shinyApp(ui=ui, server=server)
