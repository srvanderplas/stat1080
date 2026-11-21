if (!"shiny" %in% installed.packages()) {
  install.packages("shiny")
}
if (!"bslib" %in% installed.packages()) {
  install.packages("bslib")
}
if (!"qqplotr" %in% installed.packages()) {
  install.packages("qqplotr")
}
if (!"pak" %in% installed.packages()) {
  install.packages("pak")
}
if (!"youdrawitR" %in% installed.packages()) {
  pak::pak("Alisa-Krasilnikov/youdrawitR")
}

library(shiny)
library(bslib)
library(tibble)
library(dplyr)
library(qqplotr)
library(ggplot2)
library(youdrawitR)

theme_set(theme_bw())

datapanel <- nav_panel(
  title = "Generate a Dataset",
  id = "data",
  icon = icon("arrow-up-right-dots", lib = "font-awesome"),
  fluidRow(
    column(
      width = 3,
      h3("Data Set Options"),
      # sliderInput(
      #   inputId = "n_points",
      #   label = "Select the size of your dataset",
      #   min = 10, max = 300, value = 30, step = 10
      # ),
      selectInput(
        inputId = "n_points",
        label = "Select the size of the dataset",
        choices = c(30, 50, 100, 300), 
        selected = 30
      ),
      
      checkboxGroupInput(
        inputId = "dataset", 
        label = "Select the features your dataset will have:",
        choices = c("Outliers" = 'outliers', 
                    "Non-linear trend" = 'nonlinear', 
                    "Lack of Normality" = 'nonnormal'),
        selected = c(),
        inline = FALSE
      ),
      actionButton(
        inputId = "gen_dataset",
        label = "Generate a dataset",
        icon = icon("circle-play", lib = "font-awesome", class = "btn-success")
      )
    ),
    column(
      width = 9,
      h3("Data Preview"),
      plotOutput("data_preview", width = "100%", height = "600px"),
      uiOutput("data_preview_text")
    )
  )
)


youdrawitpanel <- nav_panel(
  title = "You Draw It!", 
  id = "draw",
  icon = icon("pencil", lib = "font-awesome"),
  fluidRow(
    column(
      width = 12,
      h3("You Draw It!"),
      helpText("Using your computer's mouse or touch screen interface, 
               try to draw a line that fills the yellow area. 
               Follow the trend of the points."),
      br(),
      helpText("It may be helpful to hold the mouse button down and 
               backtrack to revise your prediction."),
      br(),
      helpText("When your line covers all of the available drawing area 
                (e.g. you have one x-value and one y-value for each available point), 
                a popup will show up (click ok!) and then the linear regression line will appear."),
      
      uiOutput("youdrawit", width = "100%", height = "800px"),
      uiOutput("youdrawit_text"),
      br(),
      actionButton("youdrawit_tryagain", "Reset Plot and try again"),
      br(),
      h4("Residual and Diagnostic Plots"),
      uiOutput("youdrawit_diag")
    )
  )
)

straightlinepanel <- nav_panel(
  title = "Straight Line Challenge", 
  id = "straight",
  icon = icon("arrow-trend-up", lib = "font-awesome"),
  fluidRow(
    column(
      width = 3,
      h3("Controls"),
      helpText("The equation for a line is $Y= aX + b$."), br(),
      helpText("$b$ is the intercept (the $Y$ value when $X=0$)."), br(), 
      helpText("$a$ is the slope (the change in $Y$ when $X$ changes by one)."), br(),
      sliderInput(
        inputId = "intercept",
        label = "Intercept (b)",
        min = -10,
        max = 10,
        step = .1, 
        value = 0
      ),
      sliderInput(
        inputId = "slope",
        label = "Slope (a)",
        min = -5,
        max = 5,
        step = .1,
        value = 0
      ),
      h4("Goal"),
      uiOutput("straightline_goal")
    ),
    column(
      width = 9,
      plotOutput("straightline_plot", width = "100%", height = "600px"),
      uiOutput("straightline_text"),
      br(),
      h4("Residual and Diagnostic Plots"),
      uiOutput("straightline_diag")
    )
  )
)

explanationpanel <- nav_panel(
  title = "Explanations",
  id = "explain",
  icon = icon("lightbulb", lib = "font-awesome")
)

ui <- page_navbar(
      title = "Can You Fit a Linear Regression?",
      tags$head(
        tags$link(rel="stylesheet", 
                  href="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.css", 
                  integrity="sha384-dbVIfZGuN1Yq7/1Ocstc1lUEm+AT+/rCkibIcC/OmWo5f0EA48Vf8CytHzGrSwbQ",
                  crossorigin="anonymous"),
        HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.js" integrity="sha384-2BKqo+exmr9su6dir+qCw08N2ZKRucY4PrGQPPWU1A7FtlCGjmEGFqXCv5nyM5Ij" crossorigin="anonymous"></script>'),
        HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/contrib/auto-render.min.js" integrity="sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI" crossorigin="anonymous"></script>'),
        HTML('
          <script>
            document.addEventListener("DOMContentLoaded", function(){
              renderMathInElement(document.body, {
                delimiters: [{left: "$", right: "$", display: false}]
              });
            })
          </script>
        ')
      ),
      datapanel,
      youdrawitpanel, 
      straightlinepanel#,
      # explanationpanel
)

server <- function(input, output) {
  theme_set(theme_bw())
  
  data <- reactive({
    input$gen_dataset
    
    validate(need(input$gen_dataset > 0, 
                  'Please generate a dataset using the Generate a dataset tab at the top of the page.\nMake sure to press the "Generate a dataset" button on the left side of the page.'))
    
    n_points <- as.numeric(isolate(input$n_points))
    
    x <- seq(-10, 10, length.out = n_points) + 
      rnorm(n_points, sd = 10/n_points)
    
    y <- 0
    
    df <- tibble(x = x, y = y, outlier = FALSE)
    
    if ("nonlinear" %in% isolate(input$dataset)){
      df$y <- df$x + (df$x/3)^2 - mean((df$x/3)^2)
    } else {
      df$y <- df$x - mean(df$x)
    }
    
    if ("nonnormal" %in% isolate(input$dataset)){
      df$y <- df$y + rexp(n_points, rate = 1/(abs(diff(range(df$y)))/4))
    } else {
      df$y <- df$y + rnorm(n_points, sd =abs(diff(range(df$y)))/4)
    }
    
    if ("outliers" %in% isolate(input$dataset)){
      # Get left and right but not middle indices
      boundary <- ceiling(n_points/4)
      opts <- c(1:boundary, 3*boundary:n_points)
      outliers <- sample(opts, size = ceiling(n_points/8))
      # flip the sign, which should make the points outliers
      df <- df |>
        mutate(y = if_else(row_number() %in% outliers, -y + 5*sign(-y), y),
               outliers = row_number() %in% outliers)
    } else {
      df$outliers <- FALSE
    }
    
    reg <- lm(y ~ x, data = df)
    
    df <- df |>
      mutate(pred = predict(reg), resid = residuals(reg))
    
    df
  })
  
  output$data_preview <- renderPlot({
    p <- ggplot(data = data(), aes(x = x, y = y)) 
    
    if ("outliers" %in% isolate(input$dataset)) {
      p <- p + 
        geom_point(aes(color = outliers)) + 
        scale_color_manual("Potential\nOutlier", 
                           values = c("FALSE" = "black", "TRUE" = "red")) + 
        theme(legend.position = "bottom")
    } else {
      p <- p + geom_point()
    }
    
    p
  })
  
  output$data_preview_text <- renderUI({
    data()
    
    if("outliers" %in% isolate(input$dataset)) {
      helpText("Note that the method for randomly creating outliers in a dataset only creates a potential outlier; it does not create a point which is certain to be an outlier.")
    }
  })
  
  
  
  # This is the "bridge" between JavaScript (browser) and Shiny (R server)
  shiny_message_loc <- "scatter_points"
  
  youdrawitplot <- reactive({
    input$youdrawit_tryagain
    
    ggplot(data = data(), aes(x = x, y = y)) +
    geom_point(size = 1.5) + 
    geom_smooth(method = "lm", se = FALSE)
  })
  
  
  draw_res <- reactive({
    input$youdrawit_tryagain
    
    drawit(youdrawitplot(), 
         show_on_finish = T, 
         smoother = .5,
         draw_start = min(data()$x), 
         shiny_message_loc = shiny_message_loc)
  })
  
  draw_res_data <- reactive({
    data.frame(
      x = data()$x,
      y = data()$y,
      draw_pred = approx(x = draw_res()$points()$x, 
                         y = draw_res()$points()$y, 
                         xout = data()$x)$y
    ) |>
      mutate(draw_resid = y - draw_pred)
  })
  
  output$youdrawit <- renderUI({
    input$youdrawit_tryagain
    
    draw_res()$youdrawit_plot
  })
  
  mse_lm <- reactive(mean(data()$resid^2))
  
  mse_draw <- reactive(mean((draw_res_data()$draw_resid)^2, na.rm = T))
  
  output$youdrawit_text <- renderUI({
    list(
      helpText(sprintf("Your line has a mean squared error of %0.2f.", mse_draw())),
      br(),
      helpText(sprintf("The linear regression fitted to this data has a mean squared error of %0.2f.", mse_lm()))
    )
  })
  
  
  output$youdrawit_resid <- renderPlot({
    p <- ggplot(data = draw_res_data(), aes(x = x, y = draw_resid)) + 
      geom_point() + 
      geom_smooth(method = "lm", se = F)
    
    p
  })  
  
  output$youdrawit_qq <- renderPlot({
    p <- ggplot(data = draw_res_data(), aes(sample = draw_resid)) + 
      stat_qq_band() + 
      stat_qq_line() + 
      stat_qq_point() + 
      coord_fixed()
    
    p
  })
  
  output$youdrawit_diag <- renderUI({
    fluidRow(
      column(
        width = 6,
        h5("Residuals"),
        plotOutput("youdrawit_resid")
      ),
      column(
        width = 6,
        h5("Q-Q Plot"),
        plotOutput("youdrawit_qq")
      )
    )
  })
  
  straight_res <- reactive({
    data() |>
      mutate(ypred = x*input$slope + input$intercept,
             resid = y - ypred)
  })
  
  mse_straight <- reactive(mean(straight_res()$resid^2))
  
  output$straightline_text <- renderUI({
    list(
      helpText(sprintf("Your line has a mean squared error of %0.2f.", mse_straight()))
    )
  })
  
  output$straightline_goal <- renderUI({
    helpText(sprintf("The minimum MSE you can get (if you could optimize slope and intercept past the 0.1 level) is %0.2f.", mse_lm()))
  })
  
  
  output$straightline_plot <- renderPlot({
    p <- ggplot(data = data(), aes(x = x, y = y)) + 
      coord_cartesian(xlim = c(-11, 11), 
                      ylim = range(c(data()$y, straight_res()$ypred))) + 
      geom_point() + 
      geom_abline(slope = input$slope, intercept = input$intercept)

    p
  })
  
  output$straightline_resid <- renderPlot({
    
    p <- ggplot(data = data(), aes(x = x, y = y - (x*input$slope + input$intercept))) + 
      coord_cartesian(xlim = c(-11, 11), 
                      ylim = range(c(data()$resid))) + 
      geom_point() + 
      geom_smooth(se = F)
    
    p
  })  
  
  output$straightline_qq <- renderPlot({
    p <- ggplot(data = data(), aes(sample = y - (x*input$slope + input$intercept))) + 
      stat_qq_band() + 
      stat_qq_line() + 
      stat_qq_point() + 
      coord_fixed()
    
    p
  })
  
  output$straightline_diag <- renderUI({
    fluidRow(
      column(
        width = 6,
        h5("Residuals"),
        plotOutput("straightline_resid")
      ),
      column(
        width = 6,
        h5("Q-Q Plot"),
        plotOutput("straightline_qq")
      )
    )
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
