library(shiny)
library(data.table)
library(ggplot2)
library(dplyr)
library(forcats)
library(rmarkdown)
library(officer)
library(ggtext)
library(glue)
library(shinydashboard)

# ----Data Source----
NE = fread('NE.csv', header = TRUE)
chapter_data = read.csv('chapter.csv', header = FALSE)
CHAPTER <- setNames(chapter_data[, 2], chapter_data[, 1])


# ----Function Section----
trend <- function(df, file) {
  TOC = read.csv(file, header = FALSE)
  colnames(TOC) <- c("Chapter", "num", "MB")
  qcounts = vector(mode = "numeric", length = nrow(TOC))
  for (i in 1:length(TOC[, 2])) {
    qcounts[i] <-
      (nrow(df[grepl(paste("\n{0,}\\b", TOC[i, 2], "\\b\n{0,}", sep = ""), Chapter)]))
  }
  TOC$qcounts <- qcounts
  return(TOC)
}
# ToDo: autofit font size to placeholder
make_ppt <- function(dat, file, choices, show_answer = TRUE) {
  my_pres <- read_pptx()
  for (i in choices) {
    COLORS = rep("black", 4)
    for (j in 1:2) {
      if (j == 2) {
        COLORS[which(LETTERS == as.character(dat[i, 7]))] <- "red"
      }
      my_pres <- add_slide(my_pres)
      question <-
        fpar(ftext(
          as.character(dat[i, 2]),
          fp_text(
            bold = TRUE,
            color = "steelblue4",
            font.size = 32,
            font.family = "Microsoft JhengHei"
          )
        ))
      choices <- block_list(
        fpar(
          ftext(
            as.character(dat[i, 8]), #question origin
            fp_text(font.size = 18, color = "purple")
          ),
          fp_p = fp_par(text.align = "right", line_spacing = 1.5)
        ),
        fpar(ftext(
          paste0("A) ", as.character(dat[i, 3])),
          fp_text(font.size = 28, color = COLORS[1])
        ), fp_p = fp_par(line_spacing = 1.5)),
        fpar(ftext(
          paste0("B) ", as.character(dat[i, 4])),
          fp_text(font.size = 28, color = COLORS[2])
        ), fp_p = fp_par(line_spacing = 1.5)),
        fpar(ftext(
          paste0("C) ", as.character(dat[i, 5])),
          fp_text(font.size = 28, color = COLORS[3])
        ), fp_p = fp_par(line_spacing = 1.5)),
        fpar(ftext(
          paste0("D) ", as.character(dat[i, 6])),
          fp_text(font.size = 28, color = COLORS[4])
        ), fp_p = fp_par(line_spacing = 1.5))
      )
      my_pres <-
        ph_with(my_pres,
                value = question,
                location = ph_location_type(type = "title"))
      my_pres <-
        ph_with(my_pres,
                value = (choices),
                location = ph_location_type(type = "body"))
      if (!show_answer) {
        break
      }
    }
  }
  
  print(my_pres, target = file)
}



# user database for logins
# user_base <- tibble::tibble(
#   user = c("user1", "user2"),
#   password = purrr::map_chr(c("pass1", "pass2"), sodium::password_store),
#   permissions = c("admin", "standard"),
#   name = c("User One", "User Two")
# )
user_base <- read.csv(file = "account.csv", colClasses=c('character'))
user_base$password <- purrr::map_chr(user_base$password, sodium::password_store)



# login tab ui to be rendered on launch
login_tab <- tabPanel(title = icon("lock"),
                      value = "login",
                      shinyauthr::loginUI("login"))

# additional tabs to be added after login----
home_tab <- tabPanel(
  title = icon("user"),
  value = "home",
  column(
    width = 12,
    tags$h2("User Information"),
    verbatimTextOutput("user_data")
  )
)

data_tab <- tabPanel(
  title = icon("table"),
  value = "data",
  column(width = 12,
         uiOutput("data_title"),
         DT::DTOutput("table"))
)

review <- tabPanel(title = "按章節複習",
                   sidebarLayout(
                     sidebarPanel(
                       helpText("依據國考參考書Lehninger Principles of Biochemistry分類整理"),
                       selectInput("chapter", label = 'Chapter', choices = CHAPTER),
                       #ToDo: add a year interval slider here),
                       hr(),
                       h5("國考題號說明"),
                       
                       div(em("M-101-2-78"), style = "color:blue"),
                       br(),
                       tags$ul(
                         tags$li("M是醫師、D為牙醫師"),
                         tags$li("101為國考出題年"),
                         tags$li("2為第二次"),
                         tags$li("78為第78題")
                       ),
                       hr(),
                       p("如有錯誤或疑問請聯絡張永龍老師 (ext:18800)"),
                       a(href="mailto:ylchang@mail.ndmctsgh.edu.tw", "Email")
                     ),
                     mainPanel(
                       box(div(em(textOutput(outputId = 'ori')), style = "color:blue"),
                       uiOutput("display_question"),
                       width = 12, height = 400),
                       hr(),
                       div(style = "display: inline-block;vertical-align:middle;",
                           actionButton(
                             "left", label = "Previous", icon = icon(("backward"))
                           )),
                       div(style = "display: inline-block;vertical-align:middle;",
                           uiOutput("question_slider")),
                       div(style = "display: inline-block;vertical-align:middle;",
                           actionButton("right", label = div("Next", icon(("forward")
                           )))),
                       br(),
                       htmlOutput(outputId = "reveal_choice"),
                       br(),
                       tableOutput(outputId = 'detail'),
                       verbatimTextOutput(outputId = 'explanation')
                     )
                   ))

trend_analysis <- tabPanel(title = "出題比例分析",
                           fluidPage(fluidRow(
                             column(
                               3,
                               sliderInput(
                                 inputId = "trend_year",
                                 label = "調整試題年份區間",
                                 min = min(NE[,"Year"]),
                                 max = max(NE[,"Year"]),
                                 value = c(min(NE[,"Year"]), max(NE[,"Year"])),
                                 step = 1
                               )
                             ),
                             column(
                               3,
                               div(h4("藍色: 分子生物學範圍"), style = "color:royalblue"),
                               div(h4("黑色: 生物化學範圍"), style = "color:black")
                             ),
                           ),
                           plotOutput('trend_plot')))


testbank_tab <- tabPanel(
  '題庫匯整',
  fluidRow(column(6,
                  titlePanel("題庫篩選")),
           
           column(6,
                  titlePanel("匯出簡報檔"))),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(
      3,
      sliderInput(
        inputId = "year",
        label = "Year:",
        min = min(NE[,"Year"]),
        max = max(NE[,"Year"]),
        value = c(min(NE[,"Year"]), max(NE[,"Year"])),
        step = 1
      )
    ),
    column(3,
           selectInput("ch",
                       "Chapter:",
                       c("All", CHAPTER))),
    column(
      6,
      div(h4("說明: 先選取想要的試題(可複選)，再按Download"), style = "color:royalblue"),
      checkboxInput("show_answer", label = "包含答案slide", value = TRUE),
      downloadButton("downloadData", "Download")
    )
    
    
  ),
  DT::dataTableOutput('testbank')
)



# initial app UI with only login tab
ui <- navbarPage(title = "國醫生化國考複習",
                 id = "tabs",
                 # must give id here to add/remove tabs in server
                 collapsible = TRUE,
                 login_tab)

server <- function(input, output, session) {
  # hack to add the logout button to the navbar on app launch
  insertUI(
    selector = ".navbar .container-fluid .navbar-collapse",
    ui = tags$ul(class = "nav navbar-nav navbar-right",
                 tags$li(
                   div(style = "padding: 10px; padding-top: 8px; padding-bottom: 0;",
                       shinyauthr::logoutUI("logout"))
                 ))
  )
  
  # call the shinyauthr login and logout server modules
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = "user",
    pwd_col = "password",
    sodium_hashed = TRUE,
    reload_on_logout = TRUE,
    log_out = reactive(logout_init())
  )
  
  logout_init <- shinyauthr::logoutServer(id = "logout",
                                          active = reactive(credentials()$user_auth))
  
  observeEvent(credentials()$user_auth, {
    # if user logs in successfully
    if (credentials()$user_auth) {
      # remove the login tab
      removeTab("tabs", "login")
      ####### Show User Infomation ######
      # add home tab
      #appendTab("tabs", home_tab, select = TRUE)
      # render user data output
      # output$user_data <-
      #   renderPrint({
      #     dplyr::glimpse(credentials()$info)
      #   })
      ###################################
      
      #add review tabs ----
      appendTab("tabs", review, select = TRUE)
      output$question_slider <- renderUI(
        sliderInput(
          inputId = "qnum",
          label = "題號",
          min = 1,
          max = nrow(NE[grepl(paste("\n{0,}\\b", input$chapter, "\\b\n{0,}", sep = ""),
                              Chapter)]),
          value = 1,
          step = 1,
          width = "400px"
        )
      )
      
      observeEvent(input$left, {
        updateSliderInput(session, "qnum", value = input$qnum - 1)
      })
      observeEvent(input$right, {
        updateSliderInput(session, "qnum", value = input$qnum + 1)
      })
      
      
      #content
      single_question <-
        reactive(NE[grepl(paste("\n{0,}\\b", input$chapter, "\\b\n{0,}", sep = ""),
                          Chapter)][input$qnum,])
      
      
      output$display_question <- renderUI({
        data <- single_question()
        options_values <- c(LETTERS[1:4])
        names(options_values) <- c(
          paste0("A) ", data$A),
          paste0("B) ", data$B),
          paste0("C) ", data$C),
          paste0("D) ", data$D)
        )
        
        radioButtons(
          inputId = "choice",
          label = HTML(paste0(
            data$Question, "<br>", ifelse(
              data$Image == "TRUE",
              paste0("<img src='", data$Origin, ".png' width='400'>"),
              ""
            )
          )),
          
          choices = options_values,
          selected = character(0)
        )
      })
      
      output$ori <- renderText({
        data <- single_question()
        data$Origin
      })
      
      
      
      # show answer mechanism
      v <- reactiveValues(show_answer = FALSE)
      
      output$reveal_choice <- renderText({
        data <- single_question()
        if (v$show_answer == FALSE)
          return ()
        ifelse(
          input$choice == data$Answer,
          '<span style=\"color:green\">你答對了!</span>',
          glue(
            '<span style=\"color:red\">Oops! 錯囉...答案是: {data$Answer}</span>'
          )
        )
      })
      
      observeEvent(input$choice, {
        v$show_answer <- input$choice
      })
      observeEvent(input$left | input$right , {
        v$show_answer <- FALSE
      })
      observeEvent(input$chapter , {
        v$show_answer <- FALSE
      })
      
      output$detail <- renderTable({
        if (v$show_answer == FALSE)
          return ()
        data <- single_question()
        data[, 14:18]
      })
      
      output$explanation <- renderText({
        data <- single_question()
        if (v$show_answer == FALSE)
          return ()
        data$Note
      })
      
      # add trend analysis tab----
      appendTab("tabs", trend_analysis)
      dat <- reactive({
        NE[Year >= input$trend_year[1] & Year <= input$trend_year[2]]
      })
      output$trend_plot <- renderPlot({
        dat() %>% trend("chapter.csv") %>%
          mutate(
            color = ifelse(.$MB == 1, "blue", "black"),
            name = glue("<span style='color:{color};'>{Chapter}</span>"),
            name = fct_reorder(name, qcounts)
          ) %>%  ggplot(aes(x = name,
                            y = qcounts,
                            fill = qcounts)) +
          geom_bar(stat = "identity", width = 0.8) +
          scale_fill_gradient2(low = "blue", high = "red") +
          labs(y = "Question Number", x = "") +
          guides(fill = guide_legend(title = "")) +
          theme(text = element_text(size = 20), axis.text.y = element_markdown()) +
          coord_flip()
      }, height = 800)
      
      # add testbank tab----
      appendTab("tabs", testbank_tab)
      # Powerpoint export----
      TB <- reactive({
        data <- NE[, c(9, 1, 2, 3, 4, 5, 6, 7, 12, 13, 14)]
        data <- data[Year >= input$year[1] & Year <= input$year[2]]
        if (input$ch != "All") {
          data <-
            data[grepl(paste("\n{0,}\\b", input$ch, "\\b\n{0,}", sep = ""),
                       Chapter)]
          
        }
        data
        
      })
      output$testbank <- DT::renderDataTable(DT::datatable(TB(), options = list(pageLength = 25)))
      
      
      
      output$downloadData <- downloadHandler(
        filename = function() {
          paste0(Sys.Date(), ".pptx")
        },
        content = function(file) {
          make_ppt(
            TB(),
            file,
            choices = input$testbank_rows_selected,
            show_answer = input$show_answer
          )
        }
      )
      #以下是依不同權限顯示不同內容的範例 ----
      # add data tab ----
      # appendTab("tabs", data_tab)
      # # render data tab title and table depending on permissions
      # user_permission <- credentials()$info$permissions
      # if (user_permission == "admin") {
      #   output$data_title <-
      #     renderUI(tags$h2("Storms data. Permissions: admin"))
      #   output$table <- DT::renderDT({
      #     dplyr::storms[1:100, 1:11]
      #   })
      # } else if (user_permission == "standard") {
      #   output$data_title <-
      #     renderUI(tags$h2("Starwars data. Permissions: standard"))
      #   output$table <- DT::renderDT({
      #     dplyr::starwars[, 1:10]
      #   })
      # }
    }
  })
}

shinyApp(ui, server)