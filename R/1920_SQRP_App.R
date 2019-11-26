
library(silounloadr)
library(tidyverse)
library(lubridate)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(devtools)
library(kippcolors)
library(formattable)
library(googleCloudStorageR)
library(bigrquery)

# App UI

ui_track_student <- dashboardPage(

  dashboardHeader(title = "On-Track Status Finder"),

  dashboardSidebar(selectizeInput(inputId = "school",
                                  label = "Select School",
                                  choices = c("KAMS", "KOA", "KBCP", "KAC", "KAP", "KOP", "KACP"),
                                  selected = NULL,
                                  multiple = FALSE),

                   selectizeInput(inputId = "grade",
                                  label = "Select Grade Level",
                                  choices = c(3, 4, 5, 6, 7, 8),
                                  selected = NULL,
                                  multiple = TRUE),

                   # selectizeInput(inputId = "homeroom",
                   #                label = "Homeroom",
                   #                choices = track_student$home_room,
                   #                selected = NULL,
                   #                multiple = TRUE),

                   # conditionalPanel(condition = "input.school == any(schools$schoolabbreviation)",
                   #                  uiOutput("grade_level_ui")),
                   #
                   # conditionalPanel(condition =  "input.grade_level == any(c(3:8))",
                   #                  uiOutput("home_room_ui")
                   # ),

                   selectizeInput(inputId = "track",
                                  label = "Track Status",
                                  choices = c("On-Track", "Almost On-Track", "Near On-Track", "Far from On-Track", "Off-Track"),
                                  selected = c("On-Track", "Almost On-Track", "Near On-Track", "Far from On-Track", "Off-Track"),
                                  multiple = TRUE)),
  dashboardBody(
    # tags$head(
    #   tags$link(rel = "stylesheet",
    #             type = "text/css",
    #             href = "kippcolors_blue.css" )),
    # tags$head(tags$link(rel = "stylesheet",
    #                     type = "text/css",
    #                     href = "kippcolors_green.css")),
    fluidRow(box("Select your school from the dropdown menu, then filter by grade or Track Status.")),
    fluidRow(
      column(width = 6,
             DTOutput(outputId = "table")))
  )
)

server_track_student <- function(input, output) {
#
  # output$grade_level_ui <- renderUI({
  #   school_grade_levels <- homeroom %>%
  #     ungroup() %>%
  #     filter(schoolabbreviation %in% input$school) %>%
  #     select(grade_level) %>%
  #     arrange(grade_level)
  #
  #   grade_levels <- school_grade_levels$grade_level %>%
  #     unique()
  #
  #   selectInput("grade_level",
  #               "Grade",
  #               choices = grade_levels,
  #               selected = grade_levels[1],
  #               multiple = FALSE)
  # })
  # #
  # output$home_room_ui <- renderUI({
  #   school_home_rooms <- homeroom %>%
  #     ungroup() %>%
  #     filter(schoolabbreviation %in% input$school,
  #            grade_level %in% input$grade_level) %>%
  #     select(home_room) %>%
  #     arrange(home_room)
  #
  #   home_room <- school_home_rooms$home_room %>%
  #     unique()
  #
  #   selectInput("home_room",
  #               "Home Room",
  #               choices = home_room,
  #               selected = NULL,
  #               multiple = TRUE)
  # })
  #
  # grade_level_d <- reactive({
  #   input$grade_level
  # }) %>%
  #   debounce(2000)
  #
  # home_room_d <- reactive({
  #   input$home_room
  # }) %>%
  #   debounce(2000)
  #
  # student_status <- reactive({
  #   student_table <- track_student %>%
  #     filter(schoolabbreviation %in% input$school,
  #            grade_level %in% grade_level_d(),#input$grade_level,
  #            home_room %in% home_room_d() %>%
  #              filter(schoolabbreviation == input$school,
  #                     # grade_level %in% input$grade,
  #                     # home_room %in% input$homeroom,
  #                     current_track_status %in% input$track) %>%
  #              select("Student Number" = student_number,
  #                     "First Name" = first_name,
  #                     "Last Name" = last_name,
  #                     "Grade Level" = grade_level,
  #                     "School Abbreviation" = schoolabbreviation,
  #                     "Homeroom" = home_room,
  #                     ADA,
  #                     "Attendance Bucket" = attendance_bucket,
  #                     "GPA" = gpa,
  #                     "GPA Bucket" = gpa_bucket,
  #                     "Q1 Track Status" = current_track_status,
  #                     "Point Contribution to Average" = points)) #input$home_room
  # })
#

  # output$table_n_grades <- DT::renderDT(n_grades(),
  #                                       colnames = c('Student Number', "First Name", "Last Name", '# of Grades'),
  #                                       rownames = FALSE,
  #                                       #server = FALSE,
  #                                       filter = "top",
  #                                       extensions = 'Buttons',
  #                                       options = list(
  #                                         dom = 'Bfrtip',
  #                                         buttons =
  #                                           list('copy', 'print', list(
  #                                             extend = 'collection',
  #                                             buttons = c('csv', 'excel', 'pdf'),
  #                                             text = 'Download'
  #                                           )),
  #                                         searching = FALSE,
  #                                         paging = FALSE,
  #                                         language = list(
  #                                           zeroRecords = "Select one or more homerooms.",
  #                                           infoEmpty = "Select one or more homerooms")
  #                                       )
  # )
  #
#

        output$table <- renderDT ({
        table <- track_student %>%
          filter(schoolabbreviation == input$school,
                 grade_level %in% input$grade,
                # home_room %in% input$homeroom,
                 current_track_status %in% input$track) %>%
          select("Student Number" = student_number,
                 "First Name" = first_name,
                 "Last Name" = last_name,
                 "Grade Level" = grade_level,
                 "School Abbreviation" = schoolabbreviation,
                 "Homeroom" = home_room,
                 ADA,
                 "Attendance Bucket" = attendance_bucket,
                  "GPA" = gpa,
                 "GPA Bucket" = gpa_bucket,
                 "Q1 Track Status" = current_track_status,
                 "Point Contribution to Average" = points)


        return(as.datatable(formattable(table, list(
          "ADA" = formatter("span",
                            style = x ~ style(color = case_when(x <= 85 ~ "red",
                                                               x > 85 & x < 97.5 ~ "orange",
                                                               x >= 97.5 ~ "green"),
                                             font.weight = "bold")),
          "Attendance Bucket" = formatter("span",
                                          style = x ~ style(color = case_when(x == "< 85 ADA" ~ "red",
                                                                              x == ">= 85 and < 87.5 ADA"~ "orange",
                                                                              x == ">= 87.5 and < 90 ADA"~ "orange",
                                                                              x == ">= 90 and < 92.5 ADA"~ "orange",
                                                                              x == ">= 92.5 and < 95 ADA"~ "orange",
                                                                              x == ">= 95 and < 97.5 ADA"~ "orange",
                                                                              x == ">= 97.5 ADA"~ "green"),
                                                            font.weight = "bold")),
          "GPA" = formatter("span",
                            style = x ~ style(color = case_when(x < 2 ~ "red",
                                                                x >= 2 & x < 3.5 ~ "orange",
                                                                x >= 3.5 ~ "green"),
                                              font.weight = "bold")),
          "GPA Bucket" = formatter("span",
                                   style = x ~ style(color = case_when(x == "Below 2" ~ "red",
                                                                       x == "2.0 < 2.5"| x == "2.5 < 3.0" | x == "3.0 < 3.5" ~ "orange",
                                                                       x == "3.5 - 4.0" ~ "green"),
                                                     font.weight = "bold"))
        ))))
       },
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = list('copy', 'print', list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Download'))),
        rownames = FALSE
      )}




shinyApp(ui = ui_track_student, server = server_track_student)



# Sheets for schools

track_student_koa <- track_student %>%
  filter(schoolabbreviation == "KOA") %>%
  select('Student number' = student_number,
         First = first_name,
         Last = last_name,
         Grade = grade_level,
         Homeroom = home_room,
         ADA,
         'Attendance Bucket' = attendance_bucket,
         GPA = gpa,
         'GPA Bucket' = gpa_bucket,
         'Current Track Status' = current_track_status,
         Points = points) %>%
  arrange(Homeroom)

write_csv(x = track_student_koa, path = "/Users/carolinekinnen/Downloads/Q1_Track_Student_KOA.csv")






