# satolab-titration-helper
# Load R packages
library(shiny)
library(bslib)
library(tidyverse)
library(readxl)
library(ggprism)
theme_set(theme_prism())

# github link
github_link <- "https://github.com/mstanley-yo/satolab-titration-helper"

# ggtitration - plot titration & calculate dilutions
ggtitration <- function(data, targetvolume) {
    # process into three columns: titre, virus, rlu
    df_titration <- data %>%
        mutate(titre = as.character(c(10/2^(0:6),0))) %>%
        pivot_longer(where(is.numeric), 
                     names_to = "virus", 
                     values_to = "rlu") %>%
        mutate(virus = str_replace(virus, "\\..*", "")) %>%
        mutate(titre = as.numeric(titre))
    
    # get virus names from `virus`
    virus_names <- df_titration %>%
        distinct(virus) %>%
        pull()
    
    # function to calculate initial volume to dilute
    calculate_titre <- function(virus_name) {
        cf <- df_titration %>%
            filter(virus == virus_name) %>%
            lm(rlu ~ titre, .) %>%
            coef()
        
        return((targetvolume * 1e4 - as.numeric(cf[1])) / as.numeric(cf[2]))
    }
    
    # map and get output
    output <- map_vec(virus_names, calculate_titre) %>%
        set_names(virus_names)
    
    # plot
    round_volumes <- function(vol) {
        case_when(
            vol < 20 ~ round(vol, 2), # p20
            vol > 1000 ~ round(vol, 0), # p1000
            .default = round(vol, 1) # p200
        )
    }
    
    caption_text <- paste0(
        "To get a titre of 1e4 RLU/uL, dilute to ", targetvolume, " uL:\n",
        paste0(names(output), ": ", round_volumes(output), " uL", 
               collapse = " \n ")
    )
    
    df_titration %>%
        ggplot(aes(x = titre, y = rlu, colour = virus, group = virus)) +
        geom_point() +
        geom_smooth(method = "lm", se = F) +
        labs(caption = caption_text,
             x = "Titre (uL)",
             y = "RLU") +
        theme(legend.position = "bottom",
              plot.caption = element_text(size = 18))
}

# ui function #####
ui <- page_fluid(
    theme = bs_theme(bootswatch = "flatly"),
    
    tags$head(
        tags$title("Pseudovirus Titration Calculator")  
    ),
    tags$h3(
        "Pseudovirus Titration Calculator", 
        class = "text-primary", 
        style = "margin-top: 15px;margin-bottom: 15px;"
    ),
    layout_columns(
        col_widths = c(6, 6),  # 6/12 = half page for each column
        
        # --- Left column: inputs + example image ---
        bslib::card(
            h4("Please input results and target volume:"),
            fileInput(
                "result_input", 
                "Result input (.xlsx)", 
                accept = ".xlsx"
            ),
            numericInput(
                "targetvolume_input", 
                "Target volume at 1000 RLU/uL (uL)", 
                value = 5000, 
                min = 0
            ),
            h4("Results file example:"),
            img(
                src = "ggtitration_example.png", 
                width = "100%", 
                alt = "Example input format"
            ),
            br(),
            p("Written in R Shiny by Maximilian Stanley Yo."),
            p("Follow development here: ",
              tags$a(
                  "GitHub Repository", 
                  href = github_link,
                  target = "_blank")
            )
        ),
        
        # --- Right column: plot output ---
        bslib::card(
            h4("Titration plot:"),
            plotOutput("plot_output", height = "500px")
        )
    )
)

# server function #####
server <- function(input, output) {
    output$plot_output <- renderPlot({
        # only render the plot if it can
        validate(
            need(input$result_input != "", 
                 "Please upload a result file."),
            need(input$targetvolume_input != "", 
                 "Please input a target volume.")
        )
        
        # render the plot using ggtitration()
        ggtitration(
            read_excel(input$result_input$datapath),
            input$targetvolume_input
        )
    })
}


# Create Shiny object #####
shinyApp(ui = ui, server = server)
