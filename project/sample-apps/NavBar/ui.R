library(shiny)
library(shinythemes)

vitamins <- readRDS('../data/vitamins.rds')
amino.acids <- readRDS('../data/amino.acids.rds')
lipid.components <- readRDS('../data/lipid.components.rds')
minerals <- readRDS('../data/minerals.rds')
proximates <- readRDS('../data/proximates.rds')
foods <- readRDS('../data/foods.rds')


shinyUI(navbarPage("USDA Database Visualize Tool",
                   theme = shinytheme("flatly"),
                   tabPanel("Introduction",
                            h2('USDA National Nutrient Database for Standard Reference'),
                            h3('Release 28 (2015)'),
                            br(),
                            h4('Introduction'),
                            p('The USDA National Nutrient Database for Standard Reference (SR) is the major 
                              source of food composition data in the United States. 
                              It provides the foundation for most food composition databases in the public and 
                              private sectors. As information is updated, new versions of the database are released. 
                              This version, Release 28 (SR28), contains data on 8,789 food items and up to 150 food 
                              components. It replaces SR27, initially issued in August 2014 and revised in May 2015.
                              The data source can be found ', 
                              a(href='http://www.ars.usda.gov/ba/bhnrc/ndl', 'here'),'.'),
                            br(),
                            h4('Database Content'),
                            p('The database consists of several sets of data: food descriptions, 
                              nutrients, weights and measures, footnotes, and sources of data. This visualization
                              tool mainly focus on food description and nutrients data. Nutrient Data contains 
                              mean nutrient values per 100 g of the edible portion of food.'),
                            p('There are 8,789 different kinds of food in the database, and they are all included
                              in this visualization tool. The original data source has more than 200 nutrients, this tool only
                              included 150 main nutrients, and devided them into five categories: ',
                              strong('Proximates, ', 'Vitamins, ', 'Minerals, ', 'Lipid Components, '), 'and',
                              strong('Amino Acids'),'.')
                            
                   ),
                   navbarMenu("Plots",
                            tabPanel('Bar Chart',
                              sidebarLayout(
                                position='right',
                                sidebarPanel(
                                  #'input.dataset === "foods"',
                                  selectizeInput(
                                    'show_vars6', 'Nutrients:', 
                                    choices = names(foods[,-c(1,2,3)]), multiple = FALSE
                                  ),
                                  checkboxGroupInput(
                                    'show_vars7', 'Food Groups to compare:', 
                                    unique(foods$food_group), selected = unique(foods$food_group)[c(1:5)]
                                  )
                                ),
                                mainPanel(
                                  plotOutput("plot1")
                                )
                              )
                            ),
                            tabPanel('Cluster',
                                     sidebarLayout(
                                       position='right',
                                       sidebarPanel(
                                         #'input.dataset === "foods"',
                                         selectInput('xcol', 'X Variable', names(foods)[-c(1,2,3)],
                                                     selected='Energy_kCal'),
                                         selectInput('ycol', 'Y Variable', names(foods)[-c(1,2,3)],
                                                     selected=names(foods)[[9]]),
                                         numericInput('clusters', 'Cluster count', 3,
                                                      min = 1, max = 25)
                                       ),
                                       mainPanel(
                                         plotOutput("plot2"),
                                         fluidRow(splitLayout(tableOutput('cluster')))
                                       )
                                     )
                            )
                            
                   ),
                   navbarMenu("Nutrient Tables",
                              tabPanel('Proximates',
                                       sidebarLayout(
                                         position='right',
                                         sidebarPanel(
                                           checkboxGroupInput(
                                             'show_vars1', 'Columns in proximates to show:', 
                                             names(proximates), selected = names(proximates)[c(1:4)]
                                           )
                                         ),
                                         mainPanel(
                                           fluidRow(splitLayout(DT::dataTableOutput('mytable1')))
                                           )
                                         )
                                       ),
                              tabPanel('Minerals',
                                       sidebarLayout(
                                         position='right',
                                         sidebarPanel(
                                           checkboxGroupInput(
                                             'show_vars2', 'Columns in minerals to show:', 
                                             names(minerals), selected = names(minerals)[c(1:4)]
                                           )
                                         ),
                                         mainPanel(
                                           fluidRow(splitLayout(DT::dataTableOutput('mytable2')))
                                         )
                                       )
                              ),
                              tabPanel('Vitamins',
                                       sidebarLayout(
                                         position='right',
                                         sidebarPanel(
                                           checkboxGroupInput(
                                             'show_vars3', 'Columns in vitamins to show:', 
                                             names(vitamins), selected = names(vitamins)[c(1:4)]
                                           )
                                         ),
                                         mainPanel(
                                           fluidRow(splitLayout(DT::dataTableOutput('mytable3')))
                                         )
                                       )
                              ),
                              tabPanel('Lipid Components',
                                       sidebarLayout(
                                         position='right',
                                         sidebarPanel(
                                           checkboxGroupInput(
                                             'show_vars4', 'Columns in lipid components to show:', 
                                             names(lipid.components), selected = names(lipid.components)[c(1:4)]
                                           )
                                         ),
                                         mainPanel(
                                           fluidRow(splitLayout(DT::dataTableOutput('mytable4')))
                                         )
                                       )
                              ),
                              tabPanel('Amino Acids',
                                       sidebarLayout(
                                         position='right',
                                         sidebarPanel(
                                           checkboxGroupInput(
                                             'show_vars5', 'Columns in amino acids to show:', 
                                             names(amino.acids), selected = names(amino.acids)[c(1:4)]
                                           )
                                         ),
                                         mainPanel(
                                           fluidRow(splitLayout(DT::dataTableOutput('mytable5')))
                                         )
                                       )
                              )
                   ),
                   navbarMenu("More",
                              tabPanel("About",
                                       br(),
                                       br(),
                                       p('Zoe Wang'),
                                       p('University of San Francisco'),
                                       p('Aug, 2016')
                              ),
                              tabPanel("Download Datasets",
                                       sidebarLayout(
                                         position='right',
                                         sidebarPanel(
                                           selectInput("dataset", "Choose a dataset:", 
                                                       choices = c("Proximates", "Vitamins", "Minerals",
                                                                   "Lipid Components", "Amino Acid",
                                                                   "Whole Datasets")),
                                           downloadButton('downloadData', 'Download')
                                         ),
                                         mainPanel(
                                           fluidRow(
                                             splitLayout(tableOutput('table'))
                                             )
                                           )
                                         )
                              ),
                              tabPanel("Appendix",
                                       p('Detail data intro')
                              )
                   )
))