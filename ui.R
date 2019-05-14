#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# load libraries -----------------------------------
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinycustomloader)
library(shinycssloaders)
library(magrittr)
library(plyr)
library(dplyr)
  #axial = FALSE
# Titles -----------------------------------
  title = "LLMapR"
  info = "HowTo"
  ds = "Data setup"
  hexmaps = "pLLP maps"
  freqs = "Distributions"
  stats = "Statistics"
  statopts = "options"
  buggy = "Bug reporter"
# Vectors -----------------------------------
  labels <- c("stage", "group", "id", "date", "none")
  sum_vars <- c("stage", "group", "id", "date", "none")
# Define Header -----------------------------------
  dbHeader <- dashboardHeader(title = title,
                              tags$li(a(href = 'https://user.uni-frankfurt.de/~lecaudey/', 
                                        target="_blank",
                                        img(src = 'hex1.png', height = "41px"),
                                        style = "padding-top:5px; padding-bottom:5px;"),
                                      class = "dropdown"))
# variables -----------------------------------
  

  vars <- c("DCMean..unit.", "sav", "phi", "detect", "w.detect", "roset")
# define ui ----------------------------------------------------------------------
  dashboardPage(
  ## title ----------------------------
  dbHeader,
  ## sidebar ----------------------------
    dashboardSidebar(
      #sidebarSearchForm("test", label = "Search...", icon = shiny::icon("search")),
      selectInput(inputId = "dataset",
                  label = "Choose dataset:",
                  choices = c("sample data", "my data")),
      sidebarMenu(
        menuItem(info, tabName = "howto", icon = icon("clipboard")),
        menuItem(ds, tabName = "data_upload", icon = icon("upload")),
        menuItem(hexmaps, tabName = "hexmaps", icon = icon("layer-group")),
        menuItem(freqs, tabName = "freq", icon = icon("chart-area")),
        menuItem(stats, startExpanded = TRUE, icon = icon("brain"),
                 menuSubItem("Counts", tabName = "counts"),
                 menuSubItem("Summaries", tabName = "sum_stats"),
                 menuSubItem("Violins", tabName = "violins"),
                 menuSubItem("Scatter", tabName = "scatter"),
                 #menuSubItem("Correlations", tabName = "corrs"),
                 menuSubItem("PCA", tabName = "pca"),
                 menuSubItem("t-SNE", tabName = "tsne"),
                 menuItem(statopts, startExpanded = TRUE, icon = icon("eye"), 
                          radioButtons("sumstat", "summary function", selected = "median", inline = F,
                                      choices = c("median", "mean", "min", "max")),
                          selectInput("stattestpair", label = "test statistic", selected = "wilcox.test",
                                      c("Mann-Whitney" = "wilcox.test",
                                        "Students-T" = "t.test")),
                          selectInput("statlabel", label = "test label", selected = "..p.signif..",
                                      c("***" = "..p.signif..",
                                        "value" = "..p.format..")),
                          sliderInput("statlabelsize", label = "label size", min = 3, max = 8, value = 6, step = 1, ticks = F)
                 )
        )
      ),
      #selectInput("format", "Report format", c("PDF", "HTML", "Word")),
      #textInput(inputId = "caption",
      #          label = "Caption:",
      #          value = "data caption"),
      sidebarMenu(
        menuItem(buggy, tabName = "buggy", icon = icon("bug"))
      )
    ),
  ## body ----------------------------
    dashboardBody(
      includeCSS("styles.css"),
      tabItems(
        ### howto ----------------------------
        tabItem(tabName = "howto",
                fluidRow(
                  box(background = "yellow", solidHeader = TRUE, title="How To", width = 6,
                      includeHTML("html/instr.html"))),
                fluidRow(
                  box(status = "primary", solidHeader = TRUE, title="1 Data setup", width = 9,
                      includeHTML("html/info1.html")),
                  box(status = "primary", solidHeader = TRUE, title="2 pLLP Maps", width = 5,
                      includeHTML("html/info2.html")),
                  box(status = "info", title="mapping pLLP single cell variables", width = 4, height = 170,
                      tags$style(
                        type="text/css"),
                      imageOutput("stack")),
                  box(status = "primary", solidHeader = TRUE, title="3 Statistics", width = 9,
                            includeHTML("html/info4.html"))
        )),
        tabItem(tabName = "buggy",
                fluidRow(
                  box(status = "primary", solidHeader = TRUE,
                      tags$iframe(id = "googleform",
                                  src = "https://forms.gle/fkKj3QNsk6xHb7r29",
                                  width = "100%",
                                  height = 800,
                                  frameborder = 0,
                                  marginheight = 0)
                  )
                )
        ),
        ### dataset ----------------------------
        tabItem(tabName = "data_upload",
                fluidRow(
                  box(status = "primary", title=textOutput("datasetname"),
                      #textOutput("datasetname"),
                      tableOutput("data_sum"),
                      downloadButton("downloadData", "Download")
                  ),
                  box(background = "yellow", title="Instructions", solidHeader = TRUE,
                      includeHTML("html/data_instr.html")),
                  box(status = "warning", title="Define labels", solidHeader = TRUE,
                      includeHTML("html/data_labels.html"),
                      column(width=3, selectInput("lab_1", label = "#1", selected = labels[1], choices = labels)),
                      column(width=3, selectInput("lab_2", label = "#2", selected = labels[2], choices = labels)),
                      column(width=3, selectInput("lab_3", label = "#3", selected = labels[3], choices = labels)),
                      column(width=3, selectInput("lab_4", label = "#4", selected = labels[4], choices = labels))
                      ),
                  box(status = "warning", title="Upload data", solidHeader = TRUE,
                      fileInput(inputId="data_upload", label = "select files",
                                multiple = TRUE, buttonLabel = "browse", placeholder = "'.csv' / '.txt'"),
                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                      )
                  )
        ),
        ### hexmaps ----------------------------
        tabItem(tabName = "hexmaps",
                fluidRow(
                  box(status = "primary", solidHeader = TRUE, width = 9,
                      plotOutput("hexbins") %>% withLoader(type="image", loader="loader.gif"),
                      downloadButton("downloadPlot", "Download Plot")
                  )
                ),
                fluidRow(
                    box(h4("Variable"), width = 3, status = "warning",
                      selectInput("hex_var", label = "choose from dropdown", choices = NULL, selected = NULL),
                      includeHTML("html/variable.html")
                    ),
                    box(h4("Mapping coordinates"), width = 3, status = "warning", height = 600,
                      selectInput("pointx", label = "X coordinate:", selected = "cxn",
                                  c("centroid"="cxn",
                                    "apical feret"="fx1N",
                                    "basal feret"="fx0N")
                                  ),
                      selectInput("pointy", label = "Y coordinate:", selected = "CY..unit.",
                                  c("centroid"="CY..unit.",
                                    "apical feret"="fy1array",
                                    "basal feret" = "fy0array")
                                  ),
                      includeHTML("html/hexcoord.html"),
                      tags$style(
                        type="text/css",
                        "#cell img {max-width: 100%; width: 50%; height: auto; display: block; margin-left: auto; margin-right: auto;}"
                      ),
                      imageOutput("cell")
                    ),
                    box(h4("Hexagons"), width = 3, status = "warning", height = 600,
                      selectInput("hexstat", label = "statistic", selected = "median", c("mean"="mean",
                                                                                         "median"="median")),
                      sliderInput("bin_adjust", label="size", min = 1, max = 30, value = 4, step = 0.5),
                      #includeHTML("html/hexbins.html"),
                      selectInput("colorscale", label = "color scale", selected = "jet.colors",
                                  c("jet"="jet.colors",
                                    "angles"="angle.colors")
                      ),
                      tags$style(
                        type="text/css",
                        "#scale img {max-width: 100%; width: 80%; height: auto; display: block; margin-left: auto; margin-right: auto;}"
                      ),
                      imageOutput("scale")
                    )
                  )
                ),
        ### distributions ----------------------------
        tabItem(tabName = "freq",
                fluidRow(
                  box(status = "primary", solidHeader = TRUE, width = 9, 
                      column(width = 12, plotOutput("frequencies") %>% withLoader(type="image", loader="loader.gif")))
                  ),
                fluidRow(
                  box(h4("Variable"), width = 3, status = "warning",
                    selectInput("dist_var", label = "choose from dropdown", selected = "Vol..unit.", choices = NULL
                    ),
                    includeHTML("html/variable.html")),
                  box(h4("Smoothing"), width = 3, status = "warning",
                    sliderInput("bw_adjust", label = "", min = .01, max = 50, value = 20, step = 0.1),
                    includeHTML("html/distro.html")),
                  box(h4("Cleaning"), width = 3, status = "warning",
                      sliderInput("q_vol", label = "quantiles", min = 0, max = 1, value = c(0, 1), step = .01))
                  )
        ),
        ### counts ----------------------------
        tabItem(tabName = "counts",
                fluidRow(
                  box(status = "primary", solidHeader = TRUE, width = 10,
                      plotOutput("counts") %>% withLoader(type="image", loader="loader.gif")
                  )
                ),
                fluidRow(
                  box(h4("Geom"), width = 5, status = "warning",
                      selectInput("count_type", label = "choose from dropdown", selected = NULL, choices = c("Boxes", "Bars"))
                  ),
                  box(h4("statistics"), width = 3, status = "warning",
                      includeHTML("html/signif.html")
                  ),
                  box(h4("filter"), width = 2, status = "warning",
                      selectInput("filter", label="", selected = "none",
                                  c("lateral1" ="lateral1", "midline"="midline", "lateral2"="lateral2", "none"="none"))
                  )
                )
        ),
        ### sum_stats ----------------------------
        tabItem(tabName = "sum_stats",
                fluidRow(
                  box(status = "primary", solidHeader = TRUE, width = 10,
                    plotOutput("sum_stats") %>% withLoader(type="image", loader="loader.gif")
                  )
                ),
                fluidRow(
                box(h4("variable"), width = 5, status = "warning",
                    selectInput("statvariable", label = "choose from dropdown", selected = NULL, choices = NULL),
                    includeHTML("html/variable.html")
                ),
                box(h4("statistics"), width = 3, status = "warning",
                    includeHTML("html/signif.html")
                ),
                box(h4("filter"), width = 2, status = "warning",
                  selectInput("filter", label="", selected = "none",
                              c("lateral1" ="lateral1", "midline"="midline", "lateral2"="lateral2", "none"="none"))
                )
                )
        ),
        ### scatterplots ----------------------------
        tabItem(tabName = "scatter",
                fluidRow(
                  box(status = "primary", solidHeader = TRUE, width = 10,
                      plotOutput("scatter") %>% withLoader(type="image", loader="loader.gif"))
                ),
                fluidRow(
                box(h4("scatter"), width = 5, status = "warning",
                    selectInput("scatvar1", label = "X variable", selected = vars[1], vars),
                    selectInput("scatvar2", label = "Y variable", selected = vars[2], vars),
                    includeHTML("html/variable.html")),
                box(h4("statistics"), width = 3, status = "warning",
                    selectInput("fitstat", "fit function", c("lm", "glm", "loess")),
                    sliderInput("scat_span", "fit span", min = .1, max = 10, value = 1, step = .1)),
                box(h4("filter"), width = 2, status = "warning",
                    selectInput("filter", label="", selected = "none",
                                c("lateral1" ="lateral1", "midline"="midline", "lateral2"="lateral2", "none"="none")))
                )
        ),
        ### correlations ----------------------------
        #tabItem(tabName = "corrs",
        #       fluidRow(
        #          box(status = "primary", solidHeader = TRUE, width = 9,
        #              plotOutput("correlations") %>% withLoader(type="image", loader="loader.gif")
        #          )
        #        ),
        #        box(h4("correlate"), width = 3, status = "warning",
        #            selectInput("corrvar1", label = "variable 1", c("ACIMajor" = "ACIMajor")),
        #            selectInput("corrvar2", label = "variable 2", c("roset" = "roset")),
        #            includeHTML("html/variable.html")
        #        ),
        #        box(h4("statistics"), width = 3, status = "warning",
        #            selectInput("corstat", "cor function", c("pearson", "kendall", "spearman"))
        #        ),
        #        box(h4("filter"), width = 2, status = "warning",
        #            selectInput("filter", label="", selected = "none",
        #                        c("lateral1" ="lateral1", "midline"="midline", "lateral2"="lateral2", "none"="none")
        #            )
        #        )
        #),
        ### pca ----------------------------
        tabItem(tabName = "pca",
                fluidRow(
                  box(status = "primary", solidHeader = TRUE, width = 8,
                      plotOutput("pca") %>% withLoader(type="image", loader="loader.gif"))
                  ),
                fluidRow(
                  box(h4("labels"), width = 3, status = "warning",
                      selectInput("complabel", NULL, selected = "show none", c("show none", "variables")))
                  )
        ),
        ### t-sne ----------------------------
        tabItem(tabName = "tsne",
                fluidRow(
                  box(status = "primary", solidHeader = TRUE, width = 10,
                      plotOutput("tsne") %>% withLoader(type="image", loader="loader.gif"))
                  )
        ),
        ### violins ----------------------------
        tabItem(tabName = "violins",
                fluidRow(
                  box(status = "primary", solidHeader = TRUE, width = 10,
                    plotOutput("violins") %>% withSpinner(type=7, size=.5, color="#3c8dbc"))
                ),
                fluidRow(
                  box(h4("variable"), width = 5, status = "warning",
                    selectInput("viol_var", label = "choose from dropdown", vars),
                    includeHTML("html/variable.html")),
                  box(h4("statistics"), width = 3, status = "warning",
                    includeHTML("html/signif.html")),
                  box(h4("filter"), width = 2, status = "warning",
                    selectInput("filter", label="", selected = "none",
                                c("lateral1" ="lateral1", "midline"="midline", "lateral2"="lateral2", "none"="none")))
                )
        )
        )
      )
    )
  #)