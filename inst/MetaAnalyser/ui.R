datasets <- c("magnesium", "catheter", "aspirin", "symmetric")
datasets.title <- c("Magnesium", "Catheter", "Aspirin", "Symmetric")
userdataname <- if (exists("userdataname", envir = MetaAnalyser:::.dat_env))
                    get("userdataname", envir = MetaAnalyser:::.dat_env) else NULL
is.userdata <- !is.null(userdataname) && (!userdataname %in% datasets)
owndata.idx <- length(datasets) + 1 + is.userdata
data.choices <- seq_len(owndata.idx)
names(data.choices) <- c(datasets.title, "Upload your own...",
                         if (is.userdata) userdataname else NULL)
data.default <- if (!is.null(userdataname) && userdataname %in% datasets) match(userdataname, datasets) else 1
if (is.userdata) data.default <- length(datasets) + 2

## Fixme add user data to choice list if supplied 

shinyUI(fluidPage(
    includeCSS('style.css'),
    titlePanel("The Meta-Analyser"),
    sidebarPanel(
        div("The Meta-Analyser shows a meta-analysis as a set of weights on scales."),
        div("Each study in the analysis is represented as a weight, or a point in the graph."),
        div("The \"scales\" are balanced around the pooled estimate."),
        div("Click on a point to exclude it from the analysis, and watch the scales rebalance."),
        div("Hover over the graph for more explanation."),
        selectInput("select", label = h4("Choose a dataset or upload your own"),
                    choices = data.choices, selected = data.default),
        conditionalPanel(
            condition = sprintf("input.select == %s", length(datasets)+1),
            fileInput('data',
                      'Choose CSV file to upload. Needs exactly three columns giving study name, estimate and standard error in that order.',
                      accept = c(
                      'text/csv',
                      'text/comma-separated-values',
                      'text/tab-separated-values',
                      'text/plain',
                      '.csv',
                      '.tsv'
                      )),
            htmlOutput("filechoiceui"),
            htmlOutput("varprompt")
            ),
        htmlOutput("invisible"),
        div(class="row-fluid",
            ## fudge to align the reset buttons beside the input box
            tags$style(type='text/css', "button#resetresd { margin-bottom: -52px; }"),
            tags$style(type='text/css', "button#zeroresd { margin-bottom: -52px; }"),
            h4("Random effects standard deviation"),
            div(style="display:inline-block;",numericInput("resd","", value=0, step=0.01)),
            div(style="display:inline-block;",actionButton("resetresd", "Reset to full data value")),
            div(style="display:inline-block;",actionButton("zeroresd", "Set to zero"))
            ),
        htmlOutput("Isq"),
        div("The amount of blue-coloured area on each point is the weight assigned to the study in the pooled estimate."),
        div("The size of the hole in each point is the difference between the fixed-effects and random-effects weight."),
        checkboxInput("egger","Egger transform for small-study bias",value=FALSE),
        checkboxInput("show_scales","Show summary as scales",value=TRUE),

        radioButtons("ytype", h4("y-axis definition"),
                     c("Percentage weight" = "perc",
                       "Inverse variance (absolute weight)" = "var")),

        htmlOutput("yslider"),
#        sliderInput("yrange", label = h4("y-axis range"), min = 0,
#                    max = 100, value = c(0, 100)),

        sliderInput("pointsize", label = h4("Point size adjustment"), min = 0,
                    max = 5, value = 1, step=0.1)
    ),
    mainPanel(
        ggvisOutput("ggvis"),
        htmlOutput("pooled"),
        hr(),
        DT::dataTableOutput("datatables"),
        h4("Add study to current data"),
        div(class="row-fluid",
            tags$style(type='text/css', "button#submitnewdata { margin-bottom: -62px; }"),
            div(style="display:inline-block;",textInput("newname", label="Name", value = "")),
            div(style="display:inline-block;",numericInput("newest", label="Estimate", value = "")),
            div(style="display:inline-block;",numericInput("newse", label="SE", value = "")),
            div(style="display:inline-block;",actionButton("submitnewdata", "Add"))
            )
    )
))
