# Redacted version of ui.R
# Redacted by Z. Nickerson (nickerson@battelleecology.org) on 2023-10-09
# Redactions will appear as "ZN-REDACTED" with a brief description of the function of the redacted line

ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),
  shinyalert::useShinyalert(),
  theme = bslib::bs_theme(version = 4, bootswatch = "yeti"),
  title = "Discharge ADCP File Uploader - Shiny Application",
  
  # The link image on the top right of the page. 
  # Allows a user to immediately travel back to another site, can be changed later to a more appropriate site.
  tags$a(
    href= 'https://www.neonscience.org/',
    tags$img(src = 'NSF-NEON-BATTELLE-LOGO-COLOR.png', height = '45px',
             align = 'right'), 
    target = "_blank"),      
  
  htmltools::h1( htmltools::strong("Discharge ADCP File Uploader - Shiny Application")
  ), 
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      
      #File upload button present on the left pane of the webpage, allows user input in the form of uploading all ADCP files.
      shiny::fileInput('fileUpload', 
                       label = "Upload files here", 
                       multiple = TRUE,
                       buttonLabel = "Browse Files",
                       placeholder = "No files chosen"),
      
      #Text directions that appear within the left pane of the web page, instructing users in how to correctly use the application.
      htmltools::p('All data files, both raw (WinRiverII outputs) and processed (Q-Rev outputs), from a single bout must be uploaded together. Upload ADCP discharge data by either clicking the "Browse" button to search for files on your computer or dropping all files into the Browse button tab.'), 
      htmltools::strong("The following files are required:"),
      htmltools::br(),
      htmltools::p("- (1) .mmt file (produced by WinRiver II software)"), 
      htmltools::p("- All associated .PD0 and .PD0.nc files (produced by WinRiver II software)"), 
      htmltools::p("- (1) .xml file (produced by Q-Rev software)"),
      htmltools::p("- (1) .mat file (produced by Q-Rev software)"),

      shinyjs::hidden(actionButton('download', 'Download Ingest Table')
      ), 

      # First acknowledgement question. If answered yes then, the user may proceed. If not, an error is prompted.
      # Ensures that user goes through acknowledgement process.
      shinyjs::hidden(shiny::radioButtons("dataReview","Have you conducted QAQC on the ingest table and plotted data?",
                                          choices = c('Yes', 'No'),
                                          selected = character(0)
      ) 
      ), 

      # The second acknowledgement question. If answered yes then, the user may proceed. If not, an error is prompted.
      # Initially hidden so user is required to go through the intended steps of acknowledgement.
      shinyjs::hidden(shiny::radioButtons('zipAck', 
                                          'Are you ready to generate a data package and push it to GCS?
                                          Note: This is an irreversible action that will push a data package (compressed zip file) to a public-access GCS bucket!',
                                          choices = c('Yes', 'No'),
                                          selected = character(0)
      )
      ),

      # A link when clicked, shows the hidden buttons for downloading the Ingest Table and the GCS link.
      # Initially hidden so user is required to go through the intended steps of acknowledgement.
      shinyjs::hidden(shiny::actionButton('gcsLink',
                                          label = 'Upload Data to GCS')
      ), 
      # Download button widget, once clicked after initially uploading documents will allow the user to save the Ingest Table as a .csv.
      shinyjs::hidden(shiny::downloadButton('fileDownload', 'Download Ingest Table')
      ), 
      htmltools::br(),
      htmltools::br(),
      
      # A link when clicked, will open a new tab on the GCS site.
      # Useful so that a user may immediately travel to the site instead of manually opening a tab and typing the url.
      shinyjs::hidden(shiny::actionLink('fileUploader',
                                        label = 'Open NEON SOM',
                                        onclick = 'window.open("ZN-REDACTED", "_blank")', # Provides app user with a link the NEON internal database
                                        class = 'btn-primary btn-lg') 
      ) 
    ),
    
    shiny::mainPanel( 
      shiny::tabsetPanel(type= 'pill',
                         shiny::tabPanel('Data and Plot',
                                         
                                         # A data table output wrapped within a spinner, will present a loading indicator when rendering the Ingest Table.
                                         # Notifies a user that a process is being performed and that application has not crashed, a form of system feedback.
                                         shinycssloaders::withSpinner(DT::dataTableOutput('ingestTable',width="100%") 
                                         ),
                                         
                                         # A plotly output wrapped within a spinner, will present a loading indicator when rendering plot output. 
                                         # Notifies a user that a process is being performed and that application has not crashed, a form of system feedback.
                                         shinycssloaders::withSpinner(plotly::plotlyOutput('graph', 
                                                                                           width = '100%',
                                                                                           height = '100%',
                                                                                           inline = TRUE
                                         ) 
                                         ) 
                         ),
                         
                         shiny::tabPanel('About the App',
                                         
                                         # Populates the R markdown file within the About the App tab.
                                         # Allows user to access a markdown file containing a thorough explanation of what the app performs and common errors that may occur.
                                         shiny::includeMarkdown('markdown.Rmd')
                         ) 
      ) 
    ) 
  ) 
) 
