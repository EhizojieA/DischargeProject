# Redacted version of global.R
# Redacted by Z. Nickerson (nickerson@battelleecology.org) on 2023-10-09
# Redactions will appear as "ZN-REDACTED" with a brief description of the function of the redacted line

# Load libraries and set options
library('XML')
library('plotly')
library('shinyjs')
library('shinyalert')
library('shinycssloaders')
library('bslib')
library('shiny')
library('DT')
library('markdown')
library('rmarkdown')
library('htmltools')
library("ZN-REDACTED") #loads non-CRAN library developed by NEON to query data from internal database
library('googleCloudStorageR')
library('gargle')
options(stringsAsFactors = FALSE,
        gargle_oauth_email = TRUE,
        shiny.maxRequestSize=30*1024^2)

# GCS setup
# Retrive and pass crediantials to GCP
my_token <- gargle::token_fetch(scope='https://www.googleapis.com/auth/cloud-platform')
googleCloudStorageR::gcs_auth(token = my_token)
# setting a project name
googleCloudStorageR::gcs_global_bucket("ZN-REDACTED") # Connects application to internal GCS bucket to which the app pushes data files
# Get all current files in bucket
currGCSFiles <- googleCloudStorageR::gcs_list_objects()

