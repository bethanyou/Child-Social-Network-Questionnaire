# README (quick start)
# - Project tree expected:
#     SNQ Pipeline/
#       app.R
#       www/            # <- put your lab logo as www/logo.png (optional)
#                        # <- put your Qualtrics file as www/snq.qsf
#       data/           # intake & issue logs will be saved here
# - Run with: shiny::runApp()
# - If www/snq.qsf is missing, a minimal placeholder QSF is created on first run.

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(jsonlite)
  library(readr)
  library(readxl)
  library(writexl)
  library(ggplot2)
  library(stringr)
  library(shinyjs)
  library(httr2)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(lubridate)
})

# ------------------------------------------------------------------------------
# Helpers: write/copy only when content changes to avoid Shiny file-watcher loops
# ------------------------------------------------------------------------------
safe_write_lines <- function(lines, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = "safe_write_", fileext = ".tmp")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(lines, tmp, useBytes = TRUE)
  if (!file.exists(path) || !identical(readBin(tmp, what = "raw", n = file.info(tmp)$size),
                                       readBin(path, what = "raw", n = if (file.exists(path)) file.info(path)$size else 0))) {
    file.copy(tmp, path, overwrite = TRUE)
  }
}

safe_copy_if_changed <- function(from, to, overwrite = TRUE) {
  if (!file.exists(from)) return(FALSE)
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(to)) return(file.copy(from, to, overwrite = overwrite))
  src <- readBin(from, what = "raw", n = file.info(from)$size)
  dst <- readBin(to,   what = "raw", n = file.info(to)$size)
  if (!identical(src, dst)) return(file.copy(from, to, overwrite = overwrite))
  TRUE
}

# Data Dictionary assets are committed under `www/`; no staging needed for deploy

# Qualtrics Help HTML and images are committed under `www/qualtrics_help/`

# SNQ Data Processing Function (embedded directly)
process_snq_data_simple <- function(df) {
  library(dplyr)
  library(tidyr)
  library(stringr)
  
  cat("Processing SNQ data...\n")
  
  # Find the actual data rows (skip header rows)
  data_start <- which(grepl("Testing", df[[18]]))
  if (length(data_start) > 0) {
    df <- df[data_start[1]:nrow(df), ]
    cat("Data rows found starting at row:", data_start[1], "\n")
    cat("Selected rows:", nrow(df), "\n")
  }
  
  # Process ego-level data
  ego_level_network_summary <- df %>%
    mutate(
      childcare_type = ifelse(!is.na(childcare_type_8_TEXT) & childcare_type_8_TEXT != '',
                              ifelse(childcare_type == '' | is.na(childcare_type),
                                     childcare_type_8_TEXT,
                                     paste(childcare_type, childcare_type_8_TEXT, sep = ', ')),
                              childcare_type),
      child_gender = ifelse(!is.na(child_gender_3_TEXT) & trimws(child_gender_3_TEXT) != '',
                            trimws(child_gender_3_TEXT), child_gender),
      child_race = ifelse(!is.na(child_race_6_TEXT) & trimws(child_race_6_TEXT) != '',
                          ifelse(child_race == '' | is.na(child_race),
                                 trimws(child_race_6_TEXT),
                                 paste(child_race, trimws(child_race_6_TEXT), sep = ', ')),
                          child_race)
    ) %>%
    select(ChildID, child_name, EndDate, Finished, birthdate_1, age_in_months_1, child_gender, child_race, child_race_detail, child_zipcode, childcare_yn, childcare_type, name_filloutsurvey_1, child_lang) %>%
    rename(child_birthdate = birthdate_1, child_age_in_months = age_in_months_1, survey_respondent = name_filloutsurvey_1, survey_enddate = EndDate, survey_completion = Finished)
  
  cat("Ego level summary created - Rows:", nrow(ego_level_network_summary), "\n")
  
  # Process node-level data (simplified)
  node_level_long <- data.frame(
    ChildID = character(0),
    ego_age_in_months = numeric(0),
    ego_gender = character(0),
    ego_race = character(0),
    ego_language = character(0),
    node_type = character(0),
    node_index = integer(0),
    node_name = character(0),
    node_mapping_code = character(0),
    node_relationship = character(0),
    node_liveathome_or_not = character(0),
    stringsAsFactors = FALSE
  )
  
  # Extract all node types
  node_patterns <- list(
    "sibling" = "Sibling",
    "liveathome" = "Liveathome", 
    "teacher" = "Teacher",
    "schoolkid" = "Schoolkid",
    "caregiver" = "Caregiver",
    "extendedfamily" = "Extendedfamily",
    "anyoneelse" = "Anyoneelse"
  )
  
  # Activity-related node patterns (these are pre-formatted strings)
  activity_node_patterns <- list(
    "Act1Adult" = "Act1Adult",
    "Act1Kid" = "Act1Kid"
  )
  
  for (i in 1:nrow(df)) {
    child_id <- df$ChildID[i]
    child_data <- df[i, ]
    
    # Extract nodes for each pattern
    for (node_prefix in names(node_patterns)) {
      pattern_cols <- grep(paste0("^", node_prefix, "_[0-9]+_[0-9]+"), names(child_data), value = TRUE)
      
      if (length(pattern_cols) > 0) {
        for (col in pattern_cols) {
          if (!is.na(child_data[[col]]) && child_data[[col]] != "") {
            # Extract node index from column name
            node_index <- as.integer(str_extract(col, paste0(node_prefix, "_([0-9]+)_"), group = 1))
            field_num <- as.integer(str_extract(col, "_([0-9]+)$", group = 1))
            
            if (field_num == 1) {  # Name field
              node_name <- child_data[[col]]
              
              # Look for corresponding relationship field
              rel_col <- gsub("_1$", "_2", col)
              node_relationship <- if (rel_col %in% names(child_data)) child_data[[rel_col]] else ""
              
              # Determine if lives at home
              node_liveathome_or_not <- if (node_prefix %in% c("sibling", "liveathome")) "yes" else "no"
              
              # Create node_mapping_code: node_type + node_index + "_" + node_name
              node_mapping_code <- paste0(node_patterns[[node_prefix]], node_index, "_", node_name)
              
              # Add to node data
              new_node <- data.frame(
                ChildID = child_id,
                ego_age_in_months = ego_level_network_summary$child_age_in_months[match(child_id, ego_level_network_summary$ChildID)],
                ego_gender = ego_level_network_summary$child_gender[match(child_id, ego_level_network_summary$ChildID)],
                ego_race = ego_level_network_summary$child_race[match(child_id, ego_level_network_summary$ChildID)],
                ego_language = ego_level_network_summary$child_lang[match(child_id, ego_level_network_summary$ChildID)],
                node_type = node_patterns[[node_prefix]],
                node_index = node_index,
                node_name = node_name,
                node_mapping_code = node_mapping_code,
                node_relationship = node_relationship,
                node_liveathome_or_not = node_liveathome_or_not,
                stringsAsFactors = FALSE
              )
              node_level_long <- rbind(node_level_long, new_node)
            }
          }
        }
      }
    }
  }
  
  # Process pre-formatted activity node strings
  cat("Processing pre-formatted activity nodes...\n")
  for (i in 1:nrow(df)) {
    child_id <- df$ChildID[i]
    child_data <- df[i, ]
    
    # Look for columns that contain pre-formatted node strings
    for (col_name in names(child_data)) {
      cell_value <- child_data[[col_name]]
      if (!is.na(cell_value) && cell_value != "" && is.character(cell_value)) {
        # Check if this cell contains activity node patterns
        for (pattern_name in names(activity_node_patterns)) {
          if (grepl(pattern_name, cell_value)) {
            # Split by comma and process each node
            node_strings <- strsplit(cell_value, ",")[[1]]
            for (node_string in node_strings) {
              node_string <- trimws(node_string)
              if (grepl(paste0("^", pattern_name, "[0-9]+_[A-Za-z]+"), node_string)) {
                # Extract node index and name from the string
                # Format: Act1Adult1_Fiona -> index=1, name=Fiona
                match_result <- stringr::str_match(node_string, paste0("^", pattern_name, "([0-9]+)_(.+)$"))
                if (!is.na(match_result[1])) {
                  node_index <- as.integer(match_result[2])
                  node_name <- match_result[3]
                  
                  # Create node_mapping_code (it's already in the correct format)
                  node_mapping_code <- node_string
                  
                  # Add to node data
                  new_node <- data.frame(
                    ChildID = child_id,
                    ego_age_in_months = ego_level_network_summary$child_age_in_months[match(child_id, ego_level_network_summary$ChildID)],
                    ego_gender = ego_level_network_summary$child_gender[match(child_id, ego_level_network_summary$ChildID)],
                    ego_race = ego_level_network_summary$child_race[match(child_id, ego_level_network_summary$ChildID)],
                    ego_language = ego_level_network_summary$child_lang[match(child_id, ego_level_network_summary$ChildID)],
                    node_type = activity_node_patterns[[pattern_name]],
                    node_index = node_index,
                    node_name = node_name,
                    node_mapping_code = node_mapping_code,
                    node_relationship = "",  # Activity nodes don't have relationships
                    node_liveathome_or_not = "no",  # Activity nodes don't live at home
                    stringsAsFactors = FALSE
                  )
                  node_level_long <- rbind(node_level_long, new_node)
                }
              }
            }
          }
        }
      }
    }
  }
  
  cat("Node level long created - Rows:", nrow(node_level_long), "\n")
  
  # Process activity-level data (simplified)
  activity_level_long <- df %>%
    select(ChildID, starts_with("act1_")) %>%
    filter(!is.na(act1_otherkids) & act1_otherkids != "") %>%
    mutate(
      activity_id = 1,
      activity_label = "act1",
      activity_name = "Activity 1",
      activity_otherkids_yn = act1_otherkids,
      activity_otherkids_n = suppressWarnings(as.numeric(act1_otherkids_2_TEXT)),
      activity_child_age = suppressWarnings(as.numeric(act1_child_age)),
      activity_gender = act1_gender,
      activity_race = act1_race,
      activity_contact = act1_contact,
      activity_lang = act1_lang,
      activity_adults = act1_adults,
      activity_kids = act1_kids
    ) %>%
    select(
      ChildID, activity_id, activity_label, activity_name, activity_otherkids_yn,
      activity_otherkids_n, activity_child_age, activity_gender, activity_race,
      activity_contact, activity_lang, activity_adults, activity_kids
    )
  
  cat("Activity level long created - Rows:", nrow(activity_level_long), "\n")
  
  return(list(
    ego_level_network_summary = ego_level_network_summary,
    node_level_long = node_level_long,
    activity_level_long = activity_level_long
  ))
}

# ============== Admin Configuration ==============
# Use environment variable for admin password, fallback to default for development
ADMIN_PASSWORD <- Sys.getenv("SNQ_ADMIN_PASSWORD", "woodwardlab")

# ------------------------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------------------------

# Null-coalescer helper
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ------------------------------------------------------------------------------
# Ensure folders exist
# ------------------------------------------------------------------------------
dir.create("data", showWarnings = FALSE, recursive = TRUE)
dir.create("www",  showWarnings = FALSE, recursive = TRUE)

# If a project-root image.png exists, copy it to www/logo.png so it is served
if (file.exists("image.png") && !file.exists(file.path("www","logo.png"))) {
  file.copy("image.png", file.path("www","logo.png"), overwrite = TRUE)
}

# Dashboard HTML is served from `www/snq_dash/`; no extra resource path needed

# ------------------------------------------------------------------------------
# Ensure there is a QSF to serve; if missing, write a minimal placeholder
# ------------------------------------------------------------------------------
canonical_qsf_path <- file.path("www", "snq.qsf")
# If a canonical QSF is already in www/snq.qsf (committed), use it; otherwise, wait for upload
if (!file.exists(canonical_qsf_path)) {
  minimal_qsf <- list(
    SurveyEntry = list(SurveyName = "SNQ Canonical (Minimal)", SurveyID = "SV_PLACEHOLDER"),
    SurveyElements = list(
      list(
        Element = "SQ",
        PrimaryAttribute = "QID_INTRO",
        Payload = list(
          QuestionID    = "QID_INTRO",
          QuestionType  = "DB",
          DataExportTag = "intro_1",
          QuestionText  = "{{INTRO_PARAGRAPH}}"
        )
      ),
      list(
        Element = "SQ",
        PrimaryAttribute = "QID_CHILDID",
        Payload = list(
          QuestionID    = "QID_CHILDID",
          QuestionType  = "TE",
          DataExportTag = "child_id",
          QuestionText  = "Please enter child identifier: {{CHILD_ID_LABEL}}"
        )
      )
    )
  )
  writeLines(toJSON(minimal_qsf, auto_unbox = TRUE, pretty = TRUE), canonical_qsf_path)
}

# ------------------------------------------------------------------------------
# Access Survey Module
# ------------------------------------------------------------------------------

# UI Function
accessSurveyUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    # Custom CSS matching UChicago style
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Source+Sans+Pro:wght@300;400;600;700&display=swap');
        
        body {
          font-family: 'Source Sans Pro', sans-serif;
          background-color: #f8f9fa;
        }
        
        .page-header {
          text-align: center;
          margin-bottom: 3rem;
        }
        
        .logo-container {
          margin-bottom: 2rem;
        }
        
        .logo-image {
          max-width: 1200px !important;
          width: 100% !important;
          max-height: 500px !important;
          height: auto !important;
          object-fit: contain !important;
        }
        
        .main-content {
          background: white;
          border-radius: 8px;
          padding: 3rem;
          margin-bottom: 2rem;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        }
        
        .page-title {
          color: #333;
          font-size: 3rem;
          font-weight: 700;
          margin-bottom: 0.5rem;
          line-height: 1.2;
        }
        
        .page-subtitle {
          color: #333;
          font-size: 2.5rem;
          font-weight: 700;
          margin-bottom: 2rem;
          line-height: 1.2;
        }
        
        .title-underline {
          width: 80px;
          height: 4px;
          background: #8B0000;
          margin: 0 auto 2rem auto;
        }
        
        .download-section {
          background: #f8f9fa;
          border: 2px solid #8B0000;
          border-radius: 8px;
          padding: 2rem;
          text-align: center;
          margin: 2rem 0;
        }
        
        .download-title {
          color: #8B0000;
          font-size: 1.5rem;
          font-weight: 600;
          margin-bottom: 1rem;
        }
        
        .download-description {
          color: #666;
          font-size: 1.1rem;
          margin-bottom: 1.5rem;
          line-height: 1.6;
        }
        
        .download-btn {
          background: #8B0000;
          color: white;
          border: none;
          padding: 12px 30px;
          border-radius: 4px;
          font-size: 1.1rem;
          font-weight: 600;
          transition: all 0.3s ease;
          text-transform: uppercase;
          letter-spacing: 0.5px;
        }
        
        .download-btn:hover {
          background: #A00000;
          color: white;
          transform: translateY(-1px);
        }
        
        .instructions-section {
          margin-top: 3rem;
        }
        
      .section-title {
        color: #333;
        font-size: 2rem;
        font-weight: 600;
        margin-bottom: 2rem;
        text-align: center;
      }
      
      .section-title::after {
        content: '';
        display: block;
        width: 100px;
        height: 3px;
        background: #8B0000;
        margin: 0.5rem auto 0;
        border-radius: 2px;
      }
      
      .section-description {
        color: #6c757d;
        font-size: 1.1rem;
        text-align: center;
        margin-bottom: 2rem;
        max-width: 800px;
        margin-left: auto;
        margin-right: auto;
      }
        
        .instruction-step {
          background: white;
          border: 1px solid #e0e0e0;
          border-radius: 12px;
          padding: 2rem;
          margin-bottom: 1.5rem;
          transition: box-shadow 0.3s ease;
          box-shadow: 0 2px 8px rgba(0,0,0,0.05);
        }
        
        .instruction-step:hover {
          box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        }
        
        .step-header {
          display: flex;
          align-items: center;
          margin-bottom: 1rem;
          gap: 2rem;
        }
        
        .step-number {
          background: #8B0000;
          color: white;
          width: 40px;
          height: 40px;
          border-radius: 50%;
          display: flex;
          align-items: center;
          justify-content: center;
          font-weight: 700;
          flex-shrink: 0;
          font-size: 1.2rem;
        }
        
        .step-title {
          color: #333;
          font-size: 1.4rem;
          font-weight: 600;
          margin: 0;
          flex: 1;
        }
        
        .step-content {
          color: #666;
          font-size: 1rem;
          line-height: 1.6;
          margin-left: 0;
          padding-left: 0;
        }
        
        .info-callout {
          background: #e3f2fd;
          border-left: 4px solid #2196f3;
          padding: 1rem;
          margin: 1rem 0;
          border-radius: 0 4px 4px 0;
        }
        
        .warning-callout {
          background: #fff3e0;
          border-left: 4px solid #ff9800;
          padding: 1rem;
          margin: 1rem 0;
          border-radius: 0 4px 4px 0;
        }
        
        .feature-list {
          list-style: none;
          padding: 0;
          margin: 1rem 0;
        }
        
        .feature-list li {
          padding: 0.6rem 0;
          position: relative;
          padding-left: 1.8rem;
          color: #555;
          line-height: 1.5;
        }
        
        .feature-list li:before {
          content: '•';
          position: absolute;
          left: 0;
          color: #8B0000;
          font-weight: bold;
          font-size: 1.3rem;
          top: 0.6rem;
        }
      "))
    ),
    
    # Page Header
    div(class = "page-header",
      # Title Section
      h1(class = "page-title", "Access Survey"),
      div(class = "title-underline")
    ),
    
    # Main Content Container
    div(class = "container-fluid",
      
      # Instructions Section
      div(class = "main-content instructions-section",
        h2(class = "section-title", "Qualtrics Implementation Guide"),
        
        # Step 1
        div(class = "instruction-step",
          div(class = "step-header",
            div(class = "step-number", "1"),
            h3(class = "step-title", "Request Access")
          ),
          div(class = "step-content",
            p("Complete the intake form on the Get Access page to request access the CSNQ Qualtrics qsf. Our team will review your request ASAP.")
          )
        ),
        
        # Step 2
        div(class = "instruction-step",
          div(class = "step-header",
            div(class = "step-number", "2"),
            h3(class = "step-title", "Download 'qsf' Survey File")
          ),
          div(class = "step-content",
            p("Once approved, you will receive the CSNQ (.qsf) file via email. This file contains the complete survey structure ready for Qualtrics import."),
            div(class = "info-callout",
              strong("Note: "), "Please allow 1-2 business days for processing your request."
            )
          )
        ),
        
        # Step 3
        div(class = "instruction-step",
          div(class = "step-header",
            div(class = "step-number", "3"),
            h3(class = "step-title", "Access Qualtrics")
          ),
          div(class = "step-content",
            p("Log into your Qualtrics account and ensure you have permissions to create new surveys and import .qsf files."),
            div(class = "info-callout",
              strong("Note: "), "Contact your institution's Qualtrics administrator if you need import permissions."
            )
          )
        ),
        
        # Step 4
        div(class = "instruction-step",
          div(class = "step-header",
            div(class = "step-number", "4"),
            h3(class = "step-title", "Import to Qualtrics")
          ),
          div(class = "step-content",
            p("Create a new project and import the survey file:"),
            tags$ul(class = "feature-list",
              tags$li("Click 'Create Project' in Qualtrics"),
              tags$li("Select 'Survey' as project type"),
              tags$li("Choose 'From a QSF file' option"),
              tags$li("Upload the CSNQ.qsf file you received via email"),
              tags$li("Complete the import wizard")
            )
          )
        ),
        
        # Step 5
        div(class = "instruction-step",
          div(class = "step-header",
            div(class = "step-number", "5"),
            h3(class = "step-title", "Review and Deploy")
          ),
          div(class = "step-content",
            p("After import, review the survey structure and configure for your research needs:"),
            div(class = "warning-callout",
              strong("Important: "), "Review all question logic and validation rules before deploying to participants."
            )
          )
        ),
        
        # What's Included Section
        div(class = "instruction-step",
          h3(class = "step-title", style = "margin-bottom: 1rem;", "Survey Components"),
          div(class = "step-content", style = "margin-left: 0;",
            p("The CSNQ includes:"),
            tags$ul(class = "feature-list",
              tags$li("Social network mapping questions"),
              tags$li("Relationship quality assessments"),
              tags$li("Demographic measures"),
              tags$li("Pre-configured logic and validation"),
              tags$li("Mobile-responsive design")
            )
          )
        ),
        
        # Support Section
        div(class = "instruction-step",
          h3(class = "step-title", style = "margin-bottom: 1rem;", "Need Help?"),
          div(class = "step-content", style = "margin-left: 0;",
            p("For technical support:"),
            tags$ul(class = "feature-list",
              tags$li(tags$a(href = "#help", "Use the form under Help & Contact page to submit a request", 
                            onclick = "Shiny.setInputValue('go_to_help_contact', Math.random(), {priority: 'event'}); return false;",
                            style = "color: #8B0000; text-decoration: underline; font-weight: 600; cursor: pointer;")),
              tags$li("Contact your institutional Qualtrics support team"),
              tags$li(tags$a(href = "#qualtrics_help", "Review the Qualtrics Help Doc", 
                            onclick = "Shiny.setInputValue('go_to_qualtrics_help', Math.random(), {priority: 'event'}); return false;",
                            style = "color: #8B0000; text-decoration: underline; font-weight: 600; cursor: pointer;",
                            " - Complete walkthrough of survey logic and flow"))
            )
          )
        )
      )
    )
  )
}

# Server Function
accessSurveyServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    # Handle survey access request
    observeEvent(input$submitRequest, {
      # Validate inputs
      if (input$requestName == "" || input$requestEmail == "") {
        showNotification("Please provide both name and email address.", type = "warning")
        return()
      }
      
      # Validate email format
      if (!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", input$requestEmail)) {
        showNotification("Please enter a valid email address.", type = "warning")
        return()
      }
      
      # Prepare email content
      subject <- paste("QSF Request from", input$requestName)
      body <- paste(
        "Survey Access Request",
        "========================",
        "",
        "Name:", input$requestName,
        "Email:", input$requestEmail,
        "Institution:", ifelse(input$requestInstitution == "", "Not provided", input$requestInstitution),
        "",
        "This person is requesting access to the Child Social Network Questionnaire (CSNQ) due to Qualtrics import limitations.",
        "",
        "Next steps:",
        "1. Share the survey with their Qualtrics account",
        "2. Grant them edit permissions",
        "3. Instruct them to duplicate the survey",
        "4. Remove access after they confirm successful duplication",
        "",
        "Generated automatically from the CSNQ Access Survey page.",
        sep = "\n"
      )
      
      # Try to send email (Note: This requires email configuration in your Shiny server)
      tryCatch({
        # You'll need to configure email sending in your server environment
        # This is a placeholder - replace with your preferred email method
        
        # For now, we'll show a confirmation and you can manually check logs
        showNotification(
          paste("Request submitted successfully for", input$requestName, ". You will be contacted within 24-48 hours."),
          type = "default",
          duration = 10
        )
        
        # Log the request (you can check server logs)
        cat("\n=== QSF REQUEST ===\n")
        cat("Subject:", subject, "\n")
        cat("Body:\n", body, "\n")
        cat("Timestamp:", Sys.time(), "\n")
        cat("==================\n")
        
        # Clear form
        updateTextInput(session, "requestName", value = "")
        updateTextInput(session, "requestEmail", value = "")
        updateTextInput(session, "requestInstitution", value = "")
        
      }, error = function(e) {
        showNotification("There was an error submitting your request. Please try again later.", type = "error")
        cat("Email sending error:", e$message, "\n")
      })
    })
  })
}

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------
badge <- function(text, cls = "bg-secondary") tags$span(class = paste("badge", cls), text)

# Logging function for debugging
log_message <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- paste0("[", timestamp, "] [", level, "] ", message)
  
  # Write to console
  cat(log_entry, "\n")
  
  # Write to log file if it exists
  log_file <- file.path("data", "app.log")
  if (dir.exists("data")) {
    write(log_entry, file = log_file, append = TRUE)
  }
}

# Safe file operations with error handling
safe_file_operation <- function(operation, error_message = "File operation failed") {
  tryCatch({
    operation()
  }, error = function(e) {
    log_message(paste(error_message, ":", conditionMessage(e)), "ERROR")
    stop(error_message, ": ", conditionMessage(e))
  })
}

# Process SNQ data using your converted R script
process_snq_data <- function(raw_data, raw_filename) {
  if (is.null(raw_data) || nrow(raw_data) == 0) {
    stop("No data provided for processing")
  }
  
  tryCatch({
    # Create a clean temporary environment
    temp_dir <- tempdir()
    old_wd <- getwd()
    
    # Ensure we return to original directory even if error occurs
    on.exit(setwd(old_wd))
    
    # Set working directory to temp for processing
    setwd(temp_dir)
    
     # Save the input data with the exact name your script expects
     file_extension <- tolower(tools::file_ext(raw_filename %||% "csv"))
     if (file_extension == "xlsx") {
      # For Excel files, save as Excel
      writexl::write_xlsx(raw_data, "input_data.xlsx")
    } else {
      # For CSV files, save as CSV
      write_csv(raw_data, "input_data.csv")
    }
    
    # Create a wrapper script that sources your pipeline
    wrapper_script <- tempfile(fileext = ".R")
    
    wrapper_code <- paste0('
# Set up environment
suppressPackageStartupMessages({
  library(readr)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(lubridate)
})

# Read the input data - make sure this matches what your script expects
if (file.exists("input_data.xlsx")) {
  df <- readxl::read_excel("input_data.xlsx")
} else {
  df <- read_csv("input_data.csv", show_col_types = FALSE)
}

# Source your converted R script (project-relative path)
source("snq_pipeline.R.bak")

# Your script should create these objects:
# - node_level_long
# - ego_level_network_summary  
# - activity_level_long

# Save the results
if (exists("node_level_long")) {
  write_csv(node_level_long, "node_level_long.csv")
} else {
  stop("node_level_long was not created by the pipeline")
}

if (exists("ego_level_network_summary")) {
  write_csv(ego_level_network_summary, "ego_level_network_summary.csv")  
} else {
  stop("ego_level_network_summary was not created by the pipeline")
}

if (exists("activity_level_long")) {
  write_csv(activity_level_long, "activity_level_long.csv")
} else {
  stop("activity_level_long was not created by the pipeline")
}
    ')
    
    writeLines(wrapper_code, wrapper_script)
    
    # Run the wrapper script
    result <- system2("Rscript", args = wrapper_script, 
                     stdout = TRUE, stderr = TRUE, 
                     wait = TRUE)
    
    # Check for execution errors
    if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
      error_msg <- paste("R script execution failed:", paste(result, collapse = "\n"))
      stop(error_msg)
    }
    
    # Verify output files exist
    output_files <- c("node_level_long.csv", "ego_level_network_summary.csv", "activity_level_long.csv")
    missing_files <- output_files[!file.exists(output_files)]
    
    if (length(missing_files) > 0) {
      stop("Pipeline did not generate expected files: ", paste(missing_files, collapse = ", "))
    }
    
    # Read the results
    node_level_long <- read_csv("node_level_long.csv", show_col_types = FALSE)
    ego_level_network_summary <- read_csv("ego_level_network_summary.csv", show_col_types = FALSE)
    activity_level_long <- read_csv("activity_level_long.csv", show_col_types = FALSE)
    
    # Clean up temporary files
    unlink(c(wrapper_script, output_files, "input_data.csv", "input_data.xlsx"))
    
    return(list(
      node_level_long = node_level_long,
      ego_level_network_summary = ego_level_network_summary,
      activity_level_long = activity_level_long
    ))
    
  }, error = function(e) {
    # Ensure we return to original directory on error
    setwd(old_wd)
    stop("Error processing SNQ data: ", conditionMessage(e))
  })
}

# ------------------------------------------------------------------------------
# THEME (vibrant)
# ------------------------------------------------------------------------------
theme <- bs_theme(
  version = 5,
    bootswatch = "minty",
  primary = "#0a7ac4",
    secondary = "#6c757d",
    "font-size-base" = "0.875rem",
    "h1-font-size" = "2rem",
    "h2-font-size" = "1.75rem", 
    "h3-font-size" = "1.5rem",
    "h4-font-size" = "1.25rem",
    "h5-font-size" = "1.1rem",
    "h6-font-size" = "1rem",
    "line-height-base" = 1.4,
    "font-size-sm" = "0.75rem",
    "font-size-lg" = "1.125rem"
)

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
# Increase upload size limit to 50MB
options(shiny.maxRequestSize = 50*1024*1024)

ui <- page_navbar(
  id    = "nav",
  title = "SNQ Distribution Hub",
  theme = theme,

  header = tagList(
    useShinyjs(),
    # Font Awesome for crisp Retina icons
     tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
     # Ensure Bootstrap JS is available
    tags$style(HTML("
        /* Desktop scaling fixes */
        html { font-size: 0.875rem; }
        body { 
          background: linear-gradient(160deg, #f7f7f7 0%, #eaf3fb 100%); 
          font-size: 0.875rem;
          line-height: 1.4;
        }
        
        /* Control heading sizes */
        h1, .h1 { font-size: 2rem !important; font-weight: 700; }
        h2, .h2 { font-size: 1.75rem !important; font-weight: 600; }
        h3, .h3 { font-size: 1.5rem !important; font-weight: 600; }
        h4, .h4 { font-size: 1.25rem !important; font-weight: 600; }
        h5, .h5 { font-size: 1.1rem !important; font-weight: 600; }
        h6, .h6 { font-size: 1rem !important; font-weight: 600; }
        
        /* Control card and container sizes */
        .card { 
          font-size: 14px;
          border-radius: 8px;
        }
        .card-body { padding: 1.25rem; }
        .card-header { 
          padding: 0.75rem 1.25rem; 
          font-size: 1.1rem;
          font-weight: 600;
        }
        
        /* Control button sizes */
        .btn { 
          font-size: 0.875rem;
          padding: 0.5rem 1rem;
          border-radius: 0.375rem;
        }
        .btn-lg { 
          font-size: 1rem;
          padding: 0.75rem 1.5rem;
        }
        .btn-sm { 
          font-size: 0.75rem;
          padding: 0.25rem 0.5rem;
        }
        
        /* Control form elements */
        .form-control, .form-select {
          font-size: 0.875rem;
          padding: 0.5rem 0.75rem;
          border-radius: 0.375rem;
        }
        .form-label {
          font-size: 0.875rem;
          font-weight: 600;
          margin-bottom: 0.5rem;
        }
        
        /* Control spacing */
        .mb-1 { margin-bottom: 0.25rem !important; }
        .mb-2 { margin-bottom: 0.5rem !important; }
        .mb-3 { margin-bottom: 1rem !important; }
        .mb-4 { margin-bottom: 1.5rem !important; }
        .mb-5 { margin-bottom: 3rem !important; }
        
        .mt-1 { margin-top: 0.25rem !important; }
        .mt-2 { margin-top: 0.5rem !important; }
        .mt-3 { margin-top: 1rem !important; }
        .mt-4 { margin-top: 1.5rem !important; }
        .mt-5 { margin-top: 3rem !important; }
        
        .p-1 { padding: 0.25rem !important; }
        .p-2 { padding: 0.5rem !important; }
        .p-3 { padding: 1rem !important; }
        .p-4 { padding: 1.5rem !important; }
        .p-5 { padding: 3rem !important; }
        
        /* Control navbar */
        .navbar-brand { 
          font-weight: 700; 
          letter-spacing: 0.3px;
          font-size: 1.25rem;
        }
        .navbar-nav .nav-link {
          font-size: 0.875rem;
          padding: 0.5rem 1rem;
        }
        
        /* Control container max-widths */
        .container-fluid { max-width: 1400px; margin: 0 auto; }
        .container { max-width: 1200px; }
        
        /* Control grid spacing */
        .row { margin-left: -0.75rem; margin-right: -0.75rem; }
        .row > * { padding-left: 0.75rem; padding-right: 0.75rem; }
        
        /* Control alert sizes */
        .alert {
          font-size: 0.875rem;
          padding: 0.75rem 1rem;
          border-radius: 0.375rem;
        }
        
        /* Control table sizes */
        .table {
          font-size: 0.875rem;
        }
        
        /* Control modal sizes */
        .modal-dialog { max-width: 600px; }
        .modal-header { padding: 1rem 1.5rem; }
        .modal-body { padding: 1.5rem; }
        .modal-footer { padding: 1rem 1.5rem; }
        
        /* Responsive adjustments */
        @media (min-width: 1200px) {
          .container-fluid { max-width: 1600px; }
        }
        
        @media (max-width: 768px) {
          html { font-size: 0.8125rem; }
          body { font-size: 0.8125rem; }
        }
      .welcome-hero { background: #f8f9fa; border-radius: 8px; }
      .welcome-card {
        max-width: 900px; margin: 0 auto; background: #ffffff; border-radius: 8px;
        padding: 3rem;
      }
      .welcome-title { font-weight: 700; font-size: 3rem; line-height: 1.2; margin-bottom: 0.5rem; text-align: center; color: #333; }
      .page-title {
        color: #333; font-size: 3rem; font-weight: 700; margin-bottom: 0.5rem; line-height: 1.2; text-align: center;
      }
      .page-subtitle {
        color: #333; font-size: 2.5rem; font-weight: 700; margin-bottom: 2rem; line-height: 1.2; text-align: center;
      }
      .accent-bar { height: 4px; width: 80px; background: #8B0000; border-radius: 3px; margin: 0 auto 2rem auto; }
      .title-underline {
        width: 80px; height: 4px; background: #8B0000; margin: 0 auto 2rem auto;
      }
      .welcome-hero h2, .welcome-hero h3 { font-weight: 800; color: #4a4a4a; }
      .welcome-hero h3 { color: #6b2a2a; }
      .welcome-hero p, .welcome-hero li { line-height: 1.7; font-size: 1.06rem; }
      .welcome-logo { display:flex; justify-content:center; align-items:center; margin-bottom: 12px; }
      .welcome-logo img { max-height: 500px !important; max-width: 1200px !important; width: 100% !important; height: auto !important; object-fit: contain !important; }
      .section-gap { margin-top: 1.25rem; }
      .disabled { pointer-events: auto !important; cursor:not-allowed; opacity:.6; }
      
      /* Enhanced Navbar Styling */
      .navbar-nav { 
        display: flex; 
        flex-wrap: nowrap; 
        overflow-x: auto; 
        overflow-y: hidden; 
        -webkit-overflow-scrolling: touch;
        scrollbar-width: none;
        -ms-overflow-style: none;
      }
      .navbar-nav::-webkit-scrollbar { display: none; }
      .nav-item { flex-shrink: 0; }
      .nav-link { 
        display: flex; 
        align-items: center; 
        gap: 0.5rem; 
        white-space: nowrap; 
        padding: 0.75rem 1rem;
        transition: all 0.3s ease;
        position: relative;
      }
      .nav-link.active { 
        font-weight: 700; 
        border-bottom: 3px solid #0d6efd;
        background-color: rgba(13, 110, 253, 0.1);
      }
      .nav-link:hover { 
        background-color: rgba(13, 110, 253, 0.05); 
        transform: translateY(-1px);
      }
      .nav-icon { 
        width: 1.2em; 
        height: 1.2em; 
        flex-shrink: 0;
        filter: drop-shadow(0 1px 2px rgba(0,0,0,0.1));
      }
      
      /* Scroll indicators */
      .nav-scroll-container { position: relative; }
      .nav-scroll-indicator { 
        position: absolute; 
        top: 50%; 
        transform: translateY(-50%); 
        z-index: 10; 
        background: rgba(255,255,255,0.9); 
        border: 1px solid #dee2e6; 
        border-radius: 50%; 
        width: 32px; 
        height: 32px; 
        display: flex; 
        align-items: center; 
        justify-content: center; 
        cursor: pointer; 
        transition: all 0.3s ease;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .nav-scroll-indicator:hover { 
        background: #0d6efd; 
        color: white; 
        transform: translateY(-50%) scale(1.1);
      }
      .nav-scroll-left { left: 10px; }
      .nav-scroll-right { right: 10px; }
      .nav-scroll-indicator.hidden { opacity: 0; pointer-events: none; }
      
      /* Access page styling */
      .access-title { 
        font-weight: 700; 
        color: #2c3e50; 
        font-size: 2.5rem;
      }
      .access-subtitle { 
        font-size: 1.1rem; 
        max-width: 600px; 
        margin: 0 auto;
      }
      .access-form-card { 
        border-radius: 20px; 
        background: linear-gradient(145deg, #ffffff 0%, #f8f9fa 100%);
        border: 1px solid rgba(0,123,255,0.1);
      }
      .form-title { 
        color: #495057; 
        font-weight: 600; 
        font-size: 1.4rem;
      }
      .form-description { 
        font-size: 1rem; 
        line-height: 1.5;
      }
      .form-group { 
        position: relative; 
      }
      .form-label { 
        color: #495057; 
        margin-bottom: 0.5rem; 
        font-size: 1rem;
      }
      .form-control-lg { 
        border-radius: 12px; 
        border: 2px solid #e9ecef; 
        padding: 0.75rem 1rem; 
        font-size: 1rem;
        transition: all 0.3s ease;
      }
      .form-control-lg:focus { 
        border-color: #0d6efd; 
        box-shadow: 0 0 0 0.2rem rgba(13, 110, 253, 0.15); 
        transform: translateY(-1px);
      }
      .form-text { 
        font-size: 0.9rem; 
        margin-top: 0.5rem; 
        line-height: 1.4;
      }
       .btn-primary { 
         background: #8B2323; 
         border: none; 
         border-radius: 12px; 
         font-weight: 600; 
         font-size: 1.1rem;
         transition: all 0.3s ease;
       }
       .btn-primary:hover { 
         background: #6B1A1A; 
         transform: translateY(-2px); 
         box-shadow: 0 8px 25px rgba(139, 35, 35, 0.3);
       }
      .privacy-note { 
        background: #f8f9fa; 
        padding: 1rem; 
        border-radius: 12px; 
        border-left: 4px solid #6c757d;
      }
      .form-check { 
        border-radius: 12px; 
        transition: all 0.3s ease;
      }
      .form-check:hover { 
        background-color: #f8f9fa !important;
      }
      .form-check-input:checked { 
        background-color: #198754; 
        border-color: #198754;
      }
      
      /* Responsive adjustments */
      @media (max-width: 991.98px) {
        .navbar-nav { padding: 0 40px; }
        .nav-link { padding: 0.5rem 0.75rem; font-size: 0.9rem; }
      }
      
      @media (min-width: 992px) {
        .navbar-toggler {
          display: none;
        }
      }
      
      /* Purpose textarea styling */
      #purpose {
        min-height: 120px !important;
        font-size: 16px !important;
        resize: vertical;
      }
    ")),
    # Enhanced JS for tab management, scrolling, and URL routing
    tags$script(HTML("
      // Tab locking functionality
      Shiny.addCustomMessageHandler('lockTabs', function(msg){
        (msg.ids||[]).forEach(function(id){
          var sel = 'a[data-bs-toggle=\"tab\"][data-value=\"'+id+'\"]';
          var el = document.querySelector(sel);
          if(!el) return;
          if(msg.disabled){ el.classList.add('disabled'); el.setAttribute('aria-disabled','true'); }
          else { el.classList.remove('disabled'); el.removeAttribute('aria-disabled'); }
          el.onclick = function(e){ if(el.classList.contains('disabled')){ e.preventDefault(); e.stopPropagation(); } };
        });
      });
      
      // Mobile menu collapse function using Bootstrap 5 Collapse API
      function collapseMobileMenu() {
        const nc = document.querySelector('.navbar .navbar-collapse');
        if (!nc) return;
        
        // Check if Bootstrap is available
        if (typeof bootstrap !== 'undefined' && bootstrap.Collapse) {
          // Use Bootstrap 5 Collapse API
          const inst = bootstrap.Collapse.getOrCreateInstance(nc, { toggle: false });
          inst.hide();
        } else {
          // Fallback: manually hide the collapse
          nc.classList.remove('show');
          const toggler = document.querySelector('.navbar-toggler');
          if (toggler) {
            toggler.setAttribute('aria-expanded', 'false');
          }
        }
      }
      
      // URL routing and deep linking
      function updateURL(tabValue) {
        const url = new URL(window.location);
        url.hash = tabValue;
        history.pushState(null, '', url);
      }
      
      function getTabFromURL() {
        return window.location.hash.substring(1) || 'home';
      }
      
      // Initialize URL routing
      $(document).ready(function() {
        // Handle browser back/forward
         window.addEventListener('popstate', function() {
           const tabValue = getTabFromURL();
           if (tabValue) {
             const el = document.querySelector('a[data-value=\"' + tabValue + '\"]');
             if (el && window.bootstrap && bootstrap.Tab) {
               bootstrap.Tab.getOrCreateInstance(el).show();
             }
             collapseMobileMenu(); // Collapse menu on navigation
           }
         });
        
        // Handle tab clicks - use event delegation for dynamic content
        $(document).on('click', 'a[data-bs-toggle=\"tab\"], .nav-link, .navbar-nav a', function(e) {
          const tabValue = $(this).attr('data-value') || $(this).attr('data-bs-value');
          
          // Always try to update URL if we can find a value
          if (tabValue) {
            updateURL(tabValue);
          }
        });
        
        // Close the menu on any nav selection
        $(document).on('click', '.navbar .nav-link, .navbar .dropdown-item, a[data-bs-toggle=\"tab\"]', function () {
          collapseMobileMenu();
        });
        
         // Set initial tab from URL
         const initialTab = getTabFromURL();
         if (initialTab) {
           const el = document.querySelector('a[data-value=\"' + initialTab + '\"]');
           if (el && window.bootstrap && bootstrap.Tab) {
             bootstrap.Tab.getOrCreateInstance(el).show();
           }
         }
      });
      
      // Navbar scrolling functionality
      function initNavbarScrolling() {
        const nav = document.querySelector('.navbar-nav');
        if (!nav) return;
        
        const container = nav.parentElement;
        const leftIndicator = document.createElement('div');
        const rightIndicator = document.createElement('div');
        
        leftIndicator.className = 'nav-scroll-indicator nav-scroll-left hidden';
        leftIndicator.innerHTML = '<i class=\"fas fa-chevron-left\"></i>';
        rightIndicator.className = 'nav-scroll-indicator nav-scroll-right hidden';
        rightIndicator.innerHTML = '<i class=\"fas fa-chevron-right\"></i>';
        
        container.style.position = 'relative';
        container.appendChild(leftIndicator);
        container.appendChild(rightIndicator);
        
        function updateScrollIndicators() {
          const scrollLeft = nav.scrollLeft;
          const maxScroll = nav.scrollWidth - nav.clientWidth;
          
          leftIndicator.classList.toggle('hidden', scrollLeft <= 0);
          rightIndicator.classList.toggle('hidden', scrollLeft >= maxScroll - 1);
        }
        
        leftIndicator.addEventListener('click', function() {
          nav.scrollBy({ left: -200, behavior: 'smooth' });
        });
        
        rightIndicator.addEventListener('click', function() {
          nav.scrollBy({ left: 200, behavior: 'smooth' });
        });
        
        nav.addEventListener('scroll', updateScrollIndicators);
        window.addEventListener('resize', updateScrollIndicators);
        updateScrollIndicators();
      }
      
      // Initialize scrolling when DOM is ready
      $(document).ready(initNavbarScrolling);
      
      
      // Listen for Shiny tab changes
      $(document).on('shown.bs.tab', function(e) {
        // Collapse mobile menu when tab changes
        collapseMobileMenu();
      });
      
      // Custom message handlers for state management
      Shiny.addCustomMessageHandler('updateTabState', function(data) {
        // Update URL when tab changes (handled by existing URL routing)
      });
      
      Shiny.addCustomMessageHandler('restoreTabState', function(data) {
        // Restore tab state on page load (handled by existing URL routing)
      });
      
      // Mobile menu collapse handler
      Shiny.addCustomMessageHandler('collapseMobileMenu', function(data) {
        collapseMobileMenu();
      });
    "))
  ),

   # --------------------------- HOME ------------------------------------------
  # Replace your existing HOME nav_panel section with this exact code:

   nav_panel(
     title = span(
       icon("house", class = "nav-icon", `aria-hidden` = "true"),
       " Home",
       `aria-label` = "Home page - Overview and introduction"
     ), 
     value = "home",
  
  # Add this CSS to your existing header section
  tags$style(HTML("
    .home-container {
      min-height: calc(100vh - 100px);
      padding: 2rem 0;
    }
    
    .hero-section {
      background: white;
      margin-bottom: 2rem;
      overflow: hidden;
      position: relative;
    }
    
    
    .hero-content {
      padding: 3rem 2rem;
      width: 100%;
      max-width: none;
    }
    
    .logo-section {
      text-align: center;
      margin-bottom: 2.5rem;
    }
    
    .logo-container {
      display: inline-block;
      padding: 1rem;
      background: white;
      transition: all 0.3s ease;
    }
    
    .logo-container:hover {
      transform: none;
    }
    
    .logo-image {
      max-height: 500px !important;
      width: 100% !important;
      max-width: 1200px !important;
      height: auto !important;
    }
    
    .hero-title {
      font-size: 2.5rem;
      font-weight: 700;
      color: #2c3e50;
      margin-bottom: 0.5rem;
      line-height: 1.2;
      text-align: center;
    }
    
    .hero-subtitle {
      font-size: 1.1rem;
      color: #6c757d;
      text-align: center;
      margin-bottom: 2rem;
      font-weight: 400;
      line-height: 1.5;
    }
    
    .accent-divider {
      width: 100px;
      height: 4px;
      background: linear-gradient(90deg, #8B0000 0%, #A00000 100%);
      margin: 0 auto 2.5rem auto;
      border-radius: 2px;
    }
    
    .content-grid {
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 3rem;
      margin-bottom: 2.5rem;
      width: 100%;
      max-width: none;
    }
    
    .content-section {
      background: white;
      padding: 2rem;
      border-radius: 15px;
      box-shadow: 0 4px 20px rgba(0,0,0,0.06);
      transition: all 0.3s ease;
      border: 1px solid rgba(139, 0, 0, 0.1);
      margin-bottom: 1.75rem;
    }
    
    .content-section:hover {
      transform: translateY(-3px);
      box-shadow: 0 8px 30px rgba(0,0,0,0.12);
      border-color: rgba(139, 0, 0, 0.2);
    }
    
    .section-icon {
      width: 60px;
      height: 60px;
      background: linear-gradient(135deg, #8B0000 0%, #A00000 100%);
      border-radius: 15px;
      display: flex;
      align-items: center;
      justify-content: center;
      color: white;
      font-size: 1.8rem;
      margin-bottom: 1.5rem;
    }
    
    .section-title {
      font-size: 1.4rem;
      font-weight: 600;
      color: #2c3e50;
      margin-bottom: 1rem;
    }
    
    .section-text {
      color: #495057;
      line-height: 1.7;
      font-size: 1.1rem;
    }
    
    .metrics-list {
      list-style: none;
      padding: 0;
      margin: 1rem 0;
    }
    
    .metrics-list li {
      padding: 0.7rem 0;
      position: relative;
      padding-left: 1.8rem;
      color: #495057;
      font-weight: 500;
      font-size: 1.05rem;
    }
    
    .metrics-list li::before {
      content: '▶';
      position: absolute;
      left: 0;
      color: #8B0000;
      font-size: 0.8rem;
    }
    
    .workflow-steps {
      background: white;
      padding: 2rem;
      border-radius: 15px;
      box-shadow: 0 4px 20px rgba(0,0,0,0.06);
      margin-bottom: 2rem;
      border: 1px solid rgba(139, 0, 0, 0.1);
    }
    
    .workflow-title {
      font-size: 2.2rem;
      font-weight: 700;
      color: #2c3e50;
      margin-bottom: 2rem;
      text-align: center;
    }
    
    .steps-container {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
      gap: 1.5rem;
    }
    
    .step-card {
      background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
      padding: 1.5rem;
      border-radius: 12px;
      transition: all 0.3s ease;
      text-align: center;
    }
    .step-card.clickable { cursor: pointer; }
    
    .step-card:hover {
      border-color: #8B0000;
      transform: translateY(-2px);
      box-shadow: 0 6px 20px rgba(139, 0, 0, 0.15);
    }
    
    .step-number {
      width: 40px;
      height: 40px;
      background: linear-gradient(135deg, #8B0000 0%, #A00000 100%);
      border-radius: 50%;
      display: flex;
      align-items: center;
      justify-content: center;
      color: white;
      font-weight: 700;
      font-size: 1.2rem;
      margin: 0 auto 1rem auto;
    }
    
    .step-title {
      font-size: 1.3rem;
      font-weight: 600;
      color: #2c3e50;
      margin-bottom: 0.7rem;
    }
    
    .step-description {
      font-size: 1.05rem;
      color: #495057;
      line-height: 1.5;
    }
    /* Ensure all text inside step-card is centered */
    .step-card .step-title,
    .step-card .step-description,
    .step-card .step-cta { text-align: center; }
    .step-link { display: block; color: inherit; text-decoration: none; }
    .step-cta { margin-top: 0.75rem; color: #8B0000; font-weight: 600; font-size: 0.95rem; }
    
    .cta-section {
      background: linear-gradient(135deg, #8B0000 0%, #A00000 100%);
      padding: 2.5rem;
      border-radius: 15px;
      text-align: center;
      color: white;
      margin-bottom: 2rem;
      position: relative;
      overflow: hidden;
    }
    
    .cta-title {
      font-size: 2.5rem;
      font-weight: 700;
      margin-bottom: 1rem;
      position: relative;
      z-index: 1;
      color: white;
      text-align: center;
    }
    
    .cta-subtitle {
      font-size: 1.1rem;
      margin-bottom: 2rem;
      opacity: 1;
      color: #ffffff;
      position: relative;
      text-align: center;
      z-index: 1;
    }
    
    .cta-button {
      background: white;
      color: #8B0000;
      border: none;
      padding: 1rem 2.5rem;
      border-radius: 50px;
      font-size: 1.1rem;
      font-weight: 700;
      transition: all 0.3s ease;
      position: relative;
      z-index: 1;
      text-transform: uppercase;
      letter-spacing: 0.5px;
    }
    
    .cta-button:hover {
      background: #f8f9fa;
      color: #8B0000;
      transform: translateY(-2px);
      box-shadow: 0 8px 25px rgba(0,0,0,0.2);
    }
    
    @media (max-width: 1200px) {
      .hero-title { font-size: 2.5rem; }
      .section-title { font-size: 1.5rem; }
      .workflow-title { font-size: 2rem; }
      .logo-image { max-height: 400px !important; }
    }
    
    @media (max-width: 1024px) {
      .content-grid { grid-template-columns: 1fr; gap: 1.5rem; }
      .steps-container { grid-template-columns: repeat(2, 1fr); }
      .hero-title { font-size: 2.2rem; }
      .cta-title { font-size: 2.2rem; }
    }
    
    @media (max-width: 900px) {
      .steps-container { grid-template-columns: 1fr; }
      .logo-image { max-height: 350px !important; }
    }
    
    @media (max-width: 768px) {
      .hero-title { font-size: 2rem; }
      .cta-title { font-size: 1.8rem; }
      .logo-image { max-height: 300px !important; }
    }
  ")),
  
  div(class = "home-container",
    div(class = "container-fluid",
      div(class = "hero-section",
        div(class = "hero-content",
          div(class = "logo-section",
            div(class = "logo-container",
                if (file.exists("www/logo.png")) {
                tags$img(src = "logo.png", class = "logo-image", alt = "Laboratory Logo")
                } else {
                tags$div(class = "text-muted", icon("image"), " Drop your lab logo at ", tags$code("www/logo.png"))
              }
            )
          ),
          div(style = "display:flex; align-items:center; justify-content:center; text-align:center;",
            tags$img(
              src = "https://raw.githubusercontent.com/bethanyou/Child-Social-Network-Questionnaire/eee2dbc25a6bc5972e9bc5d9f630c785bd9a9f51/Logo2.png",
              alt = "Child Social Network Questionnaire Logo",
              style = "display:block; margin:auto; max-width:420px; height:auto;"
            )
          ),
          p(class = "hero-subtitle", "Mapping early social experiences to understand development"),
          div(class = "accent-divider"),
          div(class = "content-grid",
            div(class = "content-section",
              div(class = "section-icon", icon("chart-line")),
              h3(class = "section-title", "What CSNQ Measures"),
              tags$ul(class = "metrics-list",
                tags$li("Network size – how many individuals a child regularly interacts with"),
                tags$li("Network composition – family member, caregivers, school teacher, peers, adults; diversity in language and race"),
                tags$li("Network structure – network density, racial and linguistic entropy, racial and linguistic EI Index")
              )
            ),
            div(class = "content-section",
              div(class = "section-icon", icon("lightbulb")),
              h3(class = "section-title", "Why It Matters"),
              p(class = "section-text",
                "Early social experience relates to neural, cognitive, and social development. Sensitivity to categories like race and language emerges early, and exposure to diversity can shape social cognition. Mapping children's social networks helps test how experience with diversity relates to outcomes like perspective-taking, bias, and emotion regulation."
              )
            )
          )
        )
      ),
      # Development Team moved up and full-width
      div(class = "content-section",
        div(class = "section-icon", icon("university")),
        h3(class = "section-title", "Development Team"),
        p(class = "section-text",
          HTML("The <strong>Child Social Network Questionnaire (CSNQ)</strong> was developed in the <strong>Infant Learning and Development Laboratory (Woodward Lab)</strong> at the University of Chicago (<a href='https://woodwardlab.uchicago.edu/' target='_blank'>woodwardlab.uchicago.edu</a>). The initial CSNQ was created by <strong>Nicole Burke and colleagues</strong> (Burke, Brezack, & Woodward, 2022). Since then, <strong>Yiyi Wang, Bethany Ou</strong>, and collaborators have updated the survey and built this comprehensive research hub."))
      ),
      div(class = "workflow-steps",
        h2(class = "workflow-title", "Research Workflow"),
        div(class = "steps-container",
          div(class = "step-card clickable",
            actionLink(
              "wf_access",
              label = tagList(
                div(class = "step-number", "1"),
                h4(class = "step-title", "Request Survey Access"),
                p(class = "step-description", "Request access to the CSNQ survey file"),
                div(class = "step-cta", "Request Survey Access →")
              ),
              class = "step-link"
            )
          ),
          div(class = "step-card",
            div(class = "step-number", "2"),
            h4(class = "step-title", "Collect Data"),
            p(class = "step-description", "Deploy in Qualtrics to gather data from parents/caregivers")
          ),
          div(class = "step-card clickable",
            actionLink(
              "wf_process",
              label = tagList(
                div(class = "step-number", "3"),
                h4(class = "step-title", "Process Data"),
                p(class = "step-description", "Clean Qualtrics exports into three standardized tables"),
                div(class = "step-cta", "Open Process Data page →")
              ),
              class = "step-link"
            )
          ),
          div(class = "step-card clickable",
            actionLink(
              inputId = "go_plotting",
              label = tagList(
                div(class = "step-number", "4"),
                h4(class = "step-title", "Analyze Results"),
                p(class = "step-description", "Explore data with network plotting tools"),
                div(class = "step-cta", "Open Network Plotting →")
              ),
              class = "step-link"
            )
          )
        )
      ),
      div(class = "cta-section",
        h2(class = "cta-title", "Ready to Get Started?"),
        p(class = "cta-subtitle", "Join us in using the CSNQ to understand children's social development"),
        actionButton("btn_interested", "Start Requesting Access", class = "cta-button", icon = icon("rocket"))
      ),
      
      tags$div(class = "content-section",
        h3(class = "section-title", "Key References"),
        tags$ul(style = "list-style: none; padding: 0;",
          tags$li(style = "padding: 0.7rem 0; color: #495057; border-bottom: 1px solid #f1f3f4; font-size: 1.05rem;",
            tags$a(
              href = "https://www.frontiersin.org/journals/psychology/articles/10.3389/fpsyg.2022.1009422/full",
              target = "_blank",
              rel = "noopener noreferrer",
              style = "text-decoration: none; color: inherit;",
              "Burke, N. M., Brezack, N., & Woodward, A. L. (2022). Frontiers in Psychology."
            )
          ),
          tags$li(style = "padding: 0.7rem 0; color: #495057; font-size: 1.05rem;",
            tags$a(
              href = "https://www.frontiersin.org/journals/developmental-psychology/articles/10.3389/fdpys.2023.1221056/full",
              target = "_blank",
              rel = "noopener noreferrer",
              style = "text-decoration: none; color: inherit;",
              "Burke, N. M., Brezack, N., & Meyer, M. (2023). Frontiers in Developmental Psychology."
            )
          )
          )
        )
      )
    )
  ),

   # --------------------------- GET ACCESS ------------------------------------
# Replace your GET ACCESS nav_panel section with this:

   nav_panel(
     title = span(
       icon("key", class = "nav-icon", `aria-hidden` = "true"),
       " Request Access",
       `aria-label` = "Request Access - Request access"
     ), 
     value = "access",
    
    # Add improved styling
  tags$style(HTML("
    .access-page {
      background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
      min-height: calc(100vh - 100px);
      padding: 2rem 0;
    }
    
    .access-hero {
      background: transparent;
      padding: 2rem 2rem 1rem 2rem;
      margin-bottom: 1rem;
      text-align: center;
      position: relative;
    }
    
    .logo-section {
      text-align: center;
      margin-bottom: 2rem;
    }
    
    .logo-container {
      display: inline-block;
      padding: 1rem;
      background: white;
      transition: all 0.3s ease;
    }
    
    .logo-container:hover {
      transform: none;
    }
    
    .logo-image {
      max-height: 500px !important;
      width: 100% !important;
      max-width: 1200px !important;
      height: auto !important;
    }
    
     .access-title {
       font-size: 2.2rem;
       font-weight: 700;
       color: #2c3e50;
       margin-bottom: 1rem;
       line-height: 1.2;
     }
    
    .access-subtitle {
      font-size: 1.1rem;
      color: #6c757d;
      margin-bottom: 2rem;
      line-height: 1.4;
    }
    
    .form-container {
      background: transparent;
      padding: 1.5rem 2.5rem 2.5rem 2.5rem;
      position: relative;
      margin-top: 0;
    }
    
    .form-fields {
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 2rem;
    }
    
    .form-group.full-width {
      grid-column: 1 / -1;
    }
    
    
    .form-header {
      text-align: center;
      margin-bottom: 1.5rem;
      padding-top: 0;
    }
    
     .form-title {
       font-size: 1.6rem;
       font-weight: 600;
       color: #2c3e50;
       margin-bottom: 1rem;
     }
    
    .form-description {
      font-size: 1.1rem;
      color: #6c757d;
      line-height: 1.6;
    }
    
    .form-group {
      margin-bottom: 2rem;
    }
    
    .form-label {
      font-size: 1.1rem;
      font-weight: 600;
      color: #2c3e50;
      margin-bottom: 0.7rem;
      display: block;
    }
    
    .form-control {
      border: 1px solid #dee2e6;
      border-radius: 8px;
      padding: 0.75rem 1rem;
      font-size: 1rem;
      transition: all 0.3s ease;
      background: white;
    }
    
    .form-control:focus {
      border-color: #8B0000;
      box-shadow: 0 0 0 0.2rem rgba(139, 0, 0, 0.15);
      background: white;
    }
    
    .form-text {
      font-size: 0.95rem;
      color: #6c757d;
      margin-top: 0.5rem;
      line-height: 1.5;
    }
    
    .form-check {
      background: transparent;
      border: none;
      padding: 1rem 0;
      transition: all 0.3s ease;
    }
    
    .form-check:hover {
      background: transparent;
    }
    
    .form-check-input {
      width: 1.2rem;
      height: 1.2rem;
      border: 2px solid #8B0000;
      border-radius: 4px;
      background-color: white;
      margin-right: 0.75rem;
      cursor: pointer;
      transition: all 0.3s ease;
      position: relative;
    }
    
    .form-check-input:hover {
      border-color: #A00000;
      box-shadow: 0 0 0 0.2rem rgba(139, 0, 0, 0.15);
    }
    
    .form-check-input:checked {
      background-color: #8B0000;
      border-color: #8B0000;
    }
    
    .form-check-input:checked::after {
      content: '✓';
      color: white;
      font-weight: bold;
      font-size: 0.9rem;
      position: absolute;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
    }
    
    .form-check-input:focus {
      border-color: #8B0000;
      box-shadow: 0 0 0 0.2rem rgba(139, 0, 0, 0.25);
    }
    
    .form-check-label {
      font-size: 1.05rem;
      color: #495057;
      line-height: 1.5;
      cursor: pointer;
      display: flex;
      align-items: flex-start;
    }
    
    .btn-primary {
      background: linear-gradient(135deg, #8B0000 0%, #A00000 100%);
      border: none;
      border-radius: 12px;
      padding: 1.2rem 2rem;
      font-size: 1.2rem;
      font-weight: 700;
      text-transform: uppercase;
      letter-spacing: 0.5px;
      transition: all 0.3s ease;
      box-shadow: 0 4px 15px rgba(139, 0, 0, 0.2);
    }
    
    .btn-primary:hover {
      background: linear-gradient(135deg, #A00000 0%, #8B0000 100%);
      transform: translateY(-2px);
      box-shadow: 0 8px 25px rgba(139, 0, 0, 0.3);
    }
    
    .privacy-note {
      background: transparent;
      padding: 1.5rem 0;
      margin-top: 2rem;
      text-align: center;
    }
    
    .text-danger {
      color: #dc3545;
      font-weight: 600;
    }
    
    .text-success {
      color: #198754;
    }
    
    .alert {
      border-radius: 12px;
      padding: 1rem 1.5rem;
      margin-top: 1rem;
      border: none;
      font-size: 1.05rem;
    }
    
    .alert-success {
      background: linear-gradient(135deg, #d1e7dd 0%, #a3d9a3 100%);
      color: #0f5132;
      border-left: 4px solid #198754;
    }
    
    .alert-danger {
      background: linear-gradient(135deg, #f8d7da 0%, #f5c2c7 100%);
      color: #721c24;
      border-left: 4px solid #dc3545;
    }
    
    /* Responsive design for laptops */
    @media (max-width: 1200px) {
      .access-title { font-size: 2.5rem; }
      .form-title { font-size: 2rem; }
      .form-container { padding: 2.5rem; }
    }
    
    @media (max-width: 1024px) {
      .access-title { font-size: 2.2rem; }
      .access-subtitle { font-size: 1.2rem; }
      .form-container { padding: 2rem; }
      .form-fields { grid-template-columns: 1fr; gap: 1.5rem; }
    }
    
    @media (max-width: 768px) {
      .access-page { padding: 1.5rem 0; }
      .access-hero { padding: 2rem 1.5rem; }
      .access-title { font-size: 2rem; }
      .form-container { padding: 1.5rem; }
      .form-fields { grid-template-columns: 1fr; gap: 1rem; }
    }
  ")),
  
  div(class = "access-page",
    div(class = "container-fluid",
      div(class = "row",
        div(class = "col-lg-10 col-xl-8 mx-auto",
           # Hero section
          div(class = "access-hero",
            div(class = "page-header",
              tags$h1(class = "page-title", "Request Access"),
              div(class = "title-underline")
            )
          ),
          
          # Main form container
          div(class = "form-container",
               # Form header
            div(class = "form-header",
              tags$h2(class = "form-title", "Researcher Information"),
              tags$p(class = "form-description", 
                        "After the intake form is submitted, we will review your request and get back to you soon.")
               ),
               
               # Form fields
               div(class = "form-fields",
                 # Institution field
              div(class = "form-group",
                tags$label(class = "form-label", `for` = "institution",
                     "Institution", tags$span(class = "text-danger", " *")
                   ),
                 textInput("institution", "", placeholder = "e.g., University of Chicago"),
                div(class = "form-text",
                     icon("info-circle", class = "me-1"),
                     "Official organization name. If none, enter \"Independent researcher.\""
                   ),
                   uiOutput("institution_error")
                 ),
                 
                 # Lab field
              div(class = "form-group",
                tags$label(class = "form-label", `for` = "lab",
                     "Lab", tags$span(class = "text-danger", " *")
                   ),
                 textInput("lab", "", placeholder = "e.g., Woodward Lab"),
                div(class = "form-text",
                     icon("users", class = "me-1"),
                     "Your lab or research group. If none, enter \"Independent researcher.\""
                   ),
                   uiOutput("lab_error")
                 ),
                 
                 # Contact Email field
              div(class = "form-group",
                tags$label(class = "form-label", `for` = "contact_email",
                     "Contact Email", tags$span(class = "text-danger", " *")
                   ),
                 textInput("contact_email", "", placeholder = "researcher@university.edu"),
                div(class = "form-text",
                     icon("envelope", class = "me-1"),
                     "We send access and update notices here. Institutional email is preferred."
                   ),
                   uiOutput("email_error")
                 ),
                 
                 # Researcher Name field
                 div(class = "form-group",
                   tags$label(class = "form-label", `for` = "researcher_name",
                        "Researcher Name", tags$span(class = "text-muted", " (optional)")
                      ),
                   textInput("researcher_name", "", placeholder = "Dr. Jane Smith"),
                   div(class = "form-text",
                        icon("user", class = "me-1"),
                        "Fill this out if you want us to follow up with you."
                      ),
                     uiOutput("researcher_name_error")
                 ),
                 
                 # Phone Number field
              div(class = "form-group",
                tags$label(class = "form-label", `for` = "phone_number",
                     "Phone Number", tags$span(class = "text-muted", " (optional)")
                   ),
                 textInput("phone_number", "", placeholder = "+1-555-123-4567"),
                div(class = "form-text",
                     icon("phone", class = "me-1"),
                     "Only for logging purposes."
                   ),
                   uiOutput("phone_error")
                 ),
                 
                 # Purpose field
              div(class = "form-group",
                tags$label(class = "form-label", `for` = "purpose",
                     "Purpose", tags$span(class = "text-muted", " (optional)")
                   ),
                 textAreaInput("purpose", "", rows = 8, 
                               placeholder = "One to two sentences on how you plan to use the survey or pipeline."), 
                div(class = "form-text",
                     icon("lightbulb", class = "me-1"),
                     "One to two sentences on how you plan to use the survey or pipeline."
                   ),
                   uiOutput("purpose_error")
                 ),
                 
                 # Survey Request checkbox
                 div(
                   class = "form-group full-width mb-3",
                   div(
                     class = "form-check d-flex align-items-start gap-2 p-3 border rounded-3 bg-light shadow-sm",
                     tags$input(
                       type  = "checkbox",
                       class = "form-check-input mt-1",
                       id    = "requesting_survey"
                     ),
                     tags$label(
                       class = "form-check-label fw-semibold",
                       `for` = "requesting_survey",
                       span(
                         class = "d-flex align-items-center",
                         icon("file-lines", class = "me-2 text-primary"),
                         "I want to request a copy of the CSNQ Qualtrics Survey"
                       ),
                       tags$small(
                         class = "d-block text-muted mt-1",
                         "Check this and we will send you a copy."
                       )
                     )
                   ),
                   uiOutput("requesting_survey_error")
                 ),
                 
                 # Consent checkbox
              div(class = "form-group full-width",
                div(
                  class = "form-check d-flex align-items-start gap-2 p-3 border rounded-3 bg-light shadow-sm",
                  tags$input(
                    type  = "checkbox",
                    class = "form-check-input mt-1",
                    id    = "consent_checkbox"
                  ),
                  tags$label(
                    class = "form-check-label fw-semibold",
                    `for` = "consent_checkbox",
                    span(
                      class = "d-flex align-items-center",
                      icon("shield", class = "me-2 text-success"),
                      "I understand my information is used only support; no marketing and no sharing outside the team."
                    )
                  )
                ),
                uiOutput("consent_error")
              ),
                 
                 # Submit button
              div(class = "form-group full-width",
                     actionButton("submit_intake", 
                           tagList(icon("paper-plane"), " Submit and Continue"), 
                           class = "btn btn-primary w-100",
                                 title = "Submits the form and, if valid, directs you to the Request Survey Access page.")
                   ),
                   uiOutput("submit_status")
                 ),
                 
                 # Privacy note
            div(class = "privacy-note",
                   div(class = "small text-muted",
                     icon("lock", class = "me-1"),
                     "Thanks for your interest in the CSNQ. "
                   ),
                   div(class = "mt-2",
                     tags$a(href = "#", "We'll get back to you soon.", 
                           class = "btn btn-link btn-sm text-decoration-none")
                 )
               )
             )
           )
         )
       )
     )
   ),

   # --------------------------- ACCESS SURVEY -----------------------------------
   nav_panel(
     title = span(
       icon("download", class = "nav-icon", `aria-hidden` = "true"),
       " Access Survey",
       `aria-label` = "Access Survey - Request survey files"
     ), 
     value = "download",
     accessSurveyUI("access_survey")
  ),

   # --------------------------- QUALTRICS HELP DOC ------------------------------
# --------------------------- QUALTRICS HELP DOC (anti-crop tweak) ----------------
  nav_panel(
    title = span(
       icon("book", class = "nav-icon", `aria-hidden` = "true"),
       " Qualtrics Help Doc",
       `aria-label` = "Qualtrics Help Doc - Survey logic and flow walkthrough"
     ), 
     value = "qualtrics_help",
  div(
    class = "qualtrics-help-container",
    tags$style(HTML("
      .qualtrics-doc-wrapper { max-width: 1200px; margin: 0 auto; padding: 12px 18px 24px; }
      .qualtrics-doc-title   { font-size: 24px; font-weight: 700; margin: 0 0 6px 0; }
      .qualtrics-doc-underline { height: 2px; width: 64px; background: #0b57d0; border-radius: 2px; margin-bottom: 14px; }
      .q-help-grid { display: grid; grid-template-columns: 260px minmax(0, 1fr); gap: 18px; align-items: start; isolation: isolate; }
      @media (max-width: 1000px) { .q-help-grid { grid-template-columns: 1fr; } }
      .q-toc { position: sticky; top: 80px; z-index: 3; border: 1px solid rgba(0,0,0,0.08); border-radius: 12px; padding: 10px; background: #fff; }
      .q-toc h3 { font-size: 14px; font-weight: 700; margin: 2px 0 8px 2px; }
      .q-toc ul { list-style: none; padding-left: 0; margin: 0; }
      .q-toc li { margin: 2px 0; }
      .q-toc a { display: block; padding: 6px 8px; border-radius: 8px; text-decoration: none; font-size: 13px; color: #1f2937; cursor: pointer; }
      .q-toc a:hover { background: rgba(0,0,0,0.05); }

      /* No masking anywhere */
      .q-doc { border: 1px solid rgba(0,0,0,0.08); background: #fff; position: relative; z-index: 1; border-radius: 0; overflow: visible; }

      /* Taller viewport so top content never *looks* chopped */
      .q-doc-iframe {
        width: 100%;
        height: 1200px;     /* <-- was ~780–900; push higher */
        max-height: 92vh;   /* stay responsive on short screens */
        border: 0; display: block;
      }
    ")),

    tags$script(HTML("
      (function(){
        const BASE='https://docs.google.com/document/d/e/2PACX-1vS5bh58sAujSMI4Vdpzc7BBvtuvboI1xc6yE1_tuqgnapTiXpI5PBit1TqguEh8n2VvR3kaAxsXi06W/pub?embedded=true';
        window.qdocJump = function(hid){
          const f=document.getElementById('qualtrics-help-iframe'); if(!f) return;
          f.src = hid ? (BASE + '#' + hid) : BASE;
        };
      })();
    ")),

    div(
      class = "qualtrics-doc-wrapper",
          h1(class = "qualtrics-doc-title", "Qualtrics Help Doc"),
          div(class = "qualtrics-doc-underline"),
      div(
        class = "q-help-grid",

        # Sidebar (anchors you provided; unchanged)
        div(
          class = "q-toc",
          tags$h3("Table of Contents"),
          tags$ul(
            tags$li(tags$a(onclick="qdocJump('h.fd8bujepbnba')", "Introduction")),
            tags$li(tags$a(onclick="qdocJump('h.rsip68qotqx9')", "Setup & Scope")),
            tags$li(tags$a(onclick="qdocJump('h.554bdutg6h2w')", "Global Conventions")),
            tags$li(tags$a(onclick="qdocJump('h.kdytzgarzoiq')", "Survey Flow Overview")),
            tags$li(tags$a(onclick="qdocJump('h.siy4np7o68x6')", "Module A — Child Basics")),
            tags$li(tags$a(onclick="qdocJump('h.o7fjwey2v962')", "Module B — Household")),
            tags$li(tags$a(onclick="qdocJump('h.za0yriz0oz3y')", "Module C — Daycare/Class")),
            tags$li(tags$a(onclick="qdocJump('h.57oxlu2s4eyr')", "Module D — Other Caregivers")),
            tags$li(tags$a(onclick="qdocJump('h.htm65g9zfhtf')", "Module E — Extended Family")),
            tags$li(tags$a(onclick="qdocJump('h.2pacvxm4hha2')", "Module F — Activities (1–7)")),
            tags$li(tags$a(onclick="qdocJump('h.gawt8b6mw2vq')", "Module G — Other Regular Contacts")),
            tags$li(tags$a(onclick="qdocJump('h.m67p5ocn6x24')", "Module H — De-duplication")),
            tags$li(tags$a(onclick="qdocJump('h.q7z0l5akm1tx')", "Node Demographics loops")),
            tags$li(tags$a(onclick="qdocJump('h.baa8so3c02g')", "• Live at home or not?")),
            tags$li(tags$a(onclick="qdocJump('h.6cmqu3hw8fs2')", "• Hours Spent with the Child")),
            tags$li(tags$a(onclick="qdocJump('h.c4v4rg31u0xp')", "• Age")),
            tags$li(tags$a(onclick="qdocJump('h.9hmhzootpp6')", "• Gender")),
            tags$li(tags$a(onclick="qdocJump('h.c8kw8obhe0yd')", "• Race")),
            tags$li(tags$a(onclick="qdocJump('h.guaqoc833xz5')", "• Languages")),
            tags$li(tags$a(onclick="qdocJump('h.s198c56gdyte')", "• Interaction Mode")),
            tags$li(tags$a(onclick="qdocJump('h.i73xgvxexq01')", "• Interaction Contexts")),
            tags$li(tags$a(onclick="qdocJump('h.d0pd6tjq2kv')", "• Closeness")),
            tags$li(tags$a(onclick="qdocJump('h.d9lbw4ok2x6c')", "• Clarification")),
            tags$li(tags$a(onclick="qdocJump('h.eb20pbn3fj5t')", "• Mapping (Who Knows Whom)")),
            tags$li(tags$a(onclick="qdocJump('h.lig7128f37vr')", "Validation & Logic Rules"))
          )
        ),

        # Right: strictly the published URL
        div(
          class = "q-doc",
               tags$iframe(
            id    = "qualtrics-help-iframe",
            class = "q-doc-iframe",
            src   = "https://docs.google.com/document/d/e/2PACX-1vS5bh58sAujSMI4Vdpzc7BBvtuvboI1xc6yE1_tuqgnapTiXpI5PBit1TqguEh8n2VvR3kaAxsXi06W/pub?embedded=true",
            title = "Qualtrics Survey Help Documentation",
            loading = "eager"
               )
            )
          )
        )
      )
    ),

       

   # --------------------------- EXPLORE DATA (AFTER CLEANING) ------------------
  # Explore Data page removed per request

   # --------------------------- PROCESS DATA ------------------------------------
  nav_panel(
    title = span(
      icon("cogs", class = "nav-icon", `aria-hidden` = "true"),
      " Process Data",
      `aria-label` = "Process Data - Upload and process Qualtrics data"
    ), 
    value = "processdata",
    
    # ==== Scoped CSS (only affects this page) ====
    tags$style(HTML("
      
      .step-box {
        background: white;
        border: 2px solid #e9ecef;
        border-radius: 12px;
        padding: 1.5rem;
        margin-bottom: 1.5rem;
        transition: all 0.3s ease;
      }
      
      .step-box:hover {
        border-color: #8B0000;
        box-shadow: 0 4px 12px rgba(139, 0, 0, 0.1);
      }
      
      .step-box h4 {
        color: #8B0000;
        font-weight: 600;
        margin-bottom: 1rem;
      }
      
      .status-text {
        font-weight: 600;
        padding: 0.5rem 1rem;
        border-radius: 6px;
        margin-top: 0.5rem;
        display: inline-block;
      }
      
      .status-success {
        background: #d4edda;
        color: #155724;
        border: 1px solid #c3e6cb;
      }
      
      .status-error {
        background: #f8d7da;
        color: #721c24;
        border: 1px solid #f5c6cb;
      }
      
      .status-info {
        background: #d1ecf1;
        color: #0c5460;
        border: 1px solid #bee5eb;
      }
      
      .preview-table {
        margin-top: 1rem;
      }
      
      .download-section {
        background: #f8f9fa;
        border-radius: 8px;
        padding: 1rem;
        margin-top: 1rem;
      }
      
      .btn-process {
        background: #8B0000;
        border-color: #8B0000;
        color: white;
        font-weight: 600;
        padding: 0.75rem 2rem;
        border-radius: 8px;
        transition: all 0.3s ease;
        min-width: 200px;
      }
      
      .btn-process:hover:not(:disabled) {
        background: #A00000;
        border-color: #A00000;
        transform: translateY(-1px);
        box-shadow: 0 4px 12px rgba(139, 0, 0, 0.3);
      }
      
      .btn-process:disabled {
        background: #6c757d;
        border-color: #6c757d;
        color: #adb5bd;
        cursor: not-allowed;
        transform: none;
        box-shadow: none;
      }
      
      .btn-process:not(:disabled) {
        animation: pulse-button 2s infinite;
      }
      
      @keyframes pulse-button {
        0% { box-shadow: 0 0 0 0 rgba(139, 0, 0, 0.4); }
        70% { box-shadow: 0 0 0 10px rgba(139, 0, 0, 0); }
        100% { box-shadow: 0 0 0 0 rgba(139, 0, 0, 0); }
      }
      
      /* Guide Styles */
      .guide-container {
        margin: 1.5rem 0;
      }
      
      .guide-steps {
        position: relative;
        margin-bottom: 1rem;
      }
      
      .guide-steps::before {
        content: '';
        position: absolute;
        left: 16px;
        top: 32px;
        bottom: 32px;
        width: 2px;
        background: #8B0000;
        z-index: 1;
      }
      
      .guide-step {
        display: flex;
        align-items: flex-start;
        gap: 1rem;
        padding: 1rem;
        background: #f8f9fa;
        border-radius: 8px;
        margin-bottom: 1rem;
        position: relative;
        transition: all 0.3s ease;
      }
      
      .guide-step:hover {
        background: #e9ecef;
        transform: translateX(4px);
      }
      
      .step-number {
        background: #8B0000;
        color: white;
        width: 32px;
        height: 32px;
        border-radius: 50%;
        display: flex;
        align-items: center;
        justify-content: center;
        font-weight: bold;
        font-size: 1.1rem;
        flex-shrink: 0;
        position: relative;
        z-index: 2;
        border: 2px solid white;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      .step-content {
        flex: 1;
      }
      
      .step-content h5 {
        color: #8B0000;
        font-weight: 600;
        margin-bottom: 0.25rem;
        font-size: 1.1rem;
      }
      
      .step-content p {
        color: #6c757d;
        margin-bottom: 0;
        font-size: 0.95rem;
        line-height: 1.4;
      }
      
      .guide-note {
        margin-top: 1rem;
        padding: 1rem;
        background: #e3f2fd;
        border-radius: 8px;
        color: #1565c0;
        border: 1px solid #bbdefb;
        display: flex;
        align-items: center;
        gap: 0.5rem;
      }
      
      .guide-note i {
        color: #1976d2;
        font-size: 1.1rem;
      }
      
      /* Modern Upload Area Styles */
      .upload-area {
        margin-bottom: 1rem;
      }
      
      .upload-zone {
        border: 2px dashed #8B0000;
        border-radius: 8px;
        padding: 2rem 1.5rem;
        text-align: center;
        background: #fafafa;
        transition: all 0.3s ease;
        cursor: pointer;
        position: relative;
        margin-bottom: 1rem;
        min-height: 120px;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
      }
      
      .upload-zone:hover {
        border-color: #A00000;
        background: #f5f5f5;
        transform: translateY(-1px);
        box-shadow: 0 2px 8px rgba(139, 0, 0, 0.1);
      }
      
      .upload-zone.dragover {
        border-color: #28a745;
        background: #e8f5e8;
        border-style: solid;
      }
      
      .upload-icon {
        margin-bottom: 0.75rem;
      }
      
      .upload-icon-svg {
        font-size: 2.5rem;
        color: #8B0000;
      }
      
      .upload-text {
        display: flex;
        flex-direction: column;
        align-items: center;
        gap: 0.25rem;
      }
      
      .upload-text h5 {
        color: #333;
        font-weight: 600;
        margin: 0;
        font-size: 1.1rem;
      }
      
      .choose-file-link {
        color: #8B0000;
        text-decoration: underline;
        cursor: pointer;
        font-weight: 600;
        margin: 0;
        font-size: 1rem;
      }
      
      .choose-file-link:hover {
        color: #A00000;
      }
      
      .upload-zone .shiny-file-input-progress {
        display: none;
      }
      
      .upload-zone .form-control {
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        opacity: 0;
        cursor: pointer;
        z-index: 10;
      }
      
      /* Hide browse button safely */
      .upload-zone .btn-file {
        display: none;
      }
      
      .upload-specs {
        display: flex;
        justify-content: space-between;
        color: #6c757d;
        font-size: 0.9rem;
        margin-top: 0.5rem;
      }
      
      .spec-left, .spec-right {
        padding: 0.25rem 0;
      }
      
      /* File Preview Styles */
      .file-preview {
        background: #f8f9fa;
        border: 1px solid #e9ecef;
        border-radius: 6px;
        padding: 0.75rem;
        margin-bottom: 0.75rem;
      }
      
      .file-info {
        display: flex;
        align-items: center;
        gap: 0.75rem;
        margin-bottom: 0.25rem;
      }
      
      .file-icon {
        color: #28a745;
        font-size: 1.25rem;
      }
      
      .file-details {
        flex: 1;
      }
      
      .file-name {
        font-weight: 600;
        color: #333;
        margin-bottom: 0.125rem;
        font-size: 0.9rem;
      }
      
      .file-size {
        color: #6c757d;
        font-size: 0.8rem;
      }
      
      .file-actions {
        cursor: pointer;
        color: #dc3545;
        font-size: 1rem;
        padding: 0.125rem;
        border-radius: 4px;
        transition: background 0.2s ease;
      }
      
      .file-actions:hover {
        background: #f8d7da;
      }
      
      .upload-progress-container {
        margin-top: 0.5rem;
      }
      
      .progress-bar {
        background: #e9ecef;
        border-radius: 4px;
        overflow: hidden;
        height: 8px;
        margin-bottom: 0.5rem;
      }
      
      .progress-fill {
        background: linear-gradient(90deg, #8B0000, #A00000);
        height: 100%;
        width: 0%;
        transition: width 0.3s ease;
        animation: pulse 2s infinite;
      }
      
      .progress-text {
        text-align: right;
        font-size: 0.9rem;
        color: #6c757d;
        font-weight: 600;
      }
      
      @keyframes pulse {
        0% { opacity: 1; }
        50% { opacity: 0.7; }
        100% { opacity: 1; }
      }
      
      /* Status Messages */
      .upload-status {
        margin: 0.75rem 0;
        padding: 0.5rem 0.75rem;
        border-radius: 6px;
        font-weight: 600;
        display: none;
        font-size: 0.9rem;
      }
      
      .upload-status.success {
        background: #d4edda;
        color: #155724;
        border: 1px solid #c3e6cb;
        display: block;
      }
      
      .upload-status.error {
        background: #f8d7da;
        color: #721c24;
        border: 1px solid #f5c6cb;
        display: block;
      }
      
      /* Action Buttons */
      .action-buttons {
        text-align: center;
        margin-top: 1rem;
      }
      
      /* Processing Status Styles */
      .processing-status {
        margin: 0.75rem 0;
        padding: 0.5rem 0.75rem;
        border-radius: 6px;
        text-align: center;
        display: none;
        font-size: 0.9rem;
        font-weight: 600;
      }
      
      .processing-status.processing {
        background: #fff3cd;
        color: #856404;
        border: 1px solid #ffeaa7;
        display: block;
      }
      
      .processing-status.success {
        background: #d4edda;
        color: #155724;
        border: 1px solid #c3e6cb;
        display: block;
      }
      
      .processing-status.error {
        background: #f8d7da;
        color: #721c24;
        border: 1px solid #f5c6cb;
        display: block;
      }
      
      .processing-spinner {
        display: inline-block;
        width: 20px;
        height: 20px;
        border: 3px solid #f3f3f3;
        border-top: 3px solid #8B0000;
        border-radius: 50%;
        animation: spin 1s linear infinite;
        margin-right: 0.5rem;
      }
      
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
      
      /* Success Message Styles */
      .success-message {
        background: linear-gradient(135deg, #d4edda 0%, #c3e6cb 100%);
        border: 1px solid #28a745;
        border-radius: 8px;
        padding: 1rem;
        text-align: center;
        margin: 1rem 0;
        box-shadow: 0 2px 8px rgba(40, 167, 69, 0.1);
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
      }
      
      .success-message .success-icon {
        font-size: 2rem;
        color: #28a745;
        margin-bottom: 0.5rem;
        display: block;
      }
      
      .success-message h4 {
        color: #155724;
        font-weight: 700;
        margin-bottom: 0.25rem;
        font-size: 1.1rem;
        text-align: center;
      }
      
      .success-message p {
        color: #155724;
        margin-bottom: 1rem;
        font-size: 0.9rem;
        text-align: center;
      }
      
      .success-message .btn {
        margin: 0 auto;
      }
      
      /* Pipeline Script Box Styles */
      .pipeline-script-box {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border: 2px solid #8B0000;
        border-radius: 12px;
        padding: 1.5rem;
        margin: 1.5rem 0;
        position: relative;
        overflow: hidden;
      }
      
      .pipeline-script-box::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 4px;
        background: linear-gradient(90deg, #8B0000, #A00000, #8B0000);
      }
      
      .pipeline-header {
        margin-bottom: 1rem;
      }
      
      .pipeline-header h4 {
        color: #8B0000;
        font-weight: 700;
        margin: 0;
        font-size: 1.3rem;
      }
      
      .pipeline-content p {
        color: #495057;
        font-size: 1rem;
        line-height: 1.6;
        margin-bottom: 1.25rem;
      }
      
      .pipeline-features {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
        gap: 1rem;
        margin-bottom: 1.5rem;
      }
      
      .feature-item {
        display: flex;
        align-items: center;
        gap: 0.75rem;
        padding: 0.75rem;
        background: white;
        border-radius: 8px;
        border: 1px solid #e9ecef;
        transition: all 0.3s ease;
      }
      
      .feature-item:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 12px rgba(139, 0, 0, 0.1);
        border-color: #8B0000;
      }
      
      .feature-icon {
        color: #8B0000;
        font-size: 1.2rem;
        flex-shrink: 0;
      }
      
      .feature-item span {
        color: #495057;
        font-weight: 500;
        font-size: 0.9rem;
      }
      
      .pipeline-download {
        text-align: center;
      }
      
      .btn-pipeline {
        background: linear-gradient(135deg, #8B0000 0%, #A00000 100%);
        border: none;
        color: white;
        font-weight: 600;
        padding: 0.75rem 2rem;
        border-radius: 8px;
        transition: all 0.3s ease;
        box-shadow: 0 2px 8px rgba(139, 0, 0, 0.2);
        min-width: 200px;
      }
      
      .btn-pipeline:hover {
        background: linear-gradient(135deg, #A00000 0%, #8B0000 100%);
        transform: translateY(-2px);
        box-shadow: 0 4px 16px rgba(139, 0, 0, 0.3);
        color: white;
      }
      
      .btn-pipeline:active {
        transform: translateY(0);
      }
      
      .btn-pipeline i {
        margin-right: 0.5rem;
      }
       
       /* Processing Options Styles */
       .processing-options {
         margin: 2rem 0;
       }
       
       .option-card {
         background: #fff;
         border: 2px solid #e9ecef;
         border-radius: 12px;
         padding: 1.5rem;
         height: 100%;
         transition: all 0.3s ease;
         box-shadow: 0 2px 4px rgba(0,0,0,0.1);
       }
       
       .option-card:hover {
         border-color: #8B0000;
         box-shadow: 0 4px 12px rgba(139,0,0,0.15);
         transform: translateY(-2px);
       }
       
       .option-header {
         display: flex;
         align-items: center;
         margin-bottom: 1rem;
         padding-bottom: 0.75rem;
         border-bottom: 2px solid #f8f9fa;
       }
       
       .option-icon {
         font-size: 1.5rem;
         color: #8B0000;
         margin-right: 0.75rem;
       }
       
       .option-header h5 {
         margin: 0;
         color: #8B0000;
         font-weight: 600;
       }
       
       .option-content p {
         color: #6c757d;
         margin-bottom: 1rem;
         line-height: 1.5;
       }
       
       .option-features ul {
         list-style: none;
         padding: 0;
         margin: 0;
       }
       
       .option-features li {
         padding: 0.25rem 0;
         color: #495057;
         font-size: 0.9rem;
       }
       
       .upload-section {
         margin-top: 1rem;
         padding-top: 1rem;
         border-top: 2px solid #e9ecef;
      }
      
      /* Field Dictionary Styles */
      .field-dictionary-box {
        background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
        border: 2px solid #8B0000;
        border-radius: 12px;
        padding: 2rem;
        margin: 2rem 0;
        position: relative;
        overflow: hidden;
      }
      
      .field-dictionary-box::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 4px;
        background: linear-gradient(90deg, #8B0000, #A00000, #8B0000);
      }
      
      .dictionary-header {
        margin-bottom: 2rem;
        text-align: center;
        padding-bottom: 1.5rem;
        border-bottom: 2px solid #e9ecef;
      }
      
      .dictionary-header h4 {
        color: #8B0000;
        font-weight: 700;
        margin-bottom: 1rem;
        font-size: 1.4rem;
      }
      
      .dictionary-header p {
        color: #495057;
        font-size: 1rem;
        line-height: 1.6;
        margin: 0;
        max-width: 800px;
        margin-left: auto;
        margin-right: auto;
      }
      
      .dictionary-content {
        margin-top: 1.5rem;
      }
      
      .dictionary-content .dd-wrapper {
        display: flex;
        gap: 2rem;
        margin-top: 1rem;
      }
      
      .dictionary-content .dd-toc {
        position: sticky;
        top: 100px;
        max-height: calc(100vh - 140px);
        overflow: auto;
        min-width: 280px;
        width: 280px;
        background: #ffffff;
        border: 2px solid #8B0000;
        border-radius: 12px;
        padding: 1.5rem;
        box-shadow: 0 4px 12px rgba(139, 0, 0, 0.1);
      }
      
      .dictionary-content .dd-toc h5 {
        margin: 0 0 1rem 0;
        font-weight: 700;
        color: #8B0000;
        font-size: 1.1rem;
        text-align: center;
        padding-bottom: 0.5rem;
        border-bottom: 1px solid #e9ecef;
      }
      
      .dictionary-content .dd-content {
        flex: 1;
        background: #ffffff;
        border: 1px solid #e9ecef;
        border-radius: 8px;
        padding: 2rem;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.05);
      }
      
      .dm h3{margin-top:1.1rem;font-weight:800}
      .dm h4{margin-top:.9rem;font-weight:700}
      .dm code{background:#f6f8fa;padding:.08rem .3rem;border-radius:4px}
        .callout{background:#eef7ff;border-left:4px solid #0a7ac4;padding:.75rem 1rem;border-radius:8px}
        .mono{font-family:ui-monospace, Menlo, Consolas, monospace}
        
        /* Qualtrics Help Doc Styles */
        .qualtrics-help-container {
          background: #f8f9fa;
          min-height: 100vh;
          padding: 2rem 0;
          width: 100%;
        }
        
        .qualtrics-doc-wrapper {
          max-width: 100%;
          margin: 0 auto;
          padding: 0 2rem;
          background: #ffffff;
          border-radius: 12px;
          box-shadow: 0 4px 20px rgba(0,0,0,0.1);
          overflow: hidden;
          width: calc(100% - 4rem);
        }
        
        .qualtrics-doc-title {
          font-size: 2.5rem;
          font-weight: 700;
          color: #2c3e50;
          text-align: center;
          margin: 2rem 0 1rem 0;
          padding: 0 2rem;
        }
        
        .qualtrics-doc-underline {
          width: 100px;
          height: 3px;
          background: #8B0000;
          margin: 0 auto 2rem auto;
          border-radius: 2px;
        }
        
        .qualtrics-doc-content {
          padding: 0 2rem 2rem 2rem;
          width: 100%;
          max-height: 80vh;
          overflow-y: auto;
          text-align: center;
        }
        
        .qualtrics-doc-content iframe {
          display: block;
          margin: 0 auto;
        }
        
        .qualtrics-doc-content * {
          max-width: 100%;
          box-sizing: border-box;
        }
        
        .qualtrics-doc-content img {
          max-width: 100%;
          height: auto;
        }
        
        .help-doc-content {
          line-height: 1.6;
          color: #333;
        }
        
        .help-doc-content h2 {
          color: #8B0000;
          font-size: 1.8rem;
          font-weight: 700;
          margin-bottom: 1.5rem;
          text-align: center;
        }
        
        .help-doc-content h3 {
          color: #8B0000;
          font-size: 1.4rem;
          font-weight: 600;
          margin: 2rem 0 1rem 0;
          text-align: center;
        }
        
        .help-doc-content h4 {
          color: #8B0000;
          font-size: 1.2rem;
          font-weight: 600;
          margin: 1.5rem 0 0.75rem 0;
        }
        
        .help-doc-content p {
          margin-bottom: 1rem;
          font-size: 1rem;
        }
        
        .toc-sections {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
          gap: 2rem;
          margin: 2rem 0;
        }
        
        .toc-section {
          background: #f8f9fa;
          padding: 1.5rem;
          border-radius: 8px;
          border-left: 4px solid #8B0000;
        }
        
        .toc-section ul {
          list-style: none;
          padding: 0;
          margin: 0;
        }
        
        .toc-section li {
          margin-bottom: 0.5rem;
          padding: 0.25rem 0;
          color: #8B0000;
          font-weight: 500;
        }
        
        .help-note {
          background: #e3f2fd;
          border: 1px solid #bbdefb;
          border-radius: 8px;
          padding: 1.5rem;
          margin-top: 2rem;
          text-align: center;
        }
        
        .help-note p {
          margin: 0;
          color: #1565c0;
          font-style: italic;
        }
      ")),
    
    # JavaScript for drag and drop functionality
    tags$script(HTML("
      $(document).ready(function() {
        // Wait for Shiny to render the file input
        setTimeout(function() {
          const uploadZone = document.getElementById('upload-zone');
          const fileInput = document.querySelector('input[type=\"file\"]');
          const chooseFileLink = document.querySelector('.choose-file-link');
          
          if (fileInput && chooseFileLink && uploadZone) {
            // Click to upload
            chooseFileLink.addEventListener('click', function(e) {
              e.preventDefault();
              fileInput.click();
            });
            
            // Drag and drop events
            uploadZone.addEventListener('dragover', function(e) {
              e.preventDefault();
              uploadZone.classList.add('dragover');
            });
            
            uploadZone.addEventListener('dragleave', function(e) {
              e.preventDefault();
              uploadZone.classList.remove('dragover');
            });
            
            uploadZone.addEventListener('drop', function(e) {
              e.preventDefault();
              uploadZone.classList.remove('dragover');
              const files = e.dataTransfer.files;
              if (files.length > 0) {
                // Trigger file input change
                fileInput.files = files;
                $(fileInput).trigger('change');
              }
            });
          }
        }, 1000);
        
        // Remove file functionality
        document.addEventListener('click', function(e) {
          if (e.target.classList.contains('remove-file') || e.target.closest('.remove-file')) {
            Shiny.setInputValue('remove_file', Math.random(), {priority: 'event'});
          }
        });
      });
    ")),
    
    # ==== HERO Section ====
    div(class = "main-content",
      h2(class = "section-title", "Social Network Questionnaire Data Processing"),
      p(class = "section-description", "Upload your Qualtrics data to generate analysis-ready datasets using the SNQ pipeline")
    ),
    
    # ==== Testing Data Section ====
    div(class = "step-box",
      h4(icon("flask"), " Step 0: Try Out Testing Data"),
      div(class = "guide-container",
        div(class = "guide-steps",
          div(class = "guide-step",
            div(class = "step-number", "0"),
            div(class = "step-content",
              h5("Download Sample Data"),
              p("Want to test the pipeline before collecting your own data? Download our sample dataset to see how the processing works."),
              div(class = "mt-3",
                downloadButton("download_test_data", 
                             "Download Sample Data", 
                             class = "btn btn-outline-primary btn-lg",
                             style = "font-weight: 600;")
              )
            )
          )
        )
      )
    ),
    
    # ==== Qualtrics Download Guide ====
    div(class = "step-box",
      h4(icon("download"), " Step 1: Download Your Qualtrics Data"),
      div(class = "guide-container",
        div(class = "guide-steps",
          div(class = "guide-step",
            div(class = "step-number", "1"),
            div(class = "step-content",
              h5("Go to Data & Analysis"),
              p("In your Qualtrics survey, navigate to the 'Data & Analysis' tab")
            )
          ),
          div(class = "guide-step",
            div(class = "step-number", "2"),
            div(class = "step-content",
              h5("Export & Import"),
              p("Click on 'Export & Import' in the left sidebar")
            )
          ),
          div(class = "guide-step",
            div(class = "step-number", "3"),
            div(class = "step-content",
              h5("Export Data"),
              p("Select 'Export Data' from the options")
            )
          ),
          div(class = "guide-step",
            div(class = "step-number", "4"),
            div(class = "step-content",
              h5("Choose Format"),
              p("Select 'CSV' format and check 'Export labels' (not codes)")
            )
          )
        )
      ),
      div(class = "alert alert-info guide-note",
        icon("info-circle"),
        " Make sure to export with labels, not codes, for proper processing."
      )
    ),
    
    # ==== File Upload Section ====
    div(class = "step-box",
      h4(icon("upload"), " Step 2: Process Your Data"),
      
      # Public page: only the download script option (processing moved to Admin)
      div(class = "processing-options mb-4",
        div(class = "row",
          div(class = "col-md-12",
            div(class = "option-card",
              div(class = "option-header",
                icon("code", class = "option-icon"),
                h5("Download R script that will process your data")
              ),
              div(class = "option-content",
                p("Get the latest R Markdown pipeline from our shared folder and process your data independently."),
                div(class = "option-features",
                  tags$ul(
                    tags$li("✓ Full control over processing"),
                    tags$li("✓ Customizable for your research"),
                    tags$li("✓ Reproducible analysis")
                  )
                ),
                div(class = "pipeline-download mt-3",
                  tags$a(
                    href = "https://drive.google.com/drive/folders/1-Cl97jX_CEXc-7CQcRdLB0moXLP6dkBu?usp=sharing",
                    target = "_blank",
                    class = "btn btn-pipeline",
                    icon("external-link-alt"),
                    HTML("Open folder to download <code>snq_pipeline.Rmd</code>")
                  ),
                  tags$p(class = "form-text mt-2",
                         "Note: use the most recent 'Pipeline_R_Script.Rmd' in the folder.")
                )
              )
            )
          )
        )
      )
    ),

    
    # Data Model Overview removed from Process page per request
    
    # ==== Field Dictionary (embedded) ====
    div(class = "step-box field-dictionary-box",
      div(class = "dictionary-header",
        h4(icon("table"), " Field Dictionary"),
        p("Understanding your data structure is crucial for analysis. This comprehensive dictionary explains every variable in your processed datasets, including field names, descriptions, and data types. Use this reference to interpret your results and plan your analyses.")
      ),
      div(class = "dictionary-content",
      tags$style(HTML('
        :root{--dd-accent:#b91c1c;--dd-muted:#6b7280}
        .dd-wrapper{display:flex;gap:1.25rem}
        .dd-toc{
          position:sticky;top:80px;max-height:calc(100vh - 120px);overflow:auto;
          min-width:300px;width:320px;box-sizing:border-box;
          background:#fff;border:1px solid #e5e7eb;border-radius:12px;padding:14px 12px;z-index:3;
          box-shadow:0 1px 2px rgba(0,0,0,.04)
        }
        .dd-toc h5{margin:.25rem 0 .6rem 2px;font-weight:800;color:#111827;letter-spacing:.2px}
        .dd-toc ul{list-style:none;margin:0;padding-left:0}
        .dd-toc li{margin:.1rem 0}
        .dd-toc a{
          display:block;padding:6px 10px;border-radius:8px;
          color:#0d6efd;text-decoration:none;line-height:1.25;
          white-space:normal;word-break:break-word;hyphens:auto;
        }
        .dd-toc a:hover{background:#f3f6ff;text-decoration:none}
        .dd-toc a.active{background:#eef2ff}
        .dd-toc li.toc-h1 a{font-weight:900;color:#14532d;border-left:3px solid var(--dd-accent);padding-left:12px}
        .dd-toc li.toc-h2 a{font-weight:800;color:#0d6efd}
        .dd-toc li.toc-h3{margin-left:12px}
        .dd-content{flex:1}
        /* prevent any visual cropping of the embedded page */
        .dd-frame{border:1px solid #e5e7eb;background:#fff;border-radius:10px;overflow:visible;box-shadow:0 1px 2px rgba(0,0,0,.03)}
        .dd-iframe{width:100%;height:1200px;max-height:92vh;min-height:900px;border:0;display:block;border-radius:10px}
      ')),
      div(class = "dd-wrapper",
        div(class = "dd-toc",
          tags$h5("Contents"),
          tags$ul(id = "dd-toc-list")
        ),
        div(class = "dd-content", id = "dd-content",
          div(class = "dd-frame",
            tags$iframe(
              id   = "dd-iframe",
              name = "dd-iframe",
              class = "dd-iframe",
              src   = "https://docs.google.com/document/d/e/2PACX-1vQxUmNAmvUKF_vWRq2FMusUkFg_EchZ_7Q9iJmYTzmE13dO9v_MTTQ8Fj9YI00fUkwIMy7kZ69sIacW/pub?embedded=true",
              title = "Field Dictionary",
              loading = "eager"
            )
          )
        )
      ),
      tags$script(HTML("(function(){var BASE=\"https://docs.google.com/document/d/e/2PACX-1vQxUmNAmvUKF_vWRq2FMusUkFg_EchZ_7Q9iJmYTzmE13dO9v_MTTQ8Fj9YI00fUkwIMy7kZ69sIacW/pub?embedded=true\";var toc=document.getElementById(\"dd-toc-list\");if(!toc)return;var items=[{lvl:1,label:\"1) ego_level_network_summary\",hid:\"h.lpjas0fq0kv8\"},{lvl:3,label:\"ChildID\",hid:\"h.w6twc4at5dcc\"},{lvl:3,label:\"survey_respondent\",hid:\"h.g6mbcehonjlv\"},{lvl:3,label:\"child_name\",hid:\"h.g4h2qqghuru3\"},{lvl:3,label:\"survey_enddate\",hid:\"h.dd70kd1febcc\"},{lvl:3,label:\"survey_completion\",hid:\"h.q6f8m51mjs9f\"},{lvl:3,label:\"child_birthdate\",hid:\"h.9re4h3epo9cq\"},{lvl:3,label:\"child_age_in_months\",hid:\"h.xjbiel77b8t5\"},{lvl:3,label:\"child_gender\",hid:\"h.q2cgghpd75gi\"},{lvl:3,label:\"child_race\",hid:\"h.iddyjrly659r\"},{lvl:3,label:\"child_race_detail\",hid:\"h.v5y51ozggmh7\"},{lvl:3,label:\"child_zipcode\",hid:\"h.msn5trrtqux1\"},{lvl:3,label:\"child_lang\",hid:\"h.awdq7awic5gj\"},{lvl:3,label:\"child_lang_categorized\",hid:\"h.ks0bc8kax9l\"},{lvl:3,label:\"childcare_yn\",hid:\"h.i2swp5h5b0o2\"},{lvl:3,label:\"childcare_type\",hid:\"h.b8dp559tzq1f\"},{lvl:3,label:\"childcare_start_age\",hid:\"h.f325hkoc1xsz\"},{lvl:3,label:\"childcare_size\",hid:\"h.nstrmi7xhqr7\"},{lvl:3,label:\"childcare_child_age\",hid:\"h.bip784cjscvi\"},{lvl:3,label:\"childcare_gender\",hid:\"h.rwqa61o69l22\"},{lvl:3,label:\"childcare_race\",hid:\"h.yrepkdq0bac8\"},{lvl:3,label:\"childcare_lang\",hid:\"h.civls3ydd0xw\"},{lvl:1,label:\"1.5) ego_level_network_summary: Network summary\",hid:\"h.me7fcrb4d578\"},{lvl:3,label:\"network_size\",hid:\"h.v4lp4ysjgeq2\"},{lvl:3,label:\"network_edges_number\",hid:\"h.en1c4p2cc2la\"},{lvl:3,label:\"network_density\",hid:\"h.v2ljlydxup3v\"},{lvl:3,label:\"network_component_count\",hid:\"h.hfa9mcvzib0n\"},{lvl:3,label:\"network_component_ratio\",hid:\"h.7nmpqn876w28\"},{lvl:3,label:\"network_75cutoff_age/kin/race/language\",hid:\"h.9wvtjisatydv\"},{lvl:3,label:\"network_prop_adult_relationship\",hid:\"h.slrsho4rbxxr\"},{lvl:3,label:\"network_prop_kin_relationship\",hid:\"h.ywk3wg3399pr\"},{lvl:3,label:\"network_racial_entropy\",hid:\"h.bm1o8mblqdni\"},{lvl:3,label:\"network_racial_p_labels\",hid:\"h.f6vksac1zaj3\"},{lvl:3,label:\"network_racial_p_vector\",hid:\"h.1ukqdcz7igwy\"},{lvl:3,label:\"network_racial_ingroup\",hid:\"h.se8js25au9zm\"},{lvl:3,label:\"network_racial_outgroup\",hid:\"h.iyw7pa61y12k\"},{lvl:3,label:\"network_racial_ei_index\",hid:\"h.begqrtxlkb5r\"},{lvl:3,label:\"network_language_entropy\",hid:\"h.zfr10c7v5lpf\"},{lvl:3,label:\"network_language_p_labels\",hid:\"h.cr1ci040ro7r\"},{lvl:3,label:\"network_language_p_vector\",hid:\"h.lqbspkb67gp2\"},{lvl:3,label:\"network_size_valid_lang\",hid:\"h.9fwdxd8axpe0\"},{lvl:3,label:\"network_lang_ingroup\",hid:\"h.bph5vt21yoz6\"},{lvl:3,label:\"network_lang_outgroup\",hid:\"h.7s10utfiqev3\"},{lvl:3,label:\"network_linguistic_ei_index\",hid:\"h.4sxzpn43fk14\"},{lvl:1,label:\"2) node_level_long\",hid:\"h.2qmcx6h9tkjs\"},{lvl:3,label:\"ChildID\",hid:\"h.jmq43wxbczv2\"},{lvl:3,label:\"ego_age_in_months\",hid:\"h.ohpccfwj74e5\"},{lvl:3,label:\"ego_gender\",hid:\"h.mpv5r2dchf74\"},{lvl:3,label:\"ego_race\",hid:\"h.q5gu4ewe7j1w\"},{lvl:3,label:\"ego_language\",hid:\"h.8sux4te4nrgq\"},{lvl:3,label:\"node_type\",hid:\"h.f38lia9w0wbu\"},{lvl:3,label:\"node_index\",hid:\"h.alh5v81gk68x\"},{lvl:3,label:\"node_name\",hid:\"h.tldsivulytv\"},{lvl:3,label:\"node_relationship\",hid:\"h.f5qa9v493sp\"},{lvl:3,label:\"node_liveathome_or_not\",hid:\"h.5jyknptv4zb3\"},{lvl:3,label:\"node_daily_hour_weekday\",hid:\"h.cx8hc6bwlxq0\"},{lvl:3,label:\"node_daily_hour_weekend\",hid:\"h.lzz7bahd9r5v\"},{lvl:3,label:\"node_weekly_hour\",hid:\"h.438t3wu2ddis\"},{lvl:3,label:\"node_weekly_hour_summarized\",hid:\"h.8oa1xamcarca\"},{lvl:3,label:\"node_weekly_hour_percent\",hid:\"h.wkphx0oseqbr\"},{lvl:3,label:\"node_age_in_months\",hid:\"h.226ga8seanbw\"},{lvl:3,label:\"node_age_categorized\",hid:\"h.tnhgniimoy9j\"},{lvl:3,label:\"node_gender\",hid:\"h.k04ikr2j8s7t\"},{lvl:3,label:\"node_race\",hid:\"h.lqap3kt890e2\"},{lvl:3,label:\"node_language\",hid:\"h.38dzescnwx96\"},{lvl:3,label:\"node_mode_of_contact\",hid:\"h.e0tpw7hz75a6\"},{lvl:3,label:\"node_context\",hid:\"h.b2q9wik4wumn\"},{lvl:3,label:\"node_context_count\",hid:\"h.8f3ftkm75fmk\"},{lvl:3,label:\"node_kin\",hid:\"h.sxyatg8g1ktp\"},{lvl:2,label:\"Relationship Closeness Questions:\",hid:\"h.p8iwkenwj7l2\"},{lvl:3,label:\"node_closeness_comfort\",hid:\"h.36ydw71ycn3e\"},{lvl:3,label:\"node_closeness_name\",hid:\"h.k1fvgbqfrd6i\"},{lvl:3,label:\"node_closeness_play\",hid:\"h.p78vdxlywb7m\"},{lvl:3,label:\"node_closeness_pickup\",hid:\"h.46u2fv7a5ok3\"},{lvl:3,label:\"node_closeness_score\",hid:\"h.mzn6f08mp7z\"},{lvl:3,label:\"node_clarify\",hid:\"h.5hpndz6fhdl4\"},{lvl:3,label:\"node_mapping\",hid:\"h.zg5z5kng0h37\"},{lvl:3,label:\"node_mapping_code\",hid:\"h.cnztbws0ek6h\"},{lvl:3,label:\"node_activity_specific\",hid:\"h.t9can0klrz1i\"},{lvl:3,label:\"node_component_group (helper)\",hid:\"h.cyn61e66vvn8\"},{lvl:1,label:\"3) activity_level_long\",hid:\"h.nktgmvdcezxb\"},{lvl:3,label:\"ChildID\",hid:\"h.mf90aatz1vpx\"},{lvl:3,label:\"activity_id\",hid:\"h.svddmddqzu10\"},{lvl:3,label:\"activity_label\",hid:\"h.5yb2tnghmze9\"},{lvl:3,label:\"activity_name\",hid:\"h.4ongrgxk0wcs\"},{lvl:3,label:\"activity_otherkids_yn\",hid:\"h.2p791wf3a4aj\"},{lvl:3,label:\"activity_otherkids_n\",hid:\"h.ya9w65m9qrux\"},{lvl:3,label:\"activity_child_age\",hid:\"h.jpwwm3e03coq\"},{lvl:3,label:\"activity_gender\",hid:\"h.54mwpj7ha2hn\"},{lvl:3,label:\"activity_race\",hid:\"h.ec63vc4wkvqq\"},{lvl:3,label:\"activity_contact\",hid:\"h.t119segb17au\"},{lvl:3,label:\"activity_lang\",hid:\"h.hgbmbyirfaqi\"},{lvl:3,label:\"activity_adults\",hid:\"h.59izy8p2i02h\"},{lvl:3,label:\"activity_kids\",hid:\"h.y6ot5o39v9eh\"}];for(var i=0;i<items.length;i++){var it=items[i];var li=document.createElement(\"li\");li.className=(it.lvl===1?\"toc-h1\":(it.lvl===2?\"toc-h2\":\"toc-h3\"));var a=document.createElement(\"a\");a.textContent=it.label;a.href=BASE+\"#\"+it.hid;a.target=\"dd-iframe\";a.addEventListener(\"click\",function(){var act=toc.querySelectorAll(\"a.active\");for(var j=0;j<act.length;j++){act[j].classList.remove(\"active\");}this.classList.add(\"active\");});li.appendChild(a);toc.appendChild(li);} })();"))
      )
    )
  ),



  # --------------------------- NETWORK PLOTTING --------------------------------
  nav_panel(
    title = span(
      icon("project-diagram", class = "nav-icon", `aria-hidden` = "true"),
      " Network Plotting",
      `aria-label` = "Network Plotting - Visualize cleaned data"
    ),
    value = "plotting",
    
    # Embed the external SNQ Network Plotting Dashboard HTML as-is
    tags$iframe(
      src = "snq_dash/dashboard.html?v=43",
      style = "width: 100%; height: calc(100vh - 96px); border: none;",
      `aria-label` = "Embedded SNQ Network Plotting Dashboard"
    )
  ),

   # --------------------------- ADMIN -------------------------------------------
   nav_panel(
     title = span(
       icon("shield", class = "nav-icon", `aria-hidden` = "true"),
       " Admin",
       `aria-label` = "Admin - Administrative dashboard and data management"
     ), 
     value = "admin",
     div(class = "page-header",
       h1(class = "page-title", "Admin Dashboard"),
       div(class = "title-underline")
     ),
     conditionalPanel(
       condition = "!output.admin_authenticated",
       div(class = "container-fluid py-4",
         div(class = "row justify-content-center",
          div(class = "col-lg-6 col-xl-5",
             card(
               card_header("Admin Login"),
               passwordInput("admin_password", "Admin Password", placeholder = "Enter admin password"),
               actionButton("admin_login", "Login", class = "btn btn-warning"),
             uiOutput("admin_status")
             )
           )
         )
       )
     ),
     conditionalPanel(
       condition = "output.admin_authenticated",
       div(class = "container-fluid py-4",
         # Data Management Tabs
         tabsetPanel(
           id = "admin_tabs",
           
           # Intake Data Tab
           tabPanel("Intake Data",
         card(
           card_header("Intake Data Management"),
           p("View and download intake submissions in real-time."),
           div(class = "d-grid gap-2 d-md-block mb-3",
             actionButton("refresh_data", "Refresh Data", class = "btn btn-primary me-2"),
                 downloadButton("download_intake", "Download Intake CSV", class = "btn btn-success me-2"),
             actionButton("admin_logout", "Logout", class = "btn btn-outline-secondary")
           ),
           uiOutput("data_stats"),
           div(class = "mt-3",
             tags$h5("Intake Data Preview"),
             DTOutput("admin_intake_table")
               )
             )
           ),
           
           # Help Requests Tab
           tabPanel("Help Requests",
             card(
               card_header("Help & Support Requests"),
               p("Manage user support requests and uploaded files."),
               div(class = "d-grid gap-2 d-md-block mb-3",
                 actionButton("refresh_help", "Refresh Help Requests", class = "btn btn-primary me-2"),
                 downloadButton("download_help_requests", "Download Help CSV", class = "btn btn-success me-2")
               ),
               div(class = "alert alert-info",
                 tags$strong("File Storage: "), "Uploaded files are stored in ", 
                 tags$code("data/uploads/"), " with timestamped filenames."
               ),
               div(class = "mt-3",
                 tags$h5("Help Requests Preview"),
                 DTOutput("admin_help_table")
               )
             )
           ),
           
          # System Debug Tab  
          tabPanel("System Debug",
             card(
               card_header("Debug Information"),
               p("System status and debugging information."),
               div(class = "row",
                 div(class = "col-md-6",
                   tags$h6("System Status"),
                   uiOutput("debug_status")
                 ),
                 div(class = "col-md-6",
                   tags$h6("Recent Logs"),
                   uiOutput("debug_logs")
                 )
               ),
               div(class = "mt-3",
                 actionButton("clear_logs", "Clear Logs", class = "btn btn-warning btn-sm"),
                 actionButton("export_logs", "Export Logs", class = "btn btn-info btn-sm ms-2")
               )
             )
            )
          ),
          
          # R Process Tab (Admin-only)
          tabPanel("R Process",
            card(
              card_header("We Process For You"),
              # Upload Area (same as original Option A)
              div(class = "upload-section mt-3",
                div(class = "upload-area", id = "upload-area",
                  div(class = "upload-zone", id = "upload-zone",
                    div(class = "upload-icon", icon("cloud-upload-alt", class = "upload-icon-svg")),
                    div(class = "upload-text",
                      h5("Drag and Drop file here or"),
                      span("Choose file", class = "choose-file-link")
                    ),
                    fileInput("process_file", "", accept = c(".csv", ".xlsx", ".xls"), width = "100%")
                  ),
                  div(class = "upload-specs",
                    div(class = "spec-left", "Supported formats: CSV"),
                    div(class = "spec-right", "Maximum size: 50MB")
                  )
                ),
                # File Preview/Progress Area
                div(class = "file-preview", id = "file-preview", style = "display: none;",
                  div(class = "file-info",
                    div(class = "file-icon", icon("file-csv")),
                    div(class = "file-details",
                      div(class = "file-name", id = "file-name"),
                      div(class = "file-size", id = "file-size")
                    ),
                    div(class = "file-actions", icon("times", class = "remove-file", id = "remove-file"))
                  ),
                  div(class = "upload-progress-container", id = "upload-progress-container", style = "display: none;",
                    div(class = "progress-bar", div(class = "progress-fill", id = "progress-fill")),
                    div(class = "progress-text", id = "progress-text")
                  )
                )
              ),
              # Status Messages
              div(class = "upload-status", id = "upload-status"),
              div(class = "processing-status", id = "processing-status"),
              # Action Buttons
              div(class = "action-buttons",
                actionButton("process_btn", "Process Data", class = "btn btn-process btn-lg", disabled = TRUE),
                br(), br(),
                conditionalPanel(
                  condition = "output.show_download",
                  div(class = "success-message",
                    icon("check-circle", class = "success-icon"),
                    h4("Processing Complete!"),
                    p("Your data has been successfully processed. Download the outputting data here. You can also preview the data below"),
                    downloadButton("download_all", "Download All Results (ZIP)", class = "btn btn-success btn-lg")
                  )
                )
              ),
              # Individual downloads
              conditionalPanel(
                condition = "output.show_individual_downloads",
                div(class = "row",
                  div(class = "col-md-6",
                    div(class = "step-box",
                      h4(icon("users"), " Node Level Data"),
                      p("Alter-level data: one row per relationship (alter)"),
                      downloadButton("download_node", "Download Node Data", class = "btn btn-info btn-block")
                    )
                  ),
                  div(class = "col-md-6",
                    div(class = "step-box",
                      h4(icon("child"), " Ego Level Summary"),
                      p("Child-level data: one row per child (ego)"),
                      downloadButton("download_ego", "Download Ego Data", class = "btn btn-info btn-block")
                    )
                  )
                )
              ),
              br(),
              # Previews
              div(class = "step-box",
                h4(icon("table"), " Preview: Ego Level Summary"),
                DT::dataTableOutput("preview_ego")
              ),
              div(class = "step-box",
                h4(icon("table"), " Preview: Node Level Long"),
                DT::dataTableOutput("preview_node")
              )
            )
          )
       )
     )
   ),

   # --------------------------- HELP & CONTACT ---------------------------------
   nav_panel(
     title = span(
       icon("question-circle", class = "nav-icon", `aria-hidden` = "true"),
       " Help & Contact",
       `aria-label` = "Help & Contact - Support and assistance"
     ), 
     value = "help",
     
     # Page Header with styling matching Access Survey
     div(class = "page-header",
       h1(class = "page-title", "Help & Contact"),
       div(class = "title-underline")
     ),
     
     # Main Content Container
     div(class = "container-fluid",
       div(class = "row justify-content-center",
         div(class = "col-lg-8 col-xl-6",
           # Contact Form Card
      card(
             class = "access-form-card shadow-lg border-0",
             card_body(
               class = "p-4",
               
               # Form header
               div(class = "form-header mb-4",
                 tags$h4(class = "form-title mb-2", "Get Support"),
                 tags$p(class = "form-description text-muted", 
                        "Having trouble with your survey or analysis? We're here to help.")
               ),
               
               # Form fields
               div(class = "form-fields",
                 # Name field
                 div(class = "form-group mb-3",
                   tags$label(class = "form-label fw-semibold", `for` = "c_name",
                     "Your Name", tags$span(class = "text-danger", " *")
                   ),
                   textInput("c_name", "", placeholder = "Enter your full name"),
                   uiOutput("c_name_error")
                 ),
                 
                 # Email field
                 div(class = "form-group mb-3",
                   tags$label(class = "form-label fw-semibold", `for` = "c_email",
                     "Email Address", tags$span(class = "text-danger", " *")
                   ),
                   textInput("c_email", "", placeholder = "your.email@institution.edu"),
                   div(class = "form-text text-muted",
                     icon("info-circle", class = "me-1"),
                     "We'll respond to this email address within 24-48 hours."
                   ),
                   uiOutput("c_email_error")
                 ),
                 
                 # Subject field
                 div(class = "form-group mb-3",
                   tags$label(class = "form-label fw-semibold", `for` = "c_subject",
                     "Subject", tags$span(class = "text-danger", " *")
                   ),
                   textInput("c_subject", "", 
                            placeholder = "Brief description of your issue"),
                   uiOutput("c_subject_error")
                 ),
                 
                 # Message field
                 div(class = "form-group mb-4",
                   tags$label(class = "form-label fw-semibold", `for` = "c_body",
                     "Message", tags$span(class = "text-danger", " *")
                   ),
                   textAreaInput("c_body", "", rows = 4,
                                placeholder = "Describe your issue in detail. What were you trying to do? What went wrong? Include any error messages."),
                   div(class = "form-text text-muted",
                     icon("lightbulb", class = "me-1"),
                     "Be specific about steps you took and what you expected vs. what happened."
                   ),
                   uiOutput("c_body_error")
                 ),
                 
                # File Upload Section removed; provide email instruction for attachments
                div(class = "form-group mb-4",
                  div(class = "alert alert-info",
                    icon("envelope"), " For attachments, we will ask you to attach them to the email we send you."
                  )
                ),
                 
                 # Submit button
                 div(class = "form-group mb-4",
                   div(class = "d-grid",
                     actionButton("submit_issue", 
                                 tagList(icon("paper-plane"), " Submit"), 
                                 class = "btn btn-primary btn-lg py-3")
                   ),
                   uiOutput("issue_status")
                 ),
                 
                 # Alternative actions
                 div(class = "text-center mt-3",
                   actionButton("compose_email", 
                               tagList(icon("envelope"), " Or send direct email"), 
                               class = "btn btn-outline-primary btn-sm")
                 )
               )
             )
           )
         ),
         
         # Troubleshooting Info Sidebar
         div(class = "col-lg-4 mt-4 mt-lg-0",
           div(class = "main-content",
             h3(class = "section-title", "Quick Troubleshooting"),
             
             div(class = "instruction-step",
               h4(class = "step-title", style = "margin-bottom: 1rem;", "Common Issues"),
               div(class = "step-content", style = "margin-left: 0;",
                 tags$ul(class = "feature-list",
                   tags$li("Survey logic not working → Check QuestionIDs match canonical version"),
                   tags$li("Missing data in export → Verify all required fields are mapped"),
                   tags$li("Processing errors → Ensure CSV format matches Qualtrics export")
                 )
               )
             ),
             
             div(class = "instruction-step",
               h4(class = "step-title", style = "margin-bottom: 1rem;", "Before Contacting Us"),
               div(class = "step-content", style = "margin-left: 0;",
                 tags$ol(class = "feature-list",
                   tags$li("Try re-importing the canonical QSF"),
                   tags$li("Check our documentation for similar issues")
                 )
               )
             ),
             
             div(class = "instruction-step",
               h4(class = "step-title", style = "margin-bottom: 1rem;", "Response Time"),
               div(class = "step-content", style = "margin-left: 0;",
                 tags$p("We typically respond within 24-48 hours. Complex technical issues may take longer to resolve.")
               )
             )
           )
         )
       )
    )
  )
)

# ------------------------------------------------------------------------------
# SERVER
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # ---- Google Apps Script Integration ----
  WEB_APP_EXEC <- "https://script.google.com/macros/s/AKfycbwo-x3s-dFfBkcOMHNiWVkC1vOkVht3fGbe5SvEVrcTKqh_FFOLtOafTz1t1QCGgiO8SQ/exec"
  
  `%||%` <- function(x, y) if (is.null(x) || !nzchar(x)) y else x
  
  # Small helper to render bootstrap alerts in your existing slot
  .render_alert <- function(type = c("success","danger"), text) {
    type <- match.arg(type)
    htmltools::div(class = paste0("alert alert-", type), role = "alert", text)
  }
  
  # Ensure status area exists
  output$submit_status <- shiny::renderUI({ NULL })

  # ---- Small helper to append one-row data.frames to CSV safely ----
  safe_append_csv <- function(path, row_df) {
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    if (!file.exists(path)) {
      readr::write_csv(row_df, path)
    } else {
      utils::write.table(row_df, file = path, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE, na = "")
    }
  }
  
  # ---- Help & Contact Google Apps Script Integration ----
  WEB_APP_SUPPORT <- "https://script.google.com/macros/s/AKfycbwSZu3PUCZWgzWtDZV98YCpTF5ziHDe7RECFqXz70un3fpDxHEP868jWUPcQQX65Etl/exec"

  trim2     <- function(x) trimws(x %||% "")

  email_is_valid <- function(x) {
    grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", x)
  }

  # simple helpers to surface validation messages beside fields
  show_error <- function(output_slot, msg) {
    renderUI(div(class = "text-danger small", icon("exclamation-triangle"), " ", msg))
  }
  clear_ui   <- function() renderUI(HTML(""))
  
  # Enable bookmarking for state preservation
  enableBookmarking(store = "url")
  
  # Handle navigation to Qualtrics Help Doc
  observeEvent(input$go_to_qualtrics_help, {
    bslib::nav_select("nav", "qualtrics_help")
  })
  
  # Handle navigation to Help & Contact page
  observeEvent(input$go_to_help_contact, {
    bslib::nav_select("nav", "help")
  })
  
  # Exclude admin password from bookmarking for security
  setBookmarkExclude(c("admin_password"))
  
  # Log session start
  log_message("New session started", "INFO")
  
  # Cleanup function for session end
  session$onSessionEnded(function() {
    # Clean up any temporary files or resources
    # This helps prevent memory leaks
    log_message("Session ended", "INFO")
    gc() # Force garbage collection
  })

  # ---------------- Tab state management ----------------
  # Track current tab for URL updates
  observe({
    if (!is.null(input$nav)) {
      session$sendCustomMessage("updateTabState", list(tab = input$nav))
    }
  })
  
  # Restore tab state from URL on session start
  observe({
    session$sendCustomMessage("restoreTabState", list())
  })
  
  # ---------------- Mobile menu collapse ----------------
  # Collapse mobile menu when tab changes
  observe({
    if (!is.null(input$nav)) {
      # Send message to collapse mobile menu
      session$sendCustomMessage("collapseMobileMenu", list())
    }
  })

  # ---------------- Gate state ----------------
  interested <- reactiveVal(FALSE)
  intake_ok  <- reactiveVal(TRUE)  # TEMPORARILY DISABLED FOR TESTING
  gate_override <- reactiveVal(FALSE)  # allow one-time navigation to Get Access

   # lock/unlock tabs util
   lock_tabs <- function(disabled) {
    session$sendCustomMessage("lockTabs",
      list(ids = c("download","qualtrics_help","processdata","plotting"), disabled = isTRUE(disabled)))
   }
  lock_tabs(TRUE)  # start locked

  # Home → Interested → navigate to access
  observeEvent(input$btn_interested, ignoreInit = TRUE, {
    interested(TRUE)
    bslib::nav_select("nav", "access")
    shinyjs::runjs("window.scrollTo({top: 0, left: 0, behavior: 'instant'});")
  })

  # Research Workflow quick links
  observeEvent(input$wf_access, ignoreInit = TRUE, {
    bslib::nav_select("nav", "download")
    shinyjs::runjs("window.scrollTo({top: 0, left: 0, behavior: 'instant'});")
  })
  observeEvent(input$wf_process, ignoreInit = TRUE, {
    bslib::nav_select("nav", "processdata")
    shinyjs::runjs("window.scrollTo({top: 0, left: 0, behavior: 'instant'});")
  })

  # Clear all error messages
  clear_errors <- function() {
    output$institution_error <- renderUI(NULL)
    output$lab_error <- renderUI(NULL)
    output$email_error <- renderUI(NULL)
    output$researcher_name_error <- renderUI(NULL)
    output$phone_error <- renderUI(NULL)
    output$purpose_error <- renderUI(NULL)
    output$requesting_survey_error <- renderUI(NULL)
    output$consent_error <- renderUI(NULL)
    output$submit_status <- renderUI(NULL)
  }
  
  # ---- Main submit handler ----
  observeEvent(input$submit_intake, ignoreInit = TRUE, {
    log_message("Intake form submission started")
    clear_errors()
    
    # Collect fields
    inst   <- input$institution    %||% ""
    lab    <- input$lab            %||% ""
    email  <- input$contact_email  %||% ""
    phone  <- input$phone_number   %||% ""
    purpose<- input$purpose        %||% ""
    name   <- (if (!is.null(input$researcher_name)) input$researcher_name else "") %||% ""
    requesting_survey <- isTRUE(input$requesting_survey)
    consent<- isTRUE(input$consent_checkbox)

    # Validate
    missing <- character()
    if (!nzchar(inst))  missing <- c(missing, "Institution")
    if (!nzchar(lab))   missing <- c(missing, "Lab")
    if (!nzchar(email)) missing <- c(missing, "Contact Email")
    if (!consent) {
      output$submit_status <- renderUI(.render_alert("danger","You must accept the consent statement."))
      return(invisible(NULL))
    }
    if (length(missing)) {
    output$submit_status <- renderUI(
        .render_alert("danger", paste("Missing required field(s):", paste(missing, collapse = ", ")))
      )
      return(invisible(NULL))
    }

    # Disable button during request
    shinyjs::disable("submit_intake")

    # Call GAS /exec via GET with query params
    ok <- FALSE; err_msg <- NULL
    resp <- NULL
    
    # Debug: Log the request details
    log_message(paste("Making request to GAS with data:", 
                     "name=", name, 
                     "institution=", inst, 
                     "lab=", lab, 
                     "email=", email))
    
    try({
      # Build URL manually to avoid URLSearchParams issues
      base_url <- WEB_APP_EXEC
      params <- list(
        name = name,
        institution = inst,
        lab = lab,
        email = email,
        phone = phone,
        purpose = purpose,
        requesting_survey = if(requesting_survey) "yes" else "no",
        notes = "shiny-intake"
      )
      
      # URL encode parameters manually
      param_strings <- character()
      for (i in seq_along(params)) {
        key <- names(params)[i]
        value <- params[[i]]
        if (!is.null(value) && nzchar(value)) {
          param_strings <- c(param_strings, paste0(key, "=", URLencode(as.character(value), reserved = TRUE)))
        }
      }
      
      full_url <- paste0(base_url, "?", paste(param_strings, collapse = "&"))
      
      resp <- request(full_url) |>
        req_method("GET") |>
        req_perform()

      # Debug: Log response details
      log_message(paste("GAS Response status:", resp_status(resp)))
      log_message(paste("GAS Response headers:", paste(names(resp_headers(resp)), collapse = ", ")))
      
      # Expect JSON with ok = TRUE
      j <- NULL
      try(j <- resp_body_json(resp), silent = TRUE)
      
      # Debug: Log JSON response
      if (!is.null(j)) {
        log_message(paste("GAS JSON response:", paste(names(j), "=", j, collapse = ", ")))
      } else {
        log_message(paste("GAS Raw response:", resp_body_string(resp)))
      }
      
      if (!is.null(j) && isTRUE(j$ok)) {
        ok <- TRUE
        log_message("GAS request successful - email sent")
      } else {
        # Surface an error string if GAS returned one; otherwise raw response body
        err_msg <- if (!is.null(j) && !is.null(j$error)) as.character(j$error) else resp_body_string(resp)
        if (!nzchar(err_msg)) err_msg <- paste("HTTP", resp_status(resp))
        log_message(paste("GAS request failed:", err_msg))
      }
    }, silent = FALSE)  # Changed to FALSE to see any errors

    # Persist a row into data/intake.csv indicating whether email was sent
    try({
      row <- data.frame(
        timestamp = format(Sys.time(), "%F %T"),
      type = "intake",
        institution = inst,
        lab = lab,
        email = email,
        researcher_name = name,
        phone_number = phone,
        purpose = purpose,
        requesting_survey = if (requesting_survey) "yes" else "no",
        request_survey = if (requesting_survey) "yes" else "no",
        email_sent = if (ok) "success" else "fail",
      stringsAsFactors = FALSE
    )
      safe_append_csv(file.path("data","intake.csv"), row)
      data_refresh_trigger(data_refresh_trigger() + 1)
    }, silent = TRUE)

    if (ok) {
    output$submit_status <- renderUI(
        .render_alert("success","Submitted. Your info has been emailed to the lab.")
      )
      # Clear fields
      updateTextInput(session, "institution", value = "")
      updateTextInput(session, "lab", value = "")
      updateTextInput(session, "contact_email", value = "")
      updateTextInput(session, "researcher_name", value = "")
      updateTextInput(session, "phone_number", value = "")
      updateTextAreaInput(session, "purpose", value = "")
      updateCheckboxInput(session, "requesting_survey", value = FALSE)
    # Update state
    intake_ok(TRUE)
      # No automatic redirect - user stays on current page to see success message
    } else {
      output$submit_status <- renderUI(
        .render_alert("danger", paste("Submission failed.", if (nzchar(err_msg)) paste("Detail:", err_msg) else ""))
      )
    }

    shinyjs::enable("submit_intake")
  })
  

  # Gate logic: lock until intake is complete (no modal/redirect)
  observe({
    ok <- isTRUE(intake_ok())
    lock_tabs(!ok)
  })

  # Hard gate: prevent navigation to locked tabs even if a link is clicked
  observe({
    current <- input$nav
    if (is.null(current)) return()
    allowed <- isTRUE(intake_ok())
    locked_targets <- c("download","qualtrics_help","processdata","plotting")
    if (!allowed && current %in% locked_targets) {
      bslib::nav_select("nav", "home")
    }
  })


  # ---------------- Access Survey Module ----------------
  accessSurveyServer("access_survey")




  # ---------------- QSF management ----------------
  observeEvent(input$qsf_upload, {
    req(input$qsf_upload$datapath)
    file.copy(input$qsf_upload$datapath, canonical_qsf_path, overwrite = TRUE)
    output$qsf_status <- renderUI(tags$div(class="text-success", icon("check"),
                                           " QSF saved to ", code("www/snq.qsf")))
  })

  output$qsf_status <- renderUI({
    if (file.exists(canonical_qsf_path)) {
      tags$div(class="text-muted", icon("file"),
               " Serving ", code("www/snq.qsf"), " (", format(file.info(canonical_qsf_path)$size, big.mark=","), " bytes)")
    } else {
      tags$div(class="text-danger", icon("triangle-exclamation"),
               HTML("No QSF found. Download the canonical CSNQ from <a href='https://github.com/bethanyou/CSNQ/blob/12696482a7b5f8d4b20444857a551529ddb57b19/Social_Network_Questionnaire_-_Bethany_working.qsf' target='_blank' rel='noopener'>GitHub</a> and upload it here."))
    }
  })

  # Canonical download only
  output$dl_qsf_canonical <- downloadHandler(
    filename = function() "CSNQ_canonical.qsf",
    content  = function(file) file.copy(canonical_qsf_path, file, overwrite = TRUE)
  )

  # ---------------- Explore Data ----------------
  # Use reactive values to cache data and avoid re-reading files
  explore_data <- reactiveValues(
    nw = NULL,
    ld = NULL,
    al = NULL,
    loading = FALSE
  )
  
  # Attempt to auto-load from a project data directory on startup
  observe({
    current_data_dir <- file.path("data", "current")
    nw_path <- file.path(current_data_dir, "ego_level_network_summary.csv")
    ld_path <- file.path(current_data_dir, "node_level_long.csv")
    al_path <- file.path(current_data_dir, "activity_level_long.csv")
    try({
      if (is.null(explore_data$nw) && file.exists(nw_path)) {
        explore_data$nw <- suppressWarnings(readr::read_csv(nw_path, show_col_types = FALSE))
      }
    }, silent = TRUE)
    try({
      if (is.null(explore_data$ld) && file.exists(ld_path)) {
        explore_data$ld <- suppressWarnings(readr::read_csv(ld_path, show_col_types = FALSE))
      }
    }, silent = TRUE)
    try({
      if (is.null(explore_data$al) && file.exists(al_path)) {
        explore_data$al <- suppressWarnings(readr::read_csv(al_path, show_col_types = FALSE))
      }
    }, silent = TRUE)
  })
  
  nw <- reactive({
    if (!is.null(explore_data$nw)) return(explore_data$nw)
    req(input$nw_csv)
    if (is.null(explore_data$nw) || explore_data$loading) {
      explore_data$loading <- TRUE
      tryCatch({
        explore_data$nw <- suppressWarnings(readr::read_csv(input$nw_csv$datapath, show_col_types = FALSE))
        explore_data$loading <- FALSE
        log_message("Loaded ego-level network data")
      }, error = function(e) {
        explore_data$loading <- FALSE
        log_message(paste("Error loading ego-level data:", conditionMessage(e)), "ERROR")
        stop("Error loading ego-level data: ", conditionMessage(e))
      })
    }
    explore_data$nw
  })
  
  ld <- reactive({
    if (!is.null(explore_data$ld)) return(explore_data$ld)
    req(input$ld_csv)
    if (is.null(explore_data$ld) || explore_data$loading) {
      explore_data$loading <- TRUE
      tryCatch({
        explore_data$ld <- suppressWarnings(readr::read_csv(input$ld_csv$datapath, show_col_types = FALSE))
        explore_data$loading <- FALSE
        log_message("Loaded node-level data")
      }, error = function(e) {
        explore_data$loading <- FALSE
        log_message(paste("Error loading node-level data:", conditionMessage(e)), "ERROR")
        stop("Error loading node-level data: ", conditionMessage(e))
      })
    }
    explore_data$ld
  })
  
  al <- reactive({
    if (!is.null(explore_data$al)) return(explore_data$al)
    req(input$al_csv)
    if (is.null(explore_data$al) || explore_data$loading) {
      explore_data$loading <- TRUE
      tryCatch({
        explore_data$al <- suppressWarnings(readr::read_csv(input$al_csv$datapath, show_col_types = FALSE))
        explore_data$loading <- FALSE
        log_message("Loaded activity-level data")
      }, error = function(e) {
        explore_data$loading <- FALSE
        log_message(paste("Error loading activity-level data:", conditionMessage(e)), "ERROR")
        stop("Error loading activity-level data: ", conditionMessage(e))
      })
    }
    explore_data$al
  })

  output$explore_status <- renderUI({
    if (explore_data$loading) {
      tags$div(class="text-info", icon("spinner", class="fa-spin"), " Loading data...")
    } else {
      has_preloaded <- !is.null(explore_data$nw) && !is.null(explore_data$ld) && !is.null(explore_data$al)
      has_uploads <- !is.null(input$nw_csv) && !is.null(input$ld_csv) && !is.null(input$al_csv)
      if (has_preloaded || has_uploads) {
        tags$div(class="text-success", icon("check"),
                 " Files loaded. Use previews and charts below.")
      } else {
        tags$div(class="text-muted",
                 icon("circle-info"),
                 " Upload all three or place CSVs in Current data: ",
                 code("ego_level_network_summary.csv"), ", ", code("node_level_long.csv"), ", ", code("activity_level_long.csv"))
      }
    }
  })

  output$dt_nw <- renderDT({ req(nw()); datatable(nw(), options = list(pageLength = 10, scrollX = TRUE)) })
  output$dt_ld <- renderDT({ req(ld()); datatable(ld(), options = list(pageLength = 10, scrollX = TRUE)) })
  output$dt_al <- renderDT({ req(al()); datatable(al(), options = list(pageLength = 10, scrollX = TRUE)) })

  output$plt_sizes <- renderPlot({
    req(nw())
    df <- nw()
    size_col <- if ("network_size" %in% names(df)) "network_size" else NULL
    if (is.null(size_col)) return()
    ggplot(df, aes(x = .data[[size_col]])) +
      geom_histogram(bins = 20) +
      labs(title = "Network size distribution", x = "network_size", y = "count")
  })

  output$plt_entropy <- renderPlot({
    req(nw())
    df <- nw()
    xcol <- if ("network_racial_entropy" %in% names(df)) "network_racial_entropy" else NULL
    ycol <- if ("network_language_entropy" %in% names(df)) "network_language_entropy" else NULL
    if (is.null(xcol) || is.null(ycol)) return()
    ggplot(df, aes(x = .data[[xcol]], y = .data[[ycol]])) +
      geom_point(alpha = 0.6) +
      labs(title = "Racial vs. Language entropy", x = "Racial entropy", y = "Language entropy")
  })

  # Optional: reflect threshold in preview (view-only)
  observeEvent(input$validity_threshold, {
    if (is.null(input$nw_csv)) return()
    df <- nw()
    if ("network_75cutoff_validity" %in% names(df)) {
      df$valid_at_threshold <- ifelse(input$validity_threshold >= 0.75,
                                      df$network_75cutoff_validity, df$network_75cutoff_validity)
      output$dt_nw <- renderDT({ datatable(df, options = list(pageLength = 10, scrollX = TRUE)) })
    }
  })


  # ---------------- Admin Functions ----------------
  # Admin authentication state
  admin_authenticated <- reactiveVal(FALSE)
  
  # Data refresh trigger
  data_refresh_trigger <- reactiveVal(0)
  
  # Admin login
  observeEvent(input$admin_login, {
    if (input$admin_password == ADMIN_PASSWORD) {
      admin_authenticated(TRUE)
      output$admin_status <- renderUI(
        tags$div(class="text-success", icon("check"), " Admin authenticated successfully!")
      )
    } else {
      admin_authenticated(FALSE)
      output$admin_status <- renderUI(
        tags$div(class="text-danger", icon("exclamation-triangle"), " Invalid password!")
      )
    }
  })

  # Non-admin: download standalone dashboard HTML
  output$download_dashboard_html <- downloadHandler(
    filename = function() {
      paste0("SNQ_Dashboard_Standalone_", Sys.Date(), ".html")
    },
    content = function(file) {
      # Use the packaged HTML in www directory
      src <- file.path(getwd(), "www", "snq_dash", "dashboard.html")
      if (!file.exists(src)) stop("dashboard.html not found at ", src)
      file.copy(src, file, overwrite = TRUE)
    }
  )
  
  # Admin logout
  observeEvent(input$admin_logout, {
    admin_authenticated(FALSE)
    updateTextInput(session, "admin_password", value = "")
  })

  # ======================== PROCESS DATA PAGE ================================
  
  # Reactive values to store data
  process_values <- reactiveValues(
    raw_data = NULL,
    node_level_long = NULL,
    ego_level_network_summary = NULL,
    activity_level_long = NULL,
    processing_complete = FALSE
  )
  
  # Enable process button when file is uploaded
  observe({
    if (!is.null(input$process_file)) {
      shinyjs::enable("process_btn")
    } else {
      shinyjs::disable("process_btn")
    }
  })

  # Workflow card navigation handlers (landing page)
  observeEvent(input$wf_access, {
    updateNavbarPage(session, inputId = "nav", selected = "access_survey")
  }, ignoreInit = TRUE)
  observeEvent(input$wf_process, {
    updateNavbarPage(session, inputId = "nav", selected = "processdata")
  }, ignoreInit = TRUE)
  observeEvent(input$go_plotting, {
    updateNavbarPage(session, inputId = "nav", selected = "plotting")
  }, ignoreInit = TRUE)
  
  # File upload with modern interface
  observeEvent(input$process_file, {
    req(input$process_file)
    
    # Show file preview
    file_name <- input$process_file$name
    file_size <- format_file_size(input$process_file$size)
    
    shinyjs::runjs(paste0("
      document.getElementById('file-preview').style.display = 'block';
      document.getElementById('file-name').textContent = '", file_name, "';
      document.getElementById('file-size').textContent = '", file_size, "';
      document.getElementById('upload-zone').style.display = 'none';
    "))
    
    # Show upload progress
    shinyjs::runjs("
      document.getElementById('upload-progress-container').style.display = 'block';
      document.getElementById('progress-fill').style.width = '0%';
      document.getElementById('progress-text').textContent = '0%';
    ")
    
    # Simulate progress
    for (i in 1:10) {
      shinyjs::runjs(paste0("
        setTimeout(function() {
          document.getElementById('progress-fill').style.width = '", i * 10, "%';
          document.getElementById('progress-text').textContent = '", i * 10, "%';
        }, ", i * 100, ");
      "))
    }
    
    ext <- tools::file_ext(input$process_file$datapath)
    
    if (ext %in% c("csv", "xlsx", "xls")) {
      process_values$raw_data <- tryCatch({
        if (ext == "csv") {
          df <- read_csv(input$process_file$datapath, show_col_types = FALSE)
          # Remove Qualtrics header rows if they exist
          if (nrow(df) > 2) {
            first_row_content <- paste(as.character(df[1,]), collapse = " ")
            if (grepl("Start Date|End Date|Response Type", first_row_content)) {
              df <- df %>% slice(-(1:2))
            } else if (any(grepl("ImportId|QID", names(df)))) {
              df <- df %>% slice(-(1:2))
            }
          }
          df
        } else if (ext %in% c("xlsx", "xls")) {
          read_excel(input$process_file$datapath)
        }
      }, error = function(e) {
        # Show error status
        shinyjs::runjs(paste0("
          document.getElementById('upload-status').innerHTML = '<i class=\"fas fa-exclamation-triangle\"></i> Error reading file: ", e$message, "';
          document.getElementById('upload-status').className = 'upload-status error';
        "))
        NULL
      })
      
      # Show success status if data was loaded successfully
      if (!is.null(process_values$raw_data)) {
        shinyjs::runjs("
          document.getElementById('upload-progress-container').style.display = 'none';
          document.getElementById('upload-status').innerHTML = '<i class=\"fas fa-check-circle\"></i> File uploaded successfully! Ready to process.';
          document.getElementById('upload-status').className = 'upload-status success';
        ")
      }
    } else {
      # Show error status for wrong file type
      shinyjs::runjs("
        document.getElementById('upload-status').innerHTML = '<i class=\"fas fa-exclamation-triangle\"></i> Please upload a CSV or Excel file';
        document.getElementById('upload-status').className = 'upload-status error';
      ")
    }
    
    process_values$processing_complete <- FALSE
  })
  
  # Helper function to format file size
  format_file_size <- function(bytes) {
    if (bytes < 1024) {
      return(paste(bytes, "B"))
    } else if (bytes < 1024^2) {
      return(paste(round(bytes / 1024, 1), "KB"))
    } else {
      return(paste(round(bytes / (1024^2), 1), "MB"))
    }
  }
  
  # Remove file functionality
  observeEvent(input$remove_file, {
    shinyjs::runjs("
      document.getElementById('file-preview').style.display = 'none';
      document.getElementById('upload-zone').style.display = 'block';
      document.getElementById('upload-status').style.display = 'none';
    ")
    process_values$raw_data <- NULL
    process_values$processing_complete <- FALSE
  })
  
  # Process data when button is clicked
  observeEvent(input$process_btn, {
    req(process_values$raw_data)
    
    # Show processing status with spinner
    shinyjs::runjs("
      document.getElementById('processing-status').innerHTML = '<div class=\"processing-spinner\"></div> Processing your data... This may take a few moments.';
      document.getElementById('processing-status').className = 'processing-status processing';
    ")
     
     tryCatch({
      # Use the comprehensive pipeline (Option A): source and capture outputs
      # Parent must see base/utils and loaded packages; use globalenv() as parent
      env <- new.env(parent = globalenv())
      env$df <- process_values$raw_data
      # Ensure the extracted pipeline uses the already-uploaded DF (no re-read, no extra slicing)
      env$read_csv <- function(...) env$df
      env$slice <- function(.data, ...) .data
      # Source the canonical extracted pipeline derived from snq_pipeline.rmd
      sys.source("snq_pipeline_extracted.R", envir = env)
      # Debug: summarize what the pipeline saw and produced
      try({
        child_ids_in <- if ("ChildID" %in% names(env$df)) unique(as.character(env$df$ChildID)) else character(0)
        child_ids_out <- if (exists("ego_level_network_summary", envir = env) && "ChildID" %in% names(env$ego_level_network_summary)) {
          unique(as.character(env$ego_level_network_summary$ChildID))
        } else character(0)
        msg1 <- paste("[Process Debug] df rows:", nrow(env$df), " unique ChildIDs in: ", paste(child_ids_in, collapse=", "))
        msg2 <- paste("[Process Debug] ego rows:", if (exists("ego_level_network_summary", envir = env)) nrow(env$ego_level_network_summary) else NA,
            " unique ChildIDs out: ", paste(child_ids_out, collapse=", "), "\n")
        cat(msg1, "\n")
        cat(msg2)
        try({
          write(paste0(format(Sys.time(), "%F %T"), " ", msg1), file = "app.log", append = TRUE)
          write(paste0(format(Sys.time(), "%F %T"), " ", msg2), file = "app.log", append = TRUE)
        }, silent = TRUE)
      }, silent = TRUE)
      
      # Store the results produced by the comprehensive pipeline
      process_values$node_level_long <- env$node_level_long
      process_values$ego_level_network_summary <- env$ego_level_network_summary
      # activity_level_long removed from app scope
      process_values$processing_complete <- TRUE
      
      # Show success status
      shinyjs::runjs(paste0("
        document.getElementById('processing-status').innerHTML = '<i class=\"fas fa-check-circle\"></i> Processing completed successfully! Generated ", 
        nrow(process_values$ego_level_network_summary), " children and ", 
        nrow(process_values$node_level_long), " network nodes.';
        document.getElementById('processing-status').className = 'processing-status success';
      "))
      
     }, error = function(e) {
      # Show error status
      shinyjs::runjs(paste0("
        document.getElementById('processing-status').innerHTML = '<i class=\"fas fa-exclamation-triangle\"></i> Error processing data: ", e$message, "';
        document.getElementById('processing-status').className = 'processing-status error';
      "))
      
      cat("Detailed error:\n")
      print(e)
    })
  })
  
  # Download handlers
  output$download_node <- downloadHandler(
    filename = function() {
      paste0("node_level_long_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(process_values$node_level_long, file, row.names = FALSE)
    }
  )
  
  output$download_ego <- downloadHandler(
    filename = function() {
      paste0("ego_level_network_summary_", Sys.Date(), ".csv")
    },
     content = function(file) {
      write.csv(process_values$ego_level_network_summary, file, row.names = FALSE)
    }
  )
  
  # activity_level_long download removed
  
  output$download_all <- downloadHandler(
    filename = function() {
      paste0("SNQ_processed_data_", Sys.Date(), ".zip")
    },
     content = function(file) {
      temp_dir <- tempdir()
      
      node_file <- file.path(temp_dir, "node_level_long.csv")
      ego_file <- file.path(temp_dir, "ego_level_network_summary.csv")
      # activity_level_long removed
      
      write.csv(process_values$node_level_long, node_file, row.names = FALSE)
      write.csv(process_values$ego_level_network_summary, ego_file, row.names = FALSE)
      # no activity file
      
      zip(file, c(node_file, ego_file), flags = "-j")
    }
  )
  
  # Control visibility of download buttons
  output$show_download <- reactive({
    process_values$processing_complete
  })
  
  output$show_individual_downloads <- reactive({
    process_values$processing_complete
  })
  
  outputOptions(output, "show_download", suspendWhenHidden = FALSE)
  outputOptions(output, "show_individual_downloads", suspendWhenHidden = FALSE)
  
  # ---------- Full previews ----------
  output$preview_ego <- DT::renderDataTable({
    req(process_values$processing_complete)
    DT::datatable(process_values$ego_level_network_summary, options = list(scrollX = TRUE, pageLength = 25))
  })
  output$preview_node <- DT::renderDataTable({
    req(process_values$processing_complete)
    DT::datatable(process_values$node_level_long, options = list(scrollX = TRUE, pageLength = 25))
  })
  # activity_level_long preview removed
  
  
  # Load intake data
  intake_data <- reactive({
    # Trigger refresh when data_refresh_trigger changes
    data_refresh_trigger()
    
    path <- file.path("data", "intake.csv")
    if (file.exists(path)) {
      df <- read_csv(path, show_col_types = FALSE)
      if (!"email_sent" %in% names(df)) df$email_sent <- NA_character_
      if (!"researcher_name" %in% names(df)) df$researcher_name <- NA_character_
      if (!"requesting_survey" %in% names(df)) df$requesting_survey <- NA_character_
      # Admin column alias expected as request_survey
      df$request_survey <- df$requesting_survey
      df
    } else {
      data.frame(
        timestamp = character(),
        type = character(),
        institution = character(),
        lab = character(),
        email = character(),
        researcher_name = character(),
        phone_number = character(),
        purpose = character(),
        requesting_survey = character(),
        request_survey = character(),
        email_sent = character(),
        stringsAsFactors = FALSE
      )
    }
  })
  
  # Refresh data
  observeEvent(input$refresh_data, {
    # Increment trigger to force reactive update
    data_refresh_trigger(data_refresh_trigger() + 1)
  })
  
  # Data statistics
  output$data_stats <- renderUI({
    data <- intake_data()
    if (nrow(data) > 0) {
      tagList(
        tags$div(class="alert alert-info",
          tags$h6("Data Summary"),
          tags$p(tags$strong("Total submissions:"), nrow(data)),
          tags$p(tags$strong("Latest submission:"), max(data$timestamp)),
          tags$p(tags$strong("Unique institutions:"), length(unique(data$institution))),
          tags$p(tags$strong("Unique labs:"), length(unique(data$lab))),
          tags$p(tags$strong("With phone numbers:"), sum(nzchar(data$phone_number))),
          tags$p(tags$strong("Requesting survey:"), sum(data$requesting_survey == "yes", na.rm = TRUE))
        )
      )
    } else {
      tags$div(class="alert alert-warning", "No intake data available.")
    }
  })
  
  # Admin intake table
  output$admin_intake_table <- renderDT({
    data <- intake_data()
    if (nrow(data) > 0) {
      datatable(
        data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          order = list(list(0, 'desc'))  # Sort by timestamp descending
        ),
        rownames = FALSE
      )
    } else {
      datatable(
        data.frame(Message = "No data available"),
        options = list(pageLength = 1, dom = 't'),
        rownames = FALSE
      )
    }
  })
  
  # Download intake data
  output$download_intake <- downloadHandler(
    filename = function() {
      paste0("intake_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      data <- intake_data()
      write_csv(data, file)
    }
  )

  # Download handler for test data (Process Data page)
  output$download_test_data <- downloadHandler(
    filename = function() {
      "SNQ_Test_Data.csv"
    },
    content = function(file) {
      test_file_path <- "/Users/beiniou/Desktop/STEEG/Social+Network+Questionnaire+-+Bethany+working_September+28,+2025_04.53.csv"
      if (file.exists(test_file_path)) {
        file.copy(test_file_path, file, overwrite = TRUE)
      } else {
        stop("Test data file not found")
      }
    }
  )

  # Download handler for snq_pipeline.rmd (Process Data page)
  output$download_pipeline_rmd <- downloadHandler(
    filename = function() {
      "snq_pipeline.rmd"
    },
    content = function(file) {
      file.copy("snq_pipeline.rmd", file, overwrite = TRUE)
    }
  )
  
  # Load help requests data
  help_requests_data <- reactive({
    data_refresh_trigger()
    
    path <- file.path("data", "help_requests.csv")
    if (file.exists(path)) {
      df <- read_csv(path, show_col_types = FALSE)
      if (!"email_sent" %in% names(df)) df$email_sent <- NA_character_
      df
    } else {
      data.frame(
        timestamp = character(),
        type = character(),
        name = character(),
        email = character(),
        subject = character(),
        message = character(),
        uploaded_files = character(),
        status = character(),
        email_sent = character(),
        stringsAsFactors = FALSE
      )
    }
  })

  # Refresh help requests
  observeEvent(input$refresh_help, {
    data_refresh_trigger(data_refresh_trigger() + 1)
  })

  # Help requests table
  output$admin_help_table <- renderDT({
    data <- help_requests_data()
    if (nrow(data) > 0) {
      datatable(
        data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          order = list(list(0, 'desc'))
        ),
        rownames = FALSE
      ) %>%
      formatStyle(
        "status",
        backgroundColor = styleEqual("pending", "#fff3cd")
      )
    } else {
      datatable(
        data.frame(Message = "No help requests available"),
        options = list(pageLength = 1, dom = 't'),
        rownames = FALSE
      )
    }
  })

  # Download help requests
  output$download_help_requests <- downloadHandler(
    filename = function() {
      paste0("help_requests_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      data <- help_requests_data()
      write_csv(data, file)
    }
  )
  
  # Admin authentication status for UI
  output$admin_authenticated <- reactive({
    admin_authenticated()
  })
  outputOptions(output, "admin_authenticated", suspendWhenHidden = FALSE)

  # Debug information
  output$debug_status <- renderUI({
    tagList(
      tags$p(tags$strong("R Version:"), R.version.string),
      tags$p(tags$strong("Shiny Version:"), packageVersion("shiny")),
       tags$p(tags$strong("Memory Usage:"), {
         mem_txt <- tryCatch({
           paste(round(memory.size()/1024/1024, 2), "MB")
         }, error = function(...) "n/a")
         mem_txt
       }),
      tags$p(tags$strong("Data Directory:"), file.path(getwd(), "data")),
      tags$p(tags$strong("Log File:"), ifelse(file.exists(file.path("data", "app.log")), 
                                              "Available", "Not created")),
      tags$p(tags$strong("Intake Records:"), nrow(intake_data())),
      tags$p(tags$strong("Session ID:"), session$token)
    )
  })
  
  # Debug logs
  output$debug_logs <- renderUI({
    log_file <- file.path("data", "app.log")
    if (file.exists(log_file)) {
      logs <- readLines(log_file, warn = FALSE)
      recent_logs <- tail(logs, 10) # Show last 10 log entries
      if (length(recent_logs) > 0) {
        tags$pre(paste(recent_logs, collapse = "\n"), 
                style = "font-size: 0.8em; max-height: 200px; overflow-y: auto;")
      } else {
        tags$p("No logs available")
      }
    } else {
      tags$p("Log file not found")
    }
  })
  
  # Clear logs
  observeEvent(input$clear_logs, {
    log_file <- file.path("data", "app.log")
    if (file.exists(log_file)) {
      file.remove(log_file)
      log_message("Log file cleared by admin")
      output$debug_logs <- renderUI(tags$p("Logs cleared"))
    }
  })
  
  # Export logs
  output$export_logs <- downloadHandler(
    filename = function() {
      paste0("app_logs_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
    },
    content = function(file) {
      log_file <- file.path("data", "app.log")
      if (file.exists(log_file)) {
        file.copy(log_file, file)
      } else {
        write("No logs available", file)
      }
    }
  )

  # ---------------- Help & Contact with file uploads and enhanced validation --------------
  clear_help_errors <- function() {
    output$c_name_error <- renderUI(NULL)
    output$c_email_error <- renderUI(NULL)
    output$c_subject_error <- renderUI(NULL)
    output$c_body_error <- renderUI(NULL)
    output$issue_status <- renderUI(NULL)
  }

  # File upload helper - save uploaded files to data directory
  save_uploaded_file <- function(file_input, prefix = "help") {
    if (is.null(file_input) || nrow(file_input) == 0) return(NULL)
    
    saved_files <- character(0)
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    
    for (i in seq_len(nrow(file_input))) {
      original_name <- file_input$name[i]
      file_ext <- tools::file_ext(original_name)
      new_name <- paste0(prefix, "_", timestamp, "_", i, ".", file_ext)
      dest_path <- file.path("data", "uploads", new_name)
      
      # Create uploads directory if it doesn't exist
      dir.create(file.path("data", "uploads"), showWarnings = FALSE, recursive = TRUE)
      
      # Copy file
      file.copy(file_input$datapath[i], dest_path, overwrite = TRUE)
      saved_files <- c(saved_files, paste0(original_name, " -> ", new_name))
    }
    
    return(paste(saved_files, collapse = "; "))
  }

  # ---- Help & Contact Submit Handler ----
  observeEvent(input$submit_issue, ignoreInit = TRUE, {
    # 1) grab and clean inputs
    name    <- trim2(input$c_name)
    email   <- trim2(input$c_email)
    subject <- trim2(input$c_subject)
    body    <- trim2(input$c_body)

    # 2) reset per-field errors
    output$c_name_error    <- clear_ui()
    output$c_email_error   <- clear_ui()
    output$c_subject_error <- clear_ui()
    output$c_body_error    <- clear_ui()
    output$issue_status    <- clear_ui()

    # 3) validate
    bad <- FALSE
    if (name == "")    { output$c_name_error    <- show_error(output$c_name_error,    "Name is required."); bad <- TRUE }
    if (email == "")   { output$c_email_error   <- show_error(output$c_email_error,   "Email is required."); bad <- TRUE }
    if (email != "" && !email_is_valid(email)) {
      output$c_email_error <- show_error(output$c_email_error, "Email format looks invalid."); bad <- TRUE
    }
    if (subject == "") { output$c_subject_error <- show_error(output$c_subject_error, "Subject is required."); bad <- TRUE }
    if (body == "")    { output$c_body_error    <- show_error(output$c_body_error,    "Message is required."); bad <- TRUE }

    if (bad) {
      output$issue_status <- renderUI(div(class="alert alert-danger", "Please fix the errors above and resubmit."))
      return(invisible(NULL))
    }

    # 4) build and send request (GET with URL parameters)
    # Build URL manually to avoid URLSearchParams issues
    base_url <- WEB_APP_SUPPORT
    params <- list(
      name    = name,
      email   = email,
      subject = subject,
      message = body
    )
    
    # Manual URL encoding
    encoded_params <- sapply(params, function(x) {
      if (is.null(x) || is.na(x)) return("")
      URLencode(as.character(x), reserved = TRUE)
    })
    
    param_string <- paste(
      paste0(names(encoded_params), "=", encoded_params),
      collapse = "&"
    )
    
    full_url <- paste0(base_url, "?", param_string)
    
    req <- request(full_url) |>
      req_method("GET") |>
      req_timeout(15) |>
      req_error(is_error = function(resp) FALSE)  # let us inspect non-2xx

    resp <- tryCatch(req_perform(req), error = function(e) e)

    # 5) handle transport errors
    if (inherits(resp, "error")) {
      output$issue_status <- renderUI(
        div(class="alert alert-danger",
            strong("Network error: "), HTML(htmltools::htmlEscape(conditionMessage(resp))),
            br(), "If this persists, email woodwardlab@uchicago.edu.")
      )
      return(invisible(NULL))
    }

    # 6) parse response
    ok  <- FALSE
    msg <- NULL
    code <- resp_status(resp)

    if (code >= 200 && code < 300) {
      # try JSON body
      parsed <- tryCatch(resp_body_json(resp), error = function(e) NULL)
      if (!is.null(parsed) && isTRUE(parsed$ok)) {
        ok <- TRUE
    } else {
        # some deployments return empty body on success — treat 2xx as success
        ok <- TRUE
      }
    }

    # 7) update UI
    if (ok) {
      output$issue_status <- renderUI(
        div(class="alert alert-success",
            icon("check-circle"), " Your support request was sent. We'll reply within 24–48 hours.")
      )
      # keep name/email; clear subject/body so they can send another issue quickly
      updateTextInput(session, "c_subject", value = "")
      updateTextAreaInput(session, "c_body", value = "")
      # Persist to help_requests.csv with email_sent success
      try({
        row <- data.frame(
          timestamp = format(Sys.time(), "%F %T"),
          type = "support",
          name = name,
          email = email,
          subject = subject,
          message = body,
          uploaded_files = "",
          status = "sent",
          email_sent = "success",
          stringsAsFactors = FALSE
        )
        safe_append_csv(file.path("data","help_requests.csv"), row)
        data_refresh_trigger(data_refresh_trigger() + 1)
      }, silent = TRUE)
    } else {
      body_txt <- tryCatch(resp_body_string(resp), error = function(e) "")
      output$issue_status <- renderUI(
        div(class="alert alert-danger",
            icon("exclamation-triangle"), 
            sprintf(" Send failed (HTTP %s).", code),
            if (nzchar(body_txt)) div(class="small mt-1", HTML(htmltools::htmlEscape(substr(body_txt, 1, 500)))) else NULL,
            div(class="mt-1", "Please try again or email ", tags$strong("woodwardlab@uchicago.edu"), ".")
        )
      )
      # Persist failed attempt
      try({
        row <- data.frame(
          timestamp = format(Sys.time(), "%F %T"),
          type = "support",
          name = name,
          email = email,
          subject = subject,
          message = body,
          uploaded_files = "",
          status = paste0("HTTP ", code),
          email_sent = "fail",
          stringsAsFactors = FALSE
        )
        safe_append_csv(file.path("data","help_requests.csv"), row)
        data_refresh_trigger(data_refresh_trigger() + 1)
      }, silent = TRUE)
    }
  })

  # Retry help submission handler
  observeEvent(input$retry_help, {
    output$issue_status <- renderUI(NULL)
  })

  observeEvent(input$compose_email, {
    if (nzchar(trimws(input$c_subject)) && nzchar(trimws(input$c_body))) {
      subj <- utils::URLencode(paste("CSNQ Support:", input$c_subject), reserved = TRUE)
      body_text <- paste0("Name: ", input$c_name, "\nEmail: ", input$c_email, "\n\nIssue:\n", input$c_body)
      body <- utils::URLencode(body_text, reserved = TRUE)
      shinyjs::runjs(paste0("window.location.href='mailto:woodwardlab@uchicago.edu?subject=", subj, "&body=", body, "';"))
    } else {
      subj <- utils::URLencode("CSNQ Support Request", reserved = TRUE)
      shinyjs::runjs(paste0("window.location.href='mailto:woodwardlab@uchicago.edu?subject=", subj, "';"))
    }
  })
}

shinyApp(ui, server)
