#' Jenkins
#'
#' Some wrappers to control jobs and builds on a Jenkins server.
#'
#' @export
#' @import curl
#' @rdname jenkins
#' @param server naked url of the server
#' @param username name of the user to login
#' @param token authentication token (or password)
#' @param verbose show http output
jenkins <- function(server = 'http://jenkins.ropensci.org', username = 'jeroen',
                    token = jenkins_pat(), verbose = FALSE){
  server <- gsub("/$", "", server)
  GET_JSON <- function(endpoint = "/"){
    endpoint <- paste0(sub("/$", "", endpoint), '/api/json')
    jsonlite::fromJSON(GET_DATA(endpoint = endpoint))
  }

  GET_DATA <- function(endpoint = "/"){
    handle <- curl::new_handle(username = username, password = token, verbose = verbose, httpauth = 1L)
    url <- paste0(server, endpoint)
    req <- curl::curl_fetch_memory(url, handle = handle)
    text <- rawToChar(req$content)
    if(req$status >= 400){
      stop(sprintf("HTTP ERROR %d: %s",req$status, text), call. = FALSE)
    }
    return(text)
  }

  POST_XML <- function(endpoint = "/", data = NULL){
    handle <- curl::new_handle(username = username, password = token, verbose = verbose,
                               httpauth = 1L, post = TRUE)
    if(length(data)){
      buf <- charToRaw(data)
      size <- length(buf)
      con <- rawConnection(buf, open = "rb")
      on.exit(close(con))
      readfunction <- function(n, ...){
        readBin(con = con, raw(), n = n)
      }
      handle_setopt(handle, readfunction = readfunction, postfieldsize_large = size)
      handle_setheaders(handle, "Content-Type" = "application/xml")
    }
    url <- paste0(server, sub("/$", "", endpoint))
    req <- curl::curl_fetch_memory(url, handle = handle)
    text <- rawToChar(req$content)
    if(req$status >= 400){
      stop(sprintf("HTTP ERROR %d: %s",req$status, text), call. = FALSE)
    }
    return(text)
  }

  # Test server works
  GET_JSON()

  # Exported methods
  local({
    info <- function(){
      GET_JSON()
    }
    job_list <- function(){
      info()$jobs
    }
    job_get <- function(name){
      GET_DATA(sprintf('/job/%s/config.xml', curl_escape(name)))
    }
    job_build <- function(name){
      endpoint <- sprintf('/job/%s/build', curl_escape(name))
      POST_XML(endpoint = endpoint)
      invisible()
    }
    job_create <- function(name, xml_string){
      endpoint <- sprintf("/createItem?name=%s", curl_escape(name))
      POST_XML(endpoint = endpoint, data = xml_string)
      invisible()
    }
    job_update <- function(name, xml_string){
      endpoint <- sprintf('/job/%s/config.xml', curl_escape(name))
      POST_XML(endpoint = endpoint, data = xml_string)
      invisible()
    }
    job_delete <- function(name){
      endpoint <- sprintf('/job/%s/doDelete', curl_escape(name))
      POST_XML(endpoint = endpoint)
      invisible()
    }
    job_build_all <- function(wait = 0.5){
      jobs <- jk$job_list()$name
      msg <- sprintf("This will build %d jobs. Are you sure?", length(jobs))
      if(isTRUE(askYesNo(msg))){
        lapply(jobs, function(name){
          cat("Triggering build for:", name, "\n")
          jk$job_build(name)
          Sys.sleep(wait)
        })
      }
    }
    user_list <- function(){
      GET_JSON("/people")
    }
    user_get <- function(name = username){
      endpoint <- paste0('/user/', curl_escape(name))
      GET_JSON(endpoint)
    }
    view_create <- function(name, xml_string){
      endpoint <- sprintf("/createView?name=%s", curl_escape(name))
      POST_XML(endpoint = endpoint, data = xml_string)
      invisible()
    }
    view_list <- function(){
      info()$views
    }
    job_get <- function(name){
      endpoint <- sprintf('/view/%s/config.xml', curl_escape(name))
      GET_DATA(endpoint)
    }
    view_update <- function(name, xml_string){
      endpoint <- sprintf('/view/%s/config.xml', curl_escape(name))
      POST_XML(endpoint = endpoint, data = xml_string)
      invisible()
    }
    view_delete <- function(name){
      endpoint <- sprintf('/view/%s/doDelete', curl_escape(name))
      POST_XML(endpoint = endpoint)
      invisible()
    }
    structure(environment(), class=c("jenkins", "jeroen", "environment"))
  })
}

#' @export
#' @param git_url HTTPS git url of the target repository
#' @rdname jenkins
config_template <- function(git_url){
  if(!grepl("^https://", git_url))
    stop("Please use https git URL")
  template <- system.file('templates/config.xml', package = 'jenkins')
  input <- rawToChar(readBin(template, raw(), file.info(template)$size))
  gsub("INSERT_GIT_REPO_URL", git_url, input, fixed = TRUE)
}

#' @export
#' @param view_name Full name (with spaces) of the view
#' @param view_jobs Character vector with jobs to add to this view
#' @rdname jenkins
view_template <- function(view_jobs){
  template <- system.file('templates/view.xml', package = 'jenkins')
  input <- rawToChar(readBin(template, raw(), file.info(template)$size))
  jobstring <- paste(sprintf('    <string>%s</string>', view_jobs), collapse = "\n")
  gsub("INSERT_VIEW_JOBS", jobstring, input, fixed = TRUE)
}

#' @export
#' @rdname jenkins
#' @param update_jobs update the xml config of existing repos.
#' @param remove_jobs delete jobs that are no longer in the registry
#' @param update_views update the views (per-author package list)
sync_jenkins_ropensci <- function(update_jobs = FALSE, remove_jobs = TRUE, update_views = TRUE){
  jk <- jenkins('http://jenkins.ropensci.org')
  jobs <- jk$job_list()
  url <- "https://ropensci.github.io/roregistry/registry.json"
  packages <- jsonlite::fromJSON(url)$packages
  for(i in seq_len(nrow(packages))){
    name <- packages[i, "name"]
    xml <- config_template(packages[i, "url"])
    if(name %in% jobs$name){
      if(isTRUE(update_jobs)){
        cat(sprintf("Updating job config for %s...", name))
        jk$job_update(name = name, xml_string = xml)
      } else {
        cat(sprintf("Job config for %s already exists...", name))
      }
    } else {
      cat(sprintf("Creating new job for %s...", name))
      jk$job_create(name = name, xml_string = xml)
    }
    cat("OK!\n")
  }
  if(isTRUE(remove_jobs)){
    gone <- !(jobs$name %in% packages$name)
    lapply(jobs$name[gone], function(name){
      cat(sprintf("Deleting job %s which is no longer in the roregistry...", name))
      jk$job_delete(name)
      cat("OK!\n")
    })
  }
  if(isTRUE(update_views)){
    views <- jk$view_list()
    packages$maintainer <- asciify(packages$maintainer)
    authors <- unique(packages$maintainer)
    lapply(authors, function(author){
      pkg_names = packages[packages$maintainer == author, "name"]
      if(!length(pkg_names))
        stop(sprintf("Failed to find packages for author %s", author))
      xml <- view_template(pkg_names)
      if(author %in% views$name){
        cat(sprintf("Updating view for %s...", author))
        jk$view_update(name = author, xml_string = xml)
      } else {
        cat(sprintf("Creating new view for %s...", author))
        jk$view_create(name = author, xml_string = xml)
      }
      cat("OK!\n")
    })
    views_gone <- !(views$name %in% c(authors, 'all'))
    lapply(views$name[views_gone], function(author){
      cat(sprintf("Deleting view %s which is no longer a maintainer...", author))
      jk$view_delete(author)
      cat("OK!\n")
    })
  }
  invisible(jk$info())
}

# Not sure how well jenkins deals with strange characters...
asciify <- function(x){
  gsub("[^a-zA-Z0-9' .-]", "", stringi::stri_trans_general(x, "latin-ascii"))
}

jenkins_pat <- function(){
  Sys.getenv('JENKINS_PAT')
}
