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
    buf <- charToRaw(data)
    size <- length(buf)
    con <- rawConnection(buf, open = "rb")
    on.exit(close(con))
    readfunction <- function(n, ...){
      readBin(con = con, raw(), n = n)
    }
    handle <- curl::new_handle(username = username, password = token, verbose = verbose, httpauth = 1L,
                               post = TRUE, readfunction = readfunction, postfieldsize_large = size)
    handle_setheaders(handle, "Content-Type" = "application/xml")
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
      GET_DATA(sprintf('/job/%s/config.xml', name))
    }
    job_create <- function(name, xml_string){
      endpoint <- sprintf("/createItem?name=%s", name)
      POST_XML(endpoint = endpoint, data = xml_string)
      invisible()
    }
    job_update <- function(name, xml_string){
      endpoint <- sprintf('/job/%s/config.xml', name)
      POST_XML(endpoint = endpoint, data = xml_string)
      invisible()
    }
    job_delete <- function(name){
      endpoint <- sprintf('/job/%s/doDelete', name)
      POST_XML(endpoint = endpoint, data = "")
      invisible()
    }
    user_list <- function(){
      GET_JSON("/people")
    }
    user_get <- function(name = username){
      GET_JSON(paste0('/user/', name))
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
#' @rdname jenkins
#' @param update update the xml config of existing repos.
#' @param remove delete jobs that are no longer in the registry
sync_jenkins_ropensci <- function(update = FALSE, remove = TRUE){
  jk <- jenkins('http://jenkins.ropensci.org')
  jobs <- jk$job_list()
  url <- "https://ropensci.github.io/roregistry/registry.json"
  packages <- jsonlite::fromJSON(url)$packages
  for(i in seq_len(nrow(packages))){
    name <- packages[i, "name"]
    xml <- config_template(packages[i, "url"])
    if(name %in% jobs$name){
      if(isTRUE(update)){
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
  if(isTRUE(remove)){
    gone <- !(jobs$name %in% packages$name)
    lapply(jobs$name[gone], function(name){
      cat(sprintf("Deleting job %s which is no longer in the roregistry...", name))
      jk$job_delete(name)
      cat("OK!\n")
    })
  }
  jk$job_list()
}

jenkins_pat <- function(){
  Sys.getenv('JENKINS_PAT')
}
