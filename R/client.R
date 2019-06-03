#' Jenkins Client
#'
#' Simple client for managing jobs and builds on a Jenkins server. Set a
#' default access token via the \code{JENKINS_PAT} environment variable.
#'
#' @section Methods:
#' \code{# jk <- jenkins("https://dev.ropensci.org", user = "jeroen")}
#' \Sexpr[results=verbatim, stage=build, echo=FALSE]{jenkins::jenkins('dummy')}
#'
#' @references \url{https://wiki.jenkins.io/display/JENKINS/Terminology}
#'
#' @examples
#' \donttest{# Make a connection
#' jk <- jenkins(server = 'http://jenkins.ropensci.org', username = 'jeroen')
#'
#' # Do stuff
#' jk$info()
#' jk$job_build('magick')
#' jk$job_status('magick') }
#' @export
#' @import curl
#' @rdname jenkins
#' @param server base url of the jenkins server
#' @param username name of the jenkins user to login
#' @param token authentication token (or password) for your jenkins server.
#' @param verbose print http output for debugging
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

  POST_XML <- function(endpoint = "/", data = NULL, handle_opts = NULL){
    handle <- curl::new_handle(username = username, password = token, verbose = verbose,
                               httpauth = 1L, post = TRUE)
    if(length(handle_opts)){
      curl::handle_setopt(handle, .list = handle_opts)
    }
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
    invisible(text)
  }

  # Test server works
  if(server != "dummy")
    GET_JSON()

  # Exported methods
  local({
    info <- function(){
      GET_JSON()
    }
    build_start <- function(name){
      endpoint <- sprintf('/job/%s/build', curl_escape(name))
      POST_XML(endpoint = endpoint)
    }
    build_start_all <- function(delay = 0.5){
      jobs <- job_list()$name
      msg <- sprintf("This will build %d jobs. Are you sure?", length(jobs))
      if(isTRUE(utils::askYesNo(msg))){
        lapply(jobs, function(name){
          cat("Triggering build for:", name, "\n")
          job_build(name)
          Sys.sleep(delay)
        })
      }
    }
    build_status <- function(name, build_number = 'lastBuild'){
      # buildno can be integer or e.g. 'lastCompletedBuild'
      GET_JSON(sprintf('/job/%s/%s', curl_escape(name), as.character(build_number)))
    }
    build_log <- function(name, build_number = 'lastBuild'){
      GET_DATA(sprintf('/job/%s/%s/logText/progressiveText',
                       curl_escape(name), as.character(build_number)))
    }
    build_stop <- function(name, build_number = 'lastBuild'){
      POST_XML(sprintf('/job/%s/%s/stop',
                       curl_escape(name), as.character(build_number)))
    }
    project_list <- function(){
      tibblify(info()$jobs)
    }
    project_config <- function(name){
      GET_DATA(sprintf('/job/%s/config.xml', curl_escape(name)))
    }
    project_create <- function(name, xml_string){
      endpoint <- sprintf("/createItem?name=%s", curl_escape(name))
      POST_XML(endpoint = endpoint, data = xml_string)
    }
    project_update <- function(name, xml_string){
      endpoint <- sprintf('/job/%s/config.xml', curl_escape(name))
      POST_XML(endpoint = endpoint, data = xml_string)
    }
    project_delete <- function(name){
      endpoint <- sprintf('/job/%s/doDelete', curl_escape(name))
      POST_XML(endpoint = endpoint)
    }
    project_enable <- function(name){
      endpoint <- sprintf('/job/%s/enable', curl_escape(name))
      POST_XML(endpoint = endpoint)
    }
    project_disable <- function(name){
      endpoint <- sprintf('/job/%s/disable', curl_escape(name))
      POST_XML(endpoint = endpoint)
    }
    user_list <- function(){
      tibblify(GET_JSON("/asynchPeople")$users)
    }
    user_get <- function(name = username){
      endpoint <- paste0('/user/', curl_escape(name))
      GET_JSON(endpoint)
    }
    view_create <- function(name, xml_string){
      endpoint <- sprintf("/createView?name=%s", curl_escape(name))
      POST_XML(endpoint = endpoint, data = xml_string)
    }
    view_list <- function(){
      tibblify(info()$views)
    }
    view_get <- function(name){
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
    user_get <- function(name = username){
      endpoint <- paste0('/user/', curl_escape(name))
      GET_JSON(endpoint)
    }
    queue_list <- function(){
      out <- GET_JSON('/queue')
      tibblify(as.data.frame(out$items))
    }
    queue_info <- function(queue_id){

    }
    queue_cancel <- function(queue_id){
      # Bug in Jenkins, it redirects to the just deleted project
      opts <- list(followlocation = FALSE)
      POST_XML(paste0('/queue/cancelItem?id=', queue_id), handle_opts = opts)
    }
    queue_cancel_all <- function(){
      queue <- queue_list()
      lapply(queue$id, queue_cancel)
      invisible()
    }
    structure(environment(), class = c("jenkins", "jeroen", "environment"))
  })
}

jenkins_pat <- function(){
  Sys.getenv('JENKINS_PAT')
}

tibblify <- function(df){
  if(!is.data.frame(df)){
    warning("Argument is not a data frame")
  } else {
    class(df) <- c("tbl_df", "tbl", "data.frame")
  }
  df
}
