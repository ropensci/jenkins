#' Jenkins Client
#'
#' Simple client for managing jobs and builds on a Jenkins server. Set a
#' default access token via the \code{JENKINS_PAT} environment variable.
#'
#' @section Methods:
#' \Sexpr[results=rd, stage=build, echo=FALSE]{jenkins:::generate_rd()}
#'
#' @references \url{https://wiki.jenkins.io/display/JENKINS/Terminology}
#'
#' @examples
#' \donttest{# Make a connection
#' jk <- jenkins(server = 'http://jenkins.ropensci.org', username = 'jeroen')
#'
#' # Do stuff
#' jk$server_info()
#' jk$project_build('magick')
#'
#' # It's now in the queue
#' jk$queue_list()
#'
#' # Check build status
#' jk$build_info('magick')
#'
#' # Get latest build log
#' jk$build_log('magick', build_id = 'lastCompletedBuild')
#' }
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
    curl::parse_headers_list(req$headers)$location
  }

  # Test server works
  if(server != "dummy")
    GET_JSON()

  # Exported methods
  local({
    server_info <- function(){
      GET_JSON()
    }
    build_info <- function(job_name, build_id = 'lastBuild'){
      # buildno can be integer or e.g. 'lastCompletedBuild'
      GET_JSON(sprintf('/job/%s/%s', curl_escape(job_name), as.character(build_id)))
    }
    build_log <- function(job_name, build_id = 'lastBuild'){
      GET_DATA(sprintf('/job/%s/%s/logText/progressiveText',
                       curl_escape(job_name), as.character(build_id)))
    }
    build_stop <- function(job_name, build_id = 'lastBuild'){
      POST_XML(sprintf('/job/%s/%s/stop',
                       curl_escape(job_name), as.character(build_id)))
    }
    project_build <- function(job_name){
      endpoint <- sprintf('/job/%s/build', curl_escape(job_name))
      url <- POST_XML(endpoint = endpoint)
      queue_id <- gsub(".*/([0-9]+)/?$", '\\1', url)
      queue_info(queue_id)
    }
    project_build_all <- function(delay = 0.5){
      jobs <- project_list()$name
      msg <- sprintf("This will build %d jobs. Are you sure?", length(jobs))
      if(isTRUE(utils::askYesNo(msg))){
        lapply(jobs, function(job_name){
          cat("Triggering build for:", job_name, "\n")
          project_build(job_name)
          Sys.sleep(delay)
        })
      }
    }
    project_list <- function(){
      tibblify(server_info()$jobs)
    }
    project_config <- function(job_name){
      GET_DATA(sprintf('/job/%s/config.xml', curl_escape(job_name)))
    }
    project_create <- function(job_name, xml_string){
      endpoint <- sprintf("/createItem?name=%s", curl_escape(job_name))
      POST_XML(endpoint = endpoint, data = xml_string)
    }
    project_update <- function(job_name, xml_string){
      endpoint <- sprintf('/job/%s/config.xml', curl_escape(job_name))
      POST_XML(endpoint = endpoint, data = xml_string)
    }
    project_delete <- function(job_name){
      endpoint <- sprintf('/job/%s/doDelete', curl_escape(job_name))
      POST_XML(endpoint = endpoint)
    }
    project_enable <- function(job_name){
      endpoint <- sprintf('/job/%s/enable', curl_escape(job_name))
      POST_XML(endpoint = endpoint)
    }
    project_disable <- function(job_name){
      endpoint <- sprintf('/job/%s/disable', curl_escape(job_name))
      POST_XML(endpoint = endpoint)
    }
    user_list <- function(){
      tibblify(GET_JSON("/asynchPeople")$users)
    }
    user_info <- function(user_name = username){
      endpoint <- paste0('/user/', curl_escape(user_name))
      GET_JSON(endpoint)
    }
    view_create <- function(view_name, xml_string){
      endpoint <- sprintf("/createView?name=%s", curl_escape(view_name))
      POST_XML(endpoint = endpoint, data = xml_string)
    }
    view_list <- function(){
      tibblify(server_info()$views)
    }
    view_info <- function(view_name){
      endpoint <- sprintf('/view/%s/config.xml', curl_escape(view_name))
      GET_DATA(endpoint)
    }
    view_update <- function(view_name, xml_string){
      endpoint <- sprintf('/view/%s/config.xml', curl_escape(view_name))
      POST_XML(endpoint = endpoint, data = xml_string)
    }
    view_delete <- function(view_name){
      endpoint <- sprintf('/view/%s/doDelete', curl_escape(view_name))
      POST_XML(endpoint = endpoint)
    }
    queue_list <- function(){
      out <- GET_JSON('/queue')
      tibblify(as.data.frame(out$items))
    }
    queue_info <- function(queue_id){
      GET_JSON(paste0('/queue/item/', queue_id))
    }
    queue_cancel <- function(queue_id){
      # Bug in Jenkins, it redirects to the just deleted project
      opts <- list(followlocation = FALSE)
      POST_XML(paste0('/queue/cancelItem?id=', queue_id), handle_opts = opts)
    }
    queue_cancel_all <- function(){
      queue <- queue_list()
      if(length(queue) && length(queue$id))
        sapply(queue$id, queue_cancel)
    }
    node_list <- function(){
      GET_JSON('/computer/api/json')
    }
    node_info <- function(node_name = "master"){
      node_name <- ifelse(identical(node_name, 'master'), '(master)', 'master')
      GET_JSON(paste0('/computer/', curl_escape(node_name)))
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

generate_rd <- function(){
  out <- paste(utils::capture.output(jenkins("dummy")), collapse = "\n")
  paste("\\preformatted{", "## jk <- jenkins('https://ci.yourserver.com')", out, "}\n", sep = "\n")
}
