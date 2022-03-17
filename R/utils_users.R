get_user <- function(dev_user = "paul.campbell@epicentre.msf.org") {
  username <- Sys.getenv("SHINYPROXY_USERNAME")
  # if not running on shinyproxy set default dev username
  if (username == "") username <- dev_user
  username
}

get_user_groups <- function(dev_groups = c("admins", "msf", "epicentre.msf.org")) {
  usergroups <- Sys.getenv("SHINYPROXY_USERGROUPS")
  # if not running on shinyproxy set default dev user groups
  if (usergroups == "") {
    usergroups <- dev_groups
  } else {
    usergroups <- tolower(strsplit(usergroups, split = ",")[[1]])
  }
  usergroups
}
