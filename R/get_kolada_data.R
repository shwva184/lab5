#' Get an object from the Kolada API
#'
#' @param entity The entity of search query for KPI, municipality, OU, kpi_groups, municipality_groups
#'
#' @import httr
#' @import jsonlite
#' @return Returns a data.frame containing the data corresponding to the entity
#' @export
#'
#' @examples getMetadata("municipality","Helsingborg")

getMetadata=function(entity,title=NULL)
{
  url=GET(paste("http://api.kolada.se/v2/", entity, sep = "", "?title=", title))
  if(entity=="kpi"| entity=="municipality" | entity=="ou" | entity=="kpi_groups" |
     entity== "municipality_groups"){
    result=fromJSON(content(url,"text",encoding = "utf-8"), flatten=TRUE)
  }
  if(entity=="kpi_groups"){
  kpi.group.data.frame=as.data.frame(result)
  kpi.data.frame = data.frame(member_id = integer(), member_title = character())
  for (i in 1:nrow(kpi.group.data.frame)) {
    group_kpi = kpi.group.data.frame[i, 3][[1]]
    kpi.data.frame = rbind(kpi.data.frame, group_kpi)
  }
  return(kpi.data.frame)
  }
  if (result["count"] == 0) return(data.frame())
  return(as.data.frame(result))
}