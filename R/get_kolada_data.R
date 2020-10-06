#' Get an object from the Kolada API
#'
#' @param entity The entity of search query for KPI, municipality, OU, kpi_groups, municipality_groups
#' @param title The title of the corresponding entity
#' 
#' @import httr
#' @import jsonlite
#' @return Returns a data.frame containing the data corresponding to the entity
#' @export
#'
#' @examples getMetadata("municipality","Helsingborg")

getMetadata=function(entity,title=NULL)
{
  stopifnot(is.character(entity),is.atomic(entity))
  if(entity=="kpi"| entity=="municipality" | entity=="ou" | entity=="kpi_groups" |
     entity== "municipality_groups"){
     final_result=data.frame()
     url = GET(paste("http://api.kolada.se/v2/", entity, sep = "", "?title=", title))
     m2=content(url,as = "text")
     m3=fromJSON(m2)[["values"]]
     if(entity=="kpi"){
       final_result = data.frame(KPI_ID=m3$id, Title=m3$title, Description=m3$description)
       return(final_result)
     }
     if(entity=="municipality"){
       final_result = data.frame(Municipality_ID=m3$id, Title=m3$title, Type=m3$type)
       return(final_result)
     }
     if(entity=="ou"){
       final_result = data.frame(Ou_ID=m3$id, Municipality=m3$municipality, Title=m3$title)
       return(final_result)
     }
     if(entity=="kpi_groups"){
       result=fromJSON(content(url,"text",encoding = "utf-8"), flatten=TRUE)
       kpi.group.data.frame=as.data.frame(result)
       kpi.data.frame = data.frame(member_id = integer(), member_title = character())
       for (i in 1:nrow(kpi.group.data.frame)) {
         group_kpi = kpi.group.data.frame[i, 3][[1]]
         kpi.data.frame = rbind(kpi.data.frame, group_kpi)
       }
       return(kpi.data.frame)
     }
     if(entity=="municipality_groups"){
       result=fromJSON(content(url,"text",encoding = "utf-8"), flatten=TRUE)
       munic.group.data.frame=as.data.frame(result)
       munic.data.frame = data.frame(member_id = integer(), member_title = character())
       for (i in 1:nrow(munic.group.data.frame)) {
         group_munic = munic.group.data.frame[i, 3][[1]]
         munic.data.frame = rbind(munic.data.frame, group_munic)
       }
       return(munic.data.frame)
     }
     }
  else{
    return("Entity is not found")
  }
}

#' Retrieving data for given kpi, municipality and years
#'
#' @param kpi Entity KPI
#' @param municipality_id Passing the id of municipality 
#' @param year A vector of years
#' @import tidyverse
#' @import tidyr
#'
#' @return Returns a data.frame containing the data for a given kpi, municipality and year
#'
#' @examples fetch_all_given_entity("N00945",1860,c(2009,2007,2008))
#' @export

fetch_all_given_entity= function(kpi,municipality_id,year){
  stopifnot(is.atomic(kpi),is.atomic(municipality_id),is.character(kpi),is.numeric(municipality_id))
  final_result=data.frame()
  for (i in year){
    url = paste("http://api.kolada.se/v2/data/kpi",kpi,"municipality",municipality_id,"year",i,sep= "/")
    m1= GET(url)
    m2=content(m1,as = "text")
    m3=fromJSON(m2)[["values"]]
    m4=unnest(m3,"values")
    final_result= rbind(final_result,as.data.frame(m4))
  }
  return(final_result)
}

#' Retrieving data for given kpi and years
#'
#' @param kpi Entity KPI
#' @param year A vector of years
#' @import tidyverse
#' @import tidyr
#' @return Returns a data.frame containing the data for a given kpi and year
#'
#' @examples fetch_given_kpiandyear("N00905",2009)
#' @export
#' 
fetch_given_kpiandyear = function(kpi,year){
  stopifnot(is.character(kpi), is.vector(year),is.atomic(kpi))
  final_result=data.frame()
  for (i in year){
    url = paste("http://api.kolada.se/v2/data/kpi",kpi,"year",i,sep= "/")
    m1= GET(url)
    m2=content(m1,as = "text")
    m3=fromJSON(m2)[["values"]]
    m4=unnest(m3,"values")
    final_result= rbind(final_result,as.data.frame(m4))
  }
  return(final_result)
}

#' Retrieving data for given municipality and years
#'
#' @param municipality_id Passing the id of municipality 
#' @param year A vector of years
#' 
#' @import tidyverse
#' @import tidyr
#' @return Returns a data.frame containing the data for a given municipality and year
#'
#' @examples fetch_given_muncipalityandyear(1860,2009)
#' @export
#' 
fetch_given_muncipalityandyear = function(municipality_id,year){
  stopifnot(is.numeric(municipality_id), is.vector(year),is.atomic(municipality_id))
  final_result=data.frame()
  for (i in year){
    url = paste("http://api.kolada.se/v2/data/municipality",municipality_id,"year",i,sep= "/")
    m1= GET(url)
    m2=content(m1,as = "text")
    m3=fromJSON(m2)[["values"]]
    m4=unnest(m3,"values")
    final_result= rbind(final_result,as.data.frame(m4))
  }
  return(final_result)
}

#' Retrieving data for given kpi and municipality.
#'
#' @param kpi Entity KPI
#' @param municipality_id Passing the id of municipality 
#' 
#' @import tidyverse
#' @import tidyr
#' @return Returns a data.frame containing the data for a given kpi and municipality.
#'
#' @examples fetch_given_kpiandmuncipality_id("N00945",1860)
#' @export
#' 

fetch_given_kpiandmuncipality_id = function(kpi,municipality_id){
  stopifnot(is.character(kpi),is.atomic(kpi),is.atomic(municipality_id))
  final_result=data.frame()
    url = paste("http://api.kolada.se/v2/data/kpi",kpi,"municipality",municipality_id,sep= "/")
    m1= GET(url)
    m2=content(m1,as = "text")
    m3 = fromJSON(m2)[["values"]]
    m4 = unnest(m3,"values")
    final_result= as.data.frame(m4)
  return(final_result)
}

