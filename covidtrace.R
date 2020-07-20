#' Crear gráfico con la red de contactos de pacientes testados positivos en COVID19
#'
#'@param codare,data,filtrofecha,salida,area codare=Ruta del cmap que por defecto será "D:/R/cmap.Rdata". data=Ruta de la tabla de datos que por defecto será=data = "D:/R/casos_contactos_13_07_2020.xlsx". filtrofecha=Fecha por la que se quiere filtrar, por defecto será 2020-07-05. salida=ruta de salida, por defecto será "D:/Program Files/Tableau/Tableau Server/data/tabsvc/httpd/htdocs/CasosContactos.html",area= string del area, por defecto será "pamplona"
#'@return grafico en html localizado en "salida"
#'@examples
#'covidtracer(codare="F:/Navarrabiomed/Teletrabajo/Dropbox/ju/covid19/Rdata/cmap.Rdata",data = "F:/Navarrabiomed/Teletrabajo/Dropbox/ju/covid19/dat/Gorricho/casos_contactos_13_07_2020.xlsx",salida="CasosContactos.html",area="pamplona")
#'@export
#

require(readxl)
require(dplyr)
require(lubridate)
require(epicontacts)
require(htmlwidgets)


mkobj= function(data, filtrofecha,codare) {
  require(readxl)
  require(dplyr)
  require(lubridate)
  require(epicontacts)

  load(codare)
  cc0713 <- read_excel(data,
                       col_types = c("text", "text", "numeric",
                                     "text", "text", "numeric", "text",
                                     "date", "text", "text", "numeric",
                                     "text", "date", "date", "numeric",
                                     "numeric", "text", "date", "date",
                                     "text", "text", "text", "date", "numeric",
                                     "date", "date"))

  names(cc0713)[8]="fdia"
  ## filtro de fecha
  cc0713=cc0713 %>% filter(fdia>filtrofecha)

  cc0713 <- cc0713 %>% group_by(`Pac#Unif_Cod_contacto`) %>% mutate(ncon=n()) %>% ungroup() %>% data.frame()

  ## casos -> extrae individuos
  ica= cc0713 %>% select(1:6,8)
  names(ica) <- c("id","cipna", "edad", "gen", "cupo", "zbs"  ,  "fdia")
  ica= ica %>% mutate(gen=factor(gen),zbs=as.numeric(zbs),cupo=factor(cupo),fdia=as.Date(fdia), positivo=TRUE) %>% distinct()

  ## contactos -> extrae individuos
  ico= cc0713 %>% select(9:12,14,19:21,23,25:26)
  names(ico)<- c("id","cipna", "edad", "gen",    "fais"  ,"fsin","cupo", "zbs", "ffin","fdia","fpcr_")
  ico=ico %>% mutate(gen=factor(gen),zbs=as.numeric(zbs),cupo=factor(cupo),fdia=as.Date(fdia), positivo=!is.na(fdia) )  %>% distinct()

  ## fusion tabla de individuos

  linelist=bind_rows(ica,ico) %>% distinct()

  # encontramos el mismo contacto introducido diferentes veces, line 3961.. y que difieren en fechas... se escogen de entre ellas la fecha minima en que aparezca como positivo
  linelist=linelist %>% arrange(id,-positivo,fais) %>% group_by(id) %>% mutate(fdia=min(fdia)) %>% distinct(id, .keep_all = TRUE) %>% data.frame()
  ## meto area y literal de zbs
  linelist=left_join(linelist,select(cmap,zbs,area,lzbs))

  ## extraeremos una lista con dos objetos, la diferencia, el primero usa el id=pacunif, el segundo CIPNA

  # anonimizado
  cacoA=cc0713 %>% select(1,9,13,17,27)
  names(cacoA)=c("from", "to", "fuc","exposure", "ncon")
  cacoA=cacoA %>% mutate(from=factor(from), to=factor(to),fuc=as.Date(fuc), exposure=factor(exposure))

  ## para vigilancia añado las diferencias de dias con CIPNA
  cacoC=cc0713 %>% select(2,10,13,17,27)
  names(cacoC)=c("from", "to", "fuc","exposure", "ncon")
  cacoC=cacoC %>% mutate(from=factor(from), to=factor(to),fuc=as.Date(fuc), exposure=factor(exposure))

  cv0713A <- make_epicontacts(linelist = linelist[,-2],
                              contacts = cacoA,
                              id="id",
                              directed = FALSE)

  cv0713C <- make_epicontacts(linelist = linelist[,-1],    contacts = cacoC,  id="cipna",  directed = FALSE)

  return(list(punif=cv0713A, cipna=cv0713C))
}



xare= function(ldata,larea) {
  dat=subset(ldata, node_attribute = list("area" = larea))
  dat=thin(dat, what="contacts")
  vis_epicontacts(dat, node_color = "positivo",  node_shape="gen",  shapes=c(Mujer="female", Hombre="male"), edge_label="exposure", legend = FALSE)
}



covidtracer<-function(codare = "D:/R/cmap.Rdata",data = "D:/R/casos_contactos_13_07_2020.xlsx",filtrofecha="2020-07-05",salida="D:/Program Files/Tableau/Tableau Server/data/tabsvc/httpd/htdocs/CasosContactos.html",area="pamplona"){
  require(htmlwidgets)
  enlis=mkobj(data,filtrofecha,codare)
  return(htmlwidgets::saveWidget(xare(enlis[[2]],area),file=salida))
}
