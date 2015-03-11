# TODO: Add comment
# 
# Author: LB
###############################################################################

moddwl_read_xml_opts = function(previous_file = previous_file, xml_file = xml_file) {

require("XML")
require("plyr")
require("ggplot2")
require("gridExtra")
prod_opt_list = NULL   ; mod_prod_list = NULL

#xmlfile = file.choose()
xmlfile=xmlParse(xml_file)

xmltop = xmlRoot(xmlfile) #gives content of root
class(xmltop)#"XMLInternalElementNode" "XMLInternalNode" "XMLAbstractNode"
top_name = xmlName(xmltop) #give name of node, PubmedArticleSet

n_products = xmlSize(xmltop) #how many children in node, 19
prodnames = NULL
for (prod in 1:n_products)
{
#	browser()
	prodopts = list()
#	prodopts$product=xmlName(xmltop[[prod]])
	prodopts$product=xmlToList(xmltop[[prod]][["name"]])
	prodopts$main_out_folder=xmlToList(xmltop[[prod]][["main_out_folder"]])
	prodopts$native_res=xmlToList(xmltop[[prod]][["native_res"]])
	file_prefix_terra=xmlToList(xmltop[[prod]][['file_prefix_terra']])
	http_terra=xmlToList(xmltop[[prod]][['http_terra']])
	file_prefix_aqua=xmlToList(xmltop[[prod]][['file_prefix_aqua']])
	http_aqua=xmlToList(xmltop[[prod]][['http_aqua']])
	prodopts$www=xmlToList(xmltop[[prod]][['www']])
	prodopts$file_prefix = hash("Terra" = file_prefix_terra,"Aqua" = file_prefix_aqua)
	prodopts$http = hash("Terra" = http_terra,"Aqua" = http_aqua)
	prodopts$multiband_bsq = T
	nbands = xmlSize(xmltop[[prod]][["bands"]])
	bandnames = NULL ; 	band_fullname = NULL ;	datatype = NULL ; 	nodata_in = NULL ; 	nodata_out = NULL
	for (band in 1:nbands) {
		bandnames = c(bandnames,xmlToList(xmltop[[prod]][['bands']][[band]][["bandname"]]))
		band_fullname = c(band_fullname,xmlToList(xmltop[[prod]][['bands']][[band]][["fullname"]]))
		datatype = c(datatype,xmlToList(xmltop[[prod]][['bands']][[band]][["datatype"]]))
		nodata_in = c(nodata_in,xmlToList(xmltop[[prod]][['bands']][[band]][["nodata_in"]]))
		nodata_out = c(nodata_out,xmlToList(xmltop[[prod]][['bands']][[band]][["nodata_out"]]))
	} #End Cycle on band
	prodopts$bandnames = bandnames
	prodopts$band_fullnames = band_fullname
	datatype = as.factor(datatype)
	datatype = revalue(datatype, c("8-bit signed integer"="Byte",
											"8-bit unsigned integer"="Byte",
											"16-bit signed integer"="Int16",
											"16-bit unsigned integer"="UInt16",
											"32-bit signed integer"="Int32",
											"32-bit unsigned integer"="UInt32"),warn_missing = F)
	prodopts$datatype = datatype
	prodopts$nodata_in = nodata_in
	prodopts$nodata_out = nodata_out
	prodopts$bandsel = rep(0, length(prodopts$bandnames))  
	
	nindexes = xmlSize(xmltop[[prod]][["indexes"]])
if (nindexes > 0 ) {
	indexes_bandnames = NULL
	indexes_fullnames = NULL
	indexes_formulas = NULL
	indexes_nodata_out = NULL
	for (index in 1:nindexes) {
		indexes_bandnames = c(indexes_bandnames,xmlToList(xmltop[[prod]][['indexes']][[index]][["indexes_bandname"]]))
		indexes_fullnames = c(indexes_fullnames,xmlToList(xmltop[[prod]][['indexes']][[index]][["indexes_fullname"]]))
		indexes_formulas = c(indexes_formulas,xmlToList(xmltop[[prod]][['indexes']][[index]][["indexes_formula"]]))
		indexes_nodata_out = c(indexes_nodata_out,xmlToList(xmltop[[prod]][['indexes']][[index]][["indexes_nodata_out"]]))
	} #End Cycle on index
	
	prodopts$indexes_bandnames = indexes_bandnames
	prodopts$indexes_fullnames = indexes_fullnames
	prodopts$indexes_formulas = indexes_formulas
	prodopts$indexes_nodata_out = indexes_nodata_out
	prodopts$indexes_bandsel = rep(0, length(prodopts$indexes_bandnames)) 
} 
	nquality = xmlSize(xmltop[[prod]][["quality_indicators"]])
	if (nquality > 0 ) {
	quality_bandnames = NULL
	quality_fullnames = NULL
	quality_source = NULL
	quality_bitN = NULL
	for (quality in 1:nquality) {
		quality_bandnames = c(quality_bandnames,xmlToList(xmltop[[prod]][['quality_indicators']][[quality]][["quality_bandname"]]))
		quality_fullnames = c(quality_fullnames,xmlToList(xmltop[[prod]][['quality_indicators']][[quality]][["quality_fullname"]]))
		quality_source = c(quality_source,xmlToList(xmltop[[prod]][['quality_indicators']][[quality]][["quality_source"]]))
		quality_bitN = c(quality_bitN,xmlToList(xmltop[[prod]][['quality_indicators']][[quality]][["quality_bitN"]]))
	} #End Cycle on quality
	
	prodopts$quality_bandnames = quality_bandnames
	prodopts$quality_fullnames = quality_fullnames
	prodopts$quality_source = quality_source
	prodopts$quality_bitN = quality_bitN
	prodopts$quality_nodata_in =  rep(255, length(prodopts$quality_bandnames))  # nodata in for quality bands (dummy - always 255)
	prodopts$quality_nodata_out =  rep(255, length(prodopts$quality_bandnames)) # nodata out for quality bands (always 255)
	prodopts$quality_bandsel = rep(0, length(prodopts$quality_bandnames))  	 #Selection of desired quality bands (all zeroes)
	} 
 #End Cycle on prodname

mod_prod_list = c(mod_prod_list, prodopts$product ) 
prod_opt_list [[prodopts$product ]] =prodopts

#prod_opt_list
}
save(prod_opt_list, mod_prod_list, file= previous_file)
#save(prod_opt_list, mod_prod_list, file= previous_file)
}
