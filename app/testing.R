xmlChildren(onefile$doc$children$svg)
getNodeSet(onefile$doc$children$svg, "/g", "g")

getNodeSet(onefile$doc$children$svg, "/svg/*")
onefile <- xmlTreeParse(gsub("'","",readLines("app/www/head1.svg")),asText = TRUE)


library(xml2)
library(stringr)

change_fill <- function(file_contents, new_fill = "#aaaaff") {
  str_replace_all(file_contents, "fill:#[0-f]{6};", sprintf("fill:%s;", new_fill))
}
file_contents <- paste(gsub("'","",readLines("app/www/head1.svg")), collapse = "") %>% change_fill()
writeLines(file_contents, "app/www/test_head1.svg")

file_contents_fix <- str_replace_all(file_contents, "fill:#[0-f]{6};", sprintf("fill:%s;", new_fill))

doc <- file_contents %>% str_replace_all("[[:space:]]{2,}", " ") %>% read_xml(as_html = T)  
res <- doc %>% xml_find_all("//svg/g/g/path")
res_fix <- purrr::map(res, change_fill)


purrr::map2(res, res_fix, ~xml_replace(doc, .x, .y))
obj <- read_xml()
obj %>% xml_find_all("/svg/g/g/path")


doc %>% xml_attr("path", ns = res)

xml2_version <- read_xml('app/www/head1.svg')
xml2_version
xml_name(xml2_version)
xml_children(xml2_version)
xml_text(xml2_version)
xml_attrs(xml2_version)
xml_list <- as_list(xml2_version)


library(svgparser)
library(grid)

file <- 'app/www/head1.svg'
img_grob <- svgparser::read_svg(file)

grid::grid.newpage()
grid::grid.draw(img_grob)

ls <- grid::grid.ls()
print(head(ls))
names <- ls$name[grepl('pathgrob', ls$name)]
print(head(names))

name <- names[1]
ngrob <- grid.get(gPath(name))
print(names(ngrob))
print(names(ngrob$gp))
print(ngrob$gp$fill )
print(ngrob$gp)
View(ngrob)

file_contents

change_fill <- function(file_contents, new_fill = "#aaaaff") {
  str_replace_all(file_contents, "fill:#[0-f]{6};", sprintf("fill:%s;", new_fill))
}

file_test <- as.data.frame(paste(gsub("'","",readLines("app/www/head1.svg")), collapse = ""))

file_split <-file_test %>% str_split(">") %>% 
  as.data.frame(col.names="svg_file") %>% filter(svg_file !="")

file_split$svg_file <- paste0(file_split$svg_file, ">")

finding_row<-mapply(grepl, "hair",file_split)

file_split[finding_row,] <- change_fill(file_split[finding_row,])

file_final <- apply(file_split,2,paste, collapse="")

writeLines(file_final, "app/www/test2_head1.svg")
                                            
