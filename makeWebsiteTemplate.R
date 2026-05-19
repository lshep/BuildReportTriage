library(stringr)
file_path <- "NewDeprecatedPackages"

lines <- readLines(file_path)
refstart <- '<li><a href="/packages/3.21/bioc/html/'

sink("forwebsite.txt")
for (line in lines) {

    pkg <- str_trim(line)
    if (pkg == "Software:"){
        refstart <- '<li><a href="/packages/3.21/bioc/html/'
        next
    }
    if (pkg == "ExperimentData:"){
        refstart <- '<li><a href="/packages/3.21/data/experiment/html/'
        next
    }
    if (pkg == "Workflows:"){
        refstart <- '<li><a href="/packages/3.21/workflows/html/'
        next
    }
    if (pkg == ""){
        cat("\n")
        next
    }
    link <- paste0(refstart, pkg, '.html">', pkg, '</a></li>')
    cat(link, "\n")    
}
sink()
