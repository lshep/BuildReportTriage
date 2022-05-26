.libPaths("/home/shepherd/R-Libraries/4.2-Bioc3.16")


both <- read.table(file = "Workflow_BothReleases.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
rel <- read.table(file = "WorkflowPkgSummary_3_15.csv", sep=",", header=TRUE,
                  stringsAsFactors=FALSE)
dev <- read.table(file = "WorkflowPkgSummary_3_16.csv", sep=",", header=TRUE,
                  stringsAsFactors=FALSE)


pkgsBoth <- both$pkgs[which(both[,dim(both)[2]]==1)]
pkgsRel <- rel$Package[which(rel$acrossAllToday)]
pkgsDev <- dev$Package[which(dev$acrossAllToday)]



# remove deprecated
#releaseDep = c()
#develDep = c()
#dep <- c(releaseDep, develDep)

# actually keep deprecated
#RelOnly <-  setdiff(setdiff(pkgsRel, pkgsBoth), dep)
#DevOnly <- setdiff(setdiff(pkgsDev, pkgsBoth), dep)

RelOnly <-  setdiff(pkgsRel, pkgsBoth)
DevOnly <- setdiff(pkgsDev, pkgsBoth)


if(length(RelOnly) > 0){
  colDF <- data.frame(RelOnly, date=1, stringsAsFactors = FALSE)
  colnames(colDF)[2] = format(Sys.time(), "%b%d")
  ## first run only
  # write.table(colDF, file = "WorkRelOnly.csv", quote=FALSE, sep=",",row.names=FALSE, na="")
 
  r = read.table(file = "WorkRelOnly.csv", sep=",", header=TRUE,
    stringsAsFactors=FALSE)
  idx = match(setdiff(as.character(colDF$RelOnly), as.character(r$pkgs)), as.character(colDF$RelOnly))
  if(length(idx)!= 0){
      subAdd = colDF[idx,]
    
      placeholder = dim(r)[2]-7
      for(i in 1:dim(subAdd)[1]){
          r = rbind(r, c(subAdd[i,1], rep("", 6), rep(0, placeholder)))
      }
  }
  tempVec = colDF[match(as.character(r$pkgs),  as.character(colDF$RelOnly)),2]
  tempVec[is.na(tempVec)] = 0
  r = cbind(r, newCol = tempVec)
  colnames(r)[dim(r)[2]] = format(Sys.time(), "%b%d")
  write.table(r, file = "WorkRelOnly.csv", quote=FALSE, sep=",",
              row.names=FALSE, na="")

}

if(length(DevOnly) > 0){
  colDF <- data.frame(DevOnly, date=1, stringsAsFactors = FALSE)
  colnames(colDF)[2] = format(Sys.time(), "%b%d")
  ## first run only
  # write.table(colDF, file = "WorkDevOnly.csv", quote=FALSE, sep=",",row.names=FALSE, na="")

  r = read.table(file = "WorkDevOnly.csv", sep=",", header=TRUE,
    stringsAsFactors=FALSE)
  idx = match(setdiff(as.character(colDF$DevOnly), as.character(r$pkgs)), as.character(colDF$DevOnly))
  if(length(idx)!= 0){
      subAdd = colDF[idx,]
    
      placeholder = dim(r)[2]-7
      for(i in 1:dim(subAdd)[1]){
          r = rbind(r, c(subAdd[i,1], rep("", 6), rep(0, placeholder)))
      }
  }
  tempVec = colDF[match(as.character(r$pkgs),  as.character(colDF$DevOnly)),2]
  tempVec[is.na(tempVec)] = 0
  r = cbind(r, newCol = tempVec)
  colnames(r)[dim(r)[2]] = format(Sys.time(), "%b%d")
  write.table(r, file = "WorkDevOnly.csv", quote=FALSE, sep=",",
            row.names=FALSE, na="")
}
