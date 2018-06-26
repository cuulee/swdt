
#' Kittler-Illingworth minimum error thresholding
#' see http://dx.doi.org/10.1016/0031-3203(86)90030-0
#' @param x the data to be thresholded
#' @param breaks the number of histogram breaks to estimate the distribution from
kit=function(x,breaks=length(x)){
  x=na.omit(as.numeric(x))
  if(length(x[!is.na(x)])==0){
    return(NA)
  }else{
    hist=hist.default(x, breaks=breaks, plot=F)
    h=as.double(hist$counts)
    g=as.double(hist$mids)
    
    c=cumsum(h)
    m=cumsum(h*g)
    s=cumsum(h*g^2)
    
    sigma_f=sqrt(s/c-(m/c)^2)
    
    cb=c[length(c)]-c
    mb=m[length(m)]-m
    sb=s[length(s)]-s
    
    sigma_b=suppressWarnings(sqrt(sb/cb-(mb/cb)^2))
    
    p=c/c[length(c)]
    v = p * log(sigma_f) + (1-p)*log(sigma_b) - p*log(p) - (1-p)*log(1-p)
    v[!is.finite(v)]=Inf
    
    idx=which.min(v)
    return(g[idx])
  }
}

#' automatic SAR image histogram thresholding for water masking using adaptive tiling
#' the image is divided into tiles, from which those with the highest standard 
#' deviation and mean lower than that of the whole image are selected.
#' On each of the tiles a threshold is computed for splitting the assumed bimodal histogram.
#' see http://dx.doi.org/10.1016/j.isprsjprs.2014.07.014
#' @param img the image matrix
#' @param splitTileSize the size of the image tiles in pixels
#' @param fun the function to be used for finding the threshold
thres.gray=function(img,splitTileSize=50,fun=kit){
  require(modeest)
  rowstrat=stratify(nrow(img),splitTileSize)
  colstrat=stratify(ncol(img),splitTileSize)
  
  sdev=matrix(NA,nrow=length(rowstrat),ncol=length(colstrat))
  means=matrix(NA,nrow=length(rowstrat),ncol=length(colstrat))
  
  for(ri in seq(rowstrat)){
    for(ci in seq(colstrat)){
      sub=img[rowstrat[[ri]],colstrat[[ci]]]
      if((length(sub[!is.na(sub)])/prod(dim(sub)))>0.5){
        sdev[ri,ci]=sd(sub,na.rm=T)
        means[ri,ci]=mean(sub,na.rm=T)
      }
    }
  }
  sdev_p95=quantile(sdev,probs=c(0.95),na.rm=T)
  sdev[sdev<sdev_p95]=NA
  sdev[means>mean(img,na.rm=T)]=NA
  
  thresholds=c()
  collection=c()
  
  for(ri in seq(rowstrat)){
    for(ci in seq(colstrat)){
      if(!is.na(sdev[ri,ci])){
        sub=na.omit(as.vector(img[rowstrat[[ri]],colstrat[[ci]]]))
        thres=fun(sub,length(sub))
        thresholds=c(thresholds,thres)
        collection=c(collection,sub)
      }
    }
  }
  return(modeest::asselin(thresholds))
}
