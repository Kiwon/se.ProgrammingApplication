# Packages ----------------------------------------------------------------
library("plyr")

# 1 Loading data -----------------------------------------------------------
rm(list = ls()) # Remove all variable
dat <- read.csv("./stPre.kr.csv")

# 2 counting number of sation ----------------------------------------------
n.st <- nrow(dat)

# 3 counting number of station each by organization -----------------------
n.st.eachOrg <- count(dat, vars = "Control_og")

# 4 counting number of station in the range ------------------------------
n.st.range <-0
for(i in 1:n.st){
    if(128<=dat$Longitude[i] && dat$Longitude[i] <=129 && 
        36<=dat$Latitude[i] && dat$Latitude[i] <= 38){
        n.st.range <- n.st.range +1}
}

# 5 Function calculating distance between two points ----------------------
f.dis <- function(x1, y1, x2, y2){
    out <- sqrt( (x1-x2)^2 + (y1-y2)^2 )
    return(out)
}

# 6 Estimating distance between two stations -------------------------------
st.1 <- "Daegi"
st.2 <- "Deungmae"
st.1.index <- grep(st.1, dat$ST_NAME_E)
st.2.index <- grep(st.2, dat$ST_NAME_E)

x1 <- dat$X_EPSG5186[st.1.index]
y1 <- dat$Y_EPSG5186[st.1.index]
x2 <- dat$X_EPSG5186[st.2.index]
y2 <- dat$Y_EPSG5186[st.2.index]

val.dis <- f.dis(x1 = x1, y1 = y1, x2 = x2, y2 = y2)

n.st.eachOrg.vec <- n.st.eachOrg[,2]
names(n.st.eachOrg.vec)<- n.st.eachOrg[,1]

barplot(n.st.eachOrg.vec, xlab = "Organization", ylab = "n", col=rgb(1,0.5,0.3), main ="Number of stations by organization" )
