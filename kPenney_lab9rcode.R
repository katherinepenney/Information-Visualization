####
#
# Author: Katherine Penney
# Week 9 Lab
#   
####

my.dir<- "C:/Users/Katherine Penney/Desktop/ist719networkobject.rda"

g

library(igraph)
install.packages('rgl')
library(rgl)

coords<- layout_with_kk(g, dim= 3)
rglplot(g,layout =coords)

l<- layout_with_kk(g)
V(g)$x<- l[,1]
V(g)$y<- l[,2]
V(g)$z<- V(g)$bet
rglplot(g)

V(g)$label<- ""
V(g)$x<- coords[,1]
V(g)$y<- coords[,2]
V(g)$z<- coords[,3]

rglplot(g)
par3d(windowRect = c(100,100,640,640))
rgl.bringtotop()
rgl.bg(color = "black")
rgl.viewpoint(0,20)

E(g)$color="yellow"
E(g)$width<- .5
V(g)$label<- ""

##########################

out.dir<- "C:/Users/Katherine Penney/Documents/Data Visualization/"

library(stringr)
install.packages('animation')
library(animation)
library(plotrix)
library(igraph)
library(rgl)

g

my.rgl.out<- paste0(out.dir, "Network3Dvisualization.png")

rglplot(g)
rgl.snapshot(my.rgl.out)

rglplot(g)
par3d(windowRect = c(100,100,500,500))
rgl.bringtotop()
rgl.bg(color = "black")
rgl.viewpoint(0,0,zoom=.7)

max.loops<- 360 *2
my.angle<- rescale(1:max.loops, c(-90,90))
for(i in 1:max.loops){
  
  rgl.viewpoint(theta = -my.angle[i], phi = my.angle[i] * .7, zoom = .75 - i/(max.loops*1.7))
  #theta is the angle spining around y, phi is spinning x
  
}
max.loops<- 360 *2
my.angle<- rescale(1:max.loops, c(-90,90))
for(i in 1:max.loops){
  
  rgl.viewpoint(theta = -my.angle[i], phi = my.angle[i] * .7, zoom = .75 - i/(max.loops*1.7))
  #theta is the angle spining around y, phi is spinning x
  
}


install.packages("stringr")
library(stringr)

images.out<- paste0(out.dir, "out\\")

# ffmpeg

rglplot(g)
par3d(windowRect = c(100,100,500,500))
rgl.bringtotop()
rgl.bg(color = "black")
rgl.viewpoint(0,0,zoom=.7)

max.loops<-40
my.angle<- rescale(1:max.loops, c(-180,180))

for(i in 1:max.loops){
  # i=1+i
  rgl.viewpoint(theta = -my.angle[i], phi = 0, zoom = .75 - i/(max.loops*1.7))
  snapshot.fname<- paste0(images.out, "network", str_pad(i, width=4, side ="left", pad="0"),".png")
  rgl.snapshot(snapshot.fname)
  
}

ani.options(interval=.1)
imgs<- list.files(images.out, pattern = "*.png")

saveGIF({
  for(img in imgs){
    im<- magick::image_read(paste0(images.out, img))
    print(im)
  }
}, movie.name= paste0(my.rgl.out, "classNetwork.gif"))

#######################################################

ani.options(ffmpeg ="C:/Users/Katherine Penney/Documents/Data Visualization/")

saveVideo({
  for(img in imgs){
    im<- magick::image_read(paste0(images.out, img))
    print(im)
  }
}, video.name= paste0(my.rgl.out, "classNetwork.mp4"), other.opts = "-vf format=yuv420p")

#######################################################

library(rgl)
install.packages("scatterplot3d")
library(scatterplot3d)
n<- 1000
x<- rnorm(n)
y<- ((2 * x)^2)/10+ rnorm(n, mean = 0, sd= .2)
z<- sqrt(abs(x))+ rnorm(n, mean=0, sd=.2)
scatterplot3d(x, y, z, pch=16, type="h")
plot3d(x, y, z, col="red", size=3)
plot3d(x,y,z, col="gold", size =1 ,type="s" )
rgl.bg(color="black")
rgl.viewpoint(0,0,zoom = .7)
rgl.clear(type = "shapes")
spheres3d(4*x, 4*y, 4*z, radius=.1, col="gold")
rgl.light(theta=0, phi=0, viewpoint.rel = TRUE, ambient = "#FFFFFF", diffuse = "#FFFFFF", specular="#FFFFFF")
rgl.clear(type="lights")
light3d(diffuse="gray75", specular="gray75", viewpoint.rel=FALSE)
rgl.light(ambient="#444444", diffuse = "#0000FF", specular = "#FF0000")

##########################3
library(rgl)
open3d()
par3d()$windowRect




lines3d(x=c(-2,2), y=c(0,0), z=c(0,0), col="red", lwd=1)
text3d(2.2,0,0, "x", color="red", cex=1, adj=0)

lines3d(x=c(0,0), y=c(-2,2), z=c(0,0), col="green", lwd=1)
text3d(0,2.2,0, "x", color="green", cex=1, adj=0)

lines3d(x=c(0,0), y=c(0,0), z=c(-2,2), col="blue", lwd=1)
text3d(0,0,2.2, "x", color="blue", cex=1, adj=0)
wire3d((cube3d()))
wire3d(scale3d(cube3d(), 2,2,2),col="red")
rgl.ids()
rgl.pop(id=5946)
rgl.clear(type="shapes")
shapelist3d(tetrahedron3d(), 2,0,0,color="tan", size=.5)
material3d(alpha=.2, shininess=75, emision="blue")
shapelist3d(cube3d(), 0,2,0,color="cadetblue", size=.2)
material3d(alpha=1, shininess=5, emision="black")
shapelist3d(tetrahedron3d(), 0,0,2.5,color="red", size=.9)
material3d(alpha=1, shininess=50, emision="gray75")
shapelist3d(icosahedron3d(), 2,0,2.5,color="orange", size=2)

##########1 
library(rgl)

web.dir<- "C:/Users/Katherine Penney/Documents/Data Visualization/"

open3d()
material3d(alpha=1, shiness= 50, emission="black")
rgl.bg(color="black")

n<- 10
x<- 0
y<- 0
z.start<- 0
z.end<- 10
M<- matrix(c(rep(x,n), rep(y, n), seq(from=z.start, to=z.end
                                      , length.out=n)),nrow=n, byrow=FALSE)
M2<- cylinder3d(center=M, radius=.2, sides=50, closed=-2)
shade3d(M2, alpha=1, color="gray45")

z.end<- .25
M<- matrix(c(rep(x,n), rep(y, n), seq(from=z.start, to=z.end
                                      , length.out=n)),nrow=n, byrow=FALSE)
M2<- cylinder3d(center=M, radius=1.5, sides=50, closed=-2)
shade3d(M2, alpha=1, color="gray45")

material3d(alpha=1, shiness= 50, emission="tan")
spheres3d(x,y, 10.5, radius= .25, col="yellow")
shapelist3d(cuboctahedron3d(), x, y, 10.5, size=.575, color="gold")
material3d(alpha=1, shiness= 50, emission="black")

M<- matrix(c(rep(x,n), rep(y, n), seq(from=11, to=11.5
                                      , length.out=n)),nrow=n, byrow=FALSE)
m4<- cylinder3d(center=M, radius = .75, sides=50, closed =-2)
shade3d(m4, alpha=1, color="gray45")
ns<- 100
xs<- rnorm(ns,0,3)
ys<- rnorm(ns,0,3)
zs<- rnorm(ns,9,2)
rs<- rnorm(ns,1,.3)
material3d(alpha=1, shiness=100, emission="black")
particles3d(xs, ys, zs, radius = rs, color="white")
writeWebGL(dir=web.dir, filename= index.html)
filename<-writeWebGL(dir=web.dir, width=500, reuse = TRUE)

############################################
writeOBJ()
readOBJ()
readSTL()
writeSTL()
library(igraph)
tmp<- installed.packages()
ip<- as.data.frame(tmp, stringAsFactors=FALSE)
View(ip)
table(ip$LibPath)

.libPaths()
ip$Package[grep(pattern = "^gg", ip$Package)]

tmp<- sessionInfo()
memory.size()
str(ip)


