# Generating batch graphs from fastStructure analysis result
# By Shenglin Liu, Aug 18, 2014


#------extra files needed------#

# file "order.txt": population id (specifies the order to be displayed on the graph)
#		one column of integers (extra columns will be ignored)
# file "popname.txt": population name
#		one column of integers and one column of characters

#----------parameters----------#

nameRoot<-"strOutputNameRoot"
Kmin<-2
Kmax<-15
output<-"plotName.pdf"
type<-"pdf"

f.order<-"order.txt"
f.popmap<-"popname.txt"

#------------------------------#

n.row<-ceiling((Kmax-Kmin+1)/2)
n.col<-2
dev.new(height=n.row+1,width=n.col*4+1)
par(mfcol=c(n.row,n.col),mai=c(0.2,0.25,0,0.25),omi=c(0.4,0.5,0.4,0))

order<-as.integer(read.table(f.order,header=F)[[1]])
popname<-read.table(f.popmap,header=F)

#------------------------------#

ids<-sort(unique(order))
b<-integer(0)
c<-integer(1)
for(i in ids)
{
	b<-c(b,which(order==i))
	c<-c(c,length(b))
}

labels<-c(NA,as.character(popname[,2][order(popname[,1])]))

kArrange<-function(q,order)
{
	ids<-unique(order)
	n.k<-ncol(q)
	n.pop<-length(ids)
	q.pop<-matrix(0,n.pop,n.k)
	for(i in 1:n.pop)
	{
		index<-which(order==ids[i])
		q.pop[i,]<-unlist(sapply(1:n.k,function(x){mean(q[index,x])}))
	}
	index.max<-sapply(1:n.pop,function(x){which.max(q.pop[x,])})
	index.max<-unique(unlist(index.max))
	if((n.k-length(index.max))==1)
	{
		index.left<-c(1:n.k)[-index.max]
		index.max<-c(index.max,index.left)
	}
	if((n.k-length(index.max))>1)
	{
		index.left<-c(1:n.k)[-index.max]
		index.left<-index.left[order(colSums(q.pop[,index.left]),decreasing=T)]
		index.max<-c(index.max,index.left)
	}
	q[,index.max]
}

color.dist<-function(weight,threshold=0.01,unit=0.01,color.scheme="rainbow")
{
	color.schemes<-c(rainbow,heat.colors,terrain.colors,topo.colors,cm.colors)
	names(color.schemes)<-c("rainbow","heat.colors","terrain.colors","topo.colors","cm.colors")
	color<-color.schemes[[color.scheme]]
	weight<-weight/sum(weight)
	index<-weight>=threshold
	weight[index]<-1
	weight[!index]<-0
	weight<-weight/sum(weight)
	weight<-weight%/%unit
	weight<-weight[-length(weight)]
	color(1%/%unit)[c(1,cumsum(weight)+1)]
}

for(i in Kmin:Kmax)
{
	a<-read.table(paste(nameRoot,i,"meanQ",sep="."),header=F)
	a<-kArrange(a,order)
	a<-a[b,]
	barplot(t(a),col=color.dist(colSums(a),color.scheme=1),space=0,border=NA,axisnames=F)
	axis(1,at=c,labels=F)
	legend("topleft",sprintf("K=%d",i),bty="n",text.font=3,adj=c(0,0))
	if((i==(n.row+Kmin-1))||(i==Kmax))
	{
		text(c,-0.2,labels=labels,xpd=NA,srt=45,adj=1)
	}
}

savePlot(output,type)
dev.off()
