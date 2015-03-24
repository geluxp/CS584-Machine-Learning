## Clear all the variables in the memory
rm(list=ls())

##Read the original data from svar_set1.dat to do "single variable regression 
oData1=read.table("D:/R_workspace/Kun_Mei_ass1/data/svar-set1.dat")
oData2=read.table("D:/R_workspace/Kun_Mei_ass1/data/svar-set2.dat")
oData3=read.table("D:/R_workspace/Kun_Mei_ass1/data/svar-set3.dat")
oData4=read.table("D:/R_workspace/Kun_Mei_ass1/data/svar-set4.dat")

##Attach the original data from svar_set1.dat to R
attach(oData1)

#Plot the data v1 and v2
plot(V1,V2)


model<-lm(V2~V1)

abline(model)

title('Singlevariate Regreesion of V2 on V1 from svar_set1.dat')
e=predict(model,data.frame(V1))-V2


m1<-lm(V2~V1)
m2<-lm(V2~V1-1)  # no bias




e1=predict(m1,data.frame())



##Save the regression graph
pdf("svar_set1_graph.pdf")
jpeg("svar_set1_graph.jpeg")
detach(OData1)

attach(oData2)

detach(oData2)

attach(oData3)

detach(oData3)

attach(oData4)

detach(oData4)