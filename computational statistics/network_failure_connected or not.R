select_node<-function(x){
  list1<-list(
    A = c('C',"D",'E','F'),
    C = c('A','D','G','H'),
    D = c('A',"C",'E','H'),
    E = c('A','D','F','I'),
    F = c('A','E','I','J'),
    G = c('C','H','B'),
    H = c('C','D','G','I','B'),
    I = c('E','F','H','J','B'),
    J = c('F','I','B'),
    B = c('G','H','I','J')
    )
	
  mytree<-list()
	A_edge = c(x[1],x[2],x[3],x[4]); mytree[['A']] = list1[['A']][A_edge==1]
	C_edge = c(x[1],x[5],x[8],x[9]); mytree[['C']] = list1[['C']][C_edge==1]
	D_edge = c(x[2],x[5],x[6],x[10]); mytree[['D']] = list1[['D']][D_edge==1]
	E_edge = c(x[3],x[6],x[7],x[11]); mytree[['E']] = list1[['E']][E_edge==1]
	F_edge = c(x[4],x[7],x[12],x[13]); mytree[['F']] = list1[['F']][F_edge==1]
	G_edge = c(x[8],x[14],x[17]); mytree[['G']] = list1[['G']][G_edge==1]
	H_edge = c(x[9],x[10],x[14],x[15],x[18]); mytree[['H']] = list1[['H']][H_edge==1]
	I_edge = c(x[11],x[12],x[15],x[16],x[19]); mytree[['I']] = list1[['I']][I_edge==1]
	J_edge = c(x[13],x[16],x[20]); mytree[['J']] = list1[['J']][J_edge==1]
	B_edge = c(x[17],x[18],x[19],x[20]); mytree[['B']] = list1[['B']][B_edge==1]

	return(mytree)

}

select_node(rep(1,20))

###plot
myplot<-function(x){
  plot(c(0,1,1,1,1,2,2,2,2,3),c(0,1.5,0.5,-0.5,-1.5,1.5,0.5,-0.5,-1.5,0),axes=FALSE,xlab="",ylab="",pch=16,
       ylim = c(-2,2))
  text(0,-0.2,"A");text(3,-0.2,"B")
  text(1,1.7,"C");text(1,0.7,"D");text(1,-0.7,"E");text(1,-1.7,"F")
  text(2,1.7,"G");text(2,0.7,"H");text(2,-0.7,"I");text(2,-1.7,"J")
  
  if(x[1]==1){lines(c(0,1),c(0,1.5))}
  if(x[2]==1){lines(c(0,1),c(0,0.5))}
  if(x[3]==1){lines(c(0,1),c(0,-0.5))}
  if(x[4]==1){lines(c(0,1),c(0,-1.5))}
  if(x[5]==1){lines(c(1,1),c(1.5,0.5))}
  if(x[6]==1){lines(c(1,1),c(0.5,-0.5))}
  if(x[7]==1){lines(c(1,1),c(-0.5,-1.5))}
  if(x[8]==1){lines(c(1,2),c(1.5,1.5))}
  if(x[9]==1){lines(c(1,2),c(1.5,0.5))}
  if(x[10]==1){lines(c(1,2),c(0.5,0.5))}
  if(x[11]==1){lines(c(1,2),c(-0.5,-0.5))}
  if(x[12]==1){lines(c(1,2),c(-1.5,-0.5))}
  if(x[13]==1){lines(c(1,2),c(-1.5,-1.5))}
  if(x[14]==1){lines(c(2,2),c(1.5,0.5))}
  if(x[15]==1){lines(c(2,2),c(0.5,-0.5))}
  if(x[16]==1){lines(c(2,2),c(-0.5,-1.5))}
  if(x[17]==1){lines(c(2,3),c(1.5,0))}
  if(x[18]==1){lines(c(2,3),c(0.5,0))}
  if(x[19]==1){lines(c(2,3),c(-0.5,0))}
  if(x[20]==1){lines(c(2,3),c(-1.5,0))}
}
myplot(rep(1,20))

BFS_path <- function(graph, start, end, visited = c()){
  
  visited <- c(visited, start)
  if(start == end) paths<<-c(paths, visited)
  
  queue <- start
  
  while (length(queue) > 0){
    #pop
    visiting <- queue[1]; queue<-queue[-1]
    
    for (node in graph[[visiting]]){
      if( !(node %in% visited) ) BFS_path(graph, node, end, visited)
    }
  }
}

DFS_path <- function(graph, start, end, visited = c() ){
  
  visited <- c(visited, start)
  if(start == end) paths<<-c(paths, visited)
  
  for(node in graph[[start]]){
    if(!(node %in% visited)) DFS_path(graph, node, end, visited)
  }
  }

connected_or_not <- function(x, method){
  paths<<-c()
  
  #select edge and 
  g1<-select_node(x)
  
  #select finding methods BFS or DFS
  if(method == "BFS") BFS_path(g1,'A','B')
  else DFS_path(g1,'A','B')

  
  n<-length(paths); where_A<-which(paths=="A");n_A <-length(where_A)
  
  if(sum(paths=='B')==sum(paths=='A') & n!=0){
    for(i in 1:n_A){
      if(i==n_A) print(paths[ where_A[i] : n])
      else print(paths[where_A[i] : where_A[i+1]-1])
    }
    
    # return(1)
  }
  else return(0)
  }


#ex1
ex1<-c(1,1,0,0,0,0,1,0,1,0,1,1,1,0,1,0,1,0,0,1)
myplot(ex1)
system.time(for(i in 1:50) connected_or_not(ex1,'BFS'))
system.time(for(i in 1:50) connected_or_not(ex1,'DFS'))

#ex2
ex2<-c(1,0,1,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,1,0)
myplot(ex2)
system.time(for(i in 1:50) connected_or_not(ex2,'BFS'))
system.time(for(i in 1:50) connected_or_not(ex2,'DFS'))

#ex3
ex3<-c(1,1,1,1,1,1,1,1,1,1,0,1,0,1,0,1,0,0,0,1)
myplot(ex3)
system.time(for(i in 1:50) connected_or_not(ex3,'BFS'))
system.time(for(i in 1:50) connected_or_not(ex3,'DFS'))


#disconnected
dis1<-c(1,1,1,1,1,1,1,1,1,1,0,1,0,1,0,0,0,0,0,1)
myplot(dis1)
system.time(for(i in 1:1000) connected_or_not(dis1,'BFS'))
system.time(for(i in 1:1000) connected_or_not(dis1,'DFS'))
