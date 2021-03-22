
show_board <- function(board)
{ 
  dim=as.integer(board['Rows'])
  col=as.integer(board['Columns'])
  dim=dim+1
  col=col+1    
  
  if(col!=dim)
  {
    
    xlim <- c(1,dim)
    ylim <- c(1,dim-2)
    plot.new()
    plot.window( xlim , ylim )
  }
  else
  {
    xlim <- c(1,dim)
    ylim <- c(1,col)
    plot.new()
    plot.window( xlim , ylim )
  }
  
  if(col==dim)
    
  {
    
    
    for (i in 1:dim)
    {
      segments(1,i,dim,i)  
    }
    for (i in 1:col)
    {
      segments(i,1,i,col)  
    }
    
    for (i in 1:(dim-1))
    {
      k=0
      for(j in 1:(col-1))
      {
        if(i==1)
          text(j+0.5,i+0.5,j)
        else if(i%%2!=0)
          text(j+0.5,i+0.5,((i-1)*10)+j)
        else if(i %%2==0)
          text(j+0.5,i+0.5,(i*10)-k)
        k=k+1
        
        
        
      }
    }
  }
  else
  {
    for (i in 1:(dim))
    {
      segments(1,i,dim-3,i)  
    }
    for (i in 1:(col))
    {
      segments(i,1,i,col+1)  
    }
    
    m=1
    k=0
    for (i in 1:(dim-1))
    {
      
      temp=FALSE
      for(j in 1:(col-1))
      {
        if(i==1)
        {
          text(j+0.5,i+0.5,m)
          m=m+1
        }
        
        else if(i%%2!=0)
        {
          text(j+0.5,i+0.5,m)
          m=m+1    
          
          
        }
        else if(i %%2==0)
        {
          if(temp==FALSE)
          {
            temp=TRUE
            k=(m-1)+(col-1)
            m=k+1
          }
          
          text(j+0.5,i+0.5,k)
          k=k-1
        }
        
        
        
      }
    }
    
  }
  for (i in 1:length(board$Start))
  {
    x=board$Start[i]
    y=board$End[i]
    
    
    y1=(as.integer((x)/10))+(0.5)
    y2=(as.integer((y)/10))+(0.5)
    if(x%%10!=0)
      y1=y1+1
    if(y%%10!=0)
      y2=y2+1
    
    if((as.integer(x/10) %% 2!=0))
    {
      a=col-1-(x%%10)
      a=a+1+0.5
      x1=a
      
      
    }
    
    
    else
    {
      x1=x%%10 + (0.5)
      
      
    }
    
    if((as.integer(y/10) %% 2!=0))
    {
      a=col-1-(y%%10)
      a=a+1+0.5
      x2=a 
    }
    else
    {
      
      
      x2=y%%10 + (0.5)
    }
    if(col==dim)
    {
      
      
      
      
      if(x%%10==0  && as.integer(x/10)%%2!=0)
      {
        x1=10.5
      }
      else if(x%%10==0)
        x1=1.5
      
      
      if(y%%10==0  && as.integer(y/10)%%2!=0 )
      {
        x2=10.5
      }
      else if(y%%10==0)
        x2=1.5
      if(x<y)
        arrows(x1,y1,x2,y2,col='green')
      else
        arrows(x1,y1,x2,y2,col='red')
      
    }
    
    
  }
  
  
  if(col!=dim)
  {
    
    a=c(1.5,1.5,4.5,2.5,5.5,4.5,4.5,6.5,4.5,7.5,2.5,6.5,6.5,5.5,4.5,3.5)
    b=c(2.5,4.5,3.5,5.5,6.5,6.5,5.5,7.5,2.5,5.5,1.5,4.5,6.5,3.5,6.5,1.5)
    
    
    
    k=1
    
    for(i in 1:length(board$Start))
    {
      
      if(i==1)
      {
        index=1
      }
      else
      {
        index=i+k
        k=k+1
      }
      
      x=board$Start[i]
      y=board$End[i]
      
      
      if(x<y)
      {
        arrows(a[index],a[index+1],b[index],b[index+1],col="green")
        
        
        
      }
      
      else if(x>y)
      {
        
        arrows(a[index],a[index+1],b[index],b[index+1],col="red")
        
        
      }                
      
      
      
    }
    
  }
  
}


play_solo <-function(board,verbose)
  
{
  middle_index=0
  x=board$Start
  y=board$End
  
  for (i in 1:length(x))
  {
    if(x[i]>y[i])
    {
      middle_index=i
      break
    }
    
  }
  
  turn_end=0
  turns=0
  ladder_tally=matrix(0,nrow=9)
  chute_tally=matrix(0,nrow=10)
  move_log=c()
  position=0
  is_ladder=FALSE
  is_chute=FALSE
  land_on=0
  start=0
  while(turn_end!=100)
  {
    
    is_ladder=FALSE
    is_chute=FALSE
    dice=sample(6,1)
    position=position+dice
    if(position>100)
    {
      position=position-dice
      next
    }
    for (i in 1:length(x))
    {
      
      if(position==x[i])
      {
        
        position=y[i]
        if(x[i]<y[i])
        {
          
          
          ladder_tally[i]=ladder_tally[i]+1
          is_ladder=TRUE
        }
        else if(x[i]>y[i])
        {
          index=i-middle_index
          chute_tally[index]=chute_tally[index]+1
          is_chute=TRUE
          
        }
        
      }
      
    }
    
    turns=turns+1
    turn_end=position
    
    if(verbose==TRUE)
    {
      cat("\n \n Turn ",turns)
      cat("\n Start at",start)
      cat("\n Spinner",dice)
      if(is_ladder==TRUE)
      {
        cat("\n  Landed On",position)
        cat(" \n Ladder !")
      }
      else if(is_chute==TRUE)
      {
        cat("\n Landed On",position)
        cat(" \n Chute ! ")
      }
      
      cat("\n Turn Ends at :",turn_end)
    }
    
    move_log=append(move_log,turn_end)
    start=turn_end
    
    
    
  }
  
  
  cat("\n Turns :",turns)
  cat("\n Chutte Tally : ",chute_tally)
  cat("\n Ladder Tally :",ladder_tally)
  cat("\n Move_log",move_log)
  
  if(verbose==FALSE)
    return(c(turns,ladder_tally,chute_tally))
  
}





