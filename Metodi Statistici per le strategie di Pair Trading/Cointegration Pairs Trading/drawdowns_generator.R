generate_drawdowns <- function(ls,kf,rl,name=NULL)
{
  ls_results <- table.Drawdowns(ls)
  
  kf_results <- table.Drawdowns(kf)
  
  rl_results <- table.Drawdowns(rl)
  
  
  write.csv(ls_results,file=paste0('/Users/carmineminichini/Desktop/Kalman Filter/output/',name,'_lsdrawdowns.csv'))
  write.csv(kf_results,file=paste0('/Users/carmineminichini/Desktop/Kalman Filter/output/',name,'_kfdrawdowns.csv'))
  write.csv(rl_results,file=paste0('/Users/carmineminichini/Desktop/Kalman Filter/output/',name,'_rldrawdowns.csv'))
  
  
  
}