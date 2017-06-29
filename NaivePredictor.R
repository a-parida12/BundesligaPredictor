poisson<-function(mu){
  
  p=sapply(0:10,function(i) dpois(i, mu) )
  return(p)
}

HomeTeamGoalAverage<-function(HomeAttack,GuestDefence,overallHomeAverage){
  
  return(HomeAttack*GuestDefence*overallHomeAverage)
}

GuestTeamGoalAverage<-function(GuestAttack,HomeDefence,overallGuestAverage){
  
  return(HomeDefence*GuestAttack*overallGuestAverage)
}
GuessScore<-function(GoalHomeTeam,GoalGuestTeam){
  details<-c(which.max(GoalHomeTeam)-1,which.max(GoalGuestTeam)-1,max(GoalHomeTeam)*max(GoalGuestTeam))
  return(details)
}
ProbabilityDraw<-function(GoalHomeTeam,GoalGuestTeam){
  sum(GoalHomeTeam*GoalGuestTeam)
  return
}
ProbabilityWin<-function(GoalHomeTeam,GoalGuestTeam){
  sum=0
  for (i in 2:10) {
    for (j in 1:i-1){
      sum=c(sum,GoalHomeTeam[i]*GoalGuestTeam[j])
    }
  }
  
  return(sum(sum))
  }


muHome=HomeTeamGoalAverage(1.235, 0.881, 1.492 )
muGuest=GuestTeamGoalAverage(1.046,0.653,1.207)
GoalHomeTeam=poisson(muHome)
GoalGuestTeam=poisson(muGuest)
MostProbableScorline=GuessScore(GoalHomeTeam,GoalGuestTeam)# return array with goals Home,Guest,probability%
Draw=ProbabilityDraw(GoalHomeTeam,GoalGuestTeam)
HomeWin=ProbablityWin(GoalHomeTeam,GoalGuestTeam)
HomeLoss=1-(HomeWin+Draw)