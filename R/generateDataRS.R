
library(deSolve)


LightRS<-function(t,PP=16.0, Intensity=150.0) {
  period=24.0;
  val=0.5*tanh(1*(t %% period)) - 0.5*tanh(1*((t %% period) - PP))
  return(Intensity*val)
}





spDE = function(t, y, parms) {
  with(as.list(c(y, parms)), {
    gamma=0.024;
    G=33.75;
    alpha_0=0.05;
    delta=0.0075;
    p=1.5;
    I0=9325;

    #Make a function for the light schedule used
    Light <- function(t) {ShiftWorkLight(t, Light_Param)}

    Bhat=G*(1.0-n)*alpha_0*Light(t)^p/(Light(t)^p+I0);
    LightAmp=A1*0.5*Bhat*(1.0-R^4)*cos(Psi+BetaL)+A2*0.5*Bhat*R*(1.0-R^8.0)*cos(2.0*Psi+BetaL2);
    LightPhase=sigma*Bhat-A1*Bhat*0.5*(R^3.0+1.0/R)*sin(Psi+BetaL)-A2*Bhat*0.5*(1.0+R^8.0)*sin(2.0*Psi+BetaL2);


    dydt=c(0,0,0)
    dydt[1]=-1.0*gamma*R+K*cos(Beta1)/2.0*R*(1.0-R^4.0)+LightAmp;
    dydt[2]=2*pi/tau+K/2.0*sin(Beta1)*(1+R^4.0)+LightPhase;
    dydt[3]=60.0*((alpha_0*Light(t)^p/(Light(t)^p+I0))*(1.0-n)-delta*n);
    list(c(R=dydt[1], Psi=dydt[2], n=dydt[3]))
  })
}

generateDataRS<- function(IntensityIn, burn.transients=TRUE, num.days=40, error.sigma=0.05) {
  times.per.day<-1;
  times = seq(0, 24*num.days, length=times.per.day*num.days+1)
  start = c(R = 0.70, Psi = 0, n=0.0)
  #org_parms = c(tau=23.84293, K=0.06358, Beta1=-0.09, A1=0.3855, A2=0.1977, BetaL=-.0026, BetaL2=-0.957756, sigma=0.04) #Choose the parameters
  parms = c(tau=24.00, K=0.06358, Beta1=-0.09, A1=0.40, A2=0.1977, BetaL=0.10, BetaL2=-0.957756, sigma=0.04, Light_Param=IntensityIn) #Choose the parameters
  out = lsoda(start, times, spDE, parms) #Perform the integration
  if (burn.transients==TRUE) {
    start=out[num.days,2:4]
    start[2]<-start[2] %% (2*pi)
    out = lsoda(start, times, spDE, parms) #Perform the integration
  }
  #Add some noise
  numDataPoints=length(times)
  outNoise = out
  outNoise[-1,3] = outNoise[-1,3] + rnorm(numDataPoints-1, 0, sd=error.sigma)

  nSamples = length(times)-1
  t0 = times[1] #initial time
  PsiVals <- outNoise[-1,3] # measured data
  initVals<- outNoise[1, 2:4]

  dataMCMC<-getMCMCCorrMatrix()
  muPrior=c(23.84, 0.0400692, 0.3855, 0.1977,-0.0026, -0.957756)
  data = list (T  = nSamples, Psi  = PsiVals, t0 = t0, ts = times[-1], y0=initVals,
               sePrior=dataMCMC$se, corrPrior=dataMCMC$corr_mat, mu_prior=muPrior, Light_Param=IntensityIn)
  return(data)

}


getMCMCCorrMatrix<-function() {
  mcmc_run_params <- read.delim("~/work/Research/DATA_SCIENCE/mcmc_run_params.dat", header=FALSE)
  mcmc_run_params<-mcmc_run_params %>% select(V1:V7)
  colnames(mcmc_run_params)<-c('Tau','K','A1', 'A2','BetaL1', 'BetaL2', 'sigma')
  mcmc_run_params$K<-NULL;
  mcmc_run_params<-mcmc_run_params %>% select(Tau,sigma,A1:BetaL2)
  se.vector<- sqrt(diag(cov(mcmc_run_params[seq(1,32032,10),])))
  corr.matrix<-cor(mcmc_run_params[seq(1,32032,10),])
  return(list(se=se.vector, corr_mat=corr.matrix))


}



