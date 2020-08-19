//
///////////////////////////////////////////////////////////////////////////////////////////
// Multifaceted Model for evaluations of Professors at classroom
// By 
// A. MOntenegro, Universidad Nacional dea Colombia
// H. Rosas, UNiversidad de Valparaíso
// C.      , Universidad Nacional dea Colombia
// J.      , Universidad Nacional dea Colombia
//
// Date: March, 2018
//
// Source. Data from a survey applied to students of Facultad de Ciencias
// to evaluate the perfomance of the profesors in classroom
// Date 2015-2
//
// Model Description
// Model 2: 
//  Professors,items,students, collegiate course, Subject, Carrer
///////////////////////////////////////////////////////////////////////////////////////////


data{
  int<lower=1>  N;           // number of data responses
  int<lower = 10> N_stud;    // number of students
  int<lower=2>  N_item;      // number of items
  int<lower=2,upper=5> N_cat;// number of categories by item
  int<lower = 3> N_sub;      // number of subject by item
  int<lower=1,upper=N_cat> y[N];        // responses; y[n], n-th response
  int<lower=1,upper=N_item> item[N];    // item of response [n]
  int<lower=1,upper=N_stud> student[N]; // student of  response [n]
  int<lower=1,upper=N_sub> subject[N];  //  subject of  response  [n]
 }

parameters{
  vector[N_sub] theta;           // latent traits of subjects
  vector[N_stud] gamma;          // students'severity
  ordered[N_cat-1] beta[N_item]; // coefficients of predictors (cut p)
  real mu_beta;                  // mean of the beta-parameters: difficulty of the item
  real<lower=0> sigma_beta;      //sd of the prior distributions of category difficulty
  real<lower=0> sigma_gamma;     //sd of the prior distributions of category difficulty
}
//end parameters


model{
  theta ~ normal(0,1);          // scale of the latent traits
  gamma ~ normal(0,sigma_gamma);//scale of the students'severity
  for(j in 1:N_item){
      beta[j]~ normal(mu_beta,sigma_beta);// prior of the item parameters
  }
  mu_beta ~ normal(0,2);    // hyper prior for mu_beta
  sigma_beta ~ cauchy(0,2); // hyper prior for sigma_beta
  sigma_gamma ~ cauchy(0,2);// hyper prior for sigma_gamma
  //likelihood
  for(n in 1:N){
   y[n] ~ ordered_logistic(theta[subject[n]]-gamma[student[n]],beta[item[n]]);
  }
}//end model

generated quantities {
vector[N_item] mu_item_beta; // the mean of betas by item
for (n in 1: N_item){
  mu_item_beta[n] = mean(beta[n]);
 }
}//end generated quantities
