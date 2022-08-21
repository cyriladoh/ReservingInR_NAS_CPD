#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins("cpp11")]]


// [[Rcpp::export]]
NumericVector GetVals(Date startdate, Date enddate, Date valuationdate){
  
  int coverperiod = enddate - startdate + 1;
  
  double startyear_unexpiredperiod = 0;
  double startyear_expiredperiod = 0;
  
  double endyear_unexpiredperiod = 0;
  double endyear_expiredperiod = 0;
  
  Date startdate_endofyear = Date(startdate.getYear(),12,31);
  
  Date enddate_startofyear = Date(enddate.getYear(),01,01);
  
  if((startdate <= valuationdate) && (enddate >= valuationdate)){
    
    if(valuationdate >= startdate_endofyear){
      
      startyear_expiredperiod = (startdate_endofyear - startdate + 1)/coverperiod;
      startyear_unexpiredperiod = 0;
      
      if(startdate.getYear() == enddate.getYear()){
        endyear_expiredperiod = 0;
        endyear_unexpiredperiod = 0;
      }
      else{
        endyear_expiredperiod =  (valuationdate - enddate_startofyear+1)/coverperiod;
        endyear_unexpiredperiod = (enddate - valuationdate)/coverperiod;
      }
      
    }
    else{
      
      startyear_expiredperiod = (valuationdate - startdate+1)/coverperiod;
      
      if(startdate.getYear() == enddate.getYear()){
        startyear_unexpiredperiod = (enddate - valuationdate)/coverperiod;
        endyear_unexpiredperiod = 0;
      }
      else{
        startyear_unexpiredperiod = (startdate_endofyear - valuationdate)/coverperiod;
        endyear_unexpiredperiod = (enddate - enddate_startofyear + 1)/coverperiod;
      }
      
      endyear_expiredperiod =  0;
      
    }
  }
  else{
    
    if(valuationdate > enddate){
      
      startyear_unexpiredperiod = 0;
      endyear_unexpiredperiod = 0;
      
      if(startdate.getYear() == enddate.getYear()){
        startyear_expiredperiod = (enddate - startdate + 1)/coverperiod;
        endyear_expiredperiod = 0;
      }
      else{
        startyear_expiredperiod = (startdate_endofyear - startdate + 1)/coverperiod;
        endyear_expiredperiod =  (enddate - enddate_startofyear+1)/coverperiod;
      }
      
    }
    else{
      
      startyear_expiredperiod = 0;
      endyear_expiredperiod = 0;
      
      if(startdate.getYear() == enddate.getYear()){
        startyear_unexpiredperiod = (enddate - startdate + 1)/coverperiod;
        endyear_unexpiredperiod = 0;
      }
      else{
        startyear_unexpiredperiod = (startdate_endofyear - startdate + 1)/coverperiod;
        endyear_unexpiredperiod = (enddate - enddate_startofyear+1)/coverperiod;
      }
      
    }
    
    
  }
  
  NumericVector ans = {startyear_expiredperiod,startyear_unexpiredperiod,endyear_expiredperiod,endyear_unexpiredperiod};
    
    return(ans);
    
}





 // [[Rcpp::export]]
 DataFrame prod_vals(DataFrame proddata, Date val_date){

   int n = proddata.nrows();

   NumericVector premium = as<NumericVector>(proddata["premium"]);
   NumericVector commission = as<NumericVector>(proddata["commission"]);

   DateVector startdates = as<DateVector>(proddata["startdate"]);
   DateVector enddates = as<DateVector>(proddata["enddate"]);
   
   NumericVector vals;
   NumericVector a (n);
   NumericVector b (n);
   NumericVector c (n);
   NumericVector d (n);
   NumericVector e (n);
   NumericVector g (n);
   NumericVector h (n);
   NumericVector j (n);
   //DataFrame result = DataFrame::create(Named("a")=v);
   for(int i = 0; i<n; i++){
     
     double premiums = premium[i];
     double commissions = commission[i];
     Function f("GetVals");
     vals = f(_["startdate"] = startdates[i], _["enddate"] = enddates[i] , _["valuationdate"] = val_date);
     double start_uexp = vals[1];
     double start_upr = start_uexp * premiums;
     b[i] = start_upr;
     double start_ep = vals[0] * premiums;
     a[i] = start_ep;
     double start_ec = vals[0] * commissions;
     h[i] = start_ec;
     double start_uc = start_uexp * commissions;
     c[i] = start_uc;

     double end_uexp = vals[3];
     double end_upr = end_uexp * premiums;
     e[i] = end_upr;
     double end_ep =  vals[2] * premiums;
     d[i] = end_ep;
     double end_ec =  vals[2] * commissions;
     j[i] = end_ec;
     double end_uc = end_uexp * commissions;
     g[i] = end_uc;
      
     //NumericVector v1 = {start_ep,start_upr,start_uc,end_ep,end_upr,end_uc};
     //result.push_back(v1);

   }
   DataFrame result = DataFrame::create(Named("start_ep")=a,Named("start_upr")=b,Named("start_uc")=c,Named("end_ep")=d,Named("end_upr")=e,Named("end_uc")=g, Named("end_ec")=h,Named("start_ec")=j);
   return result;
 }
