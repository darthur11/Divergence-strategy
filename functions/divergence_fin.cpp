#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//
// [[Rcpp::export]]
List findPeaksCPP( NumericVector vY, double threshold = 0.001) {
  int sze = vY.size();
  int i = 0;//generic iterator
  int counter = 0;
  int last_ind = 0;
  int num_elements = 0;
  int last_ts;
  
  double last_element = 0;
  double thres = mean(na_omit(vY))*threshold;
  
  NumericVector ret(0);
  NumericVector ret_fin(0);
  NumericVector empty(0);
  
  NumericVector dv = vY.attr("index");
  NumericVector new_ind(0);
  
  List dimnms = vY.attr("dimnames");
  
  for(i = 0; i < (sze-2); ++i) {
    if(vY(i + 2) < (vY(i+1) - thres) && 
       vY(i) < (vY(i+1) - thres)) {
      ret.push_back( double(vY[i + 1]) );
      new_ind.push_back( dv[i + 1] );
      ++counter;
      last_ind = int(i+1);
    }
  }
  
  if(counter>2) {
    ret_fin = ret[Range(0,counter-2)];
    last_ts = int(new_ind[counter-1]);
    new_ind = new_ind[Range(0,counter-2)];
    num_elements = counter-1;
    last_element = double(ret[counter-1]);
  } else if(counter == 2) {
    num_elements = counter-1;
    last_element = double( ret[counter-1] );
    ret_fin = ret[0];
    last_ts = int(new_ind[counter-1]);
    new_ind = new_ind[0];
  } else {
    ret_fin = empty;
    new_ind = empty;
    num_elements = 0;
    last_element = 0;
    last_ind = 0;
  }
  
  ret_fin.attr("index")   = new_ind;
  new_ind.attr("tzone")   = "UTC";         // the index has attributes
  new_ind.attr("tclass")  = "Date";
  CharacterVector klass   = CharacterVector::create("xts", "zoo");
  ret_fin.attr("class")   = klass;
  ret_fin.attr("dim")     = IntegerVector::create(num_elements,1);
  ret_fin.attr("dimnames") = dimnms;
  List L = List::create(_["peaks"] = ret_fin, 
                        _["num_elements"] = num_elements,
                        _["last_element"] = last_element,
                        _["last_index"] = last_ind,
                        _["last_ts"] = last_ts);  
  return(L);
} //End Fn


// [[Rcpp::export]]
List findValleysCPP( NumericVector vY, double threshold = 0.001) {
  int sze = vY.size();
  int i = 0;//generic iterator
  int counter = 0;
  int last_ind = 0;
  int num_elements = 0;
  int last_ts;
  
  double last_element = 0;
  double thres = mean(na_omit(vY))*threshold;
  
  NumericVector ret(0);
  NumericVector ret_fin(0);
  NumericVector empty(0);
  
  NumericVector dv = vY.attr("index");
  NumericVector new_ind(0);
  
  List dimnms = vY.attr("dimnames");
  
  for(i = 0; i < (sze-2); ++i){
    if(vY(i + 2) > (vY(i+1) + thres) && 
       vY(i) > (vY(i+1) + thres)) {
      ret.push_back( double(vY[i + 1]) );
      new_ind.push_back( dv[i + 1] );
      ++counter;
      last_ind = int(i+1);
    }
  }
  if(counter>2) {
    ret_fin = ret[Range(0,counter-2)];
    last_ts = int(new_ind[counter-1]);
    new_ind = new_ind[Range(0,counter-2)];
    num_elements = counter-1;
    last_element = double(ret[counter-1]);
  } else if(counter == 2) {
    num_elements = counter-1;
    last_element = double( ret[counter-1] );
    ret_fin = ret[0];
    last_ts = int(new_ind[counter-1]);
    new_ind = new_ind[0];
  } else {
    ret_fin = empty;
    new_ind = empty;
    num_elements = 0;
    last_element = 0;
    last_ind = 0;
  }
  
  ret_fin.attr("index")   = new_ind;
  new_ind.attr("tzone")   = "UTC";         // the index has attributes
  new_ind.attr("tclass")  = "Date";
  CharacterVector klass   = CharacterVector::create("xts", "zoo");
  ret_fin.attr("class")   = klass;
  ret_fin.attr("dim")     = IntegerVector::create(num_elements,1);
  ret_fin.attr("dimnames") = dimnms;
  
  List L = List::create(_["valleys"] = ret_fin, 
                        _["num_elements"] = num_elements,
                        _["last_element"] = last_element,
                        _["last_index"] = last_ind,
                        _["last_ts"] = last_ts);  
  return(L);
}//End Fn

// [[Rcpp::export]]
double get_val(NumericVector xv, int d) {
  return(xv[d]);
} //End Fn


// [[Rcpp::export]]
NumericVector subset_xts(Rcpp::NumericMatrix symbol, String name_col){
  CharacterVector ch = colnames(symbol);
  NumericVector dv = symbol.attr("index");
  NumericVector xts(0);
  int pos = 0;
  int i;
  for(i; i<ch.size(); ++i){
    if(ch[i]==name_col){ 
      pos = i; 
      xts = symbol( _ , pos);
      xts.attr("index") = dv;
    }
  }
  return(xts);
}


// [[Rcpp::export]]
double diver_down(
    NumericVector peaks_price_v, 
    NumericVector peaks_rsi_v, 
    double end_peaks_price, 
    double end_peaks_rsi, 
    double end_peaks_trend,
    int peaks_len,
    int peaks_rsi_len,
    double prob_hi,
    double prob_mid,
    double prob_lo) {
  
  NumericVector dv_price = peaks_price_v.attr("index");
  NumericVector dv_rsi = peaks_rsi_v.attr("index");
  
  double pk_pr, pk_rsi, diver = 0.0;
  
  int i, j;
  
  for(i = 0; i < (peaks_len-1); ++i) {
    for(j = 0; j < (peaks_rsi_len-1); ++j) {
      if(abs(dv_price[i]-dv_rsi[j])<172800){ // less than 2 days between extremums
        pk_pr = peaks_price_v[i];
        pk_rsi = peaks_rsi_v[j];
        
        if      (end_peaks_trend<.99 && 
                 pk_pr>end_peaks_price &&
                 pk_rsi<end_peaks_rsi &&
                 end_peaks_rsi>30) { diver = (+prob_mid); } 
        else if (end_peaks_trend>1.01 &&
                 pk_pr<end_peaks_price &&
                 pk_rsi>end_peaks_rsi &&
                 end_peaks_rsi>50) { diver = (+prob_hi); } 
        else if (abs(pk_pr/end_peaks_price-1)<0.01 &&
                 pk_rsi>end_peaks_rsi) { diver = (+prob_lo); }
      }
    }
  }
  return(diver);
} //End Fn

// [[Rcpp::export]]
double diver_up(
    NumericVector valleys_price_v, 
    NumericVector valleys_rsi_v, 
    double end_valleys_price, 
    double end_valleys_rsi, 
    double end_valleys_trend,
    int valleys_len,
    int valleys_rsi_len,
    double prob_hi,
    double prob_mid,
    double prob_lo) {
  
  NumericVector dv_price = valleys_price_v.attr("index");
  NumericVector dv_rsi = valleys_rsi_v.attr("index");
  
  double vl_pr, vl_rsi, diver = 0.0;
  
  int i, j;
  
  for(i = 0; i < (valleys_len-1); ++i) {
    for(j = 0; j < (valleys_rsi_len-1); ++j) {
      if(abs(dv_price[i]-dv_rsi[j])<172800){ // less than 2 days between extremums
        vl_pr = valleys_price_v[i];
        vl_rsi = valleys_rsi_v[j];
        
        if      (end_valleys_trend<.99 && 
                 vl_pr>end_valleys_price &&
                 vl_rsi<end_valleys_rsi &&
                 end_valleys_rsi<50) { diver = (+prob_hi); } 
        else if (end_valleys_trend>1.01 &&
                 vl_pr<end_valleys_price &&
                 vl_rsi>end_valleys_rsi &&
                 end_valleys_rsi<70) { diver = (+prob_mid); } 
        else if (abs(vl_pr/end_valleys_price-1)<0.01 &&
                 vl_rsi<end_valleys_rsi) { diver = (+prob_lo); }
      }
    }
  }
  return(diver);
} //End Fn


// [[Rcpp::export]]
double diver_fin(Rcpp::NumericMatrix symbol, double threshold = 0.001){
  Function findPeaksCPP("findPeaksCPP");
  Function findValleysCPP("findValleysCPP");
  Function subset_xts("subset_xts");
  Function diver_up("diver_up");
  Function diver_down("diver_down");
  Function get_val("get_val");
  
  NumericVector prices_xts = subset_xts(symbol, "close");
  NumericVector rsi_xts = subset_xts(symbol, "rsi");
  NumericVector trend_xts = subset_xts(symbol, "roll_ret");
  
  List peaks_price = findPeaksCPP(prices_xts, threshold);
  List valleys_price = findValleysCPP(prices_xts, threshold);
  List peaks_rsi = findPeaksCPP(rsi_xts, threshold);
  List valleys_rsi = findValleysCPP(rsi_xts, threshold);
  
  double diver_down_val = 0; 
  double diver_up_val = 0;
  double diver = 0;
  
  int peaks_len = peaks_price["num_elements"];
  int valleys_len = valleys_price["num_elements"];

  NumericVector peaks_price_v = peaks_price["peaks"];
  NumericVector valleys_price_v = valleys_price["valleys"];
  if(peaks_len>0) { 
    NumericVector peaks_rsi_v = peaks_rsi["peaks"]; 
    int peaks_rsi_len = peaks_rsi["num_elements"]; 
    double end_peaks_trend = as<double>(get_val(trend_xts,peaks_price["last_index"])); 
    double end_peaks_price = peaks_price["last_element"]; 
    double end_peaks_rsi = peaks_rsi["last_element"]; 
    double diver_down_val = as<double>(diver_down(peaks_price_v, peaks_rsi_v, end_peaks_price,
                                end_peaks_rsi, end_peaks_trend, peaks_len, 
                                peaks_rsi_len, -0.5, -0.225, -0.06)); 
    diver = diver + diver_down_val; 
  }
  
  if(valleys_len>0){
    NumericVector valleys_rsi_v = valleys_rsi["valleys"];
    int valleys_rsi_len = valleys_rsi["num_elements"];
    double end_valleys_trend = as<double>(get_val(trend_xts,valleys_price["last_index"]));
    double end_valleys_price = valleys_price["last_element"];
    double end_valleys_rsi = valleys_rsi["last_element"];
    diver_up_val = as<double>(diver_up(valleys_price_v, valleys_rsi_v, 
                                              end_valleys_price, end_valleys_rsi, 
                                              end_valleys_trend, valleys_len, valleys_rsi_len,
                                              0.5, 0.225, 0.06));
    diver = diver + diver_up_val;
  }
  
  return(diver);
}
