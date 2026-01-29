#include <Rcpp.h>
#include <cmath>
#include <algorithm>
#include <string>
#include <cstdio>

using namespace Rcpp;

inline double get_quantile_from_index(const NumericVector& all_y, int start_idx, int count, double p) {
  if (count == 0) return NA_REAL;

  double m = 1.0 - p;
  double j = std::floor(count * p + m);
  double g = count * p + m - j;
  int local_idx = (int)j - 1;

  if (local_idx < 0) return all_y[start_idx];
  if (local_idx >= count - 1) return all_y[start_idx + count - 1];

  double v1 = all_y[start_idx + local_idx];
  double v2 = all_y[start_idx + local_idx + 1];

  return (1.0 - g) * v1 + g * v2;
}

//' @export
 // [[Rcpp::export]]
 List compute_ribbon_stats(NumericVector x,
                           NumericVector y,
                           IntegerVector group,
                           NumericVector widths) {

   int n = x.size();
   if (n == 0) return List::create();

   // Prepare widths
   std::vector<double> w_std(widths.begin(), widths.end());
   std::sort(w_std.begin(), w_std.end(), std::greater<double>());
   int n_widths = w_std.size();

   // Output vectors
   std::vector<double> out_x;
   std::vector<int> out_group;
   std::vector<double> out_ymin;
   std::vector<double> out_ymax;
   std::vector<double> out_y;
   std::vector<double> out_width;
   std::vector<std::string> out_level;

   int start_idx = 0;

   // assume that data is sorted by group -> x -> y
   for (int i = 0; i < n; ++i) {
     if (i % 10000 == 0) Rcpp::checkUserInterrupt();

     bool end_of_segment = (i == n - 1) || (group[i] != group[i+1]) || (x[i] != x[i+1]);

     if (end_of_segment) {
       int count = i - start_idx + 1;

       double median = get_quantile_from_index(y, start_idx, count, 0.5);
       double current_x = x[i];
       int current_grp = group[i];

       for (int w = 0; w < n_widths; ++w) {
         double width = w_std[w];
         double lower_prob = (1.0 - width) / 2.0;
         double upper_prob = 1.0 - lower_prob;

         out_x.push_back(current_x);
         out_group.push_back(current_grp);
         out_y.push_back(median);
         out_width.push_back(width);

         char buffer[16];
         std::snprintf(buffer, sizeof(buffer), "%.2f", width);
         out_level.push_back(std::string(buffer));

         out_ymin.push_back(get_quantile_from_index(y, start_idx, count, lower_prob));
         out_ymax.push_back(get_quantile_from_index(y, start_idx, count, upper_prob));
       }

       start_idx = i + 1;
     }
   }

   return List::create(
     Named("x") = out_x,
     Named("group") = out_group,
     Named("y") = out_y,
     Named("ymin") = out_ymin,
     Named("ymax") = out_ymax,
     Named(".width") = out_width,
     Named("level") = out_level
   );
 }
