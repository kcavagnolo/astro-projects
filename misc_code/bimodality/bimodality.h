typedef struct {
  double *x;
  int N;
  double y;
} bin;

typedef struct {
  double x;
  double y;
} data;

int random_index(int N);

bin *allocate_bin(int N);
bin *reallocate_bin(bin *B, int N);
void free_bin(bin *B);
bin *resample_bin(bin *B);


data *read_data_guts(char *filename, int *N);
bin **read_data_null(char *filename, int *k);
bin **read_data_fixed_number(char *filename, int N,
			     int *k);
bin **read_data_fixed_width(char *filename, double yi, double yf, double dy,
			    int *k);
bin **read_data_running_bin(char *filename, int M, int *k);

double gaussian(double x, double m, double s);
double kmm_unimodal(bin *B, double *m, double *s);
void kmm_calculate_P(bin *B, double *P1, double *P2,
		     double pi1, double m1, double s1,
		     double pi2, double m2, double s2);
double kmm_calculate_M(bin *B, double *P1, double *P2,
		       double *pi1, double *m1, double *s1,
		       double *pi2, double *m2, double *s2);

double mean(double *x, int N);
double stdev(double *x, int N);
int max_index(double *x, int N);
double p_unimodal(double logL_unimodal, double logL_bimodal,int df);

/* #define DEBUG */
/* #define ABORT */




			   
			    
