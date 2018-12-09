#include "utils.h"
#include "annoylib.h"

#ifndef USE_AVX

double squared_euclidean_dist(const double* x, const double* y, MatDim_t d) {
    double out=0;
    if (d > 3) {
        // Manual loop unrolling to get the same behaviour as AVX.
        double v1=0, v2=0, v3=0, v4=0;
        for (; d > 3; d-=4) {
            double tmp1=x[0]-y[0],
                tmp2=x[1]-y[1],
                tmp3=x[2]-y[2],
                tmp4=x[3]-y[3];
            v1 += tmp1 * tmp1;
            v2 += tmp2 * tmp2;
            v3 += tmp3 * tmp3;
            v4 += tmp4 * tmp4;
            x+=4;
            y+=4;
        }

        out = v1 + v2 + v3 + v4;
    }

    for (; d > 0; --d, ++x, ++y) {
        const double tmp=*x - *y;
        out+=tmp*tmp;
    }
    return out;
}

#else 

double squared_euclidean_dist(const double* x, const double* y, MatDim_t f) {
    double result=0;
    if (f > 3) {
        __m256 d = _mm256_setzero_ps();
        for (; f > 3; f -= 4) {
            const __m256 diff = _mm256_sub_pd(_mm256_loadu_pd(x), _mm256_loadu_pd(y));
            d = _mm256_add_pd(d, _mm256_mul_pd(diff, diff)); // no support for fmadd in AVX...
            x += 4;
            y += 4;
        }

        // Sum all doubles in dot register.
        double loc[4];
        _mm256_storeu_pd(loc, d);
        result = loc[0] + loc[1] + loc[2] + loc[3];
    }
       
    // Don't forget the remaining values.
    for (; f > 0; --f) {
        double tmp = *x - *y;
        result += tmp * tmp;
        ++x;
        ++y;
    }

    return result;
}

#endif
