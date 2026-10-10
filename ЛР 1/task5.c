#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>

#define MAX_TERMS 100000000LL
#define SUMS_COUNT 4
#define MAX_SEGMENTS (1LL << 26)
#define INTEGRALS_COUNT 4

typedef enum {
    STATUS_OK,
    STATUS_INVALID_ARGUMENT,
    STATUS_INVALID_NUMBER,
    STATUS_DIVERGES,
    STATUS_OVERFLOW,
    STATUS_PRECISION_NOT_REACHED
} Status;

typedef Status (*SumFunction)(const double x, const double eps, double *result);

static Status parse_double(const char *str, double *result) {
    if (str == NULL || result == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    if (str[0] == '\0' || isspace((unsigned char)str[0])) {
        return STATUS_INVALID_NUMBER;
    }
    char *end = NULL;
    double value = strtod(str, &end);
    if (*end != '\0' || !isfinite(value)) {
        return STATUS_INVALID_NUMBER;
    }
    if (value == 0.0) {
        for (size_t i = 0; str[i] != '\0' && str[i] != 'e' && str[i] != 'E'; i++) {
            if (str[i] >= '1' && str[i] <= '9') {
                return STATUS_INVALID_NUMBER;
            }
        }
    }
    *result = value;
    return STATUS_OK;
}

static Status sum_a(const double x, const double eps, double *result) {
    if (result == NULL || eps <= 0.0) {
        return STATUS_INVALID_ARGUMENT;
    }
    double term = 1.0;
    double sum = term;
    for (long long n = 0; n < MAX_TERMS; n++) {
        term *= x / (double)(n + 1);
        sum += term;
        if (!isfinite(sum)) {
            return STATUS_OVERFLOW;
        }
        if (fabs(term) < eps) {
            *result = sum;
            return STATUS_OK;
        }
    }
    *result = sum;
    return STATUS_PRECISION_NOT_REACHED;
}

static Status sum_b(const double x, const double eps, double *result) {
    if (result == NULL || eps <= 0.0) {
        return STATUS_INVALID_ARGUMENT;
    }
    double term = 1.0;
    double sum = term;
    for (long long n = 0; n < MAX_TERMS; n++) {
        term *= -x * x / ((2.0 * n + 1.0) * (2.0 * n + 2.0));
        sum += term;
        if (!isfinite(sum)) {
            return STATUS_OVERFLOW;
        }
        if (fabs(term) < eps) {
            *result = sum;
            return STATUS_OK;
        }
    }
    *result = sum;
    return STATUS_PRECISION_NOT_REACHED;
}

static Status sum_c(const double x, const double eps, double *result) {
    if (result == NULL || eps <= 0.0) {
        return STATUS_INVALID_ARGUMENT;
    }
    if (fabs(x) >= 1.0) {
        return STATUS_DIVERGES;
    }
    double term = 1.0;
    double sum = term;
    for (long long n = 0; n < MAX_TERMS; n++) {
        double k = (double)n;
        term *= 9.0 * (k + 1.0) * (k + 1.0) * x * x / ((3.0 * k + 1.0) * (3.0 * k + 2.0));
        sum += term;
        if (!isfinite(sum)) {
            return STATUS_OVERFLOW;
        }
        if (fabs(term) < eps) {
            *result = sum;
            return STATUS_OK;
        }
    }
    *result = sum;
    return STATUS_PRECISION_NOT_REACHED;
}

static Status sum_d(const double x, const double eps, double *result) {
    if (result == NULL || eps <= 0.0) {
        return STATUS_INVALID_ARGUMENT;
    }
    if (fabs(x) >= 1.0) {
        return STATUS_DIVERGES;
    }
    double term = 0.0 - x * x / 2.0;
    double sum = term;
    if (fabs(term) < eps) {
        *result = sum;
        return STATUS_OK;
    }
    for (long long n = 1; n < MAX_TERMS; n++) {
        term *= -(2.0 * n + 1.0) * x * x / (2.0 * n + 2.0);
        sum += term;
        if (fabs(term) < eps) {
            *result = sum;
            return STATUS_OK;
        }
    }
    *result = sum;
    return STATUS_PRECISION_NOT_REACHED;
}

static double integrand_a(const double x) {
    return log(1.0 + x) / x;
}

static double integrand_b(const double x) {
    return exp(-x * x / 2.0);
}

static double integrand_c(const double x) {
    return log(1.0 / (1.0 - x));
}

static double integrand_d(const double x) {
    return pow(x, x);
}

static Status midpoint_rule(double (*f)(double), const long long n, double *result) {
    if (f == NULL || result == NULL || n <= 0) {
        return STATUS_INVALID_ARGUMENT;
    }
    double h = 1.0 / (double)n;
    double sum = 0.0;
    for (long long i = 0; i < n; i++) {
        sum += f(((double)i + 0.5) * h);
    }
    *result = sum * h;
    return STATUS_OK;
}

static Status integrate(double (*f)(double), const double eps, double *result) {
    if (f == NULL || result == NULL || eps <= 0.0) {
        return STATUS_INVALID_ARGUMENT;
    }
    long long n = 1;
    double previous = 0.0;
    Status status = midpoint_rule(f, n, &previous);
    if (status != STATUS_OK) {
        return status;
    }

    while (n < MAX_SEGMENTS) {
        n *= 2;
        double current = 0.0;
        status = midpoint_rule(f, n, &current);
        if (status != STATUS_OK) {
            return status;
        }
        if (fabs(current - previous) < eps) {
            *result = current;
            return STATUS_OK;
        }
        previous = current;
    }
    *result = previous;
    return STATUS_PRECISION_NOT_REACHED;
}

static int digits_for_eps(const double eps) {
    int digits = (int)ceil(-log10(eps)) + 1;
    if (digits < 1) {
        digits = 1;
    }
    if (digits > 15) {
        digits = 15;
    }
    return digits;
}

static void print_sums(const double x, const double eps, const double values[], const Status statuses[]) {
    const char *names[SUMS_COUNT] = {"a) sum x^n/n!", "b) sum (-1)^n x^2n/(2n)!",
                                     "c) sum 3^3n (n!)^3 x^2n/(3n)!", "d) sum (-1)^n (2n-1)!! x^2n/(2n)!!"};
    int digits = digits_for_eps(eps);
    printf("5.1 Sums, x = %g\n", x);
    for (int i = 0; i < SUMS_COUNT; i++) {
        printf("%s = ", names[i]);
        switch (statuses[i]) {
            case STATUS_OK: printf("%.*f\n", digits, values[i]); break;
            case STATUS_PRECISION_NOT_REACHED: printf("%.*f (precision not reached)\n", digits, values[i]); break;
            case STATUS_DIVERGES: printf("series diverges for this x (|x| must be < 1)\n"); break;
            case STATUS_OVERFLOW: printf("overflow, x is too large\n"); break;
            default: printf("error\n"); break;
        }
    }
    printf("check: e^x = %.*f, cos x = %.*f, 1/sqrt(1+x^2) - 1 = %.*f\n",
           digits, exp(x), digits, cos(x), digits, 1.0 / sqrt(1.0 + x * x) - 1.0);
}

static void print_integrals(const double eps, const double values[], const Status statuses[]) {
    const char *names[INTEGRALS_COUNT] = {"a) ln(1+x)/x", "b) e^(-x^2/2)", "c) ln(1/(1-x))", "d) x^x"};
    const double pi = 4.0 * atan(1.0);
    const double exact[INTEGRALS_COUNT] = {pi * pi / 12.0, sqrt(pi / 2.0) * erf(1.0 / sqrt(2.0)), 1.0,
                                           0.78343051071213440705};
    int digits = digits_for_eps(eps);
    printf("5.2 Integrals over [0, 1]\n");
    for (int i = 0; i < INTEGRALS_COUNT; i++) {
        printf("%-16s = %.*f   (exact %.*f)%s\n", names[i], digits, values[i], digits, exact[i],
               statuses[i] == STATUS_OK ? "" : "  precision not reached");
    }
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        printf("Error: wrong number of arguments\n");
        return 1;
    }

    double eps = 0.0;
    double x = 0.0;
    if (parse_double(argv[1], &eps) != STATUS_OK || parse_double(argv[2], &x) != STATUS_OK) {
        printf("Error: eps and x must be real numbers\n");
        return 1;
    }
    if (eps <= 0.0 || eps >= 1.0) {
        printf("Error: eps must satisfy 0 < eps < 1\n");
        return 1;
    }

    const SumFunction sums[SUMS_COUNT] = {sum_a, sum_b, sum_c, sum_d};
    double (*const integrands[INTEGRALS_COUNT])(double) = {integrand_a, integrand_b, integrand_c, integrand_d};

    double sum_values[SUMS_COUNT];
    Status sum_statuses[SUMS_COUNT];
    for (int i = 0; i < SUMS_COUNT; i++) {
        sum_values[i] = 0.0;
        sum_statuses[i] = sums[i](x, eps, &sum_values[i]);
    }
    double integral_values[INTEGRALS_COUNT];
    Status integral_statuses[INTEGRALS_COUNT];
    for (int i = 0; i < INTEGRALS_COUNT; i++) {
        integral_values[i] = 0.0;
        integral_statuses[i] = integrate(integrands[i], eps, &integral_values[i]);
    }

    printf("eps = %g\n", eps);
    print_sums(x, eps, sum_values, sum_statuses);
    print_integrals(eps, integral_values, integral_statuses);
    return 0;
}
