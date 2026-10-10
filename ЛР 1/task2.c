#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>

#define MAX_SERIES_TERMS 3000000000LL
#define MAX_DOUBLINGS 40
#define MAX_LN2_DOUBLINGS 24
#define MAX_PI_PRODUCT_N (1LL << 24)
#define MAX_ITERATIONS 1000
#define MAX_BISECTION_STEPS 200
#define MAX_GAMMA_LIMIT_M 32
#define MAX_GAMMA_BLOCKS (1LL << 14)
#define MAX_PRIME_POWER 24
#define MAX_PRIME (1LL << MAX_PRIME_POWER)
#define METHODS_COUNT 15

#define EULER_GAMMA 0.57721566490153286

typedef enum {
    STATUS_OK,
    STATUS_INVALID_ARGUMENT,
    STATUS_INVALID_NUMBER,
    STATUS_NO_MEMORY,
    STATUS_PRECISION_NOT_REACHED
} Status;

typedef Status (*Method)(const double eps, double *result);

typedef struct {
    const char *constant;
    const char *method;
    Method compute;
} MethodInfo;

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

static int same_sign(const double a, const double b) {
    return (a > 0.0 && b > 0.0) || (a < 0.0 && b < 0.0);
}

static Status bisection(double (*f)(double), double left, double right, const double eps, double *root) {
    if (f == NULL || root == NULL || eps <= 0.0) {
        return STATUS_INVALID_ARGUMENT;
    }

    double f_left = f(left);
    if (same_sign(f_left, f(right))) {
        return STATUS_INVALID_ARGUMENT;
    }

    for (int step = 0; step < MAX_BISECTION_STEPS; step++) {
        if (right - left < eps) {
            *root = (left + right) / 2.0;
            return STATUS_OK;
        }
        double middle = left + (right - left) / 2.0;
        double f_middle = f(middle);
        if (same_sign(f_left, f_middle)) {
            left = middle;
            f_left = f_middle;
        } else {
            right = middle;
        }
    }

    *root = (left + right) / 2.0;
    return STATUS_PRECISION_NOT_REACHED;
}

static Status e_limit(const double eps, double *result) {
    if (result == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    double n = 1.0;
    double previous = pow(1.0 + 1.0 / n, n);
    for (int i = 0; i < MAX_DOUBLINGS; i++) {
        n *= 2.0;
        double current = pow(1.0 + 1.0 / n, n);
        if (fabs(current - previous) < eps) {
            *result = current;
            return STATUS_OK;
        }
        previous = current;
    }
    *result = previous;
    return STATUS_PRECISION_NOT_REACHED;
}

static Status e_series(const double eps, double *result) {
    if (result == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    double term = 1.0;
    double sum = 1.0;
    for (long long n = 1; n <= MAX_ITERATIONS; n++) {
        term /= (double)n;
        sum += term;
        if (term < eps) {
            *result = sum;
            return STATUS_OK;
        }
    }
    *result = sum;
    return STATUS_PRECISION_NOT_REACHED;
}

static double e_equation_function(const double x) {
    return log(x) - 1.0;
}

static Status e_equation(const double eps, double *result) {
    return bisection(e_equation_function, 2.0, 3.0, eps, result);
}

static Status pi_limit(const double eps, double *result) {
    if (result == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    double value = 4.0;
    double checkpoint = value;
    long long next_checkpoint = 2;
    for (long long n = 1; n < MAX_PI_PRODUCT_N; n++) {
        double k = 2.0 * (double)n + 1.0;
        value *= 1.0 - 1.0 / (k * k);
        if (n + 1 == next_checkpoint) {
            if (fabs(value - checkpoint) < eps) {
                *result = value;
                return STATUS_OK;
            }
            checkpoint = value;
            next_checkpoint *= 2;
        }
    }
    *result = value;
    return STATUS_PRECISION_NOT_REACHED;
}

static Status pi_series(const double eps, double *result) {
    if (result == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    double sum = 0.0;
    for (long long n = 1; n <= MAX_SERIES_TERMS; n++) {
        double term = 4.0 / (2.0 * (double)n - 1.0);
        if (term < eps) {
            *result = sum;
            return STATUS_OK;
        }
        sum += (n % 2 == 1) ? term : -term;
    }
    *result = sum;
    return STATUS_PRECISION_NOT_REACHED;
}

static double pi_equation_function(const double x) {
    return -sin(x);
}

static Status pi_equation(const double eps, double *result) {
    return bisection(pi_equation_function, 2.0, 4.0, eps, result);
}

static Status ln2_limit(const double eps, double *result) {
    if (result == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    double n = 1.0;
    double previous = n * (pow(2.0, 1.0 / n) - 1.0);
    for (int i = 0; i < MAX_LN2_DOUBLINGS; i++) {
        n *= 2.0;
        double current = n * (pow(2.0, 1.0 / n) - 1.0);
        if (fabs(current - previous) < eps) {
            *result = current;
            return STATUS_OK;
        }
        previous = current;
    }
    *result = previous;
    return STATUS_PRECISION_NOT_REACHED;
}

static Status ln2_series(const double eps, double *result) {
    if (result == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    double sum = 0.0;
    for (long long n = 1; n <= MAX_SERIES_TERMS; n++) {
        double term = 1.0 / (double)n;
        if (term < eps) {
            *result = sum;
            return STATUS_OK;
        }
        sum += (n % 2 == 1) ? term : -term;
    }
    *result = sum;
    return STATUS_PRECISION_NOT_REACHED;
}

static double ln2_equation_function(const double x) {
    return exp(x) - 2.0;
}

static Status ln2_equation(const double eps, double *result) {
    return bisection(ln2_equation_function, 0.0, 1.0, eps, result);
}

static Status sqrt2_limit(const double eps, double *result) {
    if (result == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    double x = -0.5;
    for (int i = 0; i < MAX_ITERATIONS; i++) {
        double next = x - x * x / 2.0 + 1.0;
        if (fabs(next - x) < eps) {
            *result = next;
            return STATUS_OK;
        }
        x = next;
    }
    *result = x;
    return STATUS_PRECISION_NOT_REACHED;
}

static Status sqrt2_product(const double eps, double *result) {
    if (result == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    double product = 1.0;
    for (int k = 2; k < MAX_ITERATIONS; k++) {
        double factor = pow(2.0, pow(2.0, -k));
        double next = product * factor;
        if (fabs(next - product) < eps) {
            *result = next;
            return STATUS_OK;
        }
        product = next;
    }
    *result = product;
    return STATUS_PRECISION_NOT_REACHED;
}

static double sqrt2_equation_function(const double x) {
    return x * x - 2.0;
}

static Status sqrt2_equation(const double eps, double *result) {
    return bisection(sqrt2_equation_function, 1.0, 2.0, eps, result);
}

static double gamma_limit_member(const int m) {
    double sum = 0.0;
    double binomial = 1.0;
    double ln_factorial = 0.0;
    for (int k = 1; k <= m; k++) {
        binomial = binomial * (double)(m - k + 1) / (double)k;
        ln_factorial += log((double)k);
        double term = binomial * ln_factorial / (double)k;
        sum += (k % 2 == 1) ? -term : term;
    }
    return sum;
}

static Status gamma_limit(const double eps, double *result) {
    if (result == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    int m = 1;
    double previous = gamma_limit_member(m);
    while (m < MAX_GAMMA_LIMIT_M) {
        m *= 2;
        double current = gamma_limit_member(m);
        if (fabs(current - previous) < eps) {
            *result = current;
            return STATUS_OK;
        }
        previous = current;
    }
    *result = previous;
    return STATUS_PRECISION_NOT_REACHED;
}

static Status gamma_series(const double eps, double *result) {
    if (result == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    const double pi = 4.0 * atan(1.0);
    double sum = 0.0;
    double estimate = 0.0;
    double checkpoint = 0.0;
    int has_checkpoint = 0;
    long long next_checkpoint = 2;

    for (long long m = 1; m < MAX_GAMMA_BLOCKS; m++) {
        double block_value = 1.0 / (double)(m * m);
        for (long long k = m * m; k < (m + 1) * (m + 1); k++) {
            if (k >= 2) {
                sum += block_value - 1.0 / (double)k;
            }
        }
        double big_m = (double)(m + 1);
        estimate = -pi * pi / 6.0 + sum + 2.0 / big_m + 1.0 / (6.0 * big_m * big_m);

        if (m + 1 == next_checkpoint) {
            if (has_checkpoint && fabs(estimate - checkpoint) < eps) {
                *result = estimate;
                return STATUS_OK;
            }
            checkpoint = estimate;
            has_checkpoint = 1;
            next_checkpoint *= 2;
        }
    }
    *result = estimate;
    return STATUS_PRECISION_NOT_REACHED;
}

static Status gamma_equation(const double eps, double *result) {
    if (result == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    char *is_composite = calloc((size_t)MAX_PRIME + 1, 1);
    if (is_composite == NULL) {
        return STATUS_NO_MEMORY;
    }

    double x_values[MAX_PRIME_POWER + 1];
    Status status = STATUS_PRECISION_NOT_REACHED;
    double product = 1.0;
    int k = 0;
    long long next_checkpoint = 2;

    for (long long t = 2; t <= MAX_PRIME; t++) {
        if (!is_composite[t]) {
            for (long long multiple = t * t; multiple <= MAX_PRIME; multiple += t) {
                is_composite[multiple] = 1;
            }
            product *= 1.0 - 1.0 / (double)t;
        }
        if (t == next_checkpoint) {
            k++;
            double limit_value = log((double)t) * product;
            x_values[k] = -log(limit_value);
            if (k >= 5 && fabs(x_values[k] - x_values[k - 4]) < eps) {
                status = STATUS_OK;
                break;
            }
            next_checkpoint *= 2;
        }
    }

    free(is_composite);
    *result = x_values[k];
    return status;
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

static double reference_value(const char *constant) {
    switch (constant[0]) {
        case 'e': return exp(1.0);
        case 'p': return 4.0 * atan(1.0);
        case 'l': return log(2.0);
        case 's': return sqrt(2.0);
        default: return EULER_GAMMA;
    }
}

static void print_results(const MethodInfo methods[], const double values[], const Status statuses[],
                          const int count, const double eps) {
    int digits = digits_for_eps(eps);
    printf("eps = %g\n", eps);
    for (int i = 0; i < count; i++) {
        if (i == 0 || strcmp(methods[i].constant, methods[i - 1].constant) != 0) {
            printf("\n%s (reference value %.15f)\n", methods[i].constant, reference_value(methods[i].constant));
        }
        printf("  %-9s %.*f", methods[i].method, digits, values[i]);
        switch (statuses[i]) {
            case STATUS_OK: printf("\n"); break;
            case STATUS_PRECISION_NOT_REACHED: printf("   (precision not reached, best value shown)\n"); break;
            case STATUS_NO_MEMORY: printf("   (error: not enough memory)\n"); break;
            default: printf("   (error)\n"); break;
        }
    }
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Error: wrong number of arguments\n");
        return 1;
    }

    double eps = 0.0;
    if (parse_double(argv[1], &eps) != STATUS_OK) {
        printf("Error: eps must be a real number\n");
        return 1;
    }
    if (eps <= 0.0 || eps >= 1.0) {
        printf("Error: eps must satisfy 0 < eps < 1\n");
        return 1;
    }

    const MethodInfo methods[METHODS_COUNT] = {
        {"e", "limit", e_limit},
        {"e", "series", e_series},
        {"e", "equation", e_equation},
        {"pi", "limit", pi_limit},
        {"pi", "series", pi_series},
        {"pi", "equation", pi_equation},
        {"ln 2", "limit", ln2_limit},
        {"ln 2", "series", ln2_series},
        {"ln 2", "equation", ln2_equation},
        {"sqrt(2)", "limit", sqrt2_limit},
        {"sqrt(2)", "product", sqrt2_product},
        {"sqrt(2)", "equation", sqrt2_equation},
        {"gamma", "limit", gamma_limit},
        {"gamma", "series", gamma_series},
        {"gamma", "equation", gamma_equation}
    };

    double values[METHODS_COUNT];
    Status statuses[METHODS_COUNT];
    for (int i = 0; i < METHODS_COUNT; i++) {
        values[i] = 0.0;
        statuses[i] = methods[i].compute(eps, &values[i]);
    }

    print_results(methods, values, statuses, METHODS_COUNT, eps);
    return 0;
}
