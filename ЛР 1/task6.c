#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>

#define MAX_BASE 36
#define MAX_BISECTION_STEPS 1000
#define MAX_KAPREKAR_CANDIDATE 4294967295ULL

typedef enum {
    STATUS_OK,
    STATUS_INVALID_ARGUMENT,
    STATUS_INVALID_NUMBER,
    STATUS_NO_MEMORY,
    STATUS_OVERFLOW,
    STATUS_DEGENERATE_POLYGON,
    STATUS_NO_ROOT,
    STATUS_PRECISION_NOT_REACHED
} Status;

static Status is_convex_polygon(int *is_convex, const double eps, const int vertices, ...) {
    if (is_convex == NULL || eps <= 0.0 || vertices < 3) {
        return STATUS_INVALID_ARGUMENT;
    }

    double *x = malloc(sizeof(double) * (size_t)vertices);
    double *y = malloc(sizeof(double) * (size_t)vertices);
    if (x == NULL || y == NULL) {
        free(x);
        free(y);
        return STATUS_NO_MEMORY;
    }

    va_list args;
    va_start(args, vertices);
    for (int i = 0; i < vertices; i++) {
        x[i] = va_arg(args, double);
        y[i] = va_arg(args, double);
    }
    va_end(args);

    const double pi = 4.0 * atan(1.0);
    Status status = STATUS_OK;
    int turn_sign = 0;
    int convex = 1;
    double total_angle = 0.0;

    for (int i = 0; i < vertices && convex; i++) {
        int a = i;
        int b = (i + 1) % vertices;
        int c = (i + 2) % vertices;
        double edge1_x = x[b] - x[a];
        double edge1_y = y[b] - y[a];
        double edge2_x = x[c] - x[b];
        double edge2_y = y[c] - y[b];

        if (fabs(edge1_x) < eps && fabs(edge1_y) < eps) {
            status = STATUS_DEGENERATE_POLYGON;
            break;
        }

        double cross = edge1_x * edge2_y - edge1_y * edge2_x;
        double dot = edge1_x * edge2_x + edge1_y * edge2_y;

        if (fabs(cross) < eps) {
            if (dot < 0.0) {
                convex = 0;
            }
        } else {
            int sign = cross > 0.0 ? 1 : -1;
            if (turn_sign == 0) {
                turn_sign = sign;
            } else if (sign != turn_sign) {
                convex = 0;
            }
        }
        total_angle += atan2(cross, dot);
    }

    free(x);
    free(y);
    if (status != STATUS_OK) {
        return status;
    }

    *is_convex = convex && turn_sign != 0 && fabs(fabs(total_angle) - 2.0 * pi) < eps;
    return STATUS_OK;
}

static Status polynomial_value(double *result, const double x, const int degree, ...) {
    if (result == NULL || degree < 0) {
        return STATUS_INVALID_ARGUMENT;
    }

    va_list args;
    va_start(args, degree);
    double value = 0.0;
    for (int i = 0; i <= degree; i++) {
        double coefficient = va_arg(args, double);
        value = value * x + coefficient;
    }
    va_end(args);

    if (!isfinite(value)) {
        return STATUS_OVERFLOW;
    }
    *result = value;
    return STATUS_OK;
}

static int digit_value(const char c) {
    if (c >= '0' && c <= '9') {
        return c - '0';
    }
    if (c >= 'A' && c <= 'Z') {
        return c - 'A' + 10;
    }
    if (c >= 'a' && c <= 'z') {
        return c - 'a' + 10;
    }
    return -1;
}

static Status parse_in_base(const char *str, const int base, unsigned long long *result) {
    if (str == NULL || result == NULL || base < 2 || base > MAX_BASE) {
        return STATUS_INVALID_ARGUMENT;
    }
    if (str[0] == '\0') {
        return STATUS_INVALID_NUMBER;
    }

    unsigned long long value = 0;
    for (size_t i = 0; str[i] != '\0'; i++) {
        int digit = digit_value(str[i]);
        if (digit < 0 || digit >= base) {
            return STATUS_INVALID_NUMBER;
        }
        if (value > (~0ULL - (unsigned long long)digit) / (unsigned long long)base) {
            return STATUS_OVERFLOW;
        }
        value = value * (unsigned long long)base + (unsigned long long)digit;
    }
    *result = value;
    return STATUS_OK;
}

static Status is_kaprekar(const unsigned long long n, const int base, int *result) {
    if (result == NULL || base < 2 || base > MAX_BASE) {
        return STATUS_INVALID_ARGUMENT;
    }
    if (n > MAX_KAPREKAR_CANDIDATE) {
        return STATUS_OVERFLOW;
    }

    *result = 0;
    if (n == 0) {
        return STATUS_OK;
    }

    unsigned long long square = n * n;
    unsigned long long power = (unsigned long long)base;
    while (1) {
        unsigned long long left = square / power;
        unsigned long long right = square % power;
        if (right > 0 && left + right == n) {
            *result = 1;
            return STATUS_OK;
        }
        if (left == 0 || power > square / (unsigned long long)base) {
            return STATUS_OK;
        }
        power *= (unsigned long long)base;
    }
}

static Status find_kaprekar(int results[], const int base, const int count, ...) {
    if (results == NULL || base < 2 || base > MAX_BASE || count < 0) {
        return STATUS_INVALID_ARGUMENT;
    }

    va_list args;
    va_start(args, count);
    Status status = STATUS_OK;
    for (int i = 0; i < count && status == STATUS_OK; i++) {
        const char *number = va_arg(args, const char *);
        unsigned long long value = 0;
        status = parse_in_base(number, base, &value);
        if (status == STATUS_OK) {
            status = is_kaprekar(value, base, &results[i]);
        }
    }
    va_end(args);
    return status;
}

static Status geometric_mean(double *result, const int count, ...) {
    if (result == NULL || count <= 0) {
        return STATUS_INVALID_ARGUMENT;
    }

    va_list args;
    va_start(args, count);
    Status status = STATUS_OK;
    double log_sum = 0.0;
    for (int i = 0; i < count; i++) {
        double value = va_arg(args, double);
        if (value < 0.0 || !isfinite(value)) {
            status = STATUS_INVALID_NUMBER;
            break;
        }
        log_sum += log(value);
    }
    va_end(args);

    if (status != STATUS_OK) {
        return status;
    }
    *result = exp(log_sum / count);
    return STATUS_OK;
}

static double power_recursive(const double base, const int exponent) {
    if (exponent == 0) {
        return 1.0;
    }
    double half = power_recursive(base, exponent / 2);
    double result = half * half;
    if (exponent % 2 == 1) {
        result *= base;
    } else if (exponent % 2 == -1) {
        result /= base;
    }
    return result;
}

static Status fast_power(const double base, const int exponent, double *result) {
    if (result == NULL || !isfinite(base)) {
        return STATUS_INVALID_ARGUMENT;
    }
    double value = power_recursive(base, exponent);
    if (!isfinite(value)) {
        return STATUS_OVERFLOW;
    }
    *result = value;
    return STATUS_OK;
}

static int same_sign(const double a, const double b) {
    return (a > 0.0 && b > 0.0) || (a < 0.0 && b < 0.0);
}

static Status dichotomy(double (*f)(double), double left, double right, const double eps, double *root) {
    if (f == NULL || root == NULL || eps <= 0.0) {
        return STATUS_INVALID_ARGUMENT;
    }
    if (left > right) {
        double temp = left;
        left = right;
        right = temp;
    }

    double f_left = f(left);
    double f_right = f(right);
    if (!isfinite(f_left) || !isfinite(f_right)) {
        return STATUS_INVALID_ARGUMENT;
    }
    if (same_sign(f_left, f_right)) {
        return STATUS_NO_ROOT;
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

static double equation_sqrt2(const double x) {
    return x * x - 2.0;
}

static double equation_cos(const double x) {
    return cos(x) - x;
}

static double equation_cubic(const double x) {
    return x * x * x - x - 2.0;
}

static double equation_exp(const double x) {
    return exp(x) - 3.0;
}

static double equation_no_root(const double x) {
    return x * x + 1.0;
}

static void print_status(const Status status) {
    switch (status) {
        case STATUS_INVALID_ARGUMENT: printf("error: invalid argument\n"); break;
        case STATUS_INVALID_NUMBER: printf("error: invalid number\n"); break;
        case STATUS_NO_MEMORY: printf("error: not enough memory\n"); break;
        case STATUS_OVERFLOW: printf("error: overflow or division by zero\n"); break;
        case STATUS_DEGENERATE_POLYGON: printf("error: polygon has coincident neighbouring vertices\n"); break;
        case STATUS_NO_ROOT: printf("error: function has the same sign at both ends\n"); break;
        case STATUS_PRECISION_NOT_REACHED: printf("error: precision not reached\n"); break;
        default: printf("error\n"); break;
    }
}

static void print_convex(const char *name, const Status status, const int is_convex) {
    printf("  %-31s ", name);
    if (status == STATUS_OK) {
        printf("%s\n", is_convex ? "convex" : "not convex");
    } else {
        print_status(status);
    }
}

static void print_value(const char *name, const Status status, const double value) {
    printf("  %-31s ", name);
    if (status == STATUS_OK) {
        printf("%.10g\n", value);
    } else {
        print_status(status);
    }
}

static void print_kaprekar(const int base, const char *numbers[], const int results[], const int count,
                           const Status status) {
    printf("  base %2d: ", base);
    if (status != STATUS_OK) {
        print_status(status);
        return;
    }
    for (int i = 0; i < count; i++) {
        printf("%s%s%s", numbers[i], results[i] ? "(yes)" : "(no)", i + 1 < count ? " " : "\n");
    }
}

static void demo_convex(void) {
    const double eps = 1e-9;
    int convex = 0;
    Status status;
    printf("6.1 Convex polygon\n");

    status = is_convex_polygon(&convex, eps, 4, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0);
    print_convex("square", status, convex);
    status = is_convex_polygon(&convex, eps, 3, 0.0, 0.0, 4.0, 0.0, 0.0, 3.0);
    print_convex("triangle", status, convex);
    status = is_convex_polygon(&convex, eps, 4, 0.0, 0.0, 2.0, 0.0, 1.0, 0.5, 1.0, 2.0);
    print_convex("concave quadrilateral", status, convex);
    status = is_convex_polygon(&convex, eps, 5, 0.0, 1.0, 0.588, -0.809, -0.951, 0.309, 0.951, 0.309, -0.588, -0.809);
    print_convex("star (self-intersecting)", status, convex);
    status = is_convex_polygon(&convex, eps, 4, 0.0, 0.0, 1.0, 0.0, 2.0, 0.0, 1.0, 1.0);
    print_convex("triangle with point on edge", status, convex);
    status = is_convex_polygon(&convex, eps, 3, 0.0, 0.0, 1.0, 1.0, 2.0, 2.0);
    print_convex("three points on a line", status, convex);
    status = is_convex_polygon(&convex, eps, 3, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0);
    print_convex("coincident vertices", status, convex);
    status = is_convex_polygon(&convex, eps, 2, 0.0, 0.0, 1.0, 1.0);
    print_convex("only two vertices", status, convex);
}

static void demo_polynomial(void) {
    double value = 0.0;
    Status status;
    printf("6.2 Polynomial value\n");

    status = polynomial_value(&value, 2.0, 2, 1.0, -3.0, 2.0);
    print_value("x^2 - 3x + 2 at x = 2", status, value);
    status = polynomial_value(&value, -1.5, 3, 2.0, 0.0, -1.0, 5.0);
    print_value("2x^3 - x + 5 at x = -1.5", status, value);
    status = polynomial_value(&value, 10.0, 0, 7.0);
    print_value("7 (degree 0) at x = 10", status, value);
    status = polynomial_value(&value, 1e200, 2, 1.0, 0.0, 0.0);
    print_value("x^2 at x = 1e200", status, value);
    status = polynomial_value(&value, 1.0, -1);
    print_value("negative degree", status, value);
}

static void demo_kaprekar(void) {
    printf("6.3 Kaprekar numbers\n");
    int results[8];

    const char *decimal[] = {"1", "9", "10", "45", "55", "99", "297", "4879"};
    Status status = find_kaprekar(results, 10, 8, "1", "9", "10", "45", "55", "99", "297", "4879");
    print_kaprekar(10, decimal, results, 8, status);

    const char *binary[] = {"1", "10", "110", "1001"};
    status = find_kaprekar(results, 2, 4, "1", "10", "110", "1001");
    print_kaprekar(2, binary, results, 4, status);

    const char *hex[] = {"F", "6", "a", "33"};
    status = find_kaprekar(results, 16, 4, "F", "6", "a", "33");
    print_kaprekar(16, hex, results, 4, status);

    const char *bad[] = {"12", "1Z"};
    status = find_kaprekar(results, 10, 2, "12", "1Z");
    print_kaprekar(10, bad, results, 2, status);
}

static void demo_geometric_mean(void) {
    double value = 0.0;
    Status status;
    printf("6.4 Geometric mean\n");

    status = geometric_mean(&value, 3, 1.0, 3.0, 9.0);
    print_value("1, 3, 9", status, value);
    status = geometric_mean(&value, 2, 2.0, 8.0);
    print_value("2, 8", status, value);
    status = geometric_mean(&value, 3, 1e300, 1e300, 1e300);
    print_value("1e300, 1e300, 1e300", status, value);
    status = geometric_mean(&value, 3, 5.0, 0.0, 7.0);
    print_value("5, 0, 7", status, value);
    status = geometric_mean(&value, 2, 4.0, -1.0);
    print_value("4, -1", status, value);
    status = geometric_mean(&value, 0);
    print_value("no numbers", status, value);
}

static void demo_power(void) {
    double value = 0.0;
    Status status;
    printf("6.5 Fast power\n");

    status = fast_power(2.0, 10, &value);
    print_value("2^10", status, value);
    status = fast_power(-3.0, 3, &value);
    print_value("(-3)^3", status, value);
    status = fast_power(2.0, -3, &value);
    print_value("2^(-3)", status, value);
    status = fast_power(1.5, 0, &value);
    print_value("1.5^0", status, value);
    status = fast_power(10.0, 400, &value);
    print_value("10^400", status, value);
    status = fast_power(0.0, -1, &value);
    print_value("0^(-1)", status, value);
}

static void demo_dichotomy(void) {
    double root = 0.0;
    Status status;
    printf("6.6 Dichotomy\n");

    status = dichotomy(equation_sqrt2, 0.0, 2.0, 1e-3, &root);
    print_value("x^2 - 2 = 0, [0, 2], 1e-3", status, root);
    status = dichotomy(equation_sqrt2, 0.0, 2.0, 1e-12, &root);
    print_value("x^2 - 2 = 0, [0, 2], 1e-12", status, root);
    status = dichotomy(equation_cos, 0.0, 1.0, 1e-9, &root);
    print_value("cos x - x = 0, [0, 1], 1e-9", status, root);
    status = dichotomy(equation_cubic, 2.0, 1.0, 1e-6, &root);
    print_value("x^3 - x - 2 = 0, [2, 1], 1e-6", status, root);
    status = dichotomy(equation_exp, 0.0, 5.0, 1e-8, &root);
    print_value("e^x - 3 = 0, [0, 5], 1e-8", status, root);
    status = dichotomy(equation_no_root, -1.0, 1.0, 1e-6, &root);
    print_value("x^2 + 1 = 0, [-1, 1], 1e-6", status, root);
}

int main(void) {
    demo_convex();
    demo_polynomial();
    demo_kaprekar();
    demo_geometric_mean();
    demo_power();
    demo_dichotomy();
    return 0;
}
