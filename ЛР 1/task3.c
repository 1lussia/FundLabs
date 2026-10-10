#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>

#define COEFFICIENTS 3
#define MAX_PERMUTATIONS 6

typedef enum {
    STATUS_OK,
    STATUS_INVALID_ARGUMENT,
    STATUS_INVALID_NUMBER,
    STATUS_INVALID_EPS,
    STATUS_OVERFLOW,
    STATUS_ZERO_NUMBER
} Status;

typedef enum {
    ROOTS_NONE,
    ROOTS_ONE,
    ROOTS_TWO,
    ROOTS_INFINITE
} RootsCount;

typedef struct {
    RootsCount count;
    double roots[2];
} Roots;

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

static Status parse_integer(const char *str, long long *result) {
    if (str == NULL || result == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    size_t i = 0;
    int is_negative = 0;
    if (str[i] == '-' || str[i] == '+') {
        is_negative = (str[i] == '-');
        i++;
    }
    if (str[i] == '\0') {
        return STATUS_INVALID_NUMBER;
    }

    const unsigned long long limit = (~0ULL >> 1) + (is_negative ? 1ULL : 0ULL);
    unsigned long long magnitude = 0;
    for (; str[i] != '\0'; i++) {
        if (str[i] < '0' || str[i] > '9') {
            return STATUS_INVALID_NUMBER;
        }
        unsigned long long digit = (unsigned long long)(str[i] - '0');
        if (magnitude > (limit - digit) / 10) {
            return STATUS_OVERFLOW;
        }
        magnitude = magnitude * 10 + digit;
    }

    if (!is_negative) {
        *result = (long long)magnitude;
    } else if (magnitude == 0) {
        *result = 0;
    } else {
        *result = -(long long)(magnitude - 1ULL) - 1;
    }
    return STATUS_OK;
}

static int is_equal(const double a, const double b, const double eps) {
    return fabs(a - b) < eps;
}

static Status unique_permutations(const double coefficients[COEFFICIENTS], const double eps,
                                  double result[MAX_PERMUTATIONS][COEFFICIENTS], int *count) {
    if (coefficients == NULL || result == NULL || count == NULL || eps <= 0.0) {
        return STATUS_INVALID_ARGUMENT;
    }

    const int orders[MAX_PERMUTATIONS][COEFFICIENTS] = {
        {0, 1, 2}, {0, 2, 1}, {1, 0, 2}, {1, 2, 0}, {2, 0, 1}, {2, 1, 0}
    };

    *count = 0;
    for (int p = 0; p < MAX_PERMUTATIONS; p++) {
        double candidate[COEFFICIENTS];
        for (int i = 0; i < COEFFICIENTS; i++) {
            candidate[i] = coefficients[orders[p][i]];
        }

        int is_new = 1;
        for (int j = 0; j < *count && is_new; j++) {
            if (is_equal(result[j][0], candidate[0], eps) &&
                is_equal(result[j][1], candidate[1], eps) &&
                is_equal(result[j][2], candidate[2], eps)) {
                is_new = 0;
            }
        }

        if (is_new) {
            for (int i = 0; i < COEFFICIENTS; i++) {
                result[*count][i] = candidate[i];
            }
            (*count)++;
        }
    }
    return STATUS_OK;
}

static Status solve_quadratic(const double a, const double b, const double c, const double eps, Roots *roots) {
    if (roots == NULL || eps <= 0.0) {
        return STATUS_INVALID_ARGUMENT;
    }
    roots->roots[0] = 0.0;
    roots->roots[1] = 0.0;

    if (is_equal(a, 0.0, eps)) {
        if (is_equal(b, 0.0, eps)) {
            roots->count = is_equal(c, 0.0, eps) ? ROOTS_INFINITE : ROOTS_NONE;
        } else {
            roots->count = ROOTS_ONE;
            roots->roots[0] = -c / b;
        }
    } else {
        double discriminant = b * b - 4.0 * a * c;
        if (!isfinite(discriminant)) {
            return STATUS_OVERFLOW;
        }

        if (is_equal(discriminant, 0.0, eps)) {
            roots->count = ROOTS_ONE;
            roots->roots[0] = -b / (2.0 * a);
        } else if (discriminant < 0.0) {
            roots->count = ROOTS_NONE;
        } else {
            double root_of_d = sqrt(discriminant);
            roots->count = ROOTS_TWO;
            roots->roots[0] = (-b - root_of_d) / (2.0 * a);
            roots->roots[1] = (-b + root_of_d) / (2.0 * a);
        }
    }

    for (int i = 0; i < 2; i++) {
        if (!isfinite(roots->roots[i])) {
            return STATUS_OVERFLOW;
        }
        if (is_equal(roots->roots[i], 0.0, eps)) {
            roots->roots[i] = 0.0;
        }
    }
    return STATUS_OK;
}

static Status is_multiple(const long long a, const long long b, int *result) {
    if (result == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    if (a == 0 || b == 0) {
        return STATUS_ZERO_NUMBER;
    }
    if (b == -1) {
        *result = 1;
    } else {
        *result = (a % b == 0);
    }
    return STATUS_OK;
}

static Status is_right_triangle(const double a, const double b, const double c, const double eps, int *result) {
    if (result == NULL || eps <= 0.0) {
        return STATUS_INVALID_ARGUMENT;
    }
    if (a < eps || b < eps || c < eps) {
        *result = 0;
        return STATUS_OK;
    }

    double leg1 = a;
    double leg2 = b;
    double hypotenuse = c;
    if (leg1 > hypotenuse) {
        double temp = leg1;
        leg1 = hypotenuse;
        hypotenuse = temp;
    }
    if (leg2 > hypotenuse) {
        double temp = leg2;
        leg2 = hypotenuse;
        hypotenuse = temp;
    }

    double legs = leg1 * leg1 + leg2 * leg2;
    double square = hypotenuse * hypotenuse;
    if (!isfinite(legs) || !isfinite(square)) {
        return STATUS_OVERFLOW;
    }
    *result = fabs(legs - square) < eps * square;
    return STATUS_OK;
}

static void print_roots(const double coefficients[COEFFICIENTS], const Roots *roots) {
    printf("%g*x^2 + %g*x + %g = 0: ", coefficients[0], coefficients[1], coefficients[2]);
    switch (roots->count) {
        case ROOTS_NONE: printf("no real roots\n"); break;
        case ROOTS_ONE: printf("x = %g\n", roots->roots[0]); break;
        case ROOTS_TWO: printf("x1 = %g, x2 = %g\n", roots->roots[0], roots->roots[1]); break;
        case ROOTS_INFINITE: printf("any x is a root\n"); break;
    }
}

static void print_error(const Status status) {
    switch (status) {
        case STATUS_INVALID_ARGUMENT: printf("Error: invalid argument\n"); break;
        case STATUS_INVALID_NUMBER: printf("Error: invalid number\n"); break;
        case STATUS_INVALID_EPS: printf("Error: eps must be a positive number\n"); break;
        case STATUS_OVERFLOW: printf("Error: number is too large (overflow)\n"); break;
        case STATUS_ZERO_NUMBER: printf("Error: numbers must be non-zero\n"); break;
        default: printf("Error: unknown error\n"); break;
    }
}

static Status parse_eps_and_three(char *argv[], double *eps, double numbers[COEFFICIENTS]) {
    Status status = parse_double(argv[2], eps);
    if (status != STATUS_OK) {
        return status;
    }
    if (*eps <= 0.0) {
        return STATUS_INVALID_EPS;
    }
    for (int i = 0; i < COEFFICIENTS; i++) {
        status = parse_double(argv[3 + i], &numbers[i]);
        if (status != STATUS_OK) {
            return status;
        }
    }
    return STATUS_OK;
}

static Status run_quadratic(char *argv[]) {
    double eps = 0.0;
    double coefficients[COEFFICIENTS];
    Status status = parse_eps_and_three(argv, &eps, coefficients);
    if (status != STATUS_OK) {
        return status;
    }

    double permutations[MAX_PERMUTATIONS][COEFFICIENTS];
    int count = 0;
    status = unique_permutations(coefficients, eps, permutations, &count);
    if (status != STATUS_OK) {
        return status;
    }

    Roots solutions[MAX_PERMUTATIONS];
    for (int i = 0; i < count; i++) {
        status = solve_quadratic(permutations[i][0], permutations[i][1], permutations[i][2], eps, &solutions[i]);
        if (status != STATUS_OK) {
            return status;
        }
    }
    for (int i = 0; i < count; i++) {
        print_roots(permutations[i], &solutions[i]);
    }
    return STATUS_OK;
}

static Status run_multiple(char *argv[]) {
    long long a = 0;
    long long b = 0;
    Status status = parse_integer(argv[2], &a);
    if (status == STATUS_OK) {
        status = parse_integer(argv[3], &b);
    }
    if (status != STATUS_OK) {
        return status;
    }

    int result = 0;
    status = is_multiple(a, b, &result);
    if (status != STATUS_OK) {
        return status;
    }
    printf("%lld %s a multiple of %lld\n", a, result ? "is" : "is not", b);
    return STATUS_OK;
}

static Status run_triangle(char *argv[]) {
    double eps = 0.0;
    double sides[COEFFICIENTS];
    Status status = parse_eps_and_three(argv, &eps, sides);
    if (status != STATUS_OK) {
        return status;
    }

    int result = 0;
    status = is_right_triangle(sides[0], sides[1], sides[2], eps, &result);
    if (status != STATUS_OK) {
        return status;
    }
    printf("%g, %g, %g %s be the sides of a right triangle\n", sides[0], sides[1], sides[2], result ? "can" : "cannot");
    return STATUS_OK;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Error: wrong number of arguments\n");
        return 1;
    }

    const char *flag = argv[1];
    if ((flag[0] != '-' && flag[0] != '/') || flag[1] == '\0' || flag[2] != '\0') {
        printf("Error: invalid flag\n");
        return 1;
    }

    Status status = STATUS_OK;
    switch (flag[1]) {
        case 'q':
            if (argc != 6) {
                printf("Error: flag -q needs exactly 4 parameters: eps a b c\n");
                return 1;
            }
            status = run_quadratic(argv);
            break;
        case 'm':
            if (argc != 4) {
                printf("Error: flag -m needs exactly 2 parameters: a b\n");
                return 1;
            }
            status = run_multiple(argv);
            break;
        case 't':
            if (argc != 6) {
                printf("Error: flag -t needs exactly 4 parameters: eps a b c\n");
                return 1;
            }
            status = run_triangle(argv);
            break;
        default:
            printf("Error: unknown flag\n");
            return 1;
    }

    if (status != STATUS_OK) {
        print_error(status);
        return 1;
    }
    return 0;
}
