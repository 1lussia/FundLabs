#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>

typedef enum
{
    STATUS_OK = 0,
    STATUS_NULL_ARG,
    STATUS_INVALID_ARG,
    STATUS_ALLOC_ERROR,
    STATUS_PRECISION_ERROR
} Status;

typedef struct
{
    double value;
    unsigned long long numerator;
    unsigned long long denominator;
    int is_finite;
} FractionInfo;

unsigned long long gcd(unsigned long long a, unsigned long long b)
{
    while (b != 0)
    {
        unsigned long long t = a % b;
        a = b;
        b = t;
    }
    return a;
}

Status to_fraction(double x, double eps, unsigned long long *num, unsigned long long *den)
{
    if (num == NULL || den == NULL)
        return STATUS_NULL_ARG;

    const unsigned long long max = ~0ULL;
    unsigned long long h_prev = 0;
    unsigned long long h = 1;
    unsigned long long k_prev = 1;
    unsigned long long k = 0;
    double rest = x;

    for (int step = 0; step < 64; step++)
    {
        double whole = floor(rest);
        if (whole > 1e18)
            return STATUS_PRECISION_ERROR;

        unsigned long long a = (unsigned long long)whole;
        if (h != 0 && a > (max - h_prev) / h)
            return STATUS_PRECISION_ERROR;
        if (k != 0 && a > (max - k_prev) / k)
            return STATUS_PRECISION_ERROR;

        unsigned long long h_next = a * h + h_prev;
        unsigned long long k_next = a * k + k_prev;
        h_prev = h;
        h = h_next;
        k_prev = k;
        k = k_next;

        if (fabs(x - (double)h / (double)k) <= eps)
        {
            *num = h;
            *den = k;
            return STATUS_OK;
        }

        double frac = rest - whole;
        if (frac <= 0.0)
            return STATUS_PRECISION_ERROR;
        rest = 1.0 / frac;
    }
    return STATUS_PRECISION_ERROR;
}

int is_finite_in_base(unsigned long long den, unsigned int base)
{
    unsigned long long g = gcd(den, base);
    while (g > 1)
    {
        den /= g;
        g = gcd(den, base);
    }
    return den == 1;
}

Status check_fractions(FractionInfo **results, unsigned int base, double eps,
                       size_t count, ...)
{
    if (results == NULL)
        return STATUS_NULL_ARG;
    if (base < 2 || count == 0 || !(eps > 0.0 && eps < 1.0))
        return STATUS_INVALID_ARG;

    FractionInfo *res = malloc(count * sizeof(FractionInfo));
    if (res == NULL)
        return STATUS_ALLOC_ERROR;

    va_list ap;
    va_start(ap, count);
    for (size_t i = 0; i < count; i++)
        res[i].value = va_arg(ap, double);
    va_end(ap);

    Status st = STATUS_OK;
    for (size_t i = 0; i < count && st == STATUS_OK; i++)
    {
        double x = res[i].value;
        if (!(x > 0.0 && x < 1.0))
        {
            st = STATUS_INVALID_ARG;
            break;
        }

        st = to_fraction(x, eps, &res[i].numerator, &res[i].denominator);
        if (st == STATUS_OK)
            res[i].is_finite = is_finite_in_base(res[i].denominator, base);
    }

    if (st != STATUS_OK)
    {
        free(res);
        return st;
    }

    *results = res;
    return STATUS_OK;
}

void print_error(Status st)
{
    switch (st)
    {
    case STATUS_NULL_ARG:
        fprintf(stderr, "Error: null pointer passed to function\n");
        break;
    case STATUS_INVALID_ARG:
        fprintf(stderr, "Error: invalid argument\n");
        break;
    case STATUS_ALLOC_ERROR:
        fprintf(stderr, "Error: memory allocation failed\n");
        break;
    case STATUS_PRECISION_ERROR:
        fprintf(stderr, "Error: cannot find fraction with given precision\n");
        break;
    default:
        fprintf(stderr, "Error: unknown error\n");
        break;
    }
}

void print_results(Status st, FractionInfo *results, size_t count, unsigned int base)
{
    printf("Base %u:\n", base);
    fflush(stdout);
    if (st != STATUS_OK)
    {
        print_error(st);
        return;
    }

    for (size_t i = 0; i < count; i++)
    {
        printf("  %.10g = %llu/%llu -> %s\n", results[i].value,
               results[i].numerator, results[i].denominator,
               results[i].is_finite ? "finite" : "infinite");
    }
    free(results);
}

int main(void)
{
    const double eps = 1e-12;
    FractionInfo *res = NULL;
    Status st;

    st = check_fractions(&res, 10, eps, 6, 0.5, 0.125, 0.1 + 0.2, 1.0 / 3.0, 0.3125, 1.0 / 7.0);
    print_results(st, res, 6, 10);

    st = check_fractions(&res, 2, eps, 4, 0.5, 0.3125, 0.1, 1.0 / 3.0);
    print_results(st, res, 4, 2);

    st = check_fractions(&res, 3, eps, 4, 1.0 / 3.0, 2.0 / 9.0, 0.5, 1.0 / 6.0);
    print_results(st, res, 4, 3);

    st = check_fractions(&res, 12, eps, 4, 1.0 / 3.0, 0.25, 1.0 / 6.0, 0.2);
    print_results(st, res, 4, 12);

    st = check_fractions(&res, 10, eps, 2, 0.5, 1.5);
    print_results(st, res, 2, 10);

    st = check_fractions(&res, 1, eps, 1, 0.5);
    print_results(st, res, 1, 1);

    return 0;
}
