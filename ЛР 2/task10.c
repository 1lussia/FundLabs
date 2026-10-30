#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>

typedef enum
{
    STATUS_OK = 0,
    STATUS_NULL_ARG,
    STATUS_INVALID_ARG,
    STATUS_ALLOC_ERROR
} Status;

Status reexpand_polynomial(double eps, double a, double **result, int degree, ...)
{
    if (result == NULL)
        return STATUS_NULL_ARG;
    if (degree < 0 || !(eps > 0.0) || !isfinite(a))
        return STATUS_INVALID_ARG;

    size_t n = (size_t)degree;
    double *g = malloc((n + 1) * sizeof(double));
    if (g == NULL)
        return STATUS_ALLOC_ERROR;

    va_list ap;
    va_start(ap, degree);
    for (size_t i = 0; i <= n; i++)
        g[i] = va_arg(ap, double);
    va_end(ap);

    for (size_t i = 0; i <= n; i++)
    {
        if (!isfinite(g[i]))
        {
            free(g);
            return STATUS_INVALID_ARG;
        }
    }

    for (size_t i = 0; i < n; i++)
    {
        for (size_t j = n; j > i; j--)
            g[j - 1] += a * g[j];
    }

    for (size_t i = 0; i <= n; i++)
    {
        if (fabs(g[i]) < eps)
            g[i] = 0.0;
    }

    *result = g;
    return STATUS_OK;
}

double evaluate(const double *coeffs, int degree, double x)
{
    double value = 0.0;
    for (int i = degree; i >= 0; i--)
        value = value * x + coeffs[i];
    return value;
}

void print_polynomial(const double *coeffs, int degree, double a)
{
    for (int i = 0; i <= degree; i++)
    {
        if (i == 0)
        {
            printf("%g", coeffs[i]);
            continue;
        }
        printf(" %c %g", coeffs[i] < 0.0 ? '-' : '+', fabs(coeffs[i]));

        if (a == 0.0)
            printf("*x");
        else
            printf("*(x %c %g)", a > 0.0 ? '-' : '+', fabs(a));
        if (i > 1)
            printf("^%d", i);
    }
    printf("\n");
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
    default:
        fprintf(stderr, "Error: unknown error\n");
        break;
    }
}

void show(const char *title, Status st, const double *f, double *g, int degree,
          double a, double x)
{
    printf("%s\n", title);
    fflush(stdout);
    if (st != STATUS_OK)
    {
        print_error(st);
        printf("\n");
        return;
    }

    printf("f(x) = ");
    print_polynomial(f, degree, 0.0);
    printf("     = ");
    print_polynomial(g, degree, a);
    printf("check at x = %g: %g and %g\n\n", x, evaluate(f, degree, x),
           evaluate(g, degree, x - a));
    free(g);
}

int main(void)
{
    const double eps = 1e-12;
    double *g = NULL;
    Status st;

    const double f1[] = {1.0, 2.0, 3.0};
    st = reexpand_polynomial(eps, 1.0, &g, 2, f1[0], f1[1], f1[2]);
    show("a = 1", st, f1, g, 2, 1.0, 2.5);

    const double f2[] = {0.0, 0.0, 0.0, 1.0};
    st = reexpand_polynomial(eps, 2.0, &g, 3, f2[0], f2[1], f2[2], f2[3]);
    show("a = 2", st, f2, g, 3, 2.0, -1.0);

    const double f3[] = {-5.0, 4.0, 0.0, -2.0, 1.0};
    st = reexpand_polynomial(eps, -1.5, &g, 4, f3[0], f3[1], f3[2], f3[3], f3[4]);
    show("a = -1.5", st, f3, g, 4, -1.5, 0.75);

    const double f4[] = {1.0, -2.0, 1.0};
    st = reexpand_polynomial(eps, 1.0, &g, 2, f4[0], f4[1], f4[2]);
    show("a = 1", st, f4, g, 2, 1.0, 3.0);

    const double f5[] = {7.0};
    st = reexpand_polynomial(eps, 100.0, &g, 0, f5[0]);
    show("a = 100, degree 0", st, f5, g, 0, 100.0, 1.0);

    const double f6[] = {1.0, 1.0};
    st = reexpand_polynomial(eps, 0.0, &g, 1, f6[0], f6[1]);
    show("a = 0", st, f6, g, 1, 0.0, 2.0);

    st = reexpand_polynomial(eps, 1.0, &g, -1, 1.0);
    show("degree = -1", st, f6, g, 1, 1.0, 0.0);

    st = reexpand_polynomial(0.0, 1.0, &g, 1, f6[0], f6[1]);
    show("eps = 0", st, f6, g, 1, 1.0, 0.0);

    return 0;
}
