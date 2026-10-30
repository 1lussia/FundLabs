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

typedef struct
{
    const double *coords;
} Vector;

typedef double (*NormFunc)(const double *x, size_t n, const void *params);

typedef struct
{
    NormFunc func;
    const void *params;
} Norm;

typedef struct
{
    double max_norm;
    size_t count;
    Vector *vectors;
} NormResult;

double norm_inf(const double *x, size_t n, const void *params)
{
    (void)params;

    double max = 0.0;
    for (size_t i = 0; i < n; i++)
    {
        double a = fabs(x[i]);
        if (a > max)
            max = a;
    }
    return max;
}

double norm_p(const double *x, size_t n, const void *params)
{
    double p = *(const double *)params;

    double sum = 0.0;
    for (size_t i = 0; i < n; i++)
        sum += pow(fabs(x[i]), p);
    return pow(sum, 1.0 / p);
}

double norm_a(const double *x, size_t n, const void *params)
{
    const double *a = (const double *)params;

    double sum = 0.0;
    for (size_t i = 0; i < n; i++)
    {
        double row = 0.0;
        for (size_t j = 0; j < n; j++)
            row += a[i * n + j] * x[j];
        sum += row * x[i];
    }
    return sqrt(sum > 0.0 ? sum : 0.0);
}

Status check_positive_definite(const double *a, size_t n, double eps)
{
    if (a == NULL || n == 0 || eps < 0.0)
        return STATUS_INVALID_ARG;

    for (size_t i = 0; i < n; i++)
    {
        for (size_t j = i + 1; j < n; j++)
        {
            if (fabs(a[i * n + j] - a[j * n + i]) > eps)
                return STATUS_INVALID_ARG;
        }
    }

    double *l = calloc(n * n, sizeof(double));
    if (l == NULL)
        return STATUS_ALLOC_ERROR;

    Status st = STATUS_OK;
    for (size_t i = 0; i < n && st == STATUS_OK; i++)
    {
        for (size_t j = 0; j <= i && st == STATUS_OK; j++)
        {
            double sum = a[i * n + j];
            for (size_t k = 0; k < j; k++)
                sum -= l[i * n + k] * l[j * n + k];

            if (i == j)
            {
                if (sum <= eps)
                    st = STATUS_INVALID_ARG;
                else
                    l[i * n + j] = sqrt(sum);
            }
            else
            {
                l[i * n + j] = sum / l[j * n + j];
            }
        }
    }

    free(l);
    return st;
}

Status make_inf_norm(Norm *out)
{
    if (out == NULL)
        return STATUS_NULL_ARG;

    out->func = norm_inf;
    out->params = NULL;
    return STATUS_OK;
}

Status make_p_norm(Norm *out, const double *p)
{
    if (out == NULL || p == NULL)
        return STATUS_NULL_ARG;
    if (!(*p >= 1.0))
        return STATUS_INVALID_ARG;

    out->func = norm_p;
    out->params = p;
    return STATUS_OK;
}

Status make_a_norm(Norm *out, const double *matrix, size_t n, double eps)
{
    if (out == NULL || matrix == NULL)
        return STATUS_NULL_ARG;

    Status st = check_positive_definite(matrix, n, eps);
    if (st != STATUS_OK)
        return st;

    out->func = norm_a;
    out->params = matrix;
    return STATUS_OK;
}

void free_norm_results(NormResult *results, size_t norm_count)
{
    if (results == NULL)
        return;

    for (size_t k = 0; k < norm_count; k++)
        free(results[k].vectors);
    free(results);
}

Status find_longest_vectors(NormResult **results, size_t n, size_t vec_count,
                            size_t norm_count, double eps, ...)
{
    if (results == NULL)
        return STATUS_NULL_ARG;
    if (n == 0 || vec_count == 0 || norm_count == 0 || eps < 0.0)
        return STATUS_INVALID_ARG;

    Vector *vecs = malloc(vec_count * sizeof(Vector));
    Norm *norms = malloc(norm_count * sizeof(Norm));
    double *values = malloc(vec_count * sizeof(double));
    NormResult *res = calloc(norm_count, sizeof(NormResult));
    if (vecs == NULL || norms == NULL || values == NULL || res == NULL)
    {
        free(vecs);
        free(norms);
        free(values);
        free(res);
        return STATUS_ALLOC_ERROR;
    }

    va_list ap;
    va_start(ap, eps);
    for (size_t i = 0; i < vec_count; i++)
        vecs[i] = va_arg(ap, Vector);
    for (size_t k = 0; k < norm_count; k++)
        norms[k] = va_arg(ap, Norm);
    va_end(ap);

    Status st = STATUS_OK;
    for (size_t i = 0; i < vec_count && st == STATUS_OK; i++)
    {
        if (vecs[i].coords == NULL)
            st = STATUS_NULL_ARG;
    }
    for (size_t k = 0; k < norm_count && st == STATUS_OK; k++)
    {
        if (norms[k].func == NULL)
            st = STATUS_NULL_ARG;
    }

    for (size_t k = 0; k < norm_count && st == STATUS_OK; k++)
    {
        double max = -1.0;
        for (size_t i = 0; i < vec_count; i++)
        {
            values[i] = norms[k].func(vecs[i].coords, n, norms[k].params);
            if (values[i] > max)
                max = values[i];
        }

        size_t count = 0;
        for (size_t i = 0; i < vec_count; i++)
        {
            if (max - values[i] <= eps)
                count++;
        }

        res[k].vectors = malloc(count * sizeof(Vector));
        if (res[k].vectors == NULL)
        {
            st = STATUS_ALLOC_ERROR;
            break;
        }

        size_t pos = 0;
        for (size_t i = 0; i < vec_count; i++)
        {
            if (max - values[i] <= eps)
                res[k].vectors[pos++] = vecs[i];
        }
        res[k].max_norm = max;
        res[k].count = count;
    }

    free(vecs);
    free(norms);
    free(values);

    if (st != STATUS_OK)
    {
        free_norm_results(res, norm_count);
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
    default:
        fprintf(stderr, "Error: unknown error\n");
        break;
    }
}

void print_vector(const Vector *v, size_t n)
{
    printf("(");
    for (size_t i = 0; i < n; i++)
        printf("%s%g", i == 0 ? "" : ", ", v->coords[i]);
    printf(")");
}

void print_results(const NormResult *results, size_t norm_count,
                   const char *const *names, size_t n)
{
    for (size_t k = 0; k < norm_count; k++)
    {
        printf("%s: max norm = %.4f, longest vectors (%zu):", names[k],
               results[k].max_norm, results[k].count);
        for (size_t i = 0; i < results[k].count; i++)
        {
            printf(" ");
            print_vector(&results[k].vectors[i], n);
        }
        printf("\n");
    }
}

int main(void)
{
    const size_t n = 3;
    const double eps = 1e-9;

    const double c1[] = {1.0, 2.0, 3.0};
    const double c2[] = {-4.0, 0.0, 1.0};
    const double c3[] = {2.0, 2.0, -4.0};
    const double c4[] = {4.0, 4.0, 0.0};
    const double c5[] = {0.0, -4.0, 4.0};

    const double p1 = 1.0;
    const double p2 = 2.0;
    const double p3 = 3.0;
    const double matrix[] = {2.0, 0.0, 0.0,
                             0.0, 3.0, 0.0,
                             0.0, 0.0, 1.0};

    Vector v1 = {c1};
    Vector v2 = {c2};
    Vector v3 = {c3};
    Vector v4 = {c4};
    Vector v5 = {c5};

    Norm norms[5];
    Status st = make_inf_norm(&norms[0]);
    if (st == STATUS_OK)
        st = make_p_norm(&norms[1], &p1);
    if (st == STATUS_OK)
        st = make_p_norm(&norms[2], &p2);
    if (st == STATUS_OK)
        st = make_p_norm(&norms[3], &p3);
    if (st == STATUS_OK)
        st = make_a_norm(&norms[4], matrix, n, eps);
    if (st != STATUS_OK)
    {
        print_error(st);
        return 1;
    }

    const char *names[] = {"inf-norm", "p-norm (p=1)", "p-norm (p=2)",
                           "p-norm (p=3)", "A-norm"};

    printf("Vectors:");
    const Vector all[] = {v1, v2, v3, v4, v5};
    for (size_t i = 0; i < 5; i++)
    {
        printf(" ");
        print_vector(&all[i], n);
    }
    printf("\n\n");

    NormResult *results = NULL;
    st = find_longest_vectors(&results, n, 5, 5, eps, v1, v2, v3, v4, v5,
                              norms[0], norms[1], norms[2], norms[3], norms[4]);
    if (st != STATUS_OK)
    {
        print_error(st);
        return 1;
    }

    print_results(results, 5, names, n);
    free_norm_results(results, 5);
    return 0;
}
