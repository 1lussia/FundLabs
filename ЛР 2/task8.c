#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

typedef enum
{
    STATUS_OK = 0,
    STATUS_NULL_ARG,
    STATUS_INVALID_ARG,
    STATUS_ALLOC_ERROR
} Status;

int digit_value(char c)
{
    if (c >= '0' && c <= '9')
        return c - '0';
    if (c >= 'a' && c <= 'z')
        return c - 'a' + 10;
    if (c >= 'A' && c <= 'Z')
        return c - 'A' + 10;
    return -1;
}

Status check_number(const char *s, unsigned int base, size_t *len)
{
    if (s == NULL || len == NULL)
        return STATUS_NULL_ARG;
    if (s[0] == '\0')
        return STATUS_INVALID_ARG;

    size_t n = 0;
    while (s[n] != '\0')
    {
        int d = digit_value(s[n]);
        if (d < 0 || (unsigned int)d >= base)
            return STATUS_INVALID_ARG;
        n++;
    }

    *len = n;
    return STATUS_OK;
}

void strip_leading_zeros(char *s)
{
    size_t start = 0;
    while (s[start] == '0' && s[start + 1] != '\0')
        start++;

    size_t i = 0;
    while (s[start + i] != '\0')
    {
        s[i] = s[start + i];
        i++;
    }
    s[i] = '\0';
}

Status add_in_base(const char *a, const char *b, unsigned int base, char **result)
{
    if (a == NULL || b == NULL || result == NULL)
        return STATUS_NULL_ARG;
    if (base < 2 || base > 36)
        return STATUS_INVALID_ARG;

    size_t la;
    size_t lb;
    Status st = check_number(a, base, &la);
    if (st == STATUS_OK)
        st = check_number(b, base, &lb);
    if (st != STATUS_OK)
        return st;

    const char *digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    size_t len = (la > lb ? la : lb) + 1;
    char *res = malloc(len + 1);
    if (res == NULL)
        return STATUS_ALLOC_ERROR;
    res[len] = '\0';

    unsigned int carry = 0;
    for (size_t k = len; k > 0; k--)
    {
        unsigned int sum = carry;
        if (la > 0)
            sum += (unsigned int)digit_value(a[--la]);
        if (lb > 0)
            sum += (unsigned int)digit_value(b[--lb]);
        res[k - 1] = digits[sum % base];
        carry = sum / base;
    }

    strip_leading_zeros(res);
    *result = res;
    return STATUS_OK;
}

Status sum_in_base(char **result, unsigned int base, size_t count, ...)
{
    if (result == NULL)
        return STATUS_NULL_ARG;
    if (base < 2 || base > 36 || count == 0)
        return STATUS_INVALID_ARG;

    char *sum = malloc(2);
    if (sum == NULL)
        return STATUS_ALLOC_ERROR;
    sum[0] = '0';
    sum[1] = '\0';

    Status st = STATUS_OK;
    va_list ap;
    va_start(ap, count);
    for (size_t i = 0; i < count && st == STATUS_OK; i++)
    {
        const char *number = va_arg(ap, const char *);
        char *next = NULL;
        st = add_in_base(sum, number, base, &next);
        if (st == STATUS_OK)
        {
            free(sum);
            sum = next;
        }
    }
    va_end(ap);

    if (st != STATUS_OK)
    {
        free(sum);
        return st;
    }

    *result = sum;
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

void print_sum(const char *title, Status st, char **sum)
{
    printf("%s = ", title);
    fflush(stdout);
    if (st == STATUS_OK)
        printf("%s\n", *sum);
    else
        print_error(st);
    free(*sum);
    *sum = NULL;
}

int main(void)
{
    char *sum = NULL;
    Status st;

    st = sum_in_base(&sum, 10, 3, "123", "877", "1000");
    print_sum("base 10: 123 + 877 + 1000", st, &sum);

    st = sum_in_base(&sum, 10, 2, "99999999999999999999999999999999", "1");
    print_sum("base 10: 99999999999999999999999999999999 + 1", st, &sum);

    st = sum_in_base(&sum, 2, 4, "1", "1", "1", "1");
    print_sum("base 2: 1 + 1 + 1 + 1", st, &sum);

    st = sum_in_base(&sum, 16, 2, "ff", "FF");
    print_sum("base 16: ff + FF", st, &sum);

    st = sum_in_base(&sum, 36, 2, "zz", "1");
    print_sum("base 36: zz + 1", st, &sum);

    st = sum_in_base(&sum, 8, 3, "0007", "0001", "000");
    print_sum("base 8: 0007 + 0001 + 000", st, &sum);

    st = sum_in_base(&sum, 10, 2, "0", "0000");
    print_sum("base 10: 0 + 0000", st, &sum);

    st = sum_in_base(&sum, 2, 2, "101", "102");
    print_sum("base 2: 101 + 102", st, &sum);

    st = sum_in_base(&sum, 10, 2, "12", "");
    print_sum("base 10: 12 + \"\"", st, &sum);

    st = sum_in_base(&sum, 37, 1, "1");
    print_sum("base 37: 1", st, &sum);

    st = sum_in_base(&sum, 10, 0);
    print_sum("base 10, no numbers", st, &sum);

    return 0;
}
