#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

typedef enum
{
    STATUS_OK = 0,
    STATUS_NULL_ARG,
    STATUS_INVALID_ARG,
    STATUS_ALLOC_ERROR
} Status;

Status str_length(const char *s, size_t *len)
{
    if (s == NULL || len == NULL)
        return STATUS_NULL_ARG;
    size_t n = 0;
    while (s[n] != '\0')
        n++;

    *len = n;
    return STATUS_OK;
}

Status str_reverse(const char *s, char **result)
{
    size_t len;
    Status st = str_length(s, &len);
    if (st != STATUS_OK)
        return st;
    if (result == NULL)
        return STATUS_NULL_ARG;

    char *res = (char *) malloc(len + 1);
    if (res == NULL)
        return STATUS_ALLOC_ERROR;
    for (size_t i = 0; i < len; i++)
        res[i] = s[len - 1 - i];
    res[len] = '\0';

    *result = res;
    return STATUS_OK;
}

Status str_upper_odd(const char *s, char **result)
{
    size_t len;
    Status st = str_length(s, &len);
    if (st != STATUS_OK)
        return st;
    if (result == NULL)
        return STATUS_NULL_ARG;

    char *res = (char *) malloc(len + 1);
    if (res == NULL)
        return STATUS_ALLOC_ERROR;

    for (size_t i = 0; i < len; i++)
    {
        if (i % 2 != 0)
            res[i] = toupper((unsigned char)s[i]);
        else
            res[i] = s[i];
    }
    res[len] = '\0';
    *result = res;
    return STATUS_OK;
}

Status str_digits_letters_rest(const char *s, char **result)
{
    size_t len;
    Status st = str_length(s, &len);
    if (st != STATUS_OK)
        return st;
    if (result == NULL)
        return STATUS_NULL_ARG;

    char *res = malloc(len + 1);
    if (res == NULL)
        return STATUS_ALLOC_ERROR;
    size_t pos = 0;
    for (int group = 0; group < 3; group++)
    {
        for (size_t i = 0; i < len; i++)
        {
            unsigned char c = (unsigned char)s[i];
            int cur;
            if (isdigit(c))
                cur = 0;
            else if (isalpha(c))
                cur = 1;
            else
                cur = 2;
            if (cur == group)
                res[pos++] = s[i];
        }
    }
    res[len] = '\0';
    *result = res;
    return STATUS_OK;
}

Status str_concat_random(const char *const *parts, size_t count, unsigned int seed, char **result)
{
    if (parts == NULL || result == NULL)
        return STATUS_NULL_ARG;
    if (count == 0)
        return STATUS_INVALID_ARG;
    size_t total = 0;
    for (size_t i = 0; i < count; i++)
    {
        size_t len;
        Status st = str_length(parts[i], &len);
        if (st != STATUS_OK)
            return st;
        total += len;
    }
    size_t *order = (size_t *) malloc(count * sizeof(size_t));
    if (order == NULL)
        return STATUS_ALLOC_ERROR;
    char *res = (char *) malloc(total + 1);
    if (res == NULL)
    {
        free(order);
        return STATUS_ALLOC_ERROR;
    }
    for (size_t i = 0; i < count; i++)
        order[i] = i;
    srand(seed);
    for (size_t i = count - 1; i > 0; i--)
    {
        size_t j = rand() % (i + 1);
        size_t temp = order[i];
        order[i] = order[j];
        order[j] = temp;
    }
    size_t pos = 0;
    for (size_t i = 0; i < count; i++)
    {
        const char *p = parts[order[i]];
        for (size_t j = 0; p[j] != '\0'; j++)
            res[pos++] = p[j];
    }
    res[total] = '\0';
    free(order);
    *result = res;
    return STATUS_OK;
}

Status parse_uint(const char *s, unsigned int *value)
{
    if (s == NULL || value == NULL)
        return STATUS_NULL_ARG;
    if (s[0] == '\0')
        return STATUS_INVALID_ARG;

    const unsigned int max = ~0u;
    unsigned int res = 0;

    for (size_t i = 0; s[i] != '\0'; i++)
    {
        if (s[i] < '0' || s[i] > '9')
            return STATUS_INVALID_ARG;
        unsigned int d = (unsigned int)(s[i] - '0');
        if (res > (max - d) / 10)
            return STATUS_INVALID_ARG;
        res = res * 10 + d;
    }
    *value = res;
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

int main(int argc, char *argv[])
{
    if (argc < 3)
    {
        fprintf(stderr, "Wrong number of arguments.\n");
        return 1;
    }
    const char *flag = argv[1];
    if (flag[0] != '-' || flag[1] == '\0' || flag[2] != '\0')
    {
        fprintf(stderr, "Error: invalid flag\n");
        return 1;
    }

    Status st = STATUS_OK;
    char *result = NULL;

    switch (flag[1])
    {
    case 'l':
    {
        if (argc != 3)
        {
            fprintf(stderr, "Wrong number of arguments.\n");
            return 1;
        }
        size_t len = 0;
        st = str_length(argv[2], &len);
        if (st == STATUS_OK)
            printf("%zu\n", len);
        break;
    }
    case 'r':
    case 'u':
    case 'n':
    {
        if (argc != 3)
        {
            fprintf(stderr, "Wrong number of arguments.\n");
            return 1;
        }
        if (flag[1] == 'r')
            st = str_reverse(argv[2], &result);
        else if (flag[1] == 'u')
            st = str_upper_odd(argv[2], &result);
        else
            st = str_digits_letters_rest(argv[2], &result);

        if (st == STATUS_OK)
            printf("%s\n", result);
        break;
    }
    case 'c':
    {
        if (argc < 4)
        {
            fprintf(stderr, "Wrong number of arguments.\n");
            return 1;
        }
        unsigned int seed;
        st = parse_uint(argv[3], &seed);
        if (st != STATUS_OK)
            break;

        size_t count = (size_t)(argc - 3);
        const char **parts = malloc(count * sizeof(char *));
        if (parts == NULL)
        {
            st = STATUS_ALLOC_ERROR;
            break;
        }

        parts[0] = argv[2];
        for (int i = 4; i < argc; i++)
            parts[i - 3] = argv[i];

        st = str_concat_random(parts, count, seed, &result);
        free(parts);

        if (st == STATUS_OK)
            printf("%s\n", result);
        break;
    }
    default:
        fprintf(stderr, "Error: unknown flag %s\n", flag);
        return 1;
    }

    free(result);

    if (st != STATUS_OK)
    {
        print_error(st);
        return 1;
    }
    return 0;
}
