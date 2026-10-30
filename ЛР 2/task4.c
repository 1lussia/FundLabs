#include <stdio.h>
#include <stdlib.h>

typedef enum
{
    STATUS_OK = 0,
    STATUS_NULL_ARG,
    STATUS_INVALID_ARG,
    STATUS_ALLOC_ERROR,
    STATUS_FORMAT_ERROR,
    STATUS_OVERFLOW,
    STATUS_IO_ERROR,
    STATUS_EOF
} Status;

typedef struct
{
    char *name;
    unsigned long long size;
    unsigned long long importance;
} FileInfo;

typedef struct
{
    FileInfo *files;
    size_t count;
    unsigned long long total_size;
    unsigned long long total_importance;
} BackupResult;

Status parse_ull(const char *s, unsigned long long *value)
{
    if (s == NULL || value == NULL)
        return STATUS_NULL_ARG;
    if (s[0] == '\0')
        return STATUS_INVALID_ARG;

    const unsigned long long max = ~0ULL;
    unsigned long long res = 0;

    for (size_t i = 0; s[i] != '\0'; i++)
    {
        if (s[i] < '0' || s[i] > '9')
            return STATUS_INVALID_ARG;

        unsigned long long d = (unsigned long long)(s[i] - '0');
        if (res > (max - d) / 10)
            return STATUS_INVALID_ARG;
        res = res * 10 + d;
    }

    *value = res;
    return STATUS_OK;
}

int is_space(int c)
{
    return c == ' ' || c == '\t' || c == '\n' || c == '\r' ||
           c == '\v' || c == '\f';
}

Status read_token(FILE *f, char **token)
{
    if (f == NULL || token == NULL)
        return STATUS_NULL_ARG;

    int c = fgetc(f);
    while (c != EOF && is_space(c))
        c = fgetc(f);
    if (c == EOF)
        return ferror(f) ? STATUS_IO_ERROR : STATUS_EOF;

    size_t cap = 32;
    size_t len = 0;
    char *buf = malloc(cap);
    if (buf == NULL)
        return STATUS_ALLOC_ERROR;

    while (c != EOF && !is_space(c))
    {
        if (len + 1 >= cap)
        {
            char *tmp = realloc(buf, cap * 2);
            if (tmp == NULL)
            {
                free(buf);
                return STATUS_ALLOC_ERROR;
            }
            buf = tmp;
            cap *= 2;
        }
        buf[len++] = (char)c;
        c = fgetc(f);
    }

    if (c == EOF && ferror(f))
    {
        free(buf);
        return STATUS_IO_ERROR;
    }

    buf[len] = '\0';
    *token = buf;
    return STATUS_OK;
}

Status read_word(FILE *f, char **word)
{
    Status st = read_token(f, word);
    if (st == STATUS_EOF)
        return STATUS_FORMAT_ERROR;
    return st;
}

Status read_ull(FILE *f, unsigned long long *value)
{
    char *token = NULL;
    Status st = read_word(f, &token);
    if (st != STATUS_OK)
        return st;

    st = parse_ull(token, value);
    free(token);
    if (st != STATUS_OK)
        return STATUS_FORMAT_ERROR;
    return STATUS_OK;
}

void free_files(FileInfo *files, size_t n)
{
    if (files == NULL)
        return;

    for (size_t i = 0; i < n; i++)
        free(files[i].name);
    free(files);
}

void free_result(BackupResult *result)
{
    if (result == NULL)
        return;

    free(result->files);
    result->files = NULL;
    result->count = 0;
}

Status read_input(FILE *in, FileInfo **files, size_t *n,
                  unsigned long long *capacity)
{
    if (in == NULL || files == NULL || n == NULL || capacity == NULL)
        return STATUS_NULL_ARG;

    unsigned long long count_ull;
    unsigned long long cap;
    Status st = read_ull(in, &count_ull);
    if (st != STATUS_OK)
        return st;
    st = read_ull(in, &cap);
    if (st != STATUS_OK)
        return st;
    if (count_ull == 0 || count_ull > (unsigned long long)((size_t)-1))
        return STATUS_FORMAT_ERROR;

    size_t count = (size_t)count_ull;
    FileInfo *arr = calloc(count, sizeof(FileInfo));
    if (arr == NULL)
        return STATUS_ALLOC_ERROR;

    for (size_t i = 0; i < count; i++)
    {
        st = read_word(in, &arr[i].name);
        if (st == STATUS_OK)
            st = read_ull(in, &arr[i].size);
        if (st == STATUS_OK && arr[i].size == 0)
            st = STATUS_FORMAT_ERROR;
        if (st == STATUS_OK)
            st = read_ull(in, &arr[i].importance);

        if (st != STATUS_OK)
        {
            free_files(arr, count);
            return st;
        }
    }

    *files = arr;
    *n = count;
    *capacity = cap;
    return STATUS_OK;
}

Status select_backup_files(const FileInfo *files, size_t n,
                           unsigned long long capacity, BackupResult *result)
{
    if (result == NULL || (files == NULL && n > 0))
        return STATUS_NULL_ARG;

    result->files = NULL;
    result->count = 0;
    result->total_size = 0;
    result->total_importance = 0;
    if (n == 0)
        return STATUS_OK;

    const unsigned long long max = ~0ULL;
    unsigned long long all_importance = 0;
    unsigned long long limit = 0;
    int capped = 0;

    for (size_t i = 0; i < n; i++)
    {
        if (files[i].size == 0)
            return STATUS_INVALID_ARG;
        if (files[i].importance > max - all_importance)
            return STATUS_OVERFLOW;
        all_importance += files[i].importance;

        if (!capped)
        {
            if (files[i].size > capacity - limit)
            {
                limit = capacity;
                capped = 1;
            }
            else
            {
                limit += files[i].size;
            }
        }
    }

    if (limit >= (unsigned long long)((size_t)-1))
        return STATUS_ALLOC_ERROR;

    size_t width = (size_t)limit + 1;
    unsigned long long *best = calloc(width, sizeof(unsigned long long));
    unsigned char *choice = calloc(n, width);
    unsigned char *picked = calloc(n, 1);
    if (best == NULL || choice == NULL || picked == NULL)
    {
        free(best);
        free(choice);
        free(picked);
        return STATUS_ALLOC_ERROR;
    }

    for (size_t i = 0; i < n; i++)
    {
        if (files[i].size > limit)
            continue;

        size_t sz = (size_t)files[i].size;
        for (size_t w = (size_t)limit; w >= sz; w--)
        {
            unsigned long long cand = best[w - sz] + files[i].importance;
            if (cand > best[w])
            {
                best[w] = cand;
                choice[i * width + w] = 1;
            }
        }
    }

    size_t w = (size_t)limit;
    size_t k = 0;
    unsigned long long total_size = 0;
    unsigned long long total_importance = 0;
    for (size_t i = n; i > 0; i--)
    {
        size_t idx = i - 1;
        if (choice[idx * width + w])
        {
            picked[idx] = 1;
            k++;
            w -= (size_t)files[idx].size;
            total_size += files[idx].size;
            total_importance += files[idx].importance;
        }
    }

    FileInfo *selected = NULL;
    if (k > 0)
    {
        selected = malloc(k * sizeof(FileInfo));
        if (selected == NULL)
        {
            free(best);
            free(choice);
            free(picked);
            return STATUS_ALLOC_ERROR;
        }

        size_t pos = 0;
        for (size_t i = 0; i < n; i++)
        {
            if (picked[i])
                selected[pos++] = files[i];
        }
    }

    free(best);
    free(choice);
    free(picked);

    result->files = selected;
    result->count = k;
    result->total_size = total_size;
    result->total_importance = total_importance;
    return STATUS_OK;
}

void print_result(const BackupResult *result)
{
    printf("TOTAL_SIZE %llu\n", result->total_size);
    printf("TOTAL_IMPORTANCE %llu\n", result->total_importance);
    printf("FILES %zu\n", result->count);
    for (size_t i = 0; i < result->count; i++)
        printf("%s\n", result->files[i].name);
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
    case STATUS_FORMAT_ERROR:
        fprintf(stderr, "Error: invalid input data format\n");
        break;
    case STATUS_OVERFLOW:
        fprintf(stderr, "Error: total importance is too large\n");
        break;
    case STATUS_IO_ERROR:
        fprintf(stderr, "Error: input read failure\n");
        break;
    default:
        fprintf(stderr, "Error: unknown error\n");
        break;
    }
}

int main(void)
{
    FileInfo *files = NULL;
    size_t n = 0;
    unsigned long long capacity = 0;

    Status st = read_input(stdin, &files, &n, &capacity);
    if (st != STATUS_OK)
    {
        print_error(st);
        return 1;
    }

    BackupResult result;
    st = select_backup_files(files, n, capacity, &result);
    if (st == STATUS_OK)
    {
        print_result(&result);
        free_result(&result);
    }

    free_files(files, n);

    if (st != STATUS_OK)
    {
        print_error(st);
        return 1;
    }
    return 0;
}
