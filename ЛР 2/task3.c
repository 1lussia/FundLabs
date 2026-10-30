#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

typedef enum
{
    STATUS_OK = 0,
    STATUS_NULL_ARG,
    STATUS_INVALID_ARG,
    STATUS_ALLOC_ERROR,
    STATUS_FILE_OPEN_ERROR,
    STATUS_IO_ERROR
} Status;

typedef struct
{
    size_t line;
    size_t column;
} Match;

typedef struct
{
    const char *path;
    Status status;
    Match *matches;
    size_t count;
} FileResult;

Status read_file(const char *path, char **text, size_t *size)
{
    if (path == NULL || text == NULL || size == NULL)
        return STATUS_NULL_ARG;

    FILE *f = fopen(path, "r");
    if (f == NULL)
        return STATUS_FILE_OPEN_ERROR;

    size_t cap = 4096;
    size_t len = 0;
    char *buf = malloc(cap);
    if (buf == NULL)
    {
        fclose(f);
        return STATUS_ALLOC_ERROR;
    }

    while (1)
    {
        if (len == cap)
        {
            char *tmp = realloc(buf, cap * 2);
            if (tmp == NULL)
            {
                free(buf);
                fclose(f);
                return STATUS_ALLOC_ERROR;
            }
            buf = tmp;
            cap *= 2;
        }

        size_t got = fread(buf + len, 1, cap - len, f);
        if (got == 0)
            break;
        len += got;
    }

    if (ferror(f))
    {
        free(buf);
        fclose(f);
        return STATUS_IO_ERROR;
    }
    fclose(f);

    *text = buf;
    *size = len;
    return STATUS_OK;
}

Status find_matches(const char *text, size_t text_len,
                    const char *sub, size_t sub_len,
                    Match **matches, size_t *count)
{
    if (text == NULL || sub == NULL || matches == NULL || count == NULL)
        return STATUS_NULL_ARG;
    if (sub_len == 0)
        return STATUS_INVALID_ARG;

    Match *arr = NULL;
    size_t cap = 0;
    size_t n = 0;
    size_t line = 1;
    size_t column = 1;

    for (size_t i = 0; i + sub_len <= text_len; i++)
    {
        size_t j = 0;
        while (j < sub_len && text[i + j] == sub[j])
            j++;

        if (j == sub_len)
        {
            if (n == cap)
            {
                size_t new_cap = (cap == 0) ? 8 : cap * 2;
                Match *tmp = realloc(arr, new_cap * sizeof(Match));
                if (tmp == NULL)
                {
                    free(arr);
                    return STATUS_ALLOC_ERROR;
                }
                arr = tmp;
                cap = new_cap;
            }
            arr[n].line = line;
            arr[n].column = column;
            n++;
        }

        if (text[i] == '\n')
        {
            line++;
            column = 1;
        }
        else
        {
            column++;
        }
    }

    *matches = arr;
    *count = n;
    return STATUS_OK;
}

void free_results(FileResult *results, size_t file_count)
{
    if (results == NULL)
        return;

    for (size_t i = 0; i < file_count; i++)
        free(results[i].matches);
    free(results);
}

Status find_in_files(FileResult **results, const char *substr,
                     size_t file_count, ...)
{
    if (results == NULL || substr == NULL)
        return STATUS_NULL_ARG;
    if (substr[0] == '\0' || file_count == 0)
        return STATUS_INVALID_ARG;

    size_t sub_len = 0;
    while (substr[sub_len] != '\0')
        sub_len++;

    FileResult *res = calloc(file_count, sizeof(FileResult));
    if (res == NULL)
        return STATUS_ALLOC_ERROR;

    va_list args;
    va_start(args, file_count);
    for (size_t i = 0; i < file_count; i++)
    {
        const char *path = va_arg(args, const char *);
        res[i].path = path;

        char *text = NULL;
        size_t text_len = 0;
        res[i].status = read_file(path, &text, &text_len);
        if (res[i].status != STATUS_OK)
            continue;

        res[i].status = find_matches(text, text_len, substr, sub_len,
                                     &res[i].matches, &res[i].count);
        free(text);
    }
    va_end(args);

    *results = res;
    return STATUS_OK;
}

const char *status_message(Status st)
{
    switch (st)
    {
    case STATUS_NULL_ARG:
        return "null pointer passed to function";
    case STATUS_INVALID_ARG:
        return "invalid argument";
    case STATUS_ALLOC_ERROR:
        return "memory allocation failed";
    case STATUS_FILE_OPEN_ERROR:
        return "cannot open file";
    case STATUS_IO_ERROR:
        return "file read failure";
    default:
        return "unknown error";
    }
}

void print_error(Status st)
{
    fprintf(stderr, "Error: %s\n", status_message(st));
}

void print_results(const FileResult *results, size_t file_count)
{
    for (size_t i = 0; i < file_count; i++)
    {
        printf("File: %s\n", results[i].path);

        if (results[i].status != STATUS_OK)
        {
            printf("  Error: %s\n", status_message(results[i].status));
        }
        else if (results[i].count == 0)
        {
            printf("  No occurrences found\n");
        }
        else
        {
            printf("  Occurrences found: %zu\n", results[i].count);
            for (size_t k = 0; k < results[i].count; k++)
            {
                printf("  line %zu, position %zu\n",
                       results[i].matches[k].line,
                       results[i].matches[k].column);
            }
        }
    }
}

int main(int argc, char *argv[])
{
    if (argc < 3 || argc > 5)
    {
        fprintf(stderr, "Wrong number of arguments.\n");
        return 1;
    }

    size_t file_count = (size_t)(argc - 2);
    FileResult *results = NULL;
    Status st;

    switch (file_count)
    {
    case 1:
        st = find_in_files(&results, argv[1], 1, argv[2]);
        break;
    case 2:
        st = find_in_files(&results, argv[1], 2, argv[2], argv[3]);
        break;
    case 3:
        st = find_in_files(&results, argv[1], 3, argv[2], argv[3], argv[4]);
        break;
    default:
        st = STATUS_INVALID_ARG;
        break;
    }

    if (st != STATUS_OK)
    {
        print_error(st);
        return 1;
    }

    print_results(results, file_count);
    free_results(results, file_count);
    return 0;
}
