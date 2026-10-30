#include <stdio.h>
#include <stdlib.h>
#include <float.h>

#define SALARY_EPS 1e-9

typedef enum
{
    STATUS_OK = 0,
    STATUS_NULL_ARG,
    STATUS_INVALID_ARG,
    STATUS_ALLOC_ERROR,
    STATUS_FILE_OPEN_ERROR,
    STATUS_IO_ERROR,
    STATUS_FORMAT_ERROR,
    STATUS_EMPTY_LINE,
    STATUS_EOF
} Status;

typedef struct
{
    unsigned int id;
    char *name;
    char *surname;
    double salary;
} Employee;

Status copy_string(const char *s, char **result)
{
    if (s == NULL || result == NULL)
        return STATUS_NULL_ARG;

    size_t len = 0;
    while (s[len] != '\0')
        len++;

    char *res = (char *)malloc(len + 1);
    if (res == NULL)
        return STATUS_ALLOC_ERROR;

    for (size_t i = 0; i <= len; i++)
        res[i] = s[i];

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

Status parse_salary(const char *s, double *value)
{
    if (s == NULL || value == NULL)
        return STATUS_NULL_ARG;

    int digits = 0;
    int dots = 0;
    for (size_t i = 0; s[i] != '\0'; i++)
    {
        if (s[i] >= '0' && s[i] <= '9')
            digits++;
        else if (s[i] == '.')
            dots++;
        else
            return STATUS_INVALID_ARG;
    }
    if (digits == 0 || dots > 1)
        return STATUS_INVALID_ARG;

    char *end;
    double res = strtod(s, &end);
    if (*end != '\0' || res > DBL_MAX)
        return STATUS_INVALID_ARG;

    *value = res;
    return STATUS_OK;
}

int is_latin_word(const char *s)
{
    if (s == NULL || s[0] == '\0')
        return 0;

    for (size_t i = 0; s[i] != '\0'; i++)
    {
        int is_upper = (s[i] >= 'A' && s[i] <= 'Z');
        int is_lower = (s[i] >= 'a' && s[i] <= 'z');
        if (!is_upper && !is_lower)
            return 0;
    }
    return 1;
}

Status parse_flag(const char *flag, char *order)
{
    if (flag == NULL || order == NULL)
        return STATUS_NULL_ARG;
    if (flag[0] != '-' && flag[0] != '/')
        return STATUS_INVALID_ARG;
    if (flag[1] != 'a' && flag[1] != 'd')
        return STATUS_INVALID_ARG;
    if (flag[2] != '\0')
        return STATUS_INVALID_ARG;

    *order = flag[1];
    return STATUS_OK;
}

char *next_token(char **cursor)
{
    char *p = *cursor;
    while (*p == ' ' || *p == '\t' || *p == '\r')
        p++;
    if (*p == '\0')
    {
        *cursor = p;
        return NULL;
    }

    char *start = p;
    while (*p != '\0' && *p != ' ' && *p != '\t' && *p != '\r')
        p++;
    if (*p != '\0')
    {
        *p = '\0';
        p++;
    }

    *cursor = p;
    return start;
}

Status parse_employee(char *line, Employee *emp)
{
    if (line == NULL || emp == NULL)
        return STATUS_NULL_ARG;

    char *cursor = line;
    char *tok_id = next_token(&cursor);
    if (tok_id == NULL)
        return STATUS_EMPTY_LINE;

    char *tok_name = next_token(&cursor);
    char *tok_surname = next_token(&cursor);
    char *tok_salary = next_token(&cursor);
    char *tok_extra = next_token(&cursor);
    if (tok_name == NULL || tok_surname == NULL || tok_salary == NULL || tok_extra != NULL)
        return STATUS_FORMAT_ERROR;

    unsigned int id;
    double salary;
    if (parse_uint(tok_id, &id) != STATUS_OK)
        return STATUS_FORMAT_ERROR;
    if (!is_latin_word(tok_name) || !is_latin_word(tok_surname))
        return STATUS_FORMAT_ERROR;
    if (parse_salary(tok_salary, &salary) != STATUS_OK)
        return STATUS_FORMAT_ERROR;

    char *name;
    char *surname;
    Status st = copy_string(tok_name, &name);
    if (st != STATUS_OK)
        return st;
    st = copy_string(tok_surname, &surname);
    if (st != STATUS_OK)
    {
        free(name);
        return st;
    }

    emp->id = id;
    emp->name = name;
    emp->surname = surname;
    emp->salary = salary;
    return STATUS_OK;
}

Status read_line(FILE *f, char **line)
{
    if (f == NULL || line == NULL)
        return STATUS_NULL_ARG;

    size_t cap = 64;
    size_t len = 0;
    char *buf = (char *)malloc(cap);
    if (buf == NULL)
        return STATUS_ALLOC_ERROR;

    int c;
    while ((c = fgetc(f)) != EOF && c != '\n')
    {
        if (len + 1 >= cap)
        {
            char *tmp = (char *)realloc(buf, cap * 2);
            if (tmp == NULL)
            {
                free(buf);
                return STATUS_ALLOC_ERROR;
            }
            buf = tmp;
            cap *= 2;
        }
        buf[len++] = (char)c;
    }
    if (c == EOF && ferror(f))
    {
        free(buf);
        return STATUS_IO_ERROR;
    }
    if (c == EOF && len == 0)
    {
        free(buf);
        return STATUS_EOF;
    }
    buf[len] = '\0';
    *line = buf;
    return STATUS_OK;
}

void free_employees(Employee *arr, size_t count)
{
    if (arr == NULL)
        return;
    for (size_t i = 0; i < count; i++)
    {
        free(arr[i].name);
        free(arr[i].surname);
    }
    free(arr);
}

Status read_employees(FILE *f, Employee **result, size_t *count)
{
    if (f == NULL || result == NULL || count == NULL)
        return STATUS_NULL_ARG;

    size_t cap = 8;
    size_t n = 0;
    Employee *arr = malloc(cap * sizeof(Employee));
    if (arr == NULL)
        return STATUS_ALLOC_ERROR;

    Status st = STATUS_OK;
    while (1)
    {
        char *line = NULL;
        st = read_line(f, &line);
        if (st != STATUS_OK)
            break;

        Employee emp;
        st = parse_employee(line, &emp);
        free(line);
        if (st == STATUS_EMPTY_LINE)
        {
            st = STATUS_OK;
            continue;
        }
        if (st != STATUS_OK)
            break;

        if (n == cap)
        {
            Employee *tmp = realloc(arr, cap * 2 * sizeof(Employee));
            if (tmp == NULL)
            {
                free(emp.name);
                free(emp.surname);
                st = STATUS_ALLOC_ERROR;
                break;
            }
            arr = tmp;
            cap *= 2;
        }
        arr[n++] = emp;
    }

    if (st == STATUS_EOF)
        st = STATUS_OK;
    if (st != STATUS_OK)
    {
        free_employees(arr, n);
        return st;
    }

    *result = arr;
    *count = n;
    return STATUS_OK;
}

int compare_double(double a, double b, double eps)
{
    if (a - b > eps)
        return 1;
    if (b - a > eps)
        return -1;
    return 0;
}

int str_compare(const char *a, const char *b)
{
    while (*a != '\0' && *a == *b)
    {
        a++;
        b++;
    }
    return (unsigned char)*a - (unsigned char)*b;
}

int compare_employees(const Employee *a, const Employee *b)
{
    int r = compare_double(a->salary, b->salary, SALARY_EPS);
    if (r != 0)
        return r;

    r = str_compare(a->surname, b->surname);
    if (r != 0)
        return r;

    r = str_compare(a->name, b->name);
    if (r != 0)
        return r;

    if (a->id > b->id)
        return 1;
    if (a->id < b->id)
        return -1;
    return 0;
}

int compare_asc(const void *a, const void *b)
{
    return compare_employees((const Employee *)a, (const Employee *)b);
}

int compare_desc(const void *a, const void *b)
{
    return compare_employees((const Employee *)b, (const Employee *)a);
}

Status sort_employees(Employee *arr, size_t count, char order)
{
    if (arr == NULL)
        return STATUS_NULL_ARG;

    switch (order)
    {
    case 'a':
        qsort(arr, count, sizeof(Employee), compare_asc);
        return STATUS_OK;
    case 'd':
        qsort(arr, count, sizeof(Employee), compare_desc);
        return STATUS_OK;
    default:
        return STATUS_INVALID_ARG;
    }
}

Status write_employees(FILE *f, const Employee *arr, size_t count)
{
    if (f == NULL || arr == NULL)
        return STATUS_NULL_ARG;

    for (size_t i = 0; i < count; i++)
    {
        if (fprintf(f, "%u %s %s %.2f\n", arr[i].id, arr[i].name,
                    arr[i].surname, arr[i].salary) < 0)
            return STATUS_IO_ERROR;
    }
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
    case STATUS_FILE_OPEN_ERROR:
        fprintf(stderr, "Error: cannot open file\n");
        break;
    case STATUS_IO_ERROR:
        fprintf(stderr, "Error: file read/write failure\n");
        break;
    case STATUS_FORMAT_ERROR:
        fprintf(stderr, "Error: invalid data format in input file\n");
        break;
    default:
        fprintf(stderr, "Error: unknown error\n");
        break;
    }
}

int main(int argc, char *argv[])
{
    if (argc != 4)
    {
        fprintf(stderr, "Wrong number of arguments.\n");
        return 1;
    }

    char order;
    Status st = parse_flag(argv[2], &order);
    if (st != STATUS_OK)
    {
        fprintf(stderr, "Error: invalid flag\n");
        return 1;
    }

    FILE *in = fopen(argv[1], "r");
    if (in == NULL)
    {
        print_error(STATUS_FILE_OPEN_ERROR);
        return 1;
    }

    Employee *emp = NULL;
    size_t count = 0;
    st = read_employees(in, &emp, &count);
    fclose(in);
    if (st != STATUS_OK)
    {
        print_error(st);
        return 1;
    }

    st = sort_employees(emp, count, order);
    if (st == STATUS_OK)
    {
        FILE *out = fopen(argv[3], "w");
        if (out == NULL)
        {
            st = STATUS_FILE_OPEN_ERROR;
        }
        else
        {
            st = write_employees(out, emp, count);
            if (fclose(out) != 0 && st == STATUS_OK)
                st = STATUS_IO_ERROR;
        }
    }

    free_employees(emp, count);

    if (st != STATUS_OK)
    {
        print_error(st);
        return 1;
    }
    return 0;
}
