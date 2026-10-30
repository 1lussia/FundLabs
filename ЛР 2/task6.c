#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#define SPEC_MAX 64

typedef enum
{
    STATUS_OK = 0,
    STATUS_NULL_ARG,
    STATUS_NO_MATCH,
    STATUS_EOF,
    STATUS_FORMAT_ERROR
} Status;

typedef struct
{
    FILE *stream;
    const char *str;
    size_t consumed;
} Source;

int is_space(int c)
{
    return c == ' ' || c == '\t' || c == '\n' || c == '\r' ||
           c == '\v' || c == '\f';
}

int digit_value(int c, int upper)
{
    if (c >= '0' && c <= '9')
        return c - '0';
    if (!upper && c >= 'a' && c <= 'z')
        return c - 'a' + 10;
    if (upper && c >= 'A' && c <= 'Z')
        return c - 'A' + 10;
    return -1;
}

int source_get(Source *src)
{
    if (src->stream != NULL)
    {
        int c = fgetc(src->stream);
        if (c != EOF)
            src->consumed++;
        return c;
    }

    if (src->str[src->consumed] == '\0')
        return EOF;
    return (unsigned char)src->str[src->consumed++];
}

void source_unget(Source *src, int c)
{
    if (c == EOF)
        return;
    if (src->stream != NULL)
        ungetc(c, src->stream);
    src->consumed--;
}

void source_skip_spaces(Source *src)
{
    int c = source_get(src);
    while (c != EOF && is_space(c))
        c = source_get(src);
    source_unget(src, c);
}

long long roman_digit(int c)
{
    switch (c)
    {
    case 'I':
        return 1;
    case 'V':
        return 5;
    case 'X':
        return 10;
    case 'L':
        return 50;
    case 'C':
        return 100;
    case 'D':
        return 500;
    case 'M':
        return 1000;
    default:
        return 0;
    }
}

Status scan_roman(Source *src, int *target)
{
    source_skip_spaces(src);
    int c = source_get(src);
    if (c == EOF)
        return STATUS_EOF;

    int negative = 0;
    if (c == '-' || c == '+')
    {
        negative = (c == '-');
        c = source_get(src);
    }

    long long total = 0;
    long long pending = 0;
    int digits = 0;

    if (c == 'N')
    {
        digits = 1;
        c = source_get(src);
    }
    else
    {
        while (c != EOF && roman_digit(c) != 0)
        {
            long long cur = roman_digit(c);
            if (pending != 0)
            {
                if (pending < cur)
                    total -= pending;
                else
                    total += pending;
            }
            pending = cur;
            digits++;
            c = source_get(src);
        }
        total += pending;
    }
    source_unget(src, c);

    if (digits == 0)
        return STATUS_NO_MATCH;

    if (target != NULL)
        *target = (int)(negative ? -total : total);
    return STATUS_OK;
}

Status scan_zeckendorf(Source *src, unsigned int *target)
{
    source_skip_spaces(src);
    int c = source_get(src);
    if (c == EOF)
        return STATUS_EOF;

    unsigned long long value = 0;
    unsigned long long fib_cur = 1;
    unsigned long long fib_next = 2;
    int pending = -1;

    while (c == '0' || c == '1')
    {
        if (pending == '1')
            value += fib_cur;
        if (pending != -1)
        {
            unsigned long long next = fib_cur + fib_next;
            fib_cur = fib_next;
            fib_next = next;
        }
        pending = c;
        c = source_get(src);
    }
    source_unget(src, c);

    if (pending == -1)
        return STATUS_NO_MATCH;

    if (target != NULL)
        *target = (unsigned int)value;
    return STATUS_OK;
}

Status scan_base(Source *src, int *target, int base, int upper)
{
    if (base < 2 || base > 36)
        base = 10;

    source_skip_spaces(src);
    int c = source_get(src);
    if (c == EOF)
        return STATUS_EOF;

    int negative = 0;
    if (c == '-' || c == '+')
    {
        negative = (c == '-');
        c = source_get(src);
    }

    unsigned long long value = 0;
    int digits = 0;
    while (c != EOF)
    {
        int d = digit_value(c, upper);
        if (d < 0 || d >= base)
            break;
        value = value * (unsigned long long)base + (unsigned long long)d;
        digits++;
        c = source_get(src);
    }
    source_unget(src, c);

    if (digits == 0)
        return STATUS_NO_MATCH;

    if (target != NULL)
        *target = (int)(negative ? -(long long)value : (long long)value);
    return STATUS_OK;
}

Status scan_standard(Source *src, const char *spec, void *target, int suppress)
{
    int n = -1;
    int r;

    if (src->stream != NULL)
    {
        if (suppress)
            r = fscanf(src->stream, spec, &n);
        else
            r = fscanf(src->stream, spec, target, &n);
    }
    else
    {
        const char *rest = src->str + src->consumed;
        if (suppress)
            r = sscanf(rest, spec, &n);
        else
            r = sscanf(rest, spec, target, &n);
    }

    if (n >= 0)
    {
        src->consumed += (size_t)n;
        return STATUS_OK;
    }
    return (r == EOF) ? STATUS_EOF : STATUS_NO_MATCH;
}

Status spec_push(char *spec, size_t *len, char c)
{
    if (*len + 1 >= SPEC_MAX)
        return STATUS_FORMAT_ERROR;
    spec[(*len)++] = c;
    spec[*len] = '\0';
    return STATUS_OK;
}

int scan_result(int count, Status st)
{
    if (st == STATUS_EOF && count == 0)
        return EOF;
    return count;
}

int scan_format(Source *src, const char *format, va_list *ap)
{
    if (src == NULL || format == NULL || ap == NULL)
        return EOF;

    int count = 0;
    const char *p = format;

    while (*p != '\0')
    {
        if (is_space(*p))
        {
            source_skip_spaces(src);
            p++;
            continue;
        }

        if (*p != '%')
        {
            int c = source_get(src);
            if (c == EOF)
                return scan_result(count, STATUS_EOF);
            if (c != (unsigned char)*p)
            {
                source_unget(src, c);
                return count;
            }
            p++;
            continue;
        }

        p++;
        if (*p == '%')
        {
            source_skip_spaces(src);
            int c = source_get(src);
            if (c == EOF)
                return scan_result(count, STATUS_EOF);
            if (c != '%')
            {
                source_unget(src, c);
                return count;
            }
            p++;
            continue;
        }

        int suppress = 0;
        if (*p == '*')
        {
            suppress = 1;
            p++;
        }

        Status st = STATUS_OK;
        int custom = 1;

        if (p[0] == 'R' && p[1] == 'o')
        {
            int *target = suppress ? NULL : va_arg(*ap, int *);
            st = scan_roman(src, target);
        }
        else if (p[0] == 'Z' && p[1] == 'r')
        {
            unsigned int *target = suppress ? NULL : va_arg(*ap, unsigned int *);
            st = scan_zeckendorf(src, target);
        }
        else if (p[0] == 'C' && (p[1] == 'v' || p[1] == 'V'))
        {
            int *target = suppress ? NULL : va_arg(*ap, int *);
            int base = va_arg(*ap, int);
            st = scan_base(src, target, base, p[1] == 'V');
        }
        else
        {
            custom = 0;
        }

        if (custom)
        {
            p += 2;
            if (st != STATUS_OK)
                return scan_result(count, st);
            if (!suppress)
                count++;
            continue;
        }

        char spec[SPEC_MAX];
        size_t sl = 0;
        spec[0] = '\0';
        if (spec_push(spec, &sl, '%') != STATUS_OK)
            return count;
        if (suppress && spec_push(spec, &sl, '*') != STATUS_OK)
            return count;

        while (*p >= '0' && *p <= '9')
        {
            if (spec_push(spec, &sl, *p++) != STATUS_OK)
                return count;
        }
        while (*p == 'h' || *p == 'l' || *p == 'j' || *p == 'z' ||
               *p == 't' || *p == 'L')
        {
            if (spec_push(spec, &sl, *p++) != STATUS_OK)
                return count;
        }

        char conv = *p;
        if (conv == '\0')
            return count;
        p++;
        if (spec_push(spec, &sl, conv) != STATUS_OK)
            return count;

        if (conv == '[')
        {
            if (*p == '^' && spec_push(spec, &sl, *p++) != STATUS_OK)
                return count;
            if (*p == ']' && spec_push(spec, &sl, *p++) != STATUS_OK)
                return count;
            while (*p != '\0' && *p != ']')
            {
                if (spec_push(spec, &sl, *p++) != STATUS_OK)
                    return count;
            }
            if (*p != ']')
                return count;
            if (spec_push(spec, &sl, *p++) != STATUS_OK)
                return count;
        }

        if (conv == 'n')
        {
            int *target = suppress ? NULL : va_arg(*ap, int *);
            if (target != NULL)
                *target = (int)src->consumed;
            continue;
        }

        const char *valid = "diouxXeEfFgGaAcsp[";
        int known = 0;
        for (size_t i = 0; valid[i] != '\0'; i++)
        {
            if (valid[i] == conv)
                known = 1;
        }
        if (!known)
            return count;

        if (spec_push(spec, &sl, '%') != STATUS_OK ||
            spec_push(spec, &sl, 'n') != STATUS_OK)
            return count;

        void *target = suppress ? NULL : va_arg(*ap, void *);
        st = scan_standard(src, spec, target, suppress);
        if (st != STATUS_OK)
            return scan_result(count, st);
        if (!suppress)
            count++;
    }

    return count;
}

int overfscanf(FILE *stream, const char *format, ...)
{
    if (stream == NULL || format == NULL)
        return EOF;

    Source src;
    src.stream = stream;
    src.str = NULL;
    src.consumed = 0;

    va_list ap;
    va_start(ap, format);
    int result = scan_format(&src, format, &ap);
    va_end(ap);
    return result;
}

int oversscanf(const char *str, const char *format, ...)
{
    if (str == NULL || format == NULL)
        return EOF;

    Source src;
    src.stream = NULL;
    src.str = str;
    src.consumed = 0;

    va_list ap;
    va_start(ap, format);
    int result = scan_format(&src, format, &ap);
    va_end(ap);
    return result;
}

int main(void)
{
    int roman = 0;
    unsigned int zeck = 0;
    int low = 0;
    int high = 0;
    int plain = 0;
    double real = 0.0;
    char word[32] = "";
    int n;

    n = oversscanf("MCMXCIV 100011 ff 1A hello 42 2.5",
                   "%Ro %Zr %Cv %CV %s %d %lf",
                   &roman, &zeck, &low, 16, &high, 16, word, &plain, &real);
    printf("oversscanf returned %d\n", n);
    printf("%%Ro -> %d\n", roman);
    printf("%%Zr -> %u\n", zeck);
    printf("%%Cv (base 16) -> %d\n", low);
    printf("%%CV (base 16) -> %d\n", high);
    printf("%%s -> %s, %%d -> %d, %%lf -> %.2f\n\n", word, plain, real);

    int a = 0;
    int b = 0;
    int c = 0;
    unsigned int z = 0;
    n = oversscanf("XIV -XLII MMMM 100011 | 1010 -zz",
                   "%Ro %Ro %Ro %Zr | %Cv %Cv",
                   &a, &b, &c, &z, &low, 2, &high, 36);
    printf("oversscanf returned %d: %d %d %d %u %d %d\n\n", n, a, b, c, z, low, high);

    FILE *f = tmpfile();
    if (f == NULL)
    {
        fprintf(stderr, "cannot create temporary file\n");
        return 1;
    }
    fputs("MMXXIV 1001011 -7f 777\nXLII\n", f);
    rewind(f);

    int year = 0;
    unsigned int fib = 0;
    int hex = 0;
    int oct = 0;
    int answer = 0;
    n = overfscanf(f, "%Ro %Zr %Cv %Cv %Ro", &year, &fib, &hex, 16, &oct, 8, &answer);
    printf("overfscanf returned %d\n", n);
    printf("year=%d fib=%u hex=%d oct=%d answer=%d\n", year, fib, hex, oct, answer);
    fclose(f);

    return 0;
}
