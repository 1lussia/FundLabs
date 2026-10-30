#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>

#define SPEC_MAX 64

typedef enum
{
    STATUS_OK = 0,
    STATUS_NULL_ARG,
    STATUS_INVALID_ARG,
    STATUS_ALLOC_ERROR,
    STATUS_FORMAT_ERROR
} Status;

typedef struct
{
    char *data;
    size_t len;
    size_t cap;
} Buffer;

typedef enum
{
    LEN_NONE,
    LEN_HH,
    LEN_H,
    LEN_L,
    LEN_LL,
    LEN_J,
    LEN_Z,
    LEN_T,
    LEN_BIGL
} LengthMod;

size_t str_length(const char *s)
{
    size_t n = 0;
    while (s[n] != '\0')
        n++;
    return n;
}

void buffer_init(Buffer *b)
{
    b->data = NULL;
    b->len = 0;
    b->cap = 0;
}

void buffer_free(Buffer *b)
{
    free(b->data);
    b->data = NULL;
    b->len = 0;
    b->cap = 0;
}

Status buffer_reserve(Buffer *b, size_t extra)
{
    if (b == NULL)
        return STATUS_NULL_ARG;
    if (extra > (size_t)-1 - b->len - 1)
        return STATUS_ALLOC_ERROR;

    size_t need = b->len + extra + 1;
    if (need <= b->cap)
        return STATUS_OK;

    size_t new_cap = (b->cap == 0) ? 64 : b->cap;
    while (new_cap < need)
    {
        if (new_cap > (size_t)-1 / 2)
        {
            new_cap = need;
            break;
        }
        new_cap *= 2;
    }

    char *tmp = realloc(b->data, new_cap);
    if (tmp == NULL)
        return STATUS_ALLOC_ERROR;

    b->data = tmp;
    b->cap = new_cap;
    b->data[b->len] = '\0';
    return STATUS_OK;
}

Status buffer_append(Buffer *b, const char *s, size_t n)
{
    Status st = buffer_reserve(b, n);
    if (st != STATUS_OK)
        return st;

    for (size_t i = 0; i < n; i++)
        b->data[b->len + i] = s[i];
    b->len += n;
    b->data[b->len] = '\0';
    return STATUS_OK;
}

Status buffer_push(Buffer *b, char c)
{
    return buffer_append(b, &c, 1);
}

Status buffer_printf(Buffer *b, const char *spec, ...)
{
    va_list ap;
    va_list ap_copy;
    va_start(ap, spec);
    va_copy(ap_copy, ap);

    int n = vsnprintf(NULL, 0, spec, ap);
    va_end(ap);
    if (n < 0)
    {
        va_end(ap_copy);
        return STATUS_FORMAT_ERROR;
    }

    Status st = buffer_reserve(b, (size_t)n);
    if (st != STATUS_OK)
    {
        va_end(ap_copy);
        return st;
    }

    vsnprintf(b->data + b->len, (size_t)n + 1, spec, ap_copy);
    va_end(ap_copy);
    b->len += (size_t)n;
    return STATUS_OK;
}

int digit_value(char c, int upper)
{
    if (c >= '0' && c <= '9')
        return c - '0';
    if (!upper && c >= 'a' && c <= 'z')
        return c - 'a' + 10;
    if (upper && c >= 'A' && c <= 'Z')
        return c - 'A' + 10;
    return -1;
}

Status append_roman(Buffer *out, int value)
{
    unsigned long long n;
    Status st;

    if (value < 0)
    {
        st = buffer_push(out, '-');
        if (st != STATUS_OK)
            return st;
        n = (unsigned long long)(-(long long)value);
    }
    else
    {
        n = (unsigned long long)value;
    }

    if (n == 0)
        return buffer_push(out, 'N');

    const unsigned long long values[13] = {1000, 900, 500, 400, 100, 90, 50,
                                           40, 10, 9, 5, 4, 1};
    const char *symbols[13] = {"M", "CM", "D", "CD", "C", "XC", "L",
                               "XL", "X", "IX", "V", "IV", "I"};

    for (int i = 0; i < 13; i++)
    {
        while (n >= values[i])
        {
            st = buffer_append(out, symbols[i], str_length(symbols[i]));
            if (st != STATUS_OK)
                return st;
            n -= values[i];
        }
    }
    return STATUS_OK;
}

Status append_zeckendorf(Buffer *out, unsigned int value)
{
    unsigned long long fib[64];
    char coef[64];
    size_t cnt = 2;

    fib[0] = 1;
    fib[1] = 2;
    while (fib[cnt - 1] <= value)
    {
        fib[cnt] = fib[cnt - 1] + fib[cnt - 2];
        cnt++;
    }

    size_t digits = cnt - 1;
    unsigned long long rest = value;
    for (size_t i = digits; i > 0; i--)
    {
        if (fib[i - 1] <= rest)
        {
            coef[i - 1] = '1';
            rest -= fib[i - 1];
        }
        else
        {
            coef[i - 1] = '0';
        }
    }

    Status st = buffer_append(out, coef, digits);
    if (st != STATUS_OK)
        return st;
    return buffer_push(out, '1');
}

Status append_base(Buffer *out, int value, int base, int upper)
{
    if (base < 2 || base > 36)
        base = 10;

    const char *alphabet = upper ? "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                 : "0123456789abcdefghijklmnopqrstuvwxyz";
    char tmp[72];
    size_t n = 0;
    unsigned long long u;
    Status st;

    if (value < 0)
    {
        st = buffer_push(out, '-');
        if (st != STATUS_OK)
            return st;
        u = (unsigned long long)(-(long long)value);
    }
    else
    {
        u = (unsigned long long)value;
    }

    do
    {
        tmp[n++] = alphabet[u % (unsigned long long)base];
        u /= (unsigned long long)base;
    } while (u > 0);

    while (n > 0)
    {
        st = buffer_push(out, tmp[--n]);
        if (st != STATUS_OK)
            return st;
    }
    return STATUS_OK;
}

Status parse_in_base(const char *s, int base, int upper, long long *result)
{
    if (s == NULL || result == NULL)
        return STATUS_NULL_ARG;
    if (base < 2 || base > 36)
        base = 10;

    int negative = 0;
    size_t i = 0;
    if (s[0] == '-')
    {
        negative = 1;
        i = 1;
    }
    else if (s[0] == '+')
    {
        i = 1;
    }
    if (s[i] == '\0')
        return STATUS_INVALID_ARG;

    const unsigned long long max = ~0ULL >> 1;
    unsigned long long value = 0;
    for (; s[i] != '\0'; i++)
    {
        int d = digit_value(s[i], upper);
        if (d < 0 || d >= base)
            return STATUS_INVALID_ARG;
        if (value > (max - (unsigned long long)d) / (unsigned long long)base)
            return STATUS_INVALID_ARG;
        value = value * (unsigned long long)base + (unsigned long long)d;
    }

    *result = negative ? -(long long)value : (long long)value;
    return STATUS_OK;
}

Status append_from_base(Buffer *out, const char *s, int base, int upper)
{
    long long value;
    Status st = parse_in_base(s, base, upper, &value);
    if (st != STATUS_OK)
        return st;
    return buffer_printf(out, "%lld", value);
}

Status append_dump(Buffer *out, const void *object, size_t size)
{
    const unsigned char *bytes = (const unsigned char *)object;

    for (size_t i = 0; i < size; i++)
    {
        Status st;
        if (i > 0)
        {
            st = buffer_push(out, ' ');
            if (st != STATUS_OK)
                return st;
        }
        for (int bit = 7; bit >= 0; bit--)
        {
            st = buffer_push(out, ((bytes[i] >> bit) & 1) ? '1' : '0');
            if (st != STATUS_OK)
                return st;
        }
    }
    return STATUS_OK;
}

Status try_custom(Buffer *out, const char **pp, va_list *ap, int *handled)
{
    const char *p = *pp;
    Status st = STATUS_OK;
    *handled = 0;

    if (p[0] == '\0')
        return STATUS_OK;
    char a = p[0];
    char b = p[1];

    if (a == 'R' && b == 'o')
    {
        int v = va_arg(*ap, int);
        st = append_roman(out, v);
    }
    else if (a == 'Z' && b == 'r')
    {
        unsigned int v = va_arg(*ap, unsigned int);
        st = append_zeckendorf(out, v);
    }
    else if (a == 'C' && (b == 'v' || b == 'V'))
    {
        int v = va_arg(*ap, int);
        int base = va_arg(*ap, int);
        st = append_base(out, v, base, b == 'V');
    }
    else if ((a == 't' && b == 'o') || (a == 'T' && b == 'O'))
    {
        const char *s = va_arg(*ap, const char *);
        int base = va_arg(*ap, int);
        st = append_from_base(out, s, base, a == 'T');
    }
    else if (a == 'm' && b == 'i')
    {
        int v = va_arg(*ap, int);
        st = append_dump(out, &v, sizeof v);
    }
    else if (a == 'm' && b == 'u')
    {
        unsigned int v = va_arg(*ap, unsigned int);
        st = append_dump(out, &v, sizeof v);
    }
    else if (a == 'm' && b == 'd')
    {
        double v = va_arg(*ap, double);
        st = append_dump(out, &v, sizeof v);
    }
    else if (a == 'm' && b == 'f')
    {
        float v = (float)va_arg(*ap, double);
        st = append_dump(out, &v, sizeof v);
    }
    else
    {
        return STATUS_OK;
    }

    *handled = 1;
    *pp = p + 2;
    return st;
}

Status spec_push(char *spec, size_t *len, char c)
{
    if (*len + 1 >= SPEC_MAX)
        return STATUS_FORMAT_ERROR;
    spec[(*len)++] = c;
    spec[*len] = '\0';
    return STATUS_OK;
}

Status spec_push_int(char *spec, size_t *len, int value)
{
    char tmp[24];
    size_t n = 0;
    long long v = value;
    unsigned long long u = (v < 0) ? (unsigned long long)(-v) : (unsigned long long)v;

    do
    {
        tmp[n++] = (char)('0' + u % 10);
        u /= 10;
    } while (u > 0);

    Status st;
    if (v < 0)
    {
        st = spec_push(spec, len, '-');
        if (st != STATUS_OK)
            return st;
    }
    while (n > 0)
    {
        st = spec_push(spec, len, tmp[--n]);
        if (st != STATUS_OK)
            return st;
    }
    return STATUS_OK;
}

Status handle_standard(Buffer *out, const char **pp, va_list *ap)
{
    const char *p = *pp;
    char spec[SPEC_MAX];
    size_t sl = 0;
    Status st;

    if (*p == '%')
    {
        *pp = p + 1;
        return buffer_push(out, '%');
    }

    spec[0] = '\0';
    st = spec_push(spec, &sl, '%');
    if (st != STATUS_OK)
        return st;

    while (*p == '-' || *p == '+' || *p == ' ' || *p == '#' || *p == '0')
    {
        st = spec_push(spec, &sl, *p++);
        if (st != STATUS_OK)
            return st;
    }

    if (*p == '*')
    {
        int w = va_arg(*ap, int);
        p++;
        st = spec_push_int(spec, &sl, w);
        if (st != STATUS_OK)
            return st;
    }
    else
    {
        while (*p >= '0' && *p <= '9')
        {
            st = spec_push(spec, &sl, *p++);
            if (st != STATUS_OK)
                return st;
        }
    }

    if (*p == '.')
    {
        p++;
        if (*p == '*')
        {
            int prec = va_arg(*ap, int);
            p++;
            if (prec >= 0)
            {
                st = spec_push(spec, &sl, '.');
                if (st == STATUS_OK)
                    st = spec_push_int(spec, &sl, prec);
                if (st != STATUS_OK)
                    return st;
            }
        }
        else
        {
            st = spec_push(spec, &sl, '.');
            if (st != STATUS_OK)
                return st;
            while (*p >= '0' && *p <= '9')
            {
                st = spec_push(spec, &sl, *p++);
                if (st != STATUS_OK)
                    return st;
            }
        }
    }

    LengthMod lm = LEN_NONE;
    if (*p == 'h')
    {
        st = spec_push(spec, &sl, *p++);
        lm = LEN_H;
        if (st == STATUS_OK && *p == 'h')
        {
            st = spec_push(spec, &sl, *p++);
            lm = LEN_HH;
        }
    }
    else if (*p == 'l')
    {
        st = spec_push(spec, &sl, *p++);
        lm = LEN_L;
        if (st == STATUS_OK && *p == 'l')
        {
            st = spec_push(spec, &sl, *p++);
            lm = LEN_LL;
        }
    }
    else if (*p == 'j' || *p == 'z' || *p == 't' || *p == 'L')
    {
        lm = (*p == 'j') ? LEN_J : (*p == 'z') ? LEN_Z : (*p == 't') ? LEN_T : LEN_BIGL;
        st = spec_push(spec, &sl, *p++);
    }
    else
    {
        st = STATUS_OK;
    }
    if (st != STATUS_OK)
        return st;

    char conv = *p;
    if (conv == '\0')
        return STATUS_FORMAT_ERROR;
    p++;
    st = spec_push(spec, &sl, conv);
    if (st != STATUS_OK)
        return st;

    *pp = p;

    switch (conv)
    {
    case 'd':
    case 'i':
        switch (lm)
        {
        case LEN_L:
        {
            long v = va_arg(*ap, long);
            return buffer_printf(out, spec, v);
        }
        case LEN_LL:
        {
            long long v = va_arg(*ap, long long);
            return buffer_printf(out, spec, v);
        }
        case LEN_J:
        {
            intmax_t v = va_arg(*ap, intmax_t);
            return buffer_printf(out, spec, v);
        }
        case LEN_Z:
        case LEN_T:
        {
            ptrdiff_t v = va_arg(*ap, ptrdiff_t);
            return buffer_printf(out, spec, v);
        }
        default:
        {
            int v = va_arg(*ap, int);
            return buffer_printf(out, spec, v);
        }
        }
    case 'u':
    case 'o':
    case 'x':
    case 'X':
        switch (lm)
        {
        case LEN_L:
        {
            unsigned long v = va_arg(*ap, unsigned long);
            return buffer_printf(out, spec, v);
        }
        case LEN_LL:
        {
            unsigned long long v = va_arg(*ap, unsigned long long);
            return buffer_printf(out, spec, v);
        }
        case LEN_J:
        {
            uintmax_t v = va_arg(*ap, uintmax_t);
            return buffer_printf(out, spec, v);
        }
        case LEN_Z:
        case LEN_T:
        {
            size_t v = va_arg(*ap, size_t);
            return buffer_printf(out, spec, v);
        }
        default:
        {
            unsigned int v = va_arg(*ap, unsigned int);
            return buffer_printf(out, spec, v);
        }
        }
    case 'c':
    {
        int v = va_arg(*ap, int);
        return buffer_printf(out, spec, v);
    }
    case 's':
    {
        if (lm == LEN_L)
        {
            const wchar_t *v = va_arg(*ap, const wchar_t *);
            return buffer_printf(out, spec, v);
        }
        const char *v = va_arg(*ap, const char *);
        return buffer_printf(out, spec, v);
    }
    case 'p':
    {
        void *v = va_arg(*ap, void *);
        return buffer_printf(out, spec, v);
    }
    case 'e':
    case 'E':
    case 'f':
    case 'F':
    case 'g':
    case 'G':
    case 'a':
    case 'A':
        if (lm == LEN_BIGL)
        {
            long double v = va_arg(*ap, long double);
            return buffer_printf(out, spec, v);
        }
        else
        {
            double v = va_arg(*ap, double);
            return buffer_printf(out, spec, v);
        }
    default:
        return STATUS_FORMAT_ERROR;
    }
}

Status format_to_buffer(Buffer *out, const char *format, va_list *ap)
{
    if (out == NULL || format == NULL || ap == NULL)
        return STATUS_NULL_ARG;

    const char *p = format;
    while (*p != '\0')
    {
        Status st;
        if (*p != '%')
        {
            st = buffer_push(out, *p);
            p++;
            if (st != STATUS_OK)
                return st;
            continue;
        }

        p++;
        int handled = 0;
        st = try_custom(out, &p, ap, &handled);
        if (st != STATUS_OK)
            return st;
        if (handled)
            continue;

        st = handle_standard(out, &p, ap);
        if (st != STATUS_OK)
            return st;
    }

    return buffer_reserve(out, 0);
}

int overfprintf(FILE *stream, const char *format, ...)
{
    if (stream == NULL || format == NULL)
        return -1;

    Buffer buf;
    buffer_init(&buf);

    va_list ap;
    va_start(ap, format);
    Status st = format_to_buffer(&buf, format, &ap);
    va_end(ap);

    int result = -1;
    if (st == STATUS_OK && buf.len <= (size_t)(~0u >> 1))
    {
        if (fwrite(buf.data, 1, buf.len, stream) == buf.len)
            result = (int)buf.len;
    }

    buffer_free(&buf);
    return result;
}

int oversprintf(char *buffer, const char *format, ...)
{
    if (buffer == NULL || format == NULL)
        return -1;

    Buffer buf;
    buffer_init(&buf);

    va_list ap;
    va_start(ap, format);
    Status st = format_to_buffer(&buf, format, &ap);
    va_end(ap);

    int result = -1;
    if (st == STATUS_OK && buf.len <= (size_t)(~0u >> 1))
    {
        for (size_t i = 0; i < buf.len; i++)
            buffer[i] = buf.data[i];
        buffer[buf.len] = '\0';
        result = (int)buf.len;
    }

    buffer_free(&buf);
    return result;
}

int main(void)
{
    char text[512];
    int n;

    n = overfprintf(stdout, "standard: %d | %5.2f | %-6s| %c | %x | %%\n",
                    42, 3.14159, "ab", 'z', 255);
    printf("returned %d\n\n", n);

    overfprintf(stdout, "%%Ro: %Ro %Ro %Ro %Ro %Ro %Ro\n", 1, 4, 9, 14, 1994, 3999);
    overfprintf(stdout, "%%Ro: %Ro %Ro %Ro\n", 0, -42, 4000);

    overfprintf(stdout, "%%Zr: %Zr %Zr %Zr %Zr %Zr %Zr\n", 0u, 1u, 2u, 9u, 100u, 4294967295u);

    overfprintf(stdout, "%%Cv: %Cv %Cv %Cv %Cv\n", 255, 16, -255, 2, 100, 36, 77, 99);
    overfprintf(stdout, "%%CV: %CV %CV %CV\n", 255, 16, -255, 2, 100, 36);

    overfprintf(stdout, "%%to: %to %to %to\n", "ff", 16, "-101", 2, "zz", 36);
    overfprintf(stdout, "%%TO: %TO %TO %TO\n", "FF", 16, "-101", 2, "ZZ", 36);

    overfprintf(stdout, "%%mi: %mi\n", 1);
    overfprintf(stdout, "%%mi: %mi\n", -1);
    overfprintf(stdout, "%%mu: %mu\n", 4000000000u);
    overfprintf(stdout, "%%md: %md\n", 1.0);
    overfprintf(stdout, "%%mf: %mf\n", 1.5f);

    n = oversprintf(text, "[%Ro] [%Zr] [%Cv] [%to] [%mi] %s %d", 2024, 12u, 255, 16, "7f", 16, 5, "tail", -7);
    printf("oversprintf -> \"%s\"\nreturned %d\n", text, n);

    n = oversprintf(text, "%to", "12xz", 16);
    printf("invalid input for %%to -> returned %d\n", n);

    return 0;
}
