#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIN_BASE 2
#define MAX_BASE 36
#define INITIAL_TOKEN_CAPACITY 16
#define NUMBER_BUFFER_SIZE 70
#define OUTPUT_BASES_COUNT 4

typedef enum {
    STATUS_OK,
    STATUS_INVALID_ARGUMENT,
    STATUS_INVALID_NUMBER,
    STATUS_OVERFLOW,
    STATUS_NO_MEMORY,
    STATUS_END_OF_FILE
} Status;

typedef struct {
    long long count;
    long long max_abs;
    long long sum;
} Statistics;

static long long max_long_long(void) {
    return (long long)(~0ULL >> 1);
}

static int is_separator(const int c) {
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

static Status read_token(FILE *stream, char **token) {
    if (stream == NULL || token == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    int c = fgetc(stream);
    while (c != EOF && is_separator(c)) {
        c = fgetc(stream);
    }
    if (c == EOF) {
        return STATUS_END_OF_FILE;
    }

    size_t capacity = INITIAL_TOKEN_CAPACITY;
    size_t length = 0;
    char *buffer = malloc(capacity);
    if (buffer == NULL) {
        return STATUS_NO_MEMORY;
    }
    while (c != EOF && !is_separator(c)) {
        if (length + 1 >= capacity) {
            capacity *= 2;
            char *bigger = realloc(buffer, capacity);
            if (bigger == NULL) {
                free(buffer);
                return STATUS_NO_MEMORY;
            }
            buffer = bigger;
        }
        buffer[length] = (char)c;
        length++;
        c = fgetc(stream);
    }
    buffer[length] = '\0';
    *token = buffer;
    return STATUS_OK;
}

static int digit_value(const char c) {
    if (c >= '0' && c <= '9') {
        return c - '0';
    }
    if (c >= 'A' && c <= 'Z') {
        return c - 'A' + 10;
    }
    return -1;
}

static Status parse_base(const char *str, int *base) {
    if (str == NULL || base == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    int value = 0;
    size_t i = 0;
    for (; str[i] != '\0'; i++) {
        if (str[i] < '0' || str[i] > '9' || i >= 2) {
            return STATUS_INVALID_NUMBER;
        }
        value = value * 10 + (str[i] - '0');
    }
    if (i == 0 || value < MIN_BASE || value > MAX_BASE) {
        return STATUS_INVALID_NUMBER;
    }
    *base = value;
    return STATUS_OK;
}

static Status parse_in_base(const char *str, const int base, long long *result) {
    if (str == NULL || result == NULL || base < MIN_BASE || base > MAX_BASE) {
        return STATUS_INVALID_ARGUMENT;
    }
    int is_negative = 0;
    if (*str == '-' || *str == '+') {
        is_negative = (*str == '-');
        str++;
    }
    if (*str == '\0') {
        return STATUS_INVALID_NUMBER;
    }

    const long long limit = max_long_long();
    long long magnitude = 0;
    for (; *str != '\0'; str++) {
        int digit = digit_value(*str);
        if (digit < 0 || digit >= base) {
            return STATUS_INVALID_NUMBER;
        }
        if (magnitude > (limit - digit) / base) {
            return STATUS_OVERFLOW;
        }
        magnitude = magnitude * base + digit;
    }
    *result = is_negative ? -magnitude : magnitude;
    return STATUS_OK;
}

static Status add_number(Statistics *stats, const long long value) {
    if (stats == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    const long long limit = max_long_long();
    if ((value > 0 && stats->sum > limit - value) || (value < 0 && stats->sum < -limit - value)) {
        return STATUS_OVERFLOW;
    }
    stats->sum += value;

    long long abs_value = value < 0 ? -value : value;
    long long abs_max = stats->max_abs < 0 ? -stats->max_abs : stats->max_abs;
    if (stats->count == 0 || abs_value > abs_max) {
        stats->max_abs = value;
    }
    stats->count++;
    return STATUS_OK;
}

static Status to_base(const long long value, const int base, char buffer[], const size_t size) {
    if (buffer == NULL || size < NUMBER_BUFFER_SIZE || base < MIN_BASE || base > MAX_BASE) {
        return STATUS_INVALID_ARGUMENT;
    }
    const char alphabet[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    unsigned long long magnitude = value < 0 ? 0ULL - (unsigned long long)value : (unsigned long long)value;

    char reversed[NUMBER_BUFFER_SIZE];
    size_t length = 0;
    do {
        reversed[length] = alphabet[magnitude % (unsigned long long)base];
        length++;
        magnitude /= (unsigned long long)base;
    } while (magnitude > 0);

    size_t position = 0;
    if (value < 0) {
        buffer[position] = '-';
        position++;
    }
    while (length > 0) {
        length--;
        buffer[position] = reversed[length];
        position++;
    }
    buffer[position] = '\0';
    return STATUS_OK;
}

static void print_number(const char *title, const long long value, const int input_base) {
    const int bases[OUTPUT_BASES_COUNT] = {9, 18, 27, 36};
    char buffer[NUMBER_BUFFER_SIZE];

    to_base(value, input_base, buffer, sizeof(buffer));
    printf("%s: %s (base %d)\n", title, buffer, input_base);
    for (int i = 0; i < OUTPUT_BASES_COUNT; i++) {
        to_base(value, bases[i], buffer, sizeof(buffer));
        printf("  base %2d: %s\n", bases[i], buffer);
    }
}

int main(void) {
    printf("Enter the base (2..36): ");
    char *token = NULL;
    Status status = read_token(stdin, &token);
    if (status != STATUS_OK) {
        printf("Error: base was not entered\n");
        return 1;
    }
    int base = 0;
    status = parse_base(token, &base);
    free(token);
    if (status != STATUS_OK) {
        printf("Error: base must be an integer from 2 to 36\n");
        return 1;
    }

    printf("Enter numbers in base %d (digits above 9 as capital letters), finish with Stop:\n", base);
    Statistics stats = {0, 0, 0};
    while (1) {
        status = read_token(stdin, &token);
        if (status == STATUS_END_OF_FILE) {
            break;
        }
        if (status != STATUS_OK) {
            printf("Error: not enough memory\n");
            return 1;
        }
        if (strcmp(token, "Stop") == 0) {
            free(token);
            break;
        }

        long long value = 0;
        status = parse_in_base(token, base, &value);
        if (status == STATUS_OK) {
            status = add_number(&stats, value);
            if (status == STATUS_OVERFLOW) {
                printf("Error: the sum is too large (overflow)\n");
                free(token);
                return 1;
            }
        } else if (status == STATUS_OVERFLOW) {
            printf("Number %s is too large, ignored\n", token);
        } else {
            printf("Invalid number %s for base %d, ignored\n", token, base);
        }
        free(token);
    }

    if (stats.count == 0) {
        printf("No numbers were entered\n");
        return 0;
    }
    print_number("Max by absolute value", stats.max_abs, base);
    print_number("Sum", stats.sum, base);
    return 0;
}
