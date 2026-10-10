#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INITIAL_TOKEN_CAPACITY 16
#define MIN_BASE 2
#define MAX_BASE 36

typedef enum {
    STATUS_OK,
    STATUS_INVALID_ARGUMENT,
    STATUS_INVALID_NUMBER,
    STATUS_NO_MEMORY,
    STATUS_FILE_ERROR,
    STATUS_END_OF_FILE
} Status;

typedef struct {
    int is_negative;
    char *digits;
    int base;
    char *decimal;
} NumberInfo;

static int is_separator(const int c) {
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

static int digit_value(const char c) {
    if (c >= '0' && c <= '9') {
        return c - '0';
    }
    if (c >= 'A' && c <= 'Z') {
        return c - 'A' + 10;
    }
    if (c >= 'a' && c <= 'z') {
        return c - 'a' + 10;
    }
    return -1;
}

static Status read_token(FILE *file, char **token) {
    if (file == NULL || token == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    int c = fgetc(file);
    while (c != EOF && is_separator(c)) {
        c = fgetc(file);
    }
    if (c == EOF) {
        return ferror(file) ? STATUS_FILE_ERROR : STATUS_END_OF_FILE;
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
        c = fgetc(file);
    }
    if (ferror(file)) {
        free(buffer);
        return STATUS_FILE_ERROR;
    }
    buffer[length] = '\0';
    *token = buffer;
    return STATUS_OK;
}

static Status to_decimal(const char *digits, const int base, const int is_negative, char **result) {
    if (digits == NULL || result == NULL || base < MIN_BASE || base > MAX_BASE) {
        return STATUS_INVALID_ARGUMENT;
    }

    size_t length = strlen(digits);
    size_t capacity = 2 * length + 1;
    unsigned char *decimal = calloc(capacity, 1);
    if (decimal == NULL) {
        return STATUS_NO_MEMORY;
    }
    size_t used = 1;

    for (size_t i = 0; i < length; i++) {
        int carry = digit_value(digits[i]);
        for (size_t j = 0; j < used; j++) {
            int value = decimal[j] * base + carry;
            decimal[j] = (unsigned char)(value % 10);
            carry = value / 10;
        }
        while (carry > 0 && used < capacity) {
            decimal[used] = (unsigned char)(carry % 10);
            used++;
            carry /= 10;
        }
    }

    char *text = malloc(used + 2);
    if (text == NULL) {
        free(decimal);
        return STATUS_NO_MEMORY;
    }
    size_t position = 0;
    if (is_negative) {
        text[position] = '-';
        position++;
    }
    for (size_t j = used; j > 0; j--) {
        text[position] = (char)('0' + decimal[j - 1]);
        position++;
    }
    text[position] = '\0';

    free(decimal);
    *result = text;
    return STATUS_OK;
}

static Status analyze_number(const char *token, NumberInfo *info) {
    if (token == NULL || info == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    const char *p = token;
    int is_negative = 0;
    if (*p == '-') {
        is_negative = 1;
        p++;
    }
    if (*p == '\0') {
        return STATUS_INVALID_NUMBER;
    }

    int max_digit = 0;
    for (const char *q = p; *q != '\0'; q++) {
        int digit = digit_value(*q);
        if (digit < 0) {
            return STATUS_INVALID_NUMBER;
        }
        if (digit > max_digit) {
            max_digit = digit;
        }
    }

    while (*p == '0' && *(p + 1) != '\0') {
        p++;
    }
    if (*p == '0') {
        is_negative = 0;
    }

    size_t length = strlen(p);
    char *digits = malloc(length + 1);
    if (digits == NULL) {
        return STATUS_NO_MEMORY;
    }
    for (size_t i = 0; i <= length; i++) {
        digits[i] = (p[i] >= 'a' && p[i] <= 'z') ? (char)(p[i] - 'a' + 'A') : p[i];
    }

    int base = max_digit + 1 < MIN_BASE ? MIN_BASE : max_digit + 1;
    char *decimal = NULL;
    Status status = to_decimal(digits, base, is_negative, &decimal);
    if (status != STATUS_OK) {
        free(digits);
        return status;
    }

    info->is_negative = is_negative;
    info->digits = digits;
    info->base = base;
    info->decimal = decimal;
    return STATUS_OK;
}

static Status process_file(FILE *input, FILE *output, long long *number_index) {
    if (input == NULL || output == NULL || number_index == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    *number_index = 0;
    Status status = STATUS_OK;

    while (status == STATUS_OK) {
        char *token = NULL;
        status = read_token(input, &token);
        if (status != STATUS_OK) {
            break;
        }
        (*number_index)++;

        NumberInfo info;
        status = analyze_number(token, &info);
        free(token);
        if (status != STATUS_OK) {
            break;
        }

        fprintf(output, "%s%s %d %s\n", info.is_negative ? "-" : "", info.digits, info.base, info.decimal);
        free(info.digits);
        free(info.decimal);
        if (ferror(output)) {
            status = STATUS_FILE_ERROR;
        }
    }
    return status == STATUS_END_OF_FILE ? STATUS_OK : status;
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        printf("Error: wrong number of arguments\n");
        return 1;
    }
    if (strcmp(argv[1], argv[2]) == 0) {
        printf("Error: input and output files must be different\n");
        return 1;
    }

    FILE *input = fopen(argv[1], "r");
    if (input == NULL) {
        printf("Error: cannot open input file\n");
        return 1;
    }
    FILE *output = fopen(argv[2], "w");
    if (output == NULL) {
        printf("Error: cannot open output file\n");
        fclose(input);
        return 1;
    }

    long long number_index = 0;
    Status status = process_file(input, output, &number_index);
    fclose(input);
    if (fclose(output) != 0 && status == STATUS_OK) {
        status = STATUS_FILE_ERROR;
    }

    switch (status) {
        case STATUS_OK:
            printf("Done\n");
            return 0;
        case STATUS_INVALID_NUMBER:
            printf("Error: number #%lld is not a valid number in bases 2..36\n", number_index);
            return 1;
        case STATUS_NO_MEMORY:
            printf("Error: not enough memory\n");
            return 1;
        case STATUS_FILE_ERROR:
            printf("Error: file read/write error\n");
            return 1;
        default:
            printf("Error: unknown error\n");
            return 1;
    }
}
