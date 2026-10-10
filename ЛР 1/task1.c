#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_MULTIPLE 100
#define TABLE_BASES 10
#define MAX_EXPONENT 10
#define HEX_BUFFER_SIZE 32

typedef enum {
    STATUS_OK,
    STATUS_INVALID_ARGUMENT,
    STATUS_INVALID_NUMBER,
    STATUS_INVALID_FLAG,
    STATUS_OUT_OF_RANGE,
    STATUS_OVERFLOW
} Status;

typedef enum {
    FLAG_MULTIPLES,
    FLAG_PRIME,
    FLAG_HEX_DIGITS,
    FLAG_POWER_TABLE,
    FLAG_SUM,
    FLAG_FACTORIAL
} Flag;

static Status parse_integer(const char *str, long long *result) {
    if (str == NULL || result == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    size_t i = 0;
    int is_negative = 0;
    if (str[i] == '-' || str[i] == '+') {
        is_negative = (str[i] == '-');
        i++;
    }
    if (str[i] == '\0') {
        return STATUS_INVALID_NUMBER;
    }

    const unsigned long long limit = (~0ULL >> 1) + (is_negative ? 1ULL : 0ULL);
    unsigned long long magnitude = 0;
    for (; str[i] != '\0'; i++) {
        if (str[i] < '0' || str[i] > '9') {
            return STATUS_INVALID_NUMBER;
        }
        unsigned long long digit = (unsigned long long)(str[i] - '0');
        if (magnitude > (limit - digit) / 10) {
            return STATUS_OVERFLOW;
        }
        magnitude = magnitude * 10 + digit;
    }

    if (!is_negative) {
        *result = (long long)magnitude;
    } else if (magnitude == 0) {
        *result = 0;
    } else {
        *result = -(long long)(magnitude - 1ULL) - 1;
    }
    return STATUS_OK;
}

static Status parse_flag(const char *str, Flag *flag) {
    if (str == NULL || flag == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    if ((str[0] != '-' && str[0] != '/') || str[1] == '\0' || str[2] != '\0') {
        return STATUS_INVALID_FLAG;
    }

    switch (str[1]) {
        case 'h': *flag = FLAG_MULTIPLES; break;
        case 'p': *flag = FLAG_PRIME; break;
        case 's': *flag = FLAG_HEX_DIGITS; break;
        case 'e': *flag = FLAG_POWER_TABLE; break;
        case 'a': *flag = FLAG_SUM; break;
        case 'f': *flag = FLAG_FACTORIAL; break;
        default: return STATUS_INVALID_FLAG;
    }
    return STATUS_OK;
}

static Status find_multiples(const long long x, long long multiples[], const int capacity, int *count) {
    if (multiples == NULL || count == NULL || capacity < MAX_MULTIPLE) {
        return STATUS_INVALID_ARGUMENT;
    }

    *count = 0;
    if (x == 0 || x > MAX_MULTIPLE || x < -MAX_MULTIPLE) {
        return STATUS_OK;
    }

    long long step = x > 0 ? x : -x;
    for (long long n = step; n <= MAX_MULTIPLE; n += step) {
        multiples[*count] = n;
        (*count)++;
    }
    return STATUS_OK;
}

static Status check_prime(const long long x, int *is_prime, int *is_composite) {
    if (is_prime == NULL || is_composite == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    if (x < 2) {
        *is_prime = 0;
        *is_composite = 0;
        return STATUS_OK;
    }

    int prime = 1;
    if (x % 2 == 0) {
        prime = (x == 2);
    } else {
        for (long long d = 3; d <= x / d; d += 2) {
            if (x % d == 0) {
                prime = 0;
                break;
            }
        }
    }

    *is_prime = prime;
    *is_composite = !prime;
    return STATUS_OK;
}

static Status to_hex_digits(const long long x, char digits[], const int capacity, int *count, int *is_negative) {
    if (digits == NULL || count == NULL || is_negative == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    const char alphabet[] = "0123456789ABCDEF";
    unsigned long long value = x < 0 ? 0ULL - (unsigned long long)x : (unsigned long long)x;

    char reversed[HEX_BUFFER_SIZE];
    int length = 0;
    do {
        reversed[length] = alphabet[value % 16];
        length++;
        value /= 16;
    } while (value > 0);

    if (length > capacity) {
        return STATUS_INVALID_ARGUMENT;
    }
    for (int i = 0; i < length; i++) {
        digits[i] = reversed[length - 1 - i];
    }

    *count = length;
    *is_negative = x < 0;
    return STATUS_OK;
}

static Status build_power_table(const long long x, long long table[TABLE_BASES][MAX_EXPONENT]) {
    if (table == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    if (x < 1 || x > MAX_EXPONENT) {
        return STATUS_OUT_OF_RANGE;
    }

    for (int base = 1; base <= TABLE_BASES; base++) {
        long long power = 1;
        for (int exponent = 1; exponent <= x; exponent++) {
            power *= base;
            table[base - 1][exponent - 1] = power;
        }
    }
    return STATUS_OK;
}

static Status sum_to(const long long x, unsigned long long *sum) {
    if (sum == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    if (x < 1) {
        return STATUS_OUT_OF_RANGE;
    }

    unsigned long long a = (unsigned long long)x;
    unsigned long long b = a + 1;
    if (a % 2 == 0) {
        a /= 2;
    } else {
        b /= 2;
    }

    if (a > ~0ULL / b) {
        return STATUS_OVERFLOW;
    }

    *sum = a * b;
    return STATUS_OK;
}

static Status factorial(const long long x, unsigned long long *result) {
    if (result == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    if (x < 0) {
        return STATUS_OUT_OF_RANGE;
    }

    unsigned long long value = 1;
    for (long long i = 2; i <= x; i++) {
        if (value > ~0ULL / (unsigned long long)i) {
            return STATUS_OVERFLOW;
        }
        value *= (unsigned long long)i;
    }

    *result = value;
    return STATUS_OK;
}

static void print_error(const Status status) {
    switch (status) {
        case STATUS_INVALID_ARGUMENT: printf("Error: invalid function argument\n"); break;
        case STATUS_INVALID_NUMBER: printf("Error: x must be an integer number\n"); break;
        case STATUS_INVALID_FLAG: printf("Error: unknown flag\n"); break;
        case STATUS_OUT_OF_RANGE: printf("Error: x is out of the allowed range for this flag\n"); break;
        case STATUS_OVERFLOW: printf("Error: number is too large (overflow)\n"); break;
        default: printf("Error: unknown error\n"); break;
    }
}

static void print_multiples(const long long x, const long long multiples[], const int count) {
    if (count == 0) {
        printf("There are no natural numbers up to %d that are multiples of %lld\n", MAX_MULTIPLE, x);
        return;
    }
    printf("Multiples of %lld up to %d:", x, MAX_MULTIPLE);
    for (int i = 0; i < count; i++) {
        printf(" %lld", multiples[i]);
    }
    printf("\n");
}

static void print_prime(const long long x, const int is_prime, const int is_composite) {
    if (!is_prime && !is_composite) {
        printf("%lld is neither prime nor composite\n", x);
    } else if (is_prime) {
        printf("%lld is prime\n", x);
    } else {
        printf("%lld is composite\n", x);
    }
}

static void print_hex_digits(const char digits[], const int count, const int is_negative) {
    if (is_negative) {
        printf("-");
    }
    for (int i = 0; i < count; i++) {
        if (i > 0) {
            printf(" ");
        }
        printf("%c", digits[i]);
    }
    printf("\n");
}

static void print_power_table(long long table[TABLE_BASES][MAX_EXPONENT], const int exponents) {
    printf("base");
    for (int exponent = 1; exponent <= exponents; exponent++) {
        printf(" %12d", exponent);
    }
    printf("\n");
    for (int base = 1; base <= TABLE_BASES; base++) {
        printf("%4d", base);
        for (int exponent = 1; exponent <= exponents; exponent++) {
            printf(" %12lld", table[base - 1][exponent - 1]);
        }
        printf("\n");
    }
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        printf("Error: wrong number of arguments\n");
        return 1;
    }

    long long x = 0;
    Status status = parse_integer(argv[1], &x);
    if (status != STATUS_OK) {
        print_error(status);
        return 1;
    }

    Flag flag;
    status = parse_flag(argv[2], &flag);
    if (status != STATUS_OK) {
        print_error(status);
        return 1;
    }

    switch (flag) {
        case FLAG_MULTIPLES: {
            long long multiples[MAX_MULTIPLE];
            int count = 0;
            status = find_multiples(x, multiples, MAX_MULTIPLE, &count);
            if (status == STATUS_OK) {
                print_multiples(x, multiples, count);
            }
            break;
        }
        case FLAG_PRIME: {
            int is_prime = 0;
            int is_composite = 0;
            status = check_prime(x, &is_prime, &is_composite);
            if (status == STATUS_OK) {
                print_prime(x, is_prime, is_composite);
            }
            break;
        }
        case FLAG_HEX_DIGITS: {
            char digits[HEX_BUFFER_SIZE];
            int count = 0;
            int is_negative = 0;
            status = to_hex_digits(x, digits, HEX_BUFFER_SIZE, &count, &is_negative);
            if (status == STATUS_OK) {
                print_hex_digits(digits, count, is_negative);
            }
            break;
        }
        case FLAG_POWER_TABLE: {
            long long table[TABLE_BASES][MAX_EXPONENT];
            status = build_power_table(x, table);
            if (status == STATUS_OK) {
                print_power_table(table, (int)x);
            }
            break;
        }
        case FLAG_SUM: {
            unsigned long long sum = 0;
            status = sum_to(x, &sum);
            if (status == STATUS_OK) {
                printf("Sum of natural numbers from 1 to %lld = %llu\n", x, sum);
            }
            break;
        }
        case FLAG_FACTORIAL: {
            unsigned long long result = 0;
            status = factorial(x, &result);
            if (status == STATUS_OK) {
                printf("%lld! = %llu\n", x, result);
            }
            break;
        }
    }

    if (status != STATUS_OK) {
        print_error(status);
        return 1;
    }
    return 0;
}
