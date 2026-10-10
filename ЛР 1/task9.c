#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define ARRAY_SIZE 20
#define MIN_VALUE (-1000000000L)
#define MAX_VALUE 1000000000L
#define MIN_SIZE 10
#define MAX_SIZE 10000
#define MIN_ELEMENT (-1000)
#define MAX_ELEMENT 1000
#define PRINT_LIMIT 15

typedef enum {
    STATUS_OK,
    STATUS_INVALID_ARGUMENT,
    STATUS_INVALID_NUMBER,
    STATUS_OUT_OF_RANGE,
    STATUS_NO_MEMORY
} Status;

static Status parse_integer(const char *str, long *result) {
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

    unsigned long long magnitude = 0;
    for (; str[i] != '\0'; i++) {
        if (str[i] < '0' || str[i] > '9') {
            return STATUS_INVALID_NUMBER;
        }
        unsigned long long digit = (unsigned long long)(str[i] - '0');
        if (magnitude > ((unsigned long long)MAX_VALUE - digit) / 10) {
            return STATUS_OUT_OF_RANGE;
        }
        magnitude = magnitude * 10 + digit;
    }

    *result = is_negative ? -(long)magnitude : (long)magnitude;
    return STATUS_OK;
}

static Status random_in_range(const long a, const long b, int *result) {
    if (result == NULL || a > b) {
        return STATUS_INVALID_ARGUMENT;
    }
    unsigned long long random_bits = 0;
    for (int i = 0; i < 4; i++) {
        random_bits = random_bits * ((unsigned long long)RAND_MAX + 1ULL) + (unsigned long long)rand();
    }
    unsigned long long range = (unsigned long long)((long long)b - (long long)a + 1);
    *result = (int)((long long)a + (long long)(random_bits % range));
    return STATUS_OK;
}

static Status fill_random(int array[], const int size, const long a, const long b) {
    if (array == NULL || size <= 0) {
        return STATUS_INVALID_ARGUMENT;
    }
    for (int i = 0; i < size; i++) {
        Status status = random_in_range(a, b, &array[i]);
        if (status != STATUS_OK) {
            return status;
        }
    }
    return STATUS_OK;
}

static Status swap_min_max(int array[], const int size, int *min_index, int *max_index) {
    if (array == NULL || size <= 0 || min_index == NULL || max_index == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    int min_i = 0;
    int max_i = 0;
    for (int i = 1; i < size; i++) {
        if (array[i] < array[min_i]) {
            min_i = i;
        }
        if (array[i] > array[max_i]) {
            max_i = i;
        }
    }
    int temp = array[min_i];
    array[min_i] = array[max_i];
    array[max_i] = temp;

    *min_index = min_i;
    *max_index = max_i;
    return STATUS_OK;
}

static void print_array(const char *title, const int array[], const int size) {
    printf("%s:", title);
    for (int i = 0; i < size; i++) {
        printf(" %d", array[i]);
    }
    printf("\n");
}

static Status run_swap_part(const long a, const long b) {
    int array[ARRAY_SIZE];
    Status status = fill_random(array, ARRAY_SIZE, a, b);
    if (status != STATUS_OK) {
        return status;
    }
    print_array("Array ", array, ARRAY_SIZE);

    int min_index = 0;
    int max_index = 0;
    status = swap_min_max(array, ARRAY_SIZE, &min_index, &max_index);
    if (status != STATUS_OK) {
        return status;
    }
    printf("Min is at index %d, max is at index %d\n", min_index, max_index);
    print_array("Result", array, ARRAY_SIZE);
    return STATUS_OK;
}

static Status create_random_array(int **array, int *size) {
    if (array == NULL || size == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    int length = 0;
    Status status = random_in_range(MIN_SIZE, MAX_SIZE, &length);
    if (status != STATUS_OK) {
        return status;
    }
    int *data = malloc(sizeof(int) * (size_t)length);
    if (data == NULL) {
        return STATUS_NO_MEMORY;
    }
    for (int i = 0; i < length; i++) {
        random_in_range(MIN_ELEMENT, MAX_ELEMENT, &data[i]);
    }
    *array = data;
    *size = length;
    return STATUS_OK;
}

static int compare_ints(const void *left, const void *right) {
    int a = *(const int *)left;
    int b = *(const int *)right;
    return (a > b) - (a < b);
}

static Status nearest_in_sorted(const int sorted[], const int size, const int value, int *nearest) {
    if (sorted == NULL || size <= 0 || nearest == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    int left = 0;
    int right = size;
    while (left < right) {
        int middle = left + (right - left) / 2;
        if (sorted[middle] < value) {
            left = middle + 1;
        } else {
            right = middle;
        }
    }
    if (left == size) {
        *nearest = sorted[size - 1];
    } else if (left == 0) {
        *nearest = sorted[0];
    } else if (sorted[left] - value < value - sorted[left - 1]) {
        *nearest = sorted[left];
    } else {
        *nearest = sorted[left - 1];
    }
    return STATUS_OK;
}

static Status build_c(const int a[], const int size_a, const int b[], const int size_b, int **c) {
    if (a == NULL || b == NULL || c == NULL || size_a <= 0 || size_b <= 0) {
        return STATUS_INVALID_ARGUMENT;
    }
    int *sorted_b = malloc(sizeof(int) * (size_t)size_b);
    if (sorted_b == NULL) {
        return STATUS_NO_MEMORY;
    }
    memcpy(sorted_b, b, sizeof(int) * (size_t)size_b);
    qsort(sorted_b, (size_t)size_b, sizeof(int), compare_ints);

    int *result = malloc(sizeof(int) * (size_t)size_a);
    if (result == NULL) {
        free(sorted_b);
        return STATUS_NO_MEMORY;
    }
    for (int i = 0; i < size_a; i++) {
        int nearest = 0;
        nearest_in_sorted(sorted_b, size_b, a[i], &nearest);
        result[i] = a[i] + nearest;
    }

    free(sorted_b);
    *c = result;
    return STATUS_OK;
}

static void print_first(const char *title, const int array[], const int size) {
    printf("%s (size %d):", title, size);
    int count = size < PRINT_LIMIT ? size : PRINT_LIMIT;
    for (int i = 0; i < count; i++) {
        printf(" %d", array[i]);
    }
    printf(size > PRINT_LIMIT ? " ...\n" : "\n");
}

static Status run_nearest_part(void) {
    int *a = NULL;
    int *b = NULL;
    int *c = NULL;
    int size_a = 0;
    int size_b = 0;

    Status status = create_random_array(&a, &size_a);
    if (status == STATUS_OK) {
        status = create_random_array(&b, &size_b);
    }
    if (status == STATUS_OK) {
        status = build_c(a, size_a, b, size_b, &c);
    }
    if (status == STATUS_OK) {
        print_first("A", a, size_a);
        print_first("B", b, size_b);
        print_first("C", c, size_a);
    }

    free(a);
    free(b);
    free(c);
    return status;
}

static void print_error(const Status status) {
    switch (status) {
        case STATUS_NO_MEMORY: printf("Error: not enough memory\n"); break;
        case STATUS_INVALID_ARGUMENT: printf("Error: invalid argument\n"); break;
        default: printf("Error: unknown error\n"); break;
    }
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        printf("Error: wrong number of arguments\n");
        return 1;
    }

    long a = 0;
    long b = 0;
    Status status = parse_integer(argv[1], &a);
    if (status == STATUS_OK) {
        status = parse_integer(argv[2], &b);
    }
    if (status == STATUS_INVALID_NUMBER) {
        printf("Error: a and b must be integers\n");
        return 1;
    }
    if (status == STATUS_OUT_OF_RANGE) {
        printf("Error: a and b must be from %ld to %ld\n", MIN_VALUE, MAX_VALUE);
        return 1;
    }
    if (a > b) {
        printf("Error: a must not be greater than b\n");
        return 1;
    }

    srand((unsigned int)time(NULL));

    printf("9.1 Array of %d numbers from [%ld, %ld]: swap min and max\n", ARRAY_SIZE, a, b);
    status = run_swap_part(a, b);
    if (status != STATUS_OK) {
        print_error(status);
        return 1;
    }

    printf("\n9.2 Arrays A and B, array C[i] = A[i] + nearest element of B\n");
    status = run_nearest_part();
    if (status != STATUS_OK) {
        print_error(status);
        return 1;
    }
    return 0;
}
