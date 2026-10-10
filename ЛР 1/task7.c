#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INITIAL_TOKEN_CAPACITY 16
#define MAX_CODE_DIGITS 8

typedef enum {
    STATUS_OK,
    STATUS_INVALID_ARGUMENT,
    STATUS_NO_MEMORY,
    STATUS_FILE_ERROR,
    STATUS_END_OF_FILE
} Status;

static int is_separator(const int c) {
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
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

static Status write_token(FILE *output, const char *token, int *is_first) {
    if (output == NULL || token == NULL || is_first == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    if (!*is_first) {
        fputc(' ', output);
    }
    fputs(token, output);
    *is_first = 0;
    return ferror(output) ? STATUS_FILE_ERROR : STATUS_OK;
}

static Status copy_one_token(FILE *file, FILE *output, int *is_first, int *finished) {
    char *token = NULL;
    Status status = read_token(file, &token);
    if (status == STATUS_END_OF_FILE) {
        *finished = 1;
        return STATUS_OK;
    }
    if (status != STATUS_OK) {
        return status;
    }
    status = write_token(output, token, is_first);
    free(token);
    return status;
}

static Status merge_files(FILE *first, FILE *second, FILE *output) {
    if (first == NULL || second == NULL || output == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    int is_first = 1;
    int first_finished = 0;
    int second_finished = 0;
    Status status = STATUS_OK;

    while (status == STATUS_OK && (!first_finished || !second_finished)) {
        if (!first_finished) {
            status = copy_one_token(first, output, &is_first, &first_finished);
        }
        if (status == STATUS_OK && !second_finished) {
            status = copy_one_token(second, output, &is_first, &second_finished);
        }
    }
    return status;
}

static Status to_lowercase(char *token) {
    if (token == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    for (size_t i = 0; token[i] != '\0'; i++) {
        if (token[i] >= 'A' && token[i] <= 'Z') {
            token[i] = (char)(token[i] - 'A' + 'a');
        }
    }
    return STATUS_OK;
}

static Status to_codes(const char *token, const int base, char **result) {
    if (token == NULL || result == NULL || base < 2 || base > 10) {
        return STATUS_INVALID_ARGUMENT;
    }
    size_t length = strlen(token);
    char *codes = malloc(length * MAX_CODE_DIGITS + 1);
    if (codes == NULL) {
        return STATUS_NO_MEMORY;
    }

    size_t position = 0;
    for (size_t i = 0; i < length; i++) {
        unsigned int code = (unsigned char)token[i];
        char digits[MAX_CODE_DIGITS];
        int count = 0;
        do {
            digits[count] = (char)('0' + code % (unsigned int)base);
            count++;
            code /= (unsigned int)base;
        } while (code > 0);
        while (count > 0) {
            count--;
            codes[position] = digits[count];
            position++;
        }
    }
    codes[position] = '\0';
    *result = codes;
    return STATUS_OK;
}

static Status copy_string(const char *source, char **result) {
    if (source == NULL || result == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    size_t length = strlen(source);
    char *copy = malloc(length + 1);
    if (copy == NULL) {
        return STATUS_NO_MEMORY;
    }
    memcpy(copy, source, length + 1);
    *result = copy;
    return STATUS_OK;
}

static Status transform_token(const char *token, const long long index, char **result) {
    if (token == NULL || result == NULL || index < 1) {
        return STATUS_INVALID_ARGUMENT;
    }

    char *copy = NULL;
    Status status = copy_string(token, &copy);
    if (status != STATUS_OK) {
        return status;
    }

    if (index % 10 == 0) {
        to_lowercase(copy);
        status = to_codes(copy, 4, result);
        free(copy);
    } else if (index % 2 == 0) {
        to_lowercase(copy);
        *result = copy;
    } else if (index % 5 == 0) {
        status = to_codes(copy, 8, result);
        free(copy);
    } else {
        *result = copy;
    }
    return status;
}

static Status transform_file(FILE *input, FILE *output) {
    if (input == NULL || output == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    int is_first = 1;
    long long index = 0;
    Status status = STATUS_OK;

    while (status == STATUS_OK) {
        char *token = NULL;
        status = read_token(input, &token);
        if (status != STATUS_OK) {
            break;
        }
        index++;
        char *converted = NULL;
        status = transform_token(token, index, &converted);
        free(token);
        if (status == STATUS_OK) {
            status = write_token(output, converted, &is_first);
            free(converted);
        }
    }
    return status == STATUS_END_OF_FILE ? STATUS_OK : status;
}

static void print_error(const Status status) {
    switch (status) {
        case STATUS_INVALID_ARGUMENT: printf("Error: invalid argument\n"); break;
        case STATUS_NO_MEMORY: printf("Error: not enough memory\n"); break;
        case STATUS_FILE_ERROR: printf("Error: file read/write error\n"); break;
        default: printf("Error: unknown error\n"); break;
    }
}

static Status close_file(FILE *file) {
    if (file != NULL && fclose(file) != 0) {
        return STATUS_FILE_ERROR;
    }
    return STATUS_OK;
}

static int run_merge(char *argv[]) {
    const char *first_path = argv[2];
    const char *second_path = argv[3];
    const char *output_path = argv[4];
    if (strcmp(output_path, first_path) == 0 || strcmp(output_path, second_path) == 0) {
        printf("Error: output file must differ from input files\n");
        return 1;
    }

    FILE *first = fopen(first_path, "r");
    if (first == NULL) {
        printf("Error: cannot open %s\n", first_path);
        return 1;
    }
    FILE *second = fopen(second_path, "r");
    if (second == NULL) {
        printf("Error: cannot open %s\n", second_path);
        fclose(first);
        return 1;
    }
    FILE *output = fopen(output_path, "w");
    if (output == NULL) {
        printf("Error: cannot open %s\n", output_path);
        fclose(first);
        fclose(second);
        return 1;
    }

    Status status = merge_files(first, second, output);
    fclose(first);
    fclose(second);
    if (close_file(output) != STATUS_OK && status == STATUS_OK) {
        status = STATUS_FILE_ERROR;
    }

    if (status != STATUS_OK) {
        print_error(status);
        return 1;
    }
    printf("Done\n");
    return 0;
}

static int run_transform(char *argv[]) {
    const char *input_path = argv[2];
    const char *output_path = argv[3];
    if (strcmp(input_path, output_path) == 0) {
        printf("Error: output file must differ from input file\n");
        return 1;
    }

    FILE *input = fopen(input_path, "r");
    if (input == NULL) {
        printf("Error: cannot open %s\n", input_path);
        return 1;
    }
    FILE *output = fopen(output_path, "w");
    if (output == NULL) {
        printf("Error: cannot open %s\n", output_path);
        fclose(input);
        return 1;
    }

    Status status = transform_file(input, output);
    fclose(input);
    if (close_file(output) != STATUS_OK && status == STATUS_OK) {
        status = STATUS_FILE_ERROR;
    }

    if (status != STATUS_OK) {
        print_error(status);
        return 1;
    }
    printf("Done\n");
    return 0;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Error: wrong number of arguments\n");
        return 1;
    }
    const char *flag = argv[1];
    if ((flag[0] != '-' && flag[0] != '/') || flag[1] == '\0' || flag[2] != '\0') {
        printf("Error: invalid flag\n");
        return 1;
    }

    switch (flag[1]) {
        case 'r':
            if (argc != 5) {
                printf("Error: flag -r needs exactly 3 paths: file1 file2 output\n");
                return 1;
            }
            return run_merge(argv);
        case 'a':
            if (argc != 4) {
                printf("Error: flag -a needs exactly 2 paths: input output\n");
                return 1;
            }
            return run_transform(argv);
        default:
            printf("Error: unknown flag\n");
            return 1;
    }
}
