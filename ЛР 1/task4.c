#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define OUTPUT_PREFIX "out_"
#define INITIAL_LINE_CAPACITY 64

typedef enum {
    STATUS_OK,
    STATUS_INVALID_ARGUMENT,
    STATUS_INVALID_FLAG,
    STATUS_INVALID_PATH,
    STATUS_NO_MEMORY,
    STATUS_FILE_ERROR,
    STATUS_END_OF_FILE
} Status;

typedef enum {
    ACTION_REMOVE_DIGITS,
    ACTION_COUNT_LETTERS,
    ACTION_COUNT_SPECIAL,
    ACTION_HEX_CODES
} Action;

static int is_latin_letter(const int c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

static int is_digit(const int c) {
    return c >= '0' && c <= '9';
}

static Status parse_flag(const char *flag, Action *action, int *has_n) {
    if (flag == NULL || action == NULL || has_n == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    if (flag[0] != '-' && flag[0] != '/') {
        return STATUS_INVALID_FLAG;
    }

    const char *letter = flag + 1;
    *has_n = 0;
    if (*letter == 'n') {
        *has_n = 1;
        letter++;
    }
    if (letter[0] == '\0' || letter[1] != '\0') {
        return STATUS_INVALID_FLAG;
    }

    switch (letter[0]) {
        case 'd': *action = ACTION_REMOVE_DIGITS; break;
        case 'i': *action = ACTION_COUNT_LETTERS; break;
        case 's': *action = ACTION_COUNT_SPECIAL; break;
        case 'a': *action = ACTION_HEX_CODES; break;
        default: return STATUS_INVALID_FLAG;
    }
    return STATUS_OK;
}

static Status make_output_path(const char *input_path, char **output_path) {
    if (input_path == NULL || output_path == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    const char *name = input_path;
    for (const char *p = input_path; *p != '\0'; p++) {
        if (*p == '/' || *p == '\\') {
            name = p + 1;
        }
    }
    if (*name == '\0') {
        return STATUS_INVALID_PATH;
    }

    size_t directory_length = (size_t)(name - input_path);
    size_t prefix_length = strlen(OUTPUT_PREFIX);
    size_t name_length = strlen(name);

    char *path = malloc(directory_length + prefix_length + name_length + 1);
    if (path == NULL) {
        return STATUS_NO_MEMORY;
    }
    memcpy(path, input_path, directory_length);
    memcpy(path + directory_length, OUTPUT_PREFIX, prefix_length);
    memcpy(path + directory_length + prefix_length, name, name_length + 1);

    *output_path = path;
    return STATUS_OK;
}

static Status read_line(FILE *file, char **line, int *has_newline) {
    if (file == NULL || line == NULL || has_newline == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    size_t capacity = INITIAL_LINE_CAPACITY;
    size_t length = 0;
    char *buffer = malloc(capacity);
    if (buffer == NULL) {
        return STATUS_NO_MEMORY;
    }

    *has_newline = 0;
    int c;
    while ((c = fgetc(file)) != EOF) {
        if (c == '\n') {
            *has_newline = 1;
            break;
        }
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
    }

    if (ferror(file)) {
        free(buffer);
        return STATUS_FILE_ERROR;
    }
    if (length == 0 && !*has_newline) {
        free(buffer);
        return STATUS_END_OF_FILE;
    }

    buffer[length] = '\0';
    *line = buffer;
    return STATUS_OK;
}

static Status remove_digits(char *line) {
    if (line == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    size_t write = 0;
    for (size_t read = 0; line[read] != '\0'; read++) {
        if (!is_digit((unsigned char)line[read])) {
            line[write] = line[read];
            write++;
        }
    }
    line[write] = '\0';
    return STATUS_OK;
}

static Status count_letters(const char *line, size_t *count) {
    if (line == NULL || count == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    *count = 0;
    for (size_t i = 0; line[i] != '\0'; i++) {
        if (is_latin_letter((unsigned char)line[i])) {
            (*count)++;
        }
    }
    return STATUS_OK;
}

static Status count_special(const char *line, size_t *count) {
    if (line == NULL || count == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    *count = 0;
    for (size_t i = 0; line[i] != '\0'; i++) {
        unsigned char c = (unsigned char)line[i];
        if (!is_latin_letter(c) && !is_digit(c) && c != ' ') {
            (*count)++;
        }
    }
    return STATUS_OK;
}

static Status to_hex_codes(const char *line, const int has_newline, char **result) {
    if (line == NULL || result == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    size_t length = strlen(line);
    char *converted = malloc(2 * length + 3);
    if (converted == NULL) {
        return STATUS_NO_MEMORY;
    }

    size_t position = 0;
    for (size_t i = 0; i < length; i++) {
        unsigned char c = (unsigned char)line[i];
        if (is_digit(c)) {
            converted[position] = (char)c;
            position++;
        } else {
            position += (size_t)sprintf(converted + position, "%X", c);
        }
    }
    if (has_newline) {
        position += (size_t)sprintf(converted + position, "%X", '\n');
    }
    converted[position] = '\0';

    *result = converted;
    return STATUS_OK;
}

static Status process_file(FILE *input, FILE *output, const Action action) {
    if (input == NULL || output == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    Status status = STATUS_OK;
    while (status == STATUS_OK) {
        char *line = NULL;
        int has_newline = 0;
        status = read_line(input, &line, &has_newline);
        if (status != STATUS_OK) {
            break;
        }

        size_t count = 0;
        char *converted = NULL;
        switch (action) {
            case ACTION_REMOVE_DIGITS:
                status = remove_digits(line);
                if (status == STATUS_OK) {
                    fprintf(output, "%s%s", line, has_newline ? "\n" : "");
                }
                break;
            case ACTION_COUNT_LETTERS:
                status = count_letters(line, &count);
                if (status == STATUS_OK) {
                    fprintf(output, "%lu\n", (unsigned long)count);
                }
                break;
            case ACTION_COUNT_SPECIAL:
                status = count_special(line, &count);
                if (status == STATUS_OK) {
                    fprintf(output, "%lu\n", (unsigned long)count);
                }
                break;
            case ACTION_HEX_CODES:
                status = to_hex_codes(line, has_newline, &converted);
                if (status == STATUS_OK) {
                    fprintf(output, "%s", converted);
                    free(converted);
                }
                break;
        }
        free(line);

        if (status == STATUS_OK && ferror(output)) {
            status = STATUS_FILE_ERROR;
        }
    }

    return status == STATUS_END_OF_FILE ? STATUS_OK : status;
}

static void print_error(const Status status) {
    switch (status) {
        case STATUS_INVALID_ARGUMENT: printf("Error: invalid argument\n"); break;
        case STATUS_INVALID_FLAG: printf("Error: invalid flag\n"); break;
        case STATUS_INVALID_PATH: printf("Error: input path does not contain a file name\n"); break;
        case STATUS_NO_MEMORY: printf("Error: not enough memory\n"); break;
        case STATUS_FILE_ERROR: printf("Error: file read/write error\n"); break;
        default: printf("Error: unknown error\n"); break;
    }
}

int main(int argc, char *argv[]) {
    if (argc < 3) {
        printf("Error: wrong number of arguments\n");
        return 1;
    }

    Action action;
    int has_n = 0;
    Status status = parse_flag(argv[1], &action, &has_n);
    if (status != STATUS_OK) {
        print_error(status);
        return 1;
    }

    if ((has_n && argc != 4) || (!has_n && argc != 3)) {
        printf("Error: wrong number of arguments for this flag\n");
        return 1;
    }

    const char *input_path = argv[2];
    char *generated_path = NULL;
    const char *output_path = NULL;
    if (has_n) {
        output_path = argv[3];
    } else {
        status = make_output_path(input_path, &generated_path);
        if (status != STATUS_OK) {
            print_error(status);
            return 1;
        }
        output_path = generated_path;
    }

    if (strcmp(input_path, output_path) == 0) {
        printf("Error: input and output files must be different\n");
        free(generated_path);
        return 1;
    }

    FILE *input = fopen(input_path, "r");
    if (input == NULL) {
        printf("Error: cannot open input file\n");
        free(generated_path);
        return 1;
    }
    FILE *output = fopen(output_path, "w");
    if (output == NULL) {
        printf("Error: cannot open output file\n");
        fclose(input);
        free(generated_path);
        return 1;
    }

    status = process_file(input, output, action);

    fclose(input);
    if (fclose(output) != 0 && status == STATUS_OK) {
        status = STATUS_FILE_ERROR;
    }

    if (status != STATUS_OK) {
        print_error(status);
        free(generated_path);
        return 1;
    }

    printf("Done, result written to %s\n", output_path);
    free(generated_path);
    return 0;
}
