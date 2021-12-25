#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int HEIGHT = 137;
int WIDTH = 139;
int SIZE = 19043;
char* FILENAME = "../input.txt";

/* int HEIGHT = 9; */
/* int WIDTH = 10; */
/* int SIZE = 90; */
/* char* FILENAME = "../example1.txt"; */

char halfstep(char* from, char* to, char set) {
  char tmp[SIZE];
  char moved = 0;
  memcpy(tmp, from, SIZE * sizeof(char));
  for (int idx = 0; idx < SIZE; idx++) {
    char val = from[idx];
    if (val == set) {
      int next_idx;
      if (val == '>') {
        int row = idx / WIDTH;
        next_idx = ((idx + 1) % WIDTH) + (row * WIDTH);
      }
      if (val == 'v') {
        next_idx = (idx + WIDTH) % SIZE;
      }
      if (from[next_idx] == '.') {
        moved = 1;
        tmp[next_idx] = val;
        tmp[idx] = '.';
      } else {
        tmp[idx] = val;
      }
    }
  }
  memcpy(to, tmp, SIZE * sizeof(char));
  return moved;
}

char step(char* data) {
  char tmp[SIZE];
  memset(tmp, 0, SIZE * sizeof(char));
  char moved = 0;
  if (halfstep(data, tmp, '>')) { moved = 1; }
  if (halfstep(tmp, data, 'v')) { moved = 1; }
  return moved;
}

void dump(char* data) {
  for (int row = 0; row < HEIGHT; row++) {
    for (int col = 0; col < WIDTH; col++) {
      char val = data[row * WIDTH + col];
      printf("%c", val);
    }
    printf("\n");
  }
  printf("\n");
}

void read(FILE* f, char* data) {
  int idx = 0;
  char c;
  while ((c = getc(f)) != -1) {
    if (c == '.' || c == '>' || c == 'v') {
      data[idx++] = c;
    }
  }
}

int count(char* data) {
  int result = 0;
  for (int idx = 0; idx < SIZE; idx++) {
    if (data[idx] != '.') {
      result += 1;
    }
  }
  return result;
}

long long checksum(char* data) {
  long long sum = 0;
  for (int idx = 0; idx < SIZE; idx++) {
    sum += idx * (data[idx] != '.');
  }
  return sum;
}

long long checksum_of_file(char* filename) {
  char data[SIZE];
  FILE* f = fopen(filename, "rt");
  read(f, data);
  fclose(f);
  return checksum(data);
}

void check_checksum(char* data, char* filename) {
  long long expected_checksum = checksum_of_file(filename);
  long long actual_checksum = checksum(data);
  if (expected_checksum != actual_checksum) {
    printf("CHECKSUM MISMATCH!: %lld -> %lld\n", expected_checksum, actual_checksum);
    exit(-1);
  }
}

int main() {
  char data[SIZE];

  memset(data, 0, SIZE * sizeof(char));

  FILE* f = fopen(FILENAME, "rt");
  read(f, data);
  fclose(f);

  int init_count = count(data);

  int steps = 0;
  while (step(data)) {
    steps++;
    printf("State after %d steps:\n", steps);
    dump(data);

    /* if (steps == 1) { check_checksum(data, "../example1_1.txt"); } */
    /* if (steps == 2) { check_checksum(data, "../example1_2.txt"); } */
    /* if (steps == 10) { check_checksum(data, "../example1_10.txt"); } */
    /* if (steps == 30) { check_checksum(data, "../example1_30.txt"); } */
    /* if (steps == 57) { check_checksum(data, "../example1_57.txt"); } */
    
    int current_count = count(data);
    if (current_count != init_count) {
      printf("COUNT MISMATCH!: %d -> %d\n", init_count, current_count);
      exit(-1);
    }
  }
  steps++;
  printf("DONE, no movement after step %d. Final state:\n", steps);
  dump(data);
}
