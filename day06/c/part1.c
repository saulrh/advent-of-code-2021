#include "stdlib.h"
#include "stdio.h"
#include "string.h"
#include "stdint.h"

#define FISH_BUFSIZE 10

void assert_eq_int(int64_t a, int64_t b) {
  if (a != b) {
    printf("\x1b[31m");
    printf("ASSERTION a == b FAILED: %ld != %ld", a, b);
    printf("\x1b[0m");
    printf("\n");
    exit(-1);
  }
}

void assert_eq_intarr(int64_t* a, int64_t* b, int len) {
  for (int i = 0; i < len; i++) {
    if (a[i] != b[i]) {
      printf("\x1b[31m");
      printf("ASSERTION a[] != b[] at idx %d FAILED: %ld != %ld", i, a[i], b[i]);
      printf("\x1b[0m");
      printf("\n");
      exit(-1);
    }
  }
}

void step(int64_t fish[FISH_BUFSIZE]) {
  int64_t newfish[FISH_BUFSIZE];
  memset(newfish, 0, FISH_BUFSIZE * sizeof(int64_t));

  /* Each fish at period 0 turns into a fish at 8 and a fish at 6 */
  newfish[6] = fish[0];
  newfish[8] = fish[0];
  /* Each fish at another period turns into a fish at a period of one
     less */
  for (int i = 1; i < FISH_BUFSIZE; i++) {
    newfish[i-1] += fish[i];
  }

  memcpy(fish, newfish, FISH_BUFSIZE * sizeof(int64_t));
}

int64_t sum(int64_t fish[FISH_BUFSIZE]) {
  int64_t count = 0;
  for (int i = 0; i < FISH_BUFSIZE; i++) {
    count += fish[i];
  }
  return count;
}

void read(char* fname, int64_t* fish) {
  memset(fish, 0, FISH_BUFSIZE * sizeof(int64_t));
  int next;
  FILE* file;
  file = fopen(fname, "rt");
  while (fscanf(file, "%d", &next) != EOF) {
    fish[next]++;
    fscanf(file, ",");
  }
  fclose(file);
}

int main(int argc, char** argv) {
  /* how many at each period */
  int64_t fish[FISH_BUFSIZE];

  /* read example */
  read("../example.txt", fish);

  /* step 18 times and test the number of fish */
  for (int i = 0; i < 18; i++) {
    step(fish);
  }
  assert_eq_int(sum(fish), 26);
  for (int i = 0; i < 80 - 18; i++) {
    step(fish);
  }
  assert_eq_int(sum(fish), 5934);

  for (int i = 0; i < 256 - 80; i++) {
    step(fish);
  }
  assert_eq_int(sum(fish), 26984457539);

  /* read real input */
  read("../input.txt", fish);
  for (int i = 0; i < 80; i++) {
    step(fish);
  }
  assert_eq_int(sum(fish), 366057);
  for (int i = 0; i < 256 - 80; i++) {
    step(fish);
  }
  printf("%ld\n", sum(fish));

  exit(0);
}
