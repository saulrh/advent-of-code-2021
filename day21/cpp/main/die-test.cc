#include "main/die.h"

#include <gtest/gtest.h>

namespace day21 {
namespace {

TEST(DieTest, StartsAtOne) {
  DeterministicDie d(100);

  EXPECT_EQ(d.roll(), 1);
}

TEST(DieTest, Max100) {
  DeterministicDie d(100);
  int max_found = 0;
  for (int i = 0; i < 1000; i++) {
    max_found = std::max(max_found, d.roll());
  }
  EXPECT_EQ(max_found, 100);
}

TEST(DieTest, Min1) {
  DeterministicDie d(100);
  int min_found = 10000;
  for (int i = 0; i < 1000; i++) {
    min_found = std::min(min_found, d.roll());
  }
  EXPECT_EQ(min_found, 1);
}

}  // namespace
}  // namespace day21
