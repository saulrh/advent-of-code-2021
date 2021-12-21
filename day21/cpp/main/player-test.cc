#include "main/player.h"

#include <gtest/gtest.h>

#include "absl/random/random.h"

namespace day21 {
namespace {

TEST(DieTest, Max100) {
  Player p(1);
  absl::BitGen bitgen;

  int max_found = 0;
  for (int i = 0; i < 1000; i++) {
    int movement = absl::Uniform(bitgen, 0, 30);
    p.move(movement);
    max_found = std::max(max_found, p.pos());
  }
  EXPECT_EQ(max_found, 10);
}

TEST(PlayerTest, Min1) {
  Player p(1);
  absl::BitGen bitgen;

  int min_found = 1000;
  for (int i = 0; i < 1000; i++) {
    int movement = absl::Uniform(bitgen, 0, 30);
    p.move(movement);
    min_found = std::min(min_found, p.pos());
  }
  EXPECT_EQ(min_found, 1);
}

TEST(PlayerTest, Scoring) {
  Player p(7);
  p.move(5);
  EXPECT_EQ(p.score(), 2);
}

}  // namespace
}  // namespace day21
