#include "main/game.h"

#include <gtest/gtest.h>

#include "main/die.h"
#include "main/player.h"

namespace day21 {
namespace {

TEST(GameTest, ExampleGameOneTurn) {
  DeterministicDie d(100);
  Game g({4, 8});
  std::vector<int> turn0_expected_scores({0, 0});
  ASSERT_EQ(g.scores(), turn0_expected_scores);

  g.turn(d);
  g.turn(d);
  std::vector<int> turn1_expected_scores({10, 3});
  ASSERT_EQ(g.scores(), turn1_expected_scores);
}

TEST(GameTest, ExampleGameWinner) {
  DeterministicDie d(100);
  Game g({4, 8});

  while (!g.won()) {
    ASSERT_EQ(g.winner(), absl::nullopt);
    g.turn(d);
  }

  ASSERT_TRUE(g.won());
  ASSERT_EQ(g.winner(), std::size_t{0});
  ASSERT_EQ(d.num_times_rolled(), 993);
  std::vector<int> expected_scores({1000, 745});
  ASSERT_EQ(g.scores(), expected_scores);
}

}  // namespace
}  // namespace day21
