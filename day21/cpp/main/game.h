#ifndef MAIN_GAME_H_
#define MAIN_GAME_H_

#include <vector>
#include "main/die.h"
#include "main/player.h"

#include "absl/types/span.h"
#include "absl/types/optional.h"

namespace day21 {

class Game {
 public:
  Game(absl::Span<const int> player_starting_positions);

  void turn(DeterministicDie& die);

  bool won();

  absl::optional<size_t> winner();

  std::vector<int> scores();

 private:
  std::vector<Player> players_;
  int next_turn_;
};

}
#endif
