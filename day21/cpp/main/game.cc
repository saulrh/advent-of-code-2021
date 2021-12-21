#include "main/game.h"

#include <vector>

#include "absl/algorithm/container.h"
#include "main/die.h"
#include "main/player.h"

namespace day21 {

Game::Game(absl::Span<const int> player_starting_positions)
    : players_(), next_turn_(0) {
  for (int pos : player_starting_positions) {
    players_.emplace_back(pos);
  }
}

void Game::turn(DeterministicDie& die) {
  Player& player_to_move = players_[next_turn_];
  std::vector<int> rolls;
  for (int i = 0; i < 3; i++) {
    rolls.push_back(die.roll());
  }
  player_to_move.move(absl::c_accumulate(rolls, 0));
  next_turn_ = (next_turn_ + 1) % players_.size();
}

std::vector<int> Game::scores() {
  std::vector<int> result;
  for (const Player& p : players_) {
    result.push_back(p.score());
  }
  return result;
}

bool Game::won() {
  return absl::c_any_of(players_,
                        [](const Player& p) { return p.score() >= 1000; });
}

absl::optional<size_t> Game::winner() {
  auto p = absl::c_find_if(players_,
                           [](const Player& p) { return p.score() >= 1000; });
  if (p == players_.end()) {
    return absl::nullopt;
  } else {
    return std::distance(players_.begin(), p);
  }
}

}  // namespace day21
