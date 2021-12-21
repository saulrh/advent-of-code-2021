#include <iostream>
#include <string>
#include <vector>

#include "absl/strings/str_format.h"
#include "main/die.h"
#include "main/game.h"
#include "main/player.h"

int main() {
  day21::DeterministicDie die(100);
  day21::Game game({6, 1});
  while (!game.won()) {
    game.turn(die);
  }

  size_t winner_idx = *game.winner();
  std::vector<int> scores = game.scores();
  scores.erase(scores.begin() + winner_idx);
  int loser_score = *scores.begin();
  absl::PrintF("loser's score * number of rolls: %d\n",
               loser_score * die.num_times_rolled());
}
