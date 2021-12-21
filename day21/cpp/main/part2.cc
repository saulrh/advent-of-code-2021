#include <cstdint>
#include <iostream>
#include <string>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "absl/hash/hash.h"
#include "absl/strings/str_format.h"
#include "absl/strings/str_join.h"

constexpr uint8_t WIN_THRESHOLD = 21;

using Wins = std::array<uint64_t, 2>;

struct State {
 public:
  friend bool operator==(const State& lhs, const State& rhs) {
    return absl::c_equal(lhs.score, rhs.score) &&
           absl::c_equal(lhs.pos, rhs.pos) &&
           (lhs.whose_turn_idx == rhs.whose_turn_idx);
  };

  template <typename H>
  friend H AbslHashValue(H h, const State& s) {
    return H::combine(std::move(h), s.whose_turn_idx, s.pos[0], s.pos[1],
                      s.score[0], s.score[1]);
  }

  Wins wins() const {
    Wins result{0, 0};
    for (int i = 0; i < 2; i++) {
      if (score[i] >= WIN_THRESHOLD) {
        result[i] = 1;
      }
    }
    return result;
  };

  bool won() const {
    return absl::c_any_of(wins(), [](int w) { return w != 0; });
  }

  friend absl::FormatConvertResult<absl::FormatConversionCharSet::kString>
  AbslFormatConvert(const State& state, const absl::FormatConversionSpec& _,
                    absl::FormatSink* s) {
    s->Append(absl::StrFormat("<Pos: %d, %d ; Scr: %d, %d ; %d to play>",
                              state.pos[0], state.pos[1], state.score[0],
                              state.score[1], state.whose_turn_idx));
    return {true};
  }

  std::array<uint8_t, 2> pos;
  std::array<uint8_t, 2> score;
  size_t whose_turn_idx;
};

uint8_t move_player(uint8_t init, uint8_t dist) {
  return ((init + dist - 1) % 10) + 1;
}

uint8_t next_whose_turn_idx(uint8_t whose_turn_idx) {
  return (whose_turn_idx + 1) % 2;
}

std::array<State, 27> successors(const State& s) {
  std::array<State, 27> result;
  for (uint8_t i = 0; i < 27; i++) {
    int d1 = i % 3;
    int d2 = (i / 3) % 3;
    int d3 = (i / 9) % 3;
    result[i] = s;
    result[i].pos[s.whose_turn_idx] =
        move_player(s.pos[s.whose_turn_idx], d1 + d2 + d3 + 3);
    result[i].score[s.whose_turn_idx] += result[i].pos[s.whose_turn_idx];
    result[i].whose_turn_idx = next_whose_turn_idx(s.whose_turn_idx);
  }
  return result;
}

Wins number_of_wins(const State& cur, absl::flat_hash_map<State, Wins>& memo) {
  auto cur_count_it = memo.find(cur);
  if (cur_count_it != memo.end()) {
    return cur_count_it->second;
  }

  Wins wins{0, 0};
  for (const State& next : successors(cur)) {
    Wins next_wins{0, 0};
    if (next.won()) {
      next_wins = next.wins();
    } else {
      next_wins = number_of_wins(next, memo);
    }
    for (int i = 0; i < 2; i++) {
      wins[i] += next_wins[i];
    }
  }
  memo[cur] = wins;
  return wins;
}

int main() {
  State start{
      .pos = {6, 1},  // input
                      // .pos={4, 8}, // example
  };

  absl::flat_hash_map<State, Wins> memo;
  absl::PrintF("running...\n");
  Wins wins = number_of_wins(start, memo);
  for (int i = 0; i < 2; i++) {
    absl::PrintF("Player %d won this many games: %d\n", i, wins[i]);
  }
  absl::PrintF("done...\n");

  return 0;
}
