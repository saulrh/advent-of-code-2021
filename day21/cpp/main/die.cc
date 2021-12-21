#include "main/die.h"

namespace day21 {

int DeterministicDie::roll() {
  last_roll_++;
  int next_roll = last_roll_;
  last_roll_ %= 100;
  rolls_++;
  return next_roll;
}

int DeterministicDie::num_times_rolled() const { return rolls_; }

}  // namespace day21
