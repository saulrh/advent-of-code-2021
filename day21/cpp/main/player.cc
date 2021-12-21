#include "main/player.h"

namespace day21 {

void Player::move(int places) {
  pos_ += places;
  pos_ = ((pos_ - 1) % 10) + 1;
  score_ += pos_;
}

int Player::pos() const { return pos_; }

int Player::score() const { return score_; }

}  // namespace day21
