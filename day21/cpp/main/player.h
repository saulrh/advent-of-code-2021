#ifndef MAIN_PLAYER_H_
#define MAIN_PLAYER_H_

namespace day21 {

class Player {
 public:
  Player(int pos) : pos_(pos), score_(0) {}
  
  void move(int places);

  int pos() const;
  int score() const;

 private:
  int pos_;
  int score_;

};

}

#endif
