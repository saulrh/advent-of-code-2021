#ifndef MAIN_DIE_H_
#define MAIN_DIE_H_

namespace day21 {

class DeterministicDie {
 public:
  
  DeterministicDie(int period) : period_(period), last_roll_(0) {}

  int roll();
  int num_times_rolled() const;

 private:
  int rolls_;
  int period_;
  int last_roll_;
};

}

#endif
