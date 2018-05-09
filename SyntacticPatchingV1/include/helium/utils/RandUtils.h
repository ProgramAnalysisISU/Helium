#ifndef RAND_UTILS_H
#define RAND_UTILS_H

#include <set>
#include <string>

namespace utils {
  /*******************************
   ** random
   *******************************/
  void seed_rand();
  /**
   * Need to call seed_rand() before this.
   * [low, high], inclusive
   */
  int rand_int(int low, int high);
  std::set<int> rand_ints(int low, int high, int num);
  char rand_char();
  std::string rand_str(int size);
  bool rand_bool();

}
#endif /* RAND_UTILS_H */
