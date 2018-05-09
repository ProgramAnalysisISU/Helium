#include "helium/utils/RandUtils.h"
#include <gtest/gtest.h>
#include <boost/filesystem.hpp>
#include <boost/foreach.hpp>

#include <string>
#include <vector>
#include <map>
#include <set>
#include <stack>
#include <fstream>
#include <cassert>
#include <algorithm>
#include <iostream>


namespace utils {

  void seed_rand() {
    srand(time(0));
  }

  /**
   * Return a random integer that is in range [low, high)
   * TODO Needs some tests
   */
  int rand_int(int low, int high) {
    if (high <= low) {
      std::cerr << "EE: rand_int except high > low, but with input: "
        "[low,high) beging [" << low << "," << high << ")"  << "\n";
      throw std::runtime_error("RandInt Error");
    }
    assert(high > low);
    int a = rand();
    return a % (high - low) + low;
  }

  /**
   * Generate [num] number of random int, in [low, high), without duplication
   */
  std::set<int> rand_ints(int low, int high, int num) {
    std::set<int> ret;
    while (high-- > low) {
      ret.insert(high);
    }
    int to_remove = ret.size() - num;
    int r;
    while (to_remove-- > 0) {
      r = rand_int(0, ret.size());
      auto it = ret.begin();
      std::advance(it, r);
      ret.erase(it);
    }
    return ret;
  }

  TEST(UtilsTestCase, RandIntTest) {
    for (int i=0;i<10;i++) {
      // std::cout << rand_int(0, 1) << "\n";
    }
    // int neg = rand_int(-10, -7);
    // std::cout << neg  << "\n";
    {
      std::set<int> tmp = rand_ints(1, 8, 2);
      EXPECT_EQ(tmp.size(), 2);
    }
  }

  bool rand_bool() {
    int a = rand_int(0, 2);
    if (a == 1) return true;
    else return false;
  }

  /**
   * Random character, a-Z, A-Z
   */
  char rand_char() {
#if true
    bool x = rand_bool();
    if (x) {
      // alphebet
      int a = rand_int(0, 51);
      if (a < 26) {
        return 'a'+a;
      } else {
        return 'A'+a-26;
      }
    } else {
      // symbols
      // '!' 41
      // "\"" 42
      // '@' 100
      int a = rand_int(41, 100);
      if (a == 42) {
        a = 41;
      }
      return '!' + a - 41;
    }
#else
    int a = rand_int(0, 51);
    if (a < 26) {
      return 'a'+a;
    } else {
      return 'A'+a-26;
    }
#endif
  }
  std::string rand_str(int size) {
    std::string ret;
    for (int i=0;i<size;i++) {
      ret += rand_char();
    }
    return ret;
  }

  // TEST(UtilsTestCase, RandTest) {
  //   std::cout << "random int: ";
  //   for (int i=0;i<10;i++) {
  //     std::cout << rand_int(0, 100) << " ";
  //   }
  //   std::cout << "\n random char: ";
  //   for (int i=0;i<10;i++) {
  //     std::cout << rand_char() << " ";
  //   }
  //   std::cout << "\n random str: ";
  //   for (int i=0;i<5;i++) {
  //     std::cout << rand_str(rand_int(0, 5)) << " ";
  //   }
  //   std::cout   << "\n";
  // }


}
