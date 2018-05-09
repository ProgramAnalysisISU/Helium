#include "helium/utils/Utils.h"
#include <gtest/gtest.h>
#include <boost/filesystem.hpp>
#include <boost/foreach.hpp>
#include <iterator>
#include <fstream>
#include "helium/utils/FSUtils.h"

namespace fs = boost::filesystem;
using namespace utils;

namespace utils {

  bool is_number(const std::string& s) {
    std::string::const_iterator it = s.begin();
    while (it != s.end() &&
           // FIXME the corner case '-'
           (std::isdigit(*it) || *it == '-')
           ) {
      ++it;
    }
    return !s.empty() && it == s.end();
  }

  TEST(utils_test_case, is_number_test) {
    EXPECT_TRUE(is_number("442"));
    EXPECT_TRUE(is_number("10"));
    // EXPECT_FALSE(is_number("5-2")); // FIXME
    EXPECT_FALSE(is_number("5+2"));
    EXPECT_FALSE(is_number("SIZE"));
    EXPECT_TRUE(is_number("-5"));
  }




  /**
   * Get the line numbers in file "filename", that matches the pattern.
   */
  std::vector<int> get_line_numbers(std::string filename, std::string pattern) {
    std::ifstream is;
    is.open(filename);
    int line_number = 0;
    std::vector<int> result;
    if (is.is_open()) {
      std::string line;
      while(getline(is, line)) {
        line_number++;
        if (line.find(pattern) != std::string::npos) {
          result.push_back(line_number);
        }
      }
      is.close();
    }
    return result;
  }


  /**
   * Get only the first number line matching the pattern
   */
  int get_line_number(std::string filename, std::string pattern) {
    std::ifstream is;
    is.open(filename);
    int line_number = 0;
    if (is.is_open()) {
      std::string line;
      while(getline(is, line)) {
        line_number++;
        if (line.find(pattern) != std::string::npos) {
          is.close();
          return line_number;
        }
      }
      is.close();
    }
    return 0;
  }

  TEST(utils_test_case, get_line_number) {
    const char *raw = R"prefix(
2
hello world
really this /* @HeliumLineMark */
this lien contains none
6

8 the previous is empyt line
@HeliumLineMark

)prefix";
    std::string dir = create_tmp_dir("/tmp/helium-test.XXXXXX");
    write_file(dir+"/a.txt", raw);
    // std::cout <<dir+"/a.txt"  << "\n";
    int line = get_line_number(dir+"/a.txt", "@HeliumLineMark");
    EXPECT_EQ(line, 4);
    std::vector<int> lines = get_line_numbers(dir+"/a.txt", "@HeliumLineMark");
    ASSERT_EQ(lines.size(), 2);
    EXPECT_EQ(lines[0], 4);
    EXPECT_EQ(lines[1], 9);
  }


#ifdef __MACH__
#include <sys/time.h>
#define CLOCK_REALTIME 0
#define CLOCK_MONOTONIC 0
  //clock_gettime is not implemented on OSX
  int clock_gettime(int /*clk_id*/, struct timespec* t) {
    struct timeval now;
    int rv = gettimeofday(&now, NULL);
    if (rv) return rv;
    t->tv_sec  = now.tv_sec;
    t->tv_nsec = now.tv_usec * 1000;
    return 0;
  }
#endif

  double get_time() {
    struct timespec ts;
    ts.tv_sec=0;
    ts.tv_nsec=0;
    clock_gettime(CLOCK_REALTIME, &ts);
    double d = (double)ts.tv_sec + 1.0e-9*ts.tv_nsec;
    return d;
  }


  void debug_time(std::string id) {
    static double last_time = 0;
    double t = get_time();
    if (!id.empty()) {
      // ignore print when id is empty. This allows to set last_time
      std::cout << id << ": " << t-last_time << "\n";
    }
    last_time = t;
  }


}
