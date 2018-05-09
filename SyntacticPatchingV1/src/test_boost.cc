#include <boost/filesystem.hpp>
#include <gtest/gtest.h>
#include <fstream>
#include "helium/utils/fs_utils.h"
namespace fs = boost::filesystem;


TEST(BoostCase, FileSystemTest) {
  EXPECT_FALSE(fs::exists("~/.bashrc")); // boost cannot expand ~
  std::string full = utils::escape_tide("~/.bashrc"); // this util function works
  // std::cout << full << "\n";
  EXPECT_TRUE(fs::exists(full)); // the exists will check symbolic links
  EXPECT_FALSE(fs::exists("~"));
  fs::path home("~");
  EXPECT_EQ(home.string(), "~");
  // canonical will throw exception if it cannot found by using exists,
  // which means the ~ can cause the error.
  EXPECT_ANY_THROW(fs::canonical(home));
}


// This needs to be not hard-coded
TEST(StdCase, StreamTest) {
  std::ifstream ifs;
  // ifs can open both regular file and symbolic link
  ifs.open("/home/hebi/.bashrc");
  EXPECT_TRUE(ifs.is_open());
  ifs.open("/home/hebi/.bashrc.local");
  EXPECT_TRUE(ifs.is_open());
  // but it cannot expand ~
  // YES, it can!!!
  ifs.open("~/.bashrc");
  EXPECT_TRUE(ifs.is_open());
  ifs.open("~/.bashrc.local");
  EXPECT_TRUE(ifs.is_open());
}
