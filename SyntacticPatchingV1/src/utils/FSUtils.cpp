#include "helium/utils/FSUtils.h"

#include <string>
#include <vector>
#include <map>
#include <set>
#include <stack>
#include <fstream>
#include <cassert>
#include <algorithm>
#include <iostream>
#include <gtest/gtest.h>
#include <boost/filesystem.hpp>
#include <boost/foreach.hpp>

namespace utils {
  std::string escape_tide(std::string path) {
    if (path.empty()) return path;
    if (path[0] == '~') {
      const char *home = getenv("HOME");
      if (home) {
        path.replace(0, 1, home);
      }
    }
    return path;
  }

  
  /*******************************
   ** FileUtil
   *******************************/

  /**
   * Get all files in folder.
   */
  std::vector<std::string> get_files(const std::string& folder) {
    std::vector<std::string> vs;
    get_files(folder, vs);
    return vs;
  }
  /**
   * get all files of the folder.
   * @param[in] folder
   * @param[out] vs
   * This is recursive
   * The input must be path without ~
   * The output will be full path
   */
  void get_files(const std::string& folder, std::vector<std::string>& vs) {
    fs::path project_folder(folder);
    fs::recursive_directory_iterator it(folder), eod;
    BOOST_FOREACH (fs::path const & p, std::make_pair(it, eod)) {
      if (is_regular_file(p)) {
        vs.push_back(p.string());
      }
    }
  }

  // TEST(UtilsTestCase, GetFilesTest) {
  //   std::vector<std::string> files = get_files("/home/hebi/tmp");
  //   for (std::string file : files) {
  //     std::cout << file  << "\n";
  //   }
  // }

  /**
   * Get files in a folder, with ONE specific extension.
   * @param[in] folder
   * @param[out] vs
   * @param[in] extension a suffix as string
   */
  void get_files_by_extension(
                                     const std::string& folder,
                                     std::vector<std::string>& vs,
                                     const std::string& extension
                                     ) {
    if (!is_dir(folder)) return;
    std::vector<std::string> ve {extension};
    get_files_by_extension(folder, vs, ve);
  }

  /**
   * Get files in a folder, with some specific extensions.
   * @param[in] folder folder to search
   * @param[out] vs a list of matched files
   * @param[in] extension a list of extensions
   */
  void get_files_by_extension(
                                     const std::string& folder,
                                     std::vector<std::string>& vs,
                                     const std::vector<std::string>& extension
                                     ) {
    if (!is_dir(folder)) return;
    fs::path project_folder(folder);
    fs::recursive_directory_iterator it(folder), eod;
    BOOST_FOREACH(fs::path const& p, std::make_pair(it, eod)) {
      if (is_regular_file(p)) {
        std::string with_dot = p.extension().string();
        // the extension may not exist
        if (!with_dot.empty()) {
          // if extension does not exist,
          // the substr(1) will cause exception of out of range of basic_string
          std::string without_dot = with_dot.substr(1);
          if (std::find(extension.begin(), extension.end(), with_dot) != extension.end()
              || std::find(extension.begin(), extension.end(), without_dot) != extension.end()) {
            vs.push_back(p.string());
          }
        }
      }
    }
  }

  /**
   * Well, unforturnately the home directory tide, ~, is not able to be recognized by boost.
   */
  bool
  file_exists(const std::string& file) {
    fs::path file_path(file);
    return fs::exists(file_path);
  }

  bool exists(const std::string &file) {
    fs::path file_path(file);
    return fs::exists(file_path);
  }

  TEST(UtilsTestCase, DISABLED_FileExistsTest) {
    EXPECT_TRUE(file_exists("~/tmp/b.csv")); // this will gives false. tide is not recognized
  }

  /**
   * Get the full path, and canonical one.
   * I.e. no symbol link, no dots, no extra slashes
   * Will throw exception if the file or directory does not exist
   */
  std::string full_path(const std::string &file_or_dir) {
    fs::path file_path(file_or_dir);
    // may throw exception
    fs::path can_path = fs::canonical(file_path);
    return can_path.string();
  }

  /**
   * This is very system specific test.
   * Should be disabled.
   */
  TEST(UtilsTestCase, DISABLED_FullPathTest) {
    std::cout << full_path("./src/utils.cc") << "\n";
    std::cout << full_path(".//src/../src/utils.cc")  << "\n";
  }

  bool is_file(const std::string &file) {
    struct stat sb;
    if (stat(file.c_str(), &sb) == 0 && S_ISREG(sb.st_mode)) {
      return true;
    }
    return false;
  }
  bool is_dir(const std::string &file) {
    struct stat sb;
    if (stat(file.c_str(), &sb) == 0 && S_ISDIR(sb.st_mode)) {
      return true;
    }
    return false;
  }
  TEST(utils_test_case, is_file_dir) {
    EXPECT_TRUE(is_file("/usr/include/stdio.h"));
    EXPECT_FALSE(is_file("/usr/include"));
    EXPECT_FALSE(is_file("/fjlsdjn/fdj"));

    EXPECT_FALSE(is_dir("/usr/include/stdio.h"));
    EXPECT_TRUE(is_dir("/usr/include"));
    EXPECT_FALSE(is_dir("/fjlsdjn/fdj"));

  }

  /**
   * Write file with content.
   * Will overwrite. If the file doesn't exist, it will create it recursively(the parent directory)
   */
  void
  write_file(fs::path file, const std::string& content) {
    std::ofstream os;
    os.open(file.string());
    if (os.is_open()) {
      os << content;
      os.close();
    } else {
      std::cerr << "Cannot open file " << file << "\n";
    }
  }

  /**
   * Append content to file. Create if not existing.
   */
  void
  append_file(fs::path file, const std::string& content) {
    fs::path file_path(file);
    // fs::path dir = file_path.parent_path();
    // if (!fs::exists(dir)) {
    //   std::cout << dir  << "\n";
    //   fs::create_directories(dir);
    // }
    std::ofstream os;
    os.open(file_path.string(), std::ios_base::app);
    if (os.is_open()) {
      os<<content;
      os.close();
    }
  }

  /**
   * Read a file into string.
   */
  std::string
  read_file(fs::path file) {
    std::ifstream is;
    is.open(file.string());
    std::string code;
    if (is.is_open()) {
      std::string line;
      while(getline(is, line)) {
        code += line+"\n";
      }
      is.close();
    }
    return code;
  }

  /**
   * line: 1,2,3
   * column: 1,2,3
   * get from [(beginline,begincolumn), (endline,endcol)]
   * INCLUDE THE END
   * - e.g. 1:1 to 1:1 will contain 1 character
   * - 1:1 to 1:2 will contain two
   */
  std::string read_file(fs::path file, int beginLine, int beginColumn, int endLine, int endColumn) {
    std::ifstream is;
    is.open(file.string());
    int l=0;
    std::string ret;
    if (is.is_open()) {
      std::string line;
      while(getline(is, line)) {
        l++;
        if (l < beginLine || l > endLine) {
        } else if (l>beginLine && l < endLine) {
          ret += line + "\n";
        } else if (l==beginLine && l == endLine) {
          ret += line.substr(beginColumn-1, endColumn+1 - beginColumn);
        } else if (l== endLine) {
          ret += line.substr(0, endColumn);
        } else if (l==beginLine) {
          ret += line.substr(beginColumn-1) + "\n";
        } else {
          break;
        }
      }
      is.close();
    }
    return ret;
  }

  /**
   * Get the last component of the path.
   * /path/to/a.c => a.c
   * /path/to/ => ""
   * path/to/a.c => a.c
   */
  std::string file_name_last_component(std::string filename) {
    if (filename.find('/') == std::string::npos) {
      return filename;
    } else {
      filename = filename.substr(filename.find_last_of('/')+1);
      return filename;
    }
  }

  TEST(UtilsTestCase, FileNameLastComponentTest) {
    std::string name;
    name = file_name_last_component("/path/to/a.c");
    EXPECT_EQ(name, "a.c");
    name = file_name_last_component("/path/to/");
    EXPECT_EQ(name, "");
    name = file_name_last_component("path/to/a.c");
    EXPECT_EQ(name, "a.c");
  }


  /**
   * rm -r <folder>
   */
  void
  remove_folder(const std::string& folder) {
    fs::path folder_path(folder);
    if (fs::exists(folder_path)) {
      fs::remove_all(folder_path);
    }
  }

  void remove_file(const std::string &file) {
    fs::path file_path(file);
    if (fs::exists(file)) {
      fs::remove(file);
    }
  }

  /**
   * mkdir -p <folder>
   */
  void
  create_folder(const std::string& folder) {
    if (folder.empty()) return;
    fs::path folder_path(folder);
    if (!fs::exists(folder_path)) {
      fs::create_directories(folder_path);
    }
  }

  /**
   * create tmp dir, return it.
   * @input s /tmp/helium-XXXXXX (must have 6 X at the end.)
   */
  std::string create_tmp_dir(std::string s) {
    // char tmp_dir[] = "/tmp/helium-test-temp.XXXXXX";
    std::string sub = s.substr(s.find_last_not_of('X'));
    if (sub.size() !=7) return "";
    sub = sub.substr(1);
    assert(sub.size() == 6 && "tmp dir url format error!");
    if (sub.find_first_not_of('X') != std::string::npos) return "";
    char tmp_dir[s.size()+1];
    strcpy(tmp_dir, s.c_str());
    char *result = mkdtemp(tmp_dir);
    if (result == NULL) return "";
    std::string dir = tmp_dir;
    return dir;
  }

  TEST(utils_test_case, tmp_dir) {
    std::string s = create_tmp_dir("/garjb");
    EXPECT_EQ(s, "");
    s = create_tmp_dir("/tmp/helium.XXXXXX");
    EXPECT_NE(s, "");
    EXPECT_TRUE(is_dir(s));
  }




  bool match_suffix(fs::path p, fs::path suffix) {
    if (p == suffix) return true;
    while (p.filename() == suffix.filename() && !suffix.empty() && !p.empty()) {
      p = p.parent_path();
      suffix = suffix.parent_path();
    }
    if (suffix.empty()) return true;
    else return false;
  }

  fs::path substract_suffix(fs::path p, fs::path suffix) {
    assert(match_suffix(p, suffix));
    if (p == suffix) return fs::path("");
    while (p.filename() == suffix.filename() && !suffix.empty() && !p.empty()) {
      p = p.parent_path();
      suffix = suffix.parent_path();
    }
    return p;
  }

  TEST(FsUtilTest, SuffixTest) {
    EXPECT_TRUE(match_suffix("/hello/world", "world"));
    EXPECT_FALSE(match_suffix("world", "/hello/world"));
    EXPECT_FALSE(match_suffix("/hello/world", "/world"));
    EXPECT_TRUE(match_suffix("/hello/world/a.c", "a.c"));
    EXPECT_TRUE(match_suffix("/hello/world/a.c", "world/a.c"));
    EXPECT_FALSE(match_suffix("/hello/world/a.c", "hello/a.c"));

    EXPECT_EQ(substract_suffix("/hello/world", "world"), fs::path("/hello"));
    EXPECT_EQ(substract_suffix("/hello/world/a.c", "a.c"), fs::path("/hello/world"));
    EXPECT_EQ(substract_suffix("/hello/world/a.c", "world/a.c"), fs::path("/hello"));
  }
  
}
