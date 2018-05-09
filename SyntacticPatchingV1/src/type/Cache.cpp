#include "helium/type/Cache.h"

#include "helium/utils/FSUtils.h"
#include "helium/utils/StringUtils.h"
#include "helium/utils/ThreadUtils.h"

#include <boost/algorithm/string/join.hpp>

#include <iostream>
#include <vector>
using namespace std;


void helium_copy_file_with_modify(fs::path from, fs::path to) {
  // fs::copy_file(p, to);
  std::ifstream ifs(from.string());
  std::ofstream ofs(to.string());
  assert(ifs.is_open());
  assert(ofs.is_open());
  std::string line;
  while (std::getline(ifs, line)) {
    // check if it is #include <assert.h>
    if (line.find("<assert.h>") != std::string::npos) {
      ofs << "// HELIUM_REMOVED " << line << "\n";
    } else {
      ofs << line << "\n";
    }
  }
  ifs.close();
  ofs.close();
}

/**
 * Some modifications to the source code:
 * - remove #include <assert.h> with // 
 */
void create_src(fs::path target, fs::path target_cache_dir, fs::path target_sel_dir) {
  fs::path src = target_cache_dir / "src";
  if (fs::exists(src)) fs::remove_all(src);
  fs::create_directories(src);

  // fs::create_directories(target_cache_dir / "sel");
  fs::create_directories(target_sel_dir);
  
  // copy only source files. Keep directory structure
  fs::recursive_directory_iterator it(target), eod;
  BOOST_FOREACH (fs::path const & p, std::make_pair(it, eod)) {
    if (is_regular_file(p)) {
      if (p.extension() == ".c" || p.extension() == ".h") {
        fs::path to = src / fs::relative(p, target);
        fs::create_directories(to.parent_path());
        // std::cout << "From: " << p << "\n";
        // std::cout << "Copying to " << to.string() << "\n";
        // this can also fail!!
        // fs::copy_file  will fail if file is found
        // 
        // even if I removed src folder at the beginning of this
        // procedure, it still happens because:
        // fs::relative_path will follow symbolic link
        if (fs::exists(to)) fs::remove(to);

        // copy file
        // fs::copy_file(p, to);
        // post process the file
        helium_copy_file_with_modify(p, to);
        
      } else if (p.extension() == ".sel") {
        // selection file
        // fs::copy_file(p, target_cache_dir / "sel" / p.filename());
        fs::path to = target_sel_dir / p.filename();
        if (fs::exists(to)) fs::remove(to);
        fs::copy_file(p, to);
      }
    }
  }
}



/**
 * 1. create cpp folder
 * 2. preprocess src and put into cpp folder
 * 3. post-process cpp files
 */
void create_cpp(fs::path target_cache_dir) {
  fs::path cpp = target_cache_dir / "cpp";
  fs::path src = target_cache_dir / "src";
  if (fs::exists(cpp)) fs::remove_all(cpp);
  fs::create_directories(cpp);
  fs::recursive_directory_iterator it(src), eod;
  vector<fs::path> dirs;
  BOOST_FOREACH(fs::path const &p, std::make_pair(it, eod)) {
    if (fs::is_directory(p)) {
      dirs.push_back(p);
    }
  }

  std::string include_cmd;
  for (auto &p : dirs) {
    include_cmd += "-I" + p.string() + " ";
  }

  std::string cpp_cmd = "clang -E " + include_cmd;
    
  it = fs::recursive_directory_iterator(target_cache_dir / "src");
  BOOST_FOREACH(fs::path const &p, std::make_pair(it, eod)) {
    if (fs::is_regular_file(p)) {
      fs::path to = target_cache_dir / "cpp" / fs::relative(p, target_cache_dir / "src");
      fs::create_directories(to.parent_path());
      // redirect the clang preprocessor stderr to /dev/null
      std::string cmd = cpp_cmd + " " + p.string()
        + " >> " + to.string() + " 2>/dev/null";
      ThreadExecutor(cmd).run();
    }
  }

  // remove extra things added by preprocessor using include files
  // std::cout << "Removing extra for cpp-ed file .." << "\n";
  it = fs::recursive_directory_iterator(target_cache_dir / "cpp");
  BOOST_FOREACH(fs::path const &p, std::make_pair(it, eod)) {
    if (fs::is_regular_file(p)) {
      std::ifstream is;
      is.open(p.string());
      std::string code;
      if (is.is_open()) {
        std::string line;
        std::string output;
        bool b = false;
        while(getline(is, line)) {
          if (!line.empty() && line[0] == '#') {
            // this might be a line marker
            vector<string> v = utils::split(line);
            if (v.size() < 3) continue;
            string filename = v[2];
            if (filename.empty() || filename[0] != '"' || filename.back() != '"') continue;
            filename = filename.substr(1);
            filename.pop_back();
            fs::path file(filename);
            fs::path newp = fs::relative(p, target_cache_dir / "cpp");
            fs::path newfile = fs::relative(file, target_cache_dir / "src");
            if (newfile.string() == newp.string()) {
              b = true;
              // I don't need the line marker line, because clang will complain when process it
              // output += line + "\n";
            } else {
              b = false;
            }
          } else {
            if (b) {
              output += line + "\n";
            }
          }
        }
        is.close();
        // output to the same file
        utils::write_file(p.string(), output);
      }
    }
  }
}










static void copy_src_files(fs::path indir, fs::path outdir) {
  fs::recursive_directory_iterator it(indir), eod;
  BOOST_FOREACH (fs::path const & p, std::make_pair(it, eod)) {
    if (is_regular_file(p)) {
      if (p.extension() == ".c" || p.extension() == ".h") {
        fs::path to = outdir / fs::relative(p, indir);
        fs::create_directories(to.parent_path());
        if (fs::exists(to)) fs::remove(to);
        fs::copy_file(p, to);
      }
    }
  }
}

/**
 * remove #include<assert.h>
 */
static void post_copy(fs::path dir) {
  fs::recursive_directory_iterator it(dir), eod;
  BOOST_FOREACH (fs::path const & p, std::make_pair(it, eod)) {
    if (is_regular_file(p)) {
      if (p.extension() == ".c" || p.extension() == ".h") {
        // includes
        std::vector<std::string> includes;
        fs::path from(p);
        fs::path to(p.string() + ".helium");
        std::ifstream ifs(from.string());
        std::ofstream ofs(to.string());
        assert(ifs.is_open());
        assert(ofs.is_open());
        std::string line;
        while (std::getline(ifs, line)) {
          // check if it is #include <assert.h>
          if (line.find("<assert.h>") != std::string::npos) {
            ofs << "// HELIUM_REMOVED " << line << "\n";
          } else {
            ofs << line << "\n";
          }
          // get includes
          if (line.find("#include") != std::string::npos
              && line.find("<") != std::string::npos) {
            includes.push_back(line);
          }
        }
        ifs.close();
        ofs.close();
        fs::remove(from);
        fs::copy_file(to, from);
        fs::remove(to);
        // dump it into a file
        fs::path inc_file = from;
        inc_file.replace_extension(".inc");
        utils::append_file(inc_file, boost::algorithm::join(includes, "\n"));
      }
    }
  }
}

static std::string get_cpp_cmd(fs::path dir) {
  fs::recursive_directory_iterator it(dir), eod;
  vector<fs::path> dirs;
  BOOST_FOREACH(fs::path const &p, std::make_pair(it, eod)) {
    if (fs::is_directory(p)) {
      dirs.push_back(p);
    }
  }
  std::string include_cmd;
  for (auto &p : dirs) {
    include_cmd += "-I" + p.string() + " ";
  }
  std::string cpp_cmd = "clang -E " + include_cmd;
  return cpp_cmd;
}

// remove extra things added by preprocessor using include files
// std::cout << "Removing extra for cpp-ed file .." << "\n";
static void post_cpp(fs::path dir) {
  fs::recursive_directory_iterator it(dir), eod;
  BOOST_FOREACH(fs::path const &p, std::make_pair(it, eod)) {
    if (fs::is_regular_file(p)) {
      if (p.extension() == ".c" || p.extension() == ".h"){
        fs::path inc_file = p;
        inc_file.replace_extension(".inc");
        std::ifstream is;
        is.open(p.string());
        std::string code;
        if (is.is_open()) {
          std::string line;
          std::string output;
          // include the .inc file
          // if (fs::exists(inc_file)) {
          //   output += "#include \"" + inc_file.filename().string() + "\"\n";
          // }
          bool b = false;
          while(getline(is, line)) {
            if (!line.empty() && line[0] == '#') {
              // this might be a line marker
              vector<string> v = utils::split(line);
              if (v.size() < 3) continue;
              string filename = v[2];
              if (filename.empty() || filename[0] != '"' || filename.back() != '"') continue;
              filename = filename.substr(1);
              filename.pop_back();
              if (filename == p.string()) b=true;
              else b=false;
            } else {
              if (b) {
                output += line + "\n";
              }
            }
          }
          is.close();
          // output to the same file
          utils::write_file(p.string(), output);
        }
      }
    }
  }
}

static void cpp(fs::path dir) {
  std::string cpp_cmd = get_cpp_cmd(dir);
  fs::recursive_directory_iterator it(dir), eod;
  BOOST_FOREACH(fs::path const &p, std::make_pair(it, eod)) {
    if (fs::is_regular_file(p)) {
      if (p.extension() ==".c" || p.extension() == ".h") {
        std::string cmd = cpp_cmd + " " + p.string();
        ThreadExecutor exe(cmd);
        exe.run();
        std::string output = exe.getStdOut();
        utils::write_file(p, output);
      }
    }
  }
}


/**
 * @param indir dir of oroginal benchmark
 * @param outdir dir to put preprocessed files
 */
void preprocess(fs::path indir, fs::path outdir) {
  if (fs::exists(outdir)) fs::remove_all(outdir);
  fs::create_directories(outdir);

  copy_src_files(indir, outdir);
  post_copy(outdir);
  // extract all the include<> information
  // (this does not take the conditional compilation into consideration)
  // std::map<fs::path, std::vector<std::string> > includes = get_includes();
  cpp(outdir);
  post_cpp(outdir);
  // add them back to the beginning
  // reapply_includes(outdir, includes);
}
