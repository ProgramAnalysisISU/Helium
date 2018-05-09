#ifndef CACHE_H
#define CACHE_H

#include <boost/filesystem.hpp>
#include <boost/foreach.hpp>
#include <string>

namespace fs = boost::filesystem;


void create_src(fs::path target, fs::path target_cache_dir, fs::path target_sel_dir);
void create_cpp(fs::path target_cache_dir);

void preprocess(fs::path indir, fs::path outdir);

#endif /* CACHE_H */
