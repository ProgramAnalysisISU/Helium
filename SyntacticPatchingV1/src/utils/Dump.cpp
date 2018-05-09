#include "helium/utils/Dump.h"
#include "helium/utils/Utils.h"

#include "helium/utils/FSUtils.h"

ExpASTDump *ExpASTDump::m_instance = NULL;
BuildRatePlotDump *BuildRatePlotDump::m_instance = NULL;
/**
 * min, max, mean
 */
std::string dump_mmm(std::vector<int> data) {
  std::string ret;
  if (data.empty()) return "";
  int sum = 0;
  int min = data[0];
  int max = data[0];
  for (int size : data) {
    sum += size;
    if (size > max) max = size;
    if (size < min) min = size;
  }
  double mean = (double)sum / (double)(data.size());
  ret += std::to_string(min) + "," + std::to_string(max) + "," + std::to_string(mean);
  return ret;
}

std::string ExpASTDump::GetHeader() {
  std::string ret;
  ret = "benchmark, file count, func count, compile success, compile error, build rate, time(s)"
    ", suc_leaf_size: min, max, mean"
    ", err_leaf_size: min, max, mean"
    ", suc_snippet_size: min, max, mean"
    ", err_snipept_size: min, max, mean";
  return ret;
}

std::string ExpASTDump::dump() {
  std::string ret;
  ret += benchmark + ","
    + std::to_string(file_count) + ","
    + std::to_string(func_count) + ","
    + std::to_string(compile_success) + ","
    + std::to_string(compile_error) + ","
    + std::to_string((double)compile_success / (double)(compile_success + compile_error)) + ","
    + std::to_string(time) + ","
    + dump_mmm(suc_leaf_size) + ","
    + dump_mmm(err_leaf_size) + ","
    + dump_mmm(suc_snippet_size) + ","
    + dump_mmm(err_snippet_size);

    
  return ret;
}

static void append_to_file(std::string filename, std::vector<int> v) {
  std::string tmp;
  for (int suc : v) {
    tmp += std::to_string(suc) + " ";
  }
  utils::append_file(filename, tmp);
}

/**
 * Build rate and complexity plot data
 */
void ExpASTDump::AppendData() {
  if (!utils::exists("/tmp/helium")) {
    utils::create_folder("/tmp/helium");
  }
  append_to_file("/tmp/helium/leaf-suc.txt", suc_leaf_size);
  append_to_file("/tmp/helium/leaf-err.txt", err_leaf_size);
  append_to_file("/tmp/helium/snippet-suc.txt", suc_snippet_size);
  append_to_file("/tmp/helium/snippet-err.txt", err_snippet_size);
}
