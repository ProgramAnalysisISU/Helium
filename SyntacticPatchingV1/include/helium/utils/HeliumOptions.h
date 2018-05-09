#ifndef HELIUM_OPTIONS_H
#define HELIUM_OPTIONS_H

#include <boost/program_options.hpp>
#include <boost/program_options/options_description.hpp>
#include <boost/program_options/variables_map.hpp>
#include <boost/program_options/parsers.hpp>

namespace po = boost::program_options;

class HeliumOptions {
public:
  HeliumOptions();
  ~HeliumOptions() {}
  void ParseCommandLine(int argc, char** argv);
  void ParseConfigFile(std::string config_file);
  void PrintHelp();
  bool Has(std::string name) {
    return m_vm.count(name) == 1;
  }

  std::string GetString(std::string key);
  bool GetBool(std::string key);
  int GetInt(std::string key);
  std::vector<std::string> GetStringVector(std::string key);

  bool HasCmdUtils();
  bool empty() {
    if (m_vm.empty()) return true;
    return false;
  }
private:
  
  bool validate();

  po::options_description m_cmdline_options;
  po::options_description m_config_options;
  po::options_description m_help_options;
  po::positional_options_description m_positional;
  po::variables_map m_vm;
};


#endif /* HELIUM_OPTIONS_H */
