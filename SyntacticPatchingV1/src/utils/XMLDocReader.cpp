#include "helium/utils/XMLDocReader.h"
#include "helium/utils/Utils.h"
#include "helium/utils/XMLNodeHelper.h"
#include "helium/utils/ThreadUtils.h"
#include <iostream>
#include <boost/filesystem.hpp>

namespace fs = boost::filesystem;

// I should try my best to fill the filename here
XMLDoc* XMLDocReader::CreateDocFromString(const std::string &code) {
  std::string cmd;
  cmd = "srcml-client.py -";

  ThreadExecutor exe(cmd);
  exe.setInput(code);
  exe.setTimeoutSec(0.2);
  exe.run();

  assert(exe.getReturnCode()==0 && "SrcML error");
  std::string output = exe.getStdOut();

  pugi::xml_document *doc = new pugi::xml_document();
  doc->load_string(output.c_str(), pugi::parse_default | pugi::parse_ws_pcdata);
  return doc;
}

XMLDoc* XMLDocReader::CreateDocFromFile(std::string filename) {
  std::string cmd;
  cmd = "srcml-client.py " + filename;

  ThreadExecutor exe(cmd);
  exe.setTimeoutSec(0.2);
  exe.run();

  assert(exe.getReturnCode() == 0 && "SrcML error");
  std::string output = exe.getStdOut();
  
  pugi::xml_document *doc = new pugi::xml_document();
  doc->load_string(output.c_str(), pugi::parse_default | pugi::parse_ws_pcdata);
  return doc;
}
