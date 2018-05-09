#ifndef IOHELPER_H
#define IOHELPER_H

#include <string>
#include "helium/utils/StringUtils.h"

class FileIOHelper {
public:
  static std::string GetIntCode();
  static std::string GetIOCode();
  static std::string GetGlobalFilePointers();
  static std::string GetFilePointersInit();
};

class IOHelper {
public:
  IOHelper() {}
  ~IOHelper() {}

  static std::string GetInputCall(std::string key, std::string var) {
    return "input_"+key+"(&"+var+");\n";
  }
  static std::string GetInputCallWithName(std::string key, std::string var, std::string name) {
    const char *format=R"prefix(
input_%s_name(&(%s), "%s");
)prefix";
    char buf[BUFSIZ];
    sprintf(buf, format, key.c_str(), var.c_str(), name.c_str());
    std::string ret=buf;
    utils::trim(ret);
    ret+="\n";
    return ret;
  }
  static std::string GetOutputCall(std::string key, std::string var, std::string name) {
    const char *format=R"prefix(
output_%s(&(%s),"%s");
)prefix";
    char buf[BUFSIZ];
    sprintf(buf, format, key.c_str(), var.c_str(), name.c_str());
    std::string ret=buf;
    utils::trim(ret);
    ret+="\n";
    return ret;
  }
  
};


#endif /* IOHELPER_H */
