#include "helium/type/Type.h"
#include "helium/type/IOHelper.h"

#include "helium/utils/Utils.h"

#include <boost/regex.hpp>




bool type_has_word(std::string type, std::string word) {
  boost::regex reg("\\b" + word + "\\b");
  if (boost::regex_search(type, reg)) {
    return true;
  }
  return false;
}

std::string type_remove_word(std::string type, std::string word) {
  std::string ret;
  boost::regex reg("\\b" + word + "\\b");
  if (boost::regex_search(type, reg)) {
    ret = boost::regex_replace<boost::regex_traits<char>, char> (type, reg, "");
  }
  return ret;
}

/**
 * "char *"
 */
bool type_is_str(std::string str) {
  if (type_has_word(str, "char")
      && std::count(str.begin(), str.end(), '*') == 1
      && str.find('[') == std::string::npos
      ) {
    return true;
  }
  return false;
}

/**
 * char [3]
 */
bool type_is_buf(std::string str) {
  if (type_has_word(str, "char")
      && str.find('*') == std::string::npos
      && std::count(str.begin(), str.end(), '[') == 1
      ) {
    return true;
  }
  return false;
}

/**
 * Check if it is a valid type that Helium will handle.
 */
bool helium_valid_type(std::string str) {
  std::vector<std::string> valid_tokens {
    "char", "int", "bool", "_Bool"
      };
  std::vector<std::string> invalid_tokens {
    "const", "static", "auto", "extern", "volatile"
      };
  // if there's still something left, it must be a local structure, otherwise put into known
  // do not handle for now, put into unknown!
  str = type_remove_word(str, "char");
  str = type_remove_word(str, "int");
  str = type_remove_word(str, "bool");
  str = type_remove_word(str, "_Bool");

  while (str.back() == ']' || str.back() == '*') {
    if (str.back() == ']') {
      str = str.substr(str.find_last_of('['));
    }
    if (str.back() == '*') {
      str.pop_back();
    }
    utils::trim(str);
  }
  if (str.empty()) {
    return true;
  }
  return false;
}


/**
 * Current Strategy: First check if it is valid.
 * Only construct type when it is, otherwise create an Unknown type, where only decl is added.
 * But, be sure to handle all valid cases!
 */
Type *TypeFactory::CreateType(std::string str) {
  if (str.empty()) return NULL;

  if (str.find("const") != std::string::npos) {
    utils::replace(str, "const", "");
    // return new ConstType(str);
  }
  
  Type *type;
  // Pointer
  if (str.back() == '*') {
    str.pop_back();
    type = new PointerType(str);
    return type;
  }

  // Array
  if (str.back() == ']') {
    str.pop_back();
    std::string numstr = str.substr(str.find_last_of('[')+1);
    str = str.substr(0, str.find_last_of('['));
    if (numstr.empty()) {
      // [], should be the argument, treat as double pointer
      return new PointerType(str);
    } else {
      // magic number, if the array size is unknown, maybe an expression, use 10 as the size
      int num = 10;
      try {
        num = stoi(numstr);
      } catch (std::exception e) {
        // helium_log_warning("Exception in TypeFactory::CreateType: " + std::string(e.what()));
        // FIXME
        // std::cerr << "Array size not a number: "  << std::string(e.what()) << ". Using magic number 10.\n";
      }
      type = new ArrayType(str, num);
      return type;
    }
  }
  // Simple Type
  if (type_has_word(str, "char")) {
    return new CharType();
  } else if (type_has_word(str, "int")) {
    return new IntType();
  } else if (type_has_word(str, "bool") || type_has_word(str, "_Bool")) {
    return new BoolType();
  }
  // TODO structure
  // TODO enumerator?
  // TODO system type?
  return NULL;
}



/**
 * If contained type is "char", use scanf("%s)
 * Otherwise, init the first index if available.
 */
std::string ArrayType::GetInputCode(std::string var) {
  return "// TODO ArrayType::GetInputCode\n";
}

std::string ArrayType::GetOutputCode(std::string var) {
  return "// TODO ArrayType::GetOutputCode\n";
}

std::string PointerType::GetInputCode(std::string var) {
  return "// TODO PointerType::GetInputCode\n";
}

std::string PointerType::GetOutputCode(std::string var) {
  return "// TODO PointerType::GetOutputCode\n";
}
