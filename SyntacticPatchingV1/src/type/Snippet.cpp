#include "helium/type/Snippet.h"
#include "helium/utils/FSUtils.h"
#include "helium/utils/StringUtils.h"

#include "helium/type/SnippetAction.h"
#include "helium/parser/IncludeManager.h"
#include "helium/parser/LibraryManager.h"

#include "helium/utils/Graph.h"
#include "helium/utils/StringUtils.h"


#include <rapidjson/istreamwrapper.h>

using std::set;
using std::vector;

/**
 * Scheme
[
    {
        "name": "foo",
        "ID": 0,
        "kind": "Record",
        "file": "/path/to/file",
        "begin": {
            "line": 8,
            "col": 10
        },
        "end": {
            "line": 8,
            "col": 10
        },
        "deps": [2,5,8],
        "outers": [3,8]
    }
]

 */

/**
 * push back
 */
rapidjson::Value Snippet::saveJson(rapidjson::Document::AllocatorType &allocator) {
  // rapidjson::Document::AllocatorType &allocator = document.GetAllocator();
  rapidjson::Value array(rapidjson::kObjectType);
  rapidjson::Value obj(rapidjson::kObjectType);
  rapidjson::Value name_str;
  name_str.SetString(Name.c_str(), allocator);
  obj.AddMember("name", name_str, allocator);
  obj.AddMember("id", ID, allocator);
  rapidjson::Value file_str;
  file_str.SetString(File.c_str(), allocator);
  obj.AddMember("file", file_str, allocator);
  // begin & end
  rapidjson::Value begin(rapidjson::kObjectType);
  rapidjson::Value end(rapidjson::kObjectType);
  begin.AddMember("line", Begin.getLine(), allocator);
  begin.AddMember("col", Begin.getColumn(), allocator);
  end.AddMember("line", End.getLine(), allocator);
  end.AddMember("col", End.getColumn(), allocator);
  obj.AddMember("begin", begin, allocator);
  obj.AddMember("end", end, allocator);

  // // dep & outer
  rapidjson::Value deps(rapidjson::kArrayType);
  rapidjson::Value outers(rapidjson::kArrayType);
  for (Snippet *dep : Deps) {
    deps.PushBack(dep->getId(), allocator);
  }
  for (Snippet *outer : Outers) {
    outers.PushBack(outer->getId(), allocator);
  }
  obj.AddMember("deps", deps, allocator);
  obj.AddMember("outers", outers, allocator);
  return obj;
  // document.PushBack(obj, allocator);
 }

void Snippet::loadJson(rapidjson::Value &obj) {
  assert(obj.IsObject());
  Name = obj["name"].GetString();
  ID = obj["id"].GetInt();
  File = obj["file"].GetString();
  Begin = {obj["begin"]["line"].GetInt(),
           obj["begin"]["col"].GetInt()};
  End = {obj["end"]["line"].GetInt(),
           obj["end"]["col"].GetInt()};
  // deps and outers have to be add later
}
rapidjson::Value EnumSnippet::saveJson(rapidjson::Document::AllocatorType &allocator) {
  rapidjson::Value v = Snippet::saveJson(allocator);
  v.AddMember("kind", "EnumSnippet", allocator);
  rapidjson::Value fields(rapidjson::kArrayType);
  for (std::string f : Fields) {
    rapidjson::Value f_s;
    f_s.SetString(f.c_str(), allocator);
    fields.PushBack(f_s, allocator);
  }
  v.AddMember("fields", fields, allocator);
  return v;
}
void EnumSnippet::loadJson(rapidjson::Value &v) {
  Snippet::loadJson(v);
  for (rapidjson::Value &field : v["fields"].GetArray()) {
    Fields.push_back(field.GetString());
  }
}


std::string read_file_for_code(fs::path file, SourceLocation begin, SourceLocation end) {
  int l1 = begin.getLine();
  int c1 = begin.getColumn();
  int l2 = end.getLine();
  int c2 = end.getColumn();
  
  std::ifstream is;
  is.open(file.string());
  int l=0;
  std::string ret;
  if (is.is_open()) {
    std::string line;
    try {
      while(getline(is, line)) {
        l++;
        if (l < l1 || l > l2) {
        } else if (l>l1 && l < l2) {
          ret += line + "\n";
        } else if (l==l1 && l == l2) {
          ret += line.substr(c1-1, c2+1 - c1);
        } else if (l==l2) {
          ret += line.substr(0, c2);
        } else if (l==l1) {
          ret += line.substr(c1-1) + "\n";
        } else {
          break;
        }
      }
    } catch (std::out_of_range e) {
      std::cerr << "Cannot get code." << "\n";
    }
    is.close();
  }
  return ret;
}

std::string read_file_for_code_until_semicolon(fs::path file, SourceLocation begin, SourceLocation end) {
  std::ifstream is;
  is.open(file.string());
  std::string ret;
  assert(is.is_open());
  int line=1,col=0;
  char c;
  while (is.get(c)) {
    if (c=='\n') {line++;col=0;}
    else col++;

    SourceLocation loc(line,col);

    if (loc < begin) ;
    else if (loc >= begin && loc <= end ) ret+=c;
    else {
      is.unget();
      break;
    }
  }
  while (is.get(c)) {
    if (c==';') break;
    ret += c;
  }
  is.close();
  return ret;
}

void MacroSnippet::readCode() {
  Code = read_file_for_code(File, Begin, End);
  Code = "#define " + Code;
}
void FunctionSnippet::readCode() {
  Code = read_file_for_code(File, Begin, End);
}
void FunctionDeclSnippet::readCode() {
  Code = read_file_for_code_until_semicolon(File, Begin, End);
}
void RecordSnippet::readCode() {
  Code = read_file_for_code(File, Begin, End);
}
void RecordDeclSnippet::readCode() {
  Code = read_file_for_code_until_semicolon(File, Begin, End);
}
void EnumSnippet::readCode() {
  Code = read_file_for_code(File, Begin, End);
}

/**
 * This performs the char-by-char read.
 * The variable does not stop until a semi-colon
 * I know this is a trick.
 */
void VarSnippet::readCode() {
  // this should read until a semi-colon
  Code = read_file_for_code_until_semicolon(File, Begin, End);
}

void TypedefSnippet::readCode() {
  Code = read_file_for_code_until_semicolon(File, Begin, End);
}

void Snippet::dump(std::ostream &os) {
  os << Name;
}

void Snippet::dumpVerbose(std::ostream &os) {
  rapidjson::Document doc;
  doc.SetObject();
  rapidjson::Value v = saveJson(doc.GetAllocator());
  rapidjson::StringBuffer sb;
  // rapidjson::PrettyWriter<rapidjson::StringBuffer> writer(sb);
  rapidjson::Writer<rapidjson::StringBuffer> writer(sb);
  v.Accept(writer);
  os << sb.GetString() << "\n";
}


std::string FunctionSnippet::getFuncDecl() {
  // it should be
  std::string code = getCode();
  std::string ret = code.substr(0, code.find('{'));
  utils::trim(ret);
  if (ret.find(';') == std::string::npos) {
    ret += ";";
  } else {
    std::string first, last;
    first = ret.substr(0, ret.find('('));
    last = ret.substr(ret.find(')')+1);
    std::string params;
    while (last.find(';') != std::string::npos) {
      int pos = last.find(';');
      params += last.substr(0, pos) + ",";
      last = last.substr(pos+1);
    }
    params.pop_back();
    ret = first + "(" + params + ");";
  }
  // add extern because inline function and inline declaration would
  // make undefined reference.  Note that this also applies to inline
  // function (which is the purpose to add this)
  // ret = "extern " + ret;
  //
  // UPDATE the extern method does not work because extern cannot be
  // combined with static So I'm going the second way: remove inline
  // when declare a function
  if (ret.find("inline") != std::string::npos) {
    utils::replace(ret, "inline", "");
  }
  return ret;
}


std::string TypedefSnippet::getDecl() {
  std::string code = getCode();
  if (code.find('{') == std::string::npos) {
    return code + ";";
  }
  assert(code.find('}') != std::string::npos);
  std::string ret;
  ret = code.substr(0, code.find_first_of('{'));
  {
    // if this is "typedef struct", return
    std::string tmp=ret;
    utils::trim(tmp);
    if (utils::split(tmp).size() == 2) return "";
  }
  ret += code.substr(code.find_last_of('}')+1);
  ret += ";";
  return ret;
}


std::string RecordSnippet::getDecl() {
  std::string code = getCode();
  std::string ret;
  if (code.find('{') == std::string::npos) return "";

  assert(code.find('}') != std::string::npos);
  ret = code.substr(0, code.find_first_of('{'));
  utils::trim(ret);
  std::vector<std::string> v = utils::split(ret);
  if (v.size() != 2) return "";
  ret += ";";
  return ret;
}


std::set<Snippet*> Snippet::getAllDeps() {
  std::set<Snippet*> worklist;
  std::set<Snippet*> done;
  std::set<Snippet*> ret;
  worklist.insert(this);
  while (!worklist.empty()) {
    Snippet *s = *worklist.begin();
    worklist.erase(s);
    if (done.count(s) == 1) continue;
    std::set<Snippet*> dep = s->getDeps();
    worklist.insert(dep.begin(), dep.end());
    ret.insert(dep.begin(), dep.end());
    done.insert(s);
  }
  ret.erase(this);
  return ret;
}
