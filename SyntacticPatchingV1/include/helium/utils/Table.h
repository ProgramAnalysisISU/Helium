#ifndef TABLE_H
#define TABLE_H


#include <string>
#include <vector>
#include <map>
#include <set>
#include <stack>
#include <fstream>
#include <cassert>
#include <algorithm>
#include <iostream>
#include <boost/filesystem.hpp>

namespace fs = boost::filesystem;

/**
 * This file defines the table data file manipulating facilities.
 */

class Table {
public:
  Table() {}
  virtual ~Table() {}
  // reader
  std::vector<std::string> Column(std::string key);
  std::vector<std::string> Headers() {return m_headers;}

  // writer
  void AddRow(std::string line);
  void AddHeader(std::string line) {
    m_headers = parseRow(line);
  }
  
protected:
  virtual std::vector<std::string> parseRow(std::string line) = 0;
  std::vector<std::string> m_headers;
  std::vector<std::vector<std::string> > m_data;
  std::map<std::string, std::vector<std::string> > m_map;
};

class TableFactory {
public:
  static Table* Create(fs::path file);
};

class CSV : public Table {
protected:
  virtual std::vector<std::string> parseRow(std::string line) override;
};


class OrgTable : public Table {
protected:
  virtual std::vector<std::string> parseRow(std::string line) override;
};

#endif /* TABLE_H */
