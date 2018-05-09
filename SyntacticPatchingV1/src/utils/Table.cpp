#include "helium/utils/Table.h"
#include <iostream>
#include "helium/utils/Utils.h"

#include "helium/utils/StringUtils.h"

Table* TableFactory::Create(fs::path file) {
  if (!fs::exists(file)) {
    std::cerr << "EE: table file does not exist" << "\n";
    return NULL;
  }
  std::ifstream ifs(file.string());
  if (!ifs.is_open()) {
    std::cerr << "EE: table file cannot be opened." << "\n";
    return NULL;
  }
  Table *ret = NULL;
  
  if (file.extension() == ".csv") {
    ret = new CSV();
  } else if (file.extension() == ".org") {
    ret = new OrgTable();
  } else {
    std::cerr << "EE: csv file format " + file.extension().string() + " not supported." << "\n";
    exit(1);
  }
  
  std::string line;
  std::getline(ifs, line);
  ret->AddHeader(line);
  while (std::getline(ifs, line)) {
    ret->AddRow(line);
  }
  ifs.close();
  
  return ret;
}


std::vector<std::string> Table::Column(std::string key) {
  // construct the map if not there already
  if (m_map.empty()) {
    // construct the map
    for (int i=0;i<(int)m_headers.size();i++) {
      for (int j=0;j<(int)m_data.size();j++) {
        m_map[m_headers[i]].push_back(m_data[j][i]);
      }
    }
  }
  if (m_map.count(key) == 1) {
    return m_map[key];
  }
  return {};
}

void Table::AddRow(std::string line) {
  std::vector<std::string> row = parseRow(line);
  if (row.size() == m_headers.size()) {
    m_data.push_back(row);
  }
}


std::vector<std::string> CSV::parseRow(std::string line) {
  std::vector<std::string> ret = utils::split(line, ',');;
  for (std::string &t : ret) {
    utils::trim(t);
  }
  return ret;
}

/**
 * help class for table reading
 * the table is of org table format.
 * Will ignore |---+---+---| lines
 * Will ignore ill-formed lines
 * The first line should NOT be ill-formed
 * The first line shall be the header
 */
std::vector<std::string> OrgTable::parseRow(std::string line) {
  std::vector<std::string> ret;
  utils::trim(line);
  if (line.empty() || line[0] == '#') return ret;
  std::vector<std::string> tmp = utils::split(line, '|');
  for (std::string &t : tmp) {
    utils::trim(t);
  }
  if (tmp.size() < 3) return ret;
  ret.insert(ret.begin(), tmp.begin()+1, tmp.end()-1);
  return ret;
}
