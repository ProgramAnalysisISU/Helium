#ifndef XML_DOC_READER_H
#define XML_DOC_READER_H

#include "XMLNode.h"

/**
 * Read the xml document by SrcML.
 * It will make sure there's only one copy of the same XML document
 * This intends to
 *   1. remove the overhead of creating the same document again and again.
 *   2. ensure the same XML node reference is comparable for the same code file.
 * But this also puts some assumptions
 *   1. the XML document should not be modified (or should be changed back after modifying)
 */

class XMLDocReader {
public:
  XMLDocReader() {}
  ~XMLDocReader() {}
  /**
   * These two methods are static, meaning the user is responsible to free the returned document
   * The cache is not used even for the file option, because there's no instance of XMLDocReader involved
   * Since it is created from string, the default filename is empty.
   * But it is able to set the file name
   */
  static XMLDoc* CreateDocFromString(const std::string &code);
  static XMLDoc* CreateDocFromFile(std::string filename);
};

#endif /* XML_DOC_READER_H */
