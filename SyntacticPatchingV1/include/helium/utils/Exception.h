#ifndef EXCEPTION_H
#define EXCEPTION_H

class HeliumException : public std::exception {
public:
  HeliumException(std::string text) {
    m_text = text;
  }
  virtual const char *what() const throw() {
    return m_text.c_str();
  }
private:
  std::string m_text;
};


#endif /* EXCEPTION_H */
