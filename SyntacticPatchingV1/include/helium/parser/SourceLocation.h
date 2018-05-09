#ifndef SOURCE_LOCATION_H
#define SOURCE_LOCATION_H

#include <ostream>
#include <istream>



/**
 * It is passed by value. So the size is pretty critical.
 * But I'm not going to worry about this so much as I would like.
 * line and column starts from 1
 */
class SourceLocation {
public:
  SourceLocation() {}
  SourceLocation(int line, int column)
    : line(line), column(column) {}
  ~SourceLocation() {}

  bool operator<(SourceLocation &rhs) {
    if (this->line < rhs.line) return true;
    else if (this->line == rhs.line && this->column < rhs.column) return true;
    else return false;
  }
  bool operator<=(SourceLocation &rhs) {
    if (this->line < rhs.line) return true;
    else if (this->line == rhs.line && this->column <= rhs.column) return true;
    else return false;
  }
  bool operator>(SourceLocation &rhs) {
    if (this->line > rhs.line) return true;
    else if (this->line == rhs.line && this->column > rhs.column) return true;
    else return false;
  }
  bool operator>=(SourceLocation &rhs) {
    if (this->line > rhs.line) return true;
    else if (this->line == rhs.line && this->column >= rhs.column) return true;
    else return false;
  }
  bool operator==(SourceLocation &rhs) {
    return this->line == rhs.line && this->column == rhs.column;
  }

  friend std::ostream& operator<<(std::ostream &os, const SourceLocation &loc);
  friend std::istream& operator>>(std::istream &is, SourceLocation &loc);
  friend SourceLocation operator+(SourceLocation s, std::pair<int,int> delta);
  
  int getLine() {return line;}
  int getColumn() {return column;}
  void setLine(int l) {line=l;}
  void setColumn(int c) {column=c;}
private:
  int line = -1;
  int column = -1;
};

std::ostream& operator<<(std::ostream &os, const SourceLocation &loc);
std::istream& operator>>(std::istream &is, const SourceLocation &loc);

SourceLocation operator+(SourceLocation s, std::pair<int,int> delta);

// class SourceRange {
// public:
//   SourceRange() {}
//   ~SourceRange() {}
//   SourceLocation getBegin() {return Begin;}
//   SourceLocation getEnd() {return End;}
// private:
//   SourceLocation Begin;
//   SourceLocation End;
// };

class SourceRange {
public:
  SourceRange(SourceLocation begin, SourceLocation end) : begin(begin), end(end) {}
  ~SourceRange() {}

  SourceLocation getBegin() {return begin;}
  SourceLocation getEnd() {return end;}
private:
  SourceLocation begin;
  SourceLocation end;
};


#endif /* SOURCE_LOCATION_H */
