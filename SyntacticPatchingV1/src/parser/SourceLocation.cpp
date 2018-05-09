#include "helium/parser/SourceLocation.h"
#include <gtest/gtest.h>

std::ostream& operator<<(std::ostream &os, const SourceLocation &loc) {
  os << loc.line << " " << loc.column;
  return os;
}
std::istream& operator>>(std::istream &is, SourceLocation &loc) {
  is >> loc.line >> loc.column;
  return is;
}
SourceLocation operator+(SourceLocation s, std::pair<int,int> delta) {
  s.line += delta.first;
  s.column += delta.second;
  return s;
}



// TEST(SourceLocationTest, RelationTest) {
//   SourceLocation l1(1, 2);
//   SourceLocation l2(1,-1);
//   SourceLocation l3(3,4);
//   SourceLocation l4(3,8);
//   SourceLocation l4(3,5);
//   SourceLocation l4(3,-1);

//   EXPECT_TRUE(l1 > l2);
//   EXPECT_TRUE(l1==l2);
// }
