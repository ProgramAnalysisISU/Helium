#ifndef COLOR_H
#define COLOR_H

namespace utils {
  extern const char* RED;
  extern const char* GREEN;
  extern const char* YELLOW;
  extern const char* BLUE;
  extern const char* PURPLE;
  extern const char* CYAN;
  extern const char* RESET;

  typedef enum {
    CK_Red,
    CK_Green,
    CK_Yellow,
    CK_Blue,
    CK_Purple,
    CK_Cyan,
    CK_Reset
  } ColorKind;

}

#endif /* COLOR_H */
