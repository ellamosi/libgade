#ifndef inputreader_hpp
#define inputreader_hpp

#include <stdint.h>

class InputReader {
public:
    enum Button { A     = 0x01, B    = 0x02, SELECT = 0x04, START = 0x08,
                  RIGHT = 0x10, LEFT = 0x20, UP     = 0x40, DOWN  = 0x80 };

    virtual ~InputReader() {}

    virtual uint8_t readButtons() = 0;
};

#endif /* inputreader_hpp */
