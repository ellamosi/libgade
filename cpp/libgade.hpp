#ifndef libgade_hpp
#define libgade_hpp

#define EXPORT __attribute__((visibility("default")))

#include <stdint.h>
#include "inputreader.hpp"

extern "C" {
    void gadeinit (void);
    void gadefinal (void);
}

typedef struct RGB32Bitmap {
    uint8_t r, g, b, unused;
} RGB32Bitmap;

class EXPORT GB {
    public:
    GB();
    ~GB();

    void load (char *romFile);
    void nextFrame (RGB32Bitmap *videoBuf);
    void setInputReader(InputReader *inputReader);

    private:
    struct Opaque;
    Opaque *opaque;

    GB(GB const &);
    GB & operator=(GB const &);
};

// Constructor functions and function types.
extern "C" GB* NewGB(void);
typedef GB * GB_creator(void);

// Destructor function and function type.
extern "C" void DeleteGB(GB* gb);
typedef void GB_disposer(GB*);

#endif /* libgade_hpp */
