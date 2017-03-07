#include "libgade.hpp"

extern "C" {
    void gadeInit (GB *gb);
    void gadeFinal (GB *gb);
    void gadeLoad (GB *gb, const char *romFile);
    void gadeNextFrame (GB *gb, RGB32Bitmap *videoBuf);
    void gadeSetInputReader (GB *gb, InputReader *inputReaderInstance);
    uint8_t InputReader_readInput (InputReader *inputReaderInstance);
}

uint8_t InputReader_readInput (InputReader *inputReaderInstance) {
    return inputReaderInstance->readButtons();
}

EXPORT
GB::GB() {
    gadeInit(this);
}

EXPORT
GB::~GB() {
    gadeFinal(this);
}

EXPORT
GB* NewGB(void) {
  // Remove this method
    return new GB;
}

EXPORT
void DeleteGB(GB* gb) {
  // Remove this method
    delete gb;
}

EXPORT
void GB::load (char *romFile) {
    gadeLoad(this, romFile);
}

EXPORT
void GB::nextFrame (RGB32Bitmap *videoBuf) {
    gadeNextFrame(this, videoBuf);
}

EXPORT
void GB::setInputReader(InputReader *inputReader) {
    gadeSetInputReader(this, inputReader);
}

