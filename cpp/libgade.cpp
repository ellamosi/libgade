#include "libgade.hpp"

extern "C" {
void gadeFinal(GB *gb);
void gadeInit(GB *gb);
void gadeLoad(GB *gb, const char *romFile);
void gadeNextFrame(GB *gb, RGB32Bitmap *videoBuf);
void gadeReset(GB *gb);
void gadeRunFor(GB *gb,
                uint32_t requestedSamples,
                uint32_t *generatedSamples,
                RGB32Bitmap *videoBuf,
                StereoSample *audioBuf,
                uint8_t *frameFinished);
void gadeSetInputReader(GB *gb, InputReader *inputReaderInstance);
void gadeSetLogger(GB *gb, Logger *loggerInstance);
uint8_t InputReader_readInput(InputReader *inputReaderInstance);
void Logger_log(Logger *loggerInstance, uint8_t level, const char *message);
}

uint8_t InputReader_readInput(InputReader *inputReaderInstance) {
    return inputReaderInstance->readButtons();
}

void Logger_log(Logger *loggerInstance, uint8_t level, const char *message) {
    loggerInstance->log(static_cast<Logger::Level>(level), message);
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
GB *NewGB(void) {
    return new GB;
}

EXPORT
void DeleteGB(GB *gb) {
    delete gb;
}

EXPORT
void GB::load(const char *romFile) {
    gadeLoad(this, romFile);
}

EXPORT
void GB::nextFrame(RGB32Bitmap *videoBuf) {
    gadeNextFrame(this, videoBuf);
}

EXPORT
void GB::reset() {
    gadeReset(this);
}

EXPORT
void GB::runFor(uint32_t requestedSamples,
                uint32_t &generatedSamples,
                RGB32Bitmap *videoBuf,
                StereoSample *audioBuf,
                bool &frameFinished) {
    uint8_t finished = 0;

    gadeRunFor(this,
               requestedSamples,
               &generatedSamples,
               videoBuf,
               audioBuf,
               &finished);
    frameFinished = finished != 0;
}

EXPORT
void GB::setInputReader(InputReader *inputReader) {
    gadeSetInputReader(this, inputReader);
}

EXPORT
void GB::setLogger(Logger *logger) {
    gadeSetLogger(this, logger);
}
