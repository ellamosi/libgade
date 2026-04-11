#ifndef LIBGADE_HPP
#define LIBGADE_HPP

#include <stdint.h>

#include "cameraprovider.hpp"
#include "inputreader.hpp"

#define EXPORT __attribute__((visibility("default")))

static const uint32_t GB_DISPLAY_WIDTH = 160;
static const uint32_t GB_DISPLAY_HEIGHT = 144;
static const uint32_t GB_CAMERA_HEIGHT = 112;
static const uint32_t GB_CAMERA_WIDTH = 128;
static const uint32_t GB_CPU_CLOCK_FREQUENCY = 4194304;
static const uint32_t GB_CPU_M_FREQUENCY = GB_CPU_CLOCK_FREQUENCY / 4;
static const uint32_t GB_CPU_CYCLES_PER_AUDIO_SAMPLE =
    GB_CPU_CLOCK_FREQUENCY / GB_CPU_M_FREQUENCY;
static const uint32_t GADE_AUDIO_EXTRA_SAMPLES = 2064;
static const uint32_t GADE_AUDIO_MAXIMUM_SAMPLES = 19619;

typedef struct RGB32Bitmap {
    uint8_t r, g, b, unused;
} RGB32Bitmap;

typedef struct StereoSample {
    int16_t left, right;
} StereoSample;

class EXPORT GB {
public:
    GB();
    ~GB();

    void load(const char *romFile);
    void nextFrame(RGB32Bitmap *videoBuf);
    void reset();
    void runFor(uint32_t requestedSamples,
                uint32_t &generatedSamples,
                RGB32Bitmap *videoBuf,
                StereoSample *audioBuf,
                bool &frameFinished);
    void setCameraProvider(CameraProvider *cameraProvider);
    void setInputReader(InputReader *inputReader);

private:
    void *vptr_;
    void *gade_;

    GB(GB const &);
    GB &operator=(GB const &);
};

extern "C" GB *NewGB(void);
typedef GB *GB_creator(void);

extern "C" void DeleteGB(GB *gb);
typedef void GB_disposer(GB *);

#endif
